#ifndef AUTOMATA_DYNAMIC_BINARY_TREE_HPP
#define AUTOMATA_DYNAMIC_BINARY_TREE_HPP

#include "util.hpp"

namespace automata {

template <typename T, bool bit0 = false, bool bit1 = false, bool bit2 = false>
struct dynamic_binary_tree {
	struct node_type {
		T data;
		uintptr_t m_left;
		uintptr_t m_right;
		uintptr_t m_parent;

		constexpr node_type() noexcept : data(), m_left(), m_right(), m_parent() {}

		template <typename...Args>
		constexpr node_type(Args&&...args) : data(std::forward<Args>(args)...), m_left(), m_right(), m_parent() {}

		template <bool b0, bool b1, bool b2>
			requires (b0 != bit0 || b1 != bit1 || b2 != bit2)
		constexpr node_type(typename dynamic_binary_tree<T, b0, b1, b2>::node_type n) : node_type() {
			*this = std::move((node_type&)n);
		}

		//template <bool b0, bool b1, bool b2>
		//operator typename dynamic_binary_tree<T, b0, b1, b2>::node_type&() {
		//	return (typename dynamic_binary_tree<T, b0, b1, b2>::node_type&)*this;
		//}

		static node_type iterative_copy(const node_type* self) {
			enum copy_stage {
				INIT,
				POSTLEFT,
				POSTRIGHT
			};
			struct Locals {
				copy_stage stage;
				const node_type* self;
				node_type result;
			};
			std::vector<Locals> locals;
			node_type ret;
			locals.push_back(Locals{INIT, self});
			while (!locals.empty()) {
				auto&& [stage, self, result] = locals.back();
				switch (stage) {
				case INIT:
					result = node_type(self->data);
					if constexpr (bit0) {
						result.set_bit0(self->get_bit0());
					}
					if constexpr (bit1) {
						result.set_bit1(self->get_bit1());
					}
					if constexpr (bit2) {
						result.set_bit2(self->get_bit2());
					}
					if (self->has_left()) {
						stage = POSTLEFT;
						locals.push_back(Locals{INIT, self->left()});
					} else if (self->has_right()) {
						stage = POSTRIGHT;
						locals.push_back(Locals{INIT, self->right()});
					} else {
						ret = std::move(result);
						locals.pop_back();
					}
					continue;
				case POSTLEFT:
					result.left(new node_type(std::move(ret)));
					if (self->has_right()) {
						stage = POSTRIGHT;
						locals.push_back(Locals{INIT, self->right()});
					} else {
						ret = std::move(result);
						locals.pop_back();
					}
					continue;
				case POSTRIGHT:
					result.right(new node_type(std::move(ret)));
					ret = std::move(result);
					locals.pop_back();
					continue;
				}
			}
			return ret;
		}

		static node_type recursive_copy(const node_type* self, size_t d) {
			// each call of recursive_copy requires sizeof(&recursive_copy) + sizeof(node_type) + sizeof(node_type*) + sizeof(size_t)
			// we do not want to go deeper than 4KiB
			static constexpr size_t RECURSION_LIMIT = (1 << 12)
				/ (sizeof(&recursive_copy) + sizeof(node_type) + sizeof(node_type*) + sizeof(size_t));
			node_type result(self->data);
			if constexpr (bit0) {
				result.set_bit0(self->get_bit0());
			}
			if constexpr (bit1) {
				result.set_bit1(self->get_bit1());
			}
			if constexpr (bit2) {
				result.set_bit2(self->get_bit2());
			}
			if (self->has_left()) {
				if (d < RECURSION_LIMIT) {
					result.left(new node_type(recursive_copy(self->left(), d + 1)));
				} else {
					result.left(new node_type(iterative_copy(self->left())));
				}
			}
			if (self->has_right()) {
				if (d < RECURSION_LIMIT) {
					result.right(new node_type(recursive_copy(self->right(), d + 1)));
				} else {
					result.right(new node_type(iterative_copy(self->right())));
				}
			}
			return result;
		}

		node_type copy() const {
			return recursive_copy(this, 0);
		}

		constexpr node_type& operator=(node_type&& o) noexcept {
			if (&o == this) {
				return *this;
			}
			destroy();
			data = std::move(o.data);
			m_left = o.m_left;
			m_right = o.m_right;
			m_parent = o.m_parent;
			o.m_left = 0;
			o.m_right = 0;
			o.m_parent = 0;
			return *this;
		}

		node_type& operator=(const node_type& o) {
			if (&o == this) {
				return *this;
			}
			return *this = o.copy();
		}

		node_type(const node_type& o) : node_type() {
			*this = o;
		}
		constexpr node_type(node_type&& o) noexcept : node_type() {
			*this = std::move(o);
		}
		constexpr ~node_type() noexcept {
			destroy();
		}

		constexpr void destroy() noexcept {
			if (has_left()) {
				delete left();
				left(nullptr);
			}
			if (has_right()) {
				delete right();
				right(nullptr);
			}
		}
		bool is_root() const noexcept {
			return parent() != nullptr;
		}
		bool has_left() const noexcept {
			return left() != nullptr;
		}
		bool has_right() const noexcept {
			return right() != nullptr;
		}
		bool get_bit0() const noexcept requires bit0 {
			return m_left & uintptr_t(1) << (bit_count<uintptr_t>() - 1);
		}
		void set_bit0(bool b) noexcept requires bit0 {
			if (b) {
				m_left |= uintptr_t(1) << (bit_count<uintptr_t>() - 1);
			} else {
				m_left &= ~(uintptr_t(1) << (bit_count<uintptr_t>() - 1));
			}
		}
		bool get_bit1() const noexcept requires bit1 {
			return m_right & uintptr_t(1) << (bit_count<uintptr_t>() - 1);
		}
		void set_bit1(bool b) noexcept requires bit1 {
			if (b) {
				m_right |= uintptr_t(1) << (bit_count<uintptr_t>() - 1);
			} else {
				m_right &= ~(uintptr_t(1) << (bit_count<uintptr_t>() - 1));
			}
		}
		bool get_bit2() const noexcept requires bit2 {
			return m_parent & uintptr_t(1) << (bit_count<uintptr_t>() - 1);
		}
		void set_bit2(bool b) noexcept requires bit2 {
			if (b) {
				m_parent |= uintptr_t(1) << (bit_count<uintptr_t>() - 1);
			} else {
				m_parent &= ~(uintptr_t(1) << (bit_count<uintptr_t>() - 1));
			}
		}
		node_type* left() const noexcept {
			if constexpr (bit0) {
				return (node_type*)(m_left & ~(uintptr_t(1) << (bit_count<uintptr_t>() - 1)));
			} else {
				return (node_type*)m_left;
			}
		}
		void left(node_type* n) noexcept {
			if constexpr (bit0) {
				m_left = (uintptr_t)n | (m_left & uintptr_t(1) << (bit_count<uintptr_t>() - 1));
			} else {
				m_left = (uintptr_t)n;
			}
		}
		node_type* right() const noexcept {
			if constexpr (bit1) {
				return (node_type*)(m_right & ~(uintptr_t(1) << (bit_count<uintptr_t>() - 1)));
			} else {
				return (node_type*)m_right;
			}
		}
		void right(node_type* n) noexcept {
			if constexpr (bit1) {
				m_right = (uintptr_t)n | (m_right & uintptr_t(1) << (bit_count<uintptr_t>() - 1));
			} else {
				m_right = (uintptr_t)n;
			}
		}
		node_type* parent() const noexcept {
			if constexpr (bit2) {
				return (node_type*)(m_parent & ~(uintptr_t(1) << (bit_count<uintptr_t>() - 1)));
			} else {
				return (node_type*)m_parent;
			}
		}
		void parent(node_type* n) noexcept {
			if constexpr (bit2) {
				m_parent = (uintptr_t)n | (m_parent & uintptr_t(1) << (bit_count<uintptr_t>() - 1));
			} else {
				m_parent = (uintptr_t)n;
			}
		}
	} m_root;

	constexpr dynamic_binary_tree() noexcept : m_root(nullptr) {}

	constexpr dynamic_binary_tree& operator=(dynamic_binary_tree&& o) noexcept {
		if (&o == this) {
			return *this;
		}
		destroy();
		root(o.root());
		o.root(nullptr);
		return *this;
	}

	dynamic_binary_tree& operator=(const dynamic_binary_tree& o) {
		if (&o == this) {
			return *this;
		}
		destroy();
		root(new node_type(o.root()->copy()));
		return *this;
	}

	constexpr dynamic_binary_tree(dynamic_binary_tree&& o) noexcept : dynamic_binary_tree() {
		*this = std::move(o);
	}
	dynamic_binary_tree(const dynamic_binary_tree& o) : dynamic_binary_tree() {
		*this = o;
	}

	constexpr ~dynamic_binary_tree() noexcept {
		destroy();
	}

	constexpr void destroy() noexcept {
		if (root()) {
			delete root();
			root(nullptr);
		}
	}

	constexpr void root(node_type* n) noexcept {
		m_root = n;
	}
	constexpr node_type* root() noexcept {
		return m_root;
	}
	constexpr const node_type* root() const noexcept {
		return m_root;
	}

};

} // namespace automata


#endif // #ifndef AUTOMATA_DYNAMIC_BINARY_TREE_HPP
