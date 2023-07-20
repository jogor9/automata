#ifndef AUTOMATA_DYNAMIC_TREE
#define AUTOMATA_DYNAMIC_TREE

#include <concepts>
#include <algorithm>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <deque>
#include <ranges>
#include "stack.hpp"
#include "queue.hpp"

namespace automata {

template <typename NodeData, std::unsigned_integral Index = size_t>
struct dynamic_tree {
	typedef NodeData data_type;
	struct node : protected data_type {
	protected:
		size_t m_parent;
		std::vector<size_t> m_children;
	public:
		template <typename...Args>
			requires std::constructible_from<data_type, Args...>
		explicit node(Args&&...args) : data_type(std::forward<Args>(args)...), m_parent(-1), m_children() {}

		data_type& data() noexcept {
			return (data_type&)*this;
		}
		const data_type& data() const noexcept {
			return (const data_type&)*this;
		}
		data_type* operator->() noexcept {
			return &data();
		}
		const data_type* operator->() const noexcept {
			return &data();
		}
		data_type& operator*() noexcept {
			return data();
		}
		const data_type& operator*() const noexcept {
			return data();
		}
		template <std::integral Int>
		size_t operator[](Int i) const noexcept {
			return children()[i];
		}
		size_t parent() const noexcept {
			return m_parent;
		}
		void parent(size_t p) noexcept {
			m_parent = p;
		}
		bool no_parent() const noexcept {
			return parent() == (size_t)-1;
		}
		std::vector<size_t>& children() noexcept {
			return m_children;
		}
		const std::vector<size_t>& children() const noexcept {
			return m_children;
		}
		template <typename T>
		void children(T&& c) noexcept(noexcept(m_children = std::forward<T>(c))) {
			m_children = std::forward<T>(c);
		}
		bool has_left() const noexcept {
			return children().size() != 0;
		}
		bool has_right() const noexcept {
			return children().size() > 1;
		}
		size_t left() const noexcept {
			if (has_left()) {
				return children().front();
			} else {
				return (size_t)-1;
			}
		}
		size_t right() const noexcept {
			if (has_right()) {
				return children().back();
			} else {
				return (size_t)-1;
			}
		}
		void clear() noexcept {
			m_children.clear();
			parent(-1);
		}
	};
	typedef node node_type;

	std::vector<node_type> m_nodes;
	std::unordered_set<size_t> m_removed;
	size_t m_root;

	dynamic_tree() : m_nodes(), m_removed(), m_root(-1) {}

	size_t size() const noexcept {
		return m_nodes.size();
	}
	bool contains(size_t n) const noexcept {
		return n < size() and not m_removed.contains(n);
	}
	size_t root() const noexcept {
		return m_root;
	}
	node_type& child(size_t n, size_t c) {
		return node(node(n).children().at(c));
	}
	node_type& child(const node_type& n, size_t c) {
		return child(node(n), c);
	}
	const node_type& child(size_t n, size_t c) const {
		return node(n).children().at(c);
	}
	const node_type& child(const node_type& n, size_t c) const {
		return child(node(n), c);
	}
	size_t rotate_left(node_type& n) {
		return rotate_left(node(n));
	}
	size_t rotate_left(size_t i) {
		auto&& n = node(i);
		auto&& right = child(n, 1);
		if (n.children().size() != 2 or right.children().size() != 2) {
			return (size_t)-1;
		}
		auto&& rightleft = child(right, 0);

		rightleft.parent(i);
		n.children()[1] = node(rightleft);

		if (is_root(i)) {
			m_root = node(right);
			right.parent(-1);
		} else {
			size_t p = n.parent();
			auto&& c = node(p).children();
			*std::find(c.begin(), c.end(), i) = node(right);
			right.parent(p);
		}

		right.children()[0] = i;
		n.parent(node(right));

		return node(right);
	}
	size_t rotate_right(size_t i) {
		if (not contains(i)) {
			return (size_t)-1;
		}
		auto&& n = node(i);
		auto&& left = child(n, 0);
		if (n.children().size() != 2 or left.children().size() != 2) {
			return (size_t)-1;
		}
		auto&& leftright = child(left, 1);

		leftright.parent(i);
		n.children()[0] = node(leftright);

		if (is_root(i)) {
			m_root = node(left);
			left.parent(-1);
		} else {
			size_t p = n.parent();
			auto&& c = node(p).children();
			*std::find(c.begin(), c.end(), i) = node(left);
			left.parent(p);
		}

		left.children()[1] = i;
		n.parent(node(left));

		return node(left);
	}
	// contracts i to its parent; children of i are inserted in place of i
	size_t contract(size_t i) {
		if (not contains(i)) {
			return (size_t)-1;
		}
		auto&& n = node(i);
		if (n.no_parent()) {
			return (size_t)-1;
		}
		size_t p = n.parent();
		auto&& parent = node(p);
		auto&& c = n.children();
		for (size_t ci : c) {
			node(ci).parent(p);
		}
		auto&& cp = parent.children();
		size_t sz = cp.size();
		if (c.size() > 0) {
			cp.resize(cp.size() + c.size());
		}
		auto end = cp.begin() + sz;
		auto ins = std::find(cp.begin(), end, i);
		replace_subrange(ins, ins + 1, end, c.begin(), c.end());
		cp.resize(cp.size() - 1);
		cp.parent(-1);
		remove(i);
		return p;
	}
	size_t replace_parent(size_t i) {
		if (not contains(i)) {
			return (size_t)-1;
		}
		auto&& n = node(i);
		if (n.no_parent()) {
			return (size_t)-1;
		}
		size_t p = n.parent();
		auto&& parent = node(p);
		if (is_root(parent)) {
			n.parent(-1);
			m_root = i;
		} else {
			n.parent(parent.parent());
			auto&& gpc = node(parent.parent()).children();
			std::replace(gpc.begin(), gpc.end(), p, i);
		}
		parent.parent(-1);
		remove(p);
		return i;
	}
	template <std::invocable<size_t> Inorder, std::invocable<size_t> Preorder, std::invocable<size_t> Postorder>
	bool traverse_iterative(size_t n, Inorder inorder, Preorder preorder, Postorder postorder) const {
		if (not contains(n)) {
			return false;
		}
		enum Stage {
			PREORDER,
			INORDER,
			POSTORDER
		};
		struct Locals {
			Stage stage;
			size_t n;
		};
		stack<Locals> stack;
		stack.push(Locals{PREORDER, POSTORDER});
		while (not stack.empty()) {
			auto&& [stage, n] = stack.peek();
			switch (stage) {
			case PREORDER:
				if constexpr (std::predicate<Preorder, size_t>) {
					if (not preorder(n)) {
						return false;
					}
				} else {
					preorder(n);
				}
				for (size_t c : node(n).children() | std::views::take(node(n).children().size() / 2) | std::views::reverse) {
					stack.push(Locals{PREORDER, c});
				}
				stage = INORDER;
				continue;
			case INORDER:
				if constexpr (std::predicate<Inorder, size_t>) {
					if (not inorder(n)) {
						return false;
					}
				} else {
					inorder(n);
				}
				for (size_t c : node(n).children() | std::views::drop(node(n).children().size() / 2) | std::views::reverse) {
					stack.push(Locals{PREORDER, c});
				}
				stage = POSTORDER;
				continue;
			case POSTORDER:
				if constexpr (std::predicate<Postorder, size_t>) {
					if (not postorder(n)) {
						return false;
					}
				} else {
					postorder(n);
				}
				stack.pop();
				continue;
			}
		}
		return true;
	}
	template <std::invocable<size_t> Inorder, std::invocable<size_t> Preorder, std::invocable<size_t> Postorder>
	bool traverse_recursive(size_t n, size_t d, Inorder inorder, Preorder preorder, Postorder postorder) const {
		static constexpr size_t MAX_STACK_USAGE = (1 << 12);
		static constexpr size_t STACK_USAGE =
			3 * sizeof(void*) + 3 * sizeof(size_t) + 2 * sizeof(std::vector<size_t>::const_iterator);
		static constexpr size_t RECURSION_LIMIT = MAX_STACK_USAGE / STACK_USAGE;
		if (not contains(n)) {
			return false;
		}
		if (d > RECURSION_LIMIT) {
			return traverse_iterative(n, std::move(inorder), std::move(preorder), std::move(postorder));
		}
		if constexpr (std::predicate<Preorder, size_t>) {
			if (not preorder(n)) {
				return false;
			}
		} else {
			preorder(n);
		}
		auto&& c = node(n).children();
		size_t sz = c.size();
		auto begin = c.begin(), end = begin + sz / 2;
		for (; begin != end; ++begin) {
			if (not traverse_recursive(*begin, d + 1, inorder, preorder, postorder)) {
				return false;
			}
		}
		if constexpr (std::predicate<Inorder, size_t>) {
			if (not inorder(n)) {
				return false;
			}
		} else {
			inorder(n);
		}
		end = c.end();
		for (; begin != end; ++begin) {
			if (not traverse_recursive(*begin, d + 1, inorder, preorder, postorder)) {
				return false;
			}
		}
		if constexpr (std::predicate<Postorder, size_t>) {
			if (not postorder(n)) {
				return false;
			}
		} else {
			postorder(n);
		}
		return true;
	}
	template <std::invocable<size_t> Inorder, std::invocable<size_t> Preorder, std::invocable<size_t> Postorder>
	bool traverse(size_t n, Inorder inorder, Preorder preorder, Postorder postorder) const {
		return traverse_recursive(n, 0, std::move(inorder), std::move(preorder), std::move(postorder));
	}
	template <std::invocable<size_t> Inorder, std::invocable<size_t> Preorder, std::invocable<size_t> Postorder>
	bool traverse(Inorder inorder, Preorder preorder, Postorder postorder) const {
		return traverse(root(), std::move(inorder), std::move(preorder), std::move(postorder));
	}
	static constexpr auto default_traverse() {
		return [](size_t){};
	}
	template <std::invocable<size_t> Visitor>
	bool traverse_preorder(size_t n, Visitor v) const {
		return traverse(n, default_traverse(), std::move(v), default_traverse());
	}
	template <std::invocable<size_t> Visitor>
	bool traverse_inorder(size_t n, Visitor v) const {
		return traverse(n, std::move(v), default_traverse(), default_traverse());
	}
	template <std::invocable<size_t> Visitor>
	bool traverse_postorder(size_t n, Visitor v) const {
		return traverse(n, default_traverse(), default_traverse(), std::move(v));
	}
	template <std::invocable<size_t> Visitor>
	bool traverse_preorder(Visitor v) const {
		return traverse(default_traverse(), std::move(v), default_traverse());
	}
	template <std::invocable<size_t> Visitor>
	bool traverse_inorder(Visitor v) const {
		return traverse(std::move(v), default_traverse(), default_traverse());
	}
	template <std::invocable<size_t> Visitor>
	bool traverse_postorder(Visitor v) const {
		return traverse(default_traverse(), default_traverse(), std::move(v));
	}
	template <std::invocable<size_t> Visitor>
	bool traverse_levelwise(Visitor v) const {
		return traverse_levelwise(m_root, std::move(v));
	}
	template <std::invocable<size_t> Visitor>
	bool traverse_levelwise(size_t n, Visitor v) const {
		if (not contains(n)) {
			return false;
		}
		queue<size_t> queue;
		queue.reserve(size());
		queue.push(n);
		while (not queue.empty()) {
			size_t r = queue.pop();
			if constexpr (std::predicate<Visitor, size_t>) {
				if (not v(r)) {
					return false;
				}
			} else {
				v(r);
			}
			for (size_t c : node(r).children()) {
				queue.push(c);
			}
		}
		return true;
	}
	size_t subtree_size(const node_type& n) const {
		return subtree_size(node(n));
	}
	size_t subtree_size(size_t n) const {
		size_t c = 0;
		traverse_preorder(n, [&c] (size_t) {
			++c;
		});
		return c;
	}
	dynamic_tree subtree(const node_type& n) const {
		return subtree(node(n));
	}
	dynamic_tree subtree(size_t n) const {
		if (not contains(n)) {
			return *this;
		}
		dynamic_tree result;
		std::unordered_map<size_t, size_t> index_map;
		index_map.reserve(subtree_size(n));
		traverse_preorder(n, [&] (size_t i) {
			auto&& n = node(i);
			index_map.emplace(i,
					is_root(i)
					? result.push_root(*n)
					: result.push(index_map.at(n.parent()), *n));
		});
		return result;
	}
	template <typename...Args>
	size_t push(const node_type& p, Args&&...args) {
		return insert(p, p.children().size(), std::forward<Args>(args)...);
	}
	template <typename...Args>
	size_t push(size_t p, Args&&...args) {
		return push(node(p), std::forward<Args>(args)...);
	}
	template <typename...Args>
	size_t prepend(const node_type& p, Args&&...args) {
		return prepend(node(p), std::forward<Args>(args)...);
	}
	template <typename...Args>
	size_t prepend(size_t p, Args&&...args) {
		return insert(p, 0, std::forward<Args>(args)...);
	}
	template <typename...Args>
	size_t insert(const node_type& p, size_t i, Args&&...args) {
		return insert(node(p), i, std::forward<Args>(args)...);
	}
	template <typename...Args>
	size_t insert(size_t p, size_t i, Args&&...args) {
		if (not contains(p)) {
			return (size_t)-1;
		}
		auto&& parent = node(p);
		auto&& c = parent.children();
		if (i > c.size()) {
			return (size_t)-1;
		}
		size_t r = next_node(std::forward<Args>(args)...);
		node(r).parent(p);
		c.insert(c.begin() + i, r);
		return r;
	}
	// allocates an unlinked node
	template <typename...Args>
	size_t next_node(Args&&...args) {
		size_t r;
		if (m_removed.size() > 0) {
			auto it = m_removed.begin();
			r = *it;
			auto alloc = m_nodes.get_allocator();
			m_nodes[r] = node_type(std::forward<Args>(args)...);
			m_removed.erase(it);
		} else {
			r = m_nodes.size();
			m_nodes.emplace_back(std::forward<Args>(args)...);
		}
		return r;
	}
	// undefined behaviour if np is in the subtree s
	dynamic_tree& move_subtree(size_t s, size_t np, size_t loc) {
		if (not contains(s) or not contains(np)) {
			return *this;
		}
		auto&& new_parent = node(np);
		auto&& c = new_parent.children();
		auto&& sub = node(s);
		size_t sp = sub.parent();
		auto&& sub_parent = node(sp);
		if (loc > c.size()) {
			return *this;
		}
		c.insert(c.begin() + loc, s);
		std::erase(sub_parent.children(), s);
		sub.parent(np);
		return *this;
	}
	template <typename...Args>
	size_t expand_upwards(size_t i, Args&&...args) {
		if (not contains(i)) {
			return (size_t)-1;
		}
		auto&& n = node(i);
		size_t r = next_node(std::forward<Args>(args)...);
		node(r).children().push_back(i);
		n.parent(r);
		if (is_root(i)) {
			m_root = r;
			return r;
		}
		size_t p = n.parent();
		auto&& parent = node(p);
		auto&& c = parent.children();
		node(r).parent(p);
		*std::find(c.begin(), c.end(), i) = r;
		return r;
	}
	template <typename...Args>
	dynamic_tree& join_with(size_t i, size_t j, Args&&...args) {
		if (not contains(i) or not (contains(j))) {
			return *this;
		}
		size_t r = expand_upwards(i, std::forward<Args>(args)...);
		move_subtree(j, r, 1);
		return *this;
	}
	template <typename...Args>
	size_t join_with(const dynamic_tree& o, Args&&...args) {
		return join_with(root(), o, std::forward<Args>(args)...);
	}
	template <typename...Args>
	size_t join_with(size_t i, const dynamic_tree& o, Args&&...args) {
		if (not contains(i)) {
			return (size_t)-1;
		}
		reserve(size() + 1 + o.size());
		size_t r = expand_upwards(i, std::forward<Args>(args)...);
		std::unordered_map<size_t, size_t> index_map;
		index_map.reserve(o.size());
		o.traverse_preorder([&](size_t i) {
			auto&& n = o.node(i);
			if (o.is_root(i)) {
				index_map.emplace(i, push(r, *n));
			} else {
				index_map.emplace(i, push(index_map.at(n.parent()), *n));
			}
		});
		return r;
	}
	template <typename...Args>
	size_t push_root(Args&&...args) {
		auto e = empty();
		size_t r = next_node(std::forward<Args>(args)...);
		if (not e) {
			node(r).children().push_back(root());
			node(root()).parent(r);
		}
		m_root = r;
		return r;
	}
	node_type& node(size_t n) noexcept(noexcept(node_type())) {
		static node_type DUMMY = node_type();
		if (contains(n)) {
			return m_nodes[n];
		} else {
			return DUMMY;
		}
	}
	const node_type& node(size_t n) const noexcept(noexcept(node_type())) {
		static const node_type DUMMY = node_type();
		if (contains(n)) {
			return m_nodes[n];
		} else {
			return DUMMY;
		}
	}
	size_t node(const node_type& n) const noexcept {
		return &n - m_nodes.data();
	}
	void remove(const node_type& n) {
		return remove(node(n));
	}
	bool is_root(size_t n) const noexcept {
		return n == m_root;
	}
	bool is_root(const node_type& n) const noexcept {
		return is_root(node(n));
	}
	void remove(size_t n) {
		if (not contains(n)) {
			return;
		}
		if (is_root(node(n))) {
			return clear();
		}
		if (not node(n).no_parent()) {
			auto&& pc = node(node(n).parent()).children();
			if (auto it = std::find(pc.begin(), pc.end(), n); it != pc.end()) {
				pc.erase(it);
			}
		}
		queue<size_t> queue;
		queue.push(n);
		while (not queue.empty()) {
			n = queue.pop();
			for (size_t c : node(n).children()) {
				queue.push(c);
			}
			if (n == m_nodes.size() - 1) {
				m_nodes.pop_back();
				typename decltype(m_removed)::iterator it;
				while ((it = m_removed.find(--n)) != m_removed.end()) {
					m_removed.erase(it);
				}
			} else {
				m_nodes[n] = node_type();
				m_removed.insert(n);
			}
		}
	}
	void clear() noexcept {
		m_nodes.clear();
		m_removed.clear();
	}
	void reserve(size_t n) {
		if (n <= m_nodes.size()) {
			return;
		}
		m_nodes.reserve(n);
	}
	bool empty() const noexcept {
		return size() == 0;
	}
	template <std::invocable<size_t> Pred>
	bool traverse_out_of_order(Pred pred) const noexcept(noexcept(pred(size_t()))) {
		auto not_removed = [&](const node_type& n) -> bool { return not m_removed.contains(node(n)); };
		for (const node_type& n : m_nodes | std::views::filter(not_removed)) {
			if constexpr (std::predicate<Pred, size_t>) {
				if (not pred(node(n))) {
					return false;
				}
			} else {
				pred(node(n));
			}
		}
		return true;
	}

	template <std::invocable<size_t> Printer>
	decltype(std::declval<Printer>()(std::declval<node_type>())) to_string(Printer printer) const {
		return to_string(std::move(printer), '|');
	}
	template <std::invocable<size_t> Printer, typename CharT>
	decltype(std::declval<Printer>()(size_t())) to_string(Printer printer, CharT delim) const {
		typedef decltype(std::declval<Printer>()(size_t())) string_type;
		size_t depth = 0;
		string_type s;
		auto preorder = [&](size_t n) {
			for (size_t i = depth; i != 0; --i) {
				s += delim;
			}
			s += printer(n);
			s += '\n';
			++depth;
		};
		auto postorder = [&] (size_t) {
			--depth;
		};
		auto inorder = [](size_t){};
		traverse(std::move(inorder), std::move(preorder), std::move(postorder));
		return s;
	}
}; // struct dynamic_tree

} // namespace automata

#endif // #ifndef AUTOMATA_DYNAMIC_TREE
