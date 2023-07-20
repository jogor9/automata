#ifndef AUTOMATA_BLOCK_HPP
#define AUTOMATA_BLOCK_HPP

#include <memory>

namespace automata {

template <typename T, typename Alloc = std::allocator<T>>
struct block : Alloc {
	T* m_data;
	size_t m_len;

	block() : m_data(nullptr), m_len(0) {}

	block(size_t m_len) : m_data(Alloc::allocate(m_len)), m_len(m_len) {
		if (valid()) {
			std::uninitialized_default_construct(data(), data() + length());
		} else {
			m_len = 0;
		}
	}

	template <std::forward_iterator It>
	block(It begin, It end) : block(std::distance(begin, end)) {
		if (valid()) {
			std::copy(begin, end, data());
		}
	}

	block(size_t m_len, const T& t) : block(m_len) {
		if (valid()) {
			std::fill(data(), data() + length(), t);
		}
	}

	block(const block& o) : block(o.m_len) {
		if (valid()) {
			std::copy(o.begin(), o.end(), begin());
		}
	}

	block(block&& o) noexcept : m_data(o.data()), m_len(o.length()) {
		o.m_data = nullptr;
		o.m_len = 0;
	}

	block& operator=(const block& o) {
		if (&o == this) {
			return *this;
		}
		clear();
		if (o.valid()) {
			m_data = Alloc::allocate(o.length());
			if (valid()) {
				m_len = o.length();
				std::uninitialized_copy(o.data(), o.data() + o.length(), data());
			}
		}
		return *this;
	}
	block& operator=(block&& o) noexcept {
		if (&o == this) {
			return *this;
		}
		clear();
		std::swap(m_len, o.m_len);
		std::swap(m_data, o.m_data);
		return *this;
	};

	~block() noexcept {
		clear();
	}

	block& clear() noexcept {
		if (valid()) {
			std::destroy(data(), data() + length());
			Alloc::deallocate(m_data);
			m_data = nullptr;
		}
	}

	block& resize(size_t new_sz) {
		if (new_sz <= length()) {
			return *this;
		}
		T* new_data = Alloc::allocate(new_sz);
		if (new_data) {
			std::uninitialized_move(data(), data() + length(), new_data);
			std::uninitialized_default_construct(new_data + length(), new_data + new_sz);
			clear();
			m_data = new_data;
			m_len = new_sz;
		}
		return *this;
	}

	block& extend(size_t n) {
		return resize(length() + n);
	}

	size_t length() const noexcept {
		return m_len;
	}

	size_t size() const noexcept {
		return length();
	}

	bool valid() const noexcept {
		return m_data != nullptr;
	}

	operator bool() const noexcept {
		return valid();
	}

	T& operator[](size_t i) noexcept {
		return data()[i];
	}

	const T& operator[](size_t i) const noexcept {
		return data()[i];
	}


	T* data() noexcept { return m_data; }
	const T* data() const noexcept { return m_data; }

	T* begin() noexcept { return data(); }
	const T* begin() const noexcept { return data(); }
	T* end() noexcept { return begin() + length(); }
	const T* end() const noexcept { return begin() + length(); }

};

} // namespace automata

#endif // #ifndef AUTOMATA_BLOCK_HPP
