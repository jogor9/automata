#ifndef AUTOMATA_STACK_HPP
#define AUTOMATA_STACK_HPP

#include <memory>
#include <bit>

namespace automata {

template <typename T, typename Alloc = std::allocator<T>>
struct stack : protected Alloc {
	typedef Alloc allocator_type;
	typedef T value_type;
	typedef value_type& reference;
	typedef const value_type& const_reference;
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef typename std::allocator_traits<allocator_type>::pointer pointer;
	typedef typename std::allocator_traits<allocator_type>::const_pointer const_pointer;
	typedef pointer iterator;
	typedef const_pointer const_iterator;
	typedef std::reverse_iterator<iterator> reverse_iterator;
	typedef std::reverse_iterator<const_iterator> reverse_const_iterator;
protected:
	pointer m_block;
	pointer m_data;
	pointer m_bottom;

	static constexpr bool ALLOC_NOTHROW = noexcept(std::declval<allocator_type>().allocate(size_type()));
	static constexpr bool DEFAULT_NOTHROW = noexcept(value_type());
	static constexpr bool COPY_NOTHROW = noexcept(value_type(std::declval<const value_type&>()));
	static constexpr bool MOVE_NOTHROW = noexcept(value_type(std::declval<value_type&&>()));
public:

	constexpr stack() noexcept : allocator_type(), m_block(), m_data(), m_bottom() {}
	stack(size_type ucap) noexcept(ALLOC_NOTHROW) : stack() {
		size_type cap = std::bit_ceil(ucap);
		m_block = allocator_type::allocate(cap);
		if (m_block) {
			m_data = m_block + cap;
			m_bottom = m_data;
		}
	}
	allocator_type& allocator() noexcept {
		return *this;
	}
	const allocator_type& allocator() const noexcept {
		return *this;
	}
protected:
	void copy_construct(const stack& o) noexcept(ALLOC_NOTHROW and COPY_NOTHROW) {
		size_type cap = std::bit_ceil(o.size());
		pointer new_block = allocator_type::allocate(cap);
		if (new_block) {
			pointer new_bottom = new_block + cap;
			pointer new_data = new_bottom - o.size();
			std::uninitialized_copy(o.begin(), o.end(), new_data);
			m_block = new_block;
			m_bottom = new_bottom;
			m_data = new_data;
		}
	}

	void move_construct(stack&& o) noexcept {
		std::swap(m_block, o.m_block);
		std::swap(m_data, o.m_data);
		std::swap(m_bottom, o.m_bottom);
	}
public:
	stack(const stack& o) noexcept(ALLOC_NOTHROW and COPY_NOTHROW) : stack() {
		copy_construct(o);
	}

	stack(stack&& o) noexcept : stack() {
		move_construct(std::move(o));
	}

	void destroy() noexcept {
		if (m_block) {
			std::destroy(m_data, m_bottom);
			allocator_type::deallocate(m_block, capacity());
			m_block = pointer();
			m_data = pointer();
			m_bottom = pointer();
		}
	}

	stack& operator=(const stack& o) noexcept(ALLOC_NOTHROW and COPY_NOTHROW) {
		if (&o == this) {
			return *this;
		}
		stack t(o);
		if (not t.empty()) { // strong exception guarantee
			*this = std::move(t);
		}
		return *this;
	}

	stack& operator=(stack&& o) noexcept {
		if (&o == this) {
			return *this;
		}
		destroy();
		move_construct(std::move(o));
		return *this;
	}

	~stack() noexcept {
		destroy();
	}

	bool empty() const noexcept {
		return size() == 0;
	}
	size_t size() const noexcept {
		return m_bottom - m_data;
	}

	size_t capacity() const noexcept {
		return m_bottom - m_block;
	}

	pointer data() noexcept {
		return m_data;
	}
	const_pointer data() const noexcept {
		return m_data;
	}
	pointer bottom() noexcept {
		return m_bottom;
	}
	const_pointer bottom() const noexcept {
		return m_bottom;
	}

	stack& reserve(size_type new_ucap) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW) {
		if (new_ucap <= capacity()) {
			return *this;
		}
		size_type new_cap = std::bit_ceil(new_ucap);
		stack n(new_cap);
		if (not n.empty()) { // strong exception guarantee
			n.m_data = n.m_bottom - size();
			std::uninitialized_move(m_data, m_bottom, n.m_data);
			*this = std::move(n);
		}
		return *this;
	}
	stack& resize(size_type new_size) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW and DEFAULT_NOTHROW) {
		if (new_size <= size()) {
			std::destroy(m_data, m_bottom - new_size);
			m_data = m_bottom - new_size;
			return *this;
		}
		if (new_size > capacity() and new_size > reserve(new_size).capacity()) {
			return *this;
		}
		pointer new_data = bottom() - new_size;
		std::uninitialized_default_construct(new_data, m_data);
		m_data = new_data;
		return *this;
	}
	stack& resize(size_type new_size, const_reference fill) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW and COPY_NOTHROW) {
		if (new_size <= size()) {
			std::destroy(m_data, m_bottom - new_size);
			m_data = m_bottom - new_size;
			return *this;
		}
		if (new_size > capacity() and new_size > reserve(new_size).capacity()) {
			return *this;
		}
		pointer new_data = bottom() - new_size;
		std::uninitialized_fill(new_data, m_data, fill);
		m_data = new_data;
		return *this;
	}
	stack& clear() noexcept {
		return resize(0);
	}
	reference peek() noexcept {
		if (empty()) {
			return *dummy_address();
		} else {
			return *m_data;
		}
	}
	const_reference peek() const noexcept {
		if (empty()) {
			return *dummy_address();
		} else {
			return *data();
		}
	}
	pointer top() noexcept {
		return data();
	}
	const_pointer top() const noexcept {
		return data();
	}
	template <typename...Args>
		requires std::constructible_from<value_type, Args...>
	stack& push(Args&&...args) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW and noexcept(value_type(std::forward<Args>(args)...))) {
		if (size() == capacity() and size() == reserve(size() + 1).capacity()) {
			return *this;
		}
		pointer new_data = m_data - 1;
		std::allocator_traits<allocator_type>::construct(*this, new_data, std::forward<Args>(args)...);
		m_data = new_data;
		return *this;
	}
	value_type pop() noexcept(MOVE_NOTHROW and DEFAULT_NOTHROW) {
		if (empty()) {
			return value_type();
		}
		value_type s = std::move(peek());
		m_data->~value_type();
		++m_data;
		return s;
	}
	template <std::input_iterator It>
	stack& push(It begin, It end) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW and noexcept(value_type(std::forward<decltype(*begin)>(*begin)))) {
		if constexpr (std::forward_iterator<It>) {
			auto dist = std::distance(begin, end);
			if (size() + dist > capacity() and size() + dist > reserve(size() + dist).capacity()) {
				return *this;
			}
		}
		for (; begin != end; ++begin) {
			push(std::forward<decltype(*begin)>(*begin));
		}
		return *this;
	}

	static auto dummy_address() noexcept(DEFAULT_NOTHROW) {
		static value_type DUMMY = value_type();
		return std::addressof(DUMMY);
	}

	template <std::integral Int>
	reference at(Int i) noexcept(DEFAULT_NOTHROW) {
		if (i < Int(0)) {
			if (size_type(-i) > size()) {
				return *dummy_address();
			}
			return bottom()[i];
		} else {
			if (size_type(i) >= size()) {
				return *dummy_address();
			}
			return data()[i];
		}
	}
	template <std::integral Int>
	const_reference at(Int i) const noexcept(DEFAULT_NOTHROW) {
		if (i < Int(0)) {
			if (size_type(-i) > size()) {
				return *dummy_address();
			}
			return bottom()[i];
		} else {
			if (size_type(i) >= size()) {
				return *dummy_address();
			}
			return data()[i];
		}
	}
	template <std::integral Int>
	reference operator[](Int i) noexcept(DEFAULT_NOTHROW) {
		return at(i);
	}
	template <std::integral Int>
	const_reference operator[](Int i) const noexcept(DEFAULT_NOTHROW) {
		return at(i);
	}
	iterator begin() noexcept {
		return data();
	}
	iterator end() noexcept {
		return bottom();
	}
	const_iterator begin() const noexcept {
		return data();
	}
	const_iterator end() const noexcept {
		return bottom();
	}
	const_iterator cbegin() const noexcept {
		return data();
	}
	const_iterator cend() const noexcept {
		return bottom();
	}
	reverse_iterator rbegin() noexcept {
		return reverse_iterator(end());
	}
	reverse_iterator rend() noexcept {
		return reverse_iterator(begin());
	}
	reverse_const_iterator rbegin() const noexcept {
		return reverse_iterator(end());
	}
	reverse_const_iterator rend() const noexcept {
		return reverse_iterator(begin());
	}
	reverse_const_iterator crbegin() const noexcept {
		return reverse_iterator(end());
	}
	reverse_const_iterator crend() const noexcept {
		return reverse_iterator(begin());
	}
}; // struct stack

} // namespace automata

#endif // #ifndef AUTOMATA_STACK_HPP
