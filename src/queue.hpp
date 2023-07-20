#ifndef AUTOMATA_QUEUE_HPP
#define AUTOMATA_QUEUE_HPP

#include <memory>
#include <bit>
#include "util2.hpp"

namespace automata {

template <typename Queue>
struct queue_iterator {
	using queue = Queue;
	typedef typename queue::value_type value_type;
	typedef typename queue::reference reference;
	typedef typename queue::const_reference const_reference;
	typedef typename queue::size_type size_type;
	typedef typename queue::difference_type difference_type;
	typedef typename queue::pointer pointer;
	typedef typename queue::const_pointer const_pointer;

	friend queue;
protected:
	static constexpr uintptr_t OWNER_MASK = pow2mask(bit_count<uintptr_t>() - 1);
	static constexpr uintptr_t BEGIN_MASK = ~OWNER_MASK;
	uintptr_t m_owner_begin;
	pointer p;

	queue* owner() noexcept {
		return (queue*)(m_owner_begin & OWNER_MASK);
	}
	void owner(queue* q) noexcept {
		m_owner_begin = (m_owner_begin & BEGIN_MASK) | ((uintptr_t)q & OWNER_MASK);
	}
	bool begin() noexcept {
		return (bool)(m_owner_begin & BEGIN_MASK);
	}
	void begin(bool b) noexcept {
		if (b) {
			m_owner_begin |= BEGIN_MASK;
		} else {
			m_owner_begin &= ~BEGIN_MASK;
		}
	}

	queue_iterator(queue* q) noexcept : m_owner_begin(), p() { owner(q); }
	queue_iterator(queue* q, pointer p) noexcept : m_owner_begin(), p(p) { owner(q); }

public:
	queue_iterator& operator++() noexcept {
		if (p) {
			pointer r = owner()->pointer_advance(p, 1);
			if (r == owner()->past_the_end_pointer()) {
				p = pointer();
			} else {
				p = r;
			}
		} else if (begin()) {
			p = owner()->m_begin;
			begin(false);
		}
		return *this;
	}
	queue_iterator& operator--() noexcept {
		if (p) {
			if (p == owner()->m_begin) {
				p = pointer();
				begin(true);
			} else {
				p = owner()->pointer_advance(p, -1);
			}
		} else if (not begin()) {
			p = owner()->pointer_advance(owner()->past_the_end_pointer(), -1);
		}
		return *this;
	}
	queue_iterator operator++(int) noexcept {
		auto t = *this;
		operator++();
		return t;
	}
	queue_iterator operator--(int) noexcept {
		auto t = *this;
		operator--();
		return t;
	}
	template <std::integral Int>
	queue_iterator& operator+=(Int i) noexcept {
		if (i == 0) {
			return *this;
		}
		if (i < 0) {
			return operator-(-i);
		}
		if (p) {
			if (owner()->pointer_index(p) + i < owner()->size()) {
				p = owner()->pointer_advance(p, i);
			} else {
				p = pointer();
			}
		} else if (begin()) {
			operator++();
			return operator+=(i - 1);
		}
		return *this;
	}
	template <std::integral Int>
	queue_iterator& operator-=(Int i) noexcept {
		if (i == 0) {
			return *this;
		}
		if (i < 0) {
			return operator+(-i);
		}
		if (p) {
			if (owner()->pointer_index(p) - i > 0) {
				p = owner()->pointer_advance(p, -i);
			} else {
				p = pointer();
				begin(true);
			}
		} else if (not begin()) {
			operator--();
			return operator-=(i - 1);
		}
		return *this;
	}
	template <std::integral Int>
	queue_iterator operator+(Int i) noexcept {
		auto r = *this;
		r += i;
		return r;
	}
	template <std::integral Int>
	queue_iterator operator-(Int i) noexcept {
		auto r = *this;
		r -= i;
		return r;
	}
	template <std::integral Int>
	reference operator[](Int i) noexcept {
		return *(*this + i);
	}

	reference operator*() noexcept {
		return *p;
	}
	pointer operator->() noexcept {
		return p;
	}

	template <typename Q>
	friend bool operator==(queue_iterator<Q> lhs, queue_iterator<Q> rhs) noexcept;
	template <typename Q>
	friend typename queue_iterator<Q>::difference_type operator-(queue_iterator<Q> lhs, queue_iterator<Q> rhs) noexcept;
};

template <typename Q>
bool operator==(queue_iterator<Q> lhs, queue_iterator<Q> rhs) noexcept {
	return lhs.p == rhs.p and lhs.begin() == rhs.begin();
}
template <typename Q>
typename queue_iterator<Q>::difference_type operator-(queue_iterator<Q> lhs, queue_iterator<Q> rhs) noexcept {
	typedef typename queue_iterator<Q>::difference_type diff_type;
	diff_type li = lhs.p ? lhs.owner()->pointer_index(lhs.p) : (lhs.begin() ? -1 : lhs.owner()->size());
	diff_type ri = rhs.p ? rhs.owner()->pointer_index(rhs.p) : (rhs.begin() ? -1 : rhs.owner()->size());
	return li - ri;
}
template <typename Q>
auto operator<=>(queue_iterator<Q> lhs, queue_iterator<Q> rhs) noexcept {
	return (lhs - rhs) <=> (rhs - lhs);
}


template <typename Queue>
struct queue_const_iterator {
	using queue = Queue;
	typedef typename queue::value_type value_type;
	typedef typename queue::const_reference reference;
	typedef typename queue::const_reference const_reference;
	typedef typename queue::size_type size_type;
	typedef typename queue::difference_type difference_type;
	typedef typename queue::const_pointer pointer;
	typedef typename queue::const_pointer const_pointer;

	friend Queue;
protected:
	static constexpr uintptr_t OWNER_MASK = pow2mask(bit_count<uintptr_t>() - 1);
	static constexpr uintptr_t BEGIN_MASK = ~OWNER_MASK;
	uintptr_t m_owner_begin;
	pointer p;

	const queue* owner() noexcept {
		return (const queue*)(m_owner_begin & OWNER_MASK);
	}
	void owner(const queue* q) noexcept {
		m_owner_begin = (m_owner_begin & BEGIN_MASK) | ((uintptr_t)q & OWNER_MASK);
	}
	bool begin() noexcept {
		return (bool)(m_owner_begin & BEGIN_MASK);
	}
	void begin(bool b) noexcept {
		if (b) {
			m_owner_begin |= BEGIN_MASK;
		} else {
			m_owner_begin &= ~BEGIN_MASK;
		}
	}

	queue_const_iterator(const queue* q) noexcept : m_owner_begin(), p() { owner(q); }
	queue_const_iterator(const queue* q, pointer p) noexcept : m_owner_begin(), p(p) { owner(q); }

public:
	queue_const_iterator& operator++() noexcept {
		if (p) {
			pointer r = owner()->pointer_advance(p, 1);
			if (r == owner()->past_the_end_pointer()) {
				p = pointer();
			} else {
				p = r;
			}
		} else if (begin()) {
			p = owner()->m_begin;
			begin(false);
		}
		return *this;
	}
	queue_const_iterator& operator--() noexcept {
		if (p) {
			if (p == owner()->m_begin) {
				p = pointer();
				begin(true);
			} else {
				p = owner()->pointer_advance(p, -1);
			}
		} else if (not begin()) {
			p = owner()->pointer_advance(owner()->past_the_end_pointer(), -1);
		}
		return *this;
	}
	queue_const_iterator operator++(int) noexcept {
		auto t = *this;
		operator++();
		return t;
	}
	queue_const_iterator operator--(int) noexcept {
		auto t = *this;
		operator--();
		return t;
	}
	template <std::integral Int>
	queue_const_iterator& operator+=(Int i) noexcept {
		if (i == 0) {
			return *this;
		}
		if (i < 0) {
			return operator-(-i);
		}
		if (p) {
			if (owner()->pointer_index(p) + i < owner()->size()) {
				p = owner()->pointer_advance(p, i);
			} else {
				p = pointer();
			}
		} else if (begin()) {
			operator++();
			return operator+=(i - 1);
		}
		return *this;
	}
	template <std::integral Int>
	queue_const_iterator& operator-=(Int i) noexcept {
		if (i == 0) {
			return *this;
		}
		if (i < 0) {
			return operator+(-i);
		}
		if (p) {
			if (owner()->pointer_index(p) - i > 0) {
				p = owner()->pointer_advance(p, -i);
			} else {
				p = pointer();
				begin(true);
			}
		} else if (not begin()) {
			operator--();
			return operator-=(i - 1);
		}
		return *this;
	}
	template <std::integral Int>
	queue_const_iterator operator+(Int i) noexcept {
		auto r = *this;
		r += i;
		return r;
	}
	template <std::integral Int>
	queue_const_iterator operator-(Int i) noexcept {
		auto r = *this;
		r -= i;
		return r;
	}
	template <std::integral Int>
	reference operator[](Int i) noexcept {
		return *(*this + i);
	}

	reference operator*() noexcept {
		return *p;
	}
	pointer operator->() noexcept {
		return p;
	}

	template <typename Q>
	friend bool operator==(queue_const_iterator<Q> lhs, queue_const_iterator<Q> rhs) noexcept;
	template <typename Q>
	friend typename queue_const_iterator<Q>::difference_type operator-(queue_const_iterator<Q> lhs, queue_const_iterator<Q> rhs) noexcept;
};

template <typename Q>
bool operator==(queue_const_iterator<Q> lhs, queue_const_iterator<Q> rhs) noexcept {
	return lhs.p == rhs.p and lhs.begin() == rhs.begin();
}
template <typename Q>
typename queue_const_iterator<Q>::difference_type operator-(queue_const_iterator<Q> lhs, queue_const_iterator<Q> rhs) noexcept {
	typedef typename queue_const_iterator<Q>::difference_type diff_type;
	diff_type li = lhs.p ? lhs.owner()->pointer_index(lhs.p) : (lhs.begin() ? -1 : lhs.owner()->size());
	diff_type ri = rhs.p ? rhs.owner()->pointer_index(rhs.p) : (rhs.begin() ? -1 : rhs.owner()->size());
	return li - ri;
}
template <typename Q>
auto operator<=>(queue_const_iterator<Q> lhs, queue_const_iterator<Q> rhs) noexcept {
	return (lhs - rhs) <=> (rhs - lhs);
}

template <typename T, typename Alloc = std::allocator<T>>
struct queue : protected Alloc {
	typedef Alloc allocator_type;
	typedef T value_type;
	typedef value_type& reference;
	typedef const value_type& const_reference;
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef typename std::allocator_traits<allocator_type>::pointer pointer;
	typedef typename std::allocator_traits<allocator_type>::const_pointer const_pointer;
	typedef queue_iterator<queue> iterator;
	typedef queue_const_iterator<queue> const_iterator;
	typedef std::reverse_iterator<iterator> reverse_iterator;
	typedef std::reverse_iterator<const_iterator> reverse_const_iterator;

	friend iterator;
	friend const_iterator;
protected:
	pointer m_block;
	pointer m_block_end;
	pointer m_begin;
	size_type m_size;

	pointer past_the_end_pointer() noexcept {
		return pointer_advance(m_begin, m_size);
	}
	pointer pointer_advance(pointer p, difference_type n) noexcept {
		difference_type pos = p - m_block;
		difference_type npos = pos + n & capacity() - 1;
		return m_block + npos;
	}
	const_pointer past_the_end_pointer() const noexcept {
		return pointer_advance(m_begin, m_size);
	}
	const_pointer pointer_advance(const_pointer p, difference_type n) const noexcept {
		difference_type pos = p - m_block;
		difference_type npos = pos + n & capacity() - 1;
		return m_block + npos;
	}
	size_type pointer_index(const_pointer p) const noexcept {
		if (p < m_begin) {
			return p - m_block + m_block_end - m_begin;
		} else {
			return p - m_begin;
		}
	}

	static constexpr bool ALLOC_NOTHROW = noexcept(std::declval<allocator_type>().allocate(size_type()));
	static constexpr bool DEFAULT_NOTHROW = noexcept(value_type());
	static constexpr bool COPY_NOTHROW = noexcept(value_type(std::declval<const value_type&>()));
	static constexpr bool MOVE_NOTHROW = noexcept(value_type(std::declval<value_type&&>()));
public:
	constexpr queue() noexcept : allocator_type(), m_block(), m_block_end(), m_begin(), m_size() {}
	queue(size_type ucap) noexcept(ALLOC_NOTHROW) : queue() {
		size_type cap = std::bit_ceil(ucap);
		m_block = allocator_type::allocate(cap);
		if (m_block) {
			m_block_end = m_block + cap;
			m_begin = m_block;
			m_size = 0;
		}
	}
	allocator_type& allocator() noexcept {
		return *this;
	}
	const allocator_type& allocator() const noexcept {
		return *this;
	}
	size_type capacity() const noexcept {
		return m_block_end - m_block;
	}
	size_type size() const noexcept {
		return m_size;
	}
	bool empty() const noexcept {
		return size() == 0;
	}
	iterator begin() noexcept {
		return iterator(this, m_begin);
	}
	iterator end() noexcept {
		return iterator(this);
	}
	const_iterator begin() const noexcept {
		return const_iterator(this, m_begin);
	}
	const_iterator end() const noexcept {
		return const_iterator(this);
	}
	const_iterator cbegin() const noexcept {
		return const_iterator(this, m_begin);
	}
	const_iterator cend() const noexcept {
		return const_iterator(this);
	}
	reverse_iterator rbegin() noexcept {
		return reverse_iterator(begin());
	}
	reverse_iterator rend() noexcept {
		return reverse_iterator(end());
	}
	reverse_const_iterator rbegin() const noexcept {
		return reverse_const_iterator(begin());
	}
	reverse_const_iterator rend() const noexcept {
		return reverse_const_iterator(end());
	}
	reverse_const_iterator crbegin() const noexcept {
		return reverse_const_iterator(begin());
	}
	reverse_const_iterator crend() const noexcept {
		return reverse_const_iterator(end());
	}
protected:
	void copy_construct(const queue& o) noexcept(ALLOC_NOTHROW and COPY_NOTHROW) {
		size_type cap = std::bit_ceil(o.size());
		pointer new_block = allocator_type::allocate(cap);
		if (new_block) {
			std::uninitialized_copy(o.begin(), o.end(), new_block);
			m_block = new_block;
			m_block_end = m_block + cap;
			m_begin = m_block;
			m_size = o.size();
		}
	}
	void move_construct(queue&& o) noexcept {
		std::swap(m_block, o.m_block);
		std::swap(m_block_end, o.m_block_end);
		std::swap(m_begin, o.m_begin);
		std::swap(m_size, o.m_size);
	}
public:
	queue(const queue& o) noexcept(ALLOC_NOTHROW and COPY_NOTHROW) : queue() {
		copy_construct(o);
	}
	queue(queue&& o) noexcept : queue() {
		move_construct(o);
	}

	void destroy() noexcept {
		if (m_block) {
			std::destroy(begin(), end());
			allocator_type::deallocate(m_block, capacity());
			m_block = pointer();
			m_begin = pointer();
			m_block_end = pointer();
			m_size = 0;
		}
	}

	queue& operator=(const queue& o) noexcept(ALLOC_NOTHROW and COPY_NOTHROW) {
		if (&o == this) {
			return *this;
		}
		queue t(o);
		if (not t.empty()) {
			*this = std::move(t);
		}
		return *this;
	}

	queue& operator=(queue&& o) noexcept {
		if (&o == this) {
			return *this;
		}
		destroy();
		move_construct(std::move(o));
		return *this;
	}

	~queue() noexcept {
		destroy();
	}

	queue& reserve(size_type new_ucap) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW) {
		if (new_ucap <= capacity()) {
			return *this;
		}
		size_type new_cap = std::bit_ceil(new_ucap);
		queue n(new_cap);
		if (not n.empty()) {
			std::uninitialized_move(begin(), end(), n.m_block);
			n.m_size = size();
			*this = std::move(n);
		}
		return *this;
	}
	queue& resize(size_type new_size) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW and DEFAULT_NOTHROW) {
		if (new_size <= size()) {
			std::destroy(begin() + new_size, end());
			m_size = new_size;
			return *this;
		}
		if (capacity() < new_size and reserve(new_size).capacity() < size() + new_size) {
			return *this;
		}
		std::uninitialized_default_construct(past_the_end_pointer(), pointer_advance(m_begin, new_size));
		m_size = new_size;
		return *this;
	}
	queue& resize(size_type new_size, const_reference fill) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW and COPY_NOTHROW) {
		if (new_size <= size()) {
			std::destroy(begin() + new_size, end());
			m_size = new_size;
			return *this;
		}
		if (capacity() < new_size and reserve(new_size).capacity() < size() + new_size) {
			return *this;
		}
		std::uninitialized_fill(past_the_end_pointer(), pointer_advance(m_begin, new_size), fill);
		m_size = new_size;
		return *this;
	}
	queue& clear() noexcept {
		return resize(0);
	}
	static auto dummy_address() noexcept(DEFAULT_NOTHROW) {
		static value_type DUMMY = value_type();
		return std::addressof(DUMMY);
	}
	reference peek() noexcept(DEFAULT_NOTHROW) {
		if (empty()) {
			return *dummy_address();
		} else {
			return *m_begin;
		}
	}
	const_reference peek() const noexcept(DEFAULT_NOTHROW) {
		if (empty()) {
			return *dummy_address();
		} else {
			return *m_begin;
		}
	}
	template <typename...Args>
	queue& push(Args&&...args) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW and noexcept(value_type(std::forward<Args>(args)...))) {
		if (capacity() == size() and reserve(size() + 1).capacity() == size()) {
			return *this;
		}
		std::allocator_traits<allocator_type>::construct(*this, past_the_end_pointer(), std::forward<Args>(args)...);
		++m_size;
		return *this;
	}
	template <std::input_iterator It>
	queue& push(It begin, It end) noexcept(ALLOC_NOTHROW and MOVE_NOTHROW and noexcept(value_type(std::forward<decltype(*begin)>(*begin)))) {
		if constexpr (std::forward_iterator<It>) {
			auto dist = std::distance(begin, end);
			if (capacity() < size() + dist and reserve(size() + dist).capacity() < size() + dist) {
				return *this;
			}
		}
		for (; begin != end; ++begin) {
			push(std::forward<decltype(*begin)>(*begin));
		}
		return *this;
	}
	value_type pop() noexcept(DEFAULT_NOTHROW and MOVE_NOTHROW) {
		if (empty()) {
			return value_type();
		}
		value_type result = std::move(peek());
		m_begin->~value_type();
		m_begin = pointer_advance(m_begin, 1);
		--m_size;
		return result;
	}
	template <std::integral Int>
	reference at(Int i) noexcept(DEFAULT_NOTHROW) {
		if ((i >= Int(0) and i >= difference_type(size())) or (i < 0 and size_type(-i) > size())) {
			return *dummy_address();
		}
		return *pointer_advance(m_begin, i);
	}
	template <std::integral Int>
	const_reference at(Int i) const noexcept(DEFAULT_NOTHROW) {
		if ((i >= Int(0) and i >= difference_type(size())) or (i < 0 and size_type(-i) > size())) {
			return *dummy_address();
		}
		return *pointer_advance(m_begin, i);
	}
	template <std::integral Int>
	reference operator[](Int i) noexcept(DEFAULT_NOTHROW) {
		return at(i);
	}
	template <std::integral Int>
	const_reference operator[](Int i) const noexcept(DEFAULT_NOTHROW) {
		return at(i);
	}
};

}

#endif // #ifndef AUTOMATA_QUEUE_HPP
