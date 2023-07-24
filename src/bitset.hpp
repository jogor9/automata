// todo: shifts, arithmetic operations
#ifndef AUTOMATA_BITSET_HPP
#define AUTOMATA_BITSET_HPP

#include <memory>
#include <tuple>
#include <bit>
#include <limits>
#include <algorithm>
#include <functional>
#include "util2.hpp"

namespace automata {

template <typename Bitset>
struct bit_const_iterator {
	typedef Bitset bitset;
	typedef typename Bitset::allocator_type allocator_type;
	typedef typename Bitset::size_type size_type;
	typedef typename Bitset::difference_type difference_type;
	typedef typename Bitset::pointer pointer;
	typedef typename Bitset::const_pointer const_pointer;
	typedef typename Bitset::block_type block_type;
	typedef typename Bitset::value_type value_type;
	typedef bit_const_iterator<bitset> reference;
	typedef reference const_reference;
private:
	void operator&() const {}
	void operator&() {}
protected:
	const bitset* m_owner;
	mutable size_type m_bit_index;

	friend bitset;

	const_pointer block() noexcept {
		return m_owner->m_block;
	}
	static constexpr auto BLOCK_SIZE = bit_count<block_type>();
	size_type& index() const noexcept {
		return m_bit_index;
	}
	block_type mask() const noexcept {
		return pow2<block_type>(index() % BLOCK_SIZE);
	}
	bool comp() const noexcept {
		return m_owner->get_comp();
	}

	bit_const_iterator(const bitset* owner, size_type bit) noexcept : m_owner(owner), m_bit_index(bit) {}
public:
	operator bool() noexcept {
		return ((block()[index() / BLOCK_SIZE] & mask()) != 0) xor comp();
	}

	bit_const_iterator& operator*() noexcept {
		return *this;
	}
	bit_const_iterator& operator++() noexcept {
		++index();
		return *this;
	}
	bit_const_iterator& operator--() noexcept {
		--index();
		return *this;
	}
	template <std::integral Int>
	bit_const_iterator& operator+=(Int i) noexcept {
		index() += i;
		return *this;
	}
	template <std::integral Int>
	bit_const_iterator& operator-=(Int i) noexcept {
		index() -= i;
		return *this;
	}
	bit_const_iterator operator++(int) noexcept {
		auto t = *this;
		operator++();
		return t;
	}
	bit_const_iterator operator--(int) noexcept {
		auto t = *this;
		operator--();
		return t;
	}
	template <std::integral Int>
	bit_const_iterator operator+(Int i) noexcept {
		auto t = *this;
		t += i;
		return t;
	}
	template <std::integral Int>
	bit_const_iterator operator-(Int i) noexcept {
		auto t = *this;
		t -= i;
		return t;
	}
	template <std::integral Int>
	bit_const_iterator operator[](Int i) noexcept {
		return *(*this + i);
	}

	template <typename A>
	friend bool operator==(bit_const_iterator<A> l, bit_const_iterator<A> r) noexcept;
	template <typename A>
	friend typename bit_const_iterator<A>::difference_type operator-(bit_const_iterator<A> l, bit_const_iterator<A> r) noexcept;
};

template <typename Bitset>
struct bit_iterator {
	typedef Bitset bitset;
	typedef typename Bitset::allocator_type allocator_type;
	typedef typename Bitset::size_type size_type;
	typedef typename Bitset::difference_type difference_type;
	typedef typename Bitset::pointer pointer;
	typedef typename Bitset::const_pointer const_pointer;
	typedef typename Bitset::block_type block_type;
	typedef typename Bitset::value_type value_type;
	typedef bit_iterator<bitset> reference;
	typedef const bit_iterator<bitset> const_reference;
private:
	void operator&() const {}
	void operator&() {}
protected:
	bitset* m_owner;
	mutable size_type m_bit_index;

	friend bitset;

	const_pointer block() const noexcept {
		return m_owner->m_block;
	}
	pointer block() noexcept {
		return m_owner->m_block;
	}
	static constexpr auto BLOCK_SIZE = bit_count<block_type>();
	size_type& index() const noexcept {
		return m_bit_index;
	}
	block_type mask() const noexcept {
		return pow2<block_type>(index() % BLOCK_SIZE);
	}
	bool comp() const noexcept {
		return m_owner->get_comp();
	}

	bit_iterator(bitset* owner, size_type bit) noexcept : m_owner(owner), m_bit_index(bit) {}
public:
	operator bool() const noexcept {
		return ((block()[index() / BLOCK_SIZE] & mask()) != 0) xor comp();
	}
	bit_iterator& operator=(bool b) noexcept {
		if (b xor comp()) {
			block()[index() / BLOCK_SIZE] |= mask();
		} else {
			block()[index() / BLOCK_SIZE] &= ~mask();
		}
		return *this;
	}

	bit_iterator& operator*() noexcept {
		return *this;
	}
	bit_iterator& operator++() noexcept {
		++index();
		return *this;
	}
	bit_iterator& operator--() noexcept {
		--index();
		return *this;
	}
	template <std::integral Int>
	bit_iterator& operator+=(Int i) noexcept {
		index() += i;
		return *this;
	}
	template <std::integral Int>
	bit_iterator& operator-=(Int i) noexcept {
		index() -= i;
		return *this;
	}
	bit_iterator operator++(int) noexcept {
		auto t = *this;
		operator++();
		return t;
	}
	bit_iterator operator--(int) noexcept {
		auto t = *this;
		operator--();
		return t;
	}
	template <std::integral Int>
	bit_iterator operator+(Int i) noexcept {
		auto t = *this;
		t += i;
		return t;
	}
	template <std::integral Int>
	bit_iterator operator-(Int i) noexcept {
		auto t = *this;
		t -= i;
		return t;
	}
	template <std::integral Int>
	bit_iterator operator[](Int i) noexcept {
		return *(*this + i);
	}

	template <typename A>
	friend bool operator==(bit_iterator<A> l, bit_iterator<A> r) noexcept;
	template <typename A>
	friend typename bit_iterator<A>::difference_type operator-(bit_iterator<A> l, bit_iterator<A> r) noexcept;
};

template <typename A>
bool operator==(bit_iterator<A> l, bit_iterator<A> r) noexcept {
	return l.owner == r.owner and l.index() == r.index();
}

template <typename A>
typename bit_iterator<A>::difference_type operator-(bit_iterator<A> l, bit_iterator<A> r) noexcept {
	return l.index() - r.index();
}

template <typename A>
auto operator<=>(bit_iterator<A> l, bit_iterator<A> r) noexcept {
	return (l - r) <=> (r - l);
}
template <typename A>
bool operator==(bit_const_iterator<A> l, bit_const_iterator<A> r) noexcept {
	return l.owner == r.owner and l.index() == r.index();
}

template <typename A>
typename bit_const_iterator<A>::difference_type operator-(bit_const_iterator<A> l, bit_const_iterator<A> r) noexcept {
	return l.index() - r.index();
}

template <typename A>
auto operator<=>(bit_const_iterator<A> l, bit_const_iterator<A> r) noexcept {
	return (l - r) <=> (r - l);
}

template <std::unsigned_integral Block = size_t, typename Alloc = std::allocator<Block>>
struct bitset : protected Alloc {
	typedef Alloc allocator_type;
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef typename std::allocator_traits<allocator_type>::pointer pointer;
	typedef typename std::allocator_traits<allocator_type>::const_pointer const_pointer;
	typedef Block block_type;
	typedef bool value_type;
	typedef bit_iterator<bitset> reference;
	typedef bit_const_iterator<bitset> const_reference;
	typedef bit_iterator<bitset> iterator;
	typedef bit_const_iterator<bitset> const_iterator;
	typedef std::reverse_iterator<iterator> reverse_iterator;
	typedef std::reverse_iterator<const_iterator> reverse_const_iterator;
protected:
	static constexpr bool ALLOC_NOTHROW = noexcept(std::declval<allocator_type>().allocate(size_type()));
	static constexpr auto BIT_COUNT = bit_count<block_type>();
	static constexpr size_type CAPACITY_MASK = pow2mask<size_type>(bit_count<size_type>() - 1);
	static constexpr size_type COMPLEMENT_MASK = ~CAPACITY_MASK;
	pointer m_block;
	size_type m_bit_count;
	size_type m_capacity_comp;

	size_type get_capacity() const noexcept {
		return m_capacity_comp & ~COMPLEMENT_MASK;
	}
	void set_capacity(size_type c) noexcept {
		m_capacity_comp = (m_capacity_comp & COMPLEMENT_MASK) | (c & CAPACITY_MASK);
	}
	bool get_comp() const noexcept {
		return (m_capacity_comp & COMPLEMENT_MASK) != 0;
	}
	void set_comp(bool b) noexcept {
		if (b) {
			m_capacity_comp |= COMPLEMENT_MASK;
		} else {
			m_capacity_comp &= ~COMPLEMENT_MASK;
		}
	}

	friend iterator;
	friend const_iterator;
public:
	bitset() noexcept : allocator_type(), m_block(), m_bit_count(), m_capacity_comp() {}
	bitset(size_type bit_cap) noexcept(ALLOC_NOTHROW) : bitset() {
		size_type cap = std::bit_ceil(bits_to_blocks(bit_cap));
		m_block = allocator_type::allocate(cap);
		if (m_block) {
			set_capacity(cap);
			m_bit_count = 0;
		}
	}
	template <typename CharT>
	bitset(const CharT* s) : bitset(std::char_traits<CharT>::length(s)) {
		for (; *s != CharT(); ++s) {
			push(std::char_traits<CharT>::lt(CharT('0'), *s));
		}
	}
	allocator_type& allocator() noexcept {
		return *this;
	}
	const allocator_type& allocator() const noexcept {
		return *this;
	}
protected:
	void copy_construct(const bitset& o) noexcept(ALLOC_NOTHROW) {
		size_type new_cap = std::bit_ceil(o.size_blocks());
		m_block = allocator_type::allocate(new_cap);
		if (m_block) {
			std::uninitialized_copy(o.m_block, o.m_block + o.size_blocks(), m_block);
			m_bit_count = o.m_bit_count;
			set_capacity(new_cap);
			set_comp(o.get_comp());
		}
	}
	void move_construct(bitset&& o) noexcept {
		std::swap(m_block, o.m_block);
		std::swap(m_bit_count, o.m_bit_count);
		std::swap(m_capacity_comp, o.m_capacity_comp);
	}
public:
	static size_type bits_to_blocks(size_type bits) noexcept {
		return bits / BIT_COUNT + bool(bits % BIT_COUNT);
	}
	static size_type blocks_to_bits(size_type blocks) noexcept {
		return blocks * BIT_COUNT;
	}
	static size_type bits_to_blocks_trunc(size_type bits) noexcept {
		return bits / BIT_COUNT;
	}
	bitset(const bitset& o) noexcept(ALLOC_NOTHROW) : bitset() {
		copy_construct(o);
	}
	bitset(bitset&& o) noexcept : bitset() {
		move_construct(std::move(o));
	}
	void destroy() noexcept {
		if (m_block) {
			std::destroy(m_block, m_block + size_blocks());
			allocator_type::deallocate(m_block, capacity_blocks());
			m_block = pointer();
			m_bit_count = 0;
			set_capacity(0);
			set_comp(0);
		}
	}

	bitset& operator=(const bitset& o) noexcept(ALLOC_NOTHROW) {
		if (&o == this) {
			return *this;
		}
		bitset t(o);
		if (not t.empty()) {
			*this = std::move(t);
		}
		return *this;
	}
	bitset& operator=(bitset&& o) noexcept {
		if (&o == this) {
			return *this;
		}
		destroy();
		move_construct(std::move(o));
		return *this;
	}
	~bitset() noexcept {
		destroy();
	}

	bool empty() const noexcept {
		return size() == 0;
	}
	size_type capacity() const noexcept {
		return blocks_to_bits(capacity_blocks());
	}
	size_type capacity_blocks() const noexcept {
		return get_capacity();
	}
	size_type size() const noexcept {
		return m_bit_count;
	}
	size_type size_blocks() const noexcept {
		return bits_to_blocks(size());
	}

	bitset& reserve(size_type new_bit_cap) noexcept(ALLOC_NOTHROW) {
		if (new_bit_cap <= capacity()) {
			return *this;
		}
		bitset t(new_bit_cap);
		if (t.capacity() != 0) {
			t.m_bit_count = m_bit_count;
			std::uninitialized_move(m_block, m_block + size_blocks(), t.m_block);
			*this = std::move(t);
		}
		return *this;
	}
	static constexpr block_type ZERO_BLOCK = block_type(0);
	static constexpr block_type ONE_BLOCK = std::numeric_limits<block_type>::max();
	bitset& resize(size_type new_size, bool v) noexcept(ALLOC_NOTHROW) {
		size_type new_size_blocks = bits_to_blocks(new_size);
		if (new_size <= size()) {
			std::destroy(m_block + new_size_blocks, m_block + size_blocks());
			m_bit_count = new_size;
			return *this;
		}
		if (new_size > capacity() and new_size > reserve(new_size).capacity()) {
			return *this;
		}
		if (v) {
			std::uninitialized_fill(m_block + size_blocks(), m_block + new_size_blocks, ONE_BLOCK);
			if (size() % BIT_COUNT) {
				m_block[size_blocks() - 1] |= ~pow2mask<block_type>(size() % BIT_COUNT);
			}
		} else {
			std::uninitialized_fill(m_block + size_blocks(), m_block + new_size_blocks, ZERO_BLOCK);
			if (size() % BIT_COUNT) {
				m_block[size_blocks() - 1] &= pow2mask<block_type>(size() % BIT_COUNT);
			}
		}
		m_bit_count = new_size;
		return *this;
	}
	bitset& resize(size_type new_size) noexcept(ALLOC_NOTHROW) {
		return resize(new_size, false);
	}
	bitset& clear() noexcept {
		return resize(0);
	}
	void swap(bitset& o) noexcept {
		std::swap(*this, o);
	}
	iterator begin() noexcept {
		return iterator(this, 0);
	}
	iterator end() noexcept {
		return iterator(this, size());
	}
	reverse_iterator rbegin() noexcept {
		return std::reverse_iterator(end());
	}
	reverse_iterator rend() noexcept {
		return std::reverse_iterator(begin());
	}
	const_iterator begin() const noexcept {
		return const_iterator(this, 0);
	}
	const_iterator end() const noexcept {
		return const_iterator(this, size());
	}
	const_iterator cbegin() const noexcept {
		return const_iterator(this, 0);
	}
	const_iterator cend() const noexcept {
		return const_iterator(this, size());
	}
	reverse_const_iterator rbegin() const noexcept {
		return std::reverse_iterator(end());
	}
	reverse_const_iterator rend() const noexcept {
		return std::reverse_iterator(begin());
	}
	reverse_const_iterator crbegin() const noexcept {
		return std::reverse_iterator(end());
	}
	reverse_const_iterator crend() const noexcept {
		return std::reverse_iterator(begin());
	}
	bool pop() noexcept {
		--m_bit_count;
		return begin()[m_bit_count];
	}
	bitset& push(bool b) noexcept(ALLOC_NOTHROW) {
		if (size() == capacity() and size() == reserve(size() + 1).capacity()) {
			return *this;
		}
		begin()[m_bit_count] = b;
		++m_bit_count;
		return *this;
	}
	static reference dummy_reference() noexcept(ALLOC_NOTHROW) {
		static bitset b(1);
		return b.begin();
	}
	static const_reference dummy_const_reference() noexcept(ALLOC_NOTHROW) {
		static bitset b(1);
		return b.cbegin();
	}
	static constexpr size_type INVALID_INDEX = size_type(-1);
	template <std::integral Int>
	size_type positive_index(Int i) const noexcept {
		if (i < 0) {
			if (size_type(-i) > size()) {
				return INVALID_INDEX;
			}
			return size_type(-i);
		} else {
			if (size_type(i) >= size()) {
				return INVALID_INDEX;
			}
			return size_type(i);
		}
	}
	template <std::integral Int>
	reference at(Int i) noexcept {
		if (positive_index(i) == INVALID_INDEX) {
			return dummy_reference();
		}
		return begin()[positive_index(i)];
	}
	template <std::integral Int>
	const_reference at(Int i) const noexcept {
		if (positive_index(i) == INVALID_INDEX) {
			return dummy_const_reference();
		}
		return begin()[positive_index(i)];
	}
	template <std::integral Int>
	reference operator[](Int i) noexcept {
		return at(i);
	}
	template <std::integral Int>
	const_reference operator[](Int i) const noexcept {
		return at(i);
	}
	template <std::integral Int>
	bitset& set(Int i, size_type l, bool val) noexcept {
		val ^= get_comp();
		if (l == 0 or positive_index(i) == INVALID_INDEX or positive_index(positive_index(i) + l - 1) == INVALID_INDEX) {
			return *this;
		}
		size_type index = positive_index(i);
		size_type end = index + l;
		size_type lead = index % BIT_COUNT;
		size_type trail = end % BIT_COUNT;
		size_type block = bits_to_blocks(index);
		size_type block_end = bits_to_blocks_trunc(end);
		if (block > block_end) {
			if (val) {
				m_block[block - 1] |= ~pow2mask<block_type>(lead) & pow2mask<block_type>(trail);
			} else {
				m_block[block - 1] &= ~(~pow2mask<block_type>(lead) & pow2mask<block_type>(trail));
			}
		} else {
			if (val) {
				if (lead != 0) {
					m_block[block - 1] |= ~pow2mask<block_type>(lead);
				}
				std::fill(m_block + block, m_block + block_end, ONE_BLOCK);
				if (trail != 0) {
					m_block[block_end] |= pow2mask<block_type>(trail);
				}
			} else {
				if (lead != 0) {
					m_block[block - 1] &= pow2mask<block_type>(lead);
				}
				std::fill(m_block + block, m_block + block_end, ZERO_BLOCK);
				if (trail != 0) {
					m_block[block_end] &= ~pow2mask<block_type>(trail);
				}
			}
		}
		return *this;
	}
	template <std::integral Int>
	bitset& set(Int i, size_type l) noexcept {
		return set(i, l, true);
	}
	template <std::integral Int>
	bitset& set(Int i, bool val) noexcept {
		return set(i, 1, val);
	}
	template <std::integral Int>
	bitset& set(Int i) noexcept {
		return set(i, true);
	}
	bitset& set() noexcept {
		return set(0, size());
	}
	template <std::integral Int>
	bitset& reset(Int i, size_type l) noexcept {
		return set(i, l, false);
	}
	template <std::integral Int>
	bitset& reset(Int i) noexcept {
		return set(i, false);
	}
	bitset& reset() noexcept {
		return reset(0, size());
	}
	template <std::integral Int>
	bitset& flip(Int i, size_type l) noexcept {
		if (l == 0 or positive_index(i) == INVALID_INDEX or positive_index(positive_index(i) + l - 1) == INVALID_INDEX) {
			return *this;
		}
		size_type index = positive_index(i);
		size_type end = index + l;
		size_type lead = index % BIT_COUNT;
		size_type trail = end % BIT_COUNT;
		size_type block = bits_to_blocks(index);
		size_type block_end = bits_to_blocks_trunc(end);
		if (block == block_end) {
			m_block[block - 1] ^= ~pow2mask<block_type>(lead) & pow2mask<block_type>(trail);
		} else {
			if (lead != 0) {
				m_block[block - 1] ^= ~pow2mask<block_type>(lead);
			}
			std::transform(m_block + block, m_block + block_end, std::bit_not<block_type>());
			if (trail != 0) {
				m_block[block_end] ^= pow2mask<block_type>(trail);
			}
		}
		return *this;
	}
	template <std::integral Int>
	bitset& flip(Int i) noexcept {
		return flip(i, 1);
	}
	bitset& flip() noexcept {
		if (get_comp()) {
			set_comp(false);
			return *this;
		} else {
			set_comp(true);
			return flip(0, size());
		}
	}
	bitset& inner_flip() noexcept {
		return flip(0, size());
	}
	bitset& outer_flip() noexcept {
		set_comp(get_comp() xor true);
		return flip(0, size());
	}
	bitset& complement() noexcept {
		set_comp(get_comp() xor true);
		return *this;
	}
protected:
	template <typename Op>
	void raw_op(const bitset& o, Op op) noexcept(ALLOC_NOTHROW) {
		if (size() < o.size()) {
			resize(o.size());
		}
		auto tbegin = m_block;
		auto obegin = o.m_block;
		auto tend = tbegin + bits_to_blocks_trunc(o.size());
		auto oend = obegin + bits_to_blocks_trunc(o.size());
		for (; tbegin < tend; ++tbegin, ++obegin) {
			*tbegin = op(*tbegin, *obegin);
		}
		auto trail = o.size() % BIT_COUNT;
		auto&& oob_block = o.get_comp() ? ONE_BLOCK : ZERO_BLOCK;
		if (trail != 0) {
			*tend = (op(*tend, *oend) & pow2mask(trail)) | (op(*tend, oob_block) & ~pow2mask(trail));
		}
		auto oob_end = m_block + size_blocks();
		auto oob_begin = tend + 1;
		for (; oob_begin < oob_end; ++oob_begin) {
			*oob_begin = op(*oob_begin, oob_block);
		}
	}
	void raw_and(const bitset& o) noexcept(ALLOC_NOTHROW) {
		raw_op(o, std::bit_and<block_type>());
	}
	void raw_or(const bitset& o) noexcept(ALLOC_NOTHROW) {
		raw_op(o, std::bit_or<block_type>());
	}
	void raw_xor(const bitset& o) noexcept(ALLOC_NOTHROW) {
		raw_op(o, std::bit_xor<block_type>());
	}
	void raw_diff(const bitset& o) noexcept(ALLOC_NOTHROW) {
		raw_op(o, [](block_type l, block_type r) noexcept -> block_type { return l & ~r; });
	}
	void raw_rdiff(const bitset& o) noexcept(ALLOC_NOTHROW) {
		raw_op(o, [](block_type l, block_type r) noexcept -> block_type { return ~l & r; });
	}
public:
	bitset& unify(const bitset& o) noexcept(ALLOC_NOTHROW) {
		if (get_comp() and o.get_comp()) {
			raw_and(o);
		} else if (get_comp() and not o.get_comp()) {
			raw_diff(o);
		} else if (not get_comp() and o.get_comp()) {
			raw_rdiff(o);
			set_comp(true);
		} else {
			raw_or(o);
		}
		return *this;
	}
	bitset& intersect(const bitset& o) noexcept(ALLOC_NOTHROW) {
		if (get_comp() and o.get_comp()) {
			raw_or(o);
		} else if (get_comp() and not o.get_comp()) {
			raw_rdiff(o);
			set_comp(false);
		} else if (not get_comp() and o.get_comp()) {
			raw_diff(o);
		} else {
			raw_and(o);
		}
		return *this;
	}
	bitset& difference(const bitset& o) noexcept(ALLOC_NOTHROW) {
		if (get_comp() and o.get_comp()) {
			raw_rdiff(o);
			set_comp(false);
		} else if (get_comp() and not o.get_comp()) {
			raw_or(o);
		} else if (not get_comp() and o.get_comp()) {
			raw_and(o);
		} else {
			raw_diff(o);
		}
		return *this;
	}
	bitset& symmetric_difference(const bitset& o) noexcept(ALLOC_NOTHROW) {
		raw_xor(o);
		set_comp(get_comp() xor o.get_comp());
		return *this;
	}
	bitset operator~() const noexcept(ALLOC_NOTHROW) {
		auto t = *this;
		t.complement();
		return t;
	}
	bitset& operator|=(const bitset& o) noexcept(ALLOC_NOTHROW) {
		return unify(o);
	}
	bitset& operator&=(const bitset& o) noexcept(ALLOC_NOTHROW) {
		return intersect(o);
	}
	bitset& operator^=(const bitset& o) noexcept(ALLOC_NOTHROW) {
		return symmetric_difference(o);
	}
	bitset& operator-=(const bitset& o) noexcept(ALLOC_NOTHROW) {
		return difference(o);
	}

	bool none_inner() const noexcept {
		auto begin = m_block;
		auto end = m_block + bits_to_blocks_trunc(size());
		auto&& cmpblock = get_comp() ? ONE_BLOCK : ZERO_BLOCK;
		for (; begin != end; ++begin) {
			if (*begin != cmpblock) {
				return false;
			}
		}
		auto trail = size() % BIT_COUNT;
		if (trail != 0) {
			auto&& cmpblock = get_comp() ? ONE_BLOCK & pow2mask(trail) : ZERO_BLOCK;
			return (*begin & pow2mask(trail)) == cmpblock;
		} else {
			return true;
		}
	}
	bool none_outer() const noexcept {
		return not get_comp();
	}
	bool none() const noexcept {
		return none_outer() and none_inner();
	}
	bool any_inner() const noexcept {
		return not none_inner();
	}
	bool any_outer() const noexcept {
		return not none_outer();
	}
	bool any() const noexcept {
		return not none();
	}
	bool all_inner() const noexcept {
		auto begin = m_block;
		auto end = m_block + bits_to_blocks_trunc(size());
		auto&& cmpblock = get_comp() ? ZERO_BLOCK : ONE_BLOCK;
		for (; begin != end; ++begin) {
			if (*begin != cmpblock) {
				return false;
			}
		}
		auto trail = size() % BIT_COUNT;
		if (trail != 0) {
			auto&& cmpblock = get_comp() ? ZERO_BLOCK : ONE_BLOCK & pow2mask(trail) ;
			return (*begin & pow2mask(trail)) == cmpblock;
		} else {
			return true;
		}
	}
	bool all_outer() const noexcept {
		return get_comp();
	}
	bool all() const noexcept {
		return all_outer() and all_inner();
	}
	template <std::invocable<size_type> Callback>
	bool for_each_in_set(Callback callback) const noexcept {
		for (size_type i = 0; i < size(); ++i) {
			if (at(i)) {
				CALL_RETURN_ON_FAIL_IF_PREDICATE(callback, i);
			}
		}
		return true;
	}
	template <std::invocable<size_type> Callback>
	bool for_each_not_in_set(Callback callback) const noexcept {
		for (size_type i = 0; i < size(); ++i) {
			if (not at(i)) {
				CALL_RETURN_ON_FAIL_IF_PREDICATE(callback, i);
			}
		}
		return true;
	}
};

template <std::unsigned_integral Block, typename Alloc>
bitset<Block, Alloc> operator|(bitset<Block, Alloc> l, const bitset<Block, Alloc>& r) {
	return l |= r;
}
template <std::unsigned_integral Block, typename Alloc>
bitset<Block, Alloc> operator&(bitset<Block, Alloc> l, const bitset<Block, Alloc>& r) {
	return l &= r;
}
template <std::unsigned_integral Block, typename Alloc>
bitset<Block, Alloc> operator^(bitset<Block, Alloc> l, const bitset<Block, Alloc>& r) {
	return l ^= r;
}
template <std::unsigned_integral Block, typename Alloc>
bitset<Block, Alloc> operator-(bitset<Block, Alloc> l, const bitset<Block, Alloc>& r) {
	return l -= r;
}

} // namespace automata

#endif // #ifndef AUTOMATA_BITSET_HPP
