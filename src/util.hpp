#ifndef AUTOMATA_UTIL_HPP
#define AUTOMATA_UTIL_HPP

#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <map>
#include <functional>
#include <algorithm>
#include <ranges>
#include <boost/locale/utf.hpp>
#include <boost/dynamic_bitset.hpp>
#include "util2.hpp"
#include "queue.hpp"
#include "bitset.hpp"

namespace automata {

template <typename T, typename...Args>
concept predicate_function = std::predicate<T, Args...>;
template <typename T>
concept state_callback = std::invocable<T, size_t>;
template <typename T>
concept state_predicate = predicate_function<T, size_t>;
template <typename T>
concept transition_callback = std::invocable<T, size_t, size_t, size_t>;
template <typename T>
concept transition_predicate = predicate_function<T, size_t, size_t, size_t>;
template <typename T>
concept indexed_state_predicate = predicate_function<T, size_t, size_t>;

template <typename T>
concept dfs_state_visitor_callback = std::invocable<T, size_t, boost::dynamic_bitset<size_t>>;
template <typename T>
concept dfs_state_visitor_predicate = predicate_function<T, size_t, boost::dynamic_bitset<size_t>>;

template <typename T>
concept dfs_transition_visitor_callback = std::invocable<T, size_t, size_t, size_t, boost::dynamic_bitset<size_t>>;
template <typename T>
concept dfs_transition_visitor_predicate = predicate_function<T, size_t, size_t, size_t, boost::dynamic_bitset<size_t>>;

template <typename T>
concept boolean_operation = std::invocable<T, bool, bool> && requires(T t, bool b) { { t(b, b) } -> std::convertible_to<bool>; };

template <typename T, typename R, typename...Args>
concept function_like = std::invocable<T, Args...> && std::convertible_to<std::invoke_result_t<T, Args...>, R>;

template <typename T, typename Result>
concept state_merger = std::invocable<T, std::add_lvalue_reference_t<Result>, size_t, size_t>;

template <typename T, typename Result>
concept transition_merger = std::invocable<T, std::add_lvalue_reference_t<Result>, size_t, size_t, size_t, size_t, size_t, size_t>;

template <typename T, typename StateData>
concept state_splitter = function_like<T, std::pair<StateData, StateData>, size_t>;

template <typename T, typename TransitionData>
concept transition_splitter = function_like<T, std::pair<TransitionData, TransitionData>, size_t, size_t, size_t, size_t>;

struct empty_struct {};

template <typename T, typename U, typename HashT = std::hash<T>, typename KeyEqT = std::equal_to<T>, typename HashU = std::hash<U>, typename KeyEqU = std::equal_to<U>>
std::unordered_map<T, std::vector<U>, HashT, KeyEqT> reverse_mapping(const std::unordered_map<U, T, HashU, KeyEqU>& m) {
	std::unordered_map<T, std::vector<U>, HashT, KeyEqT> result;
	result.reserve(m.size());
	for (auto&& [key, val] : m) {
		result[val].push_back(key);
	}
	return result;
}
template <typename T, typename U, typename HashT = std::hash<T>, typename KeyEqT = std::equal_to<T>, typename HashU = std::hash<U>, typename KeyEqU = std::equal_to<U>>
std::unordered_map<T, U, HashT, KeyEqT> reverse_mapping_exact(const std::unordered_map<U, T, HashU, KeyEqU>& m) {
	std::unordered_map<T, U, HashT, KeyEqT> result;
	result.reserve(m.size());
	for (auto&& [key, val] : m) {
		result[val] = key;
	}
	return result;
}

template <typename Map>
Map make_map(Map m) {
	return m;
}

template <typename Map, typename Key, typename Val, typename...Args>
Map make_map(Map m, Key&& key, Val&& val, Args&&...args) {
	m.emplace(std::forward<Key>(key), std::forward<Val>(val));
	return make_map(std::move(m), std::forward<Args>(args)...);
}

template <typename Map, typename...Args>
Map make_map(Args&&...args) {
	Map result;
	result.reserve(sizeof...(args) / 2);
	return make_map(std::move(result), std::forward<Args>(args)...);
}

template <typename Set>
Set make_set(Set s) {
	return s;
}

template <typename Set, typename Key, typename...Args>
Set make_set(Set s, Key&& key, Args&&...args) {
	s.emplace(std::forward<key>(key));
	return make_set(std::move(s), std::forward<Args>(args)...);
}

template <typename Set, typename...Args>
Set make_set(Args&&...args) {
	Set result;
	result.reserve(sizeof...(args));
	return make_set(std::move(result), std::forward<Args>(args)...);
}

template <typename It, typename T>
It binary_search(It begin, It end, const T& target) {
	size_t len = std::distance(begin, end);
	if (len < 16) {
		return std::find(begin, end, target);
	}
	It item = begin + len / 2;
	if (*item > target) {
		return binary_search(begin, item, target);
	} else if (*item < target) {
		return binary_search(item + 1, end, target);
	} else {
		return item;
	}
}

template <typename It, typename T>
It sorted_insert(It begin, It end, const T& target) {
	auto [it0, it1] = std::equal_range(begin, end, target);
	std::move(it0, end, it0 + 1);
	*it0 = target;
	return it0;
}

template <typename It, typename T>
It sorted_erase(It begin, It end, const T& target) {
	auto [it0, it1] = std::equal_range(begin, end, target);
	std::move(it0 + 1, end, it0);
	return it0;
}

template <typename T>
	requires requires(T t) { { t < t } -> std::convertible_to<bool>; }
std::pair<T, T> ordered_pair(const std::pair<T, T>& p) {
	std::pair<T, T> result;
	result.first = min(p.first, p.second);
	result.second = max(p.first, p.second);
	return result;
}

template <predicate_function<size_t> Pred>
bool for_n(size_t n, Pred pred) {
	for (size_t i = 0; i < n; i++) {
		if (!pred(i)) {
			return false;
		}
	}
	return true;
}

template <predicate_function<size_t, size_t> Pred>
bool for_n_and_less_than_ith(size_t n, Pred pred) {
	return for_n(n, [&] (size_t j) {
		return for_n(j, [&] (size_t i) {
			return pred(i, j);
		});
	});
}

template <std::forward_iterator It, typename T, typename Comp>
It find_misbalance(It begin, It end, const T& a, const T& b, Comp comp) {
	size_t depth = 1;
	for (; begin != end && depth != 0; ++begin) {
		if (comp(a, *begin)) {
			++depth;
		} else if (comp(b, *begin)) {
			--depth;
		}
	}
	return begin;
}

template <std::forward_iterator It, typename T>
It find_misbalance(It begin, It end, const T& a, const T& b) {
	return find_misbalance(begin, end, a, b, std::equal_to<T>());
}

template <std::forward_iterator It, typename T, typename Comp>
It find_skip_balanced(It begin, It end, const T& a, const T& b, const T& t, Comp comp) {
	for (; begin != end; ++begin) {
		if (comp(a, *begin)) {
			auto it = begin;
			begin = find_misbalance(++it, end, a, b, comp);
		} else if (comp(t, *begin)) {
			return begin;
		}
	}
	return end;
}
template <std::forward_iterator It, typename T>
It find_skip_balanced(It begin, It end, const T& a, const T& b, const T& t) {
	return find_skip_balanced(begin, end, a, b, t, std::equal_to<T>());
}

template <typename Base>
struct escaped_iterator {
	typedef typename Base::difference_type difference_type;
	typedef typename Base::value_type value_type;
	typedef typename Base::pointer pointer;
	typedef typename Base::reference reference;
	
	Base b;

	escaped_iterator(Base b) : b(b) {}

	Base& base() { return b; }

	reference operator*() {
		return *b;
	}

	escaped_iterator& operator++() {
		if (*b == '\\') {
			++b;
		}
		++b;
		return *this;
	}

	escaped_iterator operator++(int) {
		auto t = *this;
		operator++();
		return t;
	}
};

template <typename Base>
bool operator==(escaped_iterator<Base> lhs, escaped_iterator<Base> rhs) {
	return lhs.b == rhs.b;
}

template <
	std::ranges::forward_range Range,
	typename Hasher = std::hash<std::ranges::range_value_t<Range>>,
	typename LHasher = std::hash<std::ranges::range_difference_t<Range>>
>
struct set_hash : Hasher, LHasher {
	size_t operator()(const Range& r) const noexcept {
		size_t result = LHasher::operator()(std::distance(r.begin(), r.end()));
		for (auto&& el : r) {
			result ^= Hasher::operator()(std::forward<decltype(el)>(el));
		}
		return result;
	}
};

template <std::ranges::forward_range Range, typename Eq = std::equal_to<std::ranges::range_value_t<Range>>>
struct ordered_set_equal_to : Eq {
	bool operator()(const Range& lhs, const Range& rhs) const noexcept {
		return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), (Eq)*this);
	}
};

template <std::ranges::forward_range Range, typename Less = std::less<std::ranges::range_value_t<Range>>>
struct ordered_set_less : Less {
	bool operator()(const Range& lhs, const Range& rhs) const noexcept {
		return std::lexicographical_compare(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), (Less)*this);
	}
};

template <std::ranges::forward_range Range>
	requires requires(const Range& r, const std::ranges::range_value_t<Range>& v) { { r.find(v) == r.end() } -> std::convertible_to<bool>; }
struct unordered_set_equal_to {
	bool operator()(const Range& lhs, const Range& rhs) const noexcept {
		if (std::distance(lhs.begin(), lhs.end()) != std::distance(rhs.begin(), rhs.end())) {
			return false;
		}
		for (auto&& el : lhs) {
			if (rhs.find(el) == rhs.end()) {
				return false;
			}
		}
		return true;
	}
};
template <std::ranges::forward_range Range>
	requires requires(const Range& r, const typename Range::key_type& v) { { r.find(v) == r.end() } -> std::convertible_to<bool>; }
struct unordered_map_equal_to {
	bool operator()(const Range& lhs, const Range& rhs) const noexcept {
		if (std::distance(lhs.begin(), lhs.end()) != std::distance(rhs.begin(), rhs.end())) {
			return false;
		}
		for (auto&& [el, _] : lhs) {
			if (rhs.find(el) == rhs.end()) {
				return false;
			}
		}
		return true;
	}
};

struct sequence_equal_to {
	typedef void is_transparent;
	template <std::ranges::forward_range Lhs, std::ranges::forward_range Rhs>
	bool operator()(const Lhs& lhs, const Rhs& rhs) const noexcept {
		return std::ranges::equal(lhs, rhs);
	}
};

size_t pown(size_t a, size_t n) noexcept {
	if (n == 0) {
		return 1;
	}
	size_t t = pown(a, n / 2);
	if (n % 2 == 0) {
		return t * t;
	} else {
		return a * t * t;
	}
}
template <std::forward_iterator It>
std::pair<size_t, It> ascii_to_size_t(It begin, It end) {
	size_t result = 0;
	auto str_end = std::find_if(begin, end, [] (auto t) { return t > '9' || t < '0'; });
	size_t len = std::distance(begin, str_end);
	size_t power = len - 1;
	static const std::string max_num = std::to_string((size_t)-1);
	if (len > max_num.length() || (len == max_num.length() && std::lexicographical_compare(max_num.begin(), max_num.end(), begin, str_end))) {
		return { (size_t)-1, str_end };
	}
	for (; begin != str_end; ++begin, --power) {
		result += pown(10, power) * (*begin - '0');
	}
	return { result, str_end };
}

template <typename T>
	requires (sizeof(T) <= sizeof(size_t))
consteval size_t max_shift() {
	return std::bit_width(size_t(1) << (sizeof(T) * 8 - 1)) - 1;
}

template <typename T>
consteval size_t max_shift() {
	return std::bit_width(T(1) << (sizeof(T) * 8 - 1)) - 1;
}

constexpr std::string alphabet_string_lowercase() {
	return "abcdefghijklmnopqrstuvwxyz";
}

constexpr std::string alphabet_string_uppercase() {
	return "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
}

constexpr std::string alphabet_string() {
	return alphabet_string_uppercase() .append(alphabet_string_lowercase());
}

constexpr std::string binary_digits_string() {
	return "01";
}

constexpr std::string octal_digits_string() {
	return binary_digits_string() .append("234567");
}

constexpr std::string digits_string() {
	return octal_digits_string() .append("89");
}

constexpr std::string word_beginning_string() {
	return alphabet_string();
}

constexpr std::string word_string() {
	return digits_string() .append(alphabet_string()) .append("_");
}

constexpr std::string hex_letters_string_lowercase() {
	return "abcdef";
}

constexpr std::string hex_letters_string_uppercase() {
	return "ABCDEF";
}

constexpr std::string hex_letters_string() {
	return hex_letters_string_uppercase().append(hex_letters_string_lowercase());
}

constexpr std::string hex_digits_string_lowercase() {
	return digits_string().append(hex_letters_string_lowercase());
}

constexpr std::string hex_digits_string_uppercase() {
	return digits_string().append(hex_letters_string_uppercase());
}

constexpr std::string hex_digits_string() {
	return digits_string().append(hex_letters_string());
}

constexpr std::string whitespace_string() {
	return "\t\n\v\f\r ";
}

constexpr std::string symbolic_chars_string() {
	return "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
}

template <typename T, typename Range, typename Func>
constexpr T accumulate_function_over_range(const Range& r, Func f) {
	auto&& begin = r.begin();
	auto&& end = r.end();
	if (begin == end) {
		return T();
	}
	T result = *begin;
	for (++begin; begin != end; ++begin) {
		result = f(std::move(result), *begin);
	}
	return result;
}

struct boolean_contradiction {
	constexpr bool operator()(bool, bool) const noexcept { return false; }
};

struct boolean_truth {
	constexpr bool operator()(bool, bool) const noexcept { return true; }
};

struct boolean_left {
	constexpr bool operator()(bool a, bool) const noexcept { return a; }
};

struct boolean_not_left {
	constexpr bool operator()(bool a, bool) const noexcept { return not a; }
};

struct boolean_right {
	constexpr bool operator()(bool, bool b) const noexcept { return b; }
};
struct boolean_not_right {
	constexpr bool operator()(bool, bool b) const noexcept { return not b; }
};

struct boolean_disjunction {
	constexpr bool operator()(bool a, bool b) const noexcept { return a or b; }
};

struct boolean_joint_denial {
	constexpr bool operator()(bool a, bool b) const noexcept { return not (a or b); }
};

struct boolean_conjunction {
	constexpr bool operator()(bool a, bool b) const noexcept { return a and b; }
};

struct boolean_alternative_denial {
	constexpr bool operator()(bool a, bool b) const noexcept { return not (a and b); }
};

struct boolean_exclusive_disjunction {
	constexpr bool operator()(bool a, bool b) const noexcept { return a xor b; }
};

struct boolean_equivalence {
	constexpr bool operator()(bool a, bool b) const noexcept { return not (a xor b); }
};

struct boolean_implication {
	constexpr bool operator()(bool a, bool b) const noexcept { return not (a and not b); }
};

struct boolean_non_implication {
	constexpr bool operator()(bool a, bool b) const noexcept { return a and not b; }
};

struct boolean_converse {
	constexpr bool operator()(bool a, bool b) const noexcept { return not (b and not a); }
};

struct boolean_non_converse {
	constexpr bool operator()(bool a, bool b) const noexcept { return b and not a; }
};

template <boolean_operation Op>
consteval bool associative() noexcept {
	Op op;
	for (int i = 0b000; i <= 0b111; ++i) {
		bool a = i & 0b001, b = i & 0b010, c = i & 0b100;
		if ((bool)op(op(a, b), c) != (bool)op(a, op(b, c))) {
			return false;
		}
	}
	return true;
}

template <boolean_operation Op>
consteval bool commutative() noexcept {
	Op op;
	for (int i = 0b00; i <= 0b11; ++i) {
		bool a = i & 0b01, b = i & 0b10;
		if ((bool)op(a, b) != (bool)op(b, a)) {
			return false;
		}
	}
	return true;
}

template <boolean_operation Op1, boolean_operation Op2>
consteval bool left_distributive() noexcept {
	Op1 op1; Op2 op2;
	for (int i = 0b00; i <= 0b111; ++i) {
		bool a = i & 0b001, b = i & 0b010, c = i & 0b100;
		if ((bool)op1(a, op2(b, c)) != (bool)op2(op1(a, b), op1(a, c))) {
			return false;
		}
	}
	return true;
}

template <boolean_operation Op1, boolean_operation Op2>
consteval bool right_distributive() noexcept {
	Op1 op1; Op2 op2;
	for (int i = 0b00; i <= 0b111; ++i) {
		bool a = i & 0b001, b = i & 0b010, c = i & 0b100;
		if ((bool)op1(op2(b, c), a) != (bool)op2(op1(b, a), op1(c, a))) {
			return false;
		}
	}
	return true;
}

template <boolean_operation Op1, boolean_operation Op2>
consteval bool distributive() noexcept {
	return left_distributive<Op1, Op2>() && right_distributive<Op1, Op2>();
}

template <std::ranges::range Range>
struct multi_range_view {
	using range_iterator = std::ranges::iterator_t<Range>;
	using difference_type = std::ranges::range_difference_t<Range>;
	using reference = std::ranges::range_reference_t<Range>;

	std::vector<std::pair<range_iterator, range_iterator>> iterator_pairs;

	struct multi_range_iterator {
		using view_type = multi_range_view<Range>;
		using range_iterator = typename view_type::range_iterator;
		using difference_type = typename view_type::difference_type;
		using reference = typename view_type::reference;
		using dereference_type = decltype(*std::declval<range_iterator>());

		ssize_t current_range;
		range_iterator range_it;
		multi_range_view<Range>& owner;

		multi_range_iterator(multi_range_view<Range>& owner, range_iterator range_it, ssize_t current_range)
			: owner(owner), range_it(range_it), current_range(current_range) {}

		dereference_type operator*() {
			return *range_it;
		}

		multi_range_iterator& operator++() {
			if (current_range < 0) {
				current_range = 0;
			} else {
				++range_it;
				if (range_it == owner.iterator_pairs.at(current_range).second and current_range != owner.iterator_pairs.size() - 1) {
					range_it = owner.iterator_pairs.at(++current_range).first;
				}
			}
			return *this;
		}
		multi_range_iterator operator++(int) {
			auto t = *this;
			operator++();
			return t;
		}
		multi_range_iterator& operator--() requires std::ranges::bidirectional_range<Range> {
			if (range_it == owner.iterator_pairs.at(current_range).first) {
				if (current_range == 0) {
					current_range = -1;
				} else {
					range_it = owner.iterator_pairs.at(--current_range).second;
				}
			}
			--range_it;
			return *this;
		}
		multi_range_iterator operator--(int) requires std::ranges::bidirectional_range<Range> {
			auto t = *this;
			operator--();
			return t;
		}
		//	todo: random access operators
	//	template <std::integral Int>
	//	multi_range_iterator& operator+=(Int s) requires std::ranges::random_access_range<Range> {
	//		if (s < 0) {
	//			return operator-=(-s);
	//		}
	//		while (s > 0) {

	//		}
	//	}
	//	template <std::integral Int>
	//	multi_range_iterator& operator-=(Int s) requires std::ranges::random_access_range<Range> {
	//		if (s < 0) {
	//			return operator+=(-s);
	//		}
	//	}
	};

	typedef multi_range_iterator iterator;


	multi_range_view() : iterator_pairs()  {}

	template <std::forward_iterator It>
		requires requires(It it) { { *it } -> std::common_with<Range>; }
	multi_range_view(It first, It last) : multi_range_view() {
		iterator_pairs.reserve(std::distance(first, last));
		std::for_each(first, last, [this] (auto&& range) {
			iterator_pairs.emplace_back(std::ranges::begin(range), std::ranges::end(range));
		});
	}

	iterator begin() {
		return iterator(*this, iterator_pairs.front().first, 0);
	}
	iterator end() {
		return iterator(*this, iterator_pairs.back().second, iterator_pairs.size() - 1);
	}
	std::reverse_iterator<iterator> rbegin() requires std::ranges::bidirectional_range<Range> {
		return std::reverse_iterator(iterator(*this, iterator_pairs.back().second, iterator_pairs.size() - 1));
	}
	std::reverse_iterator<iterator> rend() requires std::ranges::bidirectional_range<Range> {
		return std::reverse_iterator(iterator(*this, iterator_pairs.front().first, 0));
	}
};

template <std::forward_iterator It>
multi_range_view(It first, It last) -> multi_range_view<std::iter_value_t<It>>;

template <std::ranges::range Range>
using multi_range_iterator = typename multi_range_view<Range>::multi_range_iterator;

template <std::ranges::range Range>
bool operator==(multi_range_iterator<Range> lhs, multi_range_iterator<Range> rhs) {
	return lhs.current_range == rhs.current_range and lhs.range_it == rhs.range_it;
}

template <std::ranges::random_access_range Range>
auto operator<=>(multi_range_iterator<Range> lhs, multi_range_iterator<Range> rhs) {
	if (lhs.current_range != rhs.current_range) {
		return lhs.current_range <=> rhs.current_range;
	} else {
		return lhs.range_it <=> rhs.range_it;
	}
}

template <std::forward_iterator It, function_like<bool, decltype(*std::declval<It>()), decltype(*std::declval<It>())> F>
It for_each_adjacent(It begin, It end, F pred) {
	if (begin == end) {
		return end;
	}
	auto begin2 = begin;
	++begin2;
	for (; begin2 != end; ++begin, ++begin2) {
		if (not pred(*begin, *begin2)) {
			return begin;
		}
	}
	return end;
}

template <typename Set>
concept set_like = std::ranges::forward_range<Set> && requires (Set& s, const typename Set::key_type& k) {
	{ s.contains(k) } -> std::convertible_to<bool>;
	s.insert(k);
	s.erase(k);
};

template <typename Set>
concept unordered_set_like = set_like<Set>;

template <typename Range>
concept reservable_range = requires (Range& r) { r.reserve(size_t()); };

template <unordered_set_like Set>
Set& unordered_set_union(Set& s, const Set& o) {
	for (auto&& key : o) {
		s.insert(key);
	}
	return s;
}

//template <unordered_set_like Set>
//std::pair<Set&, Set&> unordered_set_union(Set& s, Set& o) {
//	Set& r = std::ranges::size(s) > std::ranges::size(o) ? s : o;
//	Set& ro = &r == &s ? o : s;
//	unordered_set_union(r, ro);
//	return { r, ro };
//}

template <unordered_set_like Set>
Set& unordered_set_difference(Set& s, const Set& o) noexcept {
	for (auto&& key : o) {
		s.erase(key);
	}
	return s;
}

template <unordered_set_like Set>
Set& unordered_set_intersection(Set& s, const Set& o) noexcept {
	for (auto it = s.begin(), end = s.end(); it != end; ) {
		auto eit = it;
		if (not o.contains(*it++)) {
			s.erase(eit);
		}
	}
	return s;
}

//template <unordered_set_like Set>
//std::pair<Set&, Set&> unordered_set_intersection(Set& s, Set& o) noexcept {
//	Set& r = std::ranges::size(s) < std::ranges::size(o) ? s : o;
//	Set& ro = &r == &s ? o : s;
//	unordered_set_intersection(r, ro);
//	return { r, ro };
//}

template <unordered_set_like Set>
Set& unordered_set_symmetric_difference(const Set& s, const Set& o, Set& r) {
	for (auto&& key : s | std::views::filter([&o](auto&& key) { return not o.contains(key); })) {
		r.insert(key);
	}
	for (auto&& key : o | std::views::filter([&s](auto&& key) { return not s.contains(key); })) {
		r.insert(key);
	}
	return r;
}

constexpr bool is_whitespace(size_t c) noexcept {
	return (c >= 0x08 and c <= 0xd) or c == ' ';
}

template <std::input_iterator It>
std::pair<std::string, It> read_input_stream_to_string(It it, It end) {
	size_t c;
	std::string result;
	while (it != end and is_whitespace((c = *it))) {
		++it;
	}
	while (it != end and not is_whitespace(c)) {
		result += c;
		++it;
		c = *it;
	}
	return { result, it };
}

template <typename Set>
std::vector<std::string_view> split_on_boundary(const std::string& s, const Set& set) {
	std::vector<std::string_view> result;
	auto it = s.begin();
	char c = *it++;
	bool first_contained = set.contains(c);
	size_t begin = 0;
	size_t end = 1;
	while (it != s.end()) {
		while (set.contains(*it) xor first_contained) {
			++end;
			++it;
		}
		result.emplace_back(s.begin() + begin, s.begin() + end);
		begin = end;
		while (not set.contains(*it) xor first_contained) {
			++end;
			++it;
		}
		result.emplace_back(s.begin() + begin, s.begin() + end);
		begin = end;
	}
	return result;
}

//template <typename Ret, typename...Args>
//consteval auto to_functor(Ret(*callable)(Args...)) {
//	struct {
//		Ret(*callable)(Args...);
//		constexpr Ret operator()(Args&&...args) const noexcept {
//			return callable(std::forward<Args>(args)...);
//		}
//	} result{callable};
//	return result;
//};

template <std::ranges::random_access_range Range>
Range substr(const Range& r, std::ranges::range_size_t<Range> i, std::ranges::range_size_t<Range> j) {
	return Range(std::ranges::begin(r) + i, std::ranges::begin(r) + std::min(std::ranges::size(r), j));
}

template <std::ranges::forward_range Range>
Range concat(Range r) noexcept {
	return std::move(r);
}

template <std::ranges::forward_range Range, std::ranges::forward_range Range2, typename...Args>
Range concat(Range r, const Range2& r2, Args&&...args) {
	r.insert(r.end(), r2.begin(), r2.end());
	return concat(std::move(r), std::forward<Args>(args)...);
}

template <typename CharT, typename CharTraits, typename Alloc>
std::string encode_as_utf8(const std::basic_string<CharT, CharTraits, Alloc>& s) {
	using namespace boost::locale::utf;
	std::string result;
	result.reserve(s.size());
	auto it = s.begin();
	while (it != s.end()) {
		size_t w = utf_traits<char8_t>::width(*it);
		result.resize(result.size() + w);
		utf_traits<char8_t>::encode(*it, result.end() - w);
		++it;
	}
	return result;
}

template <std::bidirectional_iterator It1, std::input_iterator It2>
It1 replace_subrange(It1 srbegin, It1 srend, It1 end, It2 repbegin, It2 repend) noexcept {
	size_t c = 0;
	while (srbegin != srend and repbegin != repend) {
		*srbegin++ = *repbegin++;
		++c;
	}
	if (repbegin == repend) {
		return std::move(srend, end, srbegin);
	}
	auto realend = end;
	std::advance(realend, c);
	std::move_backward(srbegin, end, realend);
	while (repbegin != repend) {
		*srbegin++ = *repbegin++;
	}
	return srbegin;
}

template <std::input_iterator It>
struct range_lookahead {
	typedef typename std::iterator_traits<It>::value_type value_type;
	typedef typename std::iterator_traits<It>::difference_type difference_type;

	It m_it;
	It m_end;
	queue<value_type> m_lookahead;

	range_lookahead(const It&& it, const It&& end, size_t lookahead) requires (not std::forward_iterator<It>)
		: m_it(it), m_end(end), m_lookahead(lookahead) {
		increase_lookahead(lookahead);
	}

	range_lookahead(It it, It end, size_t lookahead) requires std::forward_iterator<It> : m_it(it), m_end(end), m_lookahead(lookahead) {
		increase_lookahead(lookahead);
	}

	range_lookahead(const range_lookahead&) = delete;
	range_lookahead& operator=(const range_lookahead&) = delete;

	bool has_next() const noexcept {
		return m_it != m_end;
	}

	bool empty() const noexcept {
		return size() == 0;
	}

	const queue<value_type>& lookahead() const noexcept {
		return m_lookahead;
	}
	queue<value_type>& lookahead() noexcept {
		return m_lookahead;
	}

	const value_type& peek() const noexcept {
		return lookahead().peek();
	}

	void increase_lookahead() {
		if (has_next()) {
			m_lookahead.push(*m_it);
			++m_it;
		}
	}

	void increase_lookahead(size_t n) {
		for (; n != 0; --n) {
			increase_lookahead();
		}
	}

	size_t size() const noexcept {
		return m_lookahead.size();
	}

	void advance() {
		lookahead().pop();
		if (has_next()) {
			lookahead().push(std::move(*m_it));
			++m_it;
		}
	}

	void advance(size_t n) {
		for (; n != 0; --n) {
			advance();
		}
	}

	auto left() const noexcept requires std::forward_iterator<It> {
		return std::distance(m_it, m_end);
	}

	template <std::integral Int>
	typename queue<value_type>::reference operator[](Int i) noexcept {
		return lookahead()[i];
	}
	template <std::integral Int>
	typename queue<value_type>::const_reference operator[](Int i) const noexcept {
		return lookahead()[i];
	}
};

template <std::ranges::input_range Range, set_like Set = std::unordered_set<std::ranges::range_value_t<Range>>>
Set range_to_set(const Range& r) {
	return Set(std::ranges::begin(r), std::ranges::end(r));
}

template <typename T, set_like Set = std::unordered_set<T>, std::ranges::input_range Range>
Set range_to_set(const Range& r) {
	return Set(std::ranges::begin(r), std::ranges::end(r));
}
template <set_like Set, std::ranges::input_range Range>
Set range_to_set(const Range& r) {
	return Set(std::ranges::begin(r), std::ranges::end(r));
}

template <set_like Set, std::input_iterator It>
std::pair<Set, bool> obtain_charset(It begin, It end) {
	using CharT = typename std::iterator_traits<It>::value_type;
	static const std::unordered_map<CharT, std::string> predefined_charsets = {
		{'w', (word_string())},
		{'s', (whitespace_string())},
		{'a', (alphabet_string())},
		{'d', (digits_string())},
		{'x', (hex_digits_string())},
		{'o', (octal_digits_string())}
	};
	static const std::unordered_map<CharT, CharT> escape_chars = {
		//{'a', '\a'},
		{'b', '\b'},
		{'e', '\x1b'},
		{'t', '\t'},
		{'n', '\n'},
		{'v', '\v'},
		{'r', '\r'},
		{'f', '\f'},
		{'\\', '\\'},
		{'[', '['},
		{']', ']'},
	};
	bool comp = false;
	auto is_range = [](const range_lookahead<It>& la) -> bool {
		auto&& q = la.lookahead();
		if (q.size() < 3) {
			return false;
		}
		if (q[1] != '-' or q[0] > q[2]) {
			return false;
		}
		return true;
	};
	Set S;
	if constexpr (reservable_range<Set> and std::forward_iterator<It>) {
		S.reserve(std::distance(begin, end));
	}
	range_lookahead<It> rl(std::move(begin), std::move(end), 3);

	if (rl.empty()) {
		return {};
	}
	bool permit_last_bracket = false;
	if (rl.peek() == '[') {
		permit_last_bracket = true;
		rl.advance();
		if (rl.empty()) {
			return {};
		}
	}
	if (rl.peek() == '^') {
		comp = true;
		rl.advance();
	}
	Set P;
	bool init = false;
	while (not rl.empty()) {
		if (rl.peek() == '[') {
			return {};
		} else if (rl.peek() == ']') {
			if (permit_last_bracket) {
				break;
			} else {
				return {};
			}
		} else if (is_range(rl)) {
			CharT c0 = rl[0];
			CharT c1 = rl[2];
			for (; c0 <= c1; ++c0) {
				S.insert(c0);
			}
			rl.advance(2);
		} else if (rl.size() >= 2 and rl.peek() == '\\') {
			if (predefined_charsets.contains(rl[1])) {
				auto&& s = predefined_charsets.at(rl[1]);
				S.insert(std::begin(s), std::end(s));
			} else if (predefined_charsets.contains(rl[1] + ('a' - 'A'))) {
				auto&& C = predefined_charsets.at(rl[1] + ('a' - 'A'));
				if (not init) {
					P = Set(C.begin(), C.end());
					init = true;
				} else {
					unordered_set_intersection(P, Set(C.begin(), C.end()));
				}
			} else if (escape_chars.contains(rl[1])) {
				S.insert(escape_chars.at(rl[1]));
			} else {
				return {};
			}
			rl.advance();
		} else {
			S.insert(rl.peek());
		}
		rl.advance();
	}
	if (init) {
		unordered_set_difference(P, S);
		comp ^= true;
		return { P, comp };
	} else {
		return { S, comp };
	}
}

template <std::input_iterator It, typename CharT = typename std::iterator_traits<It>::value_type>
std::pair<std::unordered_set<CharT>, bool> obtain_charset(It begin, It end) {
	return obtain_charset<std::unordered_set<CharT>>(begin, end);
}
template <set_like Set, typename CharT>
std::pair<Set, bool> obtain_charset(const CharT* c) {
	return obtain_charset<Set>(c, c + std::char_traits<CharT>::length(c));
}
template <typename CharT>
std::pair<std::unordered_set<CharT>, bool> obtain_charset(const CharT* c) {
	return obtain_charset(c, c + std::char_traits<CharT>::length(c));
}

template <typename CharT, std::integral Int>
std::basic_string<CharT> operator*(const std::basic_string<CharT>& s, Int count) {
	if (count < 0) {
		return {};
	}
	std::basic_string<CharT> result;
	for (; count != 0; --count) {
		result += s;
	}
	return result;
}

template <typename CharT, std::integral Int>
std::basic_string<CharT> operator*(Int count, const std::basic_string<CharT>& s) {
	return s * count;
}

template <typename CharT, std::integral Int>
std::basic_string<CharT>& operator*=(std::basic_string<CharT>& s, Int count) {
	return s = s * count;
}

template <size_t First = 0, typename Tuple>
Tuple& tuple_partial_argument_construct(Tuple& t) {
	return t;
}

template <size_t First = 0, typename Tuple, typename...Args>
Tuple& tuple_partial_argument_construct(Tuple& t, auto&& val, Args&&...args) {
	if constexpr (First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = std::forward<decltype(val)>(val);
		return tuple_partial_argument_construct<First + 1>(t, std::forward<Args>(args)...);
	}
}

template <size_t First = 0, typename Tuple>
Tuple tuple_partial_argument_construct(Tuple&& t) {
	return t;
}

template <size_t First = 0, typename Tuple, typename...Args>
Tuple tuple_partial_argument_construct(Tuple&& t, auto&& val, Args&&...args) {
	if constexpr (First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = std::forward<decltype(val)>(val);
		return tuple_partial_argument_construct<First + 1>(std::move(t), std::forward<Args>(args)...);
	}
}

template <size_t First = 0, size_t Count = (size_t)-1, typename Tuple>
Tuple& tuple_partial_fill(Tuple& t, const auto& val) {
	if constexpr (Count == 0 or First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = val;
		return tuple_partial_fill<First + 1, Count - 1>(t, val);
	}
}

template <size_t First = 0, size_t Count = (size_t)-1, typename Tuple, typename...Args>
Tuple& tuple_partial_construct(Tuple& t, const Args&...args) {
	if constexpr (Count == 0 or First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = std::tuple_element_t<First, Tuple>(args...);
		return tuple_partial_construct<First + 1, Count - 1>(t, args...);
	}
}

template <size_t First = 0, size_t Count = (size_t)-1, typename Tuple>
Tuple& tuple_partial_max_fill(Tuple& t) {
	if constexpr (Count == 0 or First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = std::numeric_limits<std::tuple_element_t<First, Tuple>>::max();
		return tuple_partial_max_fill(t);
	}
}

template <size_t First = 0, size_t Count = (size_t)-1, typename Tuple>
Tuple tuple_partial_min_fill(Tuple& t) {
	if constexpr (Count == 0 or First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = std::numeric_limits<std::tuple_element_t<First, Tuple>>::min();
		return tuple_partial_min_fill<First + 1, Count - 1>(t);
	}
}

template <size_t First = 0, size_t Count = (size_t)-1, typename Tuple>
Tuple tuple_partial_fill(Tuple&& t, const auto& val) {
	if constexpr (Count == 0 or First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = val;
		return tuple_partial_fill<First + 1, Count - 1>(std::move(t), val);
	}
}

template <size_t First = 0, size_t Count = (size_t)-1, typename Tuple, typename...Args>
Tuple tuple_partial_construct(Tuple&& t, const Args&...args) {
	if constexpr (Count == 0 or First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = std::tuple_element_t<First, Tuple>(args...);
		return tuple_partial_construct<First + 1, Count - 1>(std::move(t), args...);
	}
}

template <size_t First = 0, size_t Count = (size_t)-1, typename Tuple>
Tuple tuple_partial_max_fill(Tuple&& t) {
	if constexpr (Count == 0 or First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = std::numeric_limits<std::tuple_element_t<First, Tuple>>::max();
		return tuple_partial_max_fill<First + 1, Count - 1>(std::move(t));
	}
}

template <size_t First = 0, size_t Count = (size_t)-1, typename Tuple>
Tuple tuple_partial_min_fill(Tuple&& t) {
	if constexpr (Count == 0 or First >= std::tuple_size_v<Tuple>) {
		return t;
	} else {
		std::get<First>(t) = std::numeric_limits<std::tuple_element_t<First, Tuple>>::min();
		return tuple_partial_min_fill<First + 1, Count - 1>(std::move(t));
	}
}

template <typename OrderedSet, typename...Args>
auto partial_equal_range(OrderedSet&& set, Args&&...known_keys) {
	typedef typename std::decay_t<OrderedSet>::key_type tuple;
	return std::pair(
		set.lower_bound(tuple_partial_min_fill<sizeof...(known_keys)>(tuple_partial_argument_construct(tuple(), std::forward<Args>(known_keys)...))),
		set.upper_bound(tuple_partial_max_fill<sizeof...(known_keys)>(tuple_partial_argument_construct(tuple(), std::forward<Args>(known_keys)...)))
	);
}
template <typename T>
concept tuple_like = requires (T& t) {
	std::get<0>(t);
	{ std::tuple_size_v<T> } -> std::convertible_to<size_t>;
	typename std::tuple_element_t<0, T>;
};
template <typename T>
concept scalar_like = std::is_scalar_v<T>;
template <typename T>
concept boolean_testable = scalar_like<T> or std::convertible_to<T, bool>;
template <typename Map>
concept map_like = std::ranges::forward_range<Map> and tuple_like<typename Map::value_type>
and requires (Map& m, const typename Map::key_type& k, const typename Map::mapped_type& v) {
	m[k] = v;
	m.erase(k);
	{ m.contains(k) } -> boolean_testable;
};

template <typename T>
concept standard_string = std::same_as<
	T,
	std::basic_string<
		typename T::value_type,
		typename T::traits_type,
		typename T::allocator_type
	>
>;

consteval char32_t u32horizontalline() noexcept {
	return U'─';
}
consteval char32_t u32verticalline() noexcept {
	return U'│';
}
consteval char32_t u32crossline() noexcept {
	return U'┼';
}

static_assert(standard_string<std::string>);
static_assert(not tuple_like<int>);
static_assert(tuple_like<std::pair<int, int>>);
static_assert(tuple_like<std::tuple<int>>);
static_assert(tuple_like<std::tuple<int, int, int>>);
static_assert(set_like<std::unordered_set<int>>);
static_assert(set_like<std::set<int>>);
static_assert(map_like<std::unordered_map<int, int>>);
static_assert(map_like<std::map<int, int>>);
template <typename T>
std::u32string to_u32string(const T& t);
template <size_t Start, size_t End, tuple_like Tuple>
std::u32string tuple_subseq_elements_to_string(std::u32string prev, const Tuple& t) {
	if constexpr (Start >= std::tuple_size_v<Tuple> or Start >= End) {
		return prev;
	} else {
		return tuple_subseq_elements_to_string<Start + 1, End>(prev += U", " + to_u32string<std::tuple_element_t<Start, Tuple>>(std::get<Start>(t)), t);
	}
}
template <std::ranges::input_range Range, std::invocable<typename std::ranges::range_value_t<Range>> Printer>
	requires requires (Printer p, const std::ranges::range_value_t<Range>& i, std::u32string s) { s += p(i); }
std::u32string sequence_to_u32string(const Range& r, Printer printer, char32_t left, char32_t right) {
	auto begin = std::ranges::begin(r);
	auto end = std::ranges::end(r);
	if (begin == end) {
		return { left, right };
	}
	std::u32string s;
	s += left;
	s += U' ';
	s += printer(*begin);
	++begin;
	for (; begin != end; ++begin) {
		s += U", ";
		s += printer(*begin);
	}
	s += U' ';
	s += right;
	return s;
}

template <typename Bitset>
concept bitset_like = requires (Bitset& b, size_t i) {
	{ b[i] } -> std::convertible_to<bool>;
	{ b.size() } -> std::convertible_to<size_t>;
};
static_assert(bitset_like<bitset<size_t>>);
template <typename CharT>
bool is_left_bracket(CharT c) {
	return c == '(' or c == '[' or c == '{';
}
template <typename CharT>
bool is_right_bracket(CharT c) {
	return c == ')' or c == ']' or c == '}';
}
template <standard_string String>
String structure_brackets(const String& s, String tab) {
	using char_type = typename String::value_type;
	size_t depth = 0;
	const String newline = { char_type('\n') };
	String result;
	result.reserve(s.size() * 2);
	for (auto&& line : s | std::views::split(newline)) {
		result += tab * depth;
		for (auto&& c : line) {
			if (is_left_bracket(c)) {
				++depth;
				result += c;
				result += newline;
				result += tab * depth;
			} else if (is_right_bracket(c)) {
				if (depth == 0) {
					return String();
				}
				--depth;
				result += newline;
				result += tab * depth;
				result += c;
			} else {
				result += c;
			}
		}
		result += newline;
	}
	return result;
}
template <standard_string String>
String structure_brackets(const String& s) {
	return structure_brackets(s, { typename String::value_type(' ') });
}
template <typename T>
std::u32string to_u32string(const T& t) {
	if constexpr (requires (const T& t) { std::to_string(t); }) {
		auto&& s = std::to_string(t);
		return std::u32string(s.begin(), s.end());
	} else if constexpr (tuple_like<T>) {
		if constexpr (std::tuple_size_v<T> == 0) {
			return U"()";
		} else {
			return U"( " + tuple_subseq_elements_to_string<1, std::tuple_size_v<T>>(to_u32string(std::get<0>(t)), t) + U" )";
		}
	} else if constexpr (set_like<T>) {
		return sequence_to_u32string(t, [](const std::ranges::range_value_t<T>& i) { return to_u32string(i); }, U'{', U'}');
	} else if constexpr (map_like<T>) {
		return sequence_to_u32string(t, [](const std::ranges::range_value_t<T>& t) {
			return to_u32string(std::get<0>(t)) + U" -> " + to_u32string(std::get<1>(t));
		}, U'{', U'}');
	} else if constexpr (standard_string<T>) {
		if constexpr (std::same_as<T, std::u32string>) {
			return t;
		} else {
			return std::u32string(t.begin(), t.end());
		}
	} else if constexpr (std::ranges::input_range<T>) {
		return sequence_to_u32string(t, [](const std::ranges::range_value_t<T>& i) { return to_u32string(i); }, U'[', U']');
	} else if constexpr (bitset_like<T>) {
		std::u32string s;
		size_t d4 = t.size() / 4;
		size_t r4 = t.size() % 4;
		for (size_t i = 1; i <= 4 - r4; ++i) {
			s += U'0';
		}
		for (size_t i = 1; i <= r4; ++i) {
			s += U'0' + (bool)t[t.size() - i];
		}
		for (; d4 != 0; --d4) {
			s += U'\'';
			for (size_t i = 1; i <= 4; ++i) {
				s += U'0' + (bool)t[d4 * 4 - i];
			}
		}
		return s;
			
	} else {
		t.thismemberdoesnotexistdonottrytomakethismemberexistibegyouthenextlinehasathrowexpressionanyway;
		throw;
	}
}


std::ostream& operator<<(std::ostream& stream, const std::u32string& string) {
	stream << encode_as_utf8(string);
	return stream;
}


} // namespace automata
#define AUTOMATA_TUPLE_STRUCT1(struct_name,tp0,nm0) struct struct_name : public std::tuple<tp0> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0>(std::forward<Args>(args)...) {}std::tuple<tp0>& decompose() { return *this; }const std::tuple<tp0>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }}
#define AUTOMATA_TUPLE_STRUCT2(struct_name,tp0,nm0,tp1,nm1) struct struct_name : public std::tuple<tp0,tp1> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1>& decompose() { return *this; }const std::tuple<tp0,tp1>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }}
#define AUTOMATA_TUPLE_STRUCT3(struct_name,tp0,nm0,tp1,nm1,tp2,nm2) struct struct_name : public std::tuple<tp0,tp1,tp2> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }}
#define AUTOMATA_TUPLE_STRUCT4(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }}
#define AUTOMATA_TUPLE_STRUCT5(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }}
#define AUTOMATA_TUPLE_STRUCT6(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }}
#define AUTOMATA_TUPLE_STRUCT7(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }}
#define AUTOMATA_TUPLE_STRUCT8(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }}
#define AUTOMATA_TUPLE_STRUCT9(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7,tp8,nm8) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }tp8& nm8() { return std::get<8>(*this); }const tp8& nm8()  const { return std::get<8>(*this); }}
#define AUTOMATA_TUPLE_STRUCT10(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7,tp8,nm8,tp9,nm9) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }tp8& nm8() { return std::get<8>(*this); }const tp8& nm8()  const { return std::get<8>(*this); }tp9& nm9() { return std::get<9>(*this); }const tp9& nm9()  const { return std::get<9>(*this); }}
#define AUTOMATA_TUPLE_STRUCT11(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7,tp8,nm8,tp9,nm9,tp10,nm10) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }tp8& nm8() { return std::get<8>(*this); }const tp8& nm8()  const { return std::get<8>(*this); }tp9& nm9() { return std::get<9>(*this); }const tp9& nm9()  const { return std::get<9>(*this); }tp10& nm10() { return std::get<10>(*this); }const tp10& nm10()  const { return std::get<10>(*this); }}
#define AUTOMATA_TUPLE_STRUCT12(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7,tp8,nm8,tp9,nm9,tp10,nm10,tp11,nm11) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }tp8& nm8() { return std::get<8>(*this); }const tp8& nm8()  const { return std::get<8>(*this); }tp9& nm9() { return std::get<9>(*this); }const tp9& nm9()  const { return std::get<9>(*this); }tp10& nm10() { return std::get<10>(*this); }const tp10& nm10()  const { return std::get<10>(*this); }tp11& nm11() { return std::get<11>(*this); }const tp11& nm11()  const { return std::get<11>(*this); }}
#define AUTOMATA_TUPLE_STRUCT13(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7,tp8,nm8,tp9,nm9,tp10,nm10,tp11,nm11,tp12,nm12) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }tp8& nm8() { return std::get<8>(*this); }const tp8& nm8()  const { return std::get<8>(*this); }tp9& nm9() { return std::get<9>(*this); }const tp9& nm9()  const { return std::get<9>(*this); }tp10& nm10() { return std::get<10>(*this); }const tp10& nm10()  const { return std::get<10>(*this); }tp11& nm11() { return std::get<11>(*this); }const tp11& nm11()  const { return std::get<11>(*this); }tp12& nm12() { return std::get<12>(*this); }const tp12& nm12()  const { return std::get<12>(*this); }}
#define AUTOMATA_TUPLE_STRUCT14(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7,tp8,nm8,tp9,nm9,tp10,nm10,tp11,nm11,tp12,nm12,tp13,nm13) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }tp8& nm8() { return std::get<8>(*this); }const tp8& nm8()  const { return std::get<8>(*this); }tp9& nm9() { return std::get<9>(*this); }const tp9& nm9()  const { return std::get<9>(*this); }tp10& nm10() { return std::get<10>(*this); }const tp10& nm10()  const { return std::get<10>(*this); }tp11& nm11() { return std::get<11>(*this); }const tp11& nm11()  const { return std::get<11>(*this); }tp12& nm12() { return std::get<12>(*this); }const tp12& nm12()  const { return std::get<12>(*this); }tp13& nm13() { return std::get<13>(*this); }const tp13& nm13()  const { return std::get<13>(*this); }}
#define AUTOMATA_TUPLE_STRUCT15(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7,tp8,nm8,tp9,nm9,tp10,nm10,tp11,nm11,tp12,nm12,tp13,nm13,tp14,nm14) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }tp8& nm8() { return std::get<8>(*this); }const tp8& nm8()  const { return std::get<8>(*this); }tp9& nm9() { return std::get<9>(*this); }const tp9& nm9()  const { return std::get<9>(*this); }tp10& nm10() { return std::get<10>(*this); }const tp10& nm10()  const { return std::get<10>(*this); }tp11& nm11() { return std::get<11>(*this); }const tp11& nm11()  const { return std::get<11>(*this); }tp12& nm12() { return std::get<12>(*this); }const tp12& nm12()  const { return std::get<12>(*this); }tp13& nm13() { return std::get<13>(*this); }const tp13& nm13()  const { return std::get<13>(*this); }tp14& nm14() { return std::get<14>(*this); }const tp14& nm14()  const { return std::get<14>(*this); }}
#define AUTOMATA_TUPLE_STRUCT16(struct_name,tp0,nm0,tp1,nm1,tp2,nm2,tp3,nm3,tp4,nm4,tp5,nm5,tp6,nm6,tp7,nm7,tp8,nm8,tp9,nm9,tp10,nm10,tp11,nm11,tp12,nm12,tp13,nm13,tp14,nm14,tp15,nm15) struct struct_name : public std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14,tp15> { template <typename...Args> struct_name(Args&&...args) : std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14,tp15>(std::forward<Args>(args)...) {}std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14,tp15>& decompose() { return *this; }const std::tuple<tp0,tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14,tp15>& decompose() const { return *this; }tp0& nm0() { return std::get<0>(*this); }const tp0& nm0()  const { return std::get<0>(*this); }tp1& nm1() { return std::get<1>(*this); }const tp1& nm1()  const { return std::get<1>(*this); }tp2& nm2() { return std::get<2>(*this); }const tp2& nm2()  const { return std::get<2>(*this); }tp3& nm3() { return std::get<3>(*this); }const tp3& nm3()  const { return std::get<3>(*this); }tp4& nm4() { return std::get<4>(*this); }const tp4& nm4()  const { return std::get<4>(*this); }tp5& nm5() { return std::get<5>(*this); }const tp5& nm5()  const { return std::get<5>(*this); }tp6& nm6() { return std::get<6>(*this); }const tp6& nm6()  const { return std::get<6>(*this); }tp7& nm7() { return std::get<7>(*this); }const tp7& nm7()  const { return std::get<7>(*this); }tp8& nm8() { return std::get<8>(*this); }const tp8& nm8()  const { return std::get<8>(*this); }tp9& nm9() { return std::get<9>(*this); }const tp9& nm9()  const { return std::get<9>(*this); }tp10& nm10() { return std::get<10>(*this); }const tp10& nm10()  const { return std::get<10>(*this); }tp11& nm11() { return std::get<11>(*this); }const tp11& nm11()  const { return std::get<11>(*this); }tp12& nm12() { return std::get<12>(*this); }const tp12& nm12()  const { return std::get<12>(*this); }tp13& nm13() { return std::get<13>(*this); }const tp13& nm13()  const { return std::get<13>(*this); }tp14& nm14() { return std::get<14>(*this); }const tp14& nm14()  const { return std::get<14>(*this); }tp15& nm15() { return std::get<15>(*this); }const tp15& nm15()  const { return std::get<15>(*this); }}


#endif
