#ifndef AUTOMATA_FA_HPP
#define AUTOMATA_FA_HPP

#include <stack>
#include <queue>
#include <list>
#include <set>
#include <boost/container/flat_set.hpp>
#include <ranges>
#include "util.hpp"
#include "lazy_ptr.hpp"

namespace automata {

#define CONSTEXPR

template <typename StateData>
struct state_set_struct {
	using state_vector = std::vector<StateData>;
	lazy_ptr<state_vector> states;

	state_set_struct()
		: states(make_lazy<state_vector>()) {
		states.write()->push_back(StateData());
	}

	CONSTEXPR size_t size() const noexcept {
		return states->size();
	}
	CONSTEXPR StateData& get(size_t q) {
		if (q >= size()) {
			return states.write()->operator[](0);
		} else {
			return states.write()->operator[](q);
		}
	}
	const StateData& get(size_t q) const noexcept {
		if (q >= size()) {
			return states->operator[](0);
		} else {
			return states->operator[](q);
		}
	}
	template <typename...Args>
	CONSTEXPR size_t push(Args&&...args) {
		auto p = states.write();
		p->emplace_back(std::forward<Args>(args)...);
		return size() - 1;
	}
	CONSTEXPR StateData pop() {
		auto p = states.write();
		StateData result = std::move(p->back());
		p->pop_back();
		return result;
	}
	CONSTEXPR void reserve(size_t c) {
		states.write()->reserve(c);
	}
	void resize(size_t c) {
		states.write()->resize(c);
	}
	void clear() {
		if (states.lone_owner()) {
			states.write()->resize(1);
		} else {
			*this = state_set_struct();
		}
	}
};
struct input_alphabet_struct {
	using unordered_map = std::unordered_map<size_t, size_t>;
	using vector = std::vector<size_t>;
	lazy_ptr<unordered_map> input_map;
	lazy_ptr<vector> reverse_input_map;

	CONSTEXPR input_alphabet_struct() : input_map(make_lazy<unordered_map>()), reverse_input_map(make_lazy<vector>()) {}


	CONSTEXPR bool contains(size_t c) const noexcept {
		return input_map->contains(c);
	}
	CONSTEXPR bool part_of_alphabet(size_t a) const noexcept {
		return a < size();
	}

	CONSTEXPR size_t convert(size_t c) const noexcept {
		if (input_map->contains(c)) {
			return input_map->at(c);
		} else {
			return 0;
		}
	}
	CONSTEXPR size_t operator[](size_t c) {
		if (contains(c)) {
			return convert(c);
		} else {
			return push(c);
		}
	}

	CONSTEXPR size_t push(size_t input) {
		if (contains(input)) {
			return (size_t)-1;
		}
		(*reverse_input_map.write()).push_back(input);
		(*input_map.write())[input] = size() - 1;
		return size() - 1;
	}

	CONSTEXPR size_t remove(size_t input) {
		if (not contains(input)) {
			return (size_t)-1;
		}
		size_t conv = convert(input);
		auto im = input_map.write();
		auto rim = reverse_input_map.write();
		im->erase(input);
		size_t max_conv = size() - 1;
		size_t max_input = convert_reverse(max_conv);
		im->at(max_input) = conv;
		rim->at(conv - 1) = max_input;
		rim->pop_back();
		return conv;
	}

	CONSTEXPR size_t convert_reverse(size_t a) const noexcept {
		if (a - 1 < reverse_input_map->size()) {
			return reverse_input_map->at(a - 1);
		} else {
			return (size_t)-1;
		}
	}

	CONSTEXPR size_t size() const noexcept {
		return reverse_input_map->size() + 1;
	}
	CONSTEXPR void reserve(size_t c) {
		input_map.write()->reserve(c);
		reverse_input_map.write()->reserve(c);
	}
	void clear() {
		if (input_map.lone_owner()) {
			input_map.write()->clear();
		} else if (not input_map->empty()) {
			input_map = make_lazy<unordered_map>();
		}
		if (reverse_input_map.lone_owner()) {
			reverse_input_map.write()->clear();
		} else if (not reverse_input_map->empty()) {
			reverse_input_map = make_lazy<vector>();
		}
	}
};

bool operator==(const input_alphabet_struct& lhs, const input_alphabet_struct& rhs) {
	if (lhs.input_map.read() == rhs.input_map.read()) {
		return true;
	}
	if (lhs.input_map->size() != rhs.input_map->size()) {
		return false;
	}
	for (auto&& [c, _] : lhs.input_map->size() < rhs.input_map->size() ? *lhs.input_map : *rhs.input_map) {
		if (not lhs.contains(c)) {
			return false;
		}
	}
	return true;
}
struct accepting_set_struct {
	using dynamic_bitset = boost::dynamic_bitset<size_t>;
	lazy_ptr<dynamic_bitset> accepting_states;
	size_t state_count : bit_count<size_t>() - 2;
	bool count_refresh : 1;
	bool comp : 1;

	accepting_set_struct()
		: accepting_states(make_lazy<dynamic_bitset>()), comp()
	{}

	void reserve(size_t c) {
		accepting_states.write()->reserve(c);
	}
	bool contains(size_t i) const noexcept {
		if (i >= accepting_states->size()) {
			return false ^ comp;
		} else {
			return accepting_states->at(i) ^ comp;
		}
	}
	size_t count() noexcept {
		if (count_refresh) {
			auto c = accepting_states->count();
			auto sz = accepting_states->size();
			state_count = comp ? sz - c : c;
		}
		return state_count;
	}
	void add(size_t i) {
		if (contains(i)) {
			return;
		}
		auto p = accepting_states.write();
		if (i >= p->size()) {
			p->resize(i + 1, comp);
		}
		p->set(i, not comp);
		if (not count_refresh) {
			state_count += 1;
		}
	}
	void remove(size_t i) {
		if (not contains(i)) {
			return;
		}
		auto p = accepting_states.write();
		if (i == p->size() - 1) {
			p->resize(i, comp);
		} else {
			p->set(i, comp);
			if (not count_refresh) {
				state_count -= 1;
			}
		}
	}
	void set(size_t i, bool b) {
		if (b) {
			return add(i);
		} else {
			return remove(i);
		}
	}
	void complement() noexcept {
		comp = not comp;
	}
	void unify(const accepting_set_struct& o) {
		auto p = accepting_states.write();
		if (comp and o.comp) {
			*p &= *o.accepting_states;
		} else if (comp and not o.comp) {
			*p -= *o.accepting_states;
		} else if (not comp and o.comp) {
			p->flip();
			*p &= *o.accepting_states;
			comp = true;
		} else {
			*p |= *o.accepting_states;
		}
		count_refresh = true;
	}
	void intersect(const accepting_set_struct& o) {
		auto p = accepting_states.write();
		if (comp and o.comp) {
			*p |= *o.accepting_states;
		} else if (comp and not o.comp) {
			p->flip();
			*p &= *o.accepting_states;
			comp = false;
		} else if (not comp and o.comp) {
			*p -= *o.accepting_states;
		} else {
			*p &= *o.accepting_states;
		}
		count_refresh = true;
	}
	void unify_ex(const accepting_set_struct& o) {
		auto p = accepting_states.write();
		*p ^= *o.accepting_states;
		if (comp ^ o.comp) {
			comp = true;
		} else {
			comp = false;
		}
		count_refresh = true;
	}
	void subtract(const accepting_set_struct& o) {
		auto p = accepting_states.write();
		if (comp and o.comp) {
			p->flip();
			*p &= *o.accepting_states;
			comp = false;
		} else if (comp and not o.comp) {
			*p &= *o.accepting_states;
			comp = false;
		} else if (not comp and o.comp) {
			*p |= *o.accepting_states;
			comp = true;
		} else {
			*p -= *o.accepting_states;
		}
		count_refresh = true;
	}
	CONSTEXPR void swap(size_t i, size_t j) {
		bool ival = contains(i);
		bool jval = contains(j);
		set(i, jval);
		set(j, ival);
	}
	template <std::invocable<size_t> Callback>
	CONSTEXPR void for_each(Callback callback) const {
		for (size_t q = 0; q < accepting_states->size(); ++q) {
			if (contains(q)) {
				callback(q);
			}
		}
	}
	void clear() {
		if (accepting_states.lone_owner()) {
			accepting_states.write()->clear();
		} else if (not accepting_states->empty()) {
			*this = accepting_set_struct();
		}
	}
};
struct delta_function_struct {
	template <typename T>
	using vector = std::vector<T>;
	typedef std::set<size_t> state_set_type;
	typedef vector<state_set_type> nfa_state;


	// O(|Σ||Q|^2)
	lazy_ptr<vector<nfa_state>> transition_table;
	// O(|Σ||Q|^2)
	lazy_ptr<vector<nfa_state>> reverse_transition_table;

	delta_function_struct() : transition_table(make_lazy<vector<nfa_state>>()), reverse_transition_table(make_lazy<vector<nfa_state>>()) {}

	size_t state_count() const noexcept {
		return transition_table->size();
	}
	void reserve_states(size_t c) {
		transition_table.write()->reserve(c);
		reverse_transition_table.write()->reserve(c);
	}
	void reserve_symbols(size_t c) {
		for (auto&& state : *transition_table.write()) {
			state.reserve(c);
		}
		for (auto&& state : *reverse_transition_table.write()) {
			state.reserve(c);
		}
	}
	bool deterministic(size_t e) const noexcept {
		return std::all_of(transition_table->begin(), transition_table->end(), [&] (const nfa_state& s) {
			size_t i = &s - transition_table->data();
			size_t e_sz = s.size() > e ? s.at(e).size() : 0;
			if (e_sz == 0 or *s.at(e).begin() == i + 1) {
				return std::all_of(s.begin(), s.end(), [](const state_set_type& s) { return s.size() <= 1; });
			} else {
				return false;
			}
		});
	}
	const state_set_type& lookup(size_t i, size_t a) const noexcept {
		size_t q = i - 1;
		static const state_set_type empty_set = {};
		if (q >= transition_table->size()) {
			return empty_set;
		} else if (a >= transition_table->at(q).size()) {
			return empty_set;
		} else {
			return transition_table->at(q)[a];
		}
	}
	const state_set_type& lookup_reverse(size_t i, size_t a) const noexcept {
		size_t q = i - 1;
		static const state_set_type empty_set = {};
		if (q >= reverse_transition_table->size()) {
			return empty_set;
		} else if (a >= reverse_transition_table->at(q).size()) {
			return empty_set;
		} else {
			return reverse_transition_table->at(q)[a];
		}
	}
	void symbol_switch(size_t conv, size_t new_alphabet_size) {
		if (conv >= new_alphabet_size) {
			return;
		}
		for (auto&& s : *transition_table.write()) {
			if (s.size() > new_alphabet_size) {
				auto fit = std::find_if(s.rbegin(), s.rend(), [](const state_set_type& i) { return i.size() != 0; });
				s.erase(fit.base() + 1, s.end());
				s[conv] = std::move(s.back());
				s.pop_back();
			}
		}
		for (auto&& s : *reverse_transition_table.write()) {
			if (s.size() > new_alphabet_size) {
				auto rfit = std::find_if(s.rbegin(), s.rend(), [](const state_set_type& s) { return s.size() != 0; });
				s.erase(rfit.base() + 1, s.end());
				s[conv] = std::move(s.back());
				s.pop_back();
			}
		}
	}
	size_t push_state(nfa_state s, nfa_state rs) {
		auto tt = transition_table.write();
		auto rtt = reverse_transition_table.write();
		tt->push_back(std::move(s));
		rtt->push_back(std::move(rs));
		return tt->size();
	}
	size_t push_state(nfa_state s) {
		return push_state(std::move(s), nfa_state());
	}
	size_t push_state() {
		return push_state(nfa_state());
	}
	template <std::forward_iterator It1, std::forward_iterator It2>
	void push_states(size_t n, It1 sbegin, It1 send, It2 rsbegin, It2 rsend) {
		for (size_t i = 0; i < n; i++) {
			if (sbegin != send) {
				if (rsbegin != rsend) {
					push_state(*sbegin, *rsbegin);
					++rsbegin;
				} else {
					push_state(*sbegin, nfa_state());
				}
				++sbegin;
			} else if (rsbegin != rsend) {
				push_state(nfa_state(), *rsbegin);
				++rsbegin;
			} else {
				push_state();
			}
		}
	}
	void add_transition(size_t src_i, size_t a, size_t dst_i) {
		size_t src_q = src_i - 1, dst_q = dst_i - 1;
		if (src_q >= transition_table->size() or dst_q >= reverse_transition_table->size()) {
			return;
		}
		auto tt = transition_table.write();
		auto rtt = reverse_transition_table.write();
		// assuming a is in the alphabet
		if (a >= tt->at(src_q).size()) {
			tt->at(src_q).resize(a + 1);
		}
		tt->at(src_q)[a].insert(dst_i);
		if (a >= rtt->at(dst_q).size()) {
			rtt->at(dst_q).resize(a + 1);
		}
		rtt->at(dst_q)[a].insert(src_i);
	}
	void remove_transition(size_t src_i, size_t a, size_t dst_i) {
		size_t src_q = src_i - 1, dst_q = dst_i - 1;
		if (src_q >= transition_table->size()
			or dst_q >= reverse_transition_table->size()
			or a >= transition_table->at(src_q).size()
			or not transition_table->at(src_q)[a].contains(dst_i)) {
			return;
		}
		auto tt = transition_table.write();
		auto rtt = reverse_transition_table.write();
		tt->at(src_q)[a].erase(dst_i);
		auto fit = std::find_if(tt->at(src_q).rbegin(), tt->at(src_q).rend(), [](const state_set_type& i) { return i.size() != 0; });
		// fit.base() = fit + 1
		tt->at(src_q).erase(fit.base(), tt->at(src_q).end());
		rtt->at(dst_q)[a].erase(src_i);
		auto rfit = std::find_if(rtt->at(dst_q).rbegin(), rtt->at(dst_q).rend(), [](const state_set_type& s) { return s.size() != 0; });
		rtt->at(dst_q).erase(rfit.base(), rtt->at(dst_q).end());
	}
	// O(|Σ|^2|Q|)
	void disjoin_state(size_t i) {
		size_t q = i - 1;
		if (q >= transition_table->size()) {
			return;
		}
		// O(|Σ|^2)
		for (size_t a = 0; a < transition_table->at(q).size(); ++a) {
			for (size_t s : transition_table->at(q)[a]) {
				remove_transition(q, a, s);
			}
		}
		// O(|Σ|^2|Q|)
		for (size_t a = 0; a < reverse_transition_table->at(q).size(); ++a) {
			// O(|Σ||Q|)
			for (size_t s : reverse_transition_table->at(q)[a]) {
				remove_transition(s, a, q);
			}
		}
	}
	void pop_state() {
		if (transition_table->size() == 0) {
			return;
		}
		size_t q = transition_table->size();
		auto tt = transition_table.write();
		auto rtt = reverse_transition_table.write();
		// O(|Σ|^2|Q|) (but if already disjoined beforehand, should take O(1))
		disjoin_state(q);
		tt->pop_back();
		rtt->pop_back();
	}
	bool isolated(size_t i) const noexcept {
		size_t q = i - 1;
		if (q >= transition_table->size()) {
			return false;
		}
		return transition_table->operator[](q).size() == 0
			and reverse_transition_table->operator[](q).size() == 0;
	}
	void clear() {
		if (transition_table.lone_owner()) {
			transition_table.write()->clear();
		} else if (not transition_table->empty()) {
			transition_table = make_lazy<vector<nfa_state>>();
		}
		if (reverse_transition_table.lone_owner()) {
			reverse_transition_table.write()->clear();
		} else if (not reverse_transition_table->empty()) {
			reverse_transition_table = make_lazy<vector<nfa_state>>();
		}
	}
};

template <typename It>
struct converting_iterator {
	typedef typename std::iterator_traits<It>::difference_type difference_type;
	typedef size_t value_type;

	static_assert(std::input_iterator<It>, "not an iterator");

	It it;
	const input_alphabet_struct* input_alphabet;
	mutable size_t s;
	
	converting_iterator(It it, const input_alphabet_struct* input_alphabet) : it(it), input_alphabet(input_alphabet), s(-1) {}
	converting_iterator& operator++() { ++it; s = (size_t)-1; return *this; }
	converting_iterator operator++(int) { auto t = *this; operator++(); return t; }
	size_t& operator*() const {
		if (s == (size_t)-1) {
			s = input_alphabet->convert(*it);
		}
		return s;
	}
};

template <
	typename StateData = empty_struct,
	typename TransitionData = empty_struct
>
struct finite_automaton {
	template <typename U>
	using lazy_ptr = lazy_ptr<U>;
	template <typename U, typename Ballocator = std::allocator<U>>
	using vector = std::vector<U, Ballocator>;
	using state_vector = std::vector<StateData>;
	template <typename U, typename V, typename Ballocator = std::allocator<std::pair<const U, V>>>
	using unordered_map = std::unordered_map<U, V, std::hash<U>, std::equal_to<U>, Ballocator>;
	using transition = std::tuple<size_t, size_t, size_t>;
	using transition_map = std::unordered_map<transition, TransitionData, boost::hash<transition>, std::equal_to<transition>>;
	using dynamic_bitset = boost::dynamic_bitset<size_t>;
	template <typename T, typename U>
	using pair = std::pair<T, U>;
	using state_id_data_pair = std::pair<size_t, StateData>;
	template <typename T>
	using unordered_set = std::unordered_set<T>;
	template <typename T>
	using stack = std::stack<T>;
	template <typename T>
	using queue = std::queue<T>;
	template <typename T>
	using list = std::list<T>;
	using state_set_type = delta_function_struct::state_set_type;

	typedef StateData state_type;
	typedef TransitionData transition_type;

	delta_function_struct delta_function;
	lazy_ptr<std::pair<TransitionData, transition_map>> transitions;
	input_alphabet_struct input_alphabet;
	accepting_set_struct accepting_set;
	state_set_struct<StateData> state_set;
	lazy_ptr<state_set_type> start_state_set;
	lazy_ptr<unordered_set<size_t>> removed_states;
	size_t m_epsilon_symbol;


	finite_automaton()
		: delta_function(),
		transitions(make_lazy<std::pair<TransitionData, transition_map>>()),
		input_alphabet(),
		accepting_set(),
		state_set(),
		start_state_set(make_lazy<state_set_type>()),
		removed_states(make_lazy<unordered_set<size_t>>()),
		m_epsilon_symbol(input_alphabet.push(epsilon())) {}

	template <typename OtherS, typename OtherT>
		requires (not std::is_same_v<StateData, OtherS> and not std::is_same_v<TransitionData, OtherT>)
	finite_automaton& operator=(const finite_automaton<OtherS, OtherT>& o) {
		clear();
		delta_function = o.delta_function;
		auto t = transitions.write();
		if constexpr (std::constructible_from<TransitionData, const OtherT&>) {
			t->first = TransitionData(o.transitions->first);
			for (auto&& [key, val] : o.transitions->second) {
				t->second.emplace(std::piecewise_construct, std::tuple(key), std::tuple(val));
			}
		} else {
			for (auto&& [key, val] : o.transitions->second) {
				t->second[key];
			}
		}
		input_alphabet = o.input_alphabet;
		accepting_set = o.accepting_set;
		state_set.resize(o.state_set.size());
		if constexpr (std::constructible_from<StateData, const OtherS&>) {
			o.for_each_state([&](size_t q) {
				state_set.get(q) = StateData(o.get_state(q));
			});
		}
		start_state_set = o.start_state_set;
		removed_states = o.removed_states;
		m_epsilon_symbol = o.m_epsilon_symbol;
		return *this;
	}
	template <typename OtherS, typename OtherT>
		requires (not std::is_same_v<StateData, OtherS> and not std::is_same_v<TransitionData, OtherT>)
	finite_automaton(const finite_automaton<OtherS, OtherT>& o) : finite_automaton() {
		*this = o;
	}
	void clear() {
		delta_function.clear();
		input_alphabet.clear();
		accepting_set.clear();
		state_set.clear();
		if (start_state_set.lone_owner()) {
			start_state_set.write()->clear();
		} else if (not start_state_set->empty()) {
			start_state_set = make_lazy<state_set_type>();
		}
		if (removed_states.lone_owner()) {
			removed_states.write()->clear();
		} else if (not removed_states->empty()) {
			removed_states = make_lazy<unordered_set<size_t>>();
		}
		m_epsilon_symbol = input_alphabet.push(epsilon());
	}

	static CONSTEXPR auto default_callback() {
		return [](size_t,size_t,size_t){};
	}
	static CONSTEXPR auto default_delta() {
		return [](size_t,size_t,size_t,size_t){return true;};
	}
	static CONSTEXPR auto default_predicate() {
		return [](size_t,size_t,size_t){return true;};
	}
	static CONSTEXPR auto default_state_callback() {
		return [](size_t){};
	}
	static CONSTEXPR auto default_state_predicate() {
		return [](size_t){return true;};
	}
	static CONSTEXPR auto default_dfs_state_visitor() {
		return [](size_t,const dynamic_bitset&){return true;};
	}
	static size_t unknown() noexcept { return (size_t)-1; }
	static CONSTEXPR size_t epsilon() noexcept { return (size_t)-2; }
	CONSTEXPR size_t epsilon_symbol() const noexcept { return m_epsilon_symbol; }
	CONSTEXPR size_t unknown_symbol() const noexcept { return 0; }
	static const state_set_type& empty_set() noexcept {
		static const state_set_type s;
		return s;
	}

	bool empty() const noexcept {
		return state_set.size() <= 1;
	}


	CONSTEXPR const state_set_type& start_states() const noexcept {
		return *start_state_set;
	}
	CONSTEXPR bool is_start_state(size_t s) const noexcept {
		return start_state_set->contains(s);
	}
	CONSTEXPR void add_start_state(size_t s) {
		if (s == 0 or not contains(s) or is_start_state(s)) {
			return;
		}
		start_state_set.write()->insert(s);
	}
	CONSTEXPR void remove_start_state(size_t s) {
		if (s == 0 or not contains(s) or not is_start_state(s)) {
			return;
		}
		start_state_set.write()->erase(s);
	}
	CONSTEXPR size_t start_state_count() const noexcept {
		return start_states().size();
	}
	CONSTEXPR size_t first_start_state() const noexcept {
		return start_state_set->empty() ? 0 : *start_state_set->begin();
	}

	CONSTEXPR size_t alphabet_size() const noexcept {
		return input_alphabet.size();
	}
	CONSTEXPR bool accepts_input(size_t symbol) const noexcept {
		return input_alphabet.contains(symbol);
	}
	CONSTEXPR bool in_alphabet(size_t a) const noexcept {
		return a < alphabet_size();
	}
	CONSTEXPR size_t convert_symbol(size_t i) const noexcept {
		return input_alphabet.convert(i);
	}
	CONSTEXPR size_t convert_reverse(size_t a) const noexcept {
		return input_alphabet.convert_reverse(a);
	}
	CONSTEXPR size_t push_symbol(size_t symbol) {
		return input_alphabet.push(symbol);
	}
	CONSTEXPR void remove_symbol(size_t symbol) noexcept {
		if (not input_alphabet.contains(symbol)) {
			return;
		}
		size_t conv = input_alphabet.remove(symbol);
		delta_function.symbol_switch(conv, input_alphabet.size());
	}

	CONSTEXPR void reserve_states(size_t c) {
		state_set.reserve(c);
		delta_function.reserve_states(c);
	}
	CONSTEXPR void reserve_transitions(size_t c) {
		transitions.write()->second.reserve(c);
	}
	CONSTEXPR void reserve_symbols(size_t c) {
		input_alphabet.reserve(c);
		delta_function.reserve_symbols(c);
	}
	CONSTEXPR size_t state_count() const noexcept {
		return state_set.size() - removed_states->size();
	}
	CONSTEXPR size_t state_id(const StateData& s) const noexcept {
		return std::addressof(s) - std::addressof(get_state(0));
	}
	CONSTEXPR bool contains(size_t state) const noexcept {
		return state < state_set.size() and not removed_states->contains(state);
	}
	template <typename...Args>
	size_t push_state(Args&&...args) {
		if (removed_states->size() > 0) {
			auto p = removed_states.write();
			auto it = p->begin();
			size_t id = *it;
			get_state(id) = StateData(std::forward<Args>(args)...);
			p->erase(it);
			return id;
		} else {
			delta_function.push_state();
			size_t id = state_set.push(std::forward<Args>(args)...);
			return id;
		}
	}
	CONSTEXPR StateData& get_state(size_t id) & noexcept {
		return state_set.get(id);
	}
	CONSTEXPR const StateData& get_state(size_t id) const& noexcept {
		return const_cast<finite_automaton&>(*this).get_state(id);
	}
	CONSTEXPR StateData get_state(size_t id) && noexcept {
		return std::move(((finite_automaton&)*this).get_state(id));
	}

	std::u32string to_string() const {
		size_t max_name_size = (size_t)std::log10(state_set.size()) + 1;
		size_t max_input_symbol = 0;
		for_each_input_symbol([&](size_t i, size_t) {
			max_input_symbol = std::max(max_input_symbol, i);
		});
		size_t max_alphabet_name_size = (size_t)std::log10(max_input_symbol) + 1 + 1;
		size_t max_entry_name_count = 0;
		size_t left_column_size = max_name_size + 4;
		for (auto&& state : *delta_function.transition_table) {
			for (auto&& set : state) {
				max_entry_name_count = std::max(max_entry_name_count, set.size());
			}
		}
		auto delim_size = [] (size_t entry_name_count) -> size_t {
			if (entry_name_count == 0) {
				// strlen("{}")
				return 2;
			} else {
				// strlen(", ") * (entry_name_count - 1) + strlen("{ ") + strlen(" }")
				return 2 * (entry_name_count - 1) + 4;
			}
		};
		auto max_delim_size = [=] () -> size_t {
			return delim_size(max_entry_name_count);
		};
		size_t max_entry_size = max_name_size * max_entry_name_count + max_delim_size() + 2;
		std::u32string unknown = U"unknown";
		size_t min_entry_size = std::max(unknown.length(), max_alphabet_name_size);
		size_t real_entry_size = std::max(max_entry_size, min_entry_size);
		auto right_space = [=] (size_t entry_size) -> size_t {
			return (real_entry_size - entry_size) / 2;
		};
		auto left_space = [=] (size_t entry_size) -> size_t {
			return (real_entry_size - entry_size) - right_space(entry_size);
		};
		size_t table_size = left_column_size + (real_entry_size + 1) * alphabet_size();

		namespace utf = boost::locale::utf;

		std::u32string result;
		std::u32string space = U" ";
		result += space * left_column_size;
		result += u32verticalline();
		result += space * left_space(unknown.length());
		result += unknown;
		result += space * right_space(unknown.length());
		result += u32verticalline();
		result += space * left_space(1);
		result += U'ε';
		result += space * right_space(1);
		for_each_input_symbol([&](size_t i, size_t) {
			result += u32verticalline();
			std::u32string name;
			if (utf::is_valid_codepoint(i)) {
				name = char32_t(i);
			} else {
				auto&& str = std::to_string(i);
				name += U'$';
				name.append(str.begin(), str.end());
			}
			result += space * left_space(name.length());
			result += name;
			result += space * right_space(name.length());
		});
		result += U'\n';
		std::u32string dash;
		dash += u32horizontalline();
		dash *= table_size;
		for (auto it = dash.begin() + left_column_size; it < dash.end(); it += real_entry_size + 1) {
			*it = u32crossline();
		}
		for_each_state([&](size_t q) -> bool {
			result += dash;
			result += U'\n';
			if (is_start_state(q)) {
				result += U"->";
			} else {
				result += U"  ";
			}
			if (accepts(q)) {
				result += U"* ";
			} else {
				result += U"  ";
			}
			auto name = to_u32string(q);
			result += space * (left_column_size - 4 - name.length());
			result += name;
			auto set_string = [&] (size_t q, size_t a) -> std::u32string {
				auto&& set = delta_function.lookup(q, a);
				if (set.empty()) {
					return U"{}";
				}
				std::u32string result;
				result += U"{ ";
				auto it = set.begin();
				result += to_u32string(*it);
				++it;
				for (; it != set.end(); ++it) {
					result += U", ";
					result += to_u32string(*it);
				}
				result += U" }";
				return result;
			};
			auto concat_set_string = [&](size_t a) {
				result += u32verticalline();
				auto ss = set_string(q, a);
				result += space * left_space(ss.length());
				result += ss;
				result += space * right_space(ss.length());
			};
			concat_set_string(0);
			concat_set_string(epsilon_symbol());
			for_each_input_symbol([&](size_t, size_t a) {
				concat_set_string(a);
			});
			result += U'\n';
			return true;
		});
		return result;
	}
	template <transition_predicate Pred>
	bool for_each_out_transition(size_t p, Pred pred) const {
		return for_each_out_delta(p, std::move(pred));
	}
	template <transition_predicate Pred>
	bool for_each_out_delta(size_t p, Pred pred) const {
		for (size_t a = 0; a < alphabet_size(); ++a) {
			if (not for_each_out_delta(p, a, pred)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	bool for_each_out_delta(size_t p, size_t a, Pred pred) const {
		for (size_t q : delta_function.lookup(p, a)) {
			if (not pred(p, a, q)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	bool for_each_out_transition(size_t p, size_t i, Pred pred) const {
		return for_each_out_delta(p, convert_symbol(i), std::move(pred));
	}
	template <transition_predicate Pred>
	bool for_each_in_transition(size_t q, Pred pred) const {
		return for_each_in_delta(q, std::move(pred));
	}
	template <transition_predicate Pred>
	bool for_each_in_delta(size_t q, Pred pred) const {
		for (size_t a = 0; a < alphabet_size(); ++a) {
			if (not for_each_in_delta(q, a, pred)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	bool for_each_in_delta(size_t q, size_t a, Pred pred) const {
		for (size_t p : delta_function.lookup_reverse(q, a)) {
			if (not pred(p, a, q)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	bool for_each_in_delta_mutable(size_t q, Pred pred) const {
		for (size_t a = 0; a < alphabet_size(); ++a) {
			if (not for_each_in_delta_mutable(q, a, pred)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	bool for_each_in_delta_mutable(size_t q, size_t a, Pred pred) const {
		state_set_type t = delta_function.lookup_reverse(q, a);
		for (size_t p : t) {
			if (not pred(p, a, q)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	bool for_each_out_delta_mutable(size_t q, Pred pred) const {
		for (size_t a = 0; a < alphabet_size(); ++a) {
			if (not for_each_out_delta_mutable(q, a, pred)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	bool for_each_out_delta_mutable(size_t p, size_t a, Pred pred) const {
		state_set_type t = delta_function.lookup(p, a);
		for (size_t q : t) {
			if (not pred(p, a, q)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	bool for_each_in_transition(size_t p, size_t i, Pred pred) const {
		return for_each_in_delta(p, convert_symbol(i), std::move(pred));
	}
	// contracts p to q
	template <transition_merger<TransitionData> TransitionMerger>
	StateData contract_state(size_t p, size_t q, TransitionMerger merger) {
		for_each_out_delta(p, [&](size_t, size_t a, size_t r) {
			if (r == q) {
				return true;
			}
			add_delta(q, a, r);
			merger(get_delta(q, a, r), q, a, r, p, a, r);
			return true;
		});
		for_each_in_delta(p, [&](size_t o, size_t a, size_t) {
			if (o == q) {
				return true;
			}
			add_delta(o, a, q);
			merger(get_delta(o, a, q), o, a, q, o, a, p);
			return true;
		});
		return remove_state(p);
	}
	StateData contract_state(size_t p, size_t q) {
		return contract_state(p, q, [&](TransitionData& t, size_t, size_t, size_t, size_t p, size_t a, size_t q) {
			t = std::move(get_delta(p, a, q));
		});
	}

	template <transition_callback Callback>
	void disjoin_state(size_t id, Callback callback) {
		for_each_out_delta_mutable(id, [&](size_t p, size_t a, size_t q) {
			callback(p, a, q);
			remove_delta(p, a, q);
			return true;
		});
		for_each_in_delta_mutable(id, [&](size_t p, size_t a, size_t q) {
			callback(p, a, q);
			remove_delta(p, a, q);
			return true;
		});
	}
	CONSTEXPR void disjoin_state(size_t id) {
		return disjoin_state(id, default_callback());
	}
	template <transition_callback Callback>
	CONSTEXPR StateData remove_state(size_t id, Callback callback) {
		if (id == 0 or not contains(id)) {
			return std::move(get_state(0));
		}
		disjoin_state(id, std::move(callback));
		remove_accepting(id);
		remove_start_state(id);
		if (id != state_set.size() - 1) {
			removed_states.write()->insert(id);
			return std::move(get_state(id));
		} else {
			delta_function.pop_state();
			auto r = state_set.pop();
			--id;
			while (removed_states->contains(id)) {
				removed_states.write()->erase(id);
				delta_function.pop_state();
				state_set.pop();
			}
			return r;
		}
	}
	CONSTEXPR StateData remove_state(size_t id) {
		return remove_state(id, default_callback());
	}
	CONSTEXPR bool accepts(size_t q) const noexcept {
		return contains(q) and accepting_set.contains(q);
	}
	CONSTEXPR void add_accepting(size_t q) {
		return accepting_set.add(q);
	}
	CONSTEXPR void remove_accepting(size_t q) {
		return accepting_set.remove(q);
	}
	CONSTEXPR void set_accepting(size_t q, bool b) {
		return accepting_set.set(q, b);
	}
	template <std::invocable<size_t> Callback>
	CONSTEXPR void for_each_accepting(Callback callback) {
		return accepting_set.for_each([f = std::move(callback), this] (size_t q) { if (contains(q)) f(q); });
	}
	static CONSTEXPR auto default_dfs_transition_visitor() {
		return [](size_t,size_t,size_t,dynamic_bitset&){return true;};
	}
	template <state_predicate Pred>
	static CONSTEXPR auto make_dfs_state_visitor(Pred pred) {
		return [pred = std::move(pred)] (size_t s, dynamic_bitset&) mutable { return pred(s); };
	}

	bool is_deterministic() const noexcept {
		return start_state_count() <= 1 and delta_function.deterministic(epsilon_symbol());
	}
	template <transition_callback F, typename I>
	state_set_type generalized_epsilon_delta(state_set_type s, F callback, I lookup) const {
		state_set_type visited;
		while (not s.empty()) {
			auto pit = s.begin();
			size_t p = *pit;
			s.erase(pit);
			if (visited.contains(p)) {
				continue;
			}
			visited.insert(p);
			for (size_t q : lookup(p, epsilon_symbol())) {
				if constexpr (std::convertible_to<std::invoke_result_t<F, size_t, size_t, size_t>, bool>) {
					if (not callback(p, epsilon_symbol(), q)) {
						return {};
					}
				} else {
					callback(p, epsilon_symbol(), q);
				}
				s.insert(q);
			}
		}
		return visited;
	}
	template <transition_callback F>
	state_set_type epsilon_delta(state_set_type s, F callback) const {
		return generalized_epsilon_delta(std::move(s), std::move(callback), [this] (size_t p, size_t a) { return delta_function.lookup(p, a); });
	}
	state_set_type epsilon_delta(state_set_type s) const {
		return epsilon_delta(std::move(s), default_callback());
	}
	state_set_type epsilon_delta(size_t q) const {
		return epsilon_delta(state_set_type{q});
	}
	template <transition_callback F>
	state_set_type reverse_epsilon_delta(state_set_type s, F callback) const {
		return generalized_epsilon_delta(std::move(s), std::move(callback), [this] (size_t p, size_t a) { return delta_function.lookup_reverse(p, a); });
	}
	template <transition_callback F, typename I>
	state_set_type generalized_delta(state_set_type s, size_t a, F callback, I lookup) const {
		if (s.empty() or not std::all_of(s.begin(), s.end(), [this](size_t q) { return contains(q); }) or not in_alphabet(a)) {
			return {};
		}
		s = generalized_epsilon_delta(std::move(s), callback, lookup);
		state_set_type t;
		for (size_t p : s) {
			for (size_t q : lookup(p, a)) {
				callback(p, a, q);
				t.insert(q);
			}
		}
		return generalized_epsilon_delta(std::move(t), std::move(callback), std::move(lookup));
	}
	size_t deterministic_delta(size_t q, size_t a) const {
		state_set_type s = delta(q, a);
		return s.empty() ? (a == epsilon_symbol() ? q : 0) : *s.begin();
	}
	template <std::input_iterator It, predicate_function<size_t, size_t, size_t, size_t> F>
	size_t deterministic_delta_ex(size_t q, It begin, It end, F pred) const {
		return deterministic_delta_ex(q, begin, end, std::move(pred), 0);
	}
	template <std::input_iterator It, predicate_function<size_t, size_t, size_t, size_t> F>
	size_t deterministic_delta_ex(size_t q, It begin, It end, F pred, size_t c) const {
		if (begin == end) {
			return q;
		}
		size_t a = *begin;
		size_t r = deterministic_delta(q, a);
		if (not pred(q, a, r, begin) or r == 0) {
			return 0;
		}
		return deterministic_delta_ex(r, ++begin, end, std::move(pred), ++c);
	}
	size_t deterministic_transition_function(size_t q, size_t a) const {
		return deterministic_delta(q, convert_symbol(a));
	}
	template <std::input_iterator It, predicate_function<size_t, size_t, size_t, size_t> F>
	size_t deterministic_transition_function_ex(size_t q, It begin, It end, F pred) const {
		return deterministic_delta_ex(q, converting_iterator<It>(begin, &input_alphabet), converting_iterator<It>(end, &input_alphabet), std::move(pred));
	}
	template <transition_callback F>
	state_set_type delta(size_t q, size_t a, F callback) const {
		return delta(state_set_type(&q, &q + 1), a, std::move(callback));
	}
	state_set_type delta(state_set_type q, size_t a) const {
		return delta(std::move(q), a, default_callback());
	}
	template <transition_callback F>
	state_set_type delta(state_set_type q, size_t a, F callback) const {
		return generalized_delta(
				std::move(q), a, std::move(callback),
				[this](size_t q, size_t a) { return delta_function.lookup(q, a); });
	}
	state_set_type delta(size_t q, size_t a) const {
		return delta(q, a, default_callback());
	}
	template <transition_callback F>
	state_set_type transition_function(size_t q, size_t i, F callback) const {
		return delta(q, convert_symbol(i), std::move(callback));
	}
	state_set_type transition_function(size_t q, size_t i) const {
		return transition_function(q, i, default_callback());
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<state_set_type&, size_t> ClosCallback,
		std::predicate<state_set_type&, size_t> NextCallback,
		std::predicate<size_t, size_t, const state_set_type&, size_t> SymbolCallback,
		typename I
	>
	state_set_type generalized_delta_ex(
		state_set_type s,
		It begin, It end,
		TransitionCallback tcallback,
		ClosCallback ccallback,
		NextCallback ncallback,
		SymbolCallback scallback,
		size_t pos,
		I lookup
	) const {
		auto ecall = [&](size_t p, size_t a, size_t q) -> bool { return tcallback(p, a, q, pos); };
		s = generalized_epsilon_delta(std::move(s), ecall, lookup);
		ccallback(s, pos);
		if (begin == end) {
			return s;
		}
		size_t a = *begin;
		state_set_type t;
		for (size_t p : s) {
			auto&& s = lookup(p, a);
			for (size_t q : s) {
				if (not tcallback(p, a, q, pos)) {
					return {};
				}
				t.insert(q);
			}
			if (not scallback(p, a, s, pos)) {
				return {};
			}
		}
		if (not ncallback(t, pos + 1)) {
			return {};
		}
		if (t.empty()) {
			return t;
		}
		return generalized_delta_ex(
			std::move(t), ++begin, end, std::move(tcallback), std::move(ccallback), std::move(ncallback), std::move(scallback), ++pos, std::move(lookup));
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<state_set_type&, size_t> ClosCallback,
		std::predicate<state_set_type&, size_t> NextCallback,
		std::predicate<size_t, size_t, const state_set_type&, size_t> SymbolCallback
	>
	state_set_type delta_ex(
		state_set_type s, It begin, It end, TransitionCallback tcallback, ClosCallback ccallback, NextCallback ncallback, SymbolCallback scallback
	) const {
		return generalized_delta_ex(
				std::move(s), begin, end, std::move(tcallback), std::move(ccallback), std::move(ncallback), std::move(scallback), 0,
				[this] (size_t q, size_t a) { return delta_function.lookup(q, a); });
	}
	static auto default_clos_callback() {
		return [](const state_set_type&,size_t) { return true; };
	}
	static auto default_next_callback() {
		return default_clos_callback();
	}
	static auto default_symbol_callback() {
		return [](size_t,size_t,const state_set_type&,size_t) { return true; };
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<state_set_type&, size_t> ClosCallback,
		std::predicate<state_set_type&, size_t> NextCallback
	>
	state_set_type delta_ex(state_set_type s, It begin, It end, TransitionCallback tcallback, ClosCallback ccallback, NextCallback ncallback) const {
		return delta_ex(std::move(s), begin, end, std::move(tcallback), std::move(ccallback), std::move(ncallback), default_symbol_callback());
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<state_set_type&, size_t> ClosCallback
	>
	state_set_type delta_ex(state_set_type s, It begin, It end, TransitionCallback tcallback, ClosCallback ccallback) const {
		return delta_ex(std::move(s), begin, end, std::move(tcallback), std::move(ccallback), default_next_callback());
	}
	template <std::input_iterator It, std::predicate<size_t, size_t, size_t, size_t> TransitionCallback>
	state_set_type delta_ex(state_set_type s, It begin, It end, TransitionCallback tcallback) const {
		return delta_ex(std::move(s), begin, end, std::move(tcallback), default_clos_callback());
	}
	template <std::input_iterator It, std::predicate<size_t, size_t, size_t, size_t> F>
	state_set_type delta_ex(size_t q, It begin, It end, F callback) const {
		if (q == 0 or not contains(q)) {
			return {};
		}
		return delta_ex(state_set_type(&q, &q + 1), begin, end, std::move(callback));
	}
	template <std::input_iterator It>
	state_set_type delta_ex(size_t q, It begin, It end) const {
		return delta_ex(q, begin, end, default_delta());
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<state_set_type&, size_t> ClosCallback,
		std::predicate<state_set_type&, size_t> NextCallback,
		std::predicate<size_t, size_t, const state_set_type&, size_t> SymbolCallback
	>
	state_set_type transition_function_ex(
		state_set_type q, It begin, It end, TransitionCallback tcallback, ClosCallback ccallback, NextCallback ncallback, SymbolCallback scallback
	) const {
		return delta_ex(
			std::move(q), converting_iterator<It>(begin, &input_alphabet), converting_iterator<It>(end, &input_alphabet),
			std::move(tcallback), std::move(ccallback), std::move(ncallback), std::move(scallback)
		);
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<state_set_type&, size_t> ClosCallback,
		std::predicate<state_set_type&, size_t> NextCallback
	>
	state_set_type transition_function_ex(state_set_type q, It begin, It end, TransitionCallback tcallback, ClosCallback ccallback, NextCallback ncallback) const {
		return transition_function_ex(std::move(q), begin, end, std::move(tcallback), std::move(ccallback), std::move(ncallback), default_symbol_callback());
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<state_set_type&, size_t> ClosCallback
	>
	state_set_type transition_function_ex(state_set_type q, It begin, It end, TransitionCallback tcallback, ClosCallback ccallback) const {
		return transition_function_ex(std::move(q), begin, end, std::move(tcallback), std::move(ccallback), default_next_callback());
	}
	template <std::input_iterator It, std::predicate<size_t, size_t, size_t, size_t> F>
	state_set_type transition_function_ex(size_t q, It begin, It end, F callback) const {
		return transition_function_ex(state_set_type{q}, begin, end, std::move(callback));
	}
	template <std::input_iterator It>
	state_set_type transition_function_ex(size_t q, It begin, It end) const {
		return transition_function_ex(q, begin, end, default_delta());
	}

	template <transition_callback Callback>
	state_set_type reverse_delta(state_set_type s, size_t a, Callback callback) const {
		return generalized_delta(
				std::move(s), a, std::move(callback),
				[this] (size_t q, size_t a) { return delta_function.lookup_reverse(q, a); });
	}
	template <transition_callback Callback>
	state_set_type reverse_delta(size_t q, size_t a, Callback callback) const {
		if (q == 0 or not contains(q) or not in_alphabet(a)) {
			return {};
		}
		return reverse_delta(state_set_type(&q, &q + 1), a, std::move(callback));
	}
	state_set_type reverse_delta(size_t q, size_t a) const {
		return reverse_delta(q, a, default_callback());
	}
	template <std::input_iterator It, predicate_function<size_t, size_t, size_t, size_t> F>
	state_set_type reverse_delta_ex(state_set_type s, It begin, It end, F callback) const {
		return generalized_delta_ex(
				std::move(s), begin, end, std::move(callback), 0,
				[this] (size_t q, size_t a) { return delta_function.lookup_reverse(q, a); });
	}
	template <std::input_iterator It, predicate_function<size_t, size_t, size_t, size_t> F>
	state_set_type reverse_delta_ex(size_t q, It begin, It end, F callback) const {
		if (q == 0 or not contains(q)) {
			return {};
		}
		return reverse_delta_ex(state_set_type(&q, &q + 1), begin, end, std::move(callback));
	}
	template <std::input_iterator It>
	state_set_type reverse_delta_ex(size_t q, It begin, It end) const {
		return reverse_delta_ex(q, begin, end, default_delta());
	}

	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<const state_set_type&, size_t> ClosCallback,
		std::predicate<const state_set_type&, size_t> NextCallback,
		std::predicate<size_t, size_t, const state_set_type&, size_t> SymbolCallback
	>
	bool simulate(It begin, It end, TransitionCallback tcallback, ClosCallback ccallback, NextCallback ncallback, SymbolCallback scallback) {
		state_set_type final = transition_function_ex(start_states(), begin, end, std::move(tcallback), std::move(ccallback), std::move(ncallback), std::move(scallback));
		if (final.empty()) {
			return accepts(0);
		} else {
			return std::any_of(final.begin(), final.end(), [this](size_t q) { return accepts(q); });
		}
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<const state_set_type&, size_t> ClosCallback,
		std::predicate<const state_set_type&, size_t> NextCallback
	>
	bool simulate(It begin, It end, TransitionCallback tcallback, ClosCallback ccallback, NextCallback ncallback) {
		return simulate(begin, end, std::move(tcallback), std::move(ccallback), std::move(ncallback), default_symbol_callback());
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback,
		std::predicate<const state_set_type&, size_t> ClosCallback
	>
	bool simulate(It begin, It end, TransitionCallback tcallback, ClosCallback ccallback) {
		return simulate(begin, end, std::move(tcallback), std::move(ccallback), default_next_callback());
	}
	template <
		std::input_iterator It,
		std::predicate<size_t, size_t, size_t, size_t> TransitionCallback
	>
	bool simulate(It begin, It end, TransitionCallback tcallback) {
		return simulate(begin, end, std::move(tcallback), default_clos_callback());
	}
	template <std::input_iterator It>
	CONSTEXPR bool simulate(It begin, It end) {
		return simulate(begin, end, default_delta());
	}
	
	CONSTEXPR size_t transition_count() const noexcept {
		return transitions->second.size() + 1;
	}
	CONSTEXPR bool contains_delta(size_t src, size_t a, size_t dst) const noexcept {
		return transitions->second.contains(transition(src, a, dst));
	}
	CONSTEXPR bool contains_transition(size_t src, size_t i, size_t dst) const noexcept {
		return contains_delta(src, convert_symbol(i), dst);
	}
	CONSTEXPR bool valid_delta(size_t src, size_t a, size_t dst) const noexcept {
		return contains(src) and in_alphabet(a) and contains(dst);
	}
	CONSTEXPR bool valid_transition(size_t src, size_t i, size_t dst) const noexcept {
		return valid_delta(src, convert_symbol(i), dst);
	}
	template <typename...Args>
	CONSTEXPR void add_delta(size_t src, size_t a, size_t dst, Args&&...args) {
		if (src == 0 or dst == 0 or not valid_delta(src, a, dst) or contains_delta(src, a, dst)) {
			return;
		}
		transitions.write()->second.emplace(transition(src, a, dst), TransitionData(std::forward<Args>(args)...));
		delta_function.add_transition(src, a, dst);
	}
	template <typename...Args>
	CONSTEXPR void add_transition(size_t src, size_t i, size_t dst, Args&&...args) {
		return add_delta(src, convert_symbol(i), dst, std::forward<Args>(args)...);
	}
	CONSTEXPR TransitionData& get_delta(size_t src, size_t a, size_t dst) {
		if (contains_delta(src, a, dst)) {
			return transitions.write()->second.at(transition(src, a, dst));
		} else {
			return transitions.write()->first;
		}
	}
	CONSTEXPR const TransitionData& get_delta(size_t src, size_t a, size_t dst) const {
		if (contains_delta(src, a, dst)) {
			return transitions->second.at(transition(src, a, dst));
		} else {
			return transitions->first;
		}
		return transitions.at(transition(src, a, dst));
	}
	CONSTEXPR TransitionData& get_transition(size_t src, size_t i, size_t dst) {
		return get_delta(src, convert_symbol(i), dst);
	}
	CONSTEXPR const TransitionData& get_transition(size_t src, size_t i, size_t dst) const {
		return get_delta(src, convert_symbol(i), dst);
	}
	CONSTEXPR TransitionData remove_delta(size_t src, size_t a, size_t dst) {
		if (not contains_delta(src, a, dst)) {
			return std::move(transitions.write()->first);
		}
		TransitionData result = std::move(transitions.write()->second.extract(transition(src, a, dst)).mapped());
		delta_function.remove_transition(src, a, dst);
		return result;
	}
	CONSTEXPR TransitionData remove_transition(size_t src, size_t i, size_t dst) {
		return remove_delta(src, convert_symbol(i), dst);
	}

	template <dfs_state_visitor_predicate StateVisitor, dfs_transition_visitor_predicate TransitionVisitor, typename L>
	dynamic_bitset generalized_depth_first_search(
			StateVisitor state_pred, TransitionVisitor transition_pred, const state_set_type& s, dynamic_bitset visited, L lookup) const {
		visited.resize(state_count());
		stack<size_t> args;
		for (size_t q : s) {
			args.push(q);
		}
		while (not args.empty()) {
			size_t s = args.top();
			args.pop();
			if (visited[s]) {
				continue;
			}
			visited.set(s);
			if (not state_pred(s, visited)) {
				return visited;
			}
			for (ssize_t a = alphabet_size() - 1; a >= 0; --a) {
				if (lookup(s, a).empty() and not transition_pred(s, a, 0, visited)) {
					return visited;
				}
				for (size_t ns : lookup(s, a)) {
					if (not transition_pred(s, a, ns, visited)) {
						return visited;
					}
					args.push(ns);
				}
			} // for (ssize_t a = alphabet_size() - 1; ; --a)
		} // while (not args.empty())
		return visited;
	}
	template <dfs_state_visitor_predicate StateVisitor, dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset depth_first_search(StateVisitor state_pred, TransitionVisitor transition_pred, const state_set_type& st, dynamic_bitset visited) const {
		return generalized_depth_first_search(
				std::move(state_pred), std::move(transition_pred), st, std::move(visited), [this](size_t q, size_t a) { return delta_function.lookup(q, a); });
	}
	template <dfs_state_visitor_predicate StateVisitor, dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset depth_first_search(StateVisitor state_pred, TransitionVisitor transition_pred, const state_set_type& st) const {
		return depth_first_search(std::move(state_pred), std::move(transition_pred), st, dynamic_bitset());
	}
	template <dfs_state_visitor_predicate StateVisitor, dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset depth_first_search(StateVisitor state_pred, TransitionVisitor transition_pred) const {
		return depth_first_search(std::move(state_pred), std::move(transition_pred), first_start_state());
	}
	template <dfs_state_visitor_predicate StatePredicate>
	dynamic_bitset depth_first_state_search(StatePredicate pred, const state_set_type& st, dynamic_bitset visited) const {
		return depth_first_search(std::move(pred), default_dfs_transition_visitor(), st, std::move(visited));
	}
	template <dfs_state_visitor_predicate StatePredicate>
	dynamic_bitset depth_first_state_search(StatePredicate pred, const state_set_type& st) const {
		return depth_first_state_search(std::move(pred), st, dynamic_bitset());
	}
	template <dfs_state_visitor_predicate StatePredicate>
	dynamic_bitset depth_first_state_search(StatePredicate pred) const {
		return depth_first_state_search(std::move(pred), first_start_state());
	}
	template <dfs_transition_visitor_predicate Predicate> 
	dynamic_bitset depth_first_transition_search(Predicate pred, const state_set_type& st, dynamic_bitset visited) const {
		return depth_first_search(default_dfs_state_visitor(), std::move(pred), st, std::move(visited));
	}
	template <dfs_transition_visitor_predicate Predicate> 
	dynamic_bitset depth_first_transition_search(Predicate pred, const state_set_type& st) const {
		return depth_first_transition_search(default_dfs_state_visitor(), std::move(pred), st, dynamic_bitset());
	}
	template <dfs_transition_visitor_predicate Predicate> 
	dynamic_bitset depth_first_transition_search(Predicate pred) const {
		return depth_first_transition_search(default_dfs_state_visitor(), std::move(pred), start_states());
	}
	dynamic_bitset depth_first_search() const {
		return depth_first_search(default_dfs_state_visitor(), default_dfs_transition_visitor());
	}
	template <dfs_state_visitor_predicate StateVisitor, dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset reverse_depth_first_search(StateVisitor state_pred, TransitionVisitor transition_pred, const state_set_type& st, dynamic_bitset visited) const {
		return generalized_depth_first_search(
				std::move(state_pred), std::move(transition_pred), st, std::move(visited), [this](size_t q, size_t a) { return delta_function.lookup_reverse(q, a); });
	}
	template <dfs_state_visitor_predicate StateVisitor, dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset reverse_depth_first_search(StateVisitor state_pred, TransitionVisitor trans_pred, const state_set_type& st) const {
		return reverse_depth_first_search(std::move(state_pred), std::move(trans_pred), st, dynamic_bitset());
	}
	template <dfs_state_visitor_predicate StateVisitor, dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset reverse_depth_first_search(StateVisitor state_pred, TransitionVisitor trans_pred) const {
		return reverse_depth_first_search(std::move(state_pred), std::move(trans_pred), start_states());
	}
	template <dfs_state_visitor_predicate StateVisitor>
	dynamic_bitset reverse_depth_first_state_search(StateVisitor state_pred, const state_set_type& st, dynamic_bitset visited) const {
		return reverse_depth_first_search(std::move(state_pred), default_dfs_transition_visitor(), st, std::move(visited));
	}
	template <dfs_state_visitor_predicate StateVisitor>
	dynamic_bitset reverse_depth_first_state_search(StateVisitor state_pred, const state_set_type& st) const {
		return reverse_depth_first_state_search(std::move(state_pred), st, dynamic_bitset());
	}
	template <dfs_state_visitor_predicate StateVisitor>
	dynamic_bitset reverse_depth_first_state_search(StateVisitor state_pred) const {
		return reverse_depth_first_state_search(std::move(state_pred), start_states());
	}
	template <dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset reverse_depth_first_transition_search(TransitionVisitor trans_pred, const state_set_type& st, dynamic_bitset visited) const {
		return reverse_depth_first_search(default_dfs_state_visitor(), trans_pred, st, std::move(visited));
	}
	template <dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset reverse_depth_first_transition_search(TransitionVisitor trans_pred, const state_set_type& st) const {
		return reverse_depth_first_transition_search(std::move(trans_pred), st, dynamic_bitset());
	}
	template <dfs_transition_visitor_predicate TransitionVisitor>
	dynamic_bitset reverse_depth_first_transition_search(TransitionVisitor trans_pred) const {
		return reverse_depth_first_transition_search(std::move(trans_pred), start_states());
	}
	dynamic_bitset reverse_depth_first_search() const {
		return reverse_depth_first_search(default_dfs_state_visitor(), default_dfs_transition_visitor());
	}

	template <indexed_state_predicate Pred>
	CONSTEXPR bool for_each_state_indexed(Pred pred) const {
		size_t sz = state_set.size();
		for (size_t q = 0, i = 0; q < sz; ++q) {
			if (contains(q) and not pred(q, i++)) {
				return false;
			}
		}
		return true;
	}
	template <transition_predicate Pred>
	CONSTEXPR bool for_each_transition(Pred pred) const {
		for (auto&& [t, data] : transitions->second) {
			auto [src, a, dst] = t;
			// calling add_delta is UB
			if (not pred(src, a, dst)) {
				return false;
			}
		}
		return true;
	}
	template <std::invocable<size_t, size_t> Pred>
	bool for_each_input_symbol(Pred pred) const {
		for (size_t a = 1; a < alphabet_size(); ++a) {
			if (a == epsilon_symbol()) {
				continue;
			}
			size_t conv_sym = a;
			size_t sym = convert_reverse(a);
			if constexpr (std::predicate<Pred, size_t, size_t>) {
				if (not pred(sym, conv_sym)) {
					return false;
				}
			} else {
				pred(sym, conv_sym);
			}
		}
		return true;
	}
	template <state_predicate Pred>
	static CONSTEXPR auto make_trivial_indexed_state_predicate(Pred p) {
		return [p = std::move(p)](size_t q, size_t) { return p(q); };
	}
	template <state_predicate Pred>
	CONSTEXPR bool for_each_state(Pred pred) const {
		return for_each_state_indexed(make_trivial_indexed_state_predicate(std::move(pred)));
	}
	bool isolated(size_t q) const noexcept {
		return delta_function.isolated(q);
	}
	template <state_predicate Callback>
	bool for_each_isolated(Callback callback) {
		return for_each_state([&](size_t q) {
			if (not isolated(q)) {
				return true;
			}
			if (not callback(q)) {
				return false;
			}
		});

	}
	template <state_callback Callback>
	void remove_isolated(Callback callback) {
		for_each_isolated([&](size_t q) {
			callback(q);
			remove_state(q);
			return true;
		});
	}
	void remove_isolated() {
		return remove_isolated(default_state_callback());
	}
	template <state_callback Callback>
	void remove_unreachable(Callback callback) {
		dynamic_bitset visited = depth_first_search();
		for (size_t q = 1; q < visited.size(); q++) {
			if (not visited[q]) {
				callback(q);
				remove_state(q);
			}
		}
	}
	void remove_unreachable() {
		return remove_unreachable(default_state_callback());
	}
	template <state_callback Callback>
	void remove_dead(Callback callback) {
		dynamic_bitset visited;
		for_each_accepting([&visited, this](size_t q) {
			visited = reverse_depth_first_state_search(default_dfs_state_visitor(), state_set_type(&q, &q + 1), std::move(visited));
		});
		for (size_t q = 1; q < visited.size(); q++) {
			if (not visited[q]) {
				callback(q);
				remove_state(q);
			}
		}
	}
	void remove_dead() {
		return remove_dead(default_state_callback());
	}
	unordered_map<size_t, size_t> state_indices() const {
		unordered_map<size_t, size_t> states_to_indices;
		states_to_indices.reserve(state_count());
		for_each_state_indexed([&states_to_indices](size_t q, const StateData&, size_t i) -> bool {
			states_to_indices.emplace(q, i);
			return true;
		});
		return states_to_indices;
	}
	template <function_like<StateData, size_t> S, function_like<TransitionData, size_t, size_t, size_t> T>
	unordered_map<size_t, size_t> join(const finite_automaton& o, S s, T t) {
		unordered_map<size_t, size_t> result;
		result.reserve(o.state_count());
		reserve_states(state_count() + o.state_count());
		reserve_transitions(transition_count() + o.transition_count());
		o.for_each_input_symbol([this](size_t i, size_t) { push_symbol(i); });
		o.for_each_state([&](size_t q) {
			size_t nq = push_state(s(q));
			result.emplace(q, nq);
			return true;
		});
		o.for_each_transition([&](size_t p, size_t a, size_t q) {
			add_transition(result.at(p), o.convert_reverse(a), result.at(q), t(p, a, q));
			return true;
		});
		return result;
	}
	template <function_like<StateData, size_t> S, function_like<TransitionData, size_t, size_t, size_t> T>
	unordered_map<size_t, size_t> unify(const finite_automaton& o, S s, T t) {
		auto result = join(o, std::move(s), std::move(t));
		for (size_t s : o.start_states()) {
			add_start_state(result.at(s));
		}
		return result;
	}
	template <
		typename SData,
		typename TData,
		typename SAlloc,
		typename TAlloc,
		state_merger<SData> StateMerger,
		transition_merger<TData> TransitionMerger,
		typename SData1,
		typename TData1,
		typename SAlloc1,
		typename TAlloc1,
		typename SData2,
		typename TData2,
		typename SAlloc2,
		typename TAlloc2
	>
	friend finite_automaton< SData, TData > intersection(
		const finite_automaton<SData1, TData1>& lhs,
		const finite_automaton<SData2, TData2>& rhs,
		StateMerger state_merger, TransitionMerger transition_merger
	);
	template <
		typename SData,
		typename TData,
		typename SAlloc,
		typename TAlloc,
		boolean_operation Op,
		state_merger<SData> StateMerger,
		transition_merger<TData> TransitionMerger,
		typename SData1,
		typename TData1,
		typename SAlloc1,
		typename TAlloc1,
		typename SData2,
		typename TData2,
		typename SAlloc2,
		typename TAlloc2
	>
	friend finite_automaton< SData, TData > combination(
		const finite_automaton<SData1, TData1>& lhs,
		const finite_automaton<SData2, TData2>& rhs,
		StateMerger state_merger, TransitionMerger transition_merger,
		Op op
	);

	vector<list<size_t>> equal_states() const {
		if (not is_deterministic()) {
			return {};
		}
		if (state_count() <= 1) {
			return { { 0 } };
		}
		enum state_pair_status : uint8_t {
			MAYBE_EQUAL = 0x0,
			SURELY_EQUAL = 0x1,
			SURELY_NOT_EQUAL = 0x2,
			MARKED_NOT_EQUAL = 0x3,
		};
		using state_pair = pair<size_t, size_t>;
		auto get_state_pair_status = [](const dynamic_bitset& bitset, size_t index) -> state_pair_status {
			return (state_pair_status)((uint8_t)bitset.at(index * 2) | ((uint8_t)bitset.at(index * 2 + 1) << 1));
		};
		auto set_state_pair_status = [](dynamic_bitset& bitset, size_t index, state_pair_status val) -> void {
			bitset.set(index * 2, (uint8_t)val & 0x1);
			bitset.set(index * 2 + 1, (uint8_t)val & 0x2);
		};

		dynamic_bitset state_pairs;
		size_t state_count_sqr = state_count() * state_count();
		state_pairs.resize((state_count_sqr - state_count()));
		
		auto pair_index = [](const state_pair& p) -> size_t {
			state_pair ord = ordered_pair(p);
			if (ord.first == ord.second) {
				return (size_t)-1;
			}
			size_t n = ord.second - 1;
			return (n * n + n) / 2 + ord.first;
		};
		auto check_status = [&](const state_pair& p, state_pair_status s) -> bool {
			if (p.first == p.second) {
				return s == SURELY_EQUAL;
			}
			return get_state_pair_status(state_pairs, pair_index(p)) == s;
		};
		auto surely_equal = [&](const state_pair& p) -> bool { return check_status(p, SURELY_EQUAL); };
		auto surely_not_equal = [&](const state_pair& p) -> bool { return check_status(p, SURELY_NOT_EQUAL); };
		auto maybe_equal = [&](const state_pair& p) -> bool { return check_status(p, MAYBE_EQUAL); };
		auto marked_not_equal = [&](const state_pair& p) -> bool { return check_status(p, MARKED_NOT_EQUAL); };

		auto states_to_indices = state_indices();
		auto indices_to_states = reverse_mapping_exact(states_to_indices);

		// mark states whose acceptance doesn't match as not equal
		for_n_and_less_than_ith(state_count(), [&](size_t i, size_t j) -> bool {
			if (accepts(indices_to_states.at(i)) ^ accepts(indices_to_states.at(j))) {
				set_state_pair_status(state_pairs, pair_index(state_pair(i, j)), SURELY_NOT_EQUAL);
			}
			return true;
		});

		using adjacency_list = vector<size_t>;
		vector<adjacency_list> graph;
		graph.resize((state_count_sqr - state_count()) / 2);

		// link all input pairs
		for_n_and_less_than_ith(state_count(), [&](size_t i, size_t j) -> bool {
			if (!maybe_equal(state_pair(i, j))) {
				return true;
			}
			for_n(alphabet_size(), [&] (size_t a) {
				size_t di = states_to_indices.at(deterministic_delta(indices_to_states.at(i), a));
				size_t dj = states_to_indices.at(deterministic_delta(indices_to_states.at(j), a));
				if (surely_equal(state_pair(di, dj))) {
					return true;
				}
				graph.at(pair_index(di, dj)).push_back(pair_index(state_pair(i, j)));
				if (surely_not_equal(state_pair(di, dj))) {
					return false; // linking this pair to others doesn't matter at this point
				} else {
					return true;
				}
			});
			return true;
		});
		// perform depth first search for all non-equal states, marking all the states along the way as non-equal
		vector<size_t> dfs_args;
		dfs_args.reserve(graph.size());
		for_n_and_less_than_ith(state_count(), [&](size_t i, size_t j) -> bool {
			if (!surely_not_equal(i, j)) {
				return true;
			}
			dfs_args.push_back(pair_index(state_pair(i, j)));
			while (!dfs_args.empty()) {
				size_t s = dfs_args.back();
				dfs_args.pop_back();
				if (marked_not_equal(s)) {
					continue;
				}
				set_state_pair_status(state_pairs, s, MARKED_NOT_EQUAL);
				for (size_t i : graph.at(s)) {
					dfs_args.push_back(i);
				}
			}
			return true;
		});
		vector<list<size_t>> equal_state_sets;
		unordered_map<size_t, size_t> state_map;
		for_n_and_less_than_ith(state_count(), [&](size_t i, size_t j) -> bool {
			if (marked_not_equal(state_pair(i, j))) {
				return true;
			}
			bool bi = state_map.contains(i);
			bool bj = state_map.contains(j);
			if (bi && bj) {
				list<size_t>& li = equal_state_sets[state_map.at(i)];
				list<size_t>& lj = equal_state_sets[state_map.at(j)];
				li.splice(li.end(), lj);
				state_map.at(j) = state_map.at(i);
			} else if (bi) {
				equal_state_sets[state_map.at(i)].push_back(j);
			} else if (bj) {
				equal_state_sets[state_map.at(j)].push_back(i);
			} else {
				state_map.emplace(i, equal_state_sets.size());
				state_map.emplace(j, equal_state_sets.size());
				list<size_t>& l = equal_state_sets.emplace_back();
				l.push_back(i);
				l.push_back(j);
			}
		});
		auto empty_list_pred = [] (list<size_t>& l) { return !l.empty(); };
		auto eit = std::find_if(equal_state_sets.rbegin(), equal_state_sets.rend(), empty_list_pred);
		for (auto it = equal_state_sets.begin(); it < eit.base(); ++it) {
			if (it->empty()) {
				std::swap(*it, *eit);
				++eit;
				eit = std::find_if(eit, equal_state_sets.rend(), empty_list_pred);
			}
		}
		equal_state_sets.erase(++eit.base(), eit.end());
		for (list<size_t>& l : equal_state_sets) {
			for (size_t s : l) {
				for (size_t q : epsilon_delta(s) | std::views::filter([s](size_t q) { return s != q; })) {
					l.push_back(q);
				}
			}
		}
		return equal_state_sets;
	}
	template <state_merger<StateData> StateMerger, transition_merger<TransitionData> TransitionMerger>
	finite_automaton& minimize(StateMerger state_merger, TransitionMerger transition_merger, const vector<list<size_t>>& equal_states) {
		for (auto&& l : equal_states) {
			size_t target = *std::min_element(l.begin(), l.end());
			StateData& target_data = get_state(target);
			for (size_t q : l | std::views::filter([target](size_t q) { return q != target; })) {
				for (size_t a = 0; a < alphabet_size(); ++a) {
					size_t r = deterministic_delta(q, a);
					size_t tdst = deterministic_delta(target, a);
					transition_merger(get_delta(target, a, tdst), target, a, tdst, q, a, r);
					remove_delta(q, a, r);
				}
				state_merger(target_data, target, q);
				remove_state(q);
			}
		}
		return *this;
	}
	template <state_merger<StateData> StateMerger, transition_merger<TransitionData> TransitionMerger>
	finite_automaton& minimize(StateMerger state_merger, TransitionMerger transition_merger) {
		return minimize(std::move(state_merger), std::move(transition_merger), equal_states());
	}
	bool equals(const finite_automaton& o) const {
		finite_automaton t = *this;
		t.join(o, [](size_t) { return StateData(); }, [](size_t,size_t,size_t){ return TransitionData(); });
		vector<list<size_t>> equal = t.equal_states();
		for (list<size_t>& l : equal) {
			uint8_t bits = 0b00;
			for (size_t q : l) {
				bits |= contains(q) | !contains(q) << 1;
				if (bits == 0b11) {
					break;
				}
			}
			if (bits != 0b11) {
				return false;
			}
		}
		return true;
	}
	finite_automaton& complement() noexcept {
		if (is_deterministic()) {
			accepting_set.complement();
		}
	}
	template <transition_callback RemovalCallback, transition_callback AdditionCallback>
	finite_automaton& invert_transitions(size_t src, size_t dst, RemovalCallback rc, AdditionCallback ac) {
		if (not contains(src) or not contains(dst)) {
			return *this;
		}
		for (size_t a : std::views::iota(size_t(0), alphabet_size())
					  | std::views::filter([this](size_t a) { return a != epsilon_symbol(); })) {
			if (delta_function.lookup(src, a).contains(dst)) {
				rc(src, a, dst);
				remove_delta(src, a, dst);
			} else {
				add_delta(src, a, dst);
				ac(src, a, dst);
			}
		}
		return *this;
	}
	finite_automaton& invert_transitions(size_t src, size_t dst) {
		return invert_transitions(src, dst, default_callback(), default_callback());
	}
	template <state_merger<StateData> SM, transition_merger<TransitionData> TM>
	finite_automaton deterministic(SM sm, TM tm, size_t state_limit) const {
		if (is_deterministic()) {
			return *this;
		}
		finite_automaton result;
		result.input_alphabet = input_alphabet;
		std::unordered_map<state_set_type, size_t, set_hash<state_set_type>, ordered_set_equal_to<state_set_type>> set_map;
		set_map.emplace(state_set_type(), 0);
		std::stack<state_set_type> sets;
		size_t ns = result.push_state();
		result.add_start_state(ns);
		set_map.emplace(start_states(), ns);
		for_each_adjacent(start_states().begin(), start_states().end(), [&](size_t s0, size_t s1) {
			sm(result.get_state(ns), s0, s1);
			return true;
		});
		sets.push(start_states());
		while (!sets.empty() && set_map.size() < state_limit) {
			state_set_type set = std::move(sets.top());
			sets.pop();
			size_t s = set_map.at(set);
			for (size_t a = 0; a < alphabet_size(); ++a) {
				if (a != epsilon_symbol()) {
					set = epsilon_delta(std::move(set));
					state_set_type new_set = delta(set, a);
					size_t ns;
					if (not set_map.contains(new_set)) {
						ns = result.push_state();
						set_map.emplace(new_set, ns);
						result.set_accepting(ns, std::any_of(new_set.begin(), new_set.end(), [&](size_t q) { return accepts(q); }));
						for_each_adjacent(new_set.begin(), new_set.end(), [&](size_t s0, size_t s1) {
							sm(result.get_state(ns), s0, s1);
							return true;
						});
						sets.push(new_set);
					} else {
						ns = set_map.at(new_set);
					}
					result.add_delta(s, a, ns);
					for (size_t p : set) {
						for (size_t q : delta_function.lookup(p, epsilon_symbol())) {
							tm(result.get_delta(s, a, ns), s, a, ns, p, epsilon_symbol(), q);
						}
						for (size_t q : delta_function.lookup(p, a)) {
							tm(result.get_delta(s, a, ns), s, a, ns, p, a, q);
						}
					}
				}
			}
		}
		if (set_map.size() >= state_limit) {
			return finite_automaton();
		} else {
			return result;
		}
	}
};
template <
	typename SData,
	typename TData,
	typename SAlloc,
	typename TAlloc,
	state_merger<SData> StateMerger,
	transition_merger<TData> TransitionMerger,
	typename SData1,
	typename TData1,
	typename SAlloc1,
	typename TAlloc1,
	typename SData2,
	typename TData2,
	typename SAlloc2,
	typename TAlloc2
>
finite_automaton< SData, TData > intersection(
	const finite_automaton<SData1, TData1>& lhs,
	const finite_automaton<SData2, TData2>& rhs,
	StateMerger state_merger, TransitionMerger transition_merger
) {
	size_t lhs_state_count = lhs.state_count();
	size_t rhs_state_count = rhs.state_count();
	finite_automaton<SData, TData> result;
	size_t new_state_count = lhs_state_count * rhs_state_count;
	result.reserve_states(new_state_count);

	std::unordered_map<size_t, size_t> this_states_to_indices = lhs.state_indices();
	std::unordered_map<size_t, size_t> o_states_to_indices = rhs.state_indices();

	rhs.for_each_state([&](size_t q) -> bool {
		return lhs.for_each_state([&](size_t p) -> bool {
			if (q != 0 and p != 0) {
				size_t s = result.push_state();
				state_merger(result.get_state(s), p, q);
			} else {
				 state_merger(result.get_state(0), p, q);
			}
			return true;
		});
	});

	if (lhs.input_alphabet == rhs.input_alphabet) {
		result.input_alphabet = lhs.input_alphabet;
	} else {
		result.reserve_symbols(lhs.alphabet_size() + rhs.alphabet_size());
		for (auto&& [i, _] : *lhs.input_alphabet.input_map) {
			result.push_symbol(i);
		}
		for (auto&& [i, _] : *rhs.input_alphabet.input_map) {
			result.push_symbol(i);
		}
	}

	size_t alpha_size = result.alphabet_size();
	result.reserve_transitions(new_state_count * alpha_size);

	rhs.for_each_state_indexed([&](size_t q, size_t j) -> bool {
		return lhs.for_each_state_indexed([&](size_t p, size_t i) -> bool {
			size_t src = j * lhs_state_count + i;
			result.set_accepting(src, lhs.accepts(p) and rhs.accepts(q));
			for (size_t a = 0; a < alpha_size; ++a) {
				size_t c = result.convert_reverse(a);
				if (a == result.epsilon_symbol()) {
					for (size_t s : rhs.epsilon_closure(q)) {
						for (size_t r : lhs.epsilon_closure(p)) {
							size_t dst = o_states_to_indices.at(s) * lhs_state_count + this_states_to_indices.at(r);
							if (src == dst) {
								continue;
							}
							result.add_delta(src, a, dst);
							transition_merger(result.get_delta(src, a, dst), p, lhs.convert_symbol(c), r, q, rhs.convert_symbol(c), s);
						}
					}
				} else {
					for (size_t s : rhs.transition_function(q, c)) {
						for (size_t r : lhs.transition_function(p, c)) {
							size_t dst = o_states_to_indices.at(s) * lhs_state_count + this_states_to_indices.at(r);
							result.add_delta(src, a, dst);
							transition_merger(result.get_delta(src, a, dst), p, lhs.convert_symbol(c), r, q, rhs.convert_symbol(c), s);
						}
					}
				}
			}
			return true;
		});
	});

	for (size_t q : rhs.start_states()) {
		for (size_t p : lhs.start_states()) {
			result.add_start_state(p + q * lhs_state_count);
		}
	}

	return result;
}
template <
	typename SData,
	typename TData,
	typename SAlloc,
	typename TAlloc,
	boolean_operation Op,
	state_merger<SData> StateMerger,
	transition_merger<TData> TransitionMerger,
	typename SData1,
	typename TData1,
	typename SAlloc1,
	typename TAlloc1,
	typename SData2,
	typename TData2,
	typename SAlloc2,
	typename TAlloc2
>
finite_automaton< SData, TData > combination(
	const finite_automaton<SData1, TData1>& lhs,
	const finite_automaton<SData2, TData2>& rhs,
	StateMerger state_merger, TransitionMerger transition_merger,
	Op op
) {
	if (not lhs.is_deterministic() or not rhs.is_deterministic()) {
		return finite_automaton<SData, TData>();
	}
	size_t lhs_state_count = lhs.state_count();
	size_t rhs_state_count = rhs.state_count();
	finite_automaton<SData, TData> result;
	size_t new_state_count = lhs_state_count * rhs_state_count;
	result.reserve_states(new_state_count);

	std::unordered_map<size_t, size_t> this_states_to_indices = lhs.state_indices();
	std::unordered_map<size_t, size_t> o_states_to_indices = rhs.state_indices();

	rhs.for_each_state([&](size_t q) -> bool {
		return lhs.for_each_state([&](size_t p) -> bool {
			if (q != 0 or p != 0) {
				size_t s = result.push_state();
				state_merger(result.get_state(s), p, q);
			} else {
				 state_merger(result.get_state(0), p, q);
			}
			return true;
		});
	});

	if (lhs.input_alphabet == rhs.input_alphabet) {
		result.input_alphabet = lhs.input_alphabet;
	} else {
		result.reserve_symbols(lhs.alphabet_size() + rhs.alphabet_size());
		for (auto&& [i, _] : *lhs.input_alphabet.input_map) {
			result.push_symbol(i);
		}
		for (auto&& [i, _] : *rhs.input_alphabet.input_map) {
			result.push_symbol(i);
		}
	}

	size_t alpha_size = result.alphabet_size();
	result.reserve_transitions(new_state_count * alpha_size);

	rhs.for_each_state_indexed([&](size_t q, size_t j) -> bool {
		return lhs.for_each_state_indexed([&](size_t p, size_t i) -> bool {
			size_t src = j * lhs_state_count + i;
			result.set_accepting(src, op(lhs.accepts(p), rhs.accepts(q)));
			for (size_t a = 0; a < alpha_size; ++a) {
				if (a == result.epsilon_symbol()) {
					continue;
				}
				size_t c = result.convert_reverse(a);
				size_t r = lhs.deterministic_transition_function(p, c);
				size_t s = rhs.deterministic_transition_function(q, c);
				size_t dst = o_states_to_indices.at(s) * lhs_state_count + this_states_to_indices.at(r);
				result.add_delta(src, a, dst);
				transition_merger(result.get_delta(src, a, dst), p, lhs.convert_symbol(c), r, q, rhs.convert_symbol(c), s);
			}
			return true;
		});
	});
	if (lhs.start_state_count() != 0 and rhs.start_state_count() != 0) {
		result.add_start_state(lhs.first_start_state() + lhs_state_count * rhs.first_start_state());
	}

	return result;
}

template <typename It>
bool operator==(const converting_iterator<It>& lhs, const converting_iterator<It>& rhs) {
	return lhs.it == rhs.it;
}

static_assert(std::input_iterator<converting_iterator<const char*>>);

} // namespace automata

#endif // #ifndef AUTOMATA_FA_HPP
