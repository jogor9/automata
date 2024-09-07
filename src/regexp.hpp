// regexp -- regular expression
//
// todo: testing, optimization, dynamic/semi-dynamic trees
// also software stacks for recursive functions
#ifndef AUTOMATA_REGEXP_HPP
#define AUTOMATA_REGEXP_HPP

#include <iostream>
#include <vector>
#include <unordered_set>
#include <boost/container/flat_set.hpp>
#include <variant>
#include <ranges>
#include <map>
#include "util.hpp"
#include "dynamic_tree.hpp"
#include "bitset.hpp"
#include "fa.hpp"

namespace automata {

template <
	std::unsigned_integral StateInt, // should be able to hold at least the number of states of the resulting NFA plus one for the null state
	std::unsigned_integral GidInt, // should be able to hold at least the maximum index of a group
	std::unsigned_integral StringInt, // should be able to hold the size of the tested string
	std::unsigned_integral AlphaInt, // should be able to hold the size of the alphabet
	std::unsigned_integral InputInt // should be able to hold the maximum input symbol
>
struct optimized_nfa {

static std::vector<StateInt> transition_table(size_t state_count, size_t alphabet_size, size_t max_out_states) {
	std::vector<StateInt> result;
	result.resize(state_count * alphabet_size * max_out_states);
	return result;
}

AUTOMATA_TUPLE_STRUCT1(
	incomplete_capture,
	StringInt, begin
);

AUTOMATA_TUPLE_STRUCT2(
	completed_capture,
	StringInt, begin,
	StringInt, end
);

AUTOMATA_TUPLE_STRUCT2(
	state_data,
	incomplete_capture, incomplete,
	completed_capture, completed
);

static std::vector<state_data> state_set(size_t state_count, size_t max_gid) {
	std::vector<state_data> result;
	result.resize(state_count * max_gid);
	return result;
}

static std::vector<GidInt> ownership_table(size_t state_count, size_t max_shared) {
	std::vector<state_data> result;
	result.resize(state_count * state_count * max_shared);
	return result;
}

AUTOMATA_TUPLE_STRUCT2(
	hash_table_data,
	InputInt, key,
	AlphaInt, value
);
typedef std::vector<std::vector<hash_table_data>> hash_table;
static hash_table translation_table(size_t alphabet_size) {
	std::vector<std::vector<hash_table_data>> result;
	result.resize(std::bit_ceil(alphabet_size) * 2);
	for (auto&& v : result) {
		v.reserve(2);
	}
	return result;
}
static size_t hash_function(InputInt input) {
	return size_t(input) * 0xdeadbeef;
}
static size_t table_index(size_t hash, size_t table_size) {
	return hash & table_size - 1;
}

static AlphaInt translate(const hash_table& tt, InputInt in) {
	for (auto&& keyval : tt[table_index(hash_function(in))]) {
		if (keyval.key() == in) {
			return keyval.value();
		}
	}
	return AlphaInt();
}

static constexpr AlphaInt UNKNOWN_SYMBOL = AlphaInt();
static constexpr AlphaInt EPSILON_SYMBOL = UNKNOWN_SYMBOL + 1;

static AlphaInt add_input(hash_table& tt, InputInt in, AlphaInt alphabet_size) {
	if (translate(tt, in) != AlphaInt()) {
		return AlphaInt();
	}
	tt[table_index(hash_function(in))].emplace_back(in, alphabet_size);
	return alphabet_size + 1;
}



};

struct regexp_data {
	typedef boost::container::flat_set<char32_t> set_type;

	uintptr_t m_symbol;
	uint8_t m_masks;

	static constexpr uint8_t COMP_MASK    = 0b0001;
	static constexpr uint8_t CHARSET_MASK = 0b0010;
	static constexpr uint8_t CAPTURE_MASK = 0b0100;
	static constexpr uint8_t ANCHOR_MASK  = 0b1000;

	regexp_data() noexcept : m_symbol(), m_masks() {
		comp(false);
		charset(false);
		capture(false);
		anchor(false);
	}
	regexp_data(size_t s) noexcept : regexp_data() {
		symbol(s);
	}
	regexp_data(const set_type* s) noexcept : regexp_data() {
		symbol(s);
		charset(true);
	}

	void set_mask(uint8_t mask, bool b) noexcept {
		if (b) {
			m_masks |= mask;
		} else {
			m_masks &= ~mask;
		}
	}
	bool comp() const noexcept {
		return m_masks & COMP_MASK;
	}
	bool charset() const noexcept {
		return m_masks & CHARSET_MASK;
	}
	bool capture() const noexcept {
		return m_masks & CAPTURE_MASK;
	}
	bool anchor() const noexcept {
		return m_masks & ANCHOR_MASK;
	}
	void comp(bool b) noexcept {
		set_mask(COMP_MASK, b);
	}
	void charset(bool b) noexcept {
		set_mask(CHARSET_MASK, b);
	}
	void capture(bool b) noexcept {
		set_mask(CAPTURE_MASK, b);
	}
	void anchor(bool b) noexcept {
		set_mask(ANCHOR_MASK, b);
	}
	size_t symbol() const noexcept {
		return (size_t)m_symbol;
	}
	const set_type* charset_ptr() const noexcept {
		if (charset()) {
			return (const set_type*)m_symbol;
		} else {
			return nullptr;
		}
	}
	void symbol(size_t s) noexcept {
		m_symbol = (uintptr_t)s;
	}
	void symbol(const set_type* p) noexcept {
		m_symbol = (uintptr_t)p;
	}
	bool symbol_eq(size_t s) const noexcept {
		return not charset() and m_symbol == (uintptr_t)s;
	}
	bool symbol_ne(size_t s) const noexcept {
		return charset() or m_symbol != (uintptr_t)s;
	}
	bool symbol_lt(size_t s) const noexcept {
		return not charset() and m_symbol < (uintptr_t)s;
	}
};
struct regexp : protected dynamic_tree<regexp_data> { 
	typedef regexp_data::set_type set_type;
	typedef dynamic_tree<regexp_data> tree_type;
	typedef tree_type::node_type node_type;
	typedef tree_type base;

	regexp() : base() {}
	regexp(const base& b) : base(b) {}
	regexp(base&& b) : base(b) {}

	template <std::input_iterator It>
	regexp(It begin, It end) : regexp() {
		*this = parse_regexp(begin, end);
	}
	template <typename CharT>
	regexp(const CharT* c) : regexp(c, c + std::char_traits<CharT>::length(c)) {}

	template <typename T, typename U>
	regexp(finite_automaton<T, U> automaton) : regexp(std::move(automaton), 1 << 20) {}
	template <typename T, typename U>
	regexp(finite_automaton<T, U> automaton, size_t size_limit) : regexp() {
		*this = construct_from_automaton(std::move(automaton), size_limit);
	}

	static std::set<set_type, ordered_set_less<set_type>>& charsets() {
		static std::set<set_type, ordered_set_less<set_type>> cs;
		return cs;
	}

	bool structure_test() {
		return orphan_test() && oob_test() && link_test() && type_test();
	}

	bool orphan_test() {
		bool r = true;
		tree_type::traverse_out_of_order([&] (size_t i) {
			auto&& n = this->node(i);
			if (not is_root(i) and n.no_parent()) {
				std::cerr << "FAIL: node " << node(n) << " is not a root, but has no parent" << std::endl;
				r = false;
			}
		});
		if (not this->node(this->root()).no_parent()) {
			std::cerr << "FAIL: root node " << this->root() << " has a parent" << std::endl;
			r = false;
		}
		return r;
	}

	bool oob_test() {
		bool r = true;
		tree_type::traverse_out_of_order( [&] (size_t i) {
			auto&& n = this->node(i);
			if (not is_root(n) and not base::contains(n.parent())) {
				std::cerr << "FAIL: node " << node(n) << " has a parent " << n.parent() << " that does not exist" << std::endl;
				r = false;
			}
			if (n.has_left() && not base::contains(n.left())) {
				std::cerr << "FAIL: node " << node(n) << " has a left child " << n.left() << " that does not exist" << std::endl;
				r = false;
			}
			if (n.has_right() && not base::contains(n.right())) {
				std::cerr << "FAIL: node " << node(n) << " has a right child " << n.right() << " that does not exist" << std::endl;
				r = false;
			}
		});
		return r;
	}

	bool link_test() {
		bool r = true;
		tree_type::traverse_out_of_order( [&] (size_t i) {
			auto&& n = node(i);
			if (n.has_left() && node(n.left()).parent() != node(n)) {
				std::cerr << "FAIL: left child " << n.left() << " of node " << node(n) << " does not have the node as its parent" << std::endl;
				r = false;
			}
			if (n.has_right() && node(n.right()).parent() != node(n)) {
				std::cerr << "FAIL: right child " << n.right() << " of node " << node(n) << " does not have the node as its parent" << std::endl;
				r = false;
			}
		});
		return r;
	}

	bool type_test() {
		bool r = true;
		tree_type::traverse_out_of_order( [&] (size_t i) {
			auto&& n = node(i);
			switch (n->symbol()) {
			case CLOSURE:
				if (!n.has_left()) {
					std::cerr << "FAIL: node " << node(n) << " is a closure, but does not have a left child" << std::endl;
					r = false;
				} else if (node(n.left())->symbol() == EMPTY_STRING) {
					std::cerr << "FAIL: node " << node(n) << " is a closure, but its left child " << n.left() << " is an empty string" << std::endl;
					r = false;
				}
				if (n.has_right()) {
					std::cerr << "FAIL: node " << node(n) << " is a closure, but it has a right child " << n.right() << std::endl;
					r = false;
				}
				break;
			case CONCATENATION: case DISJUNCTION:
				if (!n.has_left()) {
					std::cerr << "FAIL: node " << node(n) << " is a concatenation/disjunction, but does not have a left child" << std::endl;
					r = false;
				} else {
					if (n->symbol() == CONCATENATION && node(n.left())->symbol() == EMPTY_STRING) {
						std::cerr << "FAIL: node " << node(n) << " is a concatenation, but its left child " << n.left() << " is an empty string" << std::endl;
						r = false;
					}
					//if (node(n.left())->symbol() == n->symbol()) {
					//	std::cerr << "FAIL: node " << node(n)
					//		<< " is a concatenation/disjunction, but its left child " << n.left() << " is of the same type" << std::endl;
					//	r = false;
					//}
				}
				if (!n.has_right()) {
					std::cerr << "FAIL: node " << node(n) << " is a concatenation/disjunction, but does not have a right child" << std::endl;
					r = false;
				} else if (n->symbol() == CONCATENATION && node(n.right())->symbol() == EMPTY_STRING) {
					std::cerr << "FAIL: node " << node(n) << " is a concatenation, but its right child " << n.right() << " is an empty string" << std::endl;
					r = false;
				}
				break;
			default:
				if (n.has_left()) {
					std::cerr << "FAIL: node " << node(n) << " is not compound, but it has a left child " << n.left() << std::endl;
					r = false;
				}
				if (n.has_right()) {
					std::cerr << "FAIL: node " << node(n) << " is not compound, but it has a right child " << n.right() << std::endl;
					r = false;
				}
			}
		});
		return r;
	}

	regexp& remove_captures() {
		base::traverse_out_of_order([&](size_t n) {
			node(n)->capture(false);
		});
		return *this;
	}

	static constexpr size_t
		DISJUNCTION = (size_t)-1,
		CONCATENATION = (size_t)-2,
		WILDCARD = (size_t)-3,
		EMPTY_STRING = (size_t)-4,
		CLOSURE = (size_t)-5,
		BEGINNING_OF_LINE = (size_t)-6,
		END_OF_LINE = (size_t)-7,
		WORD_BOUNDARY = (size_t)-8;

	static bool is_reserved(size_t symbol) {
		return symbol >= CLOSURE;
	}

	static regexp null_regexp() {
		return regexp();
	}

	static const regexp& empty_regexp() {
		static regexp re;
		if (re.size() == 0) {
			re.push_root(EMPTY_STRING);
		}
		return re;
	}
	static const regexp& wildcard_regexp() {
		static regexp re;
		if (re.size() == 0) {
			re.push_root(WILDCARD);
		}
		return re;
	}
	static const regexp& any_regexp() {
		static regexp re;
		if (re.size() == 0) {
			re = wildcard_regexp();
			re.push_root(CLOSURE);
		}
		return re;
	}
	static regexp symbol_regexp(size_t s) {
		regexp result;
		result.push_root(s);
		return result;
	}

	void remove_node(const node_type& n) {
		return remove_node(base::node(n));
	}
	void remove_node(size_t i) noexcept {
		auto&& n = node(i);
		if (base::is_root(n)) {
			*this = null_regexp();
			return;
		}
		size_t p = n.parent();
		node_type& parent = node(p);
		switch (parent->symbol()) {
		case CLOSURE: {
			if (not base::is_root(p) and node(parent.parent())->symbol() == CONCATENATION) {
				auto&& gp = node(parent.parent());
				if (p == gp.left()) {
					node(gp.right())->capture(gp->capture());
					return (void)base::replace_parent(gp.right());
				} else {
					node(gp.left())->capture(gp->capture());
					return (void)base::replace_parent(gp.left());
				}
			}
			parent->symbol(EMPTY_STRING);
			base::remove(i);
			return;
		}
		case CONCATENATION:
			return remove_node(p);
		case DISJUNCTION:
			if (not base::is_root(parent) and node(parent.parent())->symbol() == CONCATENATION
				and (
					(i == parent.left() and node(parent.right())->symbol() == EMPTY_STRING)
					or (i == parent.right() and node(parent.left())->symbol() == EMPTY_STRING)
					)
				) {
				auto&& gp = node(parent.parent());
				if (p == gp.left()) {
					node(gp.right())->capture(gp->capture());
					return (void)base::replace_parent(gp.right());
				} else {
					node(gp.left())->capture(gp->capture());
					return (void)base::replace_parent(gp.left());
				}
			}
			if (i == parent.left()) {
				node(parent.right())->capture(parent->capture());
				return (void)base::replace_parent(parent.right());
			} else {
				node(parent.left())->capture(parent->capture());
				return (void)base::replace_parent(parent.left());
			}
		}
	}

	template <typename A, typename B>
	static regexp construct_from_automaton(finite_automaton<A, B> automaton, size_t size_limit) {
		automaton.remove_unreachable();
		automaton.remove_dead();
		if (automaton.start_states().size() == 0) {
			return automaton.accepts(0) ? any_regexp() : null_regexp();
		} else if (automaton.start_states().size() > 1) {
			size_t new_start = automaton.push_state();
			for (size_t s : automaton.start_states()) {
				automaton.add_delta(new_start, automaton.epsilon_symbol(), s);
				automaton.remove_start_state(s);
			}
			automaton.add_start_state(new_start);
		}
		std::unordered_map<size_t, size_t> state_to_vertex;
		struct vertex {
			std::unordered_set<size_t> in;
			std::unordered_set<size_t> out;
			bool accepting;
		};
		std::vector<vertex> adjacency_lists;
		std::unordered_map<std::pair<size_t, size_t>, regexp, boost::hash<std::pair<size_t, size_t>>> edge_map;
		automaton.depth_first_transition_search([&](size_t p, size_t a, size_t q, const boost::dynamic_bitset<size_t>&) {
			size_t i = automaton.convert_reverse(a);
			if (!state_to_vertex.contains(p)) {
				state_to_vertex.emplace(p, adjacency_lists.size());
				adjacency_lists.emplace_back().accepting = automaton.accepts(p);
			}
			if (!state_to_vertex.contains(q)) {
				state_to_vertex.emplace(q, adjacency_lists.size());
				adjacency_lists.emplace_back().accepting = automaton.accepts(q);
			}
			p = state_to_vertex.at(p);
			q = state_to_vertex.at(q);
			adjacency_lists.at(p).out.insert(q);
			adjacency_lists.at(q).in.insert(p);
			if (a == automaton.epsilon_symbol()) {
				edge_map[std::pair(p, q)].unify(empty_regexp());
			} else if (a == automaton.unknown_symbol()) {
				regexp r;
				automaton.for_each_input_symbol([&](size_t i, size_t) {
					r.unify(symbol_regexp(i));
				});
				r.complement();
				edge_map[std::pair(p, q)].unify(std::move(r));
			} else {
				edge_map[std::pair(p, q)].unify(symbol_regexp(i));
			}
			return true;
		});
		edge_map[std::pair(size_t(0), size_t(0))] = wildcard_regexp();
		size_t start_v = state_to_vertex(automaton.first_start_state());
		size_t overall_size = 0;
		auto remove_v = [&] (vertex& v) {
			size_t s = &v - adjacency_lists.data();
			for (size_t p : v.in) {
				for (size_t q : v.out) {
					regexp P = edge_map.at(std::pair(p, s));
					regexp S = edge_map.contains(std::pair(s, s)) ? edge_map.at(std::pair(s, s)) : null_regexp();
					regexp Q = edge_map.at(std::pair(s, q));
					regexp& R = edge_map[std::pair(p, q)];
					S.to_closure();
					overall_size -= R.size();
					R.unify(std::move(P.concat(std::move(S)).concat(std::move(Q))));
					overall_size += R.size();
				}
			}
			for (size_t p : v.in) {
				adjacency_lists.at(p).out.erase(s);
				edge_map.erase(std::pair(p, s));
			}
			for (size_t q : v.out) {
				adjacency_lists.at(q).in.erase(s);
				edge_map.erase(std::pair(s, q));
			}
			edge_map.erase(std::pair(s, s));
		};
		for (auto&& v : adjacency_lists) {
			if (!v.accepting && (&v - adjacency_lists.data()) != start_v) {
				remove_v(v);
				if (overall_size > size_limit) {
					return null_regexp();
				}
			}
		}
		regexp result;
		for (size_t o : adjacency_lists.at(start_v)) {
			regexp R = edge_map.contains(std::pair(start_v, start_v)) ? edge_map.at(std::pair(start_v, start_v)) : null_regexp();
			regexp S = edge_map.at(std::pair(start_v, o));
			regexp U = edge_map.contains(std::pair(o, o)) ? edge_map.at(std::pair(o, o)) : null_regexp();
			regexp T = edge_map.contains(std::pair(o, start_v)) ? edge_map.at(std::pair(o, start_v)) : null_regexp();
			U.to_closure();
			result.unify(std::move(R.unify(S.concat(U).concat(std::move(T))).to_closure().concat(std::move(S)).concat(std::move(U))));
			remove_v(adjacency_lists.at(o));
			if (result.size() > size_limit) {
				return null_regexp();
			}
		}
		return result;
	}


	bool is_null() const noexcept {
		return base::size() == 0;
	}

	bool is_empty() const noexcept {
		return not is_null() and not node(root())->charset() and node(root())->symbol() == EMPTY_STRING;
	}
	regexp promote_to_regexp(const node_type& n) const {
		return regexp(base::subtree(n));
	}
	size_t root() const noexcept {
		return base::root();
	}
	size_t node(const node_type& n) const noexcept {
		return base::node(n);
	}
	node_type& node(size_t n) noexcept {
		return base::node(n);
	}
	const node_type& node(size_t n) const noexcept {
		return base::node(n);
	}
	void node_merge_with(size_t n, const regexp& o, size_t symbol) {
		if (symbol < CONCATENATION) {
			return;
		}
		base::join_with(n, (const base&)o, symbol);
	}
	regexp& merge_with(const regexp& o, size_t symbol) {
		if (symbol < CONCATENATION) {
			return *this;
		}
		node_merge_with(base::root(), (const base&)o, symbol);
		return *this;
	}
	regexp& concat(const regexp& o) {
		if (is_null()) {
			return *this;
		} else if (o.is_null()) {
			return *this = null_regexp();
		} else if (is_empty()) {
			return *this = o;
		} else if (o.is_empty()) {
			return *this;
		} else {
			return merge_with(o, CONCATENATION);
		}
	}
	regexp& concat(regexp&& o) {
		if (is_null()) {
			return *this;
		} else if (o.is_null()) {
			return *this = null_regexp();
		} else if (is_empty()) {
			return *this = std::move(o);
		} else if (o.is_empty()) {
			return *this;
		} else {
			return merge_with(o, CONCATENATION);
		}
	}
	regexp& unify(const regexp& o) {
		if (is_null()) {
			return *this = o;
		} else if (o.is_null()) {
			return *this;
		} else {
			return merge_with(o, DISJUNCTION);
		}
	}
	regexp& unify(regexp&& o) {
		if (is_null()) {
			return *this = std::move(o);
		} else if (o.is_null()) {
			return *this;
		} else {
			return merge_with(o, DISJUNCTION);
		}
	}
	regexp& to_closure() {
		if (is_null()) {
			return *this = empty_regexp();
		} else if (is_empty()) {
			return *this;
		} else if (node(root())->symbol() == CLOSURE) {
			return *this;
		} else {
			base::push_root(CLOSURE);
			return *this;
		}
	}
	regexp& to_prefix_search() {
		return concat(any_regexp());
	}
	regexp to_suffix_search() {
		return regexp(any_regexp()).concat(*this);
	}
	regexp to_search() {
		return to_suffix_search().to_prefix_search();
	}
	static regexp repeat(const regexp& re, size_t n) {
		if (n == 0) {
			return empty_regexp();
		}
		regexp result = re;
		for (size_t i = 1; i < n; ++i) {
			result.concat(re);
		}
		return result;
	}
	regexp& repeat(size_t n) {
		return *this = repeat(std::move(*this), n);
	}
	regexp& complement() noexcept {
		auto&& r = node(root());
		r->comp(r->comp() ^ true);
		return *this;
	}
	regexp& intersect(const regexp& o) {
		if (is_null()) {
			return *this;
		} else if (o.is_null()) {
			return *this = o;
		} else {
			complement();
			unify(o);
			auto&& n = base::child(root(), 1);
			n->comp(n->comp() ^ true);
			complement();
			return *this;
		}
	}
	static regexp nth_closure(regexp re, size_t n) {
		if (n == 0) {
			return empty_regexp();
		}
		regexp result = empty_regexp();
		regexp concat = re;
		for (size_t i = 0; i < n; ++i) {
			result.unify(concat);
			concat.concat(re);
		}
		return result;
	}
	static regexp char_regexp(size_t c) {
		if (is_reserved(c)) {
			return null_regexp();
		}
		regexp result;
		result.push_root(c);
		return result;
	}
	template <std::forward_iterator It>
	static regexp charset_regexp(It begin, It end) {
		if (begin == end) {
			return empty_regexp();
		}
		regexp result;
		auto&& [set, comp] = obtain_charset<set_type>(begin, end);
		result.node(result.push_root(&*charsets().insert(set).first))->comp(comp);
		return result;
	}

	static regexp charset_regexp(const char* s) {
		return charset_regexp(s, s + std::char_traits<char>::length(s));
	}

	regexp& capture() noexcept {
		if (not is_null()) {
			node(root())->capture(true);
		}
		return *this;
	}

	template <std::input_iterator It>
	static regexp parse_regexp(It begin, It end);

	std::basic_string<char32_t> node_to_string(size_t i) const {
		static const std::unordered_set<char32_t> escape_chars = {
			'$', '^', '*', '(', ')', '{', '}', '[', ']', '\\', '|', '?', '.', '+'
		};
		const node_type& n = node(i);
		if (not n->charset() and n->comp()) {
			return {};
		}
		switch(n->symbol()) {
		case EMPTY_STRING:
			return {};
		case CONCATENATION:
			return node_to_string(n.left()) + node_to_string(n.right());
		case DISJUNCTION:
			if (is_root(n) or node(n.parent())->symbol() == DISJUNCTION) {
				return node_to_string(n.left()) + U'|' + node_to_string(n.right());
			} else {
				return U'(' + node_to_string(n.left()) + U'|' + node_to_string(n.right()) + U')';
			}
		case CLOSURE:
			if (node(n.left())->symbol() != CONCATENATION) {
				return node_to_string(n.left()) + U'*';
			} else {
				return (U'(' + node_to_string(n.left()) + U')') + U'*';
			}
		case WILDCARD:
			return {'.'};
		default:
			if (n->anchor()) {
				switch (n->symbol()) {
				case BEGINNING_OF_LINE:
					return {'^'};
				case END_OF_LINE:
					return {'$'};
				case WORD_BOUNDARY:
					return {'\\', 'b'};
				default:
					return {'(', '?', '?', ')'};
				}
			} else if (n->charset()) {
				std::basic_string<char32_t> s;
				s.reserve(n->charset_ptr()->size() + 2 + n->comp());
				s += '[';
				if (n->comp()) {
					s += '^';
				}
				for (auto&& c : *n->charset_ptr()) {
					s += c;
				}
				s += ']';
				return s;
			} else if (escape_chars.contains(n->symbol())) {
				return {'\\', (char32_t)n->symbol()};
			} else {
				return {(char32_t)n->symbol()};
			}
		}
	}
	std::basic_string<char32_t> to_string() const {
		return node_to_string(root());
	}
	template <typename CharT>
	std::basic_string<char32_t> tree_to_string(CharT delim) const {
		return base::to_string([&](size_t i) -> std::basic_string<char32_t> {
			auto&& n = node(i);
			switch (n->symbol()) {
			case DISJUNCTION:
				return U"DISJUNCTION";
			case CONCATENATION:
				return U"CONCATENATION";
			case CLOSURE:
				return U"CLOSURE";
			default:
				return node_to_string(i);
			}
		}, delim);
	}
	std::basic_string<char32_t> tree_to_string() const {
		return tree_to_string('|');
	}
	bool node_has_empty_string(size_t n) const noexcept {
		return node_has_empty_string(node(n));
	}
	bool node_has_empty_string(const node_type& n) const noexcept {
		switch (n->symbol()) {
		case EMPTY_STRING:
			return true xor n->comp();
		case CONCATENATION:
			return (node_has_empty_string(node(n.left())) and node_has_empty_string(node(n.right()))) xor n->comp();
		case CLOSURE:
			return true xor n->comp();
		case DISJUNCTION:
			return (node_has_empty_string(node(n.left())) or node_has_empty_string(node(n.right()))) xor n->comp();
		default:
			return n->anchor() xor n->comp();
		}
	}
	regexp node_derivative(size_t n, size_t c) const {
		return node_derivative(node(n), c);
	}
	regexp node_derivative(const node_type& n, size_t c) const {
		if (is_reserved(c)) {
			return null_regexp();
		}
		switch (n->symbol()) {
		case EMPTY_STRING:
			return null_regexp();
		case DISJUNCTION:
			return node_derivative(node(n.left()), c).unify(node_derivative(node(n.right()), c));
		case CLOSURE:
			return node_derivative(node(n.left()), c).concat(promote_to_regexp(n));
		case CONCATENATION:
			if (node_has_empty_string(node(n.left()))) {
				return node_derivative(node(n.left()), c).concat(promote_to_regexp(node(n.right()))).unify(node_derivative(node(n.right()), c));
			} else {
				return node_derivative(node(n.left()), c).concat(promote_to_regexp(node(n.right())));
			}
		default:
			if (n->anchor()) {
				if (auto&& parent = node(n.parent()); parent->symbol() == CONCATENATION) {
					if (node(n) == parent.left()) {
						return node_derivative(parent.right(), c);
					} else {
						return node_derivative(parent.left(), c);
					}
				} else {
					return null_regexp();
				}
			} else if (n->charset()) {
				if (n->charset_ptr()->size() != 1 or *n->charset_ptr()->begin() != c) {
					return null_regexp();
				} else {
					return empty_regexp();
				}
			} else if (n->symbol() == c) {
				return empty_regexp();
			} else {
				return null_regexp();
			}
		}
	}
	regexp derivative(size_t c) const {
		if (is_null()) {
			return null_regexp();
		} else {
			return node_derivative(root(), c);
		}
	}
	template <std::forward_iterator It>
	regexp node_multiderivative(const node_type& n, It begin, It end) const {
		return promote_to_regexp(n).multiderivative(begin, end);
	}
	template <std::forward_iterator It>
	regexp multiderivative(It begin, It end) const {
		regexp result = *this;
		std::for_each(begin, end, [&result] (size_t c) { result = result.derivative(c); });
		return result;
	}
	static std::basic_string<char32_t> common_prefix(regexp lhs, const regexp& rhs) {
		return lhs.unify(rhs).prefix();
	}
	void node_remove_empty_string(size_t n) {
		return node_remove_empty_string(node(n));
	}
	void node_remove_empty_string(node_type& n) {
		size_t i = node(n);
		if (n->charset() or n->anchor()) {
			return;
		}
		switch (n->symbol()) {
		case DISJUNCTION:
			if (node_has_empty_string(node(n.left()))) {
				node_remove_empty_string(node(n.left()));
			}
			if (node_has_empty_string(node(node(i).right()))) {
				node_remove_empty_string(node(node(i).right()));
			}
			return;
		case CONCATENATION:
			if (node_has_empty_string(node(n.left())) && node_has_empty_string(node(n.right()))) {
				return node_remove_empty_string(node(n.left()));
			}
		case CLOSURE:
			if (node_has_empty_string(node(n.left()))) {
				node_remove_empty_string(node(n.left()));
			}
			return node_merge_with(i, promote_to_regexp(node(n.left())), CONCATENATION);
		case EMPTY_STRING:
			return remove_node(i);
		}
	}
	regexp& remove_empty_string() {
		node_remove_empty_string(root());
		return *this;
	}
	std::basic_string<char32_t> node_prefix(size_t n) const {
		return node_prefix(node(n));
	}
	std::basic_string<char32_t> node_prefix(const node_type& n) const {
		if (n->comp()) {
			return {};
		}
		switch (n->symbol()) {
		case EMPTY_STRING:
			return {};
		case CLOSURE:
			return {};
		case DISJUNCTION: {
			auto p0 = node_prefix(node(n.left()));
			auto p1 = node_prefix(node(n.right()));
			p0.erase(std::mismatch(p0.begin(), p0.end(), p1.begin(), p1.end()).first, p0.end());
			return p0;
		}
		case CONCATENATION: {
			if (node_has_empty_string(node(n.left()))) {
				return common_prefix(
						promote_to_regexp(node(n.left()))
						.remove_empty_string()
						.concat(promote_to_regexp(node(n.right()))),
						promote_to_regexp(node(n.right()))
				);
			} else {
				auto p = node_prefix(node(n.left()));
				return p + node_multiderivative(node(n.left()), p.begin(), p.end()).concat(promote_to_regexp(node(n.right()))).prefix();
			}
		}
		default:
			if (n->charset()) {
				if (n->charset_ptr()->size() != 1) {
					return {};
				} else {
					return { *n->charset_ptr()->begin() };
				}
			} else {
				return {(char32_t)n->symbol()};
			}
		}
	}
	std::basic_string<char32_t> prefix() const {
		if (is_null()) {
			return {};
		} else {
			return node_prefix(root());
		}
	}
	template <std::forward_iterator It>
	bool node_contains(size_t n, It begin, It end) const {
		return node_contains(node(n), begin, end);
	}
	template <std::forward_iterator It>
	bool node_contains(const node_type& n, It begin, It end) const {
		switch (n->symbol()) {
		case DISJUNCTION:
			return node_contains(n.left(), begin, end) || node_contains(n.right(), begin, end);
		case CONCATENATION: {
			auto it = begin;
			for (; it != end; ++it) {
				if (node_contains(n.left(), begin, it) && node_contains(n.right(), it, end)) {
					return true ^ n->comp();
				}
			}
			return node_contains(n.left(), begin, end) && node_contains(n.right(), end, end);
		}
		case CLOSURE: {
			std::function<bool(It, It)> matches_closure = [&] (It begin, It end) -> bool {
				if (begin == end) {
					return true;
				}
				auto it = begin;
				for (++it; it != end; ++it) {
					if (node_contains(n.left(), begin, it) && matches_closure(it, end)) {
						return true;
					}
				}
				return node_contains(n.left(), begin, end);
			};
			return matches_closure(begin, end) ^ n->comp();
		}
		case EMPTY_STRING:
			return (begin == end) ^ n->comp();
		case WILDCARD:
			return (begin != end && ++begin == end) ^ n->comp();
		default:
			return (begin != end
				&& (n->charset() ? n->charset_ptr()->contains(*begin) : *begin == n->symbol())
				&& ++begin == end) ^ n->comp();
		}
	}
	template <std::forward_iterator It>
	bool contains(It begin, It end) const {
		if (is_null()) {
			return false;
		} else {
			return node_contains(root(), begin, end);
		}
	}
	std::set<size_t> anchors() const {
		std::set<size_t> result;
		base::traverse_out_of_order([&](size_t i) {
			auto&& n = node(i);
			if (n->anchor()) {
				result.insert(n->symbol());
			}
		});
		return result;
	}
	AUTOMATA_TUPLE_STRUCT2(
		dfa_completed_capture,
		size_t, start,
		size_t, end
	);
	AUTOMATA_TUPLE_STRUCT2(
		dfa_incomplete_capture,
		size_t, start,
		size_t, nfa_state
	);
	typedef std::set<dfa_completed_capture> dfa_completed_captures;
	typedef std::set<dfa_incomplete_capture> dfa_incomplete_captures;
	AUTOMATA_TUPLE_STRUCT2(
		dfa_capture_group,
		dfa_completed_captures, completed,
		dfa_incomplete_captures, incomplete
	);
	typedef std::vector<dfa_capture_group> dfa_state;

	AUTOMATA_TUPLE_STRUCT2(
		nfa_state_pair,
		size_t, from_state,
		size_t, to_state
	);

	AUTOMATA_TUPLE_STRUCT3(
		dfa_delta,
		std::vector<std::set<size_t>>, group_starts,
		std::vector<std::set<nfa_state_pair>>, group_continues,
		std::vector<std::set<size_t>>, group_ends
	);
	
	AUTOMATA_TUPLE_STRUCT2(
		nfa_completed_capture,
		size_t, start,
		size_t, end
	);
	AUTOMATA_TUPLE_STRUCT1(
		nfa_incomplete_capture,
		size_t, start
	);
	typedef std::set<nfa_completed_capture> nfa_completed_captures;
	typedef std::set<nfa_incomplete_capture> nfa_incomplete_captures;
	AUTOMATA_TUPLE_STRUCT2(
		nfa_capture_group,
		nfa_completed_captures, completed,
		nfa_incomplete_captures, incomplete
	);
	typedef std::vector<nfa_capture_group> nfa_state;

	AUTOMATA_TUPLE_STRUCT3(
		nfa_delta,
		std::set<size_t>, starts,
		std::set<size_t>, ends,
		bitset<>, owners
	);

	typedef finite_automaton<nfa_state, nfa_delta> nfa_type;
	typedef finite_automaton<dfa_state, dfa_delta> dfa_type;

	AUTOMATA_TUPLE_STRUCT5(
		compilation_result,
		nfa_type, automaton,
		size_t, capture_gid,
		size_t, accepting_state,
		bool, clos_left,
		bool, clos_right
	);
	compilation_result compile(size_t re, size_t capture_group_id, finite_automaton<> input_alphabet) const {
		return compile(node(re), capture_group_id, std::move(input_alphabet));
	}
	// second part is the group id, third is the index of the accepting state
	compilation_result compile(
		const node_type& re,
		// this parameter should be the last owned capture group
		// 0 capture is owned by the whole string
		// the returned group id should also be the last owned capture group
		size_t capture_group_id,
		finite_automaton<> input_alphabet
	) const {
		using ret = compilation_result;
		compilation_result result;
		result.automaton() = input_alphabet;
		auto get_default_transition_construct = [] (nfa_type& A) {
			return [&A] (size_t p, size_t a, size_t q) -> nfa_delta {
				return std::move(A.get_delta(p, a, q));
			};
		};
		auto get_transition_merger = [] (nfa_type& A) {
			return [&A] (nfa_delta& t, size_t, size_t, size_t, size_t p, size_t a, size_t q) {
				auto&& delta = A.get_delta(p, a, q);
				t.starts().merge(delta.starts());
				t.ends().merge(delta.ends());
				t.owners() |= delta.owners();
			};
		};
		if (re->capture()) {
			++capture_group_id;
		}
		auto default_state_construct = [] (size_t) -> nfa_state { return {}; };
		switch (re->symbol()) {
		case CONCATENATION: {
			auto [R, cgi, ra, lclosl, lclosr] = compile(re.left(), capture_group_id, input_alphabet).decompose();
			if (R.empty()) {
				return ret();
			}
			auto [S, cgi2, sa, rclosl, rclosr] = compile(re.right(), cgi, input_alphabet).decompose();
			if (S.empty()) {
				return ret();
			}
			capture_group_id = cgi2;
			auto state_map = R.join(S, default_state_construct, get_default_transition_construct(S));
			R.remove_state(state_map.at(0));
			R.remove_accepting(ra);
			if (lclosr and rclosl) {
				R.add_delta(ra, R.epsilon_symbol(), state_map.at(S.first_start_state()));
			} else {
				if (ra == R.first_start_state()) {
					R.add_start_state(state_map.at(S.first_start_state()));
				}
				R.contract_state(ra, state_map.at(S.first_start_state()), get_transition_merger(R));
			}
			R.add_accepting(state_map.at(sa));
			result.automaton() = std::move(R);
			result.accepting_state() = state_map.at(sa);
			result.clos_left() = lclosl;
			result.clos_right() = rclosr;
			break;
		}
		case DISJUNCTION: {
			auto [R, cgi, ra, lclosl, lclosr] = compile(re.left(), capture_group_id, input_alphabet).decompose();
			if (R.empty()) {
				return ret();
			}
			auto [S, cgi2, sa, rclosl, rclosr] = compile(re.right(), cgi, input_alphabet).decompose();
			if (S.empty()) {
				return ret();
			}
			capture_group_id = cgi2;
			nfa_type* A[2] = { &R, &S };
			bool closl[2] = { lclosl, rclosl };
			bool closr[2] = { lclosr, rclosr };
			size_t accept[2] = { ra, sa };
			for (size_t i = 0; i < 2; ++i) {
				if (closl[i]) {
					size_t old_start = A[i]->first_start_state();
					A[i]->remove_start_state(old_start);
					size_t new_start = A[i]->push_state();
					A[i]->add_start_state(new_start);
					A[i]->add_delta(new_start, A[i]->epsilon_symbol(), old_start);
				}
				if (closr[i] or A[i]->first_start_state() == accept[i]) {
					A[i]->remove_accepting(accept[i]);
					size_t new_end = A[i]->push_state();
					A[i]->add_accepting(new_end);
					A[i]->add_delta(accept[i], A[i]->epsilon_symbol(), new_end);
				}
			}
			auto state_map = R.join(S, default_state_construct, get_default_transition_construct(S));
			R.remove_state(state_map.at(0));
			size_t start = R.push_state();
			size_t end = R.push_state();

			R.contract_state(R.first_start_state(), start, get_transition_merger(R));
			R.contract_state(ra, end, get_transition_merger(R));
			R.contract_state(state_map.at(S.first_start_state()), start, get_transition_merger(R));
			R.contract_state(state_map.at(sa), end, get_transition_merger(R));

			R.add_start_state(start);

			R.add_accepting(end);

			result.automaton() = std::move(R);
			result.accepting_state() = end;
			break;
		}
		case CLOSURE: {
			auto [R, cgi, ra, _, _a] = compile(re.left(), capture_group_id, input_alphabet).decompose();
			if (R.empty()) {
				return ret();
			}
			capture_group_id = cgi;
			R.contract_state_looped(R.first_start_state(), ra, get_transition_merger(R));
			if (re->capture()) {
				size_t new_start = R.push_state();
				size_t new_end = R.push_state();
				R.add_delta(new_start, R.epsilon_symbol(), ra);
				R.add_delta(ra, R.epsilon_symbol(), new_end);
				R.remove_accepting(ra);
				R.add_start_state(new_start);
				R.add_accepting(new_end);
				result.accepting_state() = new_end;
			} else {
				R.add_start_state(ra);
				result.accepting_state() = ra;
				result.clos_left() = true;
				result.clos_right() = true;
			}
			result.automaton() = std::move(R);
			break;
		}
		case WILDCARD: {
			auto&& R = result.automaton();
			size_t p = R.push_state();
			size_t q = R.push_state();
			R.add_delta(p, R.unknown_symbol(), q);
			R.for_each_input_symbol([&](size_t, size_t a) {
				R.add_delta(p, a, q);
			});
			R.add_start_state(p);
			R.add_accepting(q);
			result.accepting_state() = q;
			break;
		}
		case EMPTY_STRING: {
			auto&& R = result.automaton();
			size_t p = R.push_state();
			R.add_start_state(p);
			R.add_accepting(p);
			result.accepting_state() = p;
			break;
		}
		default: {
			auto&& R = result.automaton();
			size_t p = R.push_state();
			size_t q = R.push_state();
			if (re->charset()) {
				for (size_t c : *re->charset_ptr()) {
					R.add_transition(p, c, q);
				}
				if (re->comp()) {
					R.invert_transitions(p, q);
				}
			} else {
				R.add_transition(p, re->symbol(), q);
			}
			R.add_start_state(p);
			R.add_accepting(q);
			result.accepting_state() = q;
		}
		}
		auto&& R = result.automaton();
		if (re->capture()) {
			R.for_each_transition([&](size_t p, size_t a, size_t q) {
				auto&& delta = R.get_delta(p, a, q);
				delta.owners().resize(std::max(delta.owners().size(), capture_group_id));
				delta.owners().set(capture_group_id - 1);
				return true;
			});
			R.for_each_out_delta(R.first_start_state(), [&](size_t p, size_t a, size_t q) {
				auto&& delta = R.get_delta(p, a, q);
				delta.starts().insert(capture_group_id - 1);
				return true;
			});
			R.for_each_in_delta(result.accepting_state(), [&](size_t p, size_t a, size_t q) {
				auto&& delta = R.get_delta(p, a, q);
				delta.ends().insert(capture_group_id - 1);
				return true;
			});
		}
		result.capture_gid() = capture_group_id;
		return result;
	}
	nfa_type compile() const {
		if (is_null()) {
			return nfa_type();
		}
		finite_automaton<> input_alphabet;
		base::traverse_out_of_order([&](size_t i) {
			auto&& n = node(i);
			if (n->charset()) {
				for (size_t s : *n->charset_ptr()) {
					input_alphabet.push_symbol(s);
				}
			} else if (not is_reserved(n->symbol())) {
				input_alphabet.push_symbol(n->symbol());
			}
		});
		auto result = std::get<0>(compile(root(), 0, input_alphabet));
		for (size_t c : anchors()) {
			result.for_each_state([&](size_t q) -> bool {
				result.add_transition(q, c, q);
				auto&& delta = result.get_transition(q, c, q);
				delta.owners().complement();
				return true;
			});
		}
		return result;
	}
	//dfa_type compile_deterministic(size_t state_limit) const {
	//	auto r = compile(state_limit);
	//	r = r.deterministic(
	//		[](automaton_state_type&, size_t, size_t){},
	//		[&](automaton_delta_type& s, size_t, size_t, size_t, size_t p, size_t a, size_t q) {
	//			auto&& [marks, bitset] = r.get_delta(p, a, q);
	//			s.first.insert(marks.begin(), marks.end());
	//			size_t sz = std::max(s.second.size(), bitset.size());
	//			s.second.resize(sz);
	//			bitset.resize(sz);
	//			s.second |= bitset;
	//		},
	//		state_limit
	//	);
	//	return r;
	//}
	//auto compile_deterministic() const {
	//	return compile_deterministic(1 << 20);
	//}

	static constexpr auto NFA_COPY_ALL_INCOMPLETE_CAPTURES = [] (size_t, size_t begin, nfa_incomplete_captures& captures) -> void {
		captures.emplace(begin);
	};
	static constexpr auto NFA_COPY_ALL_COMPLETED_CAPTURES = [] (size_t, size_t begin, size_t end, nfa_completed_captures& captures) -> void {
		captures.emplace(begin, end);
	};

	static constexpr auto NFA_COPY_LEFTMOST_LONGEST_INCOMPLETE_CAPTURES = [] (size_t, size_t begin, nfa_incomplete_captures& captures) -> void {
		if (captures.empty()) {
			captures.emplace(begin);
		} else if (captures.begin()->start() > begin) {
			captures.clear();
			captures.emplace(begin);
		}
	};
	static constexpr auto NFA_COPY_LEFTMOST_LONGEST_COMPLETED_CAPTURES = [] (size_t, size_t begin, size_t end, nfa_completed_captures& captures) -> void {
		if (captures.empty()) {
			captures.emplace(begin, end);
		} else if (captures.begin()->start() > begin
			   or (captures.begin()->start() == begin and captures.begin()->end() < end)) {
			captures.clear();
			captures.emplace(begin, end);
		}
	};
	template <std::input_iterator It>
	static std::vector<nfa_completed_captures> match_all(nfa_type& automaton, It begin, It end) {
		return match(automaton, begin, end, NFA_COPY_ALL_INCOMPLETE_CAPTURES, NFA_COPY_ALL_COMPLETED_CAPTURES);
	}
	template <typename CharT>
	static std::vector<nfa_completed_captures> match_all(nfa_type& automaton, const CharT* s) {
		return match(automaton, s, NFA_COPY_ALL_INCOMPLETE_CAPTURES, NFA_COPY_ALL_COMPLETED_CAPTURES);
	}
	template <std::input_iterator It>
	static std::vector<nfa_completed_captures> match_leftmost_longest(nfa_type& automaton, It begin, It end) {
		return match(automaton, begin, end, NFA_COPY_LEFTMOST_LONGEST_INCOMPLETE_CAPTURES, NFA_COPY_LEFTMOST_LONGEST_COMPLETED_CAPTURES);
	}
	template <typename CharT>
	static std::vector<nfa_completed_captures> match_leftmost_longest(nfa_type& automaton, const CharT* s) {
		return match(automaton, s, NFA_COPY_LEFTMOST_LONGEST_INCOMPLETE_CAPTURES, NFA_COPY_LEFTMOST_LONGEST_COMPLETED_CAPTURES);
	}
	template <
		typename CharT,
		std::invocable<size_t, size_t, nfa_incomplete_captures&> IncompleteCopy,
		std::invocable<size_t, size_t, size_t, nfa_completed_captures&> CompletedCopy
	>
	static std::vector<nfa_completed_captures> match(nfa_type& automaton, const CharT* s, IncompleteCopy incomplete_copy, CompletedCopy completed_copy) {
		return match(automaton, s, s + std::char_traits<CharT>::length(s), std::move(incomplete_copy), std::move(completed_copy));
	}
	template <
		std::input_iterator It,
		std::invocable<size_t, size_t, nfa_incomplete_captures&> IncompleteCopy,
		std::invocable<size_t, size_t, size_t, nfa_completed_captures&> CompletedCopy
	>
	static std::vector<nfa_completed_captures> match(
		nfa_type& automaton, It begin, It end, IncompleteCopy incomplete_copy, CompletedCopy completed_copy
	) {
		std::unordered_map<size_t, nfa_state> saved_states;
		saved_states.reserve(automaton.state_count());
		size_t max_group = 0;
		automaton.for_each_transition([&] (size_t p, size_t a, size_t q) {
			auto&& delta = automaton.get_delta(p, a, q);
			if (not delta.starts().empty()) {
				max_group = std::max(max_group, *delta.starts().rbegin());
			}
			return true;
		});
		max_group += 1;
		automaton.for_each_state([&] (size_t q) {
			automaton.get_state(q).resize(max_group);
			saved_states[q].resize(max_group);
			return true;
		});

		std::vector<nfa_completed_captures> result;
		result.resize(max_group + 1);
		// todo: make an optimized version of this for tokenizing

		auto put_start_captures = [&](size_t p, size_t a, size_t q, size_t pos) {
			auto&& delta = automaton.get_delta(p, a, q);
			auto&& pset = automaton.get_state(p);
			for (size_t gid : delta.starts()) {
				incomplete_copy(gid, pos, pset[gid].incomplete());
			}
		};
		auto put_end_captures = [&](size_t p, size_t a, size_t q, size_t pos) {
			auto&& delta = automaton.get_delta(p, a, q);
			auto&& qset = a == automaton.epsilon_symbol() ? automaton.get_state(q) : saved_states[q];
			for (size_t gid : delta.ends()) {
				auto&& incomplete = qset[gid].incomplete();
				std::vector t(incomplete.begin(), incomplete.end());
				for (auto&& capture : t) {
					incomplete.erase(capture);
					completed_copy(gid, capture.start(), pos + (a != automaton.epsilon_symbol()), qset[gid].completed());
				}
			}
		};
		auto copy_captures = [&](size_t p, size_t a, size_t q) {
			auto&& pset = automaton.get_state(p);
			auto&& qset = a == automaton.epsilon_symbol() ? automaton.get_state(q) : saved_states[q];
			auto&& delta = automaton.get_delta(p, a, q);
			for (size_t gid = 0; gid < pset.size(); ++gid) {
				auto&& capture_group = pset[gid];
				for (auto&& completed : capture_group.completed()) {
					completed_copy(gid, completed.start(), completed.end(), qset[gid].completed());
				}
			}
			delta.owners().for_each_in_set([&](size_t gid) {
				for (auto&& incomplete : pset[gid].incomplete()) {
					incomplete_copy(gid, incomplete.start(), qset[gid].incomplete());
				}
			});
		};
		auto t = [&](size_t p, size_t a, size_t q, size_t pos) -> bool {
			if (q == 0) {
				return true;
			}
			if (a != automaton.epsilon_symbol()) {
				return true;
			}
			put_start_captures(p, a, q, pos);
			copy_captures(p, a, q);
			put_end_captures(p, a, q, pos);
			return true;
		};
		auto collect_results = [&](const auto& s, size_t pos) {
			if (std::any_of(s.begin(), s.end(), std::bind(&nfa_type::accepts, &automaton, std::placeholders::_1))) {
				for (auto&& set : result) {
					set.clear();
				}
			}
			for (size_t q : s | std::views::filter(std::bind(&nfa_type::accepts, &automaton, std::placeholders::_1))) {
				result[0].emplace(0, pos);
				for (size_t gid = 1; gid < result.size(); ++gid) {
					for (auto&& capture : automaton.get_state(q)[gid - 1].completed()) {
						completed_copy(gid - 1, capture.start(), capture.end(), result[gid]);
					}
				}
			}
		};
		auto c = [&](const auto& clos, size_t pos) {
			collect_results(clos, pos);
			return true;
		};
		auto s = [&](size_t p, size_t a, const auto& qset, size_t pos) {
			auto&& pset = automaton.get_state(p);
			for (size_t q : qset) {
				put_start_captures(p, a, q, pos);
				copy_captures(p, a, q);
				put_end_captures(p, a, q, pos);
			}
			for (auto&& capture_group : pset) {
				auto&& [completed, incomplete] = capture_group.decompose();
				completed.clear();
				incomplete.clear();
			}
			return true;
		};
		auto n = [&] (const auto& s, size_t) -> bool {
			for (size_t q : s) {
				auto&& state = saved_states[q];
				for (size_t gid = 0; gid < state.size(); ++gid) {
					auto&& astate = automaton.get_state(q);
					astate[gid].incomplete().merge(state[gid].incomplete());
					astate[gid].completed().merge(state[gid].completed());
				}
			}
			//collect_results(s, pos);
			for (auto&& [_, state] : saved_states) {
				for (auto&& capture_group : state) {
					auto&& [completed, incomplete] = capture_group.decompose();
					completed.clear();
					incomplete.clear();
				}
			}
			return true;
		};
		automaton.simulate(begin, end, t, c, n, s);
		automaton.for_each_state([&](size_t q) {
			for (auto&& set : automaton.get_state(q)) {
				set.incomplete().clear();
				set.completed().clear();
			}
			return true;
		});
		return result;
	}
};

} // namespace automata

#endif // #ifndef AUTOMATA_REGEX_HPP

#ifdef AUTOMATA_CFG_STRING_CONSTRUCTION
#include "llparser.hpp"

namespace automata {

	template <std::input_iterator It>
	regexp regexp::parse_regexp(It begin, It end) {
		static const std::unordered_map<size_t, size_t> anchors = {
			{'^', BEGINNING_OF_LINE},
			{'$', END_OF_LINE},
			{'.', WILDCARD}
		};
		static const std::unordered_set<size_t> metachars = {
			'*', '?', '(', ')', '[', ']', '+', '{', '}',
		};
		static const std::unordered_map<size_t, size_t> escape_chars = {
			{'*', '*'},
			{'?', '?'},
			{'(', '('},
			{')', ')'},
			{'[', '['},
			{']', ']'},
			{'+', '+'},
			{'{', '{'},
			{'}', '}'},
			{'^', '^'},
			{'$', '$'},
			{'.', '.'},
			{'\\', '\\'},
			{'b', WORD_BOUNDARY},
			{'t', '\t'},
			{'n', '\n'},
			{'v', '\v'},
			{'r', '\r'},
			{'f', '\f'}
		};
		static auto get_group_min_max_end = [] (const size_t* it) -> std::pair<size_t, size_t> {
			switch (*it) {
			case '*':
				return { 0, (size_t)-1 };
			case '+':
				return { 1, (size_t)-1 };
			case '?':
				return { 0, 1 };
			case '{': {
				auto [min_num, n] = ascii_to_size_t(++it, (const size_t*)(size_t)-1);
				if (n == end) {
					return { (size_t)-1, 0 };
				} else if (*n == '}') {
					return { min_num, min_num };
				} else if (*n == ',') {
					auto [max_num, m] = ascii_to_size_t(++n, (const size_t*)(size_t)-1);
					if (m == end || *m != '}') {
						return { (size_t)-1, 0 };
					} else {
						return { min_num, max_num };
					}
				} else {
					return { (size_t)-1, 0 };
				}
			}
			default:
				return { 1, 1 };
			}
		};
		static constexpr char REGEXP_GRAMMAR_SPEC[] = R"--(
			DISJUNCTION -> CONCATENATION DISJUNCTIONTAIL
			DISJUNCTIONTAIL -> '|' CONCATENATION DISJUNCTIONTAIL |
			CONCATENATION -> MINMAXEXPR CONCATENATIONTAIL
			CONCATENATIONTAIL -> MINMAXEXPR CONCATENATIONTAIL |
			MINMAXEXPR -> EXPR MINMAXSPEC
			MINMAXSPEC -> '*' | '+' | '?' | COMPOUNDMINMAXSPEC |
			COMPOUNDMINMAXSPEC -> '{' NUMBER '}' | '{' NUMBER ',' NUMBER '}'
			NUMBER -> '[0-9]' NUMBERTAIL
			NUMBERTAIL -> '[0-9]' NUMBERTAIL |
			EXPR -> '(' DISJUNCTION ')' | ESCAPE | CHAR | CHARSET |
			CHARSET -> '[' CHARSETCONTENT ']'
			CHARSETCONTENT -> '[^\]]' CHARSETCONTENT |
			ESCAPABLECHAR -> '[*+?|{}()\[\]\\^$abfnrtv]'
			ESCAPE -> '\' ESCAPABLECHAR
			CHAR -> '[^*+?|{}()\[\]\\^$.]'
			ANCHOR -> '[$^.]'
		)--"; // todo: ^^^ fix CHARSETCONTENT as it doesn't correctly parse escaped square brackets
		static const CFG regexp_grammar = CFG(REGEXP_GRAMMAR_SPEC);
		static const LL_parser parser = LL_parser(regexp_grammar);
		dynamic_tree<CFG::node_type> parse_tree = parser.parse_tree(begin, end);
		std::function<regexp(size_t)> build_regexp = [&] (size_t n) -> regexp {
			auto&& node = parse_tree.node(n);
			if (node.children().size() == 0) {
				return empty_regexp();
			}
			auto check_for = [&](std::string s) -> bool {
				return node.symbol() == regexp_grammar.variable(s);
			};
			if (check_for("CHAR")) {
				return symbol_regexp(parse_tree.node(node.children()[0])->symbol());
			} else if (check_for("ANCHOR") {
				return symbol_regexp(anchors.at(parse_tree.node(node.children()[0])->symbol()));
			} else if (check_for("ESCAPE")) {
				return build_regexp(node.children()[1]);
			} else if (check_for("ESCAPABLECHAR") {
				return symbol_regexp(escape_chars.at(parse_tree.node(node.children()[0])->symbol()));
			} else if (check_for("CHARSET")) {
				return build_regexp(node.children()[1]);
			} else if (check_for("CHARSETCONTENT")) {
				std::basic_string<char32_t> content;
				parse_tree.traverse_preorder(n, [&](size_t n) -> void {
					if (parse_tree.node(n)->variable()) {
						return;
					}
					content += parse_tree.node(n)->symbol();
				});
				return charset_regexp(content.begin(), content.end());
			} else if (check_for("EXPR")) {
				if (not node.children()[0].variable()) {
					return build_regexp(node.children()[1]);
				} else {
					return build_regexp(node.children()[0]);
				}
			} else if (check_for("MINMAXEXPR")) {
				regexp expr = build_regexp(node.children()[0]);
				auto&& minmaxexprspec = parse_tree.node(node.children()[1]);
				std::basic_string<size_t> content;
				parse_tree.traverse_preorder(node(minmaxexprspec), [&](size_t n) -> void {
					if (parse_tree.node(n)->variable()) {
						return;
					}
					content += parse_tree.node(n)->symbol();
				});
				auto [min_count, max_count] = get_group_min_max_end(content.data());
				regexp result = repeat(expr, min_count);
				if (max_count == (size_t)-1) {
					result.concat(std::move(expr.to_closure()));
				} else {
					result.concat(nth_closure(std::move(expr), max_count - min_count));
				}
				return result;
			} else if (check_for("CONCATENATION") || check_for("CONCATENATIONTAIL")) {
				return build_regexp(node.children()[0]).concat(build_regexp(node.children()[1]));
			} else if (check_for("DISJUNCTION")) {
				return build_regexp(node.children()[0]).unify(build_regexp(node.children()[1]));
			} else if (check_for("DISJUNCTIONTAIL")) {
				return build_regexp(node.children()[1]).unify(build_regexp(node.children()[2]));
			}
		};
		return build_regexp(parse_tree.root());
	}
}
#endif // #ifdef AUTOMATA_CFG_HPP
