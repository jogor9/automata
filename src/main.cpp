#include <iostream>
#include <iomanip>
#include <algorithm>
#include <string>
#include <unordered_map>
#include <cassert>
#include "lazy_ptr.hpp"
#include "fa.hpp"
#include "regexp.hpp"

// todo: tokenizers, grammars, LALR parser

using namespace std;
using namespace automata;

int main() {
	regexp re =
		regexp::symbol_regexp('a')
		.concat(regexp::symbol_regexp('a'))
		.concat(regexp::symbol_regexp('b')).capture()
		.unify(regexp::symbol_regexp('a').capture())
		.to_closure()
		.concat(regexp::symbol_regexp('b'));

	cout << encode_as_utf8(re.to_string()) << endl;
	assert(re.structure_test());

	auto cre = re.compile();
	cout << encode_as_utf8(cre.to_string()) << endl;
	std::string sa = "aabaaaaabaab";
	cout << sa << endl;
	cout << std::boolalpha << cre.simulate(sa.begin(), sa.end()) << endl;
	auto results = regexp::match(cre, sa.c_str());

	for (size_t i : std::views::iota(size_t(0), size_t(results.size()))) {
		cout << "captures for group " << i << ":" << endl;
		for (auto&& [begin, end] : results.at(i)) {
			cout << begin << " - " << end << endl;
		}
	}

	cre = re.compile_deterministic();
	cout << encode_as_utf8(cre.to_string()) << endl;
	cout << sa << endl;
	cout << std::boolalpha << cre.simulate(sa.begin(), sa.end()) << endl;
	results = regexp::match(cre, sa.c_str());

	for (size_t i : std::views::iota(size_t(0), size_t(results.size()))) {
		cout << "captures for group " << i << ":" << endl;
		for (auto&& [begin, end] : results.at(i)) {
			cout << begin << " - " << end << endl;
		}
	}

	re = regexp::symbol_regexp('a').to_closure().concat(regexp::symbol_regexp('b'));
	cout << encode_as_utf8(re.to_string()) << endl;
	assert(re.structure_test());

	cre = re.compile();
	cout << encode_as_utf8(cre.to_string()) << endl;
	sa = "b";
	cout << std::boolalpha << cre.simulate(sa.begin(), sa.end()) << endl;
	results = regexp::match(cre, sa.c_str());
	for (size_t i : std::views::iota(size_t(0), size_t(results.size()))) {
		cout << "captures for group " << i << ":" << endl;
		for (auto&& [begin, end] : results.at(i)) {
			cout << begin << " - " << end << endl;
		}
	}

	re = regexp::charset_regexp("a-c\\A").to_closure();
	cout << encode_as_utf8(re.to_string()) << endl;
	assert(re.structure_test());

	cre = re.compile();
	cout << encode_as_utf8(cre.to_string()) << endl;
	sa = "abcccbaaaccbaAAAAA   ;':':'';;[]{[][]][}-=-+_=-";
	cout << std::boolalpha << cre.simulate(sa.begin(), sa.end()) << endl;
	results = regexp::match(cre, sa.c_str());
	for (size_t i : std::views::iota(size_t(0), size_t(results.size()))) {
		cout << "captures for group " << i << ":" << endl;
		for (auto&& [begin, end] : results.at(i)) {
			cout << begin << " - " << end << endl;
		}
	}

	finite_automaton<std::string, std::string> fa;

	// Q = { q1, q2, q3 }
	size_t q1 = fa.push_state("q1");
	size_t q2 = fa.push_state("q2");
	size_t q3 = fa.push_state("q3");

	cout << "q1 = " << q1 << endl;
	cout << "q2 = " << q2 << endl;
	cout << "q3 = " << q3 << endl;

	// S = { q1 }
	fa.add_start_state(q1);

	// F = { q2 }
	fa.add_accepting(q2);

	// Σ = { '0', '1', '2' }
	fa.push_symbol('0');
	fa.push_symbol('1');
	fa.push_symbol('2');

	//  δ|'0'|'1'|'2'
	// --+---+---+---
	// q1| q1| q2| q3
	// --+---+---+---
	// q2| q1| q2| q3
	// --+---+---+---
	// q3| q2| q1| q3
	fa.add_transition(q1, '0', q1, "q1 [0] -> q1");
	fa.add_transition(q1, '1', q2, "q1 [1] -> q2");
	fa.add_transition(q1, '2', q3, "q1 [2] -> q3");
	fa.add_transition(q2, '0', q1, "q2 [0] -> q1");
	fa.add_transition(q2, '1', q2, "q2 [1] -> q2");
	fa.add_transition(q2, '2', q3, "q2 [2] -> q3");
	fa.add_transition(q3, '0', q2, "q3 [0] -> q2");
	fa.add_transition(q3, '1', q1, "q3 [1] -> q1");
	fa.add_transition(q3, '2', q3, "q3 [2] -> q3");

	cout << encode_as_utf8(fa.to_string()) << endl;

	fa.disjoin_state(q3, [&](size_t p, size_t a, size_t q) {
		cout << "removing " << p << " [" << (char)fa.convert_reverse(a) << "] -> " << q << endl;
	});

	for (auto&& [t, s] : fa.transitions->second) {
		auto [p, a, q] = t;
		cout << p << ", " << a << ", " << q << ": " << s << endl;
	}

	cout << encode_as_utf8(fa.to_string()) << endl;

	string s = "001000111";
	string s2 = "0101000010";
	auto printer = [&](size_t p, size_t a, size_t q, size_t pos) -> bool {
		cout << p << " -> " << q << " on " << (char)fa.convert_reverse(a) << " on position: " << pos << endl;
		return true;
	};
	cout << std::boolalpha << fa.simulate(s.begin(), s.end(), printer) << endl;
	cout << std::boolalpha << fa.simulate(s2.begin(), s2.end(), printer) << endl;
}
