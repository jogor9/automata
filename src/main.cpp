#include <iostream>
#include <iomanip>
#include <algorithm>
#include <thread>
#include <string>
#include <unordered_map>
#include <cassert>
#include <chrono>
#include "lazy_ptr.hpp"
#include "bitset.hpp"
#include "tests.hpp"
#include "fa.hpp"
#include "regexp.hpp"
#include "tokenizer.hpp"

// todo: tokenizers, grammars, LALR parser

using namespace std;
using namespace std::literals::string_literals;
using namespace automata;

void print_captures(auto&& results) {
	for (size_t i : std::views::iota(size_t(0), size_t(results.size()))) {
		cout << "captures for group " << i << ":" << endl;
		for (auto&& c : results.at(i)) {
			auto&& [begin, end] = c.decompose();
			cout << begin << " - " << end << endl;
		}
	}
}

//std::string read_stdin(ssize_t buf_sz = 4096) {
//	if (buf_sz <= 0) {
//		return "";
//	}
//	std::string result;
//	ssize_t bytes_read;
//	do {
//		result.resize(result.size() + buf_sz);
//		bytes_read = read(0, &*(result.end() - buf_sz), buf_sz);
//		if (bytes_read < 0) {
//			return "";
//		}
//	} while (bytes_read == buf_sz);
//	result.resize(result.size() - (buf_sz - bytes_read));
//	return result;
//}
//
//atomic_size_t pos = 0;
//size_t max_pos;
//
//void print_pos_sleep() {
//	while (pos != max_pos) {
//		using namespace std::literals::chrono_literals;
//		cout << "current position: " << pos << endl;
//		std::this_thread::sleep_for(5s);
//	}
//}

int main() {
    while (!std::cin.eof()) {
        std::string str;
        std::cin >> str;
        std::vector<int> repeats;
        char c;
        do {
            int i;
            std::cin >> i;
            repeats.push_back(i);
            std::cin >> c;
        } while (c == ',');
        auto&& damaged_re = regexp::symbol_regexp('#').unify(regexp::symbol_regexp('?'));
        auto ok_re = regexp::symbol_regexp('.').unify(regexp::symbol_regexp('?'));
        regexp re = regexp(ok_re).to_closure();
        for (auto&& repeat : repeats | std::views::take(repeats.size() - 1)) {
            re.concat(
                regexp(damaged_re).repeat(repeat)
            ).concat(
                regexp(ok_re).concat(regexp(ok_re).to_closure())
            );
        }
        re.concat(damaged_re.repeat(repeats.back())).concat(ok_re.to_closure());
        re.capture();
        auto cre = re.compile();
        print_captures(regexp::match_all(cre, str.begin(), str.end()));
    }
	//regexp res[] = {
	//	regexp::symbol_regexp('i').concat(regexp::symbol_regexp('f')),
	//	regexp::symbol_regexp('e').concat(regexp::symbol_regexp('l')).concat(regexp::symbol_regexp('s').concat(regexp::symbol_regexp('e'))),
	//	regexp::charset_regexp(R"--(\w)--").to_closure(),
	//	regexp::charset_regexp(R"--(^\w)--").to_closure(),
	//};
	//for (auto&& re : res) {
	//	cout << encode_as_utf8(re.to_string()) << endl;
	//}
	//std::string input = read_stdin();
	//auto&& tk = tokenizer(tokenizer_regexp(std::begin(res), std::end(res)));
	//std::string result;
	//max_pos = input.size();
	//auto callback = [&] (size_t gid, size_t begin, size_t end) {
	//	switch (gid) {
	//	case 0:
	//		result += 'w';
	//		break;
	//	case 1:
	//		result += 'W';
	//		break;
	//	}
	//	pos += end - begin;
	//};
	//using namespace std::chrono;
	//time_point start = steady_clock::now();
	//assert(tokenize(tk, input.begin(), input.end(), callback));
	//time_point end = steady_clock::now();
	//cout << "final time: " << duration_cast<duration<double>>(end - start) << endl;

	//FILE* f = fopen("out.txt", "wt");
	//fwrite(result.data(), 1, result.size(), f);

	//cout << encode_as_utf8(re.to_string()) << endl;
	//assert(re.structure_test());

	//auto cre = re.compile();
	//cout << encode_as_utf8(cre.to_string()) << endl;
	////cout << '{' << endl;
	////for (auto&& [transition, data] : cre.transitions->second) {
	////	cout << '\t' << encode_as_utf8(to_u32string(transition)) << " -> " << encode_as_utf8(to_u32string(data)) << endl;
	////}
	////cout << '}' << endl;
	//std::string sa = "aabaaaaabaab";
	//cout << sa << endl;
	//cout << std::boolalpha << cre.simulate(sa.begin(), sa.end()) << endl;
	//auto results = regexp::match(cre, sa.c_str(), regexp::NFA_COPY_ALL_INCOMPLETE_CAPTURES, regexp::NFA_COPY_ALL_COMPLETED_CAPTURES);
	//print_captures(results);

	//cre = re.compile_deterministic();
	//cout << encode_as_utf8(cre.to_string()) << endl;
	//cout << '{' << endl;
	//for (auto&& [transition, data] : cre.transitions->second) {
	//	cout << '\t' << encode_as_utf8(to_u32string(transition)) << " -> " << encode_as_utf8(to_u32string(data)) << endl;
	//}
	//cout << '}' << endl;
	//cout << sa << endl;
	//cout << std::boolalpha << cre.simulate(sa.begin(), sa.end()) << endl;
	//results = regexp::match(cre, sa.c_str());

	//for (size_t i : std::views::iota(size_t(0), size_t(results.size()))) {
	//	cout << "captures for group " << i << ":" << endl;
	//	for (auto&& [begin, end] : results.at(i)) {
	//		cout << begin << " - " << end << endl;
	//	}
	//}

	//re = regexp::symbol_regexp('a').to_closure().concat(regexp::symbol_regexp('b'));
	//cout << encode_as_utf8(re.to_string()) << endl;
	//assert(re.structure_test());

	//cre = re.compile();
	//cout << encode_as_utf8(cre.to_string()) << endl;
	//sa = "b";
	//cout << std::boolalpha << cre.simulate(sa.begin(), sa.end()) << endl;
	//results = regexp::match(cre, sa.c_str());
	//print_captures(results);

	//re = regexp::charset_regexp("a-c\\A").to_closure();
	//cout << encode_as_utf8(re.to_string()) << endl;
	//assert(re.structure_test());

	//cre = re.compile();
	//cout << encode_as_utf8(cre.to_string()) << endl;
	//sa = "abcccbaaaccbaAAAAA   ;':':'';;[]{[][]][}-=-+_=-";
	//cout << std::boolalpha << cre.simulate(sa.begin(), sa.end()) << endl;
	//results = regexp::match(cre, sa.c_str());
	//print_captures(results);

	//finite_automaton<std::string, std::string> fa;

	//// Q = { q1, q2, q3 }
	//size_t q1 = fa.push_state("q1");
	//size_t q2 = fa.push_state("q2");
	//size_t q3 = fa.push_state("q3");

	//cout << "q1 = " << q1 << endl;
	//cout << "q2 = " << q2 << endl;
	//cout << "q3 = " << q3 << endl;

	//// S = { q1 }
	//fa.add_start_state(q1);

	//// F = { q2 }
	//fa.add_accepting(q2);

	//// Σ = { '0', '1', '2' }
	//fa.push_symbol('0');
	//fa.push_symbol('1');
	//fa.push_symbol('2');

	////  δ|'0'|'1'|'2'
	//// --+---+---+---
	//// q1| q1| q2| q3
	//// --+---+---+---
	//// q2| q1| q2| q3
	//// --+---+---+---
	//// q3| q2| q1| q3
	//fa.add_transition(q1, '0', q1, "q1 [0] -> q1");
	//fa.add_transition(q1, '1', q2, "q1 [1] -> q2");
	//fa.add_transition(q1, '2', q3, "q1 [2] -> q3");
	//fa.add_transition(q2, '0', q1, "q2 [0] -> q1");
	//fa.add_transition(q2, '1', q2, "q2 [1] -> q2");
	//fa.add_transition(q2, '2', q3, "q2 [2] -> q3");
	//fa.add_transition(q3, '0', q2, "q3 [0] -> q2");
	//fa.add_transition(q3, '1', q1, "q3 [1] -> q1");
	//fa.add_transition(q3, '2', q3, "q3 [2] -> q3");

	//cout << encode_as_utf8(fa.to_string()) << endl;

	//fa.disjoin_state(q3, [&](size_t p, size_t a, size_t q) {
	//	cout << "removing " << p << " [" << (char)fa.convert_reverse(a) << "] -> " << q << endl;
	//});

	//for (auto&& [t, s] : fa.transitions->second) {
	//	auto [p, a, q] = t;
	//	cout << p << ", " << a << ", " << q << ": " << s << endl;
	//}

	//cout << encode_as_utf8(fa.to_string()) << endl;

	//string s = "001000111";
	//string s2 = "0101000010";
	//auto printer = [&](size_t p, size_t a, size_t q, size_t pos) -> bool {
	//	cout << p << " -> " << q << " on " << (char)fa.convert_reverse(a) << " on position: " << pos << endl;
	//	return true;
	//};
	//cout << std::boolalpha << fa.simulate(s.begin(), s.end(), printer) << endl;
	//cout << std::boolalpha << fa.simulate(s2.begin(), s2.end(), printer) << endl;

	//fa.clear();

	//q1 = fa.push_state();
	//q2 = fa.push_state();
	//q3 = fa.push_state();

	//fa.push_symbol('a');
	//fa.push_symbol('b');

	//fa.add_start_state(q1);
	//fa.add_accepting(q3);
	//
	//fa.add_transition(q1, 'a', q2);
	//fa.add_transition(q2, 'a', q3);
	//fa.add_transition(q2, 'b', q2);
	//
	//cout << fa.to_string() << endl;

	//fa.contract_state(q2, q1);

	//cout << fa.to_string() << endl;
}
