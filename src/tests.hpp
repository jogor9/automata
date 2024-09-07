#ifndef AUTOMATA_TESTS_HPP
#define AUTOMATA_TESTS_HPP

#include <cassert>
#include "bitset.hpp"

namespace automata {

void regexp_test() {

}

void bitset_test() {
	bitset<uint8_t> a("010101010101");
	bitset<uint8_t> b("001100110011");

	for (size_t off = 0; off != 3; ++off) {
		assert(not a[off * 4 + 0]);
		assert(a[off * 4 + 1]);
		assert(not a[off * 4 + 2]);
		assert(a[off * 4 + 3]);

		assert(not b[off * 4 + 0]);
		assert(not b[off * 4 + 1]);
		assert(b[off * 4 + 2]);
		assert(b[off * 4 + 3]);

		assert(a.any());
		assert(not a.any_outer());
		assert(a.any_inner());

		assert(not a.none());
		assert(a.none_outer());
		assert(not a.none_inner());

		assert(not a.all());
		assert(not a.all_inner());
		assert(not a.all_outer());

		bitset u = a | b;

		assert(not u[off * 4 + 0]);
		assert(u[off * 4 + 1]);
		assert(u[off * 4 + 2]);
		assert(u[off * 4 + 3]);

		bitset i = a & b;

		assert(not i[off * 4 + 0]);
		assert(not i[off * 4 + 1]);
		assert(not i[off * 4 + 2]);
		assert(i[off * 4 + 3]);

		bitset x = a ^ b;

		assert(not x[off * 4 + 0]);
		assert(x[off * 4 + 1]);
		assert(x[off * 4 + 2]);
		assert(not x[off * 4 + 3]);

		bitset d = a - b;
		assert(not d[off * 4 + 0]);
		assert(d[off * 4 + 1]);
		assert(not d[off * 4 + 2]);
		assert(not d[off * 4 + 3]);


		u = b | a;

		assert(not u[off * 4 + 0]);
		assert(u[off * 4 + 1]);
		assert(u[off * 4 + 2]);
		assert(u[off * 4 + 3]);

		i = b & a;

		assert(not i[off * 4 + 0]);
		assert(not i[off * 4 + 1]);
		assert(not i[off * 4 + 2]);
		assert(i[off * 4 + 3]);

		x = b ^ a;

		assert(not x[off * 4 + 0]);
		assert(x[off * 4 + 1]);
		assert(x[off * 4 + 2]);
		assert(not x[off * 4 + 3]);

		d = b - a;
		assert(not d[off * 4 + 0]);
		assert(not d[off * 4 + 1]);
		assert(d[off * 4 + 2]);
		assert(not d[off * 4 + 3]);
	}

	auto t = a;
	a.resize(a.size() + 7);
	for (size_t off = 0; off != 7; ++off) {
		assert(not (a | b)[b.size() + off]);
		assert(not (a & b)[b.size() + off]);
		assert(not (a ^ b)[b.size() + off]);
		assert(not (a - b)[b.size() + off]);
		assert(not (b | a)[b.size() + off]);
		assert(not (b & a)[b.size() + off]);
		assert(not (b ^ a)[b.size() + off]);
		assert(not (b - a)[b.size() + off]);
	}
	a = t;
	a.resize(a.size() + 7, true);
	for (size_t off = 0; off != 7; ++off) {
		assert((a | b)[b.size() + off]);
		assert(not (a & b)[b.size() + off]);
		assert((a ^ b)[b.size() + off]);
		assert((a - b)[b.size() + off]);
		assert((b | a)[b.size() + off]);
		assert(not (b & a)[b.size() + off]);
		assert((b ^ a)[b.size() + off]);
		assert(not (b - a)[b.size() + off]);
	}
}

} // namespace automata

#endif // #ifndef AUTOMATA_TESTS_HPP
