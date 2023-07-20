#ifndef AUTOMATA_UTIL2_HPP
#define AUTOMATA_UTIL2_HPP

#include <cstddef>

namespace automata {

template <typename T>
consteval size_t bit_count() {
	return sizeof(T) * 8;
}

template <typename T = size_t>
constexpr T pow2(size_t n) {
	return T(1) << n;
}

template <typename T = size_t>
constexpr T pow2mask(size_t n) {
	return pow2<T>(n) - T(1);
}

}

#endif
