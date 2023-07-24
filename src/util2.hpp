#ifndef AUTOMATA_UTIL2_HPP
#define AUTOMATA_UTIL2_HPP

#include <cstddef>

#define CALL_RETURN_ON_FAIL_IF_PREDICATE(func, ...) \
	if constexpr (std::predicate<decltype(std::bind(func __VA_OPT__(,) __VA_ARGS__))>) { \
		if (not func(__VA_ARGS__)) { \
			return false; \
		} \
	} else { \
		func(__VA_ARGS__); \
	}

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
