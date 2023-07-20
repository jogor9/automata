#ifndef AUTOMATA_SINK_ITERATOR_HPP
#define AUTOMATA_SINK_ITERATOR_HPP

#include <type_traits>
#include <iterator>

namespace automata {
	template <typename T>
		requires (!std::is_const_v<std::remove_reference_t<T>>) && std::is_default_constructible_v<T>
	struct sink_iterator {
		typedef ptrdiff_t difference_type;
		inline static T t = T();

		constexpr sink_iterator() noexcept {}

		constexpr sink_iterator& operator++() noexcept { return *this; }
		constexpr T& operator*() noexcept { return t; }
		constexpr sink_iterator operator++(int) const noexcept { return *this; }
	};

	static_assert(std::output_iterator<sink_iterator<int>, int>);
}

#endif // #ifndef AUTOMATA_SINK_ITERATOR_HPP
