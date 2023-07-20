#include <algorithm>
#include <concepts>
#include <vector>
#include <bit>
#include <unordered_set>
#include <functional>

namespace automata {
	using namespace std;

	template <typename T>
		requires (sizeof(T) <= sizeof(size_t))
	consteval size_t max_shift() {
		return bit_width(size_t(1) << (sizeof(T) * 8 - 1)) - 1;
	}

	template <typename T>
	consteval size_t max_shift() {
		return bit_width(T(1) << (sizeof(T) * 8 - 1)) - 1;
	}

	template <typename InputIt, typename Func>
		requires input_iterator<InputIt> && invocable<Func, decltype(*declval<InputIt>()), decltype(*declval<InputIt>())>
	void for_each_adjacent(InputIt begin, InputIt end, Func f) {
		if (begin == end) {
			return;
		}
		auto it0 = begin;
		auto it1 = ++begin;
		for (; it1 != end; ++it0, ++it1) {
			 f(*it0, *it1);
		}
	}

	struct phf_base_data {
		size_t first;
		size_t shift;
		size_t mask;
		size_t offset;
	};

	size_t phf_base(size_t key, phf_base_data data) {
		return ((key - data.first) >> data.shift & data.mask) + data.offset;
	}

	template <typename InputIt, typename OutputIt>
		requires input_iterator<InputIt> && output_iterator<OutputIt, size_t>
	OutputIt phf_base_transform(phf_base_data data, InputIt begin, InputIt end, OutputIt out) {
		for (; begin != end; ++begin, ++out) {
			*out = phf_base(*begin, data);
		}
		return out;
	}

	template <typename InputIt, typename OutputIt>
		requires input_iterator<InputIt> && output_iterator<OutputIt, size_t>
	OutputIt phf_base_first(size_t first, InputIt begin, InputIt end, OutputIt out) {
		for (; begin != end; ++begin, ++out) {
			*out++ = *begin - first;
		}
		return out;
	}
	template <typename InputIt, typename OutputIt>
		requires input_iterator<InputIt> && output_iterator<OutputIt, size_t>
	OutputIt phf_base_shift(size_t shift, InputIt begin, InputIt end, OutputIt out) {
		for (; begin != end; ++begin, ++out) {
			*out++ = *begin >> shift;
		}
		return out;
	}
	template <typename InputIt, typename OutputIt>
		requires input_iterator<InputIt> && output_iterator<OutputIt, size_t>
	OutputIt phf_base_mask(size_t mask, InputIt begin, InputIt end, OutputIt out) {
		for (; begin != end; ++begin, ++out) {
			*out++ = *begin & mask;
		}
		return out;
	}


	template <typename InputIt, typename It1, typename It2>
		requires input_iterator<InputIt>
		&& output_iterator<It1, size_t> && input_iterator<It1>
		&& output_iterator<It2, size_t> && input_iterator<It2>
	tuple<It1, It2, phf_base_data> phf_compute_and_transform(InputIt begin, InputIt end, It1 diff_buf, It2 trans_buf, size_t n) {
		auto diff_end = phf_compute_diffs(begin, end, diff_buf);
		size_t first = phf_base_compute_first(begin);
		size_t shift = phf_base_compute_shift(diff_buf, diff_end);
		auto trans_end = phf_base_first(first, begin, end, trans_buf);
		phf_base_shift(shift, trans_buf, trans_end, trans_buf);
		size_t mask = phf_base_compute_mask(n, *max_element(trans_buf, trans_end), trans_buf, trans_end);
		phf_base_mask(mask, trans_buf, trans_end, trans_buf);
		sort(trans_buf, trans_end);
		return { diff_end, trans_end, { first, shift, mask } };
	}
	template <typename InputIt, typename It1, typename It2>
		requires input_iterator<InputIt>
		&& output_iterator<It1, size_t> && input_iterator<It1>
		&& output_iterator<It2, size_t> && input_iterator<It2>
	tuple<It1, It2, phf_base_data> phf_compute_and_transform(InputIt begin, InputIt end, It1 diff_buf, It2 trans_buf) {
		return phf_compute_and_transform(begin, end, diff_buf, trans_buf, distance(begin, end));
	}

	template <typename InputIt>
		requires input_iterator<InputIt>
	size_t phf_base_compute_first(InputIt begin) {
		return *begin;
	}

	template <typename InputIt>
		requires input_iterator<InputIt>
	size_t phf_base_compute_shift(InputIt diffs_begin, InputIt diffs_end) {
		return bit_width(*min_element(diffs_begin, diffs_end)) - 1;
	}

	template <typename InputIt>
		requires input_iterator<InputIt>
	// [begin, end) should be offset and shifted array elements
	// n is the size of the array, max_el is the max element
	size_t phf_base_compute_mask(size_t n, size_t max_el, InputIt begin, InputIt end) {
		size_t mask = bit_ceil(n) - 1;
		size_t max_mask = bit_ceil(max_el) - 1;
		unordered_set<size_t> buf;
		buf.reserve(n);
		for (; mask < max_mask; mask = mask << 1 | 1) {
			bool all_unique = true;
			for (auto it = begin; it != end; ++it) {
				auto [_, inserted] = buf.insert(*it & mask);
				if (!inserted) {
					all_unique = false;
					break;
				}
			}
			buf.clear();
			if (all_unique) {
				return mask;
			}
		}
		return max_mask;
	}

	template <typename InputIt>
		requires input_iterator<InputIt>
	// [begin, end) should be offset, shifted and masked array elements
	size_t phf_compute_waste(InputIt begin, InputIt end) {
		size_t result = 0;
		for_each_adjacent(begin, end, [&result] (size_t l, size_t r) { result += (r - l) - 1; });
		return result;
	}

	template <typename InputIt, typename OutputIt>
		requires input_iterator<InputIt> && output_iterator<OutputIt, size_t>
	OutputIt phf_compute_diffs(InputIt begin, InputIt end, OutputIt out) {
		for_each_adjacent(begin, end, [&out] (size_t l, size_t r) { *out++ = r - l; });
		return out;
	}
	template <typename InputIt, typename It1, typename It2>
		requires input_iterator<InputIt>
		&& output_iterator<It1, size_t> && input_iterator<It1>
		&& output_iterator<It2, size_t> && input_iterator<It2>
	tuple<size_t, It1, It2, phf_base_data> phf_transform_and_compute_waste(InputIt begin, InputIt end, It1 diff_buf, It2 trans_buf, size_t n) {
		auto [diff_end, trans_end, data] = phf_compute_and_transform(begin, end, diff_buf, trans_buf, n);
		return { phf_compute_waste(trans_buf, trans_end), diff_end, trans_end, data };
	}

	template <typename InputIt, typename It1, typename It2>
		requires input_iterator<InputIt>
		&& output_iterator<It1, size_t> && input_iterator<It1>
		&& output_iterator<It2, size_t> && input_iterator<It2>
	tuple<size_t, It1, It2, phf_base_data> phf_transform_and_compute_waste(InputIt begin, InputIt end, It1 diff_buf, It2 trans_buf) {
		return phf_transform_and_compute_waste(begin, end, diff_buf, trans_buf, distance(begin, end));
	}

	template <typename InputIt, typename It1, typename It2>
		requires input_iterator<InputIt>
		&& output_iterator<It1, size_t> && input_iterator<It1>
		&& output_iterator<It2, size_t> && input_iterator<It2>
	tuple<
		InputIt, size_t, // split point, reduced waste
		size_t, phf_base_data, // waste and function data for the left array
		size_t, phf_base_data  // waste and function data for the right array
	> phf_compute_best_split(InputIt begin, InputIt end, It1 diff_buf, It2 trans_buf, size_t n, size_t waste, size_t offset) {
		auto s = begin;
		++s;
		size_t best = waste;
		auto result = { begin, best, 0, {}, 0, {} };
		size_t i = 1;
		for (; s != end; ++s, ++i) {
			auto [w0, _0, trans_end, data0] = phf_transform_and_compute_waste(begin, s, diff_buf, trans_buf, i);
			size_t first_free_index = *max_element(trans_buf, trans_end) + 1;
			auto [w1, _2, _3, data1] = phf_transform_and_compute_waste(s, end, diff_buf, trans_buf, n - i);
			size_t w = w0 + w1;
			if (w < best) {
				best = w;
				data0.offset = offset;
				data1.offset = offset + first_free_index;
				result = make_tuple(s, w, w0, data0, w1, data1);
			}
		}
		return result;
	}

	template <typename T, typename InputIt>
		requires convertible_to<T, size_t> && input_iterator<InputIt> && requires(InputIt t) { { *t } -> convertible_to<T>; }
	tuple<function<size_t(const T&)>, size_t> generate_perfect_hash_function_ordered(InputIt begin, InputIt end) {
		if (begin == end) {
			return { nullptr, -1 };
		}
		size_t element_count = distance(begin, end);
		if (element_count < 2) {
			return { [](const T&) -> size_t { return 0; }, 1 };
		}
		size_t min_gap = -1;
		for_each_adjacent(begin, end, [&min_gap] (const auto& a, const auto& b) { min_gap = min(min_gap, size_t(b) - size_t(a)); });
		size_t shift = bit_width(min_gap) - 1;

		size_t max_gap = 0;
		size_t offset = 0;
		for_each_adjacent(begin, end, [&max_gap, &offset, shift] (const auto& a, const auto& b) {
			size_t diff = (size_t(*b) >> shift) - (size_t(*a) >> shift);
			if (diff > max_gap) {
				max_gap = diff;
				offset = size_t(*a);
			}
		});

		size_t max_el = size_t(*max_element(begin, end, [](const auto& l, const auto& r) { return size_t(l) < size_t(r); }));
		max_el >>= shift;

		size_t next_pow2 = bit_ceil(max_el);
		if (next_pow2 - max_el > 
	}

	template <typename T, typename InputIt, typename Comp>
		requires convertible_to<T, size_t> && input_iterator<InputIt> && relation<Comp, T, T> &&
		requires(InputIt t) { { *t } -> convertible_to<T>; }
	auto generate_perfect_hash_function(InputIt begin, InputIt end, Comp&& cmp) {
		sort(begin, end, forward<Comp>(cmp));
		return generate_perfect_hash_function_ordered(begin, end);
	}
	template <typename T, typename InputIt>
		requires convertible_to<T, size_t> && input_iterator<InputIt> && requires(InputIt t) { { *t } -> convertible_to<T>; }
	auto generate_perfect_hash_function(InputIt begin, InputIt end) {
		sort(begin, end);
		return generate_perfect_hash_function_ordered(begin, end);
	}
}
