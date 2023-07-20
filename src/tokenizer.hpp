#ifndef AUTOMATA_TOKENIZER_HPP
#define AUTOMATA_TOKENIZER_HPP

#include <numeric>
#include "regexp.hpp"

namespace automata {

template <
	std::input_iterator RuleIt
>
regexp tokenizer_regexp(RuleIt lbegin, RuleIt lend) {
	regexp result = regexp::null_regexp();
	size_t re_count = 0;
	for (; lbegin != lend; ++lbegin, ++re_count) {
		regexp re = *lbegin;
		result.unify(std::move(re.remove_captures().capture()));
	}
	return result.to_closure();
}

regexp::automaton_type tokenizer(const regexp& tokenizer_re) {
	return tokenizer_re.compile();
}

regexp::automaton_type tokenizer_deterministic(const regexp& tokenizer_re, size_t state_limit) {
	return tokenizer_re.compile_deterministic(state_limit);
}

regexp::automaton_type tokenizer_deterministic(const regexp& tokenizer_re) {
	return tokenizer_deterministic(tokenizer_re, 1 << 20);
}

typedef std::ranges::subrange<std::set<std::pair<size_t, size_t>>::const_iterator> tokenizer_set_view;

template <std::input_iterator It, std::invocable<tokenizer_set_view, size_t> Callback>
	requires std::same_as<std::invoke_result_t<Callback, tokenizer_set_view, size_t>, size_t>
void tokenize(regexp::automaton_type& tokenizer, It begin, It end, Callback callback) {

	auto results = regexp::match(tokenizer, begin, end);
	if (results.empty()) {
		return;
	}
	// reinserting results into another std::set would take O(TClog(C)) time (C == capture count, T == token ids)
	// using a heap reduces this to O(
	std::vector<std::tuple<size_t, size_t, size_t>> intervals;
	intervals.reserve((results.size() - 1) * std::reduce(results.begin(), results.end(), size_t(0), [] (auto&& s) { s.size(); }));
	
	std::vector<std::set<std::pair<size_t, size_t>>::iterator> iters;
	iters.reserve(results.size() - 1);
	for (auto begin = results.begin() + 1, it = begin, end = results.end(); it != end; ++it) {
		iters.push_back(it);
	}

	auto cmp = [&] (size_t l, size_t r) {
		if (iters[l] == results.end()) {
			return iters[r] != results.end();
		} else if (iters[r] == results.end()) {
			return false;
		} else {
			return std::tie(iters[l]->first, l, iters[l]->second) > std::tie(iters[r]->first, r, iters[r]->second);
		}
	};
	std::vector<size_t> heap;
	heap.resize(iters.size());
	std::iota(heap.begin(), heap.end(), size_t(0));
	make_heap(iters.begin(), iters.end(), cmp);


}



} // namespace automata

#endif // #ifndef AUTOMATA_TOKENIZER_HPP
