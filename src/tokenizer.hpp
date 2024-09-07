#ifndef AUTOMATA_TOKENIZER_HPP
#define AUTOMATA_TOKENIZER_HPP

#include <numeric>
#include "regexp.hpp"

namespace automata {

template < std::input_iterator RuleIt >
regexp tokenizer_regexp(RuleIt lbegin, RuleIt lend) {
	regexp result = regexp::null_regexp();
	for (; lbegin != lend; ++lbegin) {
		regexp re(*lbegin);
		result.unify(std::move(re.remove_captures().capture()));
	}
	return result;
}

regexp::nfa_type tokenizer(const regexp& tokenizer_re) {
	return tokenizer_re.compile();
}

template <std::forward_iterator It, std::invocable<size_t, size_t, size_t> Callback>
bool tokenize(regexp::nfa_type& tokenizer, It begin, It end, Callback callback) {
	while (begin != end) {
		auto results = regexp::match_leftmost_longest(tokenizer, begin, end);
		auto sav = begin;
		for (size_t gid = 1; gid < results.size(); ++gid) {
			if (not results[gid].empty()) {
				auto&& capture = *results[gid].begin();
				std::advance(begin, capture.end() - capture.start());
				callback(gid - 1, capture.start(), capture.end());
				break;
			}
		}
		if (sav == begin) {
			return false;
		}
	}
	return true;
}



} // namespace automata

#endif // #ifndef AUTOMATA_TOKENIZER_HPP
