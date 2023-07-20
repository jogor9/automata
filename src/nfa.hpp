#ifndef AUTOMATA_NFA_HPP
#define AUTOMATA_NFA_HPP

#include "dfa.hpp"

namespace automata {



template <
	typename StateData = empty_struct,
	typename TransitionData = empty_struct,
	typename StateAllocator = std::allocator<StateData>,
	typename TransitionAllocator = std::allocator<TransitionData>
>
struct NFA {
	using set = boost::container::flat_set<size_t>;
	using map = std::unordered_map<size_t, set>;
	using rmap = std::unordered_map<size_t, size_t>;

	state_set_struct<StateData> state_set;
	input_alphabet_struct input_alphabet;
	NFA_delta_function_struct delta_function;
	size_t start_state_index;
	accepting_set_struct accepting_set;
	
	// todo: write a base class for the FAs
};

} // namespace automata

#endif // #ifndef AUTOMATA_NFA_HPP
