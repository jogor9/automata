#ifndef AUTOMATA_POSTORDER_BINARY_TREE_HPP
#define AUTOMATA_POSTORDER_BINARY_TREE_HPP
#include "util.hpp"

namespace automata {

namespace postorder_binary_tree_impl {

struct node_type {

};

} // namespace impl

template <
	typename T,
	std::unsigned_integral I = size_t,
	std::ranges::random_access_range Container = std::vector<postorder_binary_tree_impl::node_type>,
	std::invocable<Container&, Container&> Splicer = decltype([](Container& lhs, Container& rhs) { lhs.insert(lhs.end(), rhs.begin(), rhs.end()); })
>
struct postorder_binary_tree {
};


} // namespace automata

#endif // #ifndef AUTOMATA_POSTORDER_BINARY_TREE_HPP
