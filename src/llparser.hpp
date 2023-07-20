#include "cfg.hpp"

#ifndef AUTOMATA_LLPARSER_HPP
#define AUTOMATA_LLPARSER_HPP

#include <ranges>
#include <vector>
#include <variant>
#include <boost/locale/utf.hpp>
#include "dynamic_tree.hpp"

namespace automata {

struct LL_parser {
	typedef std::vector<size_t> terminal_string_type;
	typedef CFG::node_type node_type;
	typedef std::unordered_set<terminal_string_type, boost::hash<terminal_string_type>> terminal_string_set_type;
	typedef CFG::rule_index_type rule_index_type;
	typedef CFG::rule_type rule_type;
	typedef CFG::variable_type variable_type;
	//typedef std::unordered_map<rule_type, terminal_string_set_type, boost::hash<rule_type>, sequence_equal_to> rule_set_map_type;
	typedef std::unordered_map<size_t, terminal_string_set_type> variable_set_map_type;
	typedef std::pair<size_t, terminal_string_type> table_index;
	typedef decltype(
		[](const table_index& l, const table_index& r) { return l.first == r.first and sequence_equal_to()(l.second, r.second); }
	) table_index_equal_to;
	typedef std::unordered_map<
		table_index,
		rule_index_type,
		boost::hash<table_index>,
		table_index_equal_to
	> table_type;
	typedef std::unordered_map<table_index, std::vector<rule_index_type>, boost::hash<table_index>, table_index_equal_to> conflict_table;

	struct bad_grammar : std::invalid_argument {
		std::string construct(const CFG& cfg, const conflict_table& t) {
			namespace utf = boost::locale::utf;
			std::string s;
			const auto reverse_name_map = cfg.reverse_name_map();
			for (auto&& [var_ts, conflicts] : t) {
				auto&& [var_id, ts] = var_ts;
				std::string var_name;
				if (reverse_name_map.contains(var_id)) {
					var_name += ' ';
					if (reverse_name_map.at(var_id)[0] == U'(') {
						var_name = encode_as_utf8(reverse_name_map.at(var_id));
					} else {
						var_name = '(' + encode_as_utf8(reverse_name_map.at(var_id)) + ')';
					}
				}
				auto conv_term = [&](const terminal_string_type& ts) -> std::string {
					std::string result;
					result.reserve(ts.size() + 8);
					for (size_t s : ts) {
						if (s == END_OF_INPUT) {
							result += "\e[1m$\e[0m";
							break;
						} else {
							result += cfg.convert_reverse(s);
						}
					}
					return result;
				};
				s += "Conflicting rules for variable " + std::to_string(var_id) + var_name + " on lookahead '" + conv_term(ts) + "':\n";
				for (auto&& ri : conflicts) {
					s += encode_as_utf8(cfg.to_string(ri));
				}
			}
			return s;
		}
		bad_grammar(const CFG& cfg, const conflict_table& t) : std::invalid_argument(construct(cfg, t)) {}
	};

	static constexpr size_t END_OF_INPUT = (size_t)-1;

	static terminal_string_set_type truncated_concat(const terminal_string_set_type& U, const terminal_string_set_type& V, size_t k) {
		if (U.empty() || V.empty()) {
			return {};
		}
		terminal_string_set_type result;
		for (auto&& u : U) {
			if (u.size() >= k) {
				result.insert(substr(u, 0, k));
			} else {
				for (auto&& v : V) {
					result.insert(concat(u, substr(v, 0, k - u.size())));
				}
			}
		}
		return result;
	}
	static consteval size_t default_reserve_limit() noexcept {
		return size_t(1) << 20;
	}
	// T[A, u] = rule(A, w) <-> u in first(w) * follow(A)
	template <std::input_iterator It>
	static terminal_string_set_type first_set(It begin, It end, size_t k, const variable_set_map_type& var_map) {
		terminal_string_set_type s;
		s.emplace();
		for (; begin != end; ++begin) {
			auto&& node = *begin;
			if (node.variable()) {
				s = truncated_concat(s, var_map.contains(node.symbol()) ? var_map.at(node.symbol()) : terminal_string_set_type{}, k);
			} else {
				s = truncated_concat(s, terminal_string_set_type{ node.symbol() }, k);
			}
		}
		return s;
	}
	static variable_set_map_type compute_first_sets(const CFG& cfg, size_t k) {
		variable_set_map_type var_map;
		unordered_set_equal_to<terminal_string_set_type> set_eq;
		bool finished = false;
		while (not finished) {
			finished = true;
			cfg.for_each_rule([&] (const variable_type& var, const rule_type& rule) {
				size_t var_id = cfg.variable_id(var);
				auto s = first_set(rule.begin(), rule.end(), k, var_map);
				if (not set_eq(var_map[var_id], s)) {
					finished = false;
					unordered_set_union(var_map[var_id], std::as_const(s));
				}
			});
		}
		return var_map;
	}
	static variable_set_map_type compute_follow_sets(const CFG& cfg, size_t k, const variable_set_map_type& first_sets) {
		variable_set_map_type var_map;
		unordered_set_equal_to<terminal_string_set_type> set_eq;
		var_map[cfg.start_symbol()] = { terminal_string_type(k, END_OF_INPUT) };
		bool finished = false;
		while (not finished) {
			finished = true;
			cfg.for_each_rule([&] (const variable_type& var, const rule_type& rule) {
				size_t var_id = cfg.variable_id(var);
				for (auto it = rule.begin(); it != rule.end(); ++it) {
					auto&& node = *it;
					if (not node.variable()) {
						continue;
					}
					auto s = truncated_concat(first_set(++it, rule.end(), k, first_sets), var_map[var_id], k);
					if (not set_eq(var_map[node.symbol()], s)) {
						finished = false;
						unordered_set_union(var_map[node.symbol()], std::as_const(s));
					}
				}
			});
		}
		return var_map;
	}
	static table_type build_parsing_table(const CFG& cfg, size_t k) {
		auto first_sets = compute_first_sets(cfg, k);
		auto follow_sets = compute_follow_sets(cfg, k, first_sets);
		table_type result;
		conflict_table conflicts;
		cfg.for_each_rule([&](const variable_type& var, const rule_type& rule) {
			size_t var_id = cfg.variable_id(var);
			for (terminal_string_type w : truncated_concat(first_set(rule.begin(), rule.end(), k, first_sets), follow_sets.at(var_id), k)) {
				table_index table_item = { var_id, std::move(w) };
				rule_index_type rule_index = cfg.rule_index(var_id, rule);
				if (result.contains(table_item)) {
					if (conflicts[table_item].empty()) {
						conflicts[table_item].push_back(result.at(table_item));
					}
					conflicts[table_item].push_back(rule_index);
				}
				result.emplace(std::move(table_item), rule_index);
			}
		});
		if (not conflicts.empty()) {
			throw bad_grammar(cfg, conflicts);
		}
		return result;
	}

	const CFG& m_cfg;
	size_t m_lookahead;
	table_type m_table;

	LL_parser(const CFG& cfg, size_t k) : m_cfg(cfg), m_lookahead(k), m_table(build_parsing_table(cfg, m_lookahead)) {}
	LL_parser(const CFG& cfg) : LL_parser(cfg, 1) {}

	size_t start_symbol() const noexcept {
		return cfg().start_symbol();
	}
	const CFG& cfg() const noexcept {
		return m_cfg;
	}

	template <std::input_iterator It, function_like<bool, rule_index_type, size_t> Pred>
	std::pair<size_t, stack<node_type>> parse(It begin, It end, Pred pred) const noexcept(noexcept(pred)) {
		stack<node_type> stack;
		if constexpr (std::forward_iterator<It>) {
			stack.reserve(std::distance(begin, end));
		}

		stack.push(node_type(start_symbol(), true));

		size_t k = m_lookahead;
		terminal_string_type lookahead(k, END_OF_INPUT);
		auto la_begin = lookahead.begin();
		auto la_end = lookahead.end();
		auto la_last = la_end - 1;
		size_t str_pos = 0;
		auto read = [&] () {
			std::move_backward(la_begin, la_last, la_end);
			if (begin == end) {
				lookahead.back() = END_OF_INPUT;
			} else {
				lookahead.back() = *begin;
				++begin;
			}
		};
		auto read_n = [&] (size_t n) {
			for (; n != 0; --n) {
				read();
			}
		};
		auto return_value = [&] () -> std::pair<size_t, automata::stack<node_type>> {
			return { str_pos, std::move(stack) };
		};
		read_n(k);
		while (not stack.empty() and lookahead[0] != END_OF_INPUT) {
			node_type node = stack.peek();
			if (node.variable()) {
				table_index p = { node.symbol(), std::move(lookahead) };
				if (not m_table.contains(p)) {
					return return_value();
				}
				rule_index_type rule_index = m_table.at(p);;
				const rule_type& rule = cfg().rule(rule_index);
				if (not pred(rule_index, str_pos)) {
					return return_value();
				}
				stack.pop();
				stack.push(rule.rbegin(), rule.rend());
				lookahead = std::move(p.second);
			} else {
				if (node.symbol() != lookahead.back()) {
					return return_value();
				}
				stack.pop();
				++str_pos;
				read();
			}
		}
		return return_value();
	}

	struct tree_node_type : public node_type {
	protected:
		size_t m_rule;
	public:
		tree_node_type() : node(), m_rule() {}
		tree_node_type(size_t s) : node(s), m_rule() {}
		tree_node_type(size_t s, size_t r) : node(s, true), m_rule(r) {}

		size_t rule() const noexcept {
			return m_rule;
		}
		void rule(size_t r) noexcept {
			m_rule = r;
		}
	};

	template <std::input_iterator It>
	std::tuple<dynamic_tree<tree_node_type>, size_t, stack<node_type>> parse_tree(It begin, It end) const {
		dynamic_tree<tree_node_type> s;
		if constexpr (std::forward_iterator<It>) {
			s.reserve(std::distance(begin, end));
		}
		dynamic_tree<tree_node_type>::node_type* current = nullptr;
		auto&& [error_pos, stack] = parse(begin, end, [&] (rule_index_type rule_index, size_t) -> bool {
			size_t var_id = rule_index.first;
			const rule_type& rule = cfg().rule(rule_index);
			if (current == nullptr) {
				current = &s.node(s.push(var_id, true));
			}
			current->rule(rule_index.second);
			for (auto&& node : rule) {
				s.push(*current, node);
			}
			while (true) {
				auto&& c = current->children();
				auto it = std::find_if(c.begin(), c.end(), [&s](size_t n) { return s.node(n)->variable() and s.node(n).children().size() == 0; });
				if (it != rule.end()) {
					current = &s.node(*it);
					break;
				} else if (current->is_root()) {
					break;
				} else {
					current = &s.node(current->parent());
				}
			}
			return true;
		});
		return { std::move(s), error_pos, std::move(stack) };
	}
};

} // namespace automata

#include "cfg.hpp"

#endif // #ifndef AUTOMATA_LLPARSER_HPP
