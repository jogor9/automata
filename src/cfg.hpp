#ifndef AUTOMATA_CFG_HPP
#define AUTOMATA_CFG_HPP

#include <vector>
#include <unordered_set>
#include <ranges>
#include "fa.hpp"
#include "util.hpp"

namespace automata {

struct CFG {
	struct node {
	private:
		size_t m_symbol;
	public:

		static constexpr decltype(m_symbol) IDMASK = pow2mask<decltype(m_symbol)>(bit_count<decltype(m_symbol)>() - 1);
		static constexpr decltype(m_symbol) TYPEMASK = pow2<decltype(m_symbol)>(bit_count<decltype(m_symbol)>() - 1);

		node() : m_symbol(0) {}

		node(size_t s) : node() { symbol(s); }
		node(size_t s, bool v) : node(s) { variable(v); }

		size_t symbol() const noexcept {
			return m_symbol & IDMASK;
		}
		void symbol(size_t s) noexcept {
			m_symbol = (s & IDMASK) | (m_symbol & ~IDMASK);
		}
		bool variable() const noexcept {
			return bool(m_symbol & TYPEMASK);
		}
		void variable(bool b) noexcept {
			if (b) {
				m_symbol |= TYPEMASK;
			} else {
				m_symbol &= ~TYPEMASK;
			}
		}

		friend std::size_t hash_value(const node& n) noexcept;
		friend bool operator==(const node& l, const node& r) noexcept;
	};
	typedef node node_type;
	typedef std::vector<node_type> rule_type;
	typedef std::vector<rule_type> variable_type;
	typedef std::basic_string<char32_t> string_type;

	typedef std::pair<size_t, size_t> rule_index_type;

	std::unordered_map<string_type, size_t> m_names_to_vars;
	std::vector<variable_type> m_variables;
	size_t m_start_symbol;

	static constexpr rule_type EMPTY_RULE = {};
	static constexpr variable_type EMPTY_VARIABLE = {};
	input_alphabet_struct m_alphabet;

	template <std::input_iterator It>
	static CFG construct_from_string(It begin, It end);

	CFG() : m_names_to_vars(), m_variables(), m_start_symbol(-1), m_alphabet() {}

	template <std::input_iterator It>
	CFG(It begin, It end) : CFG() {
		std::tie(m_variables, m_names_to_vars) = construct_from_string(begin, end);
	}

	const std::unordered_map<string_type, size_t>& name_map() const noexcept {
		return m_names_to_vars;
	}
	std::unordered_map<size_t, string_type> reverse_name_map() const {
		auto rmap = reverse_mapping(name_map());
		std::unordered_map<size_t, string_type> result;
		result.reserve(rmap.size());
		for (auto&& [var_id, vec] : rmap) {
			if (vec.size() > 1) {
				string_type r;
				r += '(';
				auto it = vec.begin();
				r += *it++;
				for (; it != vec.end(); ++it) {
					r += U" or " + *it;
				}
				r += ')';
			} else {
				result.emplace(var_id, vec[0]);
			}
		}
		return result;
	}
	const input_alphabet_struct& alphabet() const noexcept {
		return m_alphabet;
	}
	size_t convert(char32_t s) const noexcept {
		return alphabet().convert(s);
	}
	char32_t convert_reverse(size_t s) const noexcept {
		return alphabet().convert_reverse(s);
	}
	size_t variable(const std::basic_string<char32_t>& s) const {
		if (m_names_to_vars.contains(s)) {
			return m_names_to_vars.at(s);
		} else {
			return (size_t)-1;
		}
	}

	const std::vector<variable_type>& variables() const noexcept {
		return m_variables;
	}
	size_t start_symbol() const noexcept {
		return m_start_symbol;
	}
	void start_symbol(size_t s) noexcept {
		if (has_variable(s)) {
			m_start_symbol = s;
		}
	}
	size_t variable_count() const noexcept {
		return variables().size();
	}
	size_t variable_id(const variable_type& v) const noexcept {
		return &v - m_variables.data();
	}
	size_t push_variable() {
		variables().emplace_back();
		return variable_count() - 1;
	}
	template <std::invocable<size_t, size_t> Callback>
	bool for_each_symbol_in_terminal_set(Callback callback) {
		for (auto&& [a, b] : *alphabet().input_map) {
			if constexpr (std::predicate<Callback, size_t, size_t>) {
				if (not callback(a, b)) {
					return false;
				}
			} else {
				callback(a, b);
			}
		}
		return true;
	}
	template <typename CharT>
	size_t push_charset(const CharT* s) {
		size_t result = push_variable();
		auto&& [charset, comp] = obtain_charset(s);
		for_each_symbol_in_terminal_set([&] (size_t orig, size_t conv) {
			if (conv == 0) {
				if (comp) {
					push_rule(result, rule_type{ conv });
				}
			} else if (charset.contains(orig) ^ comp) {
				push_rule(result, rule_type{ conv });
			}
		});
		return result;
	}
	size_t push_wildcard() {
		size_t result = push_variable();
		for_each_symbol_in_terminal_set([&](size_t, size_t conv) {
			push_rule(result, rule_type{ conv });
		});
		return result;
	}
	variable_type pop_variable() {
		if (variable_count() == 0) {
			return variable_type();
		}
		variable_type result = std::move(variables().back());
		variables().pop_back();
		return result;
	}
	rule_index_type push_rule(size_t var, rule_type rule) {
		if (not has_variable(var)) {
			return rule_index_type(-1, -1);
		}
		variables()[var].push_back(std::move(rule));
		return rule_index_type(var, variables()[var].size() - 1);
	}
	rule_index_type push_rule(size_t var) {
		return push_rule(var, {});
	}
	template <typename...Args>
		requires (sizeof...(Args) >= 1 and not std::same_as<std::decay_t<typename std::tuple_element<0, std::tuple<Args...>>::type>, rule_type>)
	rule_index_type push_rule(size_t var, Args&&...args) {
		return push_rule(var, make_rule(std::forward<Args>(args)...));
	}
	std::vector<variable_type>& variables() noexcept {
		return m_variables;
	}
	bool has_variable(size_t n) const noexcept {
		return variables().size() > n;
	}
	const variable_type& variable_rules(size_t n) const noexcept {
		if (has_variable(n)) {
			return variables().at(n);
		} else {
			return EMPTY_VARIABLE;
		}
	}
	variable_type& variable_rules(size_t n) {
		return variables().at(n);
	}
	bool has_rule(rule_index_type i) const noexcept {
		return has_variable(i.first) and i.second < variable_rules(i.first).size();
	}
	bool has_rule(const rule_type& c) const noexcept {
		bool result = false;
		for_each_rule([&result, &c] (const variable_type&, const rule_type& r) {
			if (&r == &c) {
				result = true;
			}
		});
		return result;
	}
	rule_index_type rule_index(size_t var_id, const rule_type& r) const noexcept {
		return { var_id, &r - variable_rules(var_id).data() };
	}
	const rule_type& rule(rule_index_type i) const noexcept {
		if (not has_rule(i)) {
			return EMPTY_RULE;
		} else {
			return variable_rules(i.first)[i.second];
		}
	}
	template <std::invocable<variable_type> F>
	void for_each_variable(F callback) noexcept(noexcept(callback)) {
		for (auto&& var : variables()) {
			callback(var);
		}
	}
	template <std::invocable<variable_type, rule_type> F>
	void for_each_rule(F callback) noexcept(noexcept(callback)) {
		for_each_variable([f = std::move(callback)](variable_type& var) mutable {
			for (auto&& rule : var) {
				f(var, rule);
			}
		});
	}
	template <std::invocable<variable_type, rule_type, node_type> F>
	void for_each_node(F callback) noexcept(noexcept(callback)) {
		for_each_rule([f = std::move(callback)] (variable_type& var, rule_type& rule) mutable {
			for (auto&& node : rule) {
				f(var, rule, node);
			}
		});
	}
	template <std::invocable<variable_type> F>
	void for_each_variable(F callback) const noexcept(noexcept(callback)) {
		for (auto&& var : variables()) {
			callback(var);
		}
	}
	template <std::invocable<variable_type, rule_type> F>
	void for_each_rule(F callback) const noexcept(noexcept(callback)) {
		for_each_variable([f = std::move(callback)](const variable_type& var) mutable {
			for (auto&& rule : var) {
				f(var, rule);
			}
		});
	}
	template <std::invocable<variable_type, rule_type, node_type> F>
	void for_each_node(F callback) const noexcept(noexcept(callback)) {
		for_each_rule([f = std::move(callback)] (const variable_type& var, const rule_type& rule) mutable {
			for (auto&& node : rule) {
				f(var, rule, node);
			}
		});
	}
	size_t node_count() const noexcept {
		size_t result = 0;
		for_each_rule([&result] (const variable_type&, const rule_type& rule) {
			result += rule.size();
		});
		return result;
	}
	bool empty() const noexcept {
		return variable_count() == 0;
	}
	size_t variable_set_size() const {
		std::unordered_set<size_t> counted;
		counted.reserve(node_count());
		size_t result = 0;
		for_each_node([&counted, &result] (const variable_type&, const rule_type&, const node_type& node) {
			if (node.variable() and not counted.contains(node.symbol())) {
				counted.insert(node.symbol());
				++result;
			}
		});
		return result;
	}
	size_t terminal_set_size() const {
		return node_count() - variable_set_size();
	}

	string_type to_string() const {
		string_type result;
		// variable "A ->"
		// first rule ... "\n"
		// subsequent rules "|" ... "\n"
		// variable " V"
		// terminal " 't'"

		for (auto&& var : variables()) {
			size_t var_id = variable_id(var);
			result.append(to_string(var_id));
		}
		return result;
	}
	string_type to_string(size_t var_id) const {
		string_type result;
		const auto rmap = reverse_name_map();
		auto var_conv = [&] (size_t s) -> string_type {
			if (rmap.contains(s)) {
				return rmap.at(s);
			} else {
				auto&& str = std::to_string(s);
				return string_type(str.begin(), str.end());
			}
		};
		auto&& rules = variable_rules(var_id);
		if (rules.empty()) {
			return result;
		}
		result.append(to_string(rule_index_type(var_id, 0)));
		auto rule_it = ++rules.begin();
		for (; rule_it != rules.end(); ++rule_it) {
			result += '|';
			for (auto&& node : *rule_it) {
				result += ' ';
				if (node.variable()) {
					result += var_conv(node.symbol());
				} else {
					result += '\'';
					result += convert_reverse(node.symbol());
					result += '\'';
				}
			}
			result += '\n';
		}
		return result;
	}
	string_type to_string(rule_index_type rule_index) const {
		string_type result;
		const auto rmap = reverse_name_map();
		auto var_conv = [&] (size_t s) -> string_type {
			if (rmap.contains(s)) {
				return rmap.at(s);
			} else {
				auto&& str = std::to_string(s);
				return string_type(str.begin(), str.end());
			}
		};
		auto&& r = rule(rule_index);
		result += var_conv(rule_index.first);
		result += ' ';
		result += '-';
		result += '>';
		for (auto&& node : r) {
			result += ' ';
			if (node.variable()) {
				result += var_conv(node.symbol());
			} else {
				result += '\'';
				result += convert_reverse(node.symbol());
				result += '\'';
			}
		}
		result += '\n';
		return result;
	}

	rule_type make_rule(rule_type r) {
		return r;
	}
	template <typename CharT, typename...Args>
	rule_type make_rule(rule_type r, const CharT* t, Args&&...args) {
		const CharT* end = t + std::char_traits<CharT>::length(t);
		r.reserve(end - t);
		for (; t != end; ++t) {
			r.push_back(node_type(convert(*t)));
		}
		return make_rule(std::move(r), std::forward<Args>(args)...);
	}
	template <typename...Args>
	rule_type make_rule(rule_type r, size_t v, Args&&...args) {
		r.push_back(node_type(v, true));
		return make_rule(std::move(r), std::forward<Args>(args)...);
	}
	template <typename...Args>
		requires (not std::same_as<std::decay_t<typename std::tuple_element<0, std::tuple<Args...>>::type>, rule_type>)
	rule_type make_rule(Args&&...args) {
		return make_rule(rule_type(), std::forward<Args>(args)...);
	}

	static const CFG& syntax_grammar() {
		static CFG cfg;
		if (cfg.empty()) {
			size_t GRAMMAR = cfg.push_variable();
			size_t DECLSTRING = cfg.push_variable();
			size_t DECLSTRINGTAIL = cfg.push_variable();
			size_t DECL = cfg.push_variable();
			size_t RULESETOPT = cfg.push_variable();
			size_t RULESETCLOS = cfg.push_variable();
			size_t RULE = cfg.push_variable();
			size_t VARIABLE = cfg.push_variable();
			size_t VARIABLETAIL = cfg.push_variable();
			size_t TERMEXPR = cfg.push_variable();
			size_t SQTERM = cfg.push_variable();
			size_t DQTERM = cfg.push_variable();
			size_t GATERM = cfg.push_variable();
			size_t ESCAPE = cfg.push_variable();
			size_t CHARSET = cfg.push_variable();
			size_t SETESCAPE = cfg.push_variable();
			size_t WS = cfg.push_variable();
			size_t WSCHAR = cfg.push_charset(R"([ \n\f\v\t\r])");
			size_t IDCHAR = cfg.push_charset(R"([^-|'`"])");
			size_t SQTERMCHARSET = cfg.push_charset(R"([^\\'\[\]])");
			size_t DQTERMCHARSET = cfg.push_charset(R"([^\\"\[\]])");
			size_t GATERMCHARSET = cfg.push_charset(R"([^\\`\[\]])");
			size_t ESCAPECHAR = cfg.push_charset(R"([^\[\]\\])");
			size_t CHARSETCHARSET = cfg.push_charset(R"([^\[\]\\])");
			size_t WILDCARD = cfg.push_wildcard();

			cfg.push_rule(GRAMMAR, VARIABLE, DECLSTRING);
			cfg.push_rule(GRAMMAR);
			cfg.push_rule(DECLSTRING, DECL, DECLSTRINGTAIL);
			cfg.push_rule(DECLSTRINGTAIL, DECL, DECLSTRINGTAIL);
			cfg.push_rule(DECLSTRINGTAIL);
			cfg.push_rule(DECL, RULESETOPT);
			cfg.push_rule(RULESETOPT, RULE, RULESETCLOS);
			cfg.push_rule(RULESETOPT);
			cfg.push_rule(RULESETCLOS, "|", RULE, RULESETCLOS);
			cfg.push_rule(RULESETCLOS);
			cfg.push_rule(RULE, VARIABLE, RULE);
			cfg.push_rule(RULE, TERMEXPR, RULE);
			cfg.push_rule(RULE);
			cfg.push_rule(VARIABLE, IDCHAR, VARIABLETAIL);
			cfg.push_rule(VARIABLETAIL, IDCHAR, VARIABLETAIL);
			cfg.push_rule(VARIABLETAIL);
			cfg.push_rule(TERMEXPR, "'", SQTERM, "'");
			cfg.push_rule(TERMEXPR, "\"", DQTERM, "\"");
			cfg.push_rule(TERMEXPR, "`", GATERM, "`");
			cfg.push_rule(SQTERM, "[", CHARSET, "]", SQTERM);
			cfg.push_rule(SQTERM, SQTERMCHARSET, SQTERM);
			cfg.push_rule(SQTERM, ESCAPE, SQTERM);
			cfg.push_rule(SQTERM);
			cfg.push_rule(DQTERM, "[", CHARSET, "]", DQTERM);
			cfg.push_rule(DQTERM, DQTERMCHARSET, DQTERM);
			cfg.push_rule(DQTERM, ESCAPE, DQTERM);
			cfg.push_rule(DQTERM);
			cfg.push_rule(GATERM, "[", CHARSET, "]", GATERM);
			cfg.push_rule(GATERM, GATERMCHARSET, GATERM);
			cfg.push_rule(GATERM, ESCAPE, GATERM);
			cfg.push_rule(GATERM);
			cfg.push_rule(ESCAPE, "\\", ESCAPECHAR);
			cfg.push_rule(CHARSET, CHARSETCHARSET, CHARSET);
			cfg.push_rule(CHARSET, SETESCAPE, CHARSET);
			cfg.push_rule(CHARSET);
			cfg.push_rule(SETESCAPE, "\\", WILDCARD);

			// WSCHAR = '[ \n\f\v\t\r]'
			// WS = WSCHAR WS | ε
			// GRAMMAR -> WS VARIABLE DECLSTRING | ε
			// DECLSTRING -> DECL DECLSTRINGTAIL
			// DECLSTRINGTAIL -> DECL DECLSTRINGTAIL | ε
			// DECL -> '->' RULESETOPT
			// # RULESET -> (RULE WS ('|' RULE)*)?
			// RULESETOPT -> RULE RULESETCLOS | ε
			// RULESETCLOS -> '|' RULE RULESETCLOS | ε
			// RULE -> VARIABLE RULE | TERMEXPR RULE | ε
			// WSVARIABLE -> WSCHAR VARIABLE
			// VARIABLE -> IDCHAR VARIABLETAIL
			// VARIABLETAIL -> IDCHAR VARIABLETAIL | ε
			// IDCHAR -> '[^#-|'`"]'
			// TERMEXPR -> `'` SQTERM `'` | '"' DQTERM '"' | '`' GATERM '`'
			// SQTERM -> '[' CHARSET ']' SQTERM | '[^\\'\[\]]' SQTERM | ESCAPE SQTERM | ε
			// DQTERM -> '[' CHARSET ']' DQTERM | '[^\\"\[\]]' DQTERM | ESCAPE DQTERM | ε
			// GATERM -> '[' CHARSET ']' GATERM | '[^\\`\[\]]' GATERM | ESCAPE GATERM | ε
			// ESCAPE -> '\\' ESCAPECHAR
			// ESCAPECHAR -> '[\\ntvrf.\[\]]'
			// CHARSET -> '[^\[\]\\]' CHARSET | SETESCAPE CHARSET | ε
			// SETESCAPE -> '\\.'
			//
			//
		}
		return cfg;
	}
};

std::size_t hash_value(const CFG::node_type& n) noexcept {
	return std::hash<decltype(n.m_symbol)>()(n.m_symbol);
}

bool operator==(const CFG::node_type& l, const CFG::node_type& r) noexcept {
	return l.m_symbol == r.m_symbol;
}


} // namespace automata
#endif // #ifndef AUTOMATA_CFG_HPP

#ifdef AUTOMATA_LLPARSER_HPP

namespace automata {
	template <std::input_iterator It>
	CFG CFG::construct_from_string(It ibegin, It iend) {
		static const LL_parser parser = LL_parser(syntax_grammar());
	}
}

#endif

