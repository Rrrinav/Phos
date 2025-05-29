#pragma once

#include <expected>
#include <variant>
#include <string>
#include "../lexer/token.hpp"

namespace utl
{
  template <typename Op>
  constexpr std::string_view get_operator_symbol()
  {
    if constexpr (std::is_same_v<Op, std::plus<>>)               return "+";
    else if constexpr (std::is_same_v<Op, std::minus<>>)         return "-";
    else if constexpr (std::is_same_v<Op, std::multiplies<>>)    return "*";
    else if constexpr (std::is_same_v<Op, std::divides<>>)       return "/";
    else if constexpr (std::is_same_v<Op, std::greater<>>)       return ">";
    else if constexpr (std::is_same_v<Op, std::greater_equal<>>) return ">=";
    else if constexpr (std::is_same_v<Op, std::less<>>)          return "<";
    else if constexpr (std::is_same_v<Op, std::less_equal<>>)    return "<=";
    else return "<unknown-op>";
  }

  template <typename T>
  concept Convertible_to_bool_or_double = std::convertible_to<T, double> || std::convertible_to<T, bool>;

  template <typename F>
  concept Binary_number_op = requires(F f, double a, double b) {
      requires Convertible_to_bool_or_double<decltype(f(a, b))>;
  };

  template <Binary_number_op Op>
  [[nodiscard]]
  std::expected<lex::Literal_obj, std::string> binary_number_op(const lex::Literal_obj &l, const lex::Literal_obj &r, Op op)
  {
    auto lv = std::get_if<double>(&l);
    auto rv = std::get_if<double>(&r);
    if (!lv || !rv)
    {
      std::string err = std::format("Invalid operands for operation ' {} '. Expected numbers, got {}, {}.\n",
        utl::get_operator_symbol<Op>(),
        lex::literalobj_type_to_string(l.index()),
        lex::literalobj_type_to_string(r.index())
      );
      return std::unexpected(err);
    }

    return op(*lv, *rv);
  }

  [[nodiscard]]
  std::expected<lex::Literal_obj, std::string> is_equal(const lex::Literal_obj& x, lex::Literal_obj& y)
  {
    if (x.index() != y.index())
    {
      std::string err = std::format(
        "Type mismatch in equality check: '{}' vs '{}'.",
        lex::literalobj_type_to_string(x.index()),
        lex::literalobj_type_to_string(y.index())
      );
      return std::unexpected(err);
    }

    return x == y;
  };

  bool is_truthy(const lex::Literal_obj& value)
  {
    if (std::holds_alternative<bool>(value))
      return std::get<bool>(value);
    if (std::holds_alternative<double>(value))
      return std::get<double>(value) != 0.0;
    if (std::holds_alternative<std::string>(value))
      return !std::get<std::string>(value).empty();
    return false; // For null or other types
  }

  std::string stringify_literal(lex::Literal_obj _l)
  {
    if (auto str = std::get_if<std::string>(&_l))
      return *str;
    else if (auto num = std::get_if<double>(&_l))
      return std::to_string(*num);
    else if (auto b = std::get_if<bool>(&_l))
      return *b ? "true" : "false";
    else if (_l.valueless_by_exception() || _l.index() == 0)
      return "nil";
    else
      return "invalid literal";
  };
}; //namespace utls
