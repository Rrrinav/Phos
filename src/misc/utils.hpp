#pragma once

#include <format>
#include <stdexcept>
#include <variant>
#include <string>
#include "../lexer/scanner.hpp"

namespace utl
{

  template <typename Op>
  constexpr std::string_view get_operator_symbol()
  {
    if constexpr (std::is_same_v<Op, std::plus<>>) return "+";
    else if constexpr (std::is_same_v<Op, std::minus<>>) return "-";
    else if constexpr (std::is_same_v<Op, std::multiplies<>>) return "*";
    else if constexpr (std::is_same_v<Op, std::divides<>>) return "/";
    else if constexpr (std::is_same_v<Op, std::greater<>>) return ">";
    else if constexpr (std::is_same_v<Op, std::greater_equal<>>) return ">=";
    else if constexpr (std::is_same_v<Op, std::less<>>) return "<";
    else if constexpr (std::is_same_v<Op, std::less_equal<>>) return "<=";
    else return "<unknown-op>";
  }

  template <typename F>
  concept Binary_number_op = requires(F f, double a, double b) {
    { f(a, b) } -> std::convertible_to<double>;
  };

  template <Binary_number_op Op>
  lex::Literal_obj binary_number_op(const lex::Literal_obj &l, const lex::Literal_obj &r, Op op)
  {
    auto lv = std::get_if<double>(&l);
    auto rv = std::get_if<double>(&r);
    if (!lv || !rv)
    {
      std::println(stderr, "Error: Invalid operands for operation ' {} '. Expected numbers.\n", utl::get_operator_symbol<Op>());
      exit(EXIT_FAILURE);
    }

    return static_cast<double>(op(*lv, *rv));
  }

  bool is_equal(const lex::Literal_obj& x, lex::Literal_obj& y)
  {
    if (x.index() != y.index())
      return false;

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
}; //namespace utls
