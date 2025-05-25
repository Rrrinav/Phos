#pragma once

#include <string>
#include <variant>

#include "./token_type.hpp"

namespace lex
{
  using Literal_obj = std::variant<std::monostate, double, std::string, bool>;

  struct Token
  {
    lex::Token_type type;
    std::string lexeme;
    Literal_obj literal;
    std::size_t line;

    Token(lex::Token_type type, std::string lexeme, Literal_obj literal, std::size_t line)
        : type(type), lexeme(std::move(lexeme)), literal(std::move(literal)), line(line)
    {}

    std::string to_string() const
    {
      std::string lit_str;
      if (std::holds_alternative<double>(literal))
        lit_str = std::to_string(std::get<double>(literal));
      else if (std::holds_alternative<std::string>(literal))
        lit_str = std::get<std::string>(literal);
      else
        lit_str = "null";

      return token_type_to_string(type) + " " + lexeme + " " + lit_str;
    }
  };
}
