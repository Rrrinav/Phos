#pragma once

#include <string>
#include <variant>

#include "./token_type.hpp"

namespace lex
{
  struct Token
  {
    lex::Token_type type;
    std::string lexeme;
    std::variant<std::monostate, double, std::string> literal;
    int line;

    Token(lex::Token_type type, std::string lexeme, std::variant<std::monostate, double, std::string> literal, int line)
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
