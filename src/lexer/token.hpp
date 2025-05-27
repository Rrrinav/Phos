#pragma once

#include <string>
#include <variant>
#include <span>
#include <format>
#include <print>

#include "./token_type.hpp"

namespace lex
{
  using Literal_obj = std::variant<std::monostate, double, std::string, bool>;
  
  std::string literalobj_type_to_string(std::size_t index)
  {
    if (index == 0) return "NULL";
    if (index == 1) return "NUMBER";
    if (index == 2) return "STRING";
    if (index == 3) return "BOOLEAN";
    return "UNKNOWN";
  }

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

    static void print_tokens(std::span<Token> tokens_)
    {
      for (const auto &token : tokens_)
      {
        std::string literal_str;

        if (std::holds_alternative<double>(token.literal))
          literal_str = std::format("{}", std::get<double>(token.literal));
        else if (std::holds_alternative<std::string>(token.literal))
          literal_str = std::format("\"{}\"", std::get<std::string>(token.literal));
        else
          literal_str = "null";

        std::println("{:<15} | lexeme: {:<10} | literal: {}", lex::token_type_to_string(token.type),
                     token.lexeme.empty() ? "\"\"" : token.lexeme, literal_str);
      }
    }
  };
}
