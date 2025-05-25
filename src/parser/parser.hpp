#pragma once

#include "../lexer/token.hpp"
#include "expr.hpp"

#include <algorithm>
#include <span>
#include <memory>

namespace pars
{
  class Parser
  {
    std::span<const lex::Token> tokens;
    std::size_t current = 0;

  public:
    Parser(std::span<const lex::Token> tokens) : tokens(tokens) {}

    Expr parse()
    {
      return expression();
    }

  private:
    Expr expression() { return equality(); }

    Expr equality()
    {
      Expr expr = comparison();

      while (match({lex::Token_type::EQUAL_EQUAL, lex::Token_type::BANG_EQUAL}))
      {
        const auto op = previous();
        auto right = std::make_unique<Expr>(comparison());
        expr = Expr{Binary_expr{std::make_unique<Expr>(std::move(expr)), op, std::move(right)}};
      }

      return expr;
    }

    Expr comparison()
    {
      Expr expr = term();

      while (match({lex::Token_type::GREATER,
                    lex::Token_type::GREATER_EQUAL,
                    lex::Token_type::LESS,
                    lex::Token_type::LESS_EQUAL}))
      {
        const auto op = previous();
        auto right = std::make_unique<Expr>(term());
        expr = Expr
        {
          Binary_expr { std::make_unique<Expr>(std::move(expr)), op, std::move(right) }
        };
      }

      return expr;
    }

    Expr term()
    {
      Expr expr = factor();

      while (match({lex::Token_type::MINUS, lex::Token_type::PLUS}))
      {
        const auto op = previous();
        auto right = std::make_unique<Expr>(factor());
        expr = Expr{Binary_expr{std::make_unique<Expr>(std::move(expr)), op, std::move(right)}};
      }

      return expr;
    }

    Expr factor()
    {
      Expr expr = unary();

      while (match({lex::Token_type::SLASH, lex::Token_type::STAR}))
      {
        const auto op = previous();
        auto right = std::make_unique<Expr>(unary());
        expr = Expr{Binary_expr{std::make_unique<Expr>(std::move(expr)), op, std::move(right)}};
      }

      return expr;
    }

    Expr unary()
    {
      if (match({lex::Token_type::BANG, lex::Token_type::MINUS}))
      {
        const auto op = previous();
        auto right = std::make_unique<Expr>(unary());
        return Expr{Unary_expr{op, std::move(right)}};
      }

      return primary();
    }

    Expr primary()
    {
      if (match({lex::Token_type::FALSE}))
        return Expr{Literal_expr{lex::Literal_obj{false}}};

      if (match({lex::Token_type::TRUE}))
        return Expr{Literal_expr{lex::Literal_obj{true}}};

      if (match({lex::Token_type::NIL}))
        return Expr{Literal_expr{lex::Literal_obj{}}};

      if (match({lex::Token_type::NUMBER, lex::Token_type::STRING}))
        return Expr{Literal_expr{previous().literal}};

      if (match({lex::Token_type::LEFT_PAREN}))
      {
        Expr expr = expression();
        consume(lex::Token_type::RIGHT_PAREN, "Expect ')' after expression.");
        return Expr{Grouping_expr{std::make_unique<Expr>(std::move(expr))}};
      }

      return Expr{Literal_expr{}};  // Placeholder fallback
    }

    bool match(std::initializer_list<lex::Token_type> types)
    {
      return std::ranges::any_of(types, [this](lex::Token_type type)
      {
        if (check(type))
        {
          advance();
          return true;
        }
        return false;
      });
    }

    const lex::Token& consume(lex::Token_type type, const std::string& message)
    {
      if (check(type))
        return advance();

      // Real error handling would go here.
      return tokens[current];  // Placeholder fallback
    }

    bool check(lex::Token_type type) const
    {
      return !is_at_end() && peek().type == type;
    }

    const lex::Token& advance()
    {
      if (!is_at_end()) ++current;
      return previous();
    }

    bool is_at_end() const
    {
      return peek().type == lex::Token_type::END_OF_FILE;
    }

    const lex::Token& peek() const { return tokens[current]; }

    const lex::Token& previous() const { return tokens[current - 1]; }
  };
}  // namespace pars

