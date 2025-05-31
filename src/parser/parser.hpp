#pragma once

#include "../lexer/token.hpp"
#include "./expr.hpp"
#include "./statement.hpp"
#include "../misc/error_reporting.hpp"

#include <algorithm>
#include <cstdlib>
#include <initializer_list>
#include <optional>
#include <vector>
#include <memory>

namespace pars
{
  class Parser
  {
    std::vector<lex::Token> tokens;
    std::size_t current = 0;

  public:
    Parser(std::vector<lex::Token> tokens_) : tokens(tokens_) {}

    std::vector<pars::Statement> parse()
    {
      std::vector<pars::Statement> statements;

      while (!is_at_end())
      {
        if (auto dec_val = delcaration_statement(); dec_val)
        {
          statements.push_back(std::move(*dec_val));
        }
      };
      return statements;
    }

  private:

    inline std::optional<pars::Statement> delcaration_statement()
    {
      try
      {
        if (match({lex::Token_type::VAR}))
          return variable_declaration();
        return statement();
      }
      catch (const std::exception &e)
      {
        synchronize();
        return std::nullopt;
      }
    }

    inline std::optional<pars::Statement> variable_declaration()
    {
      lex::Token name = consume(lex::Token_type::IDENTIFIER, "Expected variable name.");

      std::optional<Expr> initializer;
      if (match({lex::Token_type::EQUAL}))
      {
        initializer = expression();
      }

      consume(lex::Token_type::SEMICOLON, "Expect ';' after variable declaration.");
      return Variable_decl_stmt{name, std::move(initializer)};
    }

    inline pars::Statement statement()
    {
      if (match({lex::Token_type::PRINT})) return print_statement();
      if (match({lex::Token_type::LEFT_BRACE})) return block_statement();
      if (match({lex::Token_type::IF})) return if_statement();
      if (match({lex::Token_type::WHILE})) return while_statement();
      if (match({lex::Token_type::FOR})) return for_statement();
      else return expression_statement();
    }

    inline pars::Statement print_statement()
    {
      Expr value = expression();
      consume(lex::Token_type::SEMICOLON, "Expect ';' after value.");
      return pars::Statement{Print_stmt{std::make_unique<Expr>(std::move(value))}};
    };

    inline pars::Statement block_statement()
    {
      std::vector<pars::Statement> statements;

      while (!check(lex::Token_type::RIGHT_BRACE) && !is_at_end())
      {
        if (auto decl = delcaration_statement(); decl.has_value())
        {
          statements.push_back(std::move(*decl));
        }
      }

      consume(lex::Token_type::RIGHT_BRACE, "Expect '}' after block.");
      return pars::Statement{pars::Block_stmt{std::move(statements)}};
    }

    inline pars::Statement if_statement()
    {
      consume(lex::Token_type::LEFT_PAREN, "Expect '(' after 'if'.");
      Expr condition = expression();
      consume(lex::Token_type::RIGHT_PAREN, "Expect ')' after if condition.");

      pars::Statement then_branch = statement();
      std::optional<pars::Stmt_ptr> else_branch;

      if (match({lex::Token_type::ELSE}))
      {
        else_branch = std::make_unique<pars::Statement>(statement());
      }
      else
      {
        else_branch = std::nullopt;
      }

      return pars::Statement{pars::If_stmt{
        .condition    = std::make_unique<pars::Expr>(std::move(condition)),
        .then_branch  = std::make_unique<pars::Statement>(std::move(then_branch)),
        .else_branch  = std::move(else_branch),
      }};
    }

    inline pars::Statement while_statement()
    {
      consume(lex::Token_type::LEFT_PAREN, "Expect '(' after 'while'.");
      Expr condition = expression();
      consume(lex::Token_type::RIGHT_PAREN, "Expect ')' after condition.");

      pars::Statement body = statement();

      return pars::Statement{pars::While_stmt{
        .condition = std::make_unique<pars::Expr>(std::move(condition)),
        .body      = std::make_unique<pars::Statement>(std::move(body)),
      }};
    }

    inline pars::Statement for_statement()
    {
      consume(lex::Token_type::LEFT_PAREN, "Expect '(' after 'while'.");

      // Capture the initializer if it exists
      std::optional<pars::Statement> initializer;
      if (match({lex::Token_type::SEMICOLON}))
        initializer = std::nullopt;
      else if (match({lex::Token_type::VAR}))
        initializer = variable_declaration();
      else
        initializer = expression_statement();

      // Capture the condition if it exists
      std::optional<Expr> condition;
      if (!check(lex::Token_type::SEMICOLON))
        condition = expression();
      else
        condition = Literal_expr{lex::Literal_obj{true}};  // Default to true

      consume(lex::Token_type::SEMICOLON, "Expect ';' after loop condition.");

      // Capture the operation if it exists
      std::optional<Expr> operation;
      if (!check(lex::Token_type::RIGHT_PAREN))
        operation = expression();
      else
        operation = Literal_expr{lex::Literal_obj{}};  // Default to no operation

      consume(lex::Token_type::RIGHT_PAREN, "Expect ')' after for loop clauses.");

      auto body = statement();

      if (operation.has_value())
      {
        // Wrap the body in a block if it isn't already
        if (std::holds_alternative<pars::Block_stmt>(body.node))
        {
          auto& block = std::get<pars::Block_stmt>(body.node);
          block.statements.push_back(Expression_stmt{std::make_unique<Expr>(std::move(*operation))});
        }
        else
        {
          std::vector<pars::Statement> block_statements;
          block_statements.push_back(std::move(body));
          block_statements.push_back(Expression_stmt{std::make_unique<Expr>(std::move(*operation))});
          body = pars::Statement{pars::Block_stmt{std::move(block_statements)}};
        }
      }

      if (!condition.has_value())
        condition = Literal_expr{lex::Literal_obj{true}};

      body = pars::Statement{pars::While_stmt{
        .condition = std::make_unique<pars::Expr>(std::move(*condition)),
        .body      = std::make_unique<pars::Statement>(std::move(body))
      }};

      if (initializer.has_value())
      {
        std::vector<pars::Statement> block_statements;
        block_statements.push_back(std::move(*initializer));
        block_statements.push_back(std::move(body));
        body = pars::Statement{pars::Block_stmt{
          .statements = std::move(block_statements)
        }};
      }

      return body;
    }

    inline pars::Statement expression_statement()
    {
      Expr value = expression();
      consume(lex::Token_type::SEMICOLON, "Expect ';' after expression.");
      return pars::Statement{Expression_stmt{std::make_unique<Expr>(std::move(value))}};
    };

    Expr expression() { return assignment(); }

    Expr assignment()
    {
      Expr expr = or_expression();

      if (match({lex::Token_type::EQUAL}))
      {
        lex::Token equals = previous();
        Expr value = assignment();

        if (std::holds_alternative<Variable_expr>(expr.node))
        {
          lex::Token name = std::get<Variable_expr>(expr.node).name;
          return Expr{Assign_expr{name, std::make_unique<Expr>(std::move(value))}};
        }
        err::report(equals, "Invalid assignment target. Only variables can be assigned to.");
      }
      return expr;
    }

    inline Expr or_expression()
    {
      Expr expr = and_expression();

      while (match({lex::Token_type::OR}))
      {
        const auto op = previous();
        auto right = std::make_unique<Expr>(and_expression());
        expr = Expr{Logical_expr{std::make_unique<Expr>(std::move(expr)), op, std::move(right)}};
      }

      return expr;
    }

    inline Expr and_expression()
    {
      Expr expr = equality();

      while (match({lex::Token_type::AND}))
      {
        const auto op = previous();
        auto right = std::make_unique<Expr>(equality());
        expr = Expr{Logical_expr{std::make_unique<Expr>(std::move(expr)), op, std::move(right)}};
      }

      return expr;
    }

    inline Expr equality()
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

    inline Expr comparison()
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

    inline Expr term()
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

    inline Expr factor()
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

    inline Expr unary()
    {
      if (match({lex::Token_type::BANG, lex::Token_type::MINUS}))
      {
        const auto op = previous();
        auto right = std::make_unique<Expr>(unary());
        return Expr{Unary_expr{op, std::move(right)}};
      }

      return primary();
    }

    inline Expr primary()
    {
      if (match({lex::Token_type::FALSE}))
        return Expr{Literal_expr{lex::Literal_obj{false}}};

      if (match({lex::Token_type::TRUE}))
        return Expr{Literal_expr{lex::Literal_obj{true}}};

      if (match({lex::Token_type::NIL}))
        return Expr{Literal_expr{lex::Literal_obj{}}};

      if (match({lex::Token_type::NUMBER, lex::Token_type::STRING}))
        return Expr{Literal_expr{previous().literal}};

      if (match({lex::Token_type::IDENTIFIER}))
        return Expr{Variable_expr{previous()}};

      if (match({lex::Token_type::LEFT_PAREN}))
      {
        Expr expr = expression();
        consume(lex::Token_type::RIGHT_PAREN, "Expect ')' after expression.");
        return Expr{Grouping_expr{std::make_unique<Expr>(std::move(expr))}};
      }

      return Expr{Literal_expr{}};  // Placeholder fallback
    }

    inline bool match(std::initializer_list<lex::Token_type> types)
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

    inline const lex::Token& consume(lex::Token_type type, const std::string& message)
    {
      if (check(type))
        return advance();

      err::report(peek(), message);
      exit(EXIT_FAILURE);
    }

    inline bool check(lex::Token_type type) const
    {
      return !is_at_end() && peek().type == type;
    }

    inline const lex::Token& advance()
    {
      if (!is_at_end()) ++current;
      return previous();
    }

    inline bool is_at_end() const
    {
      return peek().type == lex::Token_type::END_OF_FILE;
    }

    inline const lex::Token& peek() const { return tokens[current]; }

    inline const lex::Token& previous() const { return tokens[current - 1]; }

    inline void synchronize()
    {
      advance();

      while (!is_at_end())
      {
        if (previous().type == lex::Token_type::SEMICOLON) return;
        switch (peek().type)
        {
          case lex::Token_type::CLASS:
          case lex::Token_type::FUN:
          case lex::Token_type::VAR:
          case lex::Token_type::FOR:
          case lex::Token_type::IF:
          case lex::Token_type::WHILE:
          case lex::Token_type::PRINT:
          case lex::Token_type::RETURN:
            return;
          default:
            break;
        }
        advance();
      };
    }
  };
}  // namespace pars
