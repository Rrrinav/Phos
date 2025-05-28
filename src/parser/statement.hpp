#pragma once

#include <variant>
#include <memory>
#include <optional>

#include "./expr.hpp"

namespace pars
{
  struct Statement;

  using Stmt_ptr = std::unique_ptr<Statement>;

  struct Expression_stmt
  {
    Expr_ptr expression;
  };

  struct Print_stmt
  {
    Expr_ptr expression;
  };

  struct Variable_decl_stmt
  {
    lex::Token name;
    std::optional<Expr> initializer;
  };

  struct Statement
  {
    std::variant<Expression_stmt, Print_stmt, Variable_decl_stmt> node;

    template <typename T>
    requires std::constructible_from<std::variant<Expression_stmt, Print_stmt, Variable_decl_stmt>, T>
    Statement(T val) : node(std::move(val)) {}
  };
}

