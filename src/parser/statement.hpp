#pragma once

#include <variant>
#include <memory>
#include <optional>
#include <vector>

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

  struct Block_stmt
  {
    std::vector<Statement> statements;
  };

  struct If_stmt
  {
    Expr_ptr condition;
    Stmt_ptr then_branch;
    std::optional<Stmt_ptr> else_branch;
  };

  struct While_stmt
  {
    Expr_ptr condition;
    Stmt_ptr body;
  };

  struct Statement
  {
    using Statement_variant = std::variant<
      Expression_stmt, Print_stmt,
      Variable_decl_stmt, Block_stmt,
      If_stmt, While_stmt>;

    Statement_variant node;

    template <typename T>
    requires std::constructible_from<Statement_variant, T>
    Statement(T val) : node(std::move(val)) {}
  };

}

