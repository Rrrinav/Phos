#pragma once

#include <memory>
#include <string>
#include <variant>

#include "../lexer/token.hpp"

namespace pars
{
  struct Expr;

  using Expr_ptr = std::unique_ptr<Expr>;

  struct Binary_expr
  {
    Expr_ptr    left;
    lex::Token  op;
    Expr_ptr    right;
  };

  struct Grouping_expr
  {
    Expr_ptr expression;
  };

  struct Literal_expr
  {
    lex::Literal_obj value;
  };

  struct Unary_expr
  {
    lex::Token op;
    Expr_ptr right;
  };

  struct Variable_expr
  {
    lex::Token name;
  };

  struct Assign_expr
  {
    lex::Token name;
    Expr_ptr value;
  };

  struct Logical_expr
  {
    Expr_ptr left;
    lex::Token op;
    Expr_ptr right;
  };

  struct Expr
  {
    using Expr_variant = std::variant<
      Binary_expr, Grouping_expr,
      Literal_expr, Unary_expr,
      Variable_expr, Assign_expr,
      Logical_expr
    >;

    Expr_variant node;

    template <typename T>
    requires std::constructible_from<Expr_variant, T>
    Expr(T val) : node(std::move(val)) {}
  };

  struct Ast_printer
  {
    std::string print(const Expr &e) const
    {
      return std::visit([this](const auto &node) { return print_node(node); }, e.node);
    }

  private:
    std::string print_node(const Binary_expr &b) const { return parenthesize(b.op.lexeme, *b.left, *b.right); }

    std::string print_node(const Grouping_expr &g) const { return parenthesize("group", *g.expression); }

    std::string print_node(const Literal_expr &lit) const
    {
      if (lit.value.valueless_by_exception())
        return "nil";
      if (auto str = std::get_if<std::string>(&lit.value))
        return *str;
      if (auto num = std::get_if<double>(&lit.value))
        return std::to_string(*num);
      if (auto b = std::get_if<bool>(&lit.value))
        return *b ? "true" : "false";
      return "invalid literal";
    }

    std::string print_node(const Unary_expr &u) const { return parenthesize(u.op.lexeme, *u.right); }

    std::string parenthesize(const std::string &name, const Expr &a) const { return "(" + name + " " + print(a) + ")"; }

    std::string parenthesize(const std::string &name, const Expr &a, const Expr &b) const
    {
      return "(" + name + " " + print(a) + " " + print(b) + ")";
    }

    std::string print_node(const Variable_expr &v) const
    {
      return v.name.lexeme;
    }

    std::string print_node(const Assign_expr &a) const
    {
      return parenthesize("assign", Variable_expr{a.name}, *a.value);
    }

    std::string print_node(const Logical_expr &l) const
    {
      return parenthesize(l.op.lexeme, *l.left, *l.right);
    }
  };
}  // namespace pars
