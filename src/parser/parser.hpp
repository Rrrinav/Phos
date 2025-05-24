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

  struct Expr
  {
    std::variant<Binary_expr, Grouping_expr, Literal_expr, Unary_expr> node;

    template <typename T>
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
      return "???";
    }

    std::string print_node(const Unary_expr &u) const { return parenthesize(u.op.lexeme, *u.right); }

    std::string parenthesize(const std::string &name, const Expr &a) const { return "(" + name + " " + print(a) + ")"; }

    std::string parenthesize(const std::string &name, const Expr &a, const Expr &b) const
    {
      return "(" + name + " " + print(a) + " " + print(b) + ")";
    }
  };
}  // namespace pars
