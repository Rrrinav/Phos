#include "../src/parser/expr.hpp"

#include <iostream>
#include <memory>

int main()
{
  using namespace pars;
  using namespace lex;
  using lex::Token;
  using lex::Token_type;

  // Expression: (1 + 2) * -3
  auto expr_ast =
  Expr
  {
    Binary_expr
    {
      std::make_unique<Expr>(
      Grouping_expr
      {
        std::make_unique<Expr>(
        Binary_expr
        {
          std::make_unique<Expr>( Literal_expr { 1.0 }),
          Token{Token_type::PLUS, "+", std::monostate{}, 1},
          std::make_unique<Expr> ( Literal_expr { 2.0 })
        })
      }),

      Token{Token_type::STAR, "*", std::monostate{}, 1},

      std::make_unique<Expr>(Unary_expr
      {
        Token{Token_type::MINUS, "-", std::monostate{}, 1},
        std::make_unique<Expr>(Literal_expr{3.0})
      })
    }
  };

  Ast_printer printer;
  std::cout << printer.print(expr_ast) << "\n";

  return 0;
}

