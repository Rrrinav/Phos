#include <print>
#include <stdexcept>
#include <utility>
#include <variant>

#include "../parser/expr.hpp"
#include "../lexer/token.hpp"

#include "../misc/utils.hpp"

namespace interp
{
  struct Evaluator
  {
    lex::Literal_obj evaluate(const pars::Expr& expression)
    {
      return std::visit(
        [this](const auto& node) -> lex::Literal_obj {
          return evaluate_node(node);
        },
        expression.node
      );
      std::println("./src/interpreter/evaluator.hpp:{} : Unreachable code reached.\n      Somehow couldn't evaluate an expressoin.", __LINE__);
      std::unreachable();
    };

  private:

    lex::Literal_obj evaluate_node(const pars::Literal_expr& _l)
    { return _l.value; }
 
    lex::Literal_obj evaluate_node(const pars::Grouping_expr& _g)
    { return this->evaluate(*_g.expression); }

    lex::Literal_obj evaluate_node(const pars::Unary_expr& _u)
    {
      auto right = this->evaluate(*_u.right);
      switch (_u.op.type)
      {
        case lex::Token_type::MINUS:
        {
          auto num = std::get_if<double>(&right);
          return -(num ? *num : 0.0);
          throw std::runtime_error("Unary minus requires a number");
        }
        case lex::Token_type::BANG:
        {
          auto b = std::get_if<bool>(&right);
          return !utl::is_truthy(*b);
          throw std::runtime_error("Unary bang requires a boolean");
        }
        default:
          return std::monostate();
      }
    }

    lex::Literal_obj evaluate_node(const pars::Binary_expr& _b)
    {
      using lex::Token_type;

      lex::Literal_obj left  = this->evaluate(*_b.left);
      lex::Literal_obj right = this->evaluate(*_b.right);

      switch (_b.op.type)
      {
        case lex::Token_type::PLUS:
        {
          if (auto l = std::get_if<double>(&left), r = std::get_if<double>(&right); l && r)
            return *l + *r;
          if (auto l = std::get_if<std::string>(&left), r = std::get_if<std::string>(&right); l && r)
            return *l + *r;
          throw std::runtime_error("Operands [ for '+' ] must be two numbers or two strings.");
        }
        case lex::Token_type::MINUS:           return utl::binary_number_op(left, right, std::minus<>{});
        case lex::Token_type::STAR:            return utl::binary_number_op(left, right, std::multiplies<>{});
        case lex::Token_type::SLASH:           return utl::binary_number_op(left, right, std::divides<>{});
        case lex::Token_type::GREATER:         return utl::binary_number_op(left, right, std::greater<>{});
        case lex::Token_type::GREATER_EQUAL:   return utl::binary_number_op(left, right, std::greater_equal<>{});
        case lex::Token_type::LESS:            return utl::binary_number_op(left, right, std::less<>{});
        case lex::Token_type::LESS_EQUAL:      return utl::binary_number_op(left, right, std::less_equal<>{}); 
        case lex::Token_type::BANG_EQUAL:      return !utl::is_equal(left, right);
        case lex::Token_type::EQUAL_EQUAL:     return utl::is_equal(left, right);
        default: throw std::runtime_error("Unknown binary operator.");
      }
    }
  };

  void interpret(const pars::Expr& expression)
  {
    try
    {
      Evaluator evaluator;
      auto result = evaluator.evaluate(expression);
    }
    catch (const std::runtime_error& e)
    {
      std::println("Runtime error: {}\n ", e.what());
    }
  }
}; // namespace interp
