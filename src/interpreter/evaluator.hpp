#pragma once

#include <expected>
#include <print>
#include <utility>
#include <variant>

#include "../parser/expr.hpp"
#include "../lexer/token.hpp"

#include "../misc/utils.hpp"
#include "../misc/error_reporting.hpp"
#include "../runtime/environment.hpp"

namespace interp
{
  struct Evaluator
  {
    runtime::Environment& environment_;

    lex::Literal_obj evaluate(const pars::Expr& expression)
    {
      return std::visit(
        [this](const auto& node) -> lex::Literal_obj {
          return evaluate_node(node);
        },
        expression.node
      );
      std::println("./src/interpreter/evaluator.hpp:{} : Unreachable code reached.\n "
                   "    Somehow couldn't evaluate an expressoin.", __LINE__);
      std::unreachable();
    };

  private:

    inline lex::Literal_obj evaluate_node(const pars::Literal_expr& _l)
    { return _l.value; }
 
    inline lex::Literal_obj evaluate_node(const pars::Grouping_expr& _g)
    { return this->evaluate(*_g.expression); }

    inline lex::Literal_obj evaluate_node(const pars::Unary_expr& _u)
    {
      auto right = this->evaluate(*_u.right);
      switch (_u.op.type)
      {
        case lex::Token_type::MINUS:
        {
          if (auto num = std::get_if<double>(&right))
            return -(*num);
          else {
            err::report_runtime_error(_u.op.line, "Unary minus requires a number");
            return std::monostate();
          }
        }
        case lex::Token_type::BANG:
        {
          if (auto b = std::get_if<bool>(&right))
            return !utl::is_truthy(*b);
          else
          {
            err::report_runtime_error(_u.op.line, "Unary bang requires a boolean");
            return std::monostate();
          }
        }
        default:
          return std::monostate();
      }
    }

    inline lex::Literal_obj evaluate_node(const pars::Binary_expr& _b)
    {
      using lex::Token_type;

      lex::Literal_obj left  = this->evaluate(*_b.left);
      lex::Literal_obj right = this->evaluate(*_b.right);

      std::expected<lex::Literal_obj, std::string> result;

      switch (_b.op.type)
      {
        case lex::Token_type::PLUS:
        {
          if (auto l = std::get_if<double>(&left), r = std::get_if<double>(&right); l && r)
            return *l + *r;
          if (auto l = std::get_if<std::string>(&left), r = std::get_if<std::string>(&right); l && r)
            return *l + *r;
          err::report_runtime_error(_b.op.line, "Operands [ for '+' ] must either be two numbers or two strings.");
        } break;
        case lex::Token_type::MINUS:           result = utl::binary_number_op(left, right, std::minus<>{});           break;
        case lex::Token_type::STAR:            result = utl::binary_number_op(left, right, std::multiplies<>{});      break;
        case lex::Token_type::SLASH:           result = utl::binary_number_op(left, right, std::divides<>{});         break;
        case lex::Token_type::GREATER:         result = utl::binary_number_op(left, right, std::greater<>{});         break;
        case lex::Token_type::GREATER_EQUAL:   result = utl::binary_number_op(left, right, std::greater_equal<>{});   break;
        case lex::Token_type::LESS:            result = utl::binary_number_op(left, right, std::less<>{});            break;
        case lex::Token_type::LESS_EQUAL:      result = utl::binary_number_op(left, right, std::less_equal<>{});      break;
        case lex::Token_type::BANG_EQUAL:      result = utl::isnt_equal(left, right);                                  break;
        case lex::Token_type::EQUAL_EQUAL:     result = utl::is_equal(left, right);                                   break;
        default:
        err::report_runtime_error(
          _b.op.line,
          std::format("Unknown binary operator : ' {} '.", lex::token_type_to_string(_b.op.type))
        );
        break;
      }
      if (result.has_value())
      {
        return result.value(); 
      }
      else
      {
        err::report_runtime_error(_b.op.line, std::format("Error evaluating binary expression: {}", result.error()));
        return std::monostate();
      }
    }

    inline lex::Literal_obj evaluate_node(const pars::Variable_expr& _v) { return environment_.get(_v.name); }

    inline lex::Literal_obj evaluate_node(const pars::Assign_expr& _a)
    {
      lex::Literal_obj value = this->evaluate(*_a.value);
      if (this->environment_.assign(_a.name, value))
      {
        return value;
      }
      else
      {
        err::report_runtime_error(_a.name.line, "Variable assignment failed. Variable not found in environment.");
        err::quit(err::Exit_code::_EXIT_CODE_RUNTIME_ERROR_);
      }
    }

    inline lex::Literal_obj evaluate_node(const pars::Logical_expr& _l)
    {
      lex::Literal_obj left = this->evaluate(*_l.left);
      if (_l.op.type == lex::Token_type::OR && utl::is_truthy(left))
        return left;

      if (_l.op.type == lex::Token_type::AND && !utl::is_truthy(left))
        return left;

      return this->evaluate(*_l.right);
    }
  };
}; // namespace interp
