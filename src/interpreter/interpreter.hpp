#pragma once

#include <vector>
#include <variant>

#include "./evaluator.hpp"

#include "../parser/statement.hpp"
#include "../misc/utils.hpp"
#include "../runtime/environment.hpp"

namespace interp
{
  struct Interpreter
  {
    void interpret(const std::vector<pars::Statement> &statements)
    {
      for (const auto &stmt : statements) execute(stmt);
    }

    Interpreter() : environment_(runtime::Environment{}), evaluator_(this->environment_)
    {
      evaluator_.environment_ = environment_;
    }

  private:
    runtime::Environment environment_;
    interp::Evaluator evaluator_;

    void execute(const pars::Statement &stmt)
    {
      std::visit([this](const auto &s) { execute_node(s); }, stmt.node);
    }

    void execute_node(const pars::Expression_stmt &stmt)
    {
      evaluator_.evaluate(*stmt.expression);
    }

    void execute_node(const pars::Print_stmt &stmt)
    {
      auto value = evaluator_.evaluate(*stmt.expression);
      std::println("{}", utl::stringify_literal(value));
    }

    void execute_node(const pars::Variable_decl_stmt &stmt)
    {
      lex::Literal_obj value;
      if (stmt.initializer.has_value())
        value = evaluator_.evaluate(*stmt.initializer);

      environment_.define(stmt.name.lexeme, value);
    }
  };
}; // namespace interp
