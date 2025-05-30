#pragma once

#include <span>
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
    interp::Evaluator    evaluator_;

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

    void execute_node(const pars::Block_stmt &stmt)
    {
      runtime::Environment block_env = runtime::Environment(this->environment_);
      this->execute_block(stmt.statements, block_env);
    }

    void execute_block(std::span<const pars::Statement> statements, runtime::Environment &environment)
    {
      runtime::Environment main_env = environment_;
      environment_ = environment;

      for (const auto &stmt : statements) execute(stmt);

      environment_ = main_env;
    }
  };
}; // namespace interp
