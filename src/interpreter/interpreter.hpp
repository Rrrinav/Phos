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

    Interpreter() : environment_(std::make_shared<runtime::Environment>()), evaluator_(this->environment_)
    {
      evaluator_.environment_ = environment_;
    }

  private:
    std::shared_ptr<runtime::Environment> environment_;
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

      environment_->define(stmt.name.lexeme, value);
    }

    void execute_node(const pars::Block_stmt &stmt)
    {
      auto block_env = std::make_shared<runtime::Environment>(environment_);
      this->execute_block(stmt.statements, block_env);
    }

    void execute_node(const pars::If_stmt &stmt)
    {
      if (utl::is_truthy(evaluator_.evaluate(*stmt.condition)))
      {
        execute(*stmt.then_branch);
      }
      else if (stmt.else_branch.has_value())
      {
        execute(*stmt.else_branch.value());
      }
    }

    void execute_block(std::span<const pars::Statement> statements, std::shared_ptr<runtime::Environment> new_env)
    {
      auto previous_env = environment_;     // Save current env
      environment_ = new_env;               // Switch to new (but shared) env

      for (const auto &stmt : statements)
        execute(stmt);

      environment_ = previous_env;          // Restore
    }

    void execute_node(const pars::While_stmt &stmt)
    {
      while (true)
      {
        auto value = evaluator_.evaluate(*stmt.condition);
        if (!utl::is_truthy(value)) break;
        execute(*stmt.body);
      }
    }
  };
}; // namespace interp
