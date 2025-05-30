#pragma once

#include <unordered_map>
#include <string>
#include <memory>

#include "../lexer/token.hpp"
#include "../misc/error_reporting.hpp"

namespace runtime
{
  struct Environment
  {
    std::unordered_map<std::string, lex::Literal_obj> values_;
    std::shared_ptr<Environment> enclosing_;

    Environment() = default;

    explicit Environment(std::shared_ptr<Environment> enclosing)
    : enclosing_(std::move(enclosing)) {}

    void define(const std::string& name, const lex::Literal_obj& value)
    {
      values_[name] = value;
    }

    lex::Literal_obj get(const lex::Token& name) const
    {
      if (auto it = values_.find(name.lexeme); it != values_.end())
      {
        return it->second;
      }
      if (enclosing_)
        return enclosing_->get(name);

      err::report_runtime_error(name.line, "Undefined variable '" + name.lexeme + "'.");
      err::quit(err::Exit_code::_EXIT_CODE_RUNTIME_ERROR_);
    }

    bool assign(const lex::Token& name, const lex::Literal_obj& value)
    {
      if (auto it = values_.find(name.lexeme); it != values_.end())
      {
        it->second = value;
        return true;
      }

      if (enclosing_)
        return enclosing_->assign(name, value);

      return false;
    }
  };
}
