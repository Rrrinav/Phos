#pragma once

#include <unordered_map>
#include <string>

#include "../lexer/token.hpp"
#include "../misc/error_reporting.hpp"

namespace runtime
{
  struct Environment
  {
    std::unordered_map<std::string, lex::Literal_obj> values_;

    void define(const std::string& name, const lex::Literal_obj& value)
    {
      values_[name] = value;
    }

    lex::Literal_obj get(lex::Token name) const
    {
      if (auto it = values_.find(name.lexeme); it != values_.end())
      {
        return it->second;
      }
      err::report_runtime_error(name.line, "Undefined variable '" + name.lexeme + "'.");
      err::quit(err::Exit_code::_EXIT_CODE_RUNTIME_ERROR_);
    }
  };
}
