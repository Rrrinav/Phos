#pragma once

#include <string_view>
#include <print>

#include "../lexer/token.hpp"

namespace err
{
  inline bool HAD_ERROR         = false;
  inline bool HAD_RUNTIME_ERROR = false;

  enum Exit_code
  {
    _EXIT_CODE_SUCCESS_       = 0,
    _EXIT_CODE_ERROR_         = 65,
    _EXIT_CODE_RUNTIME_ERROR_ = 70
  };

  void report(int line, std::string_view where, std::string_view message)
  {
    std::println(stderr, "line {}:{} -> ERROR: {}", line, where, message);
    HAD_ERROR = true;
  }

  void report(int line, std::string_view message) { report(line, "", message); }

  void report(lex::Token token, std::string_view message)
  {
    if (token.type == lex::Token_type::END_OF_FILE)
      report(token.line, " at end ", message);
    else
      report(token.line, std::format(" at '{}'", token.lexeme), message);
  }

  void report_runtime_error(int line, std::string_view message)
  {
    std::println(stderr, "line {} -> RUNTIME ERROR: {}", line, message);
    HAD_RUNTIME_ERROR = true;
  }

  [[noreturn]]
  void quit(Exit_code code) { std::exit(code); };
}  // namespace err
