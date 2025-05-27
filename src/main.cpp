#include <cstdlib>
#include <iostream>
#include <print>
#include <string_view>

#include "./lexer/scanner.hpp"
#include "./parser/parser.hpp"
#include "./misc/file_utls.hpp"
#include "./interpreter/evaluator.hpp"
#include "misc/error_reporting.hpp"
#include "parser/expr.hpp"

#define $__todo__primary__(x) \
  do { \
    std::println(stderr, "TODO: {}:{} {}",__FILE_NAME__, __LINE__, x);\
    abort(); \
  } while(false)

#define $todo_   $__todo__primary__("")
#define $todo(x) $__todo__primary__(x)

void run(std::string code)
{
  // A little trolling my friend
  std::string output = pars::stringify_literal(interp::Evaluator().evaluate(pars::Parser(lex::Lexer(code).scan_tokens()).parse()));
  std::println("{}", output);

  {
    using err::Exit_code;

    if (err::HAD_ERROR)
      exit(Exit_code::_EXIT_CODE_ERROR_);
    if (err::HAD_RUNTIME_ERROR)
      exit(Exit_code::_EXIT_CODE_RUNTIME_ERROR_);
  }
}

void run_file(std::string_view path)
{
  auto bytes = utl::read_entire_file(path);
  if (bytes)
  run(bytes.value());
  else
  {
    std::println(stderr, "error: could not read file '{}'", path);
    exit(EXIT_FAILURE);
  }
}

void run_prompt()
{
  std::println("Welcome to the phos REPL!");
  std::println("Type '.exit' to quit.\n");
  std::string line;

  while (true)
  {
    std::print("> ");
    if (!std::getline(std::cin, line)) break;
    if (line == ".exit") break;
    run(line);
  }

  std::println("\nGoodbye.");
}

int main(int argc, char *argv[])
{
  if (argc > 2)
  {
    std::println("Usage: lang <file>");
    return 1;
  }
  else if (argc == 1)
  {
    run_prompt();
  }
  else if (argc == 2)
  {
    run_file(argv[1]);
  }
  return 0;
}
