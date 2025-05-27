#include <cstdlib>
#include <iostream>
#include <print>
#include <string_view>

#include "./lexer/scanner.hpp"
#include "./parser/parser.hpp"
#include "./misc/file_utls.hpp"
#include "./interpreter/evaluator.hpp"
#include "lexer/token.hpp"
#include "parser/expr.hpp"

#define _todo_ \
  do { \
    std::println(stderr, "error: {}:{} todo",__FILE_NAME__, __LINE__);\
    abort(); \
  } while(false)

void run(std::string code)
{
  lex::Scanner scanner(code);
  auto tokens = scanner.scan_tokens();
  pars::Expr ast = pars::Parser(tokens).parse();
  interp::Evaluator evaluator;
  lex::Literal_obj lit = evaluator.evaluate(ast);
  std::string output = pars::stringify_literal(lit);
  std::println("{}", output);

  if (err::HAD_ERROR)
    exit(EXIT_FAILURE);
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
