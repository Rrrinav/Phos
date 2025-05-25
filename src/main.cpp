#include <cstdlib>
#include <print>
#include <fstream>
#include <string_view>

#include "./misc/error_reporting.hpp"
#include "./lexer/scanner.hpp"
#include "lexer/token_type.hpp"

#define _todo_ \
  do { \
    std::println(stderr, "error: {}:{} todo",__FILE_NAME__, __LINE__);\
    abort(); \
  } while(false)

void run(std::string code)
{
  lex::Scanner scanner(code);
  auto tokens = scanner.scan_tokens();
  for (const auto &token : tokens)
  {
    std::string literal_str;

    if (std::holds_alternative<double>(token.literal))
      literal_str = std::format("{}", std::get<double>(token.literal));
    else if (std::holds_alternative<std::string>(token.literal))
      literal_str = std::format("\"{}\"", std::get<std::string>(token.literal));
    else
      literal_str = "null";

    std::println("{:<15} | lexeme: {:<10} | literal: {}", lex::token_type_to_string(token.type),
                 token.lexeme.empty() ? "\"\"" : token.lexeme, literal_str);
  }
  if (HAD_ERROR)
    exit(EXIT_FAILURE);
}

void run_file(std::string_view path)
{
  std::ifstream file(path.data());
  if (!file.is_open())
  {
    std::println("Could not open file: {}", path);
    return;
  }
  int size = 0;
  file.seekg(0, std::ios::end);
  size = file.tellg();
  if (size == 0)
  {
    std::println("File is empty: {}", path);
    return;
  }
  file.seekg(0, std::ios::beg);

  std::string bytes(size, ' ');
  file.read(&bytes[0], size);
  run(bytes);
}

void run_prompt()
{
  throw std::runtime_error("Prompt mode is not implemented yet.");
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
