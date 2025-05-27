#include <iostream>
#include <optional>
#include <string>
#include "../src/parser/parser.hpp"
#include "../src/lexer/scanner.hpp"
#include "../src/misc/file_utls.hpp"
#include "../src/interpreter/evaluator.hpp"

int main()
{
  std::optional<std::string> code = utl::read_entire_file("./test/lex-par.phos");
  if (!code)
  {
    std::cerr << "Error: Could not read file 'lex-par.phos'\n";
    return EXIT_FAILURE;
  }

  lex::Scanner scanner(*code);
  auto tokens = scanner.scan_tokens();
  lex::Token::print_tokens(tokens);
  pars::Expr ast = pars::Parser(tokens).parse();
  pars::Ast_printer printer;
  std::cout << printer.print(ast) << '\n';
  interp::interpret(ast);
};
