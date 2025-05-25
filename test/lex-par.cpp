#include <iostream>
#include <string>
#include "../src/parser/parser.hpp"
#include "../src/lexer/scanner.hpp"

int main()
{
  std::string code = "true == false";
  lex::Scanner scanner(code);
  auto tokens = scanner.scan_tokens();
  pars::Expr ast = pars::Parser(tokens).parse();
  pars::Ast_printer printer;
  std::cout << printer.print(ast) << '\n';
};
