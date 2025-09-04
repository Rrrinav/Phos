#include <print>
#include <fstream>
#include <sstream>
#include <iostream>

#include "interpreter/interpreter.hpp"
#include "parser/ast-printer.hpp"
#include "type-checker/typechecker.hpp"
#include "parser/parser.hpp"
#include "lexer/lexer.hpp"
#include "lexer/token.hpp"
#include "repl.hpp"

int main(int argc, char *argv[])
{
    // TODO: Proper arguments parsing
    if (argc == 1)
    {
        Phos_repl repl;
        repl.run();
        return 0;
    }

    if (argc < 2)
    {
        std::println(stderr, "Usage: {} <filename.phos>", argv[0]);
        return 1;
    }
    bool print_ast = false;
    bool print_use_unicode = true;
    bool print_only_print = false;
    if (argc == 4)
    {
        std::string opt = argv[2];
        if (opt == "--ast-dump")
            print_ast = true;
    }
    std::string filename = argv[1];
    std::ifstream file(filename);
    if (!file.is_open())
    {
        std::println(stderr, "Error: Couldn't open the file: {}.", filename);
        return 1;
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();
    try
    {
        phos::lex::Lexer lexer(source);
        auto tokens = lexer.tokenize();
        for (const auto &token : tokens)
        {
            if (token.type == phos::lex::TokenType::Invalid)
            {
                std::println(stderr, "{}:{}:{}: error: Invalid token: {}", filename, token.line, token.column, token.lexeme);
                return 1;
            }
        }
        phos::Parser parser(tokens);

        auto parse_result = parser.parse();
        if (!parse_result)
        {
            std::println(stderr, "{}:{}", filename, parse_result.error().format());
            return 1;
        }
        if (print_ast)
        {
            phos::ast::AstPrinter printer;
            if (print_use_unicode)
                printer.print_statements(parse_result.value());
            else
            {
                printer.use_unicode = false;
                printer.print_statements(parse_result.value());
            }
            if (print_only_print)
                return 0;
        }
        auto statements = std::move(*parse_result);
        phos::types::Type_checker type_checker;
        type_checker.check(statements);
        if (!type_checker.has_errors())
        {
            auto errors = type_checker.get_errors();
            for (const auto &e : errors) std::println(stderr, "{}:{}", filename, e.format());
        }
        phos::Interpreter interpreter;
        auto interpret_result = interpreter.interpret(statements);
        if (!interpret_result)
        {
            std::println(stderr, "{}:{}", filename, interpret_result.error().format());
            return 1;
        }
    }
    catch (const std::exception &e)
    {
        std::cerr << "Unexpected error: " << e.what() << std::endl;
        return 1;
    }
    return 0;
}
