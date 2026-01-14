#include <cstdlib>
#include <print>
#include <fstream>
#include <sstream>
#include <iostream>
#include <string>
#include <vector>

#include "interpreter/interpreter.hpp"
#include "type-checker/type-checker.hpp"
#include "parser/ast-printer.hpp"
#include "parser/parser.hpp"
#include "lexer/lexer.hpp"
#include "lexer/token.hpp"
#include "memory/arena.hpp"

#define CL_IMPLEMENTATION
#include "cli-args.hpp"

//#include "repl.hpp"

int main(int argc, char *argv[])
{
    if (argc == 1)
    {
        std::println(stderr, "REPL currently not properly working");
        return 1;
        // TODO: start REPL
        //Phos_repl repl;
        //repl.run();
        //return 0;
    }


    cl::Parser p("Phos", "Phos interpreted programming langauage");
    phos::cli::add_arguemnts(p);
    auto parse_res = p.parse(argc, argv);
    if (!parse_res)
    {
        std::println("{}", parse_res.error());
        return EXIT_FAILURE;
    }

    std::string filename;
    bool print_ast = parse_res->is_subcmd_chosen(phos::cli::id::ast_print);
    bool print_use_unicode = parse_res->get<cl::Flag>(phos::cli::id::ast_print_unicode).value_or(false);
    bool print_only_print = false;
    filename = parse_res->get<cl::Text>(phos::cli::id::file).value_or("");

    if (filename.empty())
    {
        p.print_help();
        return 1;
    }

    std::ifstream file(filename);
    if (!file.is_open())
    {
        std::println(stderr, "Error: Couldn't open file: {}", filename);
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    phos::mem::Arena arena;
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

        phos::Parser parser(tokens, arena);
        auto parse_result = parser.parse();
        if (!parse_result)
        {
            std::println(stderr, "{}:{}", filename, parse_result.error().format());
            return 1;
        }

        if (print_ast)
        {
            std::println("============Before typechecking=============");
            phos::ast::AstPrinter printer;
            printer.use_unicode = print_use_unicode;
            printer.print_statements(parse_result.value());

            if (print_only_print)
                return 0;
        }

        auto checked = phos::Type_checker().check(*parse_result);
        if (checked.size() > 0)
        {
            for (auto e : checked) std::println(stderr, "{}:{}", filename, e.format());
        }

        if (print_ast)
        {
            std::println("============After typechecking=============");
            phos::ast::AstPrinter printer;
            printer.use_unicode = print_use_unicode;
            printer.print_statements(parse_result.value());

            if (print_only_print)
                return 0;
        }
        auto statements = std::move(*parse_result);
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
