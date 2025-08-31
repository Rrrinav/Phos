#include <print>
#include <fstream>
#include <sstream>
#include <iostream>

#include "interpreter/interpreter.hpp"
#include "type-checker/typechecker.hpp"
#include "parser/parser.hpp"
#include "lexer/lexer.hpp"
#include "lexer/token.hpp"

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        std::cerr << "Usage: " << argv[0] << " <filename.phos>" << std::endl;
        return 1;
    }
    std::string filename = argv[1];
    std::ifstream file(filename);
    if (!file.is_open())
    {
        std::cerr << "Error: Could not open file '" << filename << "'" << std::endl;
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
                std::cerr << "Lexer error: " << token.lexeme << " at line " << token.line << ", column " << token.column << std::endl;
                return 1;
            }
        }
        phos::Parser parser(tokens);
        auto parse_result = parser.parse();
        //std::println("size: {}", parse_result->size());
        //for (auto &p : parse_result.value()) phos::print_stmtt(&(*p), 2);

        if (!parse_result)
        {
            std::cerr << "Parser error: " << parse_result.error().format() << std::endl;
            return 1;
        }
        auto statements = std::move(*parse_result);
        phos::types::TypeChecker type_checker;
        auto type_result = type_checker.check(statements);
        if (!type_result)
        {
            std::cerr << "Type error: " << type_result.error().format() << std::endl;
            return 1;
        }
        phos::Interpreter interpreter;
        auto interpret_result = interpreter.interpret(statements);
        if (!interpret_result)
        {
            std::cerr << "Runtime error: " << interpret_result.error().format() << std::endl;
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
