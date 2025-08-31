#pragma once

#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <print>
#include <vector>
#include <memory>
#include <variant>

// Your existing includes
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "type-checker/typechecker.hpp"
#include "interpreter/interpreter.hpp"

// Utility function to convert values to strings for display
std::string value_to_string(const phos::Value &value)
{
    return std::visit(
        [](const auto &v) -> std::string {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, int64_t>)
                return std::to_string(v);
            else if constexpr (std::is_same_v<T, double>)
                return std::to_string(v);
            else if constexpr (std::is_same_v<T, bool>)
                return v ? "true" : "false";
            else if constexpr (std::is_same_v<T, std::string>)
                return "\"" + v + "\"";
            else if constexpr (std::is_same_v<T, std::monostate>)
                return "void";
            else
                return "unknown";
        },
    value);
}

class Phos_repl
{
private:
    phos::types::TypeChecker type_checker;
    phos::Interpreter interpreter;
    std::vector<std::unique_ptr<phos::ast::Stmt>> global_statements;

    bool is_complete_input(const std::string &input)
    {
        // Simple check for balanced braces and proper statement termination
        int brace_count = 0;
        bool in_string = false;
        bool escaped = false;

        for (char c : input)
        {
            if (escaped)
            {
                escaped = false;
                continue;
            }

            if (c == '\\')
            {
                escaped = true;
                continue;
            }

            if (c == '"')
            {
                in_string = !in_string;
                continue;
            }

            if (!in_string)
            {
                if (c == '{')
                    brace_count++;
                else if (c == '}')
                    brace_count--;
            }
        }

        // Input is complete if braces are balanced and ends with semicolon or }
        return brace_count == 0 && (!input.empty() && (input.back() == ';' || input.back() == '}'));
    }

    void print_welcome()
    {
        std::println("Phos Language REPL v1.0");
        std::println("Type 'exit' or 'quit' to exit, 'help' for commands");
        std::println("Use Ctrl+C to cancel multi-line input");
        std::println("");
    }

    void print_help()
    {
        std::println("Available commands:");
        std::println("  help          - Show this help message");
        std::println("  exit, quit    - Exit the REPL");
        std::println("  clear         - Clear all defined variables and functions");
        std::println("  vars          - Show all defined variables");
        std::println("  funcs         - Show all defined functions");
        std::println("  :load <file>  - Load and execute a .phos file");
        std::println("");
        std::println("Multi-line input: If your statement is incomplete, press Enter");
        std::println("to continue on the next line. End with ';' or '{}'", "}");
    }

    void show_variables()
    {
        // TODO: Implement when interpreter exposes variable state
        std::println("Variables: (not implemented)");
    }

    void show_functions()
    {
        // TODO: Implement when interpreter exposes function state
        std::println("Functions: (not implemented)");
    }

    bool load_file(const std::string &filename)
    {
        std::ifstream file(filename);
        if (!file.is_open())
        {
            std::println("Error: Couldn't open file: {}", filename);
            return false;
        }

        std::stringstream buffer;
        buffer << file.rdbuf();
        std::string source = buffer.str();
        file.close();

        return execute_source(source, filename);
    }

    bool execute_source(const std::string &source, const std::string &filename = "<repl>")
    {
        try
        {
            phos::lex::Lexer lexer(source);
            auto tokens = lexer.tokenize();

            // Check for lexer errors
            for (const auto &token : tokens)
            {
                if (token.type == phos::lex::TokenType::Invalid)
                {
                    std::println("{}:{}:{}: error: Invalid token: {}", filename, token.line, token.column, token.lexeme);
                    return false;
                }
            }

            phos::Parser parser(tokens);
            auto parse_result = parser.parse();

            if (!parse_result)
            {
                std::println("{}:{}", filename, parse_result.error().format());
                return false;
            }

            auto new_statements = std::move(*parse_result);

            // Type check new statements in the context of existing ones
            auto type_result = type_checker.check(new_statements);
            if (!type_result)
            {
                std::println("{}:{}", filename, type_result.error().format());
                return false;
            }

            // Execute the new statements
            auto interpret_result = interpreter.interpret(new_statements);
            if (!interpret_result)
            {
                std::println("{}:{}", filename, interpret_result.error().format());
                return false;
            }

            // Add new statements to global context
            for (auto &stmt : new_statements) global_statements.push_back(std::move(stmt));

            return true;
        }
        catch (const std::exception &e)
        {
            std::println("Unexpected error: {}", e.what());
            return false;
        }
    }

public:
    void run()
    {
        print_welcome();

        std::string input;
        std::string line;

        while (true)
        {
            // Show appropriate prompt
            if (input.empty())
                std::print(" phos > ");
            else
                std::print("  ... ");

            if (!std::getline(std::cin, line))
            {
                // EOF (Ctrl+D)
                std::println("\nGoodbye!");
                break;
            }

            // Handle special commands
            if (input.empty())
            {
                if (line == "exit" || line == "quit")
                {
                    std::println("Goodbye!");
                    break;
                }
                else if (line == "help")
                {
                    print_help();
                    continue;
                }
                else if (line == "clear")
                {
                    global_statements.clear();
                    type_checker = phos::types::TypeChecker();  // Reset type checker
                    interpreter = phos::Interpreter();          // Reset interpreter
                    std::println("Cleared all definitions.");
                    continue;
                }
                else if (line == "vars")
                {
                    show_variables();
                    continue;
                }
                else if (line == "funcs")
                {
                    show_functions();
                    continue;
                }
                else if (line.starts_with(":load "))
                {
                    std::string filename = line.substr(6);
                    load_file(filename);
                    continue;
                }
            }

            // Accumulate input
            if (!input.empty())
                input += "\n";
            input += line;

            // Check if input is complete
            if (is_complete_input(input))
            {
                execute_source(input);
                input.clear();  // Reset for next input
            }
        }
    }
};
