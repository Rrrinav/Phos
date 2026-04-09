#include "lexer/lexer.hpp"
#include "lexer/token.hpp"
#include "memory/arena.hpp"
#include "parser/ast-printer.hpp"
#include "parser/parser.hpp"
#include "type-checker/type-checker.hpp"

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <print>
#include <sstream>
#include <string>
#include <vector>

// --- VM Integrations ---
#include "vm/assembler.hpp"
#include "vm/compiler.hpp"
#include "vm/core_library.hpp"
#include "vm/virtual_machine.hpp"

#define CL_IMPLEMENTATION
#include "cli-args.hpp"

int main(int argc, char *argv[])
{
    if (argc == 1) {
        std::println(stderr, "REPL currently not properly working");
        return 1;
        // TODO: start REPL
        // Phos_repl repl;
        // repl.run();
        // return 0;
    }

    cl::Parser p("Phos", "Phos interpreted programming language");
    phos::cli::add_arguemnts(p);
    auto parse_res = p.parse(argc, argv);
    if (!parse_res) {
        std::println("Args error: {}", parse_res.error());
        return EXIT_FAILURE;
    }

    std::string filename = parse_res->get<cl::Text>(phos::cli::id::file).value_or("");
    if (filename.empty()) {
        p.print_help();
        return 1;
    }

    bool print_ast = parse_res->is_subcmd_chosen(phos::cli::id::ast_print);
    bool print_use_unicode = parse_res->get<cl::Flag>(phos::cli::id::ast_print_unicode).value_or(false);
    bool print_ir = parse_res->get<cl::Flag>(phos::cli::id::print_ir).value_or(false);
    std::string ir_out_file = parse_res->get<cl::Text>(phos::cli::id::print_ir_op).value_or("ir.phosasm");

    std::ifstream file(filename);
    if (!file.is_open()) {
        std::println(stderr, "Error: Couldn't open file: {}", filename);
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    // 1. Initialize the VM
    phos::vm::Virtual_machine vm;

    // 2. Direct Assembly Execution Path!
    if (filename.ends_with(".phosasm")) {
        auto res_main = phos::vm::Assembler::assemble(source);
        auto res = vm.interpret(res_main);
        if (!res) {
            std::println(stderr, "Runtime error: {}", res.error().format());
            return 1;
        }
        return 0;
    }

    phos::mem::Arena arena;
    try {
        phos::lex::Lexer lexer(source);
        auto tokens = lexer.tokenize();
        // TODO: Do this in lexer itself.
        for (const auto &token : tokens) {
            if (token.type == phos::lex::TokenType::Invalid) {
                std::println(stderr, "{}:{}:{}: error: Invalid token: {}", filename, token.line, token.column, token.lexeme);
                return 1;
            }
        }

        phos::Parser parser(tokens, arena);
        auto parse_result = parser.parse();
        if (!parse_result) {
            std::println(stderr, "{}:{}", filename, parse_result.error().format());
            return 1;
        }

        // 3. Register Core Library & Type Check
        phos::Type_checker type_checker;
        phos::vm::core::register_core_library(vm, type_checker); // MUST BE BEFORE .check()!

        auto checked = type_checker.check(parse_result.value());

        if (print_ast) {
            phos::ast::AstPrinter printer;
            printer.use_unicode = print_use_unicode;
            printer.print_statements(parse_result.value());
            std::cout.flush();
        }

        if (checked.size() > 0) {
            for (auto e : checked)
                std::println(stderr, "{}:{}", filename, e.format());
            return 1; // Halt compilation if types are invalid
        }

        // 4. Compile to Bytecode
        phos::vm::Compiler compiler{&type_checker};
        auto chunk_result = compiler.compile(parse_result.value());
        if (!chunk_result) {
            std::println(stderr, "Compile error: {}", chunk_result.error().format());
            return 1;
        }

        // 5. Output IR if requested
        if (print_ir) {
            auto ir = phos::vm::Assembler::serialize(chunk_result.value());
            std::ofstream out_file(ir_out_file);
            if (out_file.is_open()) {
                out_file << ir;
                out_file.close();
                std::println("IR successfully written to {}", ir_out_file);
            } else {
                std::println(stderr, "Failed to write to {}. Printing to stdout:\n{}", ir_out_file, ir);
            }
        }

        // 6. Execute the compiled script!
        auto res = vm.interpret(chunk_result.value());
        if (!res) {
            std::println(stderr, "Runtime error: {}", res.error().format());
            return 1;
        }
    } catch (const std::exception &e) {
        std::cerr << "Unexpected error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
