#include "lexer/lexer.hpp"
#include "memory/arena.hpp"
#include "parser/ast.hpp"
#include "parser/parser.hpp"
#include "semantic-checker/semantic_checker.hpp"

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <print>
#include <sstream>
#include <string>
#include <vector>

#include "value/value.hpp"
#include "vm/assembler.hpp"
#include "vm/compiler.hpp"
#include "vm/virtual_machine.hpp"

#define CL_IMPLEMENTATION
#include "cli-args.hpp"

int main(int argc, char *argv[])
{
    if (argc == 1) {
        std::println(stderr, "REPL currently not properly working");
        return 1;
    }

    cl::Parser p("Phos", "Phos interpreted programming language");
    phos::cli::add_arguemnts(p);
    auto parse_res = p.parse(argc, argv);

    if (!parse_res) {
        std::println(stderr, "Args error: {}", parse_res.error());
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

    bool trace_vm = parse_res->get<cl::Flag>("trace_vm").value_or(false);

    std::ifstream file(filename);
    if (!file.is_open()) {
        std::println(std::cerr, "Error: Couldn't open file: {}", filename);
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    // Allocate 10MB of memory for the script to use.
    phos::mem::Arena arena(10 * 1024 * 1024);

    // Create the VM exactly once
    phos::vm::Virtual_machine vm(arena);

    // Configure the debugger based on user flag
    vm.cfg.trace_execution = trace_vm;

    phos::types::Type_table type_table{};
    phos::ast::Ast_tree ast_tree;

    try {
        phos::Closure_data main_function{};

        if (filename.ends_with(".phosasm")) {
            main_function = phos::vm::Assembler::deserialize(source, arena);

        } else {
            // STANDARD PIPELINE
            phos::lex::Lexer lexer(source, arena);
            auto tokens = lexer.tokenize();
            if (!tokens.has_value()) {
                return 1;
            }

            phos::Parser parser(*tokens, type_table, ast_tree, arena);
            auto parse_result = parser.parse();
            if (!parse_result) {
                return 1;
            }

            phos::Type_environment env(type_table);
            phos::Semantic_checker checker{ast_tree, env, arena};
            auto checked = checker.check(parse_result.value());
            if (checked.size() > 0) {
                return 1;
            }

            phos::vm::Compiler compiler{ast_tree, env, arena};
            main_function = compiler.compile(parse_result.value());

            if (print_ir) {
                auto ir = phos::vm::Assembler::serialize(main_function);
                std::ofstream out_file(ir_out_file);
                if (out_file.is_open()) {
                    out_file << ir;
                }
            }
        }

        phos::vm::Call_frame frames[10];
        frames[0] = phos::vm::Call_frame(&main_function, 0);

        std::vector<phos::Value> thread_memory(256);

        phos::Green_thread_data main_thread{};
        main_thread.call_stack = frames;
        main_thread.call_stack_count = 1;
        main_thread.call_stack_capacity = 10;
        main_thread.value_stack = thread_memory.data();
        main_thread.value_stack_capacity = thread_memory.size();
        main_thread.is_completed = false;

        vm.execute(&main_thread);
    } catch (const std::exception &e) {
        std::println(stderr, "Unexpected error: {}", e.what());
        return 1;
    }

    return 0;
}
