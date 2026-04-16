#include "lexer/lexer.hpp"
#include "lexer/token.hpp"
#include "memory/arena.hpp"
#include "parser/ast-printer.hpp"
#include "parser/ast.hpp"
#include "parser/parser.hpp"
#include "semantic-checker/semantic_checker.hpp"

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <print>
#include <sstream>
#include <string>

// --- VM Integrations ---
#include "value/value.hpp"
#include "vm/compiler.hpp"
#include "vm/virtual_machine.hpp"

#define CL_IMPLEMENTATION
#include "cli-args.hpp"

int main(int argc, char *argv[])
{
    if (argc == 1) {
        // TODO: Fix
        std::println(stderr, "REPL currently not properly working");
        return 1;
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

    phos::mem::Arena arena;
    phos::vm::Virtual_machine vm(arena);
    phos::types::Type_table type_table{};
    phos::lex::Lexer lexer(source, arena);
    phos::ast::Ast_tree ast_tree;

    try {
        auto tokens = lexer.tokenize();

        if (!tokens.has_value()) {
            std::println(
                stderr,
                "{}:{}:{}: error: Invalid token: {}",
                filename,
                tokens.error().line,
                tokens.error().column,
                tokens.error().lexeme);
            return 1;
        }

        phos::Parser parser(*tokens, type_table, ast_tree, arena);
        auto parse_result = parser.parse();

        if (!parse_result) {
            std::println(stderr, "{}:{}", filename, parse_result.error().format());
            return 1;
        }

        phos::Type_environment env(type_table);
        phos::Semantic_checker checker{ast_tree, env, arena};

        auto checked = checker.check(parse_result.value());

        if (print_ast) {
            phos::ast::Tree_printer printer(ast_tree, type_table);
            printer.use_unicode = print_use_unicode;
            std::println("{}", printer.print_statements(parse_result.value()));
            std::cout.flush();
        }

        if (checked.size() > 0) {
            for (auto e : checked) {
                std::println(stderr, "{}:{}", filename, e.format());
            }
            return 1; // Halt compilation if types are invalid
        }

        // 4. Compile to Bytecode
        phos::vm::Compiler compiler{ast_tree, env};
        auto main_function = compiler.compile(parse_result.value());


        //if (!chunk_result) {
        //    std::println(stderr, "Compile error: {}", chunk_result.error().format());
        //    return 1;
        //}

        // 5. Output IR if requested
        //if (print_ir) {
        //    auto ir = phos::vm::Assembler::serialize(chunk_result.value());
        //    std::ofstream out_file(ir_out_file);
        //    if (out_file.is_open()) {
        //        out_file << ir;
        //        out_file.close();
        //        std::println("IR successfully written to {}", ir_out_file);
        //    } else {
        //        std::println(stderr, "Failed to write to {}. Printing to stdout:\n{}", ir_out_file, ir);
        //    }
        //}

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

        // 6. Execute the compiled script!
        phos::vm::Virtual_machine vm{arena};
        vm.execute(&main_thread);

    } catch (const std::exception &e) {
        std::cerr << "Unexpected error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
