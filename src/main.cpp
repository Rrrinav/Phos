#include "./semantic-checker/modules/math_module.hpp"
#include "lexer/lexer.hpp"
#include "memory/arena.hpp"
#include "parser/ast-printer.hpp"
#include "parser/ast.hpp"
#include "parser/parser.hpp"
#include "semantic-checker/module_resolver.hpp"
#include "semantic-checker/resolver.hpp"
#include "semantic-checker/semantic_checker.hpp"
#include "value/value.hpp"
#include "vm/assembler.hpp"
#include "vm/compiler.hpp"
#include "vm/virtual_machine.hpp"

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <print>
#include <sstream>
#include <string>
#include <vector>

#define CL_IMPLEMENTATION
#include "cli-args.hpp"

int main(int argc, char *argv[])
{
    if (argc == 1) {
        std::println(stderr, "REPL currently not implemented.");
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

    phos::mem::Arena arena(10 * 1024 * 1024);

    phos::vm::Virtual_machine vm(arena);
    vm.cfg.trace_execution = trace_vm;

    phos::types::Type_table type_table{};
    phos::ast::Ast_tree ast_tree{};

    phos::Closure_data main_function{};

    if (filename.ends_with(".phosasm")) {
        main_function = phos::vm::Assembler::deserialize(source, arena);
    } else {
        // -------------------------------------------------
        // 1. LEXING
        // -------------------------------------------------
        phos::lex::Lexer lexer(source, arena, filename);

        auto lexed = lexer.tokenize();

        if (!lexed.diagnostics.empty()) {
            lexed.diagnostics.print(std::cerr);

            if (lexed.diagnostics.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // -------------------------------------------------
        // 2. PARSING
        // -------------------------------------------------
        phos::Parser parser(std::move(lexed.tokens), type_table, ast_tree, arena, filename);

        auto parse_result = parser.parse();

        if (!parse_result.diagnostics.empty()) {
            parse_result.diagnostics.print(std::cerr);

            if (parse_result.diagnostics.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // -------------------------------------------------
        // 3. ENVIRONMENT
        // -------------------------------------------------
        phos::Type_environment env(type_table);
        env.register_core_methods();

        env.program_statements.insert(env.program_statements.end(), parse_result.statements.begin(), parse_result.statements.end());

        phos::vm::modules::register_math(env);
        env.native_module_names.insert("math");

        // -------------------------------------------------
        // 4. MODULE IMPORT RESOLUTION
        // -------------------------------------------------
        phos::Module_resolver module_resolver(env, ast_tree, arena, std::filesystem::path(filename).parent_path());
        module_resolver.register_ffi("math", [](phos::Type_environment &env) { phos::vm::modules::register_math(env); });

        auto import_result = module_resolver.resolve_imports(parse_result.statements);
        if (!import_result.empty()) {
            import_result.print(std::cerr);
            if (import_result.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // =================================================
        // 5. RESOLVER PASS (MUST RUN FIRST!)
        // =================================================
        phos::Resolver resolver(ast_tree, env, arena);
        auto resolved = resolver.resolve(env.program_statements);

        if (!resolved.empty()) {
            resolved.print(std::cerr);
            if (resolved.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // =================================================
        // 6. SEMANTIC CHECKING (MUST RUN SECOND!)
        // =================================================
        phos::Semantic_checker checker(ast_tree, env, arena);
        auto checked = checker.check(env.program_statements);

        if (!checked.empty()) {
            checked.print(std::cerr);
            if (checked.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // -------------------------------------------------
        // 7. OPTIONAL AST PRINT
        // -------------------------------------------------
        if (print_ast) {
            phos::ast::Tree_printer printer(ast_tree, type_table, print_use_unicode);
            std::println("{}", printer.print_statements(parse_result.statements));
        }

        // -------------------------------------------------
        // 8. COMPILATION
        // -------------------------------------------------
        phos::vm::Compiler compiler(ast_tree, env, arena);
        main_function = compiler.compile(env.program_statements);

        if (print_ir) {
            auto ir = phos::vm::Assembler::serialize(main_function);

            std::ofstream out_file(ir_out_file);

            if (out_file.is_open()) {
                out_file << ir;
            }

            return 0;
        }
    }

    // -------------------------------------------------
    // 9. VM EXECUTION
    // -------------------------------------------------
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

    return 0;
}
