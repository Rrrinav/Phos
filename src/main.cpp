#include "cli-args.hpp"
#include "core/arena.hpp"
#include "core/value/value.hpp"
#include "frontend/environment/compiler_context.hpp"
#include "frontend/lexer/lexer.hpp"
#include "frontend/parser/ast-printer.hpp"
#include "frontend/parser/parser.hpp"

// Middle-end / Semantic Passes
#include "frontend/module_resolver/module_resolver.hpp"
#include "frontend/semantic/resolver.hpp"
#include "frontend/semantic/semantic_checker.hpp"

// StdLib Native Modules
#include "frontend/core_library/core_modules/math_module.hpp"

// Backend & Execution
#include "backend/assembler.hpp"
#include "backend/compiler.hpp"
#include "virtual_machine/virtual_machine.hpp"

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <print>
#include <sstream>
#include <string>
#include <vector>

int main(int argc, char *argv[])
{
    // 1. CLI ARGUMENT PARSING (Custom V2 Parser)
    if (argc == 1) {
        phos::cli::print_help();
        return 1;
    }

    auto [config, success, err_msg] = phos::cli::parse_args(argc, argv);

    if (!success) {
        std::println(stderr, "{}", err_msg);
        std::println(stderr, "Run 'phos --help' for usage.");
        return EXIT_FAILURE;
    }

    if (config.help) {
        phos::cli::print_help();
        return 0;
    }

    std::string filename = config.input_file;

    // 2. READ INPUT FILE
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::println(stderr, "Error: Couldn't open file: {}", filename);
        return EXIT_FAILURE;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    // INITIALIZE V2 COMPILER STATE (The God Object)
    phos::mem::Arena arena(10 * 1024 * 1024); // 10MB Pool
    phos::Compiler_context ctx(arena);

    phos::vm::Virtual_machine vm(ctx.arena);
    vm.cfg.trace_execution = config.trace_vm;

    phos::Closure_data main_function{};

    // 4. COMPILER PIPELINE
    if (filename.ends_with(".phosasm")) {
        // Load raw IR directly
        main_function = phos::vm::Assembler::deserialize(source, ctx.arena);
    } else {
        // LEXING
        phos::lex::Lexer lexer(source, ctx.arena, filename);
        auto lexed = lexer.tokenize();

        if (!lexed.diagnostics.empty()) {
            lexed.diagnostics.print(std::cerr);
            if (lexed.diagnostics.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // Parsing
        phos::Parser parser(std::move(lexed.tokens), ctx, filename);
        auto parse_result = parser.parse();

        if (!parse_result.diagnostics.empty()) {
            parse_result.diagnostics.print(std::cerr);
            if (parse_result.diagnostics.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // V2 Transition: Store AST in the Workspace instead of Type Environment
        phos::Module_id main_mod = ctx.workspace.create_module("main", filename);
        for (auto stmt : parse_result.statements) {
            ctx.workspace.get_module(main_mod).add_ast_root(stmt);
        }

        // ENVIRONMENT SETUP
        ctx.type_env.register_core_methods();

        // MODULE IMPORT RESOLUTION
        phos::Module_resolver module_resolver(ctx, std::filesystem::path(filename).parent_path());
        module_resolver.register_ffi("math", [](phos::Type_environment &env) { phos::vm::modules::register_math(env); });

        auto import_result = module_resolver.resolve_imports(main_mod, parse_result.statements);

        if (!import_result.empty()) {
            import_result.print(std::cerr);
            if (import_result.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // RESOLVER (Name Binding)
        phos::Resolver resolver(ctx);
        auto resolved = resolver.resolve(parse_result.statements);

        if (!resolved.empty()) {
            resolved.print(std::cerr);
            if (resolved.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // SEMANTIC CHECKING
        phos::Semantic_checker checker(ctx);
        auto checked = checker.check(parse_result.statements);

        if (!checked.empty()) {
            checked.print(std::cerr);
            if (checked.has_errors()) {
                return EXIT_FAILURE;
            }
        }

        // AST DIAGNOSTICS
        if (config.cmd_ast_print) {
            phos::ast::Tree_printer printer(ctx.tree, ctx.tt, config.ast_print_unicode);
            std::println("{}", printer.print_statements(parse_result.statements));
        }

        // CODE GENERATION
        phos::vm::Compiler compiler(ctx);
        main_function = compiler.compile(parse_result.statements);

        if (config.print_ir) {
            auto ir = phos::vm::Assembler::serialize(main_function);
            std::ofstream out_file(config.ir_out_file);
            if (out_file.is_open()) {
                out_file << ir;
            }
            return 0; // Exit after printing IR
        }
    }

    // VM EXECUTION
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
