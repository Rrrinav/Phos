#include "phos.hpp"

#include "backend/assembler.hpp"
#include "backend/compiler.hpp"
#include "frontend/core_library/core_modules/math_module.hpp"
#include "frontend/core_library/core_modules/time_module.hpp"
#include "frontend/lexer/lexer.hpp"
#include "frontend/module_resolver/module_resolver.hpp"
#include "frontend/parser/ast-printer.hpp"
#include "frontend/parser/parser.hpp"
#include "frontend/semantic/semantic_checker.hpp"

#include <fstream>
#include <print>
#include <sstream>

namespace phos {

// Updated constructor to explicitly initialize gc_heap and pass it to vm
Phos_engine::Phos_engine(size_t memory_size_bytes, std::ostream *out, std::ostream *err)
    : arena(memory_size_bytes), ctx(arena), gc_heap(), vm(gc_heap, arena), out_stream(out), err_stream(err)
{}

bool Phos_engine::compile_file(const std::filesystem::path &file_path)
{
    std::ifstream file(file_path);

    if (!file.is_open()) {
        std::println(*err_stream, "Error: Couldn't open file: {}", file_path.string());
        return false;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    if (file_path.extension() == ".phosasm") {
        main_function = vm::Assembler::deserialize(source, arena);
        return true;
    }

    std::filesystem::path root_dir = file_path.parent_path();
    return compile_source(source, file_path.string(), root_dir);
}

bool Phos_engine::compile_source(const std::string &source, const std::string &logical_name, const std::filesystem::path &root_dir)
{
    lex::Lexer lexer(source, arena, logical_name);
    auto lexed = lexer.tokenize();
    if (!lexed.diagnostics.empty()) {
        lexed.diagnostics.print(*err_stream);
        return false;
    }

    Parser parser(std::move(lexed.tokens), ctx, logical_name);
    auto parse_result = parser.parse();
    if (!parse_result.diagnostics.empty()) {
        parse_result.diagnostics.print(*err_stream);
        return false;
    }

    Module_id main_mod = ctx.workspace.create_module("main", logical_name);
    main_statements = parse_result.statements;

    for (auto stmt_id : parse_result.statements) {
        ctx.workspace.get_module(main_mod).add_ast_root(stmt_id);
    }

    ctx.type_env.register_core_methods();

    Module_resolver module_resolver(ctx, root_dir);
    module_resolver.register_ffi("math", [](Type_environment &env) { vm::modules::register_math(env); });
    module_resolver.register_ffi("time", [](Type_environment &env) { vm::modules::register_time(env); });

    auto import_result = module_resolver.resolve_imports(main_mod, parse_result.statements);
    if (!import_result.empty()) {
        import_result.print(*err_stream);
        return false;
    }

    Semantic_checker checker(ctx);
    auto checked = checker.check_workspace();
    if (!checked.empty()) {
        checked.print(*err_stream);
        return false;
    }

    vm::Compiler compiler(ctx);
    main_function = compiler.compile_workspace(main_mod);

    return true;
}

void Phos_engine::execute()
{
    constexpr size_t call_stack_capacity = 256;

    std::vector<vm::Call_frame> frames(call_stack_capacity);
    frames[0] = vm::Call_frame(&main_function, 0);

    std::vector<Value> thread_memory(call_stack_capacity * vm::Virtual_machine::FRAME_REGISTER_WINDOW);

    Green_thread_data main_thread{};
    main_thread.call_stack = frames.data();
    main_thread.call_stack_count = 1;
    main_thread.call_stack_capacity = frames.size();
    main_thread.value_stack = thread_memory.data();
    main_thread.value_stack_capacity = thread_memory.size();
    main_thread.live_value_count = vm::Virtual_machine::FRAME_REGISTER_WINDOW;
    main_thread.is_completed = false;

    vm.execute(&main_thread);
}

std::string Phos_engine::dump_ir() const
{
    return vm::Assembler::serialize(main_function);
}

std::string Phos_engine::dump_ast(bool use_unicode) const
{
    ast::Tree_printer printer(ctx.tree, ctx.tt, use_unicode);
    return printer.print_statements(main_statements);
}

} // namespace phos
