#include "module_resolver.hpp"

#include "frontend/environment/symbol.hpp"
#include "frontend/lexer/lexer.hpp"
#include "frontend/parser/parser.hpp"

#include <filesystem>
#include <fstream>
#include <sstream>

namespace phos {

namespace {

struct Import_target
{
    std::string os_path;
    std::string logical_namespace;
};

Import_target build_import_target(const ast::Import_stmt &stmt)
{
    Import_target target{
        .os_path = stmt.path.front(),
        .logical_namespace = stmt.path.front(),
    };

    for (size_t i = 1; i < stmt.path.size(); ++i) {
        target.os_path += "/" + stmt.path[i];
        target.logical_namespace += "::" + stmt.path[i];
    }

    return target;
}

types::Type_id value_type(Type_environment &env, const Value &value)
{
    switch (value.tag()) {
    case Value_tag::Nil:
        return env.tt.get_nil();
    case Value_tag::Bool:
        return env.tt.get_bool();
    case Value_tag::I8:
        return env.tt.get_i8();
    case Value_tag::I16:
        return env.tt.get_i16();
    case Value_tag::I32:
        return env.tt.get_i32();
    case Value_tag::I64:
        return env.tt.get_i64();
    case Value_tag::U8:
        return env.tt.get_u8();
    case Value_tag::U16:
        return env.tt.get_u16();
    case Value_tag::U32:
        return env.tt.get_u32();
    case Value_tag::U64:
        return env.tt.get_u64();
    case Value_tag::F16:
        return env.tt.get_f16();
    case Value_tag::F32:
        return env.tt.get_f32();
    case Value_tag::F64:
        return env.tt.get_f64();
    case Value_tag::String:
        return env.tt.get_string();
    case Value_tag::Array:
    case Value_tag::Model:
    case Value_tag::Union:
    case Value_tag::Closure:
    case Value_tag::Iterator:
    case Value_tag::Green_thread:
    case Value_tag::Upvalue:
        return env.tt.get_any();
    }

    return env.tt.get_unknown();
}

void publish_native_exports(Compiler_context &ctx, Module_id module_id)
{
    auto &module = ctx.workspace.get_module(module_id);
    const std::string prefix = module.logical_namespace + "::";

    for (const auto &[name, signatures] : ctx.type_env.native_signatures) {
        if (!name.starts_with(prefix) || signatures.empty()) {
            continue;
        }

        const std::string export_name = name.substr(prefix.size());
        if (module.exported_symbols.contains(export_name)) {
            continue;
        }

        Symbol sym{
            .id = Symbol_id{0},
            .name = name,
            .kind = Symbol_kind::Native_func,
            .type = ctx.tt.get_unknown(),
            .owner_module = module_id,
            .is_public = true,
            .const_value = std::nullopt,
            .stack_offset = std::nullopt,
            .ffi_index = std::nullopt};

        module.add_public_symbol(export_name, ctx.registry.create_symbol(std::move(sym)));
    }

    for (const auto &[name, value] : ctx.type_env.native_constants) {
        if (!name.starts_with(prefix)) {
            continue;
        }

        const std::string export_name = name.substr(prefix.size());
        if (module.exported_symbols.contains(export_name)) {
            continue;
        }

        Symbol sym{
            .id = Symbol_id{0},
            .name = name,
            .kind = Symbol_kind::Native_const,
            .type = value_type(ctx.type_env, value),
            .owner_module = module_id,
            .is_public = true,
            .const_value = value,
            .stack_offset = std::nullopt,
            .ffi_index = std::nullopt};

        module.add_public_symbol(export_name, ctx.registry.create_symbol(std::move(sym)));
    }
}

} // namespace

Module_resolver::Module_resolver(Compiler_context &ctx, std::filesystem::path root) : ctx(ctx), root_dir(std::move(root))
{}

void Module_resolver::register_ffi(const std::string &name, std::function<void(Type_environment &)> init_fn)
{
    ffi_registry[name] = std::move(init_fn);
}

err::Engine Module_resolver::resolve_imports(Module_id current_module, const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {

        if (stmt_id.is_null()) {
            continue;
        }

        auto &node = ctx.tree.get(stmt_id).node;
        auto *import_stmt = std::get_if<ast::Import_stmt>(&node);

        if (!import_stmt) {
            continue;
        }

        const ast::Import_stmt import_copy = *import_stmt;
        auto target = build_import_target(import_copy);

        Module_id target_mod_id{0};
        bool newly_loaded = false;
        std::vector<ast::Stmt_id> parsed_stmts;

        // 1. Check if it is a native FFI module
        if (ffi_registry.contains(target.os_path)) {
            if (!loaded_modules.contains(target.os_path)) {
                ffi_registry[target.os_path](ctx.type_env);
                loaded_modules.insert(target.os_path);

                target_mod_id = ctx.workspace.create_module(target.logical_namespace, target.os_path);
                ctx.workspace.get_module(target_mod_id).is_native = true;
                publish_native_exports(ctx, target_mod_id);
            } else {
                target_mod_id = *ctx.workspace.get_module_by_path(target.os_path);
            }
        }
        // 2. Check if it is a source module or directory
        else {
            std::filesystem::path file_path = root_dir / (target.os_path + ".phos");
            std::filesystem::path dir_path = root_dir / target.os_path;

            file_path = file_path.lexically_normal();
            dir_path = dir_path.lexically_normal();

            if (!ctx.workspace.is_loaded(file_path)) {
                if (!std::filesystem::exists(file_path)) {
                    // Check if it is a directory import
                    if (std::filesystem::exists(dir_path) && std::filesystem::is_directory(dir_path)) {
                        if (!import_copy.local_alias.empty()) {
                            diagnostics_
                                .error(import_copy.loc.l, import_copy.loc.c, import_copy.loc.file, "Cannot alias an entire directory import.");
                            continue;
                        }

                        bool found_files = false;
                        // Traverse the directory and synthesize an explicit import for each file
                        for (const auto &entry : std::filesystem::directory_iterator(dir_path)) {
                            if (entry.is_regular_file() && entry.path().extension() == ".phos") {
                                ast::Import_stmt synth_import = import_copy;
                                synth_import.path.push_back(entry.path().stem().string());

                                ast::Stmt_id synth_id = ctx.tree.add_stmt(ast::Stmt(synth_import));
                                resolve_imports(current_module, {synth_id});
                                found_files = true;
                            }
                        }

                        if (!found_files) {
                            diagnostics_.error(
                                import_copy.loc.l,
                                import_copy.loc.c,
                                import_copy.loc.file,
                                "Directory '{}' contains no .phos files.",
                                target.os_path);
                        }
                        // Skip the rest of this loop iteration, as the directory contents have been handled recursively!
                        continue;
                    }

                    diagnostics_.error(import_copy.loc.l, import_copy.loc.c, import_copy.loc.file, "Module '{}' not found.", target.os_path);
                    continue;
                }

                std::ifstream file(file_path);
                if (!file.is_open()) {
                    diagnostics_.error(import_copy.loc.l, import_copy.loc.c, import_copy.loc.file, "Could not open module '{}'.", file_path.string());
                    continue;
                }

                std::stringstream buffer;
                buffer << file.rdbuf();
                std::string source = buffer.str();

                // Lex
                lex::Lexer lexer(source, ctx.arena, file_path.string());
                auto lexed = lexer.tokenize();

                if (!lexed.diagnostics.empty()) {
                    diagnostics_.append(lexed.diagnostics);
                    continue;
                }

                // Parse
                Parser parser(std::move(lexed.tokens), ctx, file_path.string());
                auto parsed = parser.parse();

                if (!parsed.diagnostics.empty()) {
                    diagnostics_.append(parsed.diagnostics);
                    continue;
                }

                target_mod_id = ctx.workspace.create_module(target.logical_namespace, file_path);
                loaded_modules.insert(target.os_path);
                parsed_stmts = parsed.statements;
                newly_loaded = true;

                for (auto sub_stmt_id : parsed.statements) {
                    if (sub_stmt_id.is_null()) {
                        continue;
                    }

                    ctx.workspace.get_module(target_mod_id).add_ast_root(sub_stmt_id);
                }
            } else {
                target_mod_id = *ctx.workspace.get_module_by_path(file_path);
            }
        }

        // Only pollute the local namespace if there are NO selective imports!
        if (import_copy.selectives.empty()) {
            std::string local_name = import_copy.local_alias.empty() ? import_copy.path.back() : import_copy.local_alias;
            ctx.workspace.get_module(current_module).add_private_import(local_name, target_mod_id);
        }

        // 4. Recurse if newly loaded
        if (newly_loaded) {
            resolve_imports(target_mod_id, parsed_stmts);
        }
    }

    return diagnostics_;
}

} // namespace phos
