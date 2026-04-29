#include "module_resolver.hpp"

#include "../lexer/lexer.hpp"
#include "../parser/parser.hpp"

#include <fstream>
#include <sstream>

namespace phos {

Module_resolver::Module_resolver(Type_environment &env, ast::Ast_tree &tree, mem::Arena &arena, std::filesystem::path root)
    : env(env), tree(tree), arena(arena), root_dir(std::move(root))
{}

void Module_resolver::register_ffi(const std::string &name, std::function<void(Type_environment &)> init_fn)
{
    ffi_registry[name] = std::move(init_fn);
}

err::Engine Module_resolver::resolve_imports(const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {

        if (stmt_id.is_null()) {
            continue;
        }

        auto &node = tree.get(stmt_id).node;

        auto *import_stmt = std::get_if<ast::Import_stmt>(&node);

        if (!import_stmt) {
            continue;
        }

        // ---------------------------------
        // Build names
        // ---------------------------------
        std::string os_path = import_stmt->path[0];
        std::string vm_namespace = import_stmt->path[0];

        for (size_t i = 1; i < import_stmt->path.size(); ++i) {
            os_path += "." + import_stmt->path[i];
            vm_namespace += "::" + import_stmt->path[i];
        }

        // =========================================================
        // ALIAS MAPPING (MOVED UP)
        // Must happen BEFORE any recursive imports resize the vector!
        // =========================================================
        if (!import_stmt->selectives.empty()) {
            for (const auto &sym : import_stmt->selectives) {
                std::string local_name = import_stmt->local_alias.empty() ? sym : import_stmt->local_alias;
                env.module_aliases[local_name] = vm_namespace + "::" + sym;
            }
        } else {
            std::string local_name = import_stmt->local_alias.empty() ? import_stmt->path.back() : import_stmt->local_alias;
            env.module_aliases[local_name] = vm_namespace;
        }

        // ---------------------------------
        // Load if not already loaded
        // ---------------------------------
        if (!loaded_modules.contains(os_path)) {

            // -----------------------------
            // Native module
            // -----------------------------
            if (ffi_registry.contains(os_path)) {
                ffi_registry[os_path](env);
                loaded_modules.insert(os_path);
            }

            // -----------------------------
            // Source module
            // -----------------------------
            else {
                std::filesystem::path file_path = root_dir / (os_path + ".phos");

                if (!std::filesystem::exists(file_path)) {
                    diagnostics_.error(import_stmt->loc.l, import_stmt->loc.c, import_stmt->loc.file, "Module '{}' not found.", os_path);
                    continue;
                }

                std::ifstream file(file_path);

                if (!file.is_open()) {
                    std::string mod_path = file_path.string();
                    diagnostics_
                        .error(import_stmt->loc.l, import_stmt->loc.c, import_stmt->loc.file, "Could not open module '{}'.", mod_path);
                    continue;
                }

                std::stringstream buffer;
                buffer << file.rdbuf();

                std::string source = buffer.str();

                // -------------------------
                // Lex
                // -------------------------
                lex::Lexer lexer(source, arena, file_path.string());
                auto lexed = lexer.tokenize();

                if (!lexed.diagnostics.empty()) {
                    diagnostics_.append(lexed.diagnostics);
                    continue;
                }

                // -------------------------
                // Parse
                // -------------------------
                Parser parser(std::move(lexed.tokens), env.tt, tree, arena, file_path.string());
                auto parsed = parser.parse();

                if (!parsed.diagnostics.empty()) {
                    diagnostics_.append(parsed.diagnostics);
                    continue;
                }

                // -------------------------
                // Add to global program & Register
                // -------------------------
                Module_id mod_id = env.registry.create_module(file_path.string(), vm_namespace);
                loaded_modules.insert(os_path);

                for (auto sub_stmt_id : parsed.statements) {
                    if (sub_stmt_id.is_null()) {
                        continue;
                    }
                    auto &sub_node = tree.get(sub_stmt_id).node;

                    // Mangle and Register Functions
                    if (auto *fn = std::get_if<ast::Function_stmt>(&sub_node)) {
                        std::string local_name = fn->name;
                        fn->name = vm_namespace + "::" + local_name;

                        Symbol sym{
                            .id = Symbol_id{0},
                            .name = fn->name,
                            .kind = Symbol_kind::Phos_func,
                            .type = env.tt.get_unknown(),
                            .owner_module = mod_id,
                            .is_public = true,
                            .const_value = std::nullopt,
                            .stack_offset = std::nullopt,
                            .ffi_index = std::nullopt};
                        Symbol_id sym_id = env.registry.create_symbol(std::move(sym));
                        env.registry.export_symbol(mod_id, local_name, sym_id);
                    }
                    // Mangle and Register Models
                    else if (auto *model = std::get_if<ast::Model_stmt>(&sub_node)) {
                        std::string local_name = model->name;
                        model->name = vm_namespace + "::" + local_name;

                        Symbol sym{
                            .id = Symbol_id{0},
                            .name = model->name,
                            .kind = Symbol_kind::Model_def,
                            .type = env.tt.get_unknown(),
                            .owner_module = mod_id,
                            .is_public = true,
                            .const_value = std::nullopt,
                            .stack_offset = std::nullopt,
                            .ffi_index = std::nullopt};
                        Symbol_id sym_id = env.registry.create_symbol(std::move(sym));
                        env.registry.export_symbol(mod_id, local_name, sym_id);
                    }
                }

                env.program_statements.insert(env.program_statements.end(), parsed.statements.begin(), parsed.statements.end());

                // -------------------------
                // Recursive imports (THIS RESIZES THE VECTOR)
                // -------------------------
                resolve_imports(parsed.statements);
            }
        }
    }

    return diagnostics_;
}

} // namespace phos
