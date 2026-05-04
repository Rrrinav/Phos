#include "module_resolver.hpp"

#include "frontend/lexer/lexer.hpp"
#include "frontend/parser/parser.hpp"

#include <fstream>
#include <iostream>

namespace phos {

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

        std::string raw_os_path;
        std::string raw_vm_namespace;
        std::string local_alias;
        std::string path_back;
        ast::Source_location import_loc;
        bool is_explicit_super = false;

        {
            auto &node = ctx.tree.get(stmt_id).node;
            auto *import_stmt = std::get_if<ast::Import_stmt>(&node);

            if (!import_stmt) {
                continue;
            }

            auto &current_mod = ctx.workspace.get_module(current_module);

            // 1. RESOLVE 'super' DIRECTORY TRAVERSAL
            if (!import_stmt->path.empty() && import_stmt->path[0] == "super") {
                is_explicit_super = true;

                // Break current namespace into a stack
                std::string c_ns = current_mod.logical_namespace;
                std::vector<std::string> ns_stack;
                size_t pos = 0;
                while ((pos = c_ns.find("::")) != std::string::npos) {
                    ns_stack.push_back(c_ns.substr(0, pos));
                    c_ns.erase(0, pos + 2);
                }
                if (!c_ns.empty()) {
                    ns_stack.push_back(c_ns);
                }

                // Pop the current file's name to get to its parent directory
                if (!ns_stack.empty()) {
                    ns_stack.pop_back();
                }

                // Pop a directory for every 'super' in the import path
                size_t path_idx = 0;
                while (path_idx < import_stmt->path.size() && import_stmt->path[path_idx] == "super") {
                    if (!ns_stack.empty()) {
                        ns_stack.pop_back();
                    }
                    path_idx++;
                }

                // Reconstruct the physical and logical paths
                for (const auto &part : ns_stack) {
                    raw_os_path += (raw_os_path.empty() ? "" : "/") + part;
                    raw_vm_namespace += (raw_vm_namespace.empty() ? "" : "::") + part;
                }

                for (; path_idx < import_stmt->path.size(); ++path_idx) {
                    raw_os_path += (raw_os_path.empty() ? "" : "/") + import_stmt->path[path_idx];
                    raw_vm_namespace += (raw_vm_namespace.empty() ? "" : "::") + import_stmt->path[path_idx];
                }

                // REWRITE AST NODE: Suppress 'super' so the Semantic Checker sees the absolute path
                std::vector<std::string> new_ast_path = ns_stack;
                size_t reset_idx = 0;
                while (reset_idx < import_stmt->path.size() && import_stmt->path[reset_idx] == "super") {
                    reset_idx++;
                }
                for (; reset_idx < import_stmt->path.size(); ++reset_idx) {
                    new_ast_path.push_back(import_stmt->path[reset_idx]);
                }
                import_stmt->path = new_ast_path;

            } else {
                // Standard Import Path (e.g. math.core)
                raw_os_path = import_stmt->path[0];
                raw_vm_namespace = import_stmt->path[0];
                for (size_t i = 1; i < import_stmt->path.size(); ++i) {
                    raw_os_path += "/" + import_stmt->path[i];
                    raw_vm_namespace += "::" + import_stmt->path[i];
                }
            }

            // 2. AST HEALING (Fixing parser's premature selective split)
            if (!import_stmt->selectives.empty()) {
                std::string extended_os_path = raw_os_path + "/" + import_stmt->selectives[0];
                bool is_fs_module = false;

                // Attempt A: Relative
                if (!is_explicit_super && !current_mod.logical_namespace.empty() && current_mod.logical_namespace != "main") {
                    std::filesystem::path rel_dir = current_mod.os_path.parent_path();
                    if (std::filesystem::exists(rel_dir / (extended_os_path + ".phos"))
                        || std::filesystem::is_directory(rel_dir / extended_os_path)) {
                        is_fs_module = true;
                    }
                }
                // Attempt B: Absolute
                if (!is_fs_module) {
                    if (std::filesystem::exists(root_dir / (extended_os_path + ".phos"))
                        || std::filesystem::is_directory(root_dir / extended_os_path)) {
                        is_fs_module = true;
                    }
                }

                if (is_fs_module) {
                    // It's a module! Move it from selectives into the true path
                    raw_os_path = extended_os_path;
                    raw_vm_namespace += "::" + import_stmt->selectives[0];

                    import_stmt->path.push_back(import_stmt->selectives[0]);

                    if (import_stmt->selectives.size() == 1) {
                        import_stmt->selectives.clear();
                    } else {
                        import_stmt->selectives.erase(import_stmt->selectives.begin());
                    }
                }
            }

            local_alias = import_stmt->local_alias;
            path_back = import_stmt->path.back();
            import_loc = import_stmt->loc;
        } // `node` reference safely goes out of scope here.

        // 3. SECURITY: Reject physically named 'super' folders/files
        if (raw_os_path == "super" || raw_os_path.find("/super") != std::string::npos || raw_os_path.find("super/") != std::string::npos) {
            if (!is_explicit_super) {
                diagnostics_.error(
                    import_loc.l,
                    import_loc.c,
                    import_loc.file,
                    "Invalid module name. 'super' is a reserved keyword and cannot be used as a file or folder name.");
                continue;
            }
        }

        Module_id target_mod_id{0};
        std::vector<ast::Stmt_id> parsed_stmts;

        // 4. Check if it is a native FFI module
        if (ffi_registry.contains(raw_os_path)) {
            if (!loaded_modules.contains(raw_os_path)) {
                ffi_registry[raw_os_path](ctx.type_env);
                loaded_modules.insert(raw_os_path);

                target_mod_id = ctx.workspace.create_module(raw_vm_namespace, raw_os_path);
                ctx.workspace.get_module(target_mod_id).is_native = true;
            } else {
                target_mod_id = *ctx.workspace.get_module_by_path(raw_os_path);
            }

            std::string local_name = local_alias.empty() ? path_back : local_alias;
            ctx.workspace.get_module(current_module).add_private_import(local_name, target_mod_id);
        }
        // 5. Check if it is a source module or a directory
        else {
            auto &current_mod = ctx.workspace.get_module(current_module);
            std::filesystem::path final_file_path;
            std::filesystem::path target_dir;
            bool is_dir = false;
            std::string final_vm_namespace = raw_vm_namespace;

            // Attempt A: Relative Lookup (Skipped if user manually used 'super')
            if (!is_explicit_super && !current_mod.logical_namespace.empty() && current_mod.logical_namespace != "main") {
                std::filesystem::path rel_dir = current_mod.os_path.parent_path();
                std::filesystem::path rel_file = rel_dir / (raw_os_path + ".phos");
                std::filesystem::path rel_folder = rel_dir / raw_os_path;

                if (std::filesystem::exists(rel_file)) {
                    final_file_path = rel_file;
                    size_t last_colon = current_mod.logical_namespace.rfind("::");
                    std::string parent_ns = (last_colon != std::string::npos) ? current_mod.logical_namespace.substr(0, last_colon) : "";
                    final_vm_namespace = parent_ns.empty() ? raw_vm_namespace : parent_ns + "::" + raw_vm_namespace;
                } else if (std::filesystem::is_directory(rel_folder)) {
                    is_dir = true;
                    target_dir = rel_folder;
                    size_t last_colon = current_mod.logical_namespace.rfind("::");
                    std::string parent_ns = (last_colon != std::string::npos) ? current_mod.logical_namespace.substr(0, last_colon) : "";
                    final_vm_namespace = parent_ns.empty() ? raw_vm_namespace : parent_ns + "::" + raw_vm_namespace;
                }
            }

            // Attempt B: Absolute Lookup (Acts as root fallback if super generated an absolute path)
            if (final_file_path.empty() && !is_dir) {
                std::filesystem::path abs_file = root_dir / (raw_os_path + ".phos");
                std::filesystem::path abs_folder = root_dir / raw_os_path;
                if (std::filesystem::exists(abs_file)) {
                    final_file_path = abs_file;
                    final_vm_namespace = raw_vm_namespace;
                } else if (std::filesystem::is_directory(abs_folder)) {
                    is_dir = true;
                    target_dir = abs_folder;
                    final_vm_namespace = raw_vm_namespace;
                }
            }

            if (final_file_path.empty() && !is_dir) {
                diagnostics_.error(import_loc.l, import_loc.c, import_loc.file, "Module '{}' not found.", raw_os_path);
                continue;
            }

            // Reusable lambda to safely process any individual file we encounter
            auto load_module_file = [&](const std::filesystem::path &file_path, const std::string &vm_ns) -> std::optional<Module_id> {
                if (ctx.workspace.is_loaded(file_path)) {
                    return *ctx.workspace.get_module_by_path(file_path);
                }

                std::ifstream file(file_path);
                if (!file.is_open()) {
                    diagnostics_.error(import_loc.l, import_loc.c, import_loc.file, "Could not open module '{}'.", file_path.string());
                    return std::nullopt;
                }

                std::stringstream buffer;
                buffer << file.rdbuf();
                std::string source = buffer.str();

                // Lex
                lex::Lexer lexer(source, ctx.arena, file_path.string());
                auto lexed = lexer.tokenize();

                if (!lexed.diagnostics.empty()) {
                    diagnostics_.append(lexed.diagnostics);
                    return std::nullopt;
                }

                // Parse
                Parser parser(std::move(lexed.tokens), ctx, file_path.string());
                auto parsed = parser.parse();

                if (!parsed.diagnostics.empty()) {
                    diagnostics_.append(parsed.diagnostics);
                    return std::nullopt;
                }

                Module_id new_mod_id = ctx.workspace.create_module(vm_ns, file_path);
                for (auto sub_stmt_id : parsed.statements) {
                    if (!sub_stmt_id.is_null()) {
                        ctx.workspace.get_module(new_mod_id).add_ast_root(sub_stmt_id);
                    }
                }

                resolve_imports(new_mod_id, parsed.statements);

                return new_mod_id;
            };

            // 6. Bulk Directory Import vs Standard Import
            if (is_dir) {
                for (const auto &entry : std::filesystem::directory_iterator(target_dir)) {
                    if (entry.is_regular_file() && entry.path().extension() == ".phos") {
                        std::string sub_name = entry.path().stem().string();
                        std::string sub_vm_namespace = final_vm_namespace + "::" + sub_name;

                        auto loaded_mod_id = load_module_file(entry.path(), sub_vm_namespace);
                        if (loaded_mod_id) {
                            ctx.workspace.get_module(current_module).add_private_import(sub_name, *loaded_mod_id);
                        }
                    }
                }
            } else {
                auto loaded_mod_id = load_module_file(final_file_path, final_vm_namespace);
                if (loaded_mod_id) {
                    std::string local_name = local_alias.empty() ? path_back : local_alias;
                    ctx.workspace.get_module(current_module).add_private_import(local_name, *loaded_mod_id);
                }
            }
        }
    }

    return diagnostics_;
}

} // namespace phos
