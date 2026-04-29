#pragma once

#include "../error/err.hpp"
#include "../parser/ast.hpp"
#include "type_environment.hpp"

#include <filesystem>
#include <functional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace phos {

class Module_resolver
{
    mem::Arena& arena;
    Type_environment &env;
    ast::Ast_tree &tree;
    std::filesystem::path root_dir;

    // The FFI Registry: Maps a module name (e.g. "math") to its C++ setup function
    std::unordered_map<std::string, std::function<void(Type_environment &)>> ffi_registry;

    // Tracks what we've already loaded to prevent duplicate work
    std::unordered_set<std::string> loaded_modules;

    err::Engine diagnostics_{"module-resolver"};

public:
    Module_resolver(Type_environment &env, ast::Ast_tree &tree, mem::Arena &arena, std::filesystem::path root);

    // Binds a C++ module initializer to a module path
    void register_ffi(const std::string &name, std::function<void(Type_environment &)> init_fn);

    // The main pre-pass entry point
    err::Engine resolve_imports(const std::vector<ast::Stmt_id> &statements);
};

} // namespace phos
