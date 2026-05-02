#pragma once

#include "core/error/err.hpp"
#include "frontend/environment/compiler_context.hpp"

#include <filesystem>
#include <functional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace phos {

class Module_resolver
{
    Compiler_context &ctx;
    std::filesystem::path root_dir;

    // The FFI Registry: Maps a module name (e.g. "math") to its C++ setup function
    std::unordered_map<std::string, std::function<void(Type_environment &)>> ffi_registry;

    // Tracks what we've already loaded to prevent duplicate work
    std::unordered_set<std::string> loaded_modules;

    err::Engine diagnostics_{"module-resolver"};

public:
    Module_resolver(Compiler_context &ctx, std::filesystem::path root);

    // Binds a C++ module initializer to a module path
    void register_ffi(const std::string &name, std::function<void(Type_environment &)> init_fn);

    // The main pre-pass entry point
    err::Engine resolve_imports(Module_id current_module, const std::vector<ast::Stmt_id> &statements);
};

} // namespace phos
