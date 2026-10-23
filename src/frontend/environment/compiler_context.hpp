#pragma once

#include "core/arena.hpp"
#include "core/value/type.hpp"
#include "frontend/environment/symbol.hpp"
#include "frontend/environment/type_environment.hpp"
#include "frontend/environment/workspace.hpp"
#include "frontend/parser/ast.hpp"

#include <cstdint>
#include <string_view>

namespace phos {

struct Compiler_context
{
    // 1. MEMORY & CORE STATE
    mem::Arena &arena;
    mem::Arena &program_arena;
    ast::Ast_tree tree;
    types::Type_table tt;

    // 2. THE THREE MIDDLE-END PILLARS
    Workspace workspace;
    Symbol_registry registry;
    Type_environment type_env;

    // In REPL sessions, top-level `let` / `mut` bindings are backed by VM
    // global slots so they survive across input entries.
    bool repl_force_global = false;

    uint32_t intern_source(std::string_view name)
    {
        return static_cast<uint32_t>(registry.intern(name).value);
    }

    std::string_view source_name(uint32_t id) const
    {
        return id == UINT32_MAX ? std::string_view{} : registry.resolve(String_id{id});
    }

    void release_frontend()
    {
        tree.expressions.clear();
        tree.expressions.shrink_to_fit();
        tree.statements.clear();
        tree.statements.shrink_to_fit();
        workspace.clear();
        type_env.clear();
        registry.clear();
        tt.reset();
    }

    // Clean constructor
    Compiler_context(mem::Arena &frontend, mem::Arena &program) : arena(frontend), program_arena(program), type_env(tt)
    {}

    explicit Compiler_context(mem::Arena &shared) : Compiler_context(shared, shared)
    {}
};

} // namespace phos
