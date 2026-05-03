#pragma once

#include "core/arena.hpp"
#include "core/value/type.hpp"
#include "frontend/parser/ast.hpp"

#include "frontend/environment/symbol.hpp"
#include "frontend/environment/type_environment.hpp"
#include "frontend/environment/workspace.hpp"

namespace phos {

struct Compiler_context
{
    // 1. MEMORY & CORE STATE
    mem::Arena &arena;
    ast::Ast_tree tree;
    types::Type_table tt;

    // 2. THE THREE MIDDLE-END PILLARS
    Workspace workspace;
    Symbol_registry registry;
    Type_environment type_env;

    // Clean constructor
    Compiler_context(mem::Arena &a) : arena(a), type_env(tt)
    {}
};

} // namespace phos
