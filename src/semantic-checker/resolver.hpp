#pragma once

#include "../error/err.hpp"
#include "../parser/ast.hpp"
#include "type_environment.hpp"

#include <string>
#include <vector>

namespace phos {

// The Resolver traverses the AST to build the global Type_environment.
// It converts Unresolved_type placeholders into concrete Type_ids.
// Mostly resolves statements and related things like Model/Union/Enum names.
class Resolver
{
public:
    Resolver(ast::Ast_tree &tree, Type_environment &env, mem::Arena& arena);

    err::Engine resolve(const std::vector<ast::Stmt_id> &statements);

    // The 3 Resolution Passes
    void declare_globals(const std::vector<ast::Stmt_id> &statements);
    void resolve_placeholders(const std::vector<ast::Stmt_id> &statements);
    void populate_signatures(const std::vector<ast::Stmt_id> &statements);

    // AST Walkers
    void resolve_type(types::Type_id &id, const ast::Source_location &loc);
    void resolve_stmt(ast::Stmt_id stmt);
    void resolve_expr(ast::Expr_id expr);

private:
    ast::Ast_tree &tree;
    Type_environment &env;
    phos::mem::Arena& arena_;
    err::Engine diagnostics_{"resolution"};

    void resolver_error(const ast::Source_location &loc, const std::string &message);
    void resolver_warning(const ast::Source_location &loc, const std::string &message);
};

} // namespace phos
