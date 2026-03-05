#pragma once

#include <vector>
#include "../parser/ast.hpp"
#include "chunk.hpp"
#include "../error/result.hpp"

namespace phos::vm {

class Compiler {
public:
    Compiler() = default;

    Result<Chunk> compile(const std::vector<ast::Stmt*> &statements);

private:
    Chunk current_chunk;

    // Dispatchers
    void compile_stmt(ast::Stmt *stmt);
    void compile_expr(ast::Expr *expr);

    // Statement Visitors
    void visit_print_stmt(const ast::Print_stmt &stmt);
    void visit_expr_stmt(const ast::Expr_stmt &stmt);

    // Expression Visitors
    void visit_literal_expr(const ast::Literal_expr &expr);
    void visit_binary_expr(const ast::Binary_expr &expr);
    void visit_unary_expr(const ast::Unary_expr &expr);

    // Bytecode Emitters
    void emit_byte(uint8_t byte, phos::ast::Source_location loc);
    void emit_op(Op_code op, phos::ast::Source_location loc);
    void emit_constant(Value value, phos::ast::Source_location loc);
};

} // namespace phos::vm
