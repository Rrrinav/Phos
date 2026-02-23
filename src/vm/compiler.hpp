#pragma once

#include <vector>
#include "../parser/ast.hpp"
#include "chunk.hpp"
#include "../error/result.hpp"

namespace phos::vm {

class Compiler {
public:
    Compiler() = default;

    Result<Chunk> compile(const std::vector<ast::Stmt*>& statements);

private:
    Chunk current_chunk;

    void compile_stmt(ast::Stmt* stmt);
    void visit_print_stmt(const ast::Print_stmt& stmt);
    void visit_expr_stmt(const ast::Expr_stmt& stmt);

    void compile_expr(ast::Expr* expr);
    void visit_literal_expr(const ast::Literal_expr& expr);
    void visit_binary_expr(const ast::Binary_expr& expr);

    void emit_byte(uint8_t byte, size_t line);
    void emit_op(Op_code op, size_t line);
    void emit_constant(Value value, size_t line);
};

} // namespace phos::vm
