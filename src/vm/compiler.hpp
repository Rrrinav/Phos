#pragma once

#include <vector>
#include <string>
#include "../parser/ast.hpp"
#include "chunk.hpp"
#include "../error/result.hpp"

namespace phos::vm
{

struct Local
{
    std::string name;
    int depth;
};

class Compiler
{
public:
    Compiler() = default;

    Result<Chunk> compile(const std::vector<ast::Stmt *> &statements);

private:
    Chunk current_chunk;

    // --- Scoping & Locals ---
    std::vector<Local> locals;
    int scope_depth = 0;

    void begin_scope();
    void end_scope(phos::ast::Source_location loc);
    int resolve_local(const std::string &name);

    // --- Dispatchers ---
    void compile_stmt(ast::Stmt *stmt);
    void compile_expr(ast::Expr *expr);

    // --- Statement Visitors ---
    void visit_print_stmt(const ast::Print_stmt &stmt);
    void visit_expr_stmt(const ast::Expr_stmt &stmt);
    void visit_var_stmt(const ast::Var_stmt &stmt);
    void visit_block_stmt(const ast::Block_stmt &stmt);
    void visit_if_stmt(const ast::If_stmt &stmt);
    void visit_while_stmt(const ast::While_stmt &stmt);

    // --- Expression Visitors ---
    void visit_literal_expr(const ast::Literal_expr &expr);
    void visit_binary_expr(const ast::Binary_expr &expr);
    void visit_unary_expr(const ast::Unary_expr &expr);
    void visit_variable_expr(const ast::Variable_expr &expr);
    void visit_assignment_expr(const ast::Assignment_expr &expr);

    // --- Bytecode Emitters ---
    void emit_byte(uint8_t byte, phos::ast::Source_location loc);
    void emit_op(Op_code op, phos::ast::Source_location loc);
    void emit_constant(Value value, phos::ast::Source_location loc);
    uint8_t identifier_constant(const std::string &name, phos::ast::Source_location loc);

    // --- Control Flow Emitters ---
    size_t emit_jump(Op_code instruction, phos::ast::Source_location loc);
    void patch_jump(size_t offset, phos::ast::Source_location loc);
    void emit_loop(size_t loop_start, phos::ast::Source_location loc);
};

}  // namespace phos::vm
