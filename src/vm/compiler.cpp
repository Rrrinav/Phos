#include "compiler.hpp"
#include <print>

namespace phos::vm
{

Result<Chunk> Compiler::compile(const std::vector<ast::Stmt *> &statements)
{
    current_chunk = Chunk();

    for (auto *stmt : statements)
        if (stmt)
            compile_stmt(stmt);

    emit_op(Op_code::Return, {0, 0});
    return std::move(current_chunk);
}

void Compiler::compile_stmt(ast::Stmt *stmt)
{
    if (!stmt)
        return;

    if (auto *node = std::get_if<ast::Print_stmt>(&stmt->node))
        visit_print_stmt(*node);
    else if (auto *node = std::get_if<ast::Expr_stmt>(&stmt->node))
        visit_expr_stmt(*node);
    else
        std::println(stderr, "Unimplemented stmt node at index: {}", stmt->node.index());
}

void Compiler::compile_expr(ast::Expr *expr)
{
    if (!expr)
        return;

    if (auto *node = std::get_if<ast::Literal_expr>(&expr->node))
        visit_literal_expr(*node);
    else if (auto *node = std::get_if<ast::Binary_expr>(&expr->node))
        visit_binary_expr(*node);
    else
        std::println(stderr, "Unimplemented expr node at index: {}", expr->node.index());
}

void Compiler::visit_print_stmt(const ast::Print_stmt &stmt)
{
    compile_expr(stmt.expression);
    emit_op(Op_code::Print, stmt.loc);
}

void Compiler::visit_expr_stmt(const ast::Expr_stmt &stmt)
{
    compile_expr(stmt.expression);
    emit_op(Op_code::Pop, stmt.loc);
}

void Compiler::visit_literal_expr(const ast::Literal_expr &expr) { emit_constant(expr.value, expr.loc); }

void Compiler::visit_binary_expr(const ast::Binary_expr &expr)
{
    compile_expr(expr.left);
    compile_expr(expr.right);

    switch (expr.op)
    {
        case lex::TokenType::Plus:
            emit_op(Op_code::Add, expr.loc);
            break;
        case lex::TokenType::Minus:
            emit_op(Op_code::Subtract, expr.loc);
            break;
        case lex::TokenType::Star:
            emit_op(Op_code::Multiply, expr.loc);
            break;
        case lex::TokenType::Slash:
            emit_op(Op_code::Divide, expr.loc);
            break;
        case lex::TokenType::Equal:
            emit_op(Op_code::Equal, expr.loc);
            break;
        case lex::TokenType::NotEqual:
            emit_op(Op_code::Not_equal, expr.loc);
            break;
        default:
            break;
    }
}

void Compiler::emit_op(Op_code op, phos::ast::Source_location loc) { current_chunk.write(static_cast<uint8_t>(op), loc); }

void Compiler::emit_byte(uint8_t byte, phos::ast::Source_location loc) { current_chunk.write(byte, loc); }

void Compiler::emit_constant(Value value, phos::ast::Source_location loc)
{
    size_t index = current_chunk.add_constant(std::move(value));

    if (index > 255)
    {
        std::println(stderr, "Constant pool overflow at line {}", loc.l);
        std::abort();
    }

    emit_op(Op_code::Constant, loc);
    emit_byte(static_cast<uint8_t>(index), loc);
}

}  // namespace phos::vm
