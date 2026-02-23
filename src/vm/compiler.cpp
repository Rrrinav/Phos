#include "compiler.hpp"
#include <cstdlib>
#include <numbers>
#include <print>
#include <variant>

template <class... Ts>
struct Overloader : Ts...
{
    using Ts::operator()...;
};
template <class... Ts>
Overloader(Ts...) -> Overloader<Ts...>;

namespace phos::vm
{

Result<Chunk> Compiler::compile(const std::vector<ast::Stmt *> &statements)
{
    current_chunk = Chunk();

    for (auto *stmt : statements)
        if (stmt)
            compile_stmt(stmt);

    emit_op(Op_code::Return, 0);
    return std::move(current_chunk);
}

void Compiler::compile_stmt(ast::Stmt *stmt)
{
    if (std::holds_alternative<ast::Print_stmt>(stmt->node)) {
        visit_print_stmt(std::get<ast::Print_stmt>(stmt->node));
        return;
    }

    if (std::holds_alternative<ast::Expr_stmt>(stmt->node)) {
        visit_expr_stmt(std::get<ast::Expr_stmt>(stmt->node));
        return;
    }

    if (std::holds_alternative<ast::Expr_stmt>(stmt->node)) {
        visit_expr_stmt(std::get<ast::Expr_stmt>(stmt->node));
        return;
    }

    std::println("Unimplemented: {}", typeid(stmt->node).name());
    //std::visit(
    //Overloader{
    //    [this](const ast::Print_stmt &node) { visit_print_stmt(node); },
    //    [this](const ast::Expr_stmt &node) { visit_expr_stmt(node); },
    //    [this](auto &&node) { std::println("Unimplemented: {}", typeid(node).name());}
    //},
    //stmt->node);
}

void Compiler::compile_expr(ast::Expr *expr)
{
    if (!expr)
        return;

    if (std::holds_alternative<ast::Binary_expr>(expr->node)) {
        visit_binary_expr(std::get<ast::Binary_expr>(expr->node));
        return;
    }

    if (std::holds_alternative<ast::Literal_expr>(expr->node)) {
        visit_literal_expr(std::get<ast::Literal_expr>(expr->node));
        return;
    }

    std::println(stderr, "Unimplemented expr: {}", typeid(expr->node).name());
}

void Compiler::visit_print_stmt(const ast::Print_stmt &stmt)
{
    compile_expr(stmt.expression);
    emit_op(Op_code::Print, stmt.loc.line);
}

void Compiler::visit_expr_stmt(const ast::Expr_stmt &stmt)
{
    compile_expr(stmt.expression);
    emit_op(Op_code::Pop, stmt.loc.line);  // Discard result of expression statements
}

void Compiler::visit_literal_expr(const ast::Literal_expr &expr) { emit_constant(expr.value, expr.loc.line); }

void Compiler::visit_binary_expr(const ast::Binary_expr &expr)
{
    // Post-order traversal: Left, then Right, then Operator
    compile_expr(expr.left);
    compile_expr(expr.right);

    switch (expr.op)
    {
        case lex::TokenType::Plus:
            emit_op(Op_code::Add, expr.loc.line);
            break;
        case lex::TokenType::Minus:
            emit_op(Op_code::Subtract, expr.loc.line);
            break;
        case lex::TokenType::Star:
            emit_op(Op_code::Multiply, expr.loc.line);
            break;
        case lex::TokenType::Slash:
            emit_op(Op_code::Divide, expr.loc.line);
            break;
        case lex::TokenType::Equal:
            emit_op(Op_code::Equal, expr.loc.line);
            break;
        case lex::TokenType::NotEqual:
            emit_op(Op_code::Not_equal, expr.loc.line);
            break;
        default:
            break;
    }
}

void Compiler::emit_byte(uint8_t byte, size_t line) { current_chunk.write(byte, line); }

void Compiler::emit_op(Op_code op, size_t line) { current_chunk.write(op, line); }

void Compiler::emit_constant(Value value, size_t line)
{
    size_t index = current_chunk.add_constant(std::move(value));
    if (index > 255)
    {
        std::println(stderr, "No more than 255 constants please, we are smoll");
        std::abort();
    }
    emit_op(Op_code::Constant, line);
    emit_byte(static_cast<uint8_t>(index), line);
}

}  // namespace phos::vm
