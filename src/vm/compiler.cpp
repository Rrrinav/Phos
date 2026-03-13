#include "compiler.hpp"
#include <print>
#include "opcodes.hpp"

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
    else if (auto *node = std::get_if<ast::Var_stmt>(&stmt->node))
        visit_var_stmt(*node);
    else if (auto *node = std::get_if<ast::Block_stmt>(&stmt->node))
        visit_block_stmt(*node);
    else if (auto *node = std::get_if<ast::If_stmt>(&stmt->node))
        visit_if_stmt(*node);
    else if (auto *node = std::get_if<ast::While_stmt>(&stmt->node))
        visit_while_stmt(*node);
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
    else if (auto *node = std::get_if<ast::Unary_expr>(&expr->node))
        visit_unary_expr(*node);
    else if (auto *node = std::get_if<ast::Variable_expr>(&expr->node))
        visit_variable_expr(*node);
    else if (auto *node = std::get_if<ast::Assignment_expr>(&expr->node))
        visit_assignment_expr(*node);
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
        case lex::TokenType::Plus:         emit_op(Op_code::Add, expr.loc); break;
        case lex::TokenType::Minus:        emit_op(Op_code::Subtract, expr.loc); break;
        case lex::TokenType::Star:         emit_op(Op_code::Multiply, expr.loc); break;
        case lex::TokenType::Slash:        emit_op(Op_code::Divide, expr.loc); break;
        case lex::TokenType::Percent:      emit_op(Op_code::Modulo, expr.loc); break;

        case lex::TokenType::Equal:        emit_op(Op_code::Equal, expr.loc); break;
        case lex::TokenType::NotEqual:     emit_op(Op_code::Not_equal, expr.loc); break;
        case lex::TokenType::Less:         emit_op(Op_code::Less, expr.loc); break;
        case lex::TokenType::LessEqual:    emit_op(Op_code::Less_equal, expr.loc); break;
        case lex::TokenType::Greater:      emit_op(Op_code::Greater, expr.loc); break;
        case lex::TokenType::GreaterEqual: emit_op(Op_code::Greater_equal, expr.loc); break;

        case lex::TokenType::BitAnd:       emit_op(Op_code::BitAnd, expr.loc); break;
        case lex::TokenType::Pipe:         emit_op(Op_code::BitOr, expr.loc); break;
        case lex::TokenType::BitXor:       emit_op(Op_code::BitXor, expr.loc); break;
        case lex::TokenType::BitRshift:    emit_op(Op_code::BitRShift, expr.loc); break;
        case lex::TokenType::BitLShift:    emit_op(Op_code::BitLShift, expr.loc); break;
        default:
            std::println(stderr, "Binary op code: {} not supported by compiler.", lex::token_to_string(expr.op));
            break;
    }
}

void Compiler::visit_unary_expr(const ast::Unary_expr &expr)
{
    compile_expr(expr.right);
    switch (expr.op) {
        case lex::TokenType::LogicalNot: {
            emit_op(Op_code::Not, expr.loc);
        } break;

        case lex::TokenType::BitNot: {
            emit_op(Op_code::BitNot, expr.loc);
        } break;

        case lex::TokenType::Minus: {
            emit_op(Op_code::Negate, expr.loc);
        } break;

        default: break;
    }
}

void Compiler::visit_var_stmt(const ast::Var_stmt &stmt)
{
    // 1. Compile the initializer if it exists.
    if (stmt.initializer)
    {
        compile_expr(stmt.initializer);
    }
    else
    {
        if (auto *prim = std::get_if<types::Primitive_kind>(&stmt.type))
        {
            switch (*prim)
            {
                case types::Primitive_kind::Int:
                    emit_constant(Value(static_cast<int64_t>(0)), stmt.loc);
                    break;
                case types::Primitive_kind::Float:
                    emit_constant(Value(0.0), stmt.loc);
                    break;
                case types::Primitive_kind::Bool:
                    emit_op(Op_code::False, stmt.loc);
                    break;
                case types::Primitive_kind::String:
                    emit_constant(Value(std::string("")), stmt.loc);
                    break;
                default:
                    emit_op(Op_code::Nil, stmt.loc);
                    break;
            }
        }
        else
        {
            // For complex types (Arrays, Models, Unions) or Optionals,
            // nil might still be the correct memory state until they are instantiated.
            emit_op(Op_code::Nil, stmt.loc);
        }
    }

    // 3. Get the string index and emit the definition opcode
    uint8_t name_idx = identifier_constant(stmt.name, stmt.loc);
    emit_op(Op_code::Define_global, stmt.loc);
    emit_byte(name_idx, stmt.loc);
}

void Compiler::visit_variable_expr(const ast::Variable_expr &expr)
{
    uint8_t name_idx = identifier_constant(expr.name, expr.loc);
    emit_op(Op_code::Get_global, expr.loc);
    emit_byte(name_idx, expr.loc);
}

void Compiler::visit_assignment_expr(const ast::Assignment_expr &expr)
{
    // 1. Compile the right-hand side of the assignment (leaves value on stack)
    compile_expr(expr.value);

    // 2. Emit the set opcode
    uint8_t name_idx = identifier_constant(expr.name, expr.loc);
    emit_op(Op_code::Set_global, expr.loc);
    emit_byte(name_idx, expr.loc);
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

uint8_t Compiler::identifier_constant(const std::string& name, phos::ast::Source_location loc)
{
    size_t index = current_chunk.add_constant(Value(name));
    if (index > 255)
    {
        std::println(stderr, "Too many globals/constants in chunk at line {}", loc.l);
        std::abort();
    }
    return static_cast<uint8_t>(index);
}

void Compiler::visit_block_stmt(const ast::Block_stmt &stmt)
{
    for (auto *s : stmt.statements) compile_stmt(s);
}

void Compiler::visit_if_stmt(const ast::If_stmt &stmt)
{
    compile_expr(stmt.condition);

    size_t then_jump = emit_jump(Op_code::Jump_if_false, stmt.loc);
    emit_op(Op_code::Pop, stmt.loc);  // Pop condition if it was true

    compile_stmt(stmt.then_branch);
    size_t else_jump = emit_jump(Op_code::Jump, stmt.loc);

    patch_jump(then_jump, stmt.loc);
    emit_op(Op_code::Pop, stmt.loc);  // Pop condition if it was false

    if (stmt.else_branch)
        compile_stmt(stmt.else_branch);

    patch_jump(else_jump, stmt.loc);
}

void Compiler::visit_while_stmt(const ast::While_stmt &stmt)
{
    size_t loop_start = current_chunk.code.size();

    compile_expr(stmt.condition);

    size_t exit_jump = emit_jump(Op_code::Jump_if_false, stmt.loc);
    emit_op(Op_code::Pop, stmt.loc);  // Pop condition if true

    compile_stmt(stmt.body);
    emit_loop(loop_start, stmt.loc);

    patch_jump(exit_jump, stmt.loc);
    emit_op(Op_code::Pop, stmt.loc);  // Pop condition when exiting loop
}

size_t Compiler::emit_jump(Op_code instruction, phos::ast::Source_location loc)
{
    emit_op(instruction, loc);
    emit_byte(0xff, loc);
    emit_byte(0xff, loc);
    return current_chunk.code.size() - 2;
}

void Compiler::patch_jump(size_t offset, phos::ast::Source_location loc)
{
    size_t jump = current_chunk.code.size() - offset - 2;
    if (jump > UINT16_MAX)
    {
        std::println(stderr, "Too much code to jump over at line {}", loc.l);
        std::abort();
    }
    current_chunk.code[offset] = (jump >> 8) & 0xff;
    current_chunk.code[offset + 1] = jump & 0xff;
}

void Compiler::emit_loop(size_t loop_start, phos::ast::Source_location loc)
{
    emit_op(Op_code::Loop, loc);
    size_t jump = current_chunk.code.size() - loop_start + 2;
    if (jump > UINT16_MAX)
    {
        std::println(stderr, "Loop body too large at line {}", loc.l);
        std::abort();
    }
    emit_byte((jump >> 8) & 0xff, loc);
    emit_byte(jump & 0xff, loc);
}

}  // namespace phos::vm
