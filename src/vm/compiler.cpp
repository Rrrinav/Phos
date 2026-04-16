#include "compiler.hpp"

#include <cstdlib>
#include <print>
#include <iostream>

namespace phos::vm {

Compiler::Compiler(const ast::Ast_tree &tree, const Type_environment &env, mem::Arena& arena_) : tree(tree), env(env), arena(arena_)
{}

Closure_data Compiler::compile(const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {
        compile_stmt(stmt_id);
    }

    // Every script must gracefully halt!
    emit(vm::Instruction::make_rrr(vm::Opcode::Return, 0, 0, 0));

    // Note: For a production VM, you would copy these to heap/arena memory.
    // For now, we rely on the caller to keep the Compiler object alive
    // while the VM executes, or change Closure_data to own the vectors.
    Closure_data data{};
    data.code_count = code_.size();
    if (data.code_count > 0) {
        data.code = arena.allocate<vm::Instruction>(data.code_count);
        std::copy(code_.begin(), code_.end(), data.code);
    }

    data.constant_count = constants_.size();
    if (data.constant_count > 0) {
        data.constants = arena.allocate<Value>(data.constant_count);
        std::copy(constants_.begin(), constants_.end(), data.constants);
    }

    return data;
}

// UTILS

uint8_t Compiler::allocate_register()
{
    if (current_register_ >= 255) {
        std::println(std::cerr, "Expression too complex! Exceeded 256 temporary registers.");
        std::exit(EXIT_FAILURE);
    }
    return current_register_++;
}

void Compiler::reset_registers()
{
    // Since we have no local variables yet, every register used in a
    // statement was just temporary scratch space. Nuke them!
    current_register_ = 0;
}

void Compiler::emit(vm::Instruction inst)
{
    code_.push_back(inst);
}

uint16_t Compiler::add_constant(Value val)
{
    constants_.push_back(val);
    return static_cast<uint16_t>(constants_.size() - 1);
}
void Compiler::compile_stmt(ast::Stmt_id stmt_id)
{
    if (stmt_id.is_null()) {
        return;
    }

    std::visit(
        [this](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, ast::Print_stmt>) {
                compile_stmt_node(s);

            } else if constexpr (std::is_same_v<T, ast::Expr_stmt>) {
                compile_stmt_node(s);

            } else if constexpr (std::is_same_v<T, ast::Var_stmt>) {
                compile_stmt_node(s);
            // Added the 'else' so it only prints if nothing matched!
            } else {
                std::println(std::cerr, "Unimplemented stmt node");
            }
        },
        tree.get(stmt_id).node
    );
}

uint8_t Compiler::compile_expr(ast::Expr_id expr_id)
{
    if (expr_id.is_null()) {
        return 0;
    }

    return std::visit(
        [this](const auto &e) -> uint8_t {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, ast::Literal_expr>) {
                return compile_expr_node(e);

            } else if constexpr (std::is_same_v<T, ast::Binary_expr>) {
                return compile_expr_node(e);

            } else if constexpr (std::is_same_v<T, ast::Variable_expr>) {
                return compile_expr_node(e);

            }

            std::println("Unimplemented expression node");

            return 0; // Fallback
        },
        tree.get(expr_id).node
    );
}

void Compiler::compile_stmt_node(const ast::Print_stmt &stmt)
{
    for (auto expr_id : stmt.expressions) {
        // 1. Run the math
        uint8_t result_reg = compile_expr(expr_id);

        // 2. Print the result
        // Note: Make sure Opcode::Print is in your instruction.hpp and vm.cpp loop!
        emit(vm::Instruction::make_rrr(vm::Opcode::Print, result_reg, 0, 0));
    }
}

void Compiler::compile_stmt_node(const ast::Expr_stmt &stmt)
{
    compile_expr(stmt.expression);
}

void Compiler::compile_stmt_node(const ast::Var_stmt &stmt)
{
    uint8_t target_reg = 0;

    if (!stmt.initializer.is_null()) {
        // Compile the math/literal, and remember which register it ended up in
        target_reg = compile_expr(stmt.initializer);
    } else {
        // Claim a fresh register for uninitialized variables
        target_reg = allocate_register();
    }

    locals_.push_back({stmt.name, target_reg});
}

// EXPRESSIONS

uint8_t Compiler::compile_expr_node(const ast::Variable_expr &expr)
{
    // Search backwards to find the physical register for this variable name
    for (auto it = locals_.rbegin(); it != locals_.rend(); ++it) {
        if (it->name == expr.name) {
            return it->reg;
        }
    }

    std::cerr << "Compiler Bug: Unresolved variable escaped semantic checker.";
    std::exit(1);
    return 0;
}

uint8_t Compiler::compile_expr_node(const ast::Literal_expr &expr)
{
    uint8_t target_reg = allocate_register();
    uint16_t const_idx = add_constant(expr.value);

    emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));
    return target_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Binary_expr &expr)
{
    // Post-order traversal: Children first
    uint8_t reg_a = compile_expr(expr.left);
    uint8_t reg_b = compile_expr(expr.right);

    uint8_t dest_reg = allocate_register();

    // Ask the type environment what math we are actually doing
    bool is_float = env.tt.is_float_primitive(expr.type);

    vm::Opcode opcode;
    switch (expr.op) {
    case lex::TokenType::Plus:
        opcode = is_float ? vm::Opcode::Add_f64 : vm::Opcode::Add_i64;
        break;
    case lex::TokenType::Minus:
        opcode = is_float ? vm::Opcode::Sub_f64 : vm::Opcode::Sub_i64;
        break;
    case lex::TokenType::Star:
        opcode = is_float ? vm::Opcode::Mul_f64 : vm::Opcode::Mul_i64;
        break;
    case lex::TokenType::Slash:
        opcode = is_float ? vm::Opcode::Div_f64 : vm::Opcode::Div_i64;
        break;
    case lex::TokenType::Percent:
        opcode = is_float ? vm::Opcode::Mod_f64 : vm::Opcode::Mod_i64;
        break;
    default: {
        std::println(std::cerr, "Unsupported binary operator in compiler");
        std::exit(EXIT_FAILURE);
    }
    }

    emit(vm::Instruction::make_rrr(opcode, dest_reg, reg_a, reg_b));
    return dest_reg;
}

} // namespace phos::vm
