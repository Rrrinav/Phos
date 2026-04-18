#include "compiler.hpp"

#include <cstdlib>
#include <iostream>
#include <print>

namespace phos::vm {

Compiler::Compiler(const ast::Ast_tree &tree, const Type_environment &env, mem::Arena &arena_) : tree(tree), env(env), arena(arena_)
{
    // Initialize the primary code block for the main script
    blocks_.push_back(Code_block{});
}

Code_block &Compiler::current_block()
{
    return blocks_.back();
}

Closure_data Compiler::compile(const std::vector<ast::Stmt_id> &statements)
{
    // Pass 1: Forward Declarations (Hoisting)
    // We scan the top-level statements for functions, reserve a dummy slot
    // in the main block's constant pool, and store that index.
    for (auto stmt_id : statements) {
        if (stmt_id.is_null()) {
            continue;
        }

        const auto &node = tree.get(stmt_id).node;
        if (std::holds_alternative<ast::Function_stmt>(node)) {
            const auto &fn_stmt = std::get<ast::Function_stmt>(node);
            uint16_t reserved_idx = add_constant(Value(nullptr));
            function_locations_[fn_stmt.name] = reserved_idx;
        }
    }

    // Pass 2: Bytecode Generation
    // Now we walk the AST and generate instructions.
    for (auto stmt_id : statements) {
        compile_stmt(stmt_id);
    }

    // Graceful halt for the main script
    emit(vm::Instruction::make_rrr(vm::Opcode::Return, 0, 0, 0));

    // Extract the completed main script block and package it into Arena memory
    Closure_data data{};
    auto &final_block = current_block();

    data.code_count = final_block.instructions.size();
    if (data.code_count > 0) {
        data.code = arena.allocate<vm::Instruction>(data.code_count);
        std::copy(final_block.instructions.begin(), final_block.instructions.end(), data.code);
    }

    data.constant_count = final_block.constants.size();
    if (data.constant_count > 0) {
        data.constants = arena.allocate<Value>(data.constant_count);
        std::copy(final_block.constants.begin(), final_block.constants.end(), data.constants);
    }

    return data;
}

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
    // Resets temporary scratch space. Lexical locals will require a more
    // advanced register allocator in the future.
    current_register_ = 0;
}

size_t Compiler::emit(vm::Instruction inst)
{
    return current_block().emit(inst);
}

uint16_t Compiler::add_constant(Value val)
{
    return current_block().add_constant(val);
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
            } else if constexpr (std::is_same_v<T, ast::Block_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::If_stmt>) {
                compile_stmt_node(s);
            } else {
                std::println(std::cerr, "Unimplemented stmt node");
            }
        },
        tree.get(stmt_id).node);
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
            } else {
                std::println("Unimplemented expression node");
            }
            return 0;
        },
        tree.get(expr_id).node);
}

void Compiler::compile_stmt_node(const ast::Print_stmt &stmt)
{
    uint8_t stream_flag = (stmt.stream == ast::Print_stream::STDERR) ? 1 : 0;
    uint8_t sep_reg = 0;
    bool has_sep = !stmt.sep.empty() && stmt.expressions.size() > 1;

    if (has_sep) {
        sep_reg = allocate_register();
        uint16_t sep_idx = add_constant(Value::make_string(arena, stmt.sep));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, sep_reg, sep_idx));
    }

    for (size_t i = 0; i < stmt.expressions.size(); ++i) {
        if (i > 0 && has_sep) {
            emit(vm::Instruction::make_rrr(vm::Opcode::Print, sep_reg, stream_flag, 0));
        }

        uint8_t result_reg = compile_expr(stmt.expressions[i]);
        emit(vm::Instruction::make_rrr(vm::Opcode::Print, result_reg, stream_flag, 0));
    }

    if (!stmt.end.empty()) {
        uint8_t end_reg = allocate_register();
        uint16_t end_idx = add_constant(Value::make_string(arena, stmt.end));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, end_reg, end_idx));
        emit(vm::Instruction::make_rrr(vm::Opcode::Print, end_reg, stream_flag, 0));
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
        target_reg = compile_expr(stmt.initializer);
    } else {
        target_reg = allocate_register();
    }

    locals_.push_back({stmt.name, target_reg});
}

void Compiler::compile_stmt_node(const ast::Block_stmt &stmt)
{
    // Save current scope boundary
    auto prev_locals_size = this->locals_.size();

    for (const auto &st : stmt.statements) {
        compile_stmt(st);
    }

    // Discard locals created inside this block
    this->locals_.resize(prev_locals_size);
}

void Compiler::compile_stmt_node(const ast::If_stmt &stmt)
{
    uint8_t cond_reg = compile_expr(stmt.condition);

    // Conditional jump over the 'then' block.
    // We emit 0 as the offset and store the instruction index to patch later.
    size_t jump_if_false_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, cond_reg, 0));

    if (!stmt.then_branch.is_null()) {
        compile_stmt(stmt.then_branch);
    }

    if (!stmt.else_branch.is_null()) {
        // Unconditional jump over the 'else' block using the new I format (24-bit).
        size_t jump_skip_else_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

        // Patch the false jump to land exactly at the start of the else block
        uint16_t else_start = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_if_false_idx].ri.imm = else_start;

        compile_stmt(stmt.else_branch);

        // Patch the unconditional skip to land exactly at the end of the chain
        uint32_t skip_target = static_cast<uint32_t>(current_block().instructions.size());
        current_block().instructions[jump_skip_else_idx].i.imm = skip_target;

    } else {
        // No else block, so the false jump just lands at the end of the 'then' block
        uint16_t end_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_if_false_idx].ri.imm = end_target;
    }
}

uint8_t Compiler::compile_expr_node(const ast::Variable_expr &expr)
{
    // Search backward to find the physical register for the nearest shadowed variable
    for (auto it = locals_.rbegin(); it != locals_.rend(); ++it) {
        if (it->name == expr.name) {
            return it->reg;
        }
    }

    // If it's not a local, check if it's a hoisted global function
    if (function_locations_.contains(expr.name)) {
        uint8_t target_reg = allocate_register();
        uint16_t const_idx = function_locations_[expr.name];
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));
        return target_reg;
    }

    std::println(std::cerr, "Compiler Bug: Unresolved variable '{}' escaped semantic checker.", expr.name);
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
    uint8_t reg_a = compile_expr(expr.left);
    uint8_t reg_b = compile_expr(expr.right);

    uint8_t dest_reg = allocate_register();
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
    case lex::TokenType::Equal:
        opcode = is_float ? vm::Opcode::Eq_f64 : vm::Opcode::Eq_i64;
        break;
    case lex::TokenType::NotEqual:
        opcode = is_float ? vm::Opcode::Neq_f64 : vm::Opcode::Neq_i64;
        break;
    case lex::TokenType::Less:
        opcode = is_float ? vm::Opcode::Lt_f64 : vm::Opcode::Lt_i64;
        break;
    case lex::TokenType::LessEqual:
        opcode = is_float ? vm::Opcode::Lte_f64 : vm::Opcode::Lte_i64;
        break;
    case lex::TokenType::Greater:
        opcode = is_float ? vm::Opcode::Gt_f64 : vm::Opcode::Gt_i64;
        break;
    case lex::TokenType::GreaterEqual:
        opcode = is_float ? vm::Opcode::Gte_f64 : vm::Opcode::Gte_i64;
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
