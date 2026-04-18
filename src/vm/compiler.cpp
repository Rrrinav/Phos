#include "compiler.hpp"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <print>

namespace phos::vm {

Compiler::Compiler(const ast::Ast_tree &tree, const Type_environment &env, mem::Arena &arena_) : tree(tree), env(env), arena(arena_)
{
    blocks_.push_back(Code_block{});
}

Code_block &Compiler::current_block()
{
    return blocks_.back();
}

/*
    Main Compilation Flow (Two-Pass Architecture):

    Pass 1: Hoisting
        For each top-level statement:
            If it's a function:
                Reserve an index in the constant pool.
                Save mapping: function_locations_[name] = index.

    Pass 2: Bytecode Generation
        For each top-level statement:
            compile_stmt(stmt)

    Packaging:
        Emit graceful Return instruction.
        Allocate final instructions and constants into the memory Arena.
*/
Closure_data Compiler::compile(const std::vector<ast::Stmt_id> &statements)
{
    // Pass 1: Forward Declarations (Hoisting)
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
    for (auto stmt_id : statements) {
        compile_stmt(stmt_id);
    }

    emit(vm::Instruction::make_rrr(vm::Opcode::Return, 0, 0, 0));

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

/*
    Virtual Register Allocation:
    Claims the next available 8-bit register for temporary use.
*/
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
            } else if constexpr (std::is_same_v<T, ast::While_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::For_stmt>) {
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

/*
    if custom separator:
        %sep = load_const "separator_string"
    for each expression 'e':
        if not first expression:
            print %sep, stream
        %res = compile(e)
        print %res, stream
    if custom end (e.g., \n):
        %end = load_const "end_string"
        print %end, stream
*/
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

/*
    if has initializer:
        %target = compile(initializer)
    else:
        %target = allocate_register()
    locals.push(name, %target)
*/
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

/*
    Save current locals count
    compile(statements inside block)
    Restore locals count (destroys block-scoped variables)
*/
void Compiler::compile_stmt_node(const ast::Block_stmt &stmt)
{
    auto prev_locals_size = this->locals_.size();

    for (const auto &st : stmt.statements) {
        compile_stmt(st);
    }

    this->locals_.resize(prev_locals_size);
}

/*
    If-Else Pseudo-IR:
        %cond = compile(condition)
        jump_if_false %cond, .else_branch
    .then_branch:
        compile(then_body)
        jump .if_end
    .else_branch:
        compile(else_body)
    .if_end:
*/
void Compiler::compile_stmt_node(const ast::If_stmt &stmt)
{
    uint8_t cond_reg = compile_expr(stmt.condition);

    size_t jump_if_false_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, cond_reg, 0));

    if (!stmt.then_branch.is_null()) {
        compile_stmt(stmt.then_branch);
    }

    if (!stmt.else_branch.is_null()) {
        size_t jump_skip_else_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

        uint16_t else_start = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_if_false_idx].ri.imm = else_start;

        compile_stmt(stmt.else_branch);

        uint32_t skip_target = static_cast<uint32_t>(current_block().instructions.size());
        current_block().instructions[jump_skip_else_idx].i.imm = skip_target;

    } else {
        uint16_t end_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_if_false_idx].ri.imm = end_target;
    }
}

/*
    .loop_start:
        %cond = compile(condition)
        jump_if_false %cond, .loop_end
    .loop_body:
        compile(body)
        jump .loop_start
    .loop_end:
*/
void Compiler::compile_stmt_node(const ast::While_stmt &stmt)
{
    std::uint32_t loop_start = static_cast<uint32_t>(current_block().instructions.size());

    std::uint8_t condition_reg = compile_expr(stmt.condition);

    size_t exit_jump_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, condition_reg, 0));

    compile_stmt(stmt.body);

    emit(vm::Instruction::make_i(vm::Opcode::Jump, loop_start));

    uint16_t exit_target = static_cast<uint16_t>(current_block().instructions.size());
    current_block().instructions[exit_jump_idx].ri.imm = exit_target;
}

/*
    .loop_init:
        compile(initializer)
    .loop_start:
        %cond = compile(condition)
        jump_if_false %cond, .loop_end
    .loop_body:
        compile(body)
    .loop_step:
        compile(increment)
        jump .loop_start
    .loop_end:
*/
void Compiler::compile_stmt_node(const ast::For_stmt &stmt)
{
    auto prev_locals_size = this->locals_.size();

    if (!stmt.initializer.is_null()) {
        compile_stmt(stmt.initializer);
    }

    uint32_t loop_start = static_cast<uint32_t>(current_block().instructions.size());

    size_t exit_jump_idx = -1;
    if (!stmt.condition.is_null()) {
        uint8_t cond_reg = compile_expr(stmt.condition);
        exit_jump_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, cond_reg, 0));
    }

    compile_stmt(stmt.body);

    if (!stmt.increment.is_null()) {
        compile_expr(stmt.increment);
    }

    emit(vm::Instruction::make_i(vm::Opcode::Jump, loop_start));

    if (exit_jump_idx != -1) {
        uint16_t exit_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[exit_jump_idx].ri.imm = exit_target;
    }

    this->locals_.resize(prev_locals_size);
}

/*
    Search backwards in locals (supports shadowing).
    If found: return physical register assigned.
    If not local: Check if it's a hoisted global function.
    If hoisted function:
        %dest = allocate_register()
        load_const %dest, $K_function_ptr
*/
uint8_t Compiler::compile_expr_node(const ast::Variable_expr &expr)
{
    for (auto it = locals_.rbegin(); it != locals_.rend(); ++it) {
        if (it->name == expr.name) {
            return it->reg;
        }
    }

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

/*
    $K = add_constant(value)
    %dest = allocate_register()
    load_const %dest, $K
*/
uint8_t Compiler::compile_expr_node(const ast::Literal_expr &expr)
{
    uint8_t target_reg = allocate_register();
    uint16_t const_idx = add_constant(expr.value);

    emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));
    return target_reg;
}

/*
    Binary Expression Dispatcher
    Contains Short-Circuit execution flows for Logical AND/OR,
    followed by standard post-order generation for math operators.
*/
uint8_t Compiler::compile_expr_node(const ast::Binary_expr &expr)
{
    /*
            %left = compile(expr.left)
            %dest = move %left
            jump_if_false %left, .short_circuit_end
        .eval_right:
            %right = compile(expr.right)
            %dest = move %right
        .short_circuit_end:
    */
    if (expr.op == lex::TokenType::LogicalAnd) {
        uint8_t reg_a = compile_expr(expr.left);
        uint8_t dest_reg = allocate_register();

        emit(vm::Instruction::make_rrr(vm::Opcode::Move, dest_reg, reg_a, 0));

        size_t jump_end_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, reg_a, 0));

        uint8_t reg_b = compile_expr(expr.right);
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, dest_reg, reg_b, 0));

        uint16_t end_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_end_idx].ri.imm = end_target;

        return dest_reg;
    }

    /*
            %left = compile(expr.left)
            %dest = move %left
            jump_if_false %left, .eval_right
        .short_circuit_true:
            jump .short_circuit_end
        .eval_right:
            %right = compile(expr.right)
            %dest = move %right
        .short_circuit_end:
    */
    if (expr.op == lex::TokenType::LogicalOr) {
        uint8_t reg_a = compile_expr(expr.left);
        uint8_t dest_reg = allocate_register();

        emit(vm::Instruction::make_rrr(vm::Opcode::Move, dest_reg, reg_a, 0));

        size_t jump_eval_b_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, reg_a, 0));

        size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

        uint16_t eval_b_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_eval_b_idx].ri.imm = eval_b_target;

        uint8_t reg_b = compile_expr(expr.right);
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, dest_reg, reg_b, 0));

        uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
        current_block().instructions[jump_end_idx].i.imm = end_target;

        return dest_reg;
    }

    /*
        %left = compile(left)
        %right = compile(right)
        %dest = allocate_register()
        [opcode] %dest, %left, %right
    */
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
