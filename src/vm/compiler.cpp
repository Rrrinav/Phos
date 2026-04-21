#include "compiler.hpp"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <print>

namespace phos::vm {

Compiler::Compiler(const ast::Ast_tree &tree, const Type_environment &env, mem::Arena &arena_) : tree(tree), env(env), arena(arena_)
{
}

// CONTEXT & LEXICAL SCOPING ENGINE
void Compiler::push_context(Compiler_context *ctx)
{
    ctx->enclosing = current_ctx_;
    current_ctx_ = ctx;
}

void Compiler::pop_context()
{
    current_ctx_ = current_ctx_->enclosing;
}

int Compiler::resolve_local(Compiler_context *ctx, const std::string &name)
{
    // Search backwards so we find the most recently declared variable with this name (shadowing)
    for (int i = static_cast<int>(ctx->locals.size()) - 1; i >= 0; i--) {
        if (ctx->locals[i].name == name) {
            return ctx->locals[i].reg;
        }
    }
    return -1; // Not found locally
}

int Compiler::add_upvalue(Compiler_context *ctx, uint8_t index, bool is_local)
{
    // Check if we already captured this exact variable! (Prevents duplicate captures)
    for (size_t i = 0; i < ctx->upvalues.size(); i++) {
        if (ctx->upvalues[i].index == index && ctx->upvalues[i].is_local == is_local) {
            return static_cast<int>(i);
        }
    }

    ctx->upvalues.push_back({index, is_local});
    return static_cast<int>(ctx->upvalues.size() - 1);
}

int Compiler::resolve_upvalue(Compiler_context *ctx, const std::string &name)
{
    if (ctx->enclosing == nullptr) {
        return -1; // We hit the global scope. It's not an upvalue.
    }

    // 1. Does the immediate parent have it as a local?
    int local_index = resolve_local(ctx->enclosing, name);
    if (local_index != -1) {
        return add_upvalue(ctx, static_cast<uint8_t>(local_index), true);
    }

    // 2. If not, does the grandparent have it? (Recursion!)
    int upvalue_index = resolve_upvalue(ctx->enclosing, name);
    if (upvalue_index != -1) {
        // The parent captured it, so we pass it down to ourselves as a non-local upvalue
        return add_upvalue(ctx, static_cast<uint8_t>(upvalue_index), false);
    }

    return -1;
}

// =============================================================================
// MAIN COMPILATION FLOW
// =============================================================================

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
    // Push the global execution context
    Compiler_context global_ctx;
    global_ctx.enclosing = nullptr;
    global_ctx.current_register = 0;
    push_context(&global_ctx);

    // Hoisting Natives
    for (const auto &[name, sigs] : env.native_signatures) {
        if (sigs.empty() || sigs[0].func == nullptr) {
            continue;
        }

        Closure_data *closure = arena.allocate<Closure_data>();
        new (closure) Closure_data();

        size_t name_size = sizeof(String_data) + name.length() + 1;
        closure->name = static_cast<String_data *>(arena.allocate_bytes(name_size));
        closure->name->length = name.length();
        std::copy(name.begin(), name.end(), closure->name->chars);
        closure->name->chars[name.length()] = '\0';

        closure->arity = sigs[0].params.size();
        closure->native_func = sigs[0].func;

        // Native functions don't have bytecode or constants!
        closure->code_count = 0;
        closure->code = nullptr;
        closure->constant_count = 0;
        closure->constants = nullptr;

        uint16_t const_idx = add_constant(Value::make_closure(closure));
        function_locations_[name] = const_idx;
    }

    // Hoisting User Functions
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

    pop_context();
    return data;
}

// =============================================================================
// BYTECODE & REGISTER HELPERS
// =============================================================================

/*
    Virtual Register Allocation:
    Claims the next available 8-bit register for temporary use in the current context.
*/
uint8_t Compiler::allocate_register()
{
    if (current_ctx_->current_register >= 255) {
        std::println(std::cerr, "Expression too complex! Exceeded 256 temporary registers.");
        std::exit(EXIT_FAILURE);
    }
    return current_ctx_->current_register++;
}

void Compiler::reset_registers()
{
    current_ctx_->current_register = 0;
}

size_t Compiler::emit(vm::Instruction inst)
{
    return current_block().emit(inst);
}

uint16_t Compiler::add_constant(Value val)
{
    return current_block().add_constant(val);
}

void Compiler::emit_numeric_normalize(uint8_t reg, types::Type_id type)
{
    if (!env.tt.is_numeric_primitive(type)) {
        return;
    }

    auto kind = env.tt.get_primitive(type);
    if (kind == types::Primitive_kind::I64 || kind == types::Primitive_kind::U64 || kind == types::Primitive_kind::F64) {
        return;
    }

    emit(vm::Instruction::make_rrr(cast_opcode_for(type), reg, 0, 0));
}

vm::Opcode Compiler::cast_opcode_for(types::Type_id type) const
{
    switch (env.tt.get_primitive(type)) {
    case types::Primitive_kind::I8:
        return vm::Opcode::Cast_i8;
    case types::Primitive_kind::I16:
        return vm::Opcode::Cast_i16;
    case types::Primitive_kind::I32:
        return vm::Opcode::Cast_i32;
    case types::Primitive_kind::I64:
        return vm::Opcode::Cast_i64;
    case types::Primitive_kind::U8:
        return vm::Opcode::Cast_u8;
    case types::Primitive_kind::U16:
        return vm::Opcode::Cast_u16;
    case types::Primitive_kind::U32:
        return vm::Opcode::Cast_u32;
    case types::Primitive_kind::U64:
        return vm::Opcode::Cast_u64;
    case types::Primitive_kind::F16:
        return vm::Opcode::Cast_f16;
    case types::Primitive_kind::F32:
        return vm::Opcode::Cast_f32;
    case types::Primitive_kind::F64:
        return vm::Opcode::Cast_f64;
    default:
        std::println(std::cerr, "Compiler Bug: No cast opcode for type '{}'.", env.tt.to_string(type));
        std::exit(EXIT_FAILURE);
    }
}

vm::Opcode Compiler::arithmetic_opcode_for(lex::TokenType op, types::Type_id type) const
{
    bool is_float = env.tt.is_float_primitive(type);
    bool is_unsigned = env.tt.is_unsigned_integer_primitive(type);

    switch (op) {
    case lex::TokenType::Plus:
        if (is_float) {
            return vm::Opcode::Add_f64;
        }
        return is_unsigned ? vm::Opcode::Add_u64 : vm::Opcode::Add_i64;
    case lex::TokenType::Minus:
        if (is_float) {
            return vm::Opcode::Sub_f64;
        }
        return is_unsigned ? vm::Opcode::Sub_u64 : vm::Opcode::Sub_i64;
    case lex::TokenType::Star:
        if (is_float) {
            return vm::Opcode::Mul_f64;
        }
        return is_unsigned ? vm::Opcode::Mul_u64 : vm::Opcode::Mul_i64;
    case lex::TokenType::Slash:
        if (is_float) {
            return vm::Opcode::Div_f64;
        }
        return is_unsigned ? vm::Opcode::Div_u64 : vm::Opcode::Div_i64;
    case lex::TokenType::Percent:
        if (is_float) {
            return vm::Opcode::Mod_f64;
        }
        return is_unsigned ? vm::Opcode::Mod_u64 : vm::Opcode::Mod_i64;
    default:
        std::println(std::cerr, "Compiler Bug: Unsupported arithmetic operator.");
        std::exit(EXIT_FAILURE);
    }
}

vm::Opcode Compiler::comparison_opcode_for(lex::TokenType op, types::Type_id left_type, types::Type_id right_type) const
{
    bool is_float = env.tt.is_float_primitive(left_type) || env.tt.is_float_primitive(right_type);
    bool is_unsigned = env.tt.is_unsigned_integer_primitive(left_type) && env.tt.is_unsigned_integer_primitive(right_type);

    switch (op) {
    case lex::TokenType::Equal:
        if (is_float) {
            return vm::Opcode::Eq_f64;
        }
        return is_unsigned ? vm::Opcode::Eq_u64 : vm::Opcode::Eq_i64;
    case lex::TokenType::NotEqual:
        if (is_float) {
            return vm::Opcode::Neq_f64;
        }
        return is_unsigned ? vm::Opcode::Neq_u64 : vm::Opcode::Neq_i64;
    case lex::TokenType::Less:
        if (is_float) {
            return vm::Opcode::Lt_f64;
        }
        return is_unsigned ? vm::Opcode::Lt_u64 : vm::Opcode::Lt_i64;
    case lex::TokenType::LessEqual:
        if (is_float) {
            return vm::Opcode::Lte_f64;
        }
        return is_unsigned ? vm::Opcode::Lte_u64 : vm::Opcode::Lte_i64;
    case lex::TokenType::Greater:
        if (is_float) {
            return vm::Opcode::Gt_f64;
        }
        return is_unsigned ? vm::Opcode::Gt_u64 : vm::Opcode::Gt_i64;
    case lex::TokenType::GreaterEqual:
        if (is_float) {
            return vm::Opcode::Gte_f64;
        }
        return is_unsigned ? vm::Opcode::Gte_u64 : vm::Opcode::Gte_i64;
    default:
        std::println(std::cerr, "Compiler Bug: Unsupported comparison operator.");
        std::exit(EXIT_FAILURE);
    }
}

// =============================================================================
// STATEMENTS
// =============================================================================

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
            } else if constexpr (std::is_same_v<T, ast::Function_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Return_stmt>) {
                compile_stmt_node(s);
            } else {
                std::println(std::cerr, "Unimplemented stmt node");
            }
        },
        tree.get(stmt_id).node);
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
        types::Type_id source_type = ast::get_type(tree.get(stmt.initializer).node);
        if (source_type != stmt.type) {
            emit_numeric_normalize(target_reg, stmt.type);
        }
    } else {
        target_reg = allocate_register();
    }

    current_ctx_->locals.push_back({stmt.name, target_reg});
}

/*
    Save current locals count
    compile(statements inside block)
    Restore locals count (destroys block-scoped variables)
*/
void Compiler::compile_stmt_node(const ast::Block_stmt &stmt)
{
    auto prev_locals_size = current_ctx_->locals.size();

    for (const auto &st : stmt.statements) {
        compile_stmt(st);
    }

    current_ctx_->locals.resize(prev_locals_size);
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
    auto prev_locals_size = current_ctx_->locals.size();

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

    current_ctx_->locals.resize(prev_locals_size);
}

/*
    1. Push a new isolated environment (Compiler_context)
    2. Assign the first registers to the parameters
    3. Compile the function body
    4. Emit a failsafe Return (in case the user forgot a return statement)
    5. Package everything into a Closure_data using the Arena directly
    6. Pop the environment
    7. If it's a global function with no upvalues, update the hoisted Constant Pool entry.
    8. Otherwise, emit Make_closure and routing bytes, and assign to a local register.
*/
void Compiler::compile_stmt_node(const ast::Function_stmt &stmt)
{
    Compiler_context fn_ctx;
    fn_ctx.current_register = 0;
    push_context(&fn_ctx);

    // Register the params from r[0].....r[arity - 1]
    for (const auto &param : stmt.parameters) {
        uint8_t reg = allocate_register();
        current_ctx_->locals.push_back({param.name, reg});
    }

    // Body
    if (!stmt.body.is_null()) {
        compile_stmt(stmt.body);
    }

    // Emit a failsafe `return nil` at the very end
    uint8_t nil_reg = allocate_register();
    emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, nil_reg, 0, 0));
    emit(vm::Instruction::make_rrr(vm::Opcode::Return, nil_reg, 0, 0));

    // Package the compiled block into a Closure_data manually
    Closure_data *closure = arena.allocate<Closure_data>();
    new (closure) Closure_data();

    // Name
    size_t name_size = sizeof(String_data) + stmt.name.length() + 1;
    closure->name = static_cast<String_data *>(arena.allocate_bytes(name_size));
    closure->name->length = stmt.name.length();
    std::copy(stmt.name.begin(), stmt.name.end(), closure->name->chars);
    closure->name->chars[stmt.name.length()] = '\0';

    // Arity
    closure->arity = stmt.parameters.size();

    // Code block
    closure->code_count = current_ctx_->block.instructions.size();
    if (closure->code_count > 0) {
        closure->code = arena.allocate<vm::Instruction>(closure->code_count);
        std::copy(current_ctx_->block.instructions.begin(), current_ctx_->block.instructions.end(), closure->code);
    }

    // Constants
    closure->constant_count = current_ctx_->block.constants.size();
    if (closure->constant_count > 0) {
        closure->constants = arena.allocate<Value>(closure->constant_count);
        std::copy(current_ctx_->block.constants.begin(), current_ctx_->block.constants.end(), closure->constants);
    }

    // Upvalues signature
    closure->native_func = std::nullopt;
    closure->upvalue_count = current_ctx_->upvalues.size();
    closure->upvalues = nullptr;

    pop_context(); // Return to the parent's compiler context

    // If it's a global function and captures nothing, statically replace the hoisted placeholder.
    // Do NOT add a new constant!
    if (current_ctx_->enclosing == nullptr && closure->upvalue_count == 0) {
        uint16_t hoisted_idx = function_locations_[stmt.name];
        current_block().constants[hoisted_idx] = Value::make_closure(closure);
        return;
    }

    // Add the blueprint to the parent's constant pool
    uint16_t const_idx = add_constant(Value::make_closure(closure));

    // If it's a global function and captures nothing, it can statically replace the hoisted placeholder
    if (current_ctx_->enclosing == nullptr && closure->upvalue_count == 0) {
        uint16_t hoisted_idx = function_locations_[stmt.name];
        current_block().constants[hoisted_idx] = Value::make_closure(closure);
        return;
    }

    // Otherwise, it requires a runtime Closure!
    uint8_t dst_reg = allocate_register();
    emit(vm::Instruction::make_ri(vm::Opcode::Make_closure, dst_reg, const_idx));

    // Emit the dynamic upvalue routing bytes (Read by VM Make_closure loop)
    for (const auto &uv : fn_ctx.upvalues) {
        vm::Instruction route;
        route.rrr.src_a = uv.is_local ? 1 : 0;
        route.rrr.src_b = uv.index;
        route.rrr.dst = 0;
        emit(route);
    }

    // Bind this newly instantiated runtime closure to a local register!
    current_ctx_->locals.push_back({stmt.name, dst_reg});
}

void Compiler::compile_stmt_node(const ast::Return_stmt &stmt)
{
    uint8_t ret_reg = 0;

    if (!stmt.expression.is_null()) {
        ret_reg = compile_expr(stmt.expression);
    } else {
        ret_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, ret_reg, 0, 0));
    }

    emit(vm::Instruction::make_rrr(vm::Opcode::Return, ret_reg, 0, 0));
}

// =============================================================================
// EXPRESSIONS
// =============================================================================

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
            } else if constexpr (std::is_same_v<T, ast::Assignment_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Cast_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Closure_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Call_expr>) {
                return compile_expr_node(e);
                } else if constexpr (std::is_same_v<T, ast::Closure_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Array_literal_expr>) { // NEW
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Array_access_expr>) {  // NEW
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>) { // NEW
                return compile_expr_node(e);
            } else {
                auto loc = ast::get_loc(e);
                std::println("{}:{}: Unimplemented expression node", loc.l, loc.c);
            }
            return 0;
        },
        tree.get(expr_id).node);
}

/*
    Search backwards in locals (supports shadowing).
    If found: return physical register assigned.
    If not local: Check upvalues (captured variables).
    If upvalue:
        %dest = allocate_register()
        get_upvalue %dest, index
    If not upvalue: Check hoisted global functions.
    If hoisted function:
        %dest = allocate_register()
        load_const %dest, $K_function_ptr
*/
uint8_t Compiler::compile_expr_node(const ast::Variable_expr &expr)
{
    // 1. Is it a local variable?
    int local_arg = resolve_local(current_ctx_, expr.name);
    if (local_arg != -1) {
        return static_cast<uint8_t>(local_arg);
    }

    // 2. Is it a captured upvalue?
    int upval_arg = resolve_upvalue(current_ctx_, expr.name);
    if (upval_arg != -1) {
        uint8_t target_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Get_upvalue, target_reg, static_cast<uint8_t>(upval_arg), 0));
        return target_reg;
    }

    // 3. Is it a global/hoisted function?
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
    %rhs = compile(expr.value)
    if local:
        move %target_reg, %rhs
    if upvalue:
        set_upvalue %target_reg, %rhs
*/
uint8_t Compiler::compile_expr_node(const ast::Assignment_expr &expr)
{
    // Evaluate the right-hand side first
    uint8_t rhs_reg = compile_expr(expr.value);
    types::Type_id source_type = ast::get_type(tree.get(expr.value).node);
    if (source_type != expr.type) {
        emit_numeric_normalize(rhs_reg, expr.type);
    }

    // Search local
    int local_arg = resolve_local(current_ctx_, expr.name);
    if (local_arg != -1) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, static_cast<uint8_t>(local_arg), rhs_reg, 0));
        return static_cast<uint8_t>(local_arg);
    }

    // Search upvalues
    int upval_arg = resolve_upvalue(current_ctx_, expr.name);
    if (upval_arg != -1) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Set_upvalue, static_cast<uint8_t>(upval_arg), rhs_reg, 0));
        return rhs_reg;
    }

    std::println(std::cerr, "Compiler Bug: Reassignment of unresolved or global variable '{}'.", expr.name);
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

uint8_t Compiler::compile_expr_node(const ast::Cast_expr &expr)
{
    uint8_t value_reg = compile_expr(expr.expression);

    if (!env.tt.is_numeric_primitive(expr.target_type)) {
        std::println(
            std::cerr,
            "Compiler Bug: Only numeric cast expressions are currently implemented, got '{}'.",
            env.tt.to_string(expr.target_type));
        std::exit(EXIT_FAILURE);
    }

    emit(vm::Instruction::make_rrr(cast_opcode_for(expr.target_type), value_reg, 0, 0));
    return value_reg;
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
    types::Type_id left_type = ast::get_type(tree.get(expr.left).node);
    types::Type_id right_type = ast::get_type(tree.get(expr.right).node);

    vm::Opcode opcode = vm::Opcode::Move;
    switch (expr.op) {
    case lex::TokenType::Plus:
    case lex::TokenType::Minus:
    case lex::TokenType::Star:
    case lex::TokenType::Slash:
    case lex::TokenType::Percent:
        opcode = arithmetic_opcode_for(expr.op, expr.type);
        break;
    case lex::TokenType::Equal:
    case lex::TokenType::NotEqual:
    case lex::TokenType::Less:
    case lex::TokenType::LessEqual:
    case lex::TokenType::Greater:
    case lex::TokenType::GreaterEqual: {
        opcode = comparison_opcode_for(expr.op, left_type, right_type);
        break;
    }
    default:
        std::println(std::cerr, "Unsupported binary operator in compiler");
        std::exit(EXIT_FAILURE);
    }

    emit(vm::Instruction::make_rrr(opcode, dest_reg, reg_a, reg_b));
    emit_numeric_normalize(dest_reg, expr.type);
    return dest_reg;
}

/*
    Compiles an anonymous closure expression.
    Operates identically to Function_stmt, but returns a register holding the closure.
*/
uint8_t Compiler::compile_expr_node(const ast::Closure_expr &expr)
{
    Compiler_context fn_ctx;
    fn_ctx.current_register = 0;
    push_context(&fn_ctx);

    // Register parameters
    for (const auto &param : expr.parameters) {
        uint8_t reg = allocate_register();
        current_ctx_->locals.push_back({param.name, reg});
    }

    // Body
    if (!expr.body.is_null()) {
        compile_stmt(expr.body);
    }

    // Failsafe return
    uint8_t nil_reg = allocate_register();
    emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, nil_reg, 0, 0));
    emit(vm::Instruction::make_rrr(vm::Opcode::Return, nil_reg, 0, 0));

    Closure_data *closure = arena.allocate<Closure_data>();
    new (closure) Closure_data();

    // Anonymous name
    std::string name = "anon";
    size_t name_size = sizeof(String_data) + name.length() + 1;
    closure->name = static_cast<String_data *>(arena.allocate_bytes(name_size));
    closure->name->length = name.length();
    std::copy(name.begin(), name.end(), closure->name->chars);
    closure->name->chars[name.length()] = '\0';

    closure->arity = expr.parameters.size();

    // Copy Bytecode
    closure->code_count = current_ctx_->block.instructions.size();
    if (closure->code_count > 0) {
        closure->code = arena.allocate<vm::Instruction>(closure->code_count);
        std::copy(current_ctx_->block.instructions.begin(), current_ctx_->block.instructions.end(), closure->code);
    }

    // Copy Constants
    closure->constant_count = current_ctx_->block.constants.size();
    if (closure->constant_count > 0) {
        closure->constants = arena.allocate<Value>(closure->constant_count);
        std::copy(current_ctx_->block.constants.begin(), current_ctx_->block.constants.end(), closure->constants);
    }

    closure->native_func = std::nullopt;
    closure->upvalue_count = current_ctx_->upvalues.size();
    closure->upvalues = nullptr;

    pop_context(); // Back to parent context

    // Since this is an expression, it is ALWAYS evaluated at runtime.
    // We add the blueprint to the constant pool, and emit a Make_closure instruction.
    uint16_t const_idx = add_constant(Value::make_closure(closure));
    uint8_t dst_reg = allocate_register();
    emit(vm::Instruction::make_ri(vm::Opcode::Make_closure, dst_reg, const_idx));

    // Emit the routing bytes
    for (const auto &uv : fn_ctx.upvalues) {
        vm::Instruction route;
        route.rrr.op = vm::Opcode::None;
        route.rrr.src_a = uv.is_local ? 1 : 0;
        route.rrr.src_b = uv.index;
        route.rrr.dst = 0;
        emit(route);
    }

    return dst_reg; // Returning the register that will hold the instantiated closure
}

/*
    %callee = compile(expr.callee)
    %arg0 = compile(expr.arguments[0])
    %arg1 = compile(expr.arguments[1])

    // Pack into contiguous ABI layout
    %call_base = allocate()
    move %call_base, %callee

    %packed0 = allocate()
    move %packed0, %arg0

    %packed1 = allocate()
    move %packed1, %arg1

    %dest = allocate()
    call %dest, %call_base, argc
*/
uint8_t Compiler::compile_expr_node(const ast::Call_expr &expr)
{
    // 1. Evaluate callee
    uint8_t callee_eval_reg = compile_expr(expr.callee);

    // 2. Evaluate all arguments into scattered temporary registers
    std::vector<uint8_t> arg_eval_regs;
    for (const auto &arg : expr.arguments) {
        arg_eval_regs.push_back(compile_expr(arg.value));
    }

    // 3. Allocate a strictly contiguous block of registers for the ABI.
    // The first register MUST hold the callee pointer.
    uint8_t call_base_reg = allocate_register();
    emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, callee_eval_reg, 0));

    // The subsequent registers MUST hold the evaluated arguments in order.
    for (size_t i = 0; i < expr.arguments.size(); ++i) {
        uint8_t arg_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, arg_eval_regs[i], 0));
    }

    // 4. Allocate destination and emit the Call instruction
    uint8_t dest_reg = allocate_register();
    uint8_t argc = static_cast<uint8_t>(expr.arguments.size());

    emit(vm::Instruction::make_rrr(vm::Opcode::Call, dest_reg, call_base_reg, argc));

    return dest_reg;
}

/*
    Evaluates all elements, packs them into a contiguous block of registers,
    and issues the Make_array instruction.
*/
uint8_t Compiler::compile_expr_node(const ast::Array_literal_expr &expr)
{
    // 1. Evaluate all elements into scattered registers
    std::vector<uint8_t> element_regs;
    for (const auto &element : expr.elements) {
        element_regs.push_back(compile_expr(element));
    }

    // 2. Allocate a contiguous block of registers and move the elements in
    uint8_t base_reg = allocate_register();
    if (!element_regs.empty()) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, base_reg, element_regs[0], 0));
        for (size_t i = 1; i < element_regs.size(); ++i) {
            uint8_t next_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, next_reg, element_regs[i], 0));
        }
    }

    // 3. Emit Make_array
    uint8_t dest_reg = allocate_register();
    uint8_t count = static_cast<uint8_t>(element_regs.size());
    emit(vm::Instruction::make_rrr(vm::Opcode::Make_array, dest_reg, base_reg, count));

    return dest_reg;
}

/*
    %array = compile(expr.array)
    %index = compile(expr.index)
    %dest = allocate_register()
    load_index %dest, %array, %index
*/
uint8_t Compiler::compile_expr_node(const ast::Array_access_expr &expr)
{
    uint8_t array_reg = compile_expr(expr.array);
    uint8_t index_reg = compile_expr(expr.index);

    uint8_t dest_reg = allocate_register();
    emit(vm::Instruction::make_rrr(vm::Opcode::Load_index, dest_reg, array_reg, index_reg));

    return dest_reg;
}

/*
    %value = compile(expr.value)
    %array = compile(expr.array)
    %index = compile(expr.index)
    store_index %array, %index, %value
*/
uint8_t Compiler::compile_expr_node(const ast::Array_assignment_expr &expr)
{
    // Evaluate the right-hand side first
    uint8_t value_reg = compile_expr(expr.value);
    types::Type_id source_type = ast::get_type(tree.get(expr.value).node);
    if (source_type != expr.type) {
        emit_numeric_normalize(value_reg, expr.type);
    }

    // Evaluate target array and index
    uint8_t array_reg = compile_expr(expr.array);
    uint8_t index_reg = compile_expr(expr.index);

    emit(vm::Instruction::make_rrr(vm::Opcode::Store_index, array_reg, index_reg, value_reg));

    // In C-like languages, assignment evaluates to the assigned value
    return value_reg;
}

} // namespace phos::vm
