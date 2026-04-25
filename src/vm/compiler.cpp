#include "compiler.hpp"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <print>

namespace phos::vm {

Compiler::Compiler(const ast::Ast_tree &tree, const Type_environment &env, mem::Arena &arena_) : tree(tree), env(env), arena(arena_)
{}

// =============================================================================
// CONTEXT & LEXICAL SCOPING ENGINE
// =============================================================================

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
            function_locations_[fn_stmt.name] = add_constant(Value(nullptr));

        } else if (std::holds_alternative<ast::Model_stmt>(node)) {
            const auto &model_stmt = std::get<ast::Model_stmt>(node);
            for (auto method_id : model_stmt.methods) {
                if (method_id.is_null()) {
                    continue;
                }
                if (auto *m_fn = std::get_if<ast::Function_stmt>(&tree.get(method_id).node)) {
                    // This is now User::p!
                    function_locations_[m_fn->name] = add_constant(Value(nullptr));
                }
            }
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
    if (env.tt.is_string(left_type) && env.tt.is_string(right_type)) {
        if (op == lex::TokenType::Equal) {
            return vm::Opcode::Eq_str;
        }
        if (op == lex::TokenType::NotEqual) {
            return vm::Opcode::Neq_str;
        }
    }
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

vm::Opcode Compiler::bitwise_opcode_for(lex::TokenType op, types::Type_id type) const
{
    bool is_unsigned = env.tt.is_unsigned_integer_primitive(type);
    switch (op) {
    case lex::TokenType::BitAnd:
        return is_unsigned ? vm::Opcode::BitAnd_u64 : vm::Opcode::BitAnd_i64;
    case lex::TokenType::Pipe:
        return is_unsigned ? vm::Opcode::BitOr_u64 : vm::Opcode::BitOr_i64;
    case lex::TokenType::BitXor:
        return is_unsigned ? vm::Opcode::BitXor_u64 : vm::Opcode::BitXor_i64;
    case lex::TokenType::BitLShift:
        return is_unsigned ? vm::Opcode::Shl_u64 : vm::Opcode::Shl_i64;
    case lex::TokenType::BitRshift:
        return is_unsigned ? vm::Opcode::Shr_u64 : vm::Opcode::Shr_i64;
    default:
        std::println(std::cerr, "Compiler Bug: Unsupported bitwise operator.");
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
            } else if constexpr (std::is_same_v<T, ast::Enum_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Model_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Union_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Match_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::For_in_stmt>) {
                compile_stmt_node(s);
            } else {
                auto l = ast::get_loc(s);
                std::println(std::cerr, "{}:{} Unimplemented stmt node", l.l, l.c);
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
        route.rrr.op = vm::Opcode::None; // Safe No-Op padding
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

void Compiler::compile_stmt_node(const ast::Enum_stmt &stmt)
{}
void Compiler::compile_stmt_node(const ast::Model_stmt &stmt)
{
    // The model fields are purely compile-time info, but we MUST compile the methods!
    for (auto method_id : stmt.methods) {
        compile_stmt(method_id);
    }
}

void Compiler::compile_stmt_node(const ast::Union_stmt &stmt)
{
    // Purely compile-time construct. No bytecode needed.
}

void Compiler::compile_stmt_node(const ast::Match_stmt &stmt)
{
    // 1. Evaluate the subject once
    uint8_t subject_reg = compile_expr(stmt.subject);

    std::vector<size_t> end_jumps;

    // 2. Iterate through each arm
    for (const auto &arm : stmt.arms) {
        if (arm.is_wildcard) {
            // Wildcard is a catch-all, just execute the body and skip the rest
            if (!arm.body.is_null()) {
                compile_stmt(arm.body);
            }
            break;
        }

        // Extract the target variant name from the arm's pattern
        std::string variant_str;
        auto &pattern_node = tree.get(arm.pattern).node;
        if (auto *sp = std::get_if<ast::Static_path_expr>(&pattern_node)) {
            variant_str = sp->member.lexeme;
        } else if (auto *em = std::get_if<ast::Enum_member_expr>(&pattern_node)) {
            variant_str = em->member_name;
        } else {
            std::println(std::cerr, "Compiler Bug: Unsupported pattern type in match arm.");
            std::exit(EXIT_FAILURE);
        }

        // 3. Load the target variant string and emit Test_union
        uint8_t str_reg = allocate_register();
        uint16_t str_idx = add_constant(Value::make_string(arena, variant_str));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, str_reg, str_idx));

        uint8_t test_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Test_union, test_reg, subject_reg, str_reg));

        // 4. Jump to the next arm if this test fails
        size_t jump_next_arm_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, test_reg, 0));

        // 5. Set up scope for the arm body
        auto prev_locals_size = current_ctx_->locals.size();

        // 6. If the user requested the payload, extract it and bind it to a local variable!
        if (!arm.bind_name.empty()) {
            uint8_t payload_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_union_payload, payload_reg, subject_reg, 0));
            current_ctx_->locals.push_back({arm.bind_name, payload_reg});
        }

        // 7. Compile the arm body
        if (!arm.body.is_null()) {
            compile_stmt(arm.body);
        }

        // 8. Restore scope
        current_ctx_->locals.resize(prev_locals_size);

        // 9. Jump to the end of the entire match statement so we don't fall through into the next arm
        end_jumps.push_back(emit(vm::Instruction::make_i(vm::Opcode::Jump, 0)));

        // 10. Patch the jump_next_arm instruction to land right here
        uint16_t next_arm_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_next_arm_idx].ri.imm = next_arm_target;
    }

    // 11. Patch all the successful arm executions to jump to the very end
    uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
    for (size_t jump_idx : end_jumps) {
        current_block().instructions[jump_idx].i.imm = end_target;
    }
}
// =============================================================================
// EXPRESSIONS
// =============================================================================

uint8_t Compiler::compile_expr(ast::Expr_id expr_id)
{
    if (expr_id.is_null()) {
        return 0;
    }

    std::uint8_t result_reg = std::visit(
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
            } else if constexpr (std::is_same_v<T, ast::Enum_member_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Static_path_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Array_literal_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Array_access_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Model_literal_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Anon_model_literal_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Method_call_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Field_access_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Field_assignment_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Unary_expr>) {
                return compile_expr_node(e);
            } else if constexpr (std::is_same_v<T, ast::Range_expr>) {
                return compile_expr_node(e);
            } else {
                auto l = ast::get_loc(e);
                std::println("{}:{} Unimplemented expression node", l.l, l.c);
            }
            return 0;
        },
        tree.get(expr_id).node);

    uint8_t wraps = tree.get(expr_id).auto_wrap_depth;
    if (wraps > 0) {
        uint8_t wrapped_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Wrap_option, wrapped_reg, result_reg, wraps));
        return wrapped_reg;
    }
    return result_reg;
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

uint8_t Compiler::compile_expr_node(const ast::Enum_member_expr &expr)
{
    // 1. Get the enum type
    const auto &enum_t = std::get<types::Enum_type>(env.tt.get(expr.type).data);

    // 2. Fetch the resolved variant map from the environment
    auto enum_data_ptr = env.get_enum(enum_t.name);

    Value variant_val(nullptr);
    if (enum_data_ptr && enum_data_ptr->variants.contains(expr.member_name)) {
        variant_val = enum_data_ptr->variants.at(expr.member_name);
    } else {
        std::println(std::cerr, "Compiler Bug: Enum variant not found: {}::{}", enum_t.name, expr.member_name);
        std::exit(EXIT_FAILURE);
    }

    // 3. Treat the enum exactly like a hardcoded primitive literal!
    uint8_t target_reg = allocate_register();
    uint16_t const_idx = add_constant(variant_val);
    emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));

    return target_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Static_path_expr &expr)
{
    types::Type_id base_type = ast::get_type(tree.get(expr.base).node);

    if (env.tt.is_enum(base_type)) {
        const auto &enum_t = std::get<types::Enum_type>(env.tt.get(base_type).data);
        auto enum_data_ptr = env.get_enum(enum_t.name);

        Value variant_val(nullptr);
        if (enum_data_ptr && enum_data_ptr->variants.contains(expr.member.lexeme)) {
            variant_val = enum_data_ptr->variants.at(expr.member.lexeme);
        } else {
            std::println(std::cerr, "Compiler Bug: Enum variant not found: {}::{}", enum_t.name, expr.member.lexeme);
            std::exit(EXIT_FAILURE);
        }

        uint8_t target_reg = allocate_register();
        uint16_t const_idx = add_constant(variant_val);
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));

        return target_reg;
    }

    if (env.tt.is_model(base_type)) {
        auto &model_name = std::get<types::Model_type>(env.tt.get(base_type).data).name;
        std::string global_func_name = model_name + "::" + expr.member.lexeme;

        if (function_locations_.contains(global_func_name)) {
            uint8_t target_reg = allocate_register();
            uint16_t const_idx = function_locations_[global_func_name];
            emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));
            return target_reg;
        }
    }

    std::println(std::cerr, "Compiler Bug: Static path expressions are currently only implemented for Enums.");
    std::exit(EXIT_FAILURE);
    return 0;
}

uint8_t Compiler::compile_expr_node(const ast::Cast_expr &expr)
{
    uint8_t value_reg = compile_expr(expr.expression);
    types::Type_id source_type = ast::get_type(tree.get(expr.expression).node);

    if (env.tt.is_string(source_type) && env.tt.is_array(expr.target_type)) {
        uint8_t dest_reg = allocate_register();
        // src_b flag: 1 if it's an i8 array, 0 if it's a u8 array
        uint8_t is_signed = (env.tt.get_array_elem(expr.target_type) == env.tt.get_i8()) ? 1 : 0;
        emit(vm::Instruction::make_rrr(vm::Opcode::Cast_str_to_arr, dest_reg, value_reg, is_signed));
        return dest_reg;
    }

    if (env.tt.is_array(source_type) && env.tt.is_string(expr.target_type)) {
        uint8_t dest_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Cast_arr_to_str, dest_reg, value_reg, 0));
        return dest_reg;
    }

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
    Binary Expression Dispatcher
*/
uint8_t Compiler::compile_expr_node(const ast::Binary_expr &expr)
{
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
    case lex::TokenType::BitAnd:
    case lex::TokenType::Pipe:
    case lex::TokenType::BitXor:
    case lex::TokenType::BitLShift:
    case lex::TokenType::BitRshift: {
        opcode = bitwise_opcode_for(expr.op, expr.type);
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

uint8_t Compiler::compile_expr_node(const ast::Unary_expr &expr)
{
    uint8_t right_reg = compile_expr(expr.right);
    uint8_t dest_reg = allocate_register();

    types::Type_id right_type = ast::get_type(tree.get(expr.right).node);
    vm::Opcode opcode = vm::Opcode::Move;

    if (expr.op == lex::TokenType::Minus) {
        if (env.tt.is_float_primitive(right_type)) {
            opcode = vm::Opcode::Neg_f64;
        } else {
            opcode = vm::Opcode::Neg_i64;
        }
    } else if (expr.op == lex::TokenType::LogicalNot) {
        opcode = vm::Opcode::Not;
    } else if (expr.op == lex::TokenType::BitNot) {
        if (env.tt.is_unsigned_integer_primitive(right_type)) {
            opcode = vm::Opcode::BitNot_u64;
        } else {
            opcode = vm::Opcode::BitNot_i64;
        }
    } else {
        std::println(std::cerr, "Compiler Bug: Unsupported unary operator");
        std::exit(EXIT_FAILURE);
    }

    // src_b is unused (0) for unary operations
    emit(vm::Instruction::make_rrr(opcode, dest_reg, right_reg, 0));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Call_expr &expr)
{
    if (auto *var_callee = std::get_if<ast::Variable_expr>(&tree.get(expr.callee).node)) {
        if (var_callee->name == "len") {
            uint8_t arg_reg = compile_expr(expr.arguments[0].value);
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Len, dst_reg, arg_reg, 0));
            return dst_reg;
        }
    }
    if (auto *var_callee = std::get_if<ast::Variable_expr>(&tree.get(expr.callee).node)) {
        if (var_callee->name == "iter") {
            uint8_t arg_reg = compile_expr(expr.arguments[0].value);
            types::Type_id arg_type = ast::get_type(tree.get(expr.arguments[0].value).node);

            if (env.tt.is_model(arg_type)) {
                // custom model desugaring
                // desugar `iter(my_model)` into `my_model.iter()`
                auto &model_name = std::get<types::Model_type>(env.tt.get(arg_type).data).name;

                uint8_t func_reg = allocate_register();
                uint16_t const_idx = function_locations_[model_name + "::iter"];
                emit(vm::Instruction::make_ri(vm::Opcode::Load_const, func_reg, const_idx));

                uint8_t call_base_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, func_reg, 0));

                uint8_t arg_this_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_this_reg, arg_reg, 0));

                uint8_t dst_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Call, dst_reg, call_base_reg, 1));
                return dst_reg;
            } else {
                // native fast-path
                uint8_t dst_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Make_iter, dst_reg, arg_reg, 0));
                return dst_reg;
            }
        }
    }
    // 1. Evaluate callee
    uint8_t callee_eval_reg = compile_expr(expr.callee);

    // 2. Evaluate all arguments into scattered temporary registers
    std::vector<uint8_t> arg_eval_regs;
    for (const auto &arg : expr.arguments) {
        arg_eval_regs.push_back(compile_expr(arg.value));
    }

    // 3. Allocate a strictly contiguous block of registers for the ABI.
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

uint8_t Compiler::compile_expr_node(const ast::Model_literal_expr &expr)
{
    // --- UNION INSTANTIATION ---
    if (env.tt.is_union(expr.type)) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(expr.type).data);
        std::string variant_name = expr.fields[0].first;
        ast::Expr_id payload_expr = expr.fields[0].second;

        // 1. Evaluate payload
        uint8_t payload_reg = 0;
        if (!payload_expr.is_null()) {
            payload_reg = compile_expr(payload_expr);
        } else {
            payload_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, payload_reg, 0, 0));
        }

        // 2. Load strings into contiguous ABI registers
        uint8_t names_base = allocate_register();
        uint16_t u_name_idx = add_constant(Value::make_string(arena, union_t.name));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, names_base, u_name_idx));

        uint8_t v_name_reg = allocate_register(); // Guaranteed to be names_base + 1
        uint16_t v_name_idx = add_constant(Value::make_string(arena, variant_name));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, v_name_reg, v_name_idx));

        // 3. Emit Make_union
        uint8_t dst_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Make_union, dst_reg, names_base, payload_reg));
        return dst_reg;
    }

    // --- MODEL INSTANTIATION ---
    std::vector<uint8_t> field_regs;
    for (const auto &[name, value_expr] : expr.fields) {
        field_regs.push_back(compile_expr(value_expr));
    }

    uint8_t base_reg = allocate_register();
    if (!field_regs.empty()) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, base_reg, field_regs[0], 0));
        for (size_t i = 1; i < field_regs.size(); ++i) {
            uint8_t next_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, next_reg, field_regs[i], 0));
        }
    }

    uint8_t dest_reg = allocate_register();
    uint8_t count = static_cast<uint8_t>(field_regs.size());
    emit(vm::Instruction::make_rrr(vm::Opcode::Make_model, dest_reg, base_reg, count));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Anon_model_literal_expr &expr)
{
    // --- UNION INSTANTIATION ---
    if (env.tt.is_union(expr.type)) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(expr.type).data);
        std::string variant_name = expr.fields[0].first;
        ast::Expr_id payload_expr = expr.fields[0].second;

        uint8_t payload_reg = 0;
        if (!payload_expr.is_null()) {
            payload_reg = compile_expr(payload_expr);
        } else {
            payload_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, payload_reg, 0, 0));
        }

        uint8_t names_base = allocate_register();
        uint16_t u_name_idx = add_constant(Value::make_string(arena, union_t.name));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, names_base, u_name_idx));

        uint8_t v_name_reg = allocate_register();
        uint16_t v_name_idx = add_constant(Value::make_string(arena, variant_name));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, v_name_reg, v_name_idx));

        uint8_t dst_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Make_union, dst_reg, names_base, payload_reg));
        return dst_reg;
    }

    // --- MODEL INSTANTIATION ---
    std::vector<uint8_t> field_regs;
    for (const auto &[name, value_expr] : expr.fields) {
        field_regs.push_back(compile_expr(value_expr));
    }

    uint8_t base_reg = allocate_register();
    if (!field_regs.empty()) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, base_reg, field_regs[0], 0));
        for (size_t i = 1; i < field_regs.size(); ++i) {
            uint8_t next_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, next_reg, field_regs[i], 0));
        }
    }

    uint8_t dest_reg = allocate_register();
    uint8_t count = static_cast<uint8_t>(field_regs.size());
    emit(vm::Instruction::make_rrr(vm::Opcode::Make_model, dest_reg, base_reg, count));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Method_call_expr &expr)
{
    types::Type_id obj_type = ast::get_type(tree.get(expr.object).node);

    if (env.tt.is_optional(obj_type)) {
        uint8_t obj_reg = compile_expr(expr.object);

        if (expr.method_name == "is_nil") {
            uint8_t dst_reg = allocate_register();
            // Test_nil perfectly identifies a Depth 0 (End of chain) Nil
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_nil, dst_reg, obj_reg, 0));
            return dst_reg;
        }

        if (expr.method_name == "is_val" || expr.method_name == "has_val") {
            uint8_t dst_reg = allocate_register();
            // Test_val cleanly handles nested optionals without a NOT operation
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, dst_reg, obj_reg, 0));
            return dst_reg;
        }

        if (expr.method_name == "get") {
            if (expr.arguments.empty()) {
                uint8_t dst_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
                return dst_reg;
            } else {
                types::Type_id arg_type = ast::get_type(tree.get(expr.arguments[0].value).node);

                if (env.tt.is_string(arg_type)) {
                    uint8_t has_val_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));

                    // Jump to panic block if FALSE (Depth == 0)
                    size_t jump_panic_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

                    // .ok_block:
                    uint8_t dst_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
                    size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

                    // .panic_block:
                    uint16_t panic_target = static_cast<uint16_t>(current_block().instructions.size());
                    current_block().instructions[jump_panic_idx].ri.imm = panic_target;

                    uint8_t msg_reg = compile_expr(expr.arguments[0].value);
                    emit(vm::Instruction::make_rrr(vm::Opcode::Panic, 0, msg_reg, 0));

                    // .end:
                    uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
                    current_block().instructions[jump_end_idx].i.imm = end_target;

                    return dst_reg;

                } else if (env.tt.is_function(arg_type)) {
                    uint8_t dst_reg = allocate_register();
                    uint8_t has_val_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));

                    // Jump to closure block if FALSE (Depth == 0)
                    size_t jump_closure_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

                    // .unwrap_block (Has Value):
                    emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
                    size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

                    // .closure_block:
                    uint16_t closure_target = static_cast<uint16_t>(current_block().instructions.size());
                    current_block().instructions[jump_closure_idx].ri.imm = closure_target;

                    uint8_t closure_reg = compile_expr(expr.arguments[0].value);
                    emit(vm::Instruction::make_rrr(vm::Opcode::Call, dst_reg, closure_reg, 0));

                    // .end:
                    uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
                    current_block().instructions[jump_end_idx].i.imm = end_target;

                    return dst_reg;
                }
            }
        }

        if (expr.method_name == "or_else") {
            uint8_t dst_reg = allocate_register();
            uint8_t has_val_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));

            // Jump to closure block if FALSE (Depth == 0)
            size_t jump_closure_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

            // .unwrap_block (Has Value):
            emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            // .closure_block:
            uint16_t closure_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_closure_idx].ri.imm = closure_target;

            uint8_t closure_reg = compile_expr(expr.arguments[0].value);
            emit(vm::Instruction::make_rrr(vm::Opcode::Call, dst_reg, closure_reg, 0));

            // .end:
            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }
        if (expr.method_name == "value_or") {
            // 1. Evaluate the fallback expression FIRST
            uint8_t fallback_reg = compile_expr(expr.arguments[0].value);

            uint8_t dst_reg = allocate_register();
            uint8_t has_val_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));

            // 2. Jump to fallback block if FALSE (Depth == 0)
            size_t jump_fallback_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

            // .unwrap_block (Has Value):
            emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            // .fallback_block:
            uint16_t fallback_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_fallback_idx].ri.imm = fallback_target;

            // Just move the already evaluated fallback into the destination!
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, dst_reg, fallback_reg, 0));

            // .end:
            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }

        if (expr.method_name == "map") {
            uint8_t dst_reg = allocate_register();
            uint8_t has_val_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));

            // Jump to nil block if FALSE (Depth == 0)
            size_t jump_nil_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

            // .call_block (Has Value):
            uint8_t unwrapped_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, unwrapped_reg, obj_reg, 0));

            uint8_t closure_reg = compile_expr(expr.arguments[0].value);

            // Setup ABI
            uint8_t call_base_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, closure_reg, 0));
            uint8_t arg_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, unwrapped_reg, 0));

            uint8_t res_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Call, res_reg, call_base_reg, 1));

            emit(vm::Instruction::make_rrr(vm::Opcode::Wrap_option, dst_reg, res_reg, 1));
            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            // .is_nil_block:
            uint16_t nil_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_nil_idx].ri.imm = nil_target;
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, dst_reg, obj_reg, 0));

            // .end:
            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }
    }

    if (env.tt.is_union(obj_type) && (expr.method_name == "has" || expr.method_name == "get")) {
        uint8_t obj_reg = compile_expr(expr.object);

        std::string variant_str;
        auto &arg_node = tree.get(expr.arguments[0].value).node;
        if (auto *sp = std::get_if<ast::Static_path_expr>(&arg_node)) {
            variant_str = sp->member.lexeme;
        } else if (auto *em = std::get_if<ast::Enum_member_expr>(&arg_node)) {
            variant_str = em->member_name;
        }

        uint8_t str_reg = allocate_register();
        uint16_t str_idx = add_constant(Value::make_string(arena, variant_str));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, str_reg, str_idx));

        if (expr.method_name == "has") {
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_union, dst_reg, obj_reg, str_reg));
            return dst_reg;
        } else { // get
            uint8_t test_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_union, test_reg, obj_reg, str_reg));

            // Jump to the panic block if the variant didn't match (FALSE)
            size_t jump_panic_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, test_reg, 0));

            // .ok_block:
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_union_payload, dst_reg, obj_reg, 0));
            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            // .panic_block:
            uint16_t panic_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_panic_idx].ri.imm = panic_target;

            uint8_t msg_reg = allocate_register();
            std::string err_msg = "Union variant mismatch. Expected " + variant_str;
            uint16_t msg_idx = add_constant(Value::make_string(arena, err_msg));
            emit(vm::Instruction::make_ri(vm::Opcode::Load_const, msg_reg, msg_idx));
            emit(vm::Instruction::make_rrr(vm::Opcode::Panic, 0, msg_reg, 0));

            // .end:
            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }
    }

    if (env.tt.is_model(obj_type)) {
        auto &model_name = std::get<types::Model_type>(env.tt.get(obj_type).data).name;
        auto model_data_ptr = env.get_model(model_name);

        if (model_data_ptr && model_data_ptr->methods.contains(expr.method_name)) {
            std::string global_func_name = model_name + "::" + expr.method_name;

            // 1. Load the function closure
            uint8_t func_reg = allocate_register();
            uint16_t const_idx = function_locations_[global_func_name];
            emit(vm::Instruction::make_ri(vm::Opcode::Load_const, func_reg, const_idx));

            // 2. Evaluate arguments
            std::vector<uint8_t> arg_eval_regs;
            for (const auto &arg : expr.arguments) {
                arg_eval_regs.push_back(compile_expr(arg.value));
            }

            // 3. Set up contiguous ABI block
            uint8_t call_base_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, func_reg, 0));

            for (size_t i = 0; i < expr.arguments.size(); ++i) {
                uint8_t arg_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, arg_eval_regs[i], 0));
            }

            uint8_t dest_reg = allocate_register();
            uint8_t argc = static_cast<uint8_t>(expr.arguments.size());
            emit(vm::Instruction::make_rrr(vm::Opcode::Call, dest_reg, call_base_reg, argc));

            return dest_reg;
        } else {
            // ... fallback for closure fields ...
            auto &model_t = std::get<types::Model_type>(env.tt.get(obj_type).data);
            for (size_t i = 0; i < model_t.fields.size(); ++i) {
                if (model_t.fields[i].first == expr.method_name) {
                    uint8_t obj_reg = compile_expr(expr.object);
                    uint8_t field_idx = static_cast<uint8_t>(i);
                    uint8_t func_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Load_field, func_reg, obj_reg, field_idx));

                    std::vector<uint8_t> arg_eval_regs;
                    for (const auto &arg : expr.arguments) {
                        arg_eval_regs.push_back(compile_expr(arg.value));
                    }

                    uint8_t call_base_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, func_reg, 0));

                    for (size_t j = 0; j < expr.arguments.size(); ++j) {
                        uint8_t arg_reg = allocate_register();
                        emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, arg_eval_regs[j], 0));
                    }

                    uint8_t dest_reg = allocate_register();
                    uint8_t argc = static_cast<uint8_t>(expr.arguments.size());
                    emit(vm::Instruction::make_rrr(vm::Opcode::Call, dest_reg, call_base_reg, argc));

                    return dest_reg;
                }
            }
        }
    }

    if (expr.method_name == "iter") {
        uint8_t obj_reg = compile_expr(expr.object);
        uint8_t dst_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Make_iter, dst_reg, obj_reg, 0));
        return dst_reg;
    }

    if (env.tt.is_iterator(obj_type)) {
        uint8_t obj_reg = compile_expr(expr.object);
        if (expr.method_name == "next") {
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Iter_next, dst_reg, obj_reg, 0));
            return dst_reg;
        }
        if (expr.method_name == "prev") {
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Iter_prev, dst_reg, obj_reg, 0));
            return dst_reg;
        }
    }
    std::string ffi_name{""};

    if (env.tt.is_array(obj_type)) {
        ffi_name = "Array::" + expr.method_name;
    } else if (env.tt.is_iterator(obj_type)) {
        ffi_name = "Iter::" + expr.method_name;
    } else if (env.tt.is_string(obj_type)) {
        ffi_name = "String::" + expr.method_name;
    }

    if (!ffi_name.empty() && function_locations_.contains(ffi_name)) {
        // 1. Evaluate the receiver (the string or array)
        uint8_t obj_reg = compile_expr(expr.object);

        // 2. Evaluate all arguments into scattered temporary registers
        std::vector<uint8_t> arg_eval_regs;
        for (const auto &arg : expr.arguments) {
            arg_eval_regs.push_back(compile_expr(arg.value));
        }

        // 3. Load the native C++ function closure from the constant pool
        uint8_t func_reg = allocate_register();
        uint16_t const_idx = function_locations_[ffi_name];
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, func_reg, const_idx));

        // 4. Set up the ABI contiguous register block: [Closure, Receiver, Arg1, Arg2...]
        uint8_t call_base_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, func_reg, 0));

        // Push the receiver as the implicit first argument
        uint8_t arg_this = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_this, obj_reg, 0));

        // Push the rest of the arguments
        for (size_t i = 0; i < expr.arguments.size(); ++i) {
            uint8_t arg_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, arg_eval_regs[i], 0));
        }

        // 5. Emit the Call opcode (argc + 1 because the receiver counts as an argument)
        uint8_t dest_reg = allocate_register();
        uint8_t argc = static_cast<uint8_t>(expr.arguments.size() + 1);
        emit(vm::Instruction::make_rrr(vm::Opcode::Call, dest_reg, call_base_reg, argc));

        return dest_reg;
    }

    std::println(std::cerr, "Compiler Bug: These type ({}) of method calls not fully implemented in Bytecode yet!", expr.method_name);
    std::exit(EXIT_FAILURE);
    return 0;
}

uint8_t Compiler::compile_expr_node(const ast::Field_access_expr &expr)
{
    uint8_t obj_reg = compile_expr(expr.object);
    types::Type_id obj_type = ast::get_type(tree.get(expr.object).node);

    auto field_idx_opt = env.tt.get_model_field_index(obj_type, expr.field_name);
    if (!field_idx_opt) {
        std::println(std::cerr, "Compiler Bug: Field access offset missing.");
        std::exit(1);
    }

    uint8_t field_idx = static_cast<uint8_t>(*field_idx_opt);
    uint8_t dest_reg = allocate_register();
    emit(vm::Instruction::make_rrr(vm::Opcode::Load_field, dest_reg, obj_reg, field_idx));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Field_assignment_expr &expr)
{
    uint8_t value_reg = compile_expr(expr.value);
    types::Type_id source_type = ast::get_type(tree.get(expr.value).node);
    if (source_type != expr.type) {
        emit_numeric_normalize(value_reg, expr.type);
    }

    uint8_t obj_reg = compile_expr(expr.object);
    types::Type_id obj_type = ast::get_type(tree.get(expr.object).node);

    auto field_idx_opt = env.tt.get_model_field_index(obj_type, expr.field_name);
    if (!field_idx_opt) {
        std::println(std::cerr, "Compiler Bug: Field assignment offset missing.");
        std::exit(1);
    }

    uint8_t field_idx = static_cast<uint8_t>(*field_idx_opt);
    emit(vm::Instruction::make_rrr(vm::Opcode::Store_field, obj_reg, field_idx, value_reg));

    return value_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Array_literal_expr &expr)
{
    // 1. Evaluate all elements into scattered temporary registers
    std::vector<uint8_t> element_regs;
    for (const auto &el_expr : expr.elements) {
        element_regs.push_back(compile_expr(el_expr));
    }

    // 2. Allocate a strictly contiguous block of registers
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

uint8_t Compiler::compile_expr_node(const ast::Array_access_expr &expr)
{
    // Evaluate array and index
    uint8_t arr_reg = compile_expr(expr.array);
    uint8_t idx_reg = compile_expr(expr.index);

    // Emit Load_index
    uint8_t dest_reg = allocate_register();
    emit(vm::Instruction::make_rrr(vm::Opcode::Load_index, dest_reg, arr_reg, idx_reg));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Array_assignment_expr &expr)
{
    // 1. Evaluate the value being assigned
    uint8_t value_reg = compile_expr(expr.value);
    types::Type_id source_type = ast::get_type(tree.get(expr.value).node);
    if (source_type != expr.type) {
        emit_numeric_normalize(value_reg, expr.type);
    }

    // 2. Evaluate array and index
    uint8_t arr_reg = compile_expr(expr.array);
    uint8_t idx_reg = compile_expr(expr.index);

    // 3. Emit Store_index (dst = array, src_a = index, src_b = value)
    emit(vm::Instruction::make_rrr(vm::Opcode::Store_index, arr_reg, idx_reg, value_reg));

    return value_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Range_expr &expr)
{
    uint8_t start_reg = compile_expr(expr.start);
    uint8_t end_reg = compile_expr(expr.end);

    uint8_t dst_reg = allocate_register();

    vm::Opcode opcode = expr.inclusive ? vm::Opcode::Make_range_in : vm::Opcode::Make_range_ex;
    emit(vm::Instruction::make_rrr(opcode, dst_reg, start_reg, end_reg));

    return dst_reg;
}

void Compiler::compile_stmt_node(const ast::For_in_stmt &stmt)
{
    auto prev_locals_size = current_ctx_->locals.size();

    // Evaluate iterable once
    uint8_t iterable_reg = compile_expr(stmt.iterable);
    types::Type_id iterable_type = ast::get_type(tree.get(stmt.iterable).node);

    bool is_custom_model = env.tt.is_model(iterable_type);

    // Pre-bind loop variable register
    uint8_t loop_var_reg = allocate_register();
    current_ctx_->locals.push_back({stmt.var_name, loop_var_reg});

    // ============================================================
    // CUSTOM MODEL ITERATORS
    // ============================================================
    if (is_custom_model) {
        auto &model_name =
            std::get<types::Model_type>(env.tt.get(iterable_type).data).name;

        auto model_data_ptr = env.get_model(model_name);

        uint8_t iter_reg = allocate_register();
        std::string iter_model_name = model_name;

        // iterable.iter()
        if (model_data_ptr && model_data_ptr->methods.contains("iter")) {
            uint8_t func_reg = allocate_register();
            uint16_t const_idx = function_locations_[model_name + "::iter"];

            emit(vm::Instruction::make_ri(
                vm::Opcode::Load_const,
                func_reg,
                const_idx));

            uint8_t call_base = allocate_register();
            emit(vm::Instruction::make_rrr(
                vm::Opcode::Move,
                call_base,
                func_reg,
                0));

            uint8_t arg_this = allocate_register();
            emit(vm::Instruction::make_rrr(
                vm::Opcode::Move,
                arg_this,
                iterable_reg,
                0));

            emit(vm::Instruction::make_rrr(
                vm::Opcode::Call,
                iter_reg,
                call_base,
                1));

            auto iter_decl_id =
                model_data_ptr->methods.at("iter").declaration;

            auto iter_decl =
                std::get_if<ast::Function_stmt>(
                    &tree.get(iter_decl_id).node);

            iter_model_name =
                std::get<types::Model_type>(
                    env.tt.get(iter_decl->return_type).data).name;
        } else {
            emit(vm::Instruction::make_rrr(
                vm::Opcode::Move,
                iter_reg,
                iterable_reg,
                0));
        }

        // Optional step=1 support for next(this, step)
        uint8_t step_reg = allocate_register();
        uint16_t one_idx = add_constant(Value(static_cast<int64_t>(1)));
        emit(vm::Instruction::make_ri(
            vm::Opcode::Load_const,
            step_reg,
            one_idx));

        // ---------------- LOOP START ----------------
        uint32_t loop_start =
            static_cast<uint32_t>(current_block().instructions.size());

        auto next_decl_id =
            env.get_model(iter_model_name)->methods.at("next").declaration;

        auto next_decl =
            std::get_if<ast::Function_stmt>(
                &tree.get(next_decl_id).node);

        uint8_t argc =
            static_cast<uint8_t>(next_decl->parameters.size());

        uint8_t next_func = allocate_register();
        uint16_t next_idx =
            function_locations_[iter_model_name + "::next"];

        emit(vm::Instruction::make_ri(
            vm::Opcode::Load_const,
            next_func,
            next_idx));

        uint8_t call_base = allocate_register();
        emit(vm::Instruction::make_rrr(
            vm::Opcode::Move,
            call_base,
            next_func,
            0));

        uint8_t arg_this = allocate_register();
        emit(vm::Instruction::make_rrr(
            vm::Opcode::Move,
            arg_this,
            iter_reg,
            0));

        if (argc == 2) {
            uint8_t arg_step = allocate_register();
            emit(vm::Instruction::make_rrr(
                vm::Opcode::Move,
                arg_step,
                step_reg,
                0));
        }

        uint8_t opt_val_reg = allocate_register();

        emit(vm::Instruction::make_rrr(
            vm::Opcode::Call,
            opt_val_reg,
            call_base,
            argc));

        // Correct sentinel logic:
        // nil(depth=0) => stop
        // Some(nil) / Some(x) => continue
        uint8_t end_reg = allocate_register();
        emit(vm::Instruction::make_rrr(
            vm::Opcode::Test_nil,
            end_reg,
            opt_val_reg,
            0));

        size_t jump_body =
            emit(vm::Instruction::make_ri(
                vm::Opcode::Jump_if_false,
                end_reg,
                0));

        size_t jump_exit =
            emit(vm::Instruction::make_i(
                vm::Opcode::Jump,
                0));

        // body target
        uint16_t body_target =
            static_cast<uint16_t>(current_block().instructions.size());

        current_block().instructions[jump_body].ri.imm = body_target;

        emit(vm::Instruction::make_rrr(
            vm::Opcode::Unwrap_option,
            loop_var_reg,
            opt_val_reg,
            0));

        compile_stmt(stmt.body);

        emit(vm::Instruction::make_i(
            vm::Opcode::Jump,
            loop_start));

        uint32_t exit_target =
            static_cast<uint32_t>(current_block().instructions.size());

        current_block().instructions[jump_exit].i.imm = exit_target;
    }

    // ============================================================
    // BUILTIN ITERATORS
    // ============================================================
    else {
        uint8_t iter_reg = allocate_register();

        emit(vm::Instruction::make_rrr(
            vm::Opcode::Make_iter,
            iter_reg,
            iterable_reg,
            0));

        uint8_t opt_val_reg = allocate_register();

        // ---------------- LOOP START ----------------
        uint32_t loop_start =
            static_cast<uint32_t>(current_block().instructions.size());

        emit(vm::Instruction::make_rrr(
            vm::Opcode::Iter_next,
            opt_val_reg,
            iter_reg,
            0));

        uint8_t end_reg = allocate_register();

        emit(vm::Instruction::make_rrr(
            vm::Opcode::Test_nil,
            end_reg,
            opt_val_reg,
            0));

        size_t jump_body =
            emit(vm::Instruction::make_ri(
                vm::Opcode::Jump_if_false,
                end_reg,
                0));

        size_t jump_exit =
            emit(vm::Instruction::make_i(
                vm::Opcode::Jump,
                0));

        uint16_t body_target =
            static_cast<uint16_t>(current_block().instructions.size());

        current_block().instructions[jump_body].ri.imm = body_target;

        emit(vm::Instruction::make_rrr(
            vm::Opcode::Unwrap_option,
            loop_var_reg,
            opt_val_reg,
            0));

        compile_stmt(stmt.body);

        emit(vm::Instruction::make_i(
            vm::Opcode::Jump,
            loop_start));

        uint32_t exit_target =
            static_cast<uint32_t>(current_block().instructions.size());

        current_block().instructions[jump_exit].i.imm = exit_target;
    }

    current_ctx_->locals.resize(prev_locals_size);
}

} // namespace phos::vm
