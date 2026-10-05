#include "compiler.hpp"

#include "frontend/ast_utils.hpp"
#include "frontend/environment/symbol.hpp"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <optional>
#include <print>
#include <utility>

namespace phos::vm {

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
        ctx.tree.get(expr_id).node);

    uint8_t wraps = ctx.tree.get(expr_id).auto_wrap_depth;
    if (wraps > 0) {
        uint8_t wrapped_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Wrap_option, wrapped_reg, result_reg, wraps));
        return wrapped_reg;
    }
    return result_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Variable_expr &expr)
{
    // Locals and upvalue-captured closure instances take precedence so that
    // functions with upvalues are invoked through a live closure, not the raw
    // prototype (whose upvalue table is never populated).
    int local_arg = resolve_local(current_ctx_, expr.name);
    if (local_arg != -1) {
        return static_cast<uint8_t>(local_arg);
    }

    int upval_arg = resolve_upvalue(current_ctx_, expr.name);
    if (upval_arg != -1) {
        uint8_t target_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Get_upvalue, target_reg, static_cast<uint8_t>(upval_arg), 0));
        return target_reg;
    }

    if (expr.resolved_symbol) {
        const auto &sym = ctx.registry.get_symbol(*expr.resolved_symbol);

        if (sym.kind == Symbol_kind::Native_const || sym.kind == Symbol_kind::Phos_const) {
            uint8_t target_reg = allocate_register();
            uint16_t const_idx = add_constant(*sym.const_value);
            emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));
            return target_reg;
        }

        if (sym.kind == Symbol_kind::Native_func || sym.kind == Symbol_kind::Phos_func) {
            if (function_locations_.contains(sym.id)) {
                Closure_data *closure = function_locations_.at(sym.id);
                return load_function_instance(sym.id, closure);
            }
        }

        if (sym.kind == Symbol_kind::Global_var) {
            uint8_t target_reg = allocate_register();
            emit(vm::Instruction::make_ri(vm::Opcode::Load_global, target_reg, *sym.global_index));
            return target_reg;
        }
    }

    if (auto sym_id = ctx.registry.lookup_global(expr.name)) {
        if (function_locations_.contains(*sym_id)) {
            Closure_data *closure = function_locations_.at(*sym_id);
            return load_function_instance(*sym_id, closure);
        }
    }

    std::println(std::cerr, "Compiler Bug: Variable '{}' resolved to unknown state.", expr.name);
    std::exit(EXIT_FAILURE);
    return 0;
}

uint8_t Compiler::compile_expr_node(const ast::Assignment_expr &expr)
{
    uint8_t rhs_reg = compile_expr(expr.value);
    types::Type_id source_type = ast::get_type(ctx.tree.get(expr.value).node);
    if (source_type != expr.type) {
        emit_numeric_normalize(rhs_reg, expr.type);
    }

    int local_arg = resolve_local(current_ctx_, expr.name);
    if (local_arg != -1) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, static_cast<uint8_t>(local_arg), rhs_reg, 0));
        return static_cast<uint8_t>(local_arg);
    }

    int upval_arg = resolve_upvalue(current_ctx_, expr.name);
    if (upval_arg != -1) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Set_upvalue, static_cast<uint8_t>(upval_arg), rhs_reg, 0));
        return rhs_reg;
    }

    if (expr.resolved_symbol) {
        const auto &sym = ctx.registry.get_symbol(*expr.resolved_symbol);
        if (sym.kind == Symbol_kind::Global_var) {
            emit(vm::Instruction::make_ri(vm::Opcode::Store_global, rhs_reg, *sym.global_index));
            return rhs_reg;
        }
    }

    std::string canonical_name = current_module_ns_.empty() || current_module_ns_ == "main" ? expr.name : current_module_ns_ + "::" + expr.name;

    if (auto global_sym_id = ctx.registry.lookup_global(canonical_name)) {
        auto &sym = ctx.registry.get_symbol(*global_sym_id);
        if (sym.kind == Symbol_kind::Global_var) {
            emit(vm::Instruction::make_ri(vm::Opcode::Store_global, rhs_reg, *sym.global_index));
            return rhs_reg;
        }
    }

    std::println(std::cerr, "Compiler Bug: Reassignment of unresolved variable '{}'.", expr.name);
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

uint8_t Compiler::compile_expr_node(const ast::Enum_member_expr &expr)
{
    types::Type_id base_type = expr.type;
    while (ctx.tt.is_optional(base_type)) {
        base_type = ctx.tt.get_optional_base(base_type);
    }

    const auto &enum_t = std::get<types::Enum_type>(ctx.tt.get(base_type).data);
    auto enum_data_ptr = ctx.type_env.get_enum(enum_t.name);

    Value variant_val(nullptr);
    if (enum_data_ptr && enum_data_ptr->variants.contains(expr.member_name)) {
        variant_val = enum_data_ptr->variants.at(expr.member_name);
    } else {
        std::println(std::cerr, "Compiler Bug: Enum variant not found: {}::{}", enum_t.name, expr.member_name);
        std::exit(EXIT_FAILURE);
    }

    uint8_t target_reg = allocate_register();
    uint16_t const_idx = add_constant(variant_val);
    emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));

    return target_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Static_path_expr &expr)
{
    if (auto *base_var = std::get_if<ast::Variable_expr>(&ctx.tree.get(expr.base).node)) {
        if (base_var->name == "env") {
            if (expr.member.lexeme == "argv") {
                if (auto sym_id = ctx.registry.lookup_global("env::__argv_impl")) {
                    uint8_t func_reg = allocate_register();
                    Closure_data *closure = function_locations_.at(*sym_id);
                    uint16_t const_idx = add_constant(Value::make_closure(closure));
                    emit(vm::Instruction::make_ri(vm::Opcode::Load_const, func_reg, const_idx));

                    uint8_t call_base = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base, func_reg, 0));

                    uint8_t dest_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Call, dest_reg, call_base, 0));

                    return dest_reg;
                } else {
                    std::println(std::cerr, "Compiler Bug: env::__argv_impl native function not found.");
                    std::exit(EXIT_FAILURE);
                }
            }

            if (expr.member.lexeme == "line" || expr.member.lexeme == "file" || expr.member.lexeme == "func" || expr.member.lexeme == "module") {

                uint8_t target_reg = allocate_register();
                uint16_t const_idx = 0;

                if (expr.member.lexeme == "line") {
                    const_idx = add_constant(Value(static_cast<int64_t>(expr.loc.l)));
                } else if (expr.member.lexeme == "file") {
                    const_idx = add_constant(Value::make_string(ctx.arena, expr.loc.file));
                } else if (expr.member.lexeme == "func") {
                    const_idx = add_constant(Value::make_string(ctx.arena, current_function_name_));
                } else if (expr.member.lexeme == "module") {
                    const_idx = add_constant(Value::make_string(ctx.arena, current_module_ns_));
                }

                emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));
                return target_reg;
            }
        }
    }

    if (expr.resolved_symbol) {
        const auto &sym = ctx.registry.get_symbol(*expr.resolved_symbol);

        if (sym.kind == Symbol_kind::Native_const || sym.kind == Symbol_kind::Phos_const) {
            uint8_t target_reg = allocate_register();
            uint16_t const_idx = add_constant(*sym.const_value);
            emit(vm::Instruction::make_ri(vm::Opcode::Load_const, target_reg, const_idx));
            return target_reg;
        }

        if (sym.kind == Symbol_kind::Native_func || sym.kind == Symbol_kind::Phos_func) {
            if (function_locations_.contains(sym.id)) {
                return load_function_instance(sym.id, function_locations_.at(sym.id));
            } else {
                std::println(std::cerr, "Compiler Bug: Function '{}' not found in function_locations_", ctx.registry.resolve(sym.name));
                std::exit(EXIT_FAILURE);
            }
        }

        if (sym.kind == Symbol_kind::Global_var) {
            uint8_t target_reg = allocate_register();
            emit(vm::Instruction::make_ri(vm::Opcode::Load_global, target_reg, *sym.global_index));
            return target_reg;
        }
    }

    types::Type_id base_type = ast::get_type(ctx.tree.get(expr.base).node);
    while (ctx.tt.is_optional(base_type)) {
        base_type = ctx.tt.get_optional_base(base_type);
    }

    if (ctx.tt.is_enum(base_type)) {
        const auto &enum_t = std::get<types::Enum_type>(ctx.tt.get(base_type).data);
        auto enum_data_ptr = ctx.type_env.get_enum(enum_t.name);

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

    if (ctx.tt.is_model(base_type)) {
        auto &model_name = std::get<types::Model_type>(ctx.tt.get(base_type).data).name;
        if (auto static_field = ctx.type_env.get_model_static_field(model_name, expr.member.lexeme)) {
            return compile_expr(*static_field);
        }

        std::string global_func_name = model_name + "::" + expr.member.lexeme;

        if (auto sym_id = ctx.registry.lookup_global(global_func_name)) {
            if (function_locations_.contains(*sym_id)) {
                return load_function_instance(*sym_id, function_locations_.at(*sym_id));
            }
        }
    }

    std::println(std::cerr, "Compiler Bug: Static path expressions are currently only implemented for Enums, FFI, and Model Static Methods.");
    std::exit(EXIT_FAILURE);
    return 0;
}

uint8_t Compiler::compile_expr_node(const ast::Cast_expr &expr)
{
    uint8_t value_reg = compile_expr(expr.expression);
    types::Type_id source_type = ast::get_type(ctx.tree.get(expr.expression).node);

    if (source_type == expr.target_type) {
        return value_reg;
    }

    if (expr.is_saturating) {
        uint8_t dest_reg = allocate_register();
        if (dest_reg != value_reg) {
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, dest_reg, value_reg, 0));
        }
        emit(vm::Instruction::make_rrr(saturating_cast_opcode_for(expr.target_type), dest_reg, 0, 0));
        return dest_reg;
    }

    if (ctx.tt.is_string(source_type) && ctx.tt.is_array(expr.target_type)) {
        uint8_t dest_reg = allocate_register();
        uint8_t is_signed = (ctx.tt.get_array_elem(expr.target_type) == ctx.tt.get_i8()) ? 1 : 0;
        emit(vm::Instruction::make_rrr(vm::Opcode::Cast_str_to_arr, dest_reg, value_reg, is_signed));
        return dest_reg;
    }

    if (ctx.tt.is_array(source_type) && ctx.tt.is_string(expr.target_type)) {
        uint8_t dest_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Cast_arr_to_str, dest_reg, value_reg, 0));
        return dest_reg;
    }

    if (ctx.tt.is_optional(source_type) && !ctx.tt.is_optional(expr.target_type)) {
        uint8_t dest_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dest_reg, value_reg, 0));
        return dest_reg;
    }

    if (!ctx.tt.is_numeric_primitive(expr.target_type)) {
        return value_reg;
    }

    uint8_t dest_reg = allocate_register();
    if (dest_reg != value_reg) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, dest_reg, value_reg, 0));
    }
    emit(vm::Instruction::make_rrr(cast_opcode_for(expr.target_type), dest_reg, 0, 0));
    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Closure_expr &expr)
{
    const auto *saved_returns = current_function_returns_;

    // 1. Track function name for env::func intrinsic
    std::string prev_func_name = current_function_name_;
    current_function_name_ = "<anonymous>";

    Function_context fn_ctx;
    fn_ctx.current_register = 0;
    push_context(&fn_ctx);
    current_function_returns_ = &expr.returns;

    push_scope();

    // 2. Inject Parameters
    for (const auto &param : expr.parameters) {
        uint8_t reg = allocate_register();
        current_ctx_->locals.push_back({param.name, reg});
    }

    // 3. Inject Named Returns & Evaluate Defaults
    for (const auto &ret : expr.returns) {
        if (ret.name.empty()) {
            continue;
        }

        uint8_t reg = allocate_register();
        if (!ret.default_value.is_null()) {
            uint8_t value_reg = compile_expr(ret.default_value);
            if (ast::get_type(ctx.tree.get(ret.default_value).node) != ret.type) {
                emit_numeric_normalize(value_reg, ret.type);
            }
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, reg, value_reg, 0));
        } else {
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, reg, 0, 0));
        }
        current_ctx_->locals.push_back({ret.name, reg});
    }

    // 4. Compile Body
    if (!expr.body.is_null()) {
        compile_stmt(expr.body);
    }

    pop_scope();
    for (auto it = current_ctx_->function_defers.rbegin(); it != current_ctx_->function_defers.rend(); ++it) {
        compile_stmt(*it);
    }

    // 5. Safely pack implicit drop-off returns
    auto normalized_returns = declared_return_types(expr.returns, ctx.tt);
    if (normalized_returns.empty()) {
        uint8_t nil_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, nil_reg, 0, 0));
        emit(vm::Instruction::make_rrr(vm::Opcode::Return, nil_reg, 0, 0));
    } else if (normalized_returns.size() == 1 && !expr.returns.empty() && !expr.returns.front().name.empty()) {
        int local_reg = resolve_local(current_ctx_, expr.returns.front().name);
        if (local_reg >= 0) {
            emit(vm::Instruction::make_rrr(vm::Opcode::Return, static_cast<uint8_t>(local_reg), 0, 0));
        } else {
            uint8_t nil_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, nil_reg, 0, 0));
            emit(vm::Instruction::make_rrr(vm::Opcode::Return, nil_reg, 0, 0));
        }
    } else if (!expr.returns.empty()) {
        std::vector<uint8_t> source_regs;
        source_regs.reserve(expr.returns.size());

        for (const auto &ret : expr.returns) {
            int local_reg = -1;
            if (!ret.name.empty()) {
                local_reg = resolve_local(current_ctx_, ret.name);
            }

            // If it has no name, or local wasn't found, safely substitute nil
            if (local_reg >= 0) {
                source_regs.push_back(static_cast<uint8_t>(local_reg));
            } else {
                uint8_t nil_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, nil_reg, 0, 0));
                source_regs.push_back(nil_reg);
            }
        }

        uint8_t base_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, base_reg, source_regs[0], 0));
        for (size_t i = 1; i < source_regs.size(); ++i) {
            uint8_t next_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, next_reg, source_regs[i], 0));
        }

        uint8_t packed_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Make_model, packed_reg, base_reg, static_cast<uint8_t>(source_regs.size())));
        emit(vm::Instruction::make_rrr(vm::Opcode::Return, packed_reg, 0, 0));
    }

    // Patch unresolved gotos
    for (const auto &[label_name, jump_idx] : current_ctx_->unresolved_gotos) {
        if (current_ctx_->labels.contains(label_name)) {
            current_ctx_->block.instructions[jump_idx].i.imm = current_ctx_->labels.at(label_name);
        } else {
            std::println(std::cerr, "Compiler Bug: Unresolved goto label '{}' bypassed semantic checker.", label_name);
            std::exit(EXIT_FAILURE);
        }
    }

    // 6. Construct Closure Object
    Closure_data *closure = ctx.arena.allocate<Closure_data>();
    new (closure) Closure_data();

    std::string name = "anon";
    size_t name_size = sizeof(String_data) + name.length() + 1;
    closure->name = static_cast<String_data *>(ctx.arena.allocate_bytes(name_size));
    closure->name->length = name.length();
    std::copy(name.begin(), name.end(), closure->name->chars);
    closure->name->chars[name.length()] = '\0';

    closure->arity = expr.parameters.size();

    closure->code_count = current_ctx_->block.instructions.size();
    if (closure->code_count > 0) {
        closure->code = ctx.arena.allocate<vm::Instruction>(closure->code_count);
        std::copy(current_ctx_->block.instructions.begin(), current_ctx_->block.instructions.end(), closure->code);
    }

    closure->constant_count = current_ctx_->block.constants.size();
    if (closure->constant_count > 0) {
        closure->constants = ctx.arena.allocate<Value>(closure->constant_count);
        std::copy(current_ctx_->block.constants.begin(), current_ctx_->block.constants.end(), closure->constants);
    }

    closure->native_func = std::nullopt;
    closure->upvalue_count = current_ctx_->upvalues.size();
    closure->upvalues = nullptr;
    closure->register_footprint = static_cast<uint16_t>(current_ctx_->current_register);

    pop_context();
    current_function_returns_ = saved_returns;

    uint16_t const_idx = add_constant(Value::make_closure(closure));
    uint8_t dst_reg = allocate_register();
    emit(vm::Instruction::make_ri(vm::Opcode::Make_closure, dst_reg, const_idx));

    for (const auto &uv : fn_ctx.upvalues) {
        vm::Instruction route;
        route.rrr.op = vm::Opcode::None;
        route.rrr.src_a = uv.is_local ? 1 : 0;
        route.rrr.src_b = uv.index;
        route.rrr.dst = 0;
        emit(route);
    }

    // 7. Restore function name state after closure ends
    current_function_name_ = prev_func_name;

    return dst_reg;
}

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

    types::Type_id left_type = ast::get_type(ctx.tree.get(expr.left).node);
    types::Type_id right_type = ast::get_type(ctx.tree.get(expr.right).node);

    if (expr.op == lex::TokenType::Equal || expr.op == lex::TokenType::NotEqual) {
        if (ctx.tt.is_optional(left_type) && ctx.tt.is_nil(right_type)) {
            uint8_t optional_reg = compile_expr(expr.left);
            uint8_t dest_reg = allocate_register();
            emit(
                vm::Instruction::make_rrr(expr.op == lex::TokenType::Equal ? vm::Opcode::Test_nil : vm::Opcode::Test_val, dest_reg, optional_reg, 0));
            return dest_reg;
        }

        if (ctx.tt.is_nil(left_type) && ctx.tt.is_optional(right_type)) {
            uint8_t optional_reg = compile_expr(expr.right);
            uint8_t dest_reg = allocate_register();
            emit(
                vm::Instruction::make_rrr(expr.op == lex::TokenType::Equal ? vm::Opcode::Test_nil : vm::Opcode::Test_val, dest_reg, optional_reg, 0));
            return dest_reg;
        }
    }

    uint8_t reg_a = compile_expr(expr.left);
    uint8_t reg_b = compile_expr(expr.right);

    uint8_t dest_reg = allocate_register();

    if (expr.op == lex::TokenType::Plus) {
        if (ctx.tt.is_string(left_type) && ctx.tt.is_string(right_type)) {
            emit(vm::Instruction::make_rrr(vm::Opcode::Concat_str, dest_reg, reg_a, reg_b));
            return dest_reg;
        }
    }

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

    types::Type_id right_type = ast::get_type(ctx.tree.get(expr.right).node);
    vm::Opcode opcode = vm::Opcode::Move;

    if (expr.op == lex::TokenType::Minus) {
        if (ctx.tt.is_float_primitive(right_type)) {
            opcode = vm::Opcode::Neg_f64;
        } else {
            opcode = vm::Opcode::Neg_i64;
        }
    } else if (expr.op == lex::TokenType::LogicalNot) {
        opcode = vm::Opcode::Not;
    } else if (expr.op == lex::TokenType::BitNot) {
        if (ctx.tt.is_unsigned_integer_primitive(right_type)) {
            opcode = vm::Opcode::BitNot_u64;
        } else {
            opcode = vm::Opcode::BitNot_i64;
        }
    } else {
        std::println(std::cerr, "Compiler Bug: Unsupported unary operator");
        std::exit(EXIT_FAILURE);
    }

    emit(vm::Instruction::make_rrr(opcode, dest_reg, right_reg, 0));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Call_expr &expr)
{
    if (auto *var_callee = std::get_if<ast::Variable_expr>(&ctx.tree.get(expr.callee).node)) {
        if (var_callee->name == "len") {
            uint8_t arg_reg = compile_expr(expr.arguments[0].value);
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Len, dst_reg, arg_reg, 0));
            return dst_reg;
        }
        if (var_callee->name == "iter") {
            uint8_t arg_reg = compile_expr(expr.arguments[0].value);
            types::Type_id arg_type = ast::get_type(ctx.tree.get(expr.arguments[0].value).node);

            if (ctx.tt.is_model(arg_type)) {
                auto &model_name = std::get<types::Model_type>(ctx.tt.get(arg_type).data).name;

                uint8_t func_reg = allocate_register();
                if (auto sym_id = ctx.registry.lookup_global(model_name + "::iter")) {
                    func_reg = load_function_instance(*sym_id, function_locations_.at(*sym_id));
                }

                uint8_t call_base_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, func_reg, 0));

                uint8_t arg_this_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_this_reg, arg_reg, 0));

                uint8_t dst_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Call, dst_reg, call_base_reg, 1));
                return dst_reg;
            } else {
                uint8_t dst_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Make_iter, dst_reg, arg_reg, 0));
                return dst_reg;
            }
        }
    }

    uint8_t callee_eval_reg = compile_expr(expr.callee);

    std::vector<uint8_t> arg_eval_regs;
    for (const auto &arg : expr.arguments) {
        arg_eval_regs.push_back(compile_expr(arg.value));
    }

    uint8_t call_base_reg = allocate_register();
    emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, callee_eval_reg, 0));

    for (size_t i = 0; i < expr.arguments.size(); ++i) {
        uint8_t arg_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, arg_eval_regs[i], 0));
    }

    uint8_t dest_reg = allocate_register();
    uint8_t argc = static_cast<uint8_t>(expr.arguments.size());

    emit(vm::Instruction::make_rrr(vm::Opcode::Call, dest_reg, call_base_reg, argc));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Model_literal_expr &expr)
{
    if (ctx.tt.is_union(expr.type)) {
        auto &union_t = std::get<types::Union_type>(ctx.tt.get(expr.type).data);
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
        uint16_t u_name_idx = add_constant(Value::make_string(ctx.arena, union_t.name));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, names_base, u_name_idx));

        uint8_t v_name_reg = allocate_register();
        uint16_t v_name_idx = add_constant(Value::make_string(ctx.arena, variant_name));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, v_name_reg, v_name_idx));

        uint8_t dst_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Make_union, dst_reg, names_base, payload_reg));
        return dst_reg;
    }

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
    if (ctx.tt.is_union(expr.type)) {
        auto &union_t = std::get<types::Union_type>(ctx.tt.get(expr.type).data);
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
        uint16_t u_name_idx = add_constant(Value::make_string(ctx.arena, union_t.name));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, names_base, u_name_idx));

        uint8_t v_name_reg = allocate_register();
        uint16_t v_name_idx = add_constant(Value::make_string(ctx.arena, variant_name));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, v_name_reg, v_name_idx));

        uint8_t dst_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Make_union, dst_reg, names_base, payload_reg));
        return dst_reg;
    }

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
    types::Type_id obj_type = ast::get_type(ctx.tree.get(expr.object).node);

    if (ctx.tt.is_optional(obj_type)) {
        uint8_t obj_reg = compile_expr(expr.object);

        if (expr.method_name == "is_nil") {
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_nil, dst_reg, obj_reg, 0));
            return dst_reg;
        }

        if (expr.method_name == "is_val" || expr.method_name == "has_val") {
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, dst_reg, obj_reg, 0));
            return dst_reg;
        }

        if (expr.method_name == "get") {
            uint8_t dst_reg = allocate_register();

            if (expr.arguments.empty()) {
                emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
                return dst_reg;
            }

            types::Type_id arg_type = ast::get_type(ctx.tree.get(expr.arguments[0].value).node);
            uint8_t has_val_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));

            size_t jump_panic_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

            emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            uint16_t panic_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_panic_idx].ri.imm = panic_target;

            if (ctx.tt.is_string(arg_type)) {
                uint8_t msg_reg = compile_expr(expr.arguments[0].value);
                emit(vm::Instruction::make_rrr(vm::Opcode::Panic, 0, msg_reg, 0));
            } else if (ctx.tt.is_function(arg_type)) {
                uint8_t closure_reg = compile_expr(expr.arguments[0].value);
                emit(vm::Instruction::make_rrr(vm::Opcode::Call, dst_reg, closure_reg, 0));
            }

            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }

        if (expr.method_name == "or_else") {
            uint8_t dst_reg = allocate_register();
            uint8_t has_val_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));

            size_t jump_closure_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

            emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            uint16_t closure_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_closure_idx].ri.imm = closure_target;

            uint8_t closure_reg = compile_expr(expr.arguments[0].value);
            emit(vm::Instruction::make_rrr(vm::Opcode::Call, dst_reg, closure_reg, 0));

            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }

        if (expr.method_name == "value_or") {
            uint8_t fallback_reg = compile_expr(expr.arguments[0].value);
            uint8_t dst_reg = allocate_register();
            uint8_t has_val_reg = allocate_register();

            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));
            size_t jump_fallback_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

            emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, dst_reg, obj_reg, 0));
            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            uint16_t fallback_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_fallback_idx].ri.imm = fallback_target;
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, dst_reg, fallback_reg, 0));

            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }

        if (expr.method_name == "map") {
            uint8_t dst_reg = allocate_register();
            uint8_t has_val_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_val, has_val_reg, obj_reg, 0));

            size_t jump_nil_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, has_val_reg, 0));

            uint8_t unwrapped_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, unwrapped_reg, obj_reg, 0));

            uint8_t closure_reg = compile_expr(expr.arguments[0].value);

            uint8_t call_base_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, closure_reg, 0));
            uint8_t arg_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, unwrapped_reg, 0));

            uint8_t res_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Call, res_reg, call_base_reg, 1));
            emit(vm::Instruction::make_rrr(vm::Opcode::Wrap_option, dst_reg, res_reg, 1));

            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            uint16_t nil_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_nil_idx].ri.imm = nil_target;
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, dst_reg, obj_reg, 0));

            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }
    }

    if (ctx.tt.is_union(obj_type) && (expr.method_name == "has" || expr.method_name == "get")) {
        uint8_t obj_reg = compile_expr(expr.object);

        std::string variant_str;
        auto &arg_node = ctx.tree.get(expr.arguments[0].value).node;
        if (auto *sp = std::get_if<ast::Static_path_expr>(&arg_node)) {
            variant_str = sp->member.lexeme;
        } else if (auto *em = std::get_if<ast::Enum_member_expr>(&arg_node)) {
            variant_str = em->member_name;
        }

        uint8_t str_reg = allocate_register();
        uint16_t str_idx = add_constant(Value::make_string(ctx.arena, variant_str));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, str_reg, str_idx));

        if (expr.method_name == "has") {
            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_union, dst_reg, obj_reg, str_reg));
            return dst_reg;
        } else {
            uint8_t test_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_union, test_reg, obj_reg, str_reg));

            size_t jump_panic_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, test_reg, 0));

            uint8_t dst_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_union_payload, dst_reg, obj_reg, 0));
            size_t jump_end_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

            uint16_t panic_target = static_cast<uint16_t>(current_block().instructions.size());
            current_block().instructions[jump_panic_idx].ri.imm = panic_target;

            uint8_t msg_reg = allocate_register();
            std::string err_msg = "Union variant mismatch. Expected " + variant_str;
            uint16_t msg_idx = add_constant(Value::make_string(ctx.arena, err_msg));
            emit(vm::Instruction::make_ri(vm::Opcode::Load_const, msg_reg, msg_idx));
            emit(vm::Instruction::make_rrr(vm::Opcode::Panic, 0, msg_reg, 0));

            uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
            current_block().instructions[jump_end_idx].i.imm = end_target;

            return dst_reg;
        }
    }

    if (ctx.tt.is_model(obj_type)) {
        auto &model_name = std::get<types::Model_type>(ctx.tt.get(obj_type).data).name;
        auto model_data_ptr = ctx.type_env.get_model(model_name);

        if (model_data_ptr && model_data_ptr->methods.contains(expr.method_name)) {
            std::string global_func_name = model_name + "::" + expr.method_name;

            if (auto sym_id = ctx.registry.lookup_global(global_func_name)) {
                if (function_locations_.contains(*sym_id)) {
                    uint8_t func_reg = load_function_instance(*sym_id, function_locations_.at(*sym_id));

                    std::vector<uint8_t> arg_eval_regs;
                    for (const auto &arg : expr.arguments) {
                        arg_eval_regs.push_back(compile_expr(arg.value));
                    }

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
                }
            }
        }

        std::string native_model_method_name = model_name + "::" + expr.method_name;
        if (!model_name.empty() && ctx.type_env.is_native_defined(native_model_method_name)) {
            if (auto sym_id = ctx.registry.lookup_global(native_model_method_name)) {
                if (function_locations_.contains(*sym_id)) {
                    uint8_t obj_reg = compile_expr(expr.object);

                    std::vector<uint8_t> arg_eval_regs;
                    for (const auto &arg : expr.arguments) {
                        arg_eval_regs.push_back(compile_expr(arg.value));
                    }

                    uint8_t func_reg = load_function_instance(*sym_id, function_locations_.at(*sym_id));

                    uint8_t call_base_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, func_reg, 0));

                    uint8_t arg_this = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_this, obj_reg, 0));

                    for (size_t i = 0; i < expr.arguments.size(); ++i) {
                        uint8_t arg_reg = allocate_register();
                        emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, arg_eval_regs[i], 0));
                    }

                    uint8_t dest_reg = allocate_register();
                    uint8_t argc = static_cast<uint8_t>(expr.arguments.size() + 1);
                    emit(vm::Instruction::make_rrr(vm::Opcode::Call, dest_reg, call_base_reg, argc));

                    return dest_reg;
                }
            }
        }

        auto &model_t = std::get<types::Model_type>(ctx.tt.get(obj_type).data);
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

    if (expr.method_name == "iter") {
        uint8_t obj_reg = compile_expr(expr.object);
        uint8_t dst_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Make_iter, dst_reg, obj_reg, 0));
        return dst_reg;
    }

    if (ctx.tt.is_iterator(obj_type)) {
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

    if (ctx.tt.is_array(obj_type)) {
        ffi_name = "Array::" + expr.method_name;
    } else if (ctx.tt.is_iterator(obj_type)) {
        ffi_name = "Iter::" + expr.method_name;
    } else if (ctx.tt.is_string(obj_type)) {
        ffi_name = "String::" + expr.method_name;
    }

    if (!ffi_name.empty()) {
        if (auto sym_id = ctx.registry.lookup_global(ffi_name)) {
            if (function_locations_.contains(*sym_id)) {
                uint8_t obj_reg = compile_expr(expr.object);

                std::vector<uint8_t> arg_eval_regs;
                for (const auto &arg : expr.arguments) {
                    arg_eval_regs.push_back(compile_expr(arg.value));
                }

                uint8_t func_reg = allocate_register();
                Closure_data *closure = function_locations_.at(*sym_id);
                uint16_t const_idx = add_constant(Value::make_closure(closure));
                emit(vm::Instruction::make_ri(vm::Opcode::Load_const, func_reg, const_idx));

                uint8_t call_base_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, func_reg, 0));

                uint8_t arg_this = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_this, obj_reg, 0));

                for (size_t i = 0; i < expr.arguments.size(); ++i) {
                    uint8_t arg_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_reg, arg_eval_regs[i], 0));
                }

                uint8_t dest_reg = allocate_register();
                uint8_t argc = static_cast<uint8_t>(expr.arguments.size() + 1);
                emit(vm::Instruction::make_rrr(vm::Opcode::Call, dest_reg, call_base_reg, argc));

                return dest_reg;
            }
        }
    }

    std::println(std::cerr, "Compiler Bug: These type ({}) of method calls not fully implemented in Bytecode yet!", expr.method_name);
    std::exit(EXIT_FAILURE);
    return 0;
}

uint8_t Compiler::compile_expr_node(const ast::Field_access_expr &expr)
{
    uint8_t obj_reg = compile_expr(expr.object);
    types::Type_id obj_type = ast::get_type(ctx.tree.get(expr.object).node);

    auto field_idx_opt = ctx.tt.get_model_field_index(obj_type, expr.field_name);
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
    types::Type_id source_type = ast::get_type(ctx.tree.get(expr.value).node);
    if (source_type != expr.type) {
        emit_numeric_normalize(value_reg, expr.type);
    }

    uint8_t obj_reg = compile_expr(expr.object);
    types::Type_id obj_type = ast::get_type(ctx.tree.get(expr.object).node);

    auto field_idx_opt = ctx.tt.get_model_field_index(obj_type, expr.field_name);
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
    std::vector<uint8_t> element_regs;
    for (const auto &el_expr : expr.elements) {
        element_regs.push_back(compile_expr(el_expr));
    }

    uint8_t base_reg = allocate_register();
    if (!element_regs.empty()) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, base_reg, element_regs[0], 0));
        for (size_t i = 1; i < element_regs.size(); ++i) {
            uint8_t next_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, next_reg, element_regs[i], 0));
        }
    }

    uint8_t dest_reg = allocate_register();
    uint8_t count = static_cast<uint8_t>(element_regs.size());
    emit(vm::Instruction::make_rrr(vm::Opcode::Make_array, dest_reg, base_reg, count));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Array_access_expr &expr)
{
    uint8_t arr_reg = compile_expr(expr.array);
    uint8_t idx_reg = compile_expr(expr.index);

    uint8_t dest_reg = allocate_register();
    emit(vm::Instruction::make_rrr(vm::Opcode::Load_index, dest_reg, arr_reg, idx_reg));

    return dest_reg;
}

uint8_t Compiler::compile_expr_node(const ast::Array_assignment_expr &expr)
{
    uint8_t value_reg = compile_expr(expr.value);
    types::Type_id source_type = ast::get_type(ctx.tree.get(expr.value).node);
    if (source_type != expr.type) {
        emit_numeric_normalize(value_reg, expr.type);
    }

    uint8_t arr_reg = compile_expr(expr.array);
    uint8_t idx_reg = compile_expr(expr.index);

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

} // namespace phos::vm
