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

void Compiler::hoist_function_placeholder(Symbol_id id)
{
    if (!function_locations_.contains(id)) {
        // Pre-allocate the closure memory! Any nested block can now
        // point to this safely, even if it hasn't been populated yet.
        Closure_data *closure = ctx.arena.allocate<Closure_data>();
        new (closure) Closure_data();
        function_locations_[id] = closure;
    }

    if (!function_global_slots_.contains(id)) {
        // Reserve a VM global slot for the live closure instance. Populated
        // at the function's declaration; referenced by every call site.
        function_global_slots_[id] = ctx.registry.next_global_index++;
    }
}

void Compiler::hoist_module_functions(const Module_unit &module)
{
    for (auto stmt_id : module.ast_roots) {
        if (stmt_id.is_null()) {
            continue;
        }

        const auto &node = ctx.tree.get(stmt_id).node;

        if (const auto *fn_stmt = std::get_if<ast::Function_stmt>(&node)) {
            if (!fn_stmt->resolved_symbol) {
                std::println(std::cerr, "Compiler Bug: Function '{}' has no resolved symbol.", fn_stmt->name);
                std::exit(EXIT_FAILURE);
            }
            hoist_function_placeholder(*fn_stmt->resolved_symbol);
            continue;
        }

        const auto *model_stmt = std::get_if<ast::Model_stmt>(&node);
        if (!model_stmt) {
            continue;
        }

        for (auto method_id : model_stmt->methods) {
            if (method_id.is_null()) {
                continue;
            }

            const auto *method = std::get_if<ast::Function_stmt>(&ctx.tree.get(method_id).node);
            if (!method) {
                continue;
            }

            if (!method->resolved_symbol) {
                std::println(std::cerr, "Compiler Bug: Method '{}' has no resolved symbol.", method->name);
                std::exit(EXIT_FAILURE);
            }

            hoist_function_placeholder(*method->resolved_symbol);
        }
    }
}

void Compiler::compile_stmt_node(const ast::Function_stmt &stmt)
{
    const std::string canonical_name = canonical_function_name(stmt);

    // A `fn` declared inside another function body is a nested function: it
    // binds its name as a local closure in the enclosing frame and can capture
    // upvalues, exactly like `let name := fn() { ... }`.
    const bool is_nested_function = (current_ctx_ != nullptr && current_ctx_->enclosing != nullptr);

    if (is_nested_function) {
        compile_nested_function(stmt);
        return;
    }

    const bool can_reuse_hoisted_slot = (current_ctx_->enclosing == nullptr);
    const auto *saved_returns = current_function_returns_;

    // 1. Track function name for env::func intrinsic
    std::string prev_func_name = current_function_name_;
    current_function_name_ = stmt.name;

    Function_context fn_ctx;
    fn_ctx.current_register = 0;
    push_context(&fn_ctx);
    current_function_returns_ = &stmt.returns;

    push_scope();

    for (const auto &param : stmt.parameters) {
        uint8_t reg = allocate_register();
        current_ctx_->locals.push_back({param.name, reg});
    }
    for (const auto &ret : stmt.returns) {
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

    if (!stmt.body.is_null()) {
        if (const auto *body_block = std::get_if<ast::Block_stmt>(&ctx.tree.get(stmt.body).node)) {
            for (const auto &body_stmt : body_block->statements) {
                compile_stmt(body_stmt);
            }
        } else {
            compile_stmt(stmt.body);
        }
    }

    // Function-scoped defers must still see locals declared in the function body,
    // so emit them before we pop the body scope.
    for (auto it = current_ctx_->function_defers.rbegin(); it != current_ctx_->function_defers.rend(); ++it) {
        compile_stmt(*it);
    }

    pop_scope();

    auto normalized_returns = declared_return_types(stmt.returns, ctx.tt);
    if (normalized_returns.empty()) {
        uint8_t nil_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, nil_reg, 0, 0));
        emit(vm::Instruction::make_rrr(vm::Opcode::Return, nil_reg, 0, 0));
    } else if (normalized_returns.size() == 1 && !stmt.returns.empty() && !stmt.returns.front().name.empty()) {
        int local_reg = resolve_local(current_ctx_, stmt.returns.front().name);
        if (local_reg >= 0) {
            emit(vm::Instruction::make_rrr(vm::Opcode::Return, static_cast<uint8_t>(local_reg), 0, 0));
        } else {
            uint8_t nil_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, nil_reg, 0, 0));
            emit(vm::Instruction::make_rrr(vm::Opcode::Return, nil_reg, 0, 0));
        }
    } else if (!stmt.returns.empty()) {
        std::vector<uint8_t> source_regs;
        source_regs.reserve(stmt.returns.size());

        for (const auto &ret : stmt.returns) {
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

    if (!stmt.resolved_symbol) {
        std::println(std::cerr, "Compiler Bug: Function '{}' missing resolved symbol.", stmt.name);
        std::exit(EXIT_FAILURE);
    }

    // Grab the pre-allocated blueprint from the registry maps!
    Closure_data *closure = function_locations_.at(*stmt.resolved_symbol);

    set_closure_name(*closure, canonical_name);
    closure->arity = stmt.parameters.size();
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

    function_upvalues_[*stmt.resolved_symbol] = current_ctx_->upvalues;

    pop_context();
    current_function_returns_ = saved_returns;

    const std::optional<uint32_t> global_slot = function_global_slot(*stmt.resolved_symbol);

    if (can_reuse_hoisted_slot && !global_slot && closure->upvalue_count == 0) {
        // Restore before early exit!
        current_function_name_ = prev_func_name;
        return;
    }

    // Lazy load the closure into the LOCAL constant pool of the current scope!
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

    if (global_slot) {
        // Persist the live instance so any reference site (recursion, forward
        // refs, cross-module static paths) can load it from the shared frame.
        emit(vm::Instruction::make_ri(vm::Opcode::Store_global, dst_reg, *global_slot));
    }

    current_ctx_->locals.push_back({stmt.name, dst_reg});

    current_function_name_ = prev_func_name;
}

void Compiler::compile_nested_function(const ast::Function_stmt &stmt)
{
    // Pre-register the function name as a local in the enclosing frame so that
    // recursive references inside the body resolve (via resolve_upvalue) to the
    // register cell that will hold this closure instance.
    const uint8_t fn_reg = allocate_register();
    current_ctx_->locals.push_back({stmt.name, fn_reg});

    // Reuse the closure-expression compiler: it compiles the body, routes the
    // captured upvalues, and emits Make_closure into the enclosing block.
    ast::Closure_expr closure;
    closure.parameters = stmt.parameters;
    closure.returns = stmt.returns;
    closure.body = stmt.body;
    closure.type = ctx.tt.get_unknown();
    closure.loc = stmt.loc;

    const uint8_t closure_reg = compile_expr_node(closure);

    // compile_expr_node allocates its own destination register; copy the live
    // instance into the pre-registered cell so recursive calls and later
    // references to the name see the closure.
    if (closure_reg != fn_reg) {
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, fn_reg, closure_reg, 0));
    }
}

void Compiler::compile_stmt_node(const ast::Model_stmt &stmt)
{
    std::string prev_ns = current_module_ns_;
    current_module_ns_ = (prev_ns == "main" || prev_ns.empty()) ? stmt.name : prev_ns + "::" + stmt.name;

    for (auto method_id : stmt.methods) {
        compile_stmt(method_id);
    }

    current_module_ns_ = prev_ns;
}


void Compiler::compile_stmt_node([[maybe_unused]] const ast::Enum_stmt &stmt)
{}
void Compiler::compile_stmt_node([[maybe_unused]] const ast::Union_stmt &stmt)
{}

} // namespace phos::vm
