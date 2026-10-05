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

void Compiler::compile_stmt(ast::Stmt_id stmt_id)
{
    if (stmt_id.is_null()) {
        return;
    }

    if (stmt_id.value >= ctx.tree.statements.size()) {
        std::println(std::cerr, "Compiler Bug: Invalid stmt id '{}' (statement table size: {}).", stmt_id.value, ctx.tree.statements.size());
        std::exit(EXIT_FAILURE);
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
            } else if constexpr (std::is_same_v<T, ast::Multi_var_stmt>) {
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
            } else if constexpr (std::is_same_v<T, ast::Break_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Continue_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Goto_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Label_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Defer_stmt>) {
                compile_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Import_stmt>) {
                // Imports are a compile-time construct. No bytecode to emit
            } else {
                auto l = ast::get_loc(s);
                std::println(std::cerr, "{}:{} Unimplemented stmt node", l.l, l.c);
            }
        },
        ctx.tree.get(stmt_id).node);
}

void Compiler::compile_stmt_node(const ast::Break_stmt &stmt)
{
    size_t target_depth = 0;

    if (stmt.target_label.empty()) {
        target_depth = current_ctx_->loops.back().scope_depth;
        emit_defers(target_depth);
        current_ctx_->loops.back().break_jumps.push_back(emit(vm::Instruction::make_i(vm::Opcode::Jump, 0)));
    } else {
        for (auto it = current_ctx_->loops.rbegin(); it != current_ctx_->loops.rend(); ++it) {
            if (it->label == stmt.target_label) {
                target_depth = it->scope_depth;
                emit_defers(target_depth);
                it->break_jumps.push_back(emit(vm::Instruction::make_i(vm::Opcode::Jump, 0)));
                return;
            }
        }
    }
}

void Compiler::compile_stmt_node(const ast::Continue_stmt &stmt)
{
    size_t target_depth = 0;

    if (stmt.target_label.empty()) {
        target_depth = current_ctx_->loops.back().scope_depth;
        emit_defers(target_depth);
        current_ctx_->loops.back().continue_jumps.push_back(emit(vm::Instruction::make_i(vm::Opcode::Jump, 0)));
    } else {
        for (auto it = current_ctx_->loops.rbegin(); it != current_ctx_->loops.rend(); ++it) {
            if (it->label == stmt.target_label) {
                target_depth = it->scope_depth;
                emit_defers(target_depth);
                it->continue_jumps.push_back(emit(vm::Instruction::make_i(vm::Opcode::Jump, 0)));
                return;
            }
        }
    }
}

void Compiler::compile_stmt_node(const ast::Label_stmt &stmt)
{
    current_ctx_->labels[stmt.name] = static_cast<uint32_t>(current_block().instructions.size());
}

void Compiler::compile_stmt_node(const ast::Goto_stmt &stmt)
{
    size_t jump_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

    auto it = current_ctx_->labels.find(stmt.target_label);
    if (it != current_ctx_->labels.end()) {
        current_block().instructions[jump_idx].i.imm = it->second;
    } else {
        current_ctx_->unresolved_gotos.push_back({stmt.target_label, jump_idx});
    }
}

void Compiler::compile_stmt_node(const ast::Defer_stmt &stmt)
{
    if (stmt.is_function_scoped) {
        current_ctx_->function_defers.push_back(stmt.call);
    } else {
        current_ctx_->scopes.back().defers.push_back(stmt.call);
    }
}

void Compiler::compile_stmt_node(const ast::Print_stmt &stmt)
{
    uint8_t stream_flag = (stmt.stream == ast::Print_stream::STDERR) ? 1 : 0;
    uint8_t sep_reg = 0;
    bool has_sep = !stmt.sep.empty() && stmt.expressions.size() > 1;

    if (has_sep) {
        sep_reg = allocate_register();
        uint16_t sep_idx = add_constant(Value::make_string(ctx.arena, stmt.sep));
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
        uint16_t end_idx = add_constant(Value::make_string(ctx.arena, stmt.end));
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
    const bool is_static = (stmt.kind == ast::Var_kind::Static || stmt.kind == ast::Var_kind::Static_mut)
        || (ctx.repl_force_global && current_ctx_ != nullptr && current_ctx_->enclosing == nullptr);
    const bool is_function_local_static = is_static && current_ctx_ != nullptr && current_ctx_->enclosing != nullptr;

    // A `static` declared inside a function body must be initialized exactly
    // once across all invocations (and all closures created in the function),
    // so guard the initializer behind a flag global slot.
    std::optional<uint32_t> guard_index;
    size_t jump_to_init_idx = 0;
    size_t skip_jump_idx = 0;

    if (is_function_local_static) {
        guard_index = ctx.registry.next_global_index++;
        uint8_t guard_reg = allocate_register();
        emit(vm::Instruction::make_ri(vm::Opcode::Load_global, guard_reg, *guard_index));
        jump_to_init_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, guard_reg, 0));
        skip_jump_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));
    }

    uint8_t target_reg = 0;

    if (!stmt.initializer.is_null()) {
        target_reg = compile_expr(stmt.initializer);
        // Reading a local returns that local's own register; a `let` binding
        // must own its own slot, so copy the value into a fresh register.
        if (std::holds_alternative<ast::Variable_expr>(ctx.tree.get(stmt.initializer).node)) {
            uint8_t fresh_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, fresh_reg, target_reg, 0));
            target_reg = fresh_reg;
        }
        types::Type_id source_type = ast::get_type(ctx.tree.get(stmt.initializer).node);
        if (source_type != stmt.type) {
            emit_numeric_normalize(target_reg, stmt.type);
        }
    } else {
        target_reg = allocate_register();
    }

    if (is_static) {
        Symbol_id sym_id = stmt.resolved_symbol.value_or(Symbol_id::null());

        if (sym_id.is_null()) {
            std::string canonical_name = current_module_ns_ + "::" + stmt.name;
            if (current_module_ns_ == "main" || current_module_ns_.empty()) {
                canonical_name = stmt.name;
            }
            sym_id = ctx.registry.lookup_global(canonical_name).value_or(Symbol_id::null());
        }

        if (!sym_id.is_null()) {
            auto &sym = ctx.registry.get_symbol(sym_id);
            // Constants carry no global slot; their value is baked in by the
            // expression compiler when the name is used.
            if (sym.global_index.has_value()) {
                emit(vm::Instruction::make_ri(vm::Opcode::Store_global, target_reg, *sym.global_index));
                if (is_function_local_static) {
                    uint8_t flag_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Load_true, flag_reg, 0, 0));
                    emit(vm::Instruction::make_ri(vm::Opcode::Store_global, flag_reg, *guard_index));
                }
            }
        }
    } else if (stmt.kind == ast::Var_kind::Const) {
        // Do nothing! Constants are baked into the bytecode when used
    } else {
        current_ctx_->locals.push_back({stmt.name, target_reg});
    }

    if (is_function_local_static) {
        current_block().instructions[jump_to_init_idx].ri.imm = static_cast<uint16_t>(skip_jump_idx + 1);
        current_block().instructions[skip_jump_idx].i.imm = static_cast<uint32_t>(current_block().instructions.size());
    }
}

void Compiler::compile_stmt_node(const ast::Multi_var_stmt &stmt)
{
    const bool is_static = (stmt.kind == ast::Var_kind::Static || stmt.kind == ast::Var_kind::Static_mut)
        || (ctx.repl_force_global && current_ctx_ != nullptr && current_ctx_->enclosing == nullptr);
    const bool is_function_local_static = is_static && current_ctx_ != nullptr && current_ctx_->enclosing != nullptr;

    std::optional<uint32_t> guard_index;
    size_t jump_to_init_idx = 0;
    size_t skip_jump_idx = 0;

    if (is_function_local_static) {
        guard_index = ctx.registry.next_global_index++;
        uint8_t guard_reg = allocate_register();
        emit(vm::Instruction::make_ri(vm::Opcode::Load_global, guard_reg, *guard_index));
        jump_to_init_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, guard_reg, 0));
        skip_jump_idx = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));
    }

    auto store_variable = [&](const std::string &name, ast::Var_kind kind, uint8_t value_reg, size_t index) {
        if (is_static) {
            Symbol_id sym_id = (index < stmt.resolved_symbols.size()) ? stmt.resolved_symbols[index] : Symbol_id::null();

            if (sym_id.is_null()) {
                std::string canonical_name = current_module_ns_ + "::" + name;
                if (current_module_ns_ == "main" || current_module_ns_.empty()) {
                    canonical_name = name;
                }
                sym_id = ctx.registry.lookup_global(canonical_name).value_or(Symbol_id::null());
            }

            if (!sym_id.is_null()) {
                auto &sym = ctx.registry.get_symbol(sym_id);
                // Constants carry no global slot; their value is baked in by the
                // expression compiler when the name is used.
                if (sym.global_index.has_value()) {
                    emit(vm::Instruction::make_ri(vm::Opcode::Store_global, value_reg, *sym.global_index));
                    if (is_function_local_static) {
                        uint8_t flag_reg = allocate_register();
                        emit(vm::Instruction::make_rrr(vm::Opcode::Load_true, flag_reg, 0, 0));
                        emit(vm::Instruction::make_ri(vm::Opcode::Store_global, flag_reg, *guard_index));
                    }
                }
            }
        } else if (kind == ast::Var_kind::Const) {
            return;
        } else {
            current_ctx_->locals.push_back({name, value_reg});
        }
    };

    auto finalize_guard = [&]() {
        if (is_function_local_static) {
            current_block().instructions[jump_to_init_idx].ri.imm = static_cast<uint16_t>(skip_jump_idx + 1);
            current_block().instructions[skip_jump_idx].i.imm = static_cast<uint32_t>(current_block().instructions.size());
        }
    };

    if (stmt.initializers.size() == stmt.names.size()) {
        for (size_t i = 0; i < stmt.names.size(); ++i) {
            uint8_t value_reg = compile_expr(stmt.initializers[i]);
            if (ast::get_type(ctx.tree.get(stmt.initializers[i]).node) != stmt.types[i]) {
                emit_numeric_normalize(value_reg, stmt.types[i]);
            }
            store_variable(stmt.names[i], stmt.kind, value_reg, i);
        }
        finalize_guard();
        return;
    }

    if (stmt.initializers.size() == 1) {
        uint8_t source_reg = compile_expr(stmt.initializers.front());
        for (size_t i = 0; i < stmt.names.size(); ++i) {
            uint8_t value_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_field, value_reg, source_reg, static_cast<uint8_t>(i)));
            store_variable(stmt.names[i], stmt.kind, value_reg, i);
        }
    }

    finalize_guard();
}

void Compiler::compile_stmt_node(const ast::Block_stmt &stmt)
{
    push_scope();
    for (const auto &st : stmt.statements) {
        compile_stmt(st);
    }
    pop_scope();
}

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

void Compiler::compile_stmt_node(const ast::While_stmt &stmt)
{
    current_ctx_->loops.push_back({stmt.label, {}, {}, current_ctx_->scopes.size()});

    std::uint32_t loop_start = static_cast<uint32_t>(current_block().instructions.size());

    std::uint8_t condition_reg = compile_expr(stmt.condition);
    size_t exit_jump_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, condition_reg, 0));

    compile_stmt(stmt.body);

    // Patch continue jumps to jump to condition evaluation
    uint32_t continue_target = loop_start;
    for (size_t idx : current_ctx_->loops.back().continue_jumps) {
        current_block().instructions[idx].i.imm = continue_target;
    }

    emit(vm::Instruction::make_i(vm::Opcode::Jump, loop_start));

    // Patch break jumps to jump past the loop
    uint16_t exit_target = static_cast<uint16_t>(current_block().instructions.size());
    current_block().instructions[exit_jump_idx].ri.imm = exit_target;

    for (size_t idx : current_ctx_->loops.back().break_jumps) {
        current_block().instructions[idx].i.imm = exit_target;
    }

    current_ctx_->loops.pop_back();
}

void Compiler::compile_stmt_node(const ast::For_stmt &stmt)
{
    push_scope();

    if (!stmt.initializer.is_null()) {
        compile_stmt(stmt.initializer);
    }

    current_ctx_->loops.push_back({stmt.label, {}, {}, current_ctx_->scopes.size()});

    uint32_t loop_start = static_cast<uint32_t>(current_block().instructions.size());

    size_t exit_jump_idx = -1;
    if (!stmt.condition.is_null()) {
        uint8_t cond_reg = compile_expr(stmt.condition);
        exit_jump_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, cond_reg, 0));
    }

    compile_stmt(stmt.body);

    // Patch continue jumps to jump to the increment expression
    uint32_t continue_target = static_cast<uint32_t>(current_block().instructions.size());
    for (size_t idx : current_ctx_->loops.back().continue_jumps) {
        current_block().instructions[idx].i.imm = continue_target;
    }

    if (!stmt.increment.is_null()) {
        compile_expr(stmt.increment);
    }

    emit(vm::Instruction::make_i(vm::Opcode::Jump, loop_start));

    // Patch break jumps to jump past the loop
    uint16_t exit_target = static_cast<uint16_t>(current_block().instructions.size());
    if (static_cast<int>(exit_jump_idx) != -1) {
        current_block().instructions[exit_jump_idx].ri.imm = exit_target;
    }

    for (size_t idx : current_ctx_->loops.back().break_jumps) {
        current_block().instructions[idx].i.imm = exit_target;
    }

    current_ctx_->loops.pop_back();
    pop_scope();
}

void Compiler::compile_stmt_node(const ast::For_in_stmt &stmt)
{
    push_scope();

    uint8_t iterable_reg = compile_expr(stmt.iterable);
    types::Type_id iterable_type = ast::get_type(ctx.tree.get(stmt.iterable).node);

    bool is_custom_model = ctx.tt.is_model(iterable_type);

    uint8_t loop_var_reg = allocate_register();
    current_ctx_->locals.push_back({stmt.var_name, loop_var_reg});

    current_ctx_->loops.push_back({stmt.label, {}, {}, current_ctx_->scopes.size()});

    // Custom Model Iterators
    if (is_custom_model) {
        auto &model_name = std::get<types::Model_type>(ctx.tt.get(iterable_type).data).name;
        auto model_data_ptr = ctx.type_env.get_model(model_name);

        uint8_t iter_reg = allocate_register();
        std::string iter_model_name = model_name;

        if (model_data_ptr && model_data_ptr->methods.contains("iter")) {
            if (auto sym_id = ctx.registry.lookup_global(model_name + "::iter")) {
                uint8_t func_reg = load_function_instance(*sym_id, function_locations_.at(*sym_id));

                uint8_t call_base = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base, func_reg, 0));

                uint8_t arg_this = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_this, iterable_reg, 0));

                emit(vm::Instruction::make_rrr(vm::Opcode::Call, iter_reg, call_base, 1));

                auto iter_decl_id = model_data_ptr->methods.at("iter").declaration;
                auto iter_decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(iter_decl_id).node);

                auto iter_return_type = effective_return_type(iter_decl->returns, ctx.tt);
                iter_model_name = std::get<types::Model_type>(ctx.tt.get(iter_return_type).data).name;
            }
        } else {
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, iter_reg, iterable_reg, 0));
        }

        uint8_t step_reg = allocate_register();
        uint16_t one_idx = add_constant(Value(static_cast<int64_t>(1)));
        emit(vm::Instruction::make_ri(vm::Opcode::Load_const, step_reg, one_idx));

        uint32_t loop_start = static_cast<uint32_t>(current_block().instructions.size());

        auto next_decl_id = ctx.type_env.get_model(iter_model_name)->methods.at("next").declaration;
        auto next_decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(next_decl_id).node);
        uint8_t argc = static_cast<uint8_t>(next_decl->parameters.size());

        uint8_t next_func = allocate_register();
        if (auto sym_id = ctx.registry.lookup_global(iter_model_name + "::next")) {
            next_func = load_function_instance(*sym_id, function_locations_.at(*sym_id));
        }

        uint8_t call_base = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base, next_func, 0));

        uint8_t arg_this = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_this, iter_reg, 0));

        if (argc == 2) {
            uint8_t arg_step = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_step, step_reg, 0));
        }

        uint8_t opt_val_reg = allocate_register();

        emit(vm::Instruction::make_rrr(vm::Opcode::Call, opt_val_reg, call_base, argc));

        uint8_t end_reg = allocate_register();
        emit(vm::Instruction::make_rrr(vm::Opcode::Test_nil, end_reg, opt_val_reg, 0));

        size_t jump_body = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, end_reg, 0));
        size_t jump_exit = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

        uint16_t body_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_body].ri.imm = body_target;

        emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, loop_var_reg, opt_val_reg, 0));

        compile_stmt(stmt.body);

        uint32_t continue_target = loop_start;
        for (size_t idx : current_ctx_->loops.back().continue_jumps) {
            current_block().instructions[idx].i.imm = continue_target;
        }

        emit(vm::Instruction::make_i(vm::Opcode::Jump, loop_start));

        uint32_t exit_target = static_cast<uint32_t>(current_block().instructions.size());
        current_block().instructions[jump_exit].i.imm = exit_target;

        for (size_t idx : current_ctx_->loops.back().break_jumps) {
            current_block().instructions[idx].i.imm = exit_target;
        }
    }
    // Builtin Iterators
    else {
        uint8_t iter_reg = allocate_register();

        emit(vm::Instruction::make_rrr(vm::Opcode::Make_iter, iter_reg, iterable_reg, 0));

        uint8_t opt_val_reg = allocate_register();

        uint32_t loop_start = static_cast<uint32_t>(current_block().instructions.size());

        emit(vm::Instruction::make_rrr(vm::Opcode::Iter_next, opt_val_reg, iter_reg, 0));

        uint8_t end_reg = allocate_register();

        emit(vm::Instruction::make_rrr(vm::Opcode::Test_nil, end_reg, opt_val_reg, 0));

        size_t jump_body = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, end_reg, 0));
        size_t jump_exit = emit(vm::Instruction::make_i(vm::Opcode::Jump, 0));

        uint16_t body_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_body].ri.imm = body_target;

        emit(vm::Instruction::make_rrr(vm::Opcode::Unwrap_option, loop_var_reg, opt_val_reg, 0));

        compile_stmt(stmt.body);

        uint32_t continue_target = loop_start;
        for (size_t idx : current_ctx_->loops.back().continue_jumps) {
            current_block().instructions[idx].i.imm = continue_target;
        }

        emit(vm::Instruction::make_i(vm::Opcode::Jump, loop_start));

        uint32_t exit_target = static_cast<uint32_t>(current_block().instructions.size());
        current_block().instructions[jump_exit].i.imm = exit_target;

        for (size_t idx : current_ctx_->loops.back().break_jumps) {
            current_block().instructions[idx].i.imm = exit_target;
        }
    }

    current_ctx_->loops.pop_back();
    pop_scope();
}

void Compiler::compile_stmt_node(const ast::Return_stmt &stmt)
{
    uint8_t ret_reg = 0;

    if (!stmt.expressions.empty()) {
        if (stmt.expressions.size() == 1) {
            ret_reg = compile_expr(stmt.expressions.front());
        } else {
            std::vector<uint8_t> value_regs;
            value_regs.reserve(stmt.expressions.size());
            for (size_t i = 0; i < stmt.expressions.size(); ++i) {
                uint8_t value_reg = compile_expr(stmt.expressions[i]);
                if (current_function_returns_ != nullptr && i < current_function_returns_->size()) {
                    const auto &ret = (*current_function_returns_)[i];
                    if (ast::get_type(ctx.tree.get(stmt.expressions[i]).node) != ret.type) {
                        emit_numeric_normalize(value_reg, ret.type);
                    }
                }
                value_regs.push_back(value_reg);
            }

            uint8_t base_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Move, base_reg, value_regs[0], 0));
            for (size_t i = 1; i < value_regs.size(); ++i) {
                uint8_t next_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, next_reg, value_regs[i], 0));
            }

            ret_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Make_model, ret_reg, base_reg, static_cast<uint8_t>(value_regs.size())));
        }
    } else {
        if (current_function_returns_ != nullptr && !current_function_returns_->empty()) {
            auto normalized_returns = declared_return_types(*current_function_returns_, ctx.tt);
            bool all_named =
                std::all_of(current_function_returns_->begin(), current_function_returns_->end(), [](const auto &ret) { return !ret.name.empty(); });

            if (all_named && normalized_returns.size() == 1) {
                int local_reg = resolve_local(current_ctx_, current_function_returns_->front().name);
                ret_reg = static_cast<uint8_t>(local_reg);
            } else if (all_named && !normalized_returns.empty()) {
                std::vector<uint8_t> source_regs;
                source_regs.reserve(current_function_returns_->size());
                for (const auto &ret : *current_function_returns_) {
                    source_regs.push_back(static_cast<uint8_t>(resolve_local(current_ctx_, ret.name)));
                }

                uint8_t base_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, base_reg, source_regs[0], 0));
                for (size_t i = 1; i < source_regs.size(); ++i) {
                    uint8_t next_reg = allocate_register();
                    emit(vm::Instruction::make_rrr(vm::Opcode::Move, next_reg, source_regs[i], 0));
                }

                ret_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Make_model, ret_reg, base_reg, static_cast<uint8_t>(source_regs.size())));
            } else {
                ret_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, ret_reg, 0, 0));
            }
        } else {
            ret_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_nil, ret_reg, 0, 0));
        }
    }

    emit_defers(0);

    for (auto it = current_ctx_->function_defers.rbegin(); it != current_ctx_->function_defers.rend(); ++it) {
        compile_stmt(*it);
    }

    emit(vm::Instruction::make_rrr(vm::Opcode::Return, ret_reg, 0, 0));
}

void Compiler::compile_stmt_node(const ast::Match_stmt &stmt)
{
    uint8_t subject_reg = compile_expr(stmt.subject);
    types::Type_id subject_type = ast::get_type(ctx.tree.get(stmt.subject).node);

    std::vector<size_t> end_jumps;

    for (const auto &arm : stmt.arms) {
        if (arm.is_wildcard) {
            if (!arm.body.is_null()) {
                compile_stmt(arm.body);
            }
            break;
        }

        uint8_t test_reg = allocate_register();
        auto &pattern_node = ctx.tree.get(arm.pattern).node;

        if (std::holds_alternative<ast::Static_path_expr>(pattern_node) || std::holds_alternative<ast::Enum_member_expr>(pattern_node)) {
            types::Type_id pattern_type = ast::get_type(ctx.tree.get(arm.pattern).node);

            if (ctx.tt.is_union(subject_type)) {
                std::string variant_str;
                if (auto *sp = std::get_if<ast::Static_path_expr>(&pattern_node)) {
                    variant_str = sp->member.lexeme;
                } else if (auto *em = std::get_if<ast::Enum_member_expr>(&pattern_node)) {
                    variant_str = em->member_name;
                }

                uint8_t str_reg = allocate_register();
                uint16_t str_idx = add_constant(Value::make_string(ctx.arena, variant_str));
                emit(vm::Instruction::make_ri(vm::Opcode::Load_const, str_reg, str_idx));
                emit(vm::Instruction::make_rrr(vm::Opcode::Test_union, test_reg, subject_reg, str_reg));
            } else {
                uint8_t pattern_reg = compile_expr(arm.pattern);

                types::Type_id subject_cmp_type = subject_type;
                if (ctx.tt.is_enum(subject_cmp_type)) {
                    subject_cmp_type = ctx.tt.get(subject_cmp_type).as<types::Enum_type>().base;
                }

                types::Type_id pattern_cmp_type = pattern_type;
                if (ctx.tt.is_enum(pattern_cmp_type)) {
                    pattern_cmp_type = ctx.tt.get(pattern_cmp_type).as<types::Enum_type>().base;
                }

                vm::Opcode eq_op = comparison_opcode_for(lex::TokenType::Equal, subject_cmp_type, pattern_cmp_type);
                emit(vm::Instruction::make_rrr(eq_op, test_reg, subject_reg, pattern_reg));
            }
        } else if (auto *range_expr = std::get_if<ast::Range_expr>(&pattern_node)) {
            uint8_t start_reg = compile_expr(range_expr->start);
            uint8_t end_reg = compile_expr(range_expr->end);

            uint8_t gte_reg = allocate_register();
            vm::Opcode gte_op = comparison_opcode_for(lex::TokenType::GreaterEqual, subject_type, subject_type);
            emit(vm::Instruction::make_rrr(gte_op, gte_reg, subject_reg, start_reg));

            uint8_t lt_reg = allocate_register();
            vm::Opcode lt_op =
                comparison_opcode_for(range_expr->inclusive ? lex::TokenType::LessEqual : lex::TokenType::Less, subject_type, subject_type);
            emit(vm::Instruction::make_rrr(lt_op, lt_reg, subject_reg, end_reg));

            emit(vm::Instruction::make_rrr(vm::Opcode::BitAnd_i64, test_reg, gte_reg, lt_reg));
        } else if (auto *lit = std::get_if<ast::Literal_expr>(&pattern_node); lit && lit->value.is_nil()) {
            emit(vm::Instruction::make_rrr(vm::Opcode::Test_nil, test_reg, subject_reg, 0));
        } else {
            types::Type_id pattern_type = ast::get_type(ctx.tree.get(arm.pattern).node);
            uint8_t pattern_reg = compile_expr(arm.pattern);

            if (ctx.tt.is_model(pattern_type)) {
                auto &model_name = std::get<types::Model_type>(ctx.tt.get(pattern_type).data).name;
                uint8_t func_reg = allocate_register();
                if (auto sym_id = ctx.registry.lookup_global(model_name + "::__match__")) {
                    func_reg = load_function_instance(*sym_id, function_locations_.at(*sym_id));
                }

                uint8_t call_base_reg = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, call_base_reg, func_reg, 0));

                uint8_t arg_this = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_this, pattern_reg, 0));

                uint8_t arg_subject = allocate_register();
                emit(vm::Instruction::make_rrr(vm::Opcode::Move, arg_subject, subject_reg, 0));

                emit(vm::Instruction::make_rrr(vm::Opcode::Call, test_reg, call_base_reg, 2));
            } else {
                vm::Opcode eq_op = comparison_opcode_for(lex::TokenType::Equal, subject_type, pattern_type);
                emit(vm::Instruction::make_rrr(eq_op, test_reg, subject_reg, pattern_reg));
            }
        }

        size_t jump_next_arm_idx = emit(vm::Instruction::make_ri(vm::Opcode::Jump_if_false, test_reg, 0));

        push_scope();

        if (!arm.bind_name.empty() && ctx.tt.is_union(subject_type)) {
            uint8_t payload_reg = allocate_register();
            emit(vm::Instruction::make_rrr(vm::Opcode::Load_union_payload, payload_reg, subject_reg, 0));
            current_ctx_->locals.push_back({arm.bind_name, payload_reg});
        }

        if (!arm.body.is_null()) {
            compile_stmt(arm.body);
        }

        pop_scope();
        end_jumps.push_back(emit(vm::Instruction::make_i(vm::Opcode::Jump, 0)));

        uint16_t next_arm_target = static_cast<uint16_t>(current_block().instructions.size());
        current_block().instructions[jump_next_arm_idx].ri.imm = next_arm_target;
    }

    uint32_t end_target = static_cast<uint32_t>(current_block().instructions.size());
    for (size_t jump_idx : end_jumps) {
        current_block().instructions[jump_idx].i.imm = end_target;
    }
}

// Expressions
} // namespace phos::vm
