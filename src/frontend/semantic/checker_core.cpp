#include "semantic_checker.hpp"

#include "frontend/ast_utils.hpp"

#include <algorithm>
#include <cctype>
#include <format>

namespace phos {

Semantic_checker::Semantic_checker(Compiler_context &ctx) : ctx(ctx)
{}

types::Type_id Semantic_checker::resolve_symbol(Symbol_id id)
{
    return resolve_symbol_type(ctx, id, *this);
}

void Semantic_checker::type_error(const ast::Source_location &loc, const std::string &message)
{
    diagnostics_.error(loc.l, loc.c, loc.file, "{}", message);
}

void Semantic_checker::type_warning(const ast::Source_location &loc, const std::string &message)
{
    diagnostics_.warning(loc.l, loc.c, loc.file, "{}", message);
}

err::Engine Semantic_checker::check_workspace()
{
    // Each call only checks modules that were added since the last call, so
    // the report must contain only this round's diagnostics.
    diagnostics_ = err::Engine{};

    label_scopes_.push_back({});
    for (const auto &[name, sigs] : ctx.type_env.native_signatures) {
        if (!ctx.registry.lookup_global(name)) {
            Symbol sym{
                .id = Symbol_id{0},
                .name = ctx.registry.intern(name),
                .kind = Symbol_kind::Native_func,
                .type = ctx.tt.get_unknown(),
                .owner_module = Module_id{0},
                .is_public = true,
                .const_value = std::nullopt,
                .global_index = std::nullopt,
                .stack_offset = std::nullopt,
                .ffi_index = std::nullopt,
                .declaration = ast::Stmt_id::null()};
            ctx.registry.create_symbol(std::move(sym));
        }
    }
    for (const auto &module : ctx.workspace.modules) {
        if (hoisted_modules.contains(module.id)) {
            continue;
        }
        hoisted_modules.insert(module.id);
        hoist_globals(module.id);
    }

    std::vector<Module_id> build_order = ctx.workspace.get_topological_order();
    for (Module_id mod_id : build_order) {
        if (checked_modules.contains(mod_id)) {
            continue;
        }
        checked_modules.insert(mod_id);
        check_module(mod_id);
    }

    auto global_labels = label_scopes_.back();
    label_scopes_.pop_back();

    for (const auto &[goto_target, loc] : global_labels.gotos) {
        if (!global_labels.defined_labels.contains(goto_target)) {
            type_error(loc, std::format("Unresolved goto label '@{}'.", goto_target));
        }
    }

    return diagnostics_;
}

void Semantic_checker::check_module(Module_id mod_id)
{
    auto &module = ctx.workspace.get_module(mod_id);
    current_module_id = mod_id;

    variables.begin_scope();
    m_nil_checked_vars_stack.emplace_back();

    for (auto stmt_id : module.ast_roots) {
        check_stmt(stmt_id);
    }

    m_nil_checked_vars_stack.pop_back();
    variables.end_scope();

    current_module_id = Module_id::null();
}

err::Engine Semantic_checker::check(const std::vector<ast::Stmt_id> &statements)
{
    (void)statements;

    for (const auto &[name, sigs] : ctx.type_env.native_signatures) {
        if (!ctx.registry.lookup_global(name)) {
            Symbol sym{
                .id = Symbol_id{0},
                .name = ctx.registry.intern(name),
                .kind = Symbol_kind::Native_func,
                .type = ctx.tt.get_unknown(),
                .owner_module = Module_id{0},
                .is_public = true,
                .const_value = std::nullopt,
                .global_index = std::nullopt,
                .stack_offset = std::nullopt,
                .ffi_index = std::nullopt,
                .declaration = ast::Stmt_id::null()};
            ctx.registry.create_symbol(std::move(sym));
        }
    }

    for (const auto &module : ctx.workspace.modules) {
        hoist_globals(module.id);
    }

    for (const auto &module : ctx.workspace.modules) {
        current_module_id = module.id;
        variables.begin_scope();
        m_nil_checked_vars_stack.emplace_back();

        for (auto stmt_id : module.ast_roots) {
            check_stmt(stmt_id);
        }

        m_nil_checked_vars_stack.pop_back();
        variables.end_scope();
    }

    current_module_id = Module_id::null();
    return diagnostics_;
}

void Semantic_checker::declare(const std::string &name, types::Type_id type, bool is_mut, const ast::Source_location &loc)
{
    if (variables.is_declared_locally(name)) {
        type_error(loc, std::format("Variable '{}' is already declared in this scope.", name));
        return;
    }
    variables.declare(name, type, is_mut, Symbol_id::null());
}

std::optional<Scope_symbol> Semantic_checker::lookup(const std::string &name, const ast::Source_location &loc)
{
    if (auto var_opt = variables.lookup(name)) {
        return *var_opt;
    }

    if (auto module_id = imported_module_id(ctx, current_module_id, name)) {
        return Scope_symbol{ctx.tt.get_any(), Symbol_id::null(), false, 0};
    }

    if (ctx.type_env.is_native_defined(name)) {
        types::Type_id dummy = ctx.tt.function({}, ctx.tt.get_any());
        return Scope_symbol{dummy, Symbol_id::null(), false, 0};
    }

    if (auto func_data = ctx.type_env.get_function(name)) {
        const auto *decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(func_data->declaration).node);
        std::vector<types::Type_id> params;
        for (const auto &p : decl->parameters) {
            params.push_back(p.type);
        }
        types::Type_id func_id = ctx.tt.function(params, declared_return_types(decl->returns, ctx.tt));
        return Scope_symbol{func_id, Symbol_id::null(), false, 0};
    }

    // Fall back to registry-backed globals (module statics, REPL bindings) so
    // assignments and other lookup-based paths see them across modules.
    if (auto global_sym_id = ctx.registry.lookup_global(name)) {
        auto &sym = ctx.registry.get_symbol(*global_sym_id);
        if (sym.kind == Symbol_kind::Global_var) {
            return Scope_symbol{resolve_symbol_type(ctx, *global_sym_id, *this), *global_sym_id, sym.is_mut, 0};
        }
    }

    type_error(loc, std::format("Undefined variable, function, or type '{}'", name));
    return std::nullopt;
}

void Semantic_checker::check_stmt(ast::Stmt_id stmt_id)
{
    if (stmt_id.is_null()) {
        return;
    }

    std::visit(
        [this, stmt_id](auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, ast::Function_stmt>) {
                check_function_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Var_stmt>) {
                check_var_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Multi_var_stmt>) {
                check_multi_var_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Model_stmt>) {
                check_model_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Union_stmt>) {
                check_union_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Enum_stmt>) {
                check_enum_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Block_stmt>) {
                check_block_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Expr_stmt>) {
                check_expr_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::If_stmt>) {
                check_if_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Print_stmt>) {
                check_print_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Return_stmt>) {
                check_return_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Break_stmt>) {
                check_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Continue_stmt>) {
                check_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Goto_stmt>) {
                check_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Label_stmt>) {
                check_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::Defer_stmt>) {
                check_stmt_node(s);
            } else if constexpr (std::is_same_v<T, ast::While_stmt>) {
                check_while_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::For_stmt>) {
                check_for_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::For_in_stmt>) {
                check_for_in_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Match_stmt>) {
                check_match_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Import_stmt>) {
                check_import_stmt(stmt_id);
            }
        },
        ctx.tree.get(stmt_id).node);
}

types::Type_id Semantic_checker::check_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    if (expr_id.is_null()) {
        return ctx.tt.get_void();
    }

    std::optional<types::Type_id> peeled_hint = context_type;
    uint8_t expected_depth = 0;

    if (peeled_hint) {
        while (ctx.tt.is_optional(*peeled_hint)) {
            peeled_hint = ctx.tt.get_optional_base(*peeled_hint);
            expected_depth++;
        }
    }

    types::Type_id actual_type = std::visit(
        [this, expr_id, peeled_hint](auto &e) -> types::Type_id {
            using T = std::decay_t<decltype(e)>;
            if constexpr (std::is_same_v<T, ast::Model_literal_expr>) {
                return check_model_literal_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Anon_model_literal_expr>) {
                return check_anon_model_literal_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Assignment_expr>) {
                return check_assignment_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Variable_expr>) {
                return check_variable_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Binary_expr>) {
                return check_binary_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Call_expr>) {
                return check_call_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Cast_expr>) {
                return check_cast_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Closure_expr>) {
                return check_closure_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Field_access_expr>) {
                return check_field_access_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Static_path_expr>) {
                return check_static_path_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Enum_member_expr>) {
                return check_enum_member_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Field_assignment_expr>) {
                return check_field_assignment_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Literal_expr>) {
                return check_literal_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Method_call_expr>) {
                return check_method_call_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Unary_expr>) {
                return check_unary_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Array_literal_expr>) {
                return check_array_literal_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Array_access_expr>) {
                return check_array_access_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>) {
                return check_array_assignment_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Range_expr>) {
                return check_range_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Spawn_expr>) {
                return check_spawn_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Await_expr>) {
                return check_await_expr(expr_id, peeled_hint);
            } else if constexpr (std::is_same_v<T, ast::Yield_expr>) {
                return check_yield_expr(expr_id, peeled_hint);
            }
            return ctx.tt.get_unknown();
        },
        ctx.tree.get(expr_id).node);

    if (context_type) {
        uint8_t actual_depth = 0;
        types::Type_id actual_base = actual_type;
        while (ctx.tt.is_optional(actual_base)) {
            actual_base = ctx.tt.get_optional_base(actual_base);
            actual_depth++;
        }

        if (expected_depth > actual_depth) {
            types::Type_id expected_base = *peeled_hint;

            if (ctx.tt.is_nil(actual_base) || ctx.tt.is_unknown(actual_base)) {
                ast::get_type(ctx.tree.get(expr_id).node) = *context_type;
                return *context_type;
            }

            if (is_compatible(expected_base, actual_base)) {
                ctx.tree.get(expr_id).auto_wrap_depth += (expected_depth - actual_depth);
                ast::get_type(ctx.tree.get(expr_id).node) = *context_type;
                return *context_type;
            }
        }
    }

    ast::get_type(ctx.tree.get(expr_id).node) = actual_type;
    return actual_type;
}

// STATEMENT VISITORS

} // namespace phos
