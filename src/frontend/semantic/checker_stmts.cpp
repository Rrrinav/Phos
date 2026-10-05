#include "semantic_checker.hpp"

#include "frontend/ast_utils.hpp"

#include <algorithm>
#include <cctype>
#include <format>

namespace phos {

void Semantic_checker::hoist_globals(Module_id mod_id)
{
    auto &module = ctx.workspace.get_module(mod_id);

    for (auto stmt_id : module.ast_roots) {
        if (stmt_id.is_null()) {
            continue;
        }

        if (std::holds_alternative<ast::Function_stmt>(ctx.tree.get(stmt_id).node)) {
            auto &fn = get_stmt<ast::Function_stmt>(ctx.tree, stmt_id);

            std::string canonical_name =
                (module.logical_namespace == "main" || module.logical_namespace.empty()) ? fn.name : module.logical_namespace + "::" + fn.name;

            Symbol sym{
                .id = Symbol_id{0},
                .name = ctx.registry.intern(canonical_name),
                .kind = Symbol_kind::Phos_func,
                .type = ctx.tt.get_unknown(),
                .owner_module = mod_id,
                .is_public = true,
                .const_value = std::nullopt,
                .global_index = std::nullopt,
                .stack_offset = std::nullopt,
                .ffi_index = std::nullopt,
                .declaration = stmt_id};

            Symbol_id sym_id = ctx.registry.create_symbol(std::move(sym));
            fn.resolved_symbol = sym_id;

            module.add_public_symbol(fn.name, sym_id);

            ctx.type_env.functions[canonical_name] = Function_type_data{stmt_id};

        } else if (std::holds_alternative<ast::Model_stmt>(ctx.tree.get(stmt_id).node)) {
            auto &model = get_stmt<ast::Model_stmt>(ctx.tree, stmt_id);

            std::string canonical_name =
                (module.logical_namespace == "main" || module.logical_namespace.empty()) ? model.name : module.logical_namespace + "::" + model.name;

            std::vector<std::pair<std::string, types::Type_id>> tt_fields;
            std::unordered_set<std::string> forbidden_names = {"this"};
            for (const auto &field : model.fields) {
                forbidden_names.insert(field.name);
            }
            for (auto &field : model.fields) {
                if (!field.type_inferred) {
                    continue;
                }
                if (field.default_value.is_null()) {
                    type_error(field.loc, std::format("Field '{}' with inferred type must have an initializer.", field.name));
                } else if (default_expr_uses_forbidden_names(field.default_value, forbidden_names)) {
                    // Reported properly by validate_model_defaults.
                } else {
                    field.type = check_expr(field.default_value, std::nullopt);
                }
            }
            for (const auto &field : model.fields) {
                if (!field.is_static) {
                    tt_fields.push_back({field.name, resolve_type_recursively(field.type, field.loc)});
                }
            }

            ctx.type_env.global_types[canonical_name] = ctx.tt.model(canonical_name, tt_fields);

            Model_type_data m_data;
            for (const auto &field : model.fields) {
                if (field.is_static) {
                    if (field.default_value.is_null()) {
                        type_error(field.loc, "Static field '" + field.name + "' must have an initializer.");
                    } else {
                        m_data.static_fields[field.name] = field.default_value;
                    }
                    m_data.static_field_types[field.name] = resolve_type_recursively(field.type, field.loc);
                } else if (!field.default_value.is_null()) {
                    m_data.field_defaults[field.name] = field.default_value;
                }
            }
            ctx.type_env.model_data[canonical_name] = std::move(m_data);

            Symbol sym{
                .id = Symbol_id{0},
                .name = ctx.registry.intern(canonical_name),
                .kind = Symbol_kind::Model_def,
                .type = ctx.type_env.global_types[canonical_name],
                .owner_module = mod_id,
                .is_public = true,
                .const_value = std::nullopt,
                .global_index = std::nullopt,
                .stack_offset = std::nullopt,
                .ffi_index = std::nullopt,
                .declaration = stmt_id};
            Symbol_id sym_id = ctx.registry.create_symbol(std::move(sym));

            model.resolved_symbol = sym_id;
            module.add_public_symbol(model.name, sym_id);

            for (auto method_id : model.methods) {
                if (method_id.is_null()) {
                    continue;
                }

                if (std::holds_alternative<ast::Function_stmt>(ctx.tree.get(method_id).node)) {
                    auto &m_fn = get_stmt<ast::Function_stmt>(ctx.tree, method_id);

                    std::string canonical_method_name = canonical_name + "::" + m_fn.name;

                    // 1. Create a global Symbol for the method
                    Symbol method_sym{
                        .id = Symbol_id{0},
                        .name = ctx.registry.intern(canonical_method_name),
                        .kind = Symbol_kind::Phos_func,
                        .type = ctx.tt.get_unknown(),
                        .owner_module = mod_id,
                        .is_public = true,
                        .const_value = std::nullopt,
                        .global_index = std::nullopt,
                        .stack_offset = std::nullopt,
                        .ffi_index = std::nullopt,
                        .declaration = method_id};

                    Symbol_id method_sym_id = ctx.registry.create_symbol(std::move(method_sym));
                    m_fn.resolved_symbol = method_sym_id;

                    // 2. Safely bind "this" parameter
                    if (!m_fn.is_static) {
                        ast::Function_param this_param;
                        this_param.name = "this";
                        this_param.type = ctx.type_env.global_types[canonical_name];
                        this_param.is_mut = true;
                        this_param.loc = m_fn.loc;
                        m_fn.parameters.insert(m_fn.parameters.begin(), this_param);
                    }

                    auto original_name = m_fn.name;
                    m_fn.name = canonical_method_name;

                    if (m_fn.is_static) {
                        ctx.type_env.model_data[canonical_name].static_methods[original_name] = Function_type_data{method_id};
                    } else {
                        ctx.type_env.model_data[canonical_name].methods[original_name] = Function_type_data{method_id};
                    }

                    ctx.type_env.functions[m_fn.name] = Function_type_data{method_id};
                }
            }
        } else if (std::holds_alternative<ast::Union_stmt>(ctx.tree.get(stmt_id).node)) {
            auto &un = get_stmt<ast::Union_stmt>(ctx.tree, stmt_id);

            std::string canonical_name =
                (module.logical_namespace == "main" || module.logical_namespace.empty()) ? un.name : module.logical_namespace + "::" + un.name;

            std::vector<std::pair<std::string, types::Type_id>> tt_variants;
            for (const auto &variant : un.variants) {
                tt_variants.push_back({variant.name, variant.type});
            }
            ctx.type_env.global_types[canonical_name] = ctx.tt.union_(canonical_name, tt_variants);

            Union_type_data u_data;
            for (const auto &variant : un.variants) {
                if (!variant.default_value.is_null()) {
                    u_data.variant_defaults[variant.name] = variant.default_value;
                }
            }
            ctx.type_env.union_data[canonical_name] = std::move(u_data);

            Symbol sym{
                .id = Symbol_id{0},
                .name = ctx.registry.intern(canonical_name),
                .kind = Symbol_kind::Union_def,
                .type = ctx.type_env.global_types[canonical_name],
                .owner_module = mod_id,
                .is_public = true,
                .const_value = std::nullopt,
                .global_index = std::nullopt,
                .stack_offset = std::nullopt,
                .ffi_index = std::nullopt,
                .declaration = stmt_id};
            Symbol_id sym_id = ctx.registry.create_symbol(std::move(sym));

            un.resolved_symbol = sym_id;
            module.add_public_symbol(un.name, sym_id);

        } else if (std::holds_alternative<ast::Enum_stmt>(ctx.tree.get(stmt_id).node)) {
            auto &en = get_stmt<ast::Enum_stmt>(ctx.tree, stmt_id);

            std::string canonical_name =
                (module.logical_namespace == "main" || module.logical_namespace.empty()) ? en.name : module.logical_namespace + "::" + en.name;

            ctx.type_env.global_types[canonical_name] = ctx.tt.enum_(canonical_name, en.base_type);

            Enum_type_data e_data;

            bool is_string_enum = ctx.tt.is_string(en.base_type);
            int64_t current_val = 0;

            for (const auto &variant : en.variants) {
                if (variant.second.has_value()) {
                    if (!is_string_enum) {
                        current_val = variant.second->as_int();
                    }
                    e_data.variants[variant.first] = *variant.second;
                } else {
                    if (is_string_enum) {
                        e_data.variants[variant.first] = Value::make_string(ctx.arena, variant.first);
                    } else {
                        e_data.variants[variant.first] = Value(current_val);
                    }
                }

                if (!is_string_enum) {
                    current_val++;
                }
            }

            ctx.type_env.enum_data[canonical_name] = std::move(e_data);

            Symbol sym{
                .id = Symbol_id{0},
                .name = ctx.registry.intern(canonical_name),
                .kind = Symbol_kind::Enum_def,
                .type = ctx.type_env.global_types[canonical_name],
                .owner_module = mod_id,
                .is_public = true,
                .const_value = std::nullopt,
                .global_index = std::nullopt,
                .stack_offset = std::nullopt,
                .ffi_index = std::nullopt,
                .declaration = stmt_id};
            Symbol_id sym_id = ctx.registry.create_symbol(std::move(sym));

            en.resolved_symbol = sym_id;
            module.add_public_symbol(en.name, sym_id);

        } else if (std::holds_alternative<ast::Var_stmt>(ctx.tree.get(stmt_id).node)) {
            auto &var = get_stmt<ast::Var_stmt>(ctx.tree, stmt_id);

            if (var.kind == ast::Var_kind::Let || var.kind == ast::Var_kind::Mut) {
                continue;
            }

            std::string canonical_name =
                (module.logical_namespace == "main" || module.logical_namespace.empty()) ? var.name : module.logical_namespace + "::" + var.name;

            std::optional<Value> const_val = std::nullopt;
            std::optional<uint32_t> global_idx = std::nullopt;
            Symbol_kind kind = Symbol_kind::Global_var;

            if (var.kind == ast::Var_kind::Const) {
                kind = Symbol_kind::Phos_const;
                if (auto *lit = std::get_if<ast::Literal_expr>(&ctx.tree.get(var.initializer).node)) {
                    const_val = lit->value;
                } else {
                    type_error(var.loc, "Constants must be initialized with a primitive literal.");
                }
            } else {
                kind = Symbol_kind::Global_var;
                global_idx = ctx.registry.next_global_index++;
            }

            Symbol sym{
                .id = Symbol_id{0},
                .name = ctx.registry.intern(canonical_name),
                .kind = kind,
                .type = ctx.tt.get_unknown(),
                .owner_module = mod_id,
                .is_public = true,
                .const_value = const_val,
                .global_index = global_idx,
                .stack_offset = std::nullopt,
                .ffi_index = std::nullopt,
                .declaration = stmt_id};

            Symbol_id sym_id = ctx.registry.create_symbol(std::move(sym));
            module.add_public_symbol(var.name, sym_id);
        } else if (std::holds_alternative<ast::Multi_var_stmt>(ctx.tree.get(stmt_id).node)) {
            auto &vars = get_stmt<ast::Multi_var_stmt>(ctx.tree, stmt_id);

            if (vars.kind == ast::Var_kind::Let || vars.kind == ast::Var_kind::Mut) {
                continue;
            }

            for (size_t i = 0; i < vars.names.size(); ++i) {
                std::string canonical_name = (module.logical_namespace == "main" || module.logical_namespace.empty())
                    ? vars.names[i]
                    : module.logical_namespace + "::" + vars.names[i];

                std::optional<Value> const_val = std::nullopt;
                std::optional<uint32_t> global_idx = std::nullopt;
                Symbol_kind kind = Symbol_kind::Global_var;

                if (vars.kind == ast::Var_kind::Const) {
                    kind = Symbol_kind::Phos_const;
                    if (vars.initializers.size() == vars.names.size()) {
                        if (auto *lit = std::get_if<ast::Literal_expr>(&ctx.tree.get(vars.initializers[i]).node)) {
                            const_val = lit->value;
                        } else {
                            type_error(vars.loc, "Constants must be initialized with primitive literals.");
                        }
                    } else {
                        type_error(vars.loc, "Constants do not support destructuring from a runtime multi-value initializer.");
                    }
                } else {
                    global_idx = ctx.registry.next_global_index++;
                }

                Symbol sym{
                    .id = Symbol_id{0},
                    .name = ctx.registry.intern(canonical_name),
                    .kind = kind,
                    .type = ctx.tt.get_unknown(),
                    .owner_module = mod_id,
                    .is_public = true,
                    .const_value = const_val,
                    .global_index = global_idx,
                    .stack_offset = std::nullopt,
                    .ffi_index = std::nullopt,
                    .declaration = stmt_id};

                Symbol_id sym_id = ctx.registry.create_symbol(std::move(sym));
                module.add_public_symbol(vars.names[i], sym_id);
            }
        }
    }
}

void Semantic_checker::check_function_stmt(ast::Stmt_id stmt_id)
{
    auto &fn_stmt = get_stmt<ast::Function_stmt>(ctx.tree, stmt_id);

    std::vector<types::Type_id> param_types;
    std::vector<types::Type_id> return_types;
    label_scopes_.push_back({});

    for (auto &param : fn_stmt.parameters) {
        param.type = resolve_type_recursively(param.type, param.loc);
        param_types.push_back(param.type);
    }
    for (auto &ret : fn_stmt.returns) {
        ret.type = resolve_type_recursively(ret.type, ret.loc);
        return_types.push_back(ret.type);
    }
    return_types = normalize_return_types(return_types, ctx.tt);

    // Nested function declarations (a `fn` inside a function body) bind their
    // name in the enclosing lexical scope as a function value, so references
    // and recursive calls resolve like any other local closure.
    const bool is_nested_function = !fn_stmt.resolved_symbol.has_value();

    if (is_nested_function) {
        declare(fn_stmt.name, ctx.tt.function(param_types, return_types), false, fn_stmt.loc);
        variables.mark_nested_function(fn_stmt.name);
        nested_fn_stack_.push_back(fn_stmt.name);
        function_name_stack_.push_back(fn_stmt.name);
    } else if (fn_stmt.resolved_symbol) {
        ctx.registry.get_symbol(*fn_stmt.resolved_symbol).type = ctx.tt.function(param_types, return_types);
        function_name_stack_.push_back(std::string(ctx.registry.resolve(ctx.registry.get_symbol(*fn_stmt.resolved_symbol).name)));
    }

    auto saved_return_types = current_return_types;
    auto saved_return_params = current_return_params;
    current_return_types = return_types;
    current_return_params = &fn_stmt.returns;

    validate_function_defaults(fn_stmt);

    variables.begin_scope();

    for (const auto &p : fn_stmt.parameters) {
        declare(p.name, p.type, p.is_mut, p.loc);
    }

    for (const auto &ret : fn_stmt.returns) {
        if (!ret.name.empty()) {
            declare(ret.name, ret.type, true, ret.loc);

            if (!ret.default_value.is_null()) {
                auto def_type = check_expr(ret.default_value, ret.type);
                if (!is_compatible(ret.type, def_type)) {
                    auto diagnostic =
                        err::msg::error(diagnostics_.phase(), ret.loc.l, ret.loc.c, ret.loc.file, "Default return value type mismatch.");
                    diagnostic.expected_got(ctx.tt.to_string(ret.type), ctx.tt.to_string(def_type));
                    diagnostics_.push(std::move(diagnostic));
                }
            }
        }
    }

    if (!fn_stmt.body.is_null()) {
        check_stmt(fn_stmt.body);
    }

    auto current_labels = label_scopes_.back();
    label_scopes_.pop_back();
    for (const auto &[goto_target, loc] : current_labels.gotos) {
        if (!current_labels.defined_labels.contains(goto_target)) {
            type_error(loc, std::format("Unresolved goto label '@{}'.", goto_target));
        }
    }

    variables.end_scope();
    current_return_types = saved_return_types;
    current_return_params = saved_return_params;

    if (is_nested_function && !nested_fn_stack_.empty()) {
        nested_fn_stack_.pop_back();
        function_name_stack_.pop_back();
    } else if (!is_nested_function && !function_name_stack_.empty()) {
        function_name_stack_.pop_back();
    }
}

ast::Expr_id Semantic_checker::synthesize_default_initializer(types::Type_id type, const ast::Source_location &loc)
{
    // Only optionals may start out nil. Models auto-construct from their field
    // defaults, numeric/bool primitives zero, strings become empty, and
    // everything else (closures, unions, enums, ...) stays nil as before.
    if (ctx.tt.is_model(type)) {
        const auto &model_type = ctx.tt.get(type).as<types::Model_type>();
        return ctx.tree.add_expr(ast::Expr{ast::Model_literal_expr{
            .model_name = model_type.name,
            .fields = {},
            .type = ctx.tt.get_unknown(),
            .loc = loc,
        }});
    }
    if (ctx.tt.is_optional(type)) {
        return ctx.tree.add_expr(ast::Expr{ast::Literal_expr{
            .value = Value(),
            .type = type,
            .loc = loc,
        }});
    }
    if (ctx.tt.is_numeric_primitive(type)) {
        return ctx.tree.add_expr(ast::Expr{ast::Literal_expr{
            .value = Value(static_cast<int64_t>(0)),
            .type = type,
            .loc = loc,
        }});
    }
    if (ctx.tt.is_bool(type)) {
        return ctx.tree.add_expr(ast::Expr{ast::Literal_expr{
            .value = Value(false),
            .type = type,
            .loc = loc,
        }});
    }
    if (ctx.tt.is_string(type)) {
        return ctx.tree.add_expr(ast::Expr{ast::Literal_expr{
            .value = Value::make_string(ctx.arena, ""),
            .type = type,
            .loc = loc,
        }});
    }
    if (ctx.tt.is_array(type)) {
        return ctx.tree.add_expr(ast::Expr{ast::Array_literal_expr{
            .elements = {},
            .type = type,
            .loc = loc,
        }});
    }
    return ctx.tree.add_expr(ast::Expr{ast::Literal_expr{
        .value = Value(),
        .type = type,
        .loc = loc,
    }});
}

void Semantic_checker::check_var_stmt(ast::Stmt_id stmt_id)
{
    auto type_inferred = get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).type_inferred;
    auto type = get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).type;
    auto loc = get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).loc;
    auto initializer = get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).initializer;
    auto name = get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).name;
    auto kind = get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).kind;

    if (!type_inferred) {
        type = resolve_type_recursively(type, loc);
        get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).type = type;
    }

    // A bare declaration with no initializer: synthesize a default value so
    // only optionals ever start out nil (models auto-construct).
    if (initializer.is_null() && !type_inferred) {
        initializer = synthesize_default_initializer(type, loc);
        get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).initializer = initializer;
    }

    types::Type_id init_type = ctx.tt.get_void();
    bool initializer_failed = false;

    if (!initializer.is_null()) {
        init_type = check_expr(initializer, type_inferred ? std::nullopt : std::make_optional(type));
        if (ctx.tt.is_unknown(init_type)) {
            initializer_failed = true;
        }
    }

    if (type_inferred) {
        if (initializer_failed) {
            get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).type = ctx.tt.get_unknown();
            return;
        }
        if (ctx.tt.is_array(init_type) && ctx.tt.get_array_elem(init_type) == ctx.tt.get_void()) {
            type_error(loc, "Cannot infer type of an empty array initializer.");
        }
        type = init_type;
        get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).type = type;
    } else if (!initializer.is_null() && !initializer_failed && !is_compatible(type, init_type)) {
        if (std::holds_alternative<ast::Literal_expr>(ctx.tree.get(initializer).node)) {
            auto &lit = get_node<ast::Literal_expr>(ctx.tree, initializer);
            if (ctx.tt.is_numeric_primitive(type) && ctx.tt.is_numeric_primitive(init_type)) {
                auto target_kind = ctx.tt.get_primitive(type);

                if ((lit.value.is_integer() && types::is_float_primitive(target_kind))
                    || (lit.value.is_float() && !types::is_float_primitive(target_kind))) {
                    type_error(
                        loc,
                        std::format(
                            "Numeric literal '{}' cannot be implicitly converted to '{}'; use an explicit cast.",
                            lit.value.to_string(),
                            ctx.tt.to_string(type)));
                } else {
                    type_error(
                        loc,
                        std::format(
                            "Numeric literal '{}' does not fit in target type '{}'. Use an explicit cast: `(value as {})` (wrapping) or "
                            "`(value sat {})` (saturating).",
                            lit.value.to_string(),
                            ctx.tt.to_string(type),
                            ctx.tt.to_string(type),
                            ctx.tt.to_string(type)));
                }
            } else {
                auto diagnostic = err::msg::error(diagnostics_.phase(), loc.l, loc.c, loc.file, "Initializer type mismatch.");
                diagnostic.expected_got(ctx.tt.to_string(type), ctx.tt.to_string(init_type));
                diagnostics_.push(std::move(diagnostic));
            }
        } else {
            report_type_mismatch(loc, type, init_type, "Initializer type mismatch.");
        }
    }
    bool is_global = (kind == ast::Var_kind::Const || kind == ast::Var_kind::Static || kind == ast::Var_kind::Static_mut)
        || (ctx.repl_force_global && function_name_stack_.empty());
    bool is_mut = (kind == ast::Var_kind::Mut || kind == ast::Var_kind::Static_mut);

    if (is_global) {
        auto &module = ctx.workspace.get_module(current_module_id);
        Symbol_id sym_id = Symbol_id::null();

        if (!function_name_stack_.empty() || ctx.repl_force_global) {
            // A `static` declared inside a function body: allocate a dedicated
            // global slot so every invocation (and every closure created in the
            // function) refers to one shared instance, like a C static local.
            // Module-level Const/Static/Static_mut were already registered by
            // hoist_globals; reuse that symbol and adopt the resolved type.
            if (function_name_stack_.empty()
                && (kind == ast::Var_kind::Const || kind == ast::Var_kind::Static || kind == ast::Var_kind::Static_mut)) {
                if (auto found = module.resolve_exported_symbol(name)) {
                    sym_id = *found;
                    ctx.registry.get_symbol(sym_id).type = type;
                    ctx.registry.get_symbol(sym_id).is_mut = is_mut;
                }
            }

            if (sym_id.is_null()) {
                std::string local_canonical =
                    (module.logical_namespace.empty() || module.logical_namespace == "main") ? "" : module.logical_namespace + "::";
                for (const auto &fn_name : function_name_stack_) {
                    local_canonical += fn_name + "::";
                }
                local_canonical += name;

                std::optional<Value> const_val = std::nullopt;
                std::optional<uint32_t> global_idx = std::nullopt;
                Symbol_kind sym_kind = Symbol_kind::Global_var;

                if (kind == ast::Var_kind::Const) {
                    sym_kind = Symbol_kind::Phos_const;
                    if (!initializer.is_null()) {
                        if (auto *lit = std::get_if<ast::Literal_expr>(&ctx.tree.get(initializer).node)) {
                            const_val = lit->value;
                        } else {
                            type_error(loc, "Constants must be initialized with a primitive literal.");
                        }
                    } else {
                        type_error(loc, "Constants must be initialized with a primitive literal.");
                    }
                } else {
                    global_idx = ctx.registry.next_global_index++;
                }

                Symbol sym{
                    .id = Symbol_id{0},
                    .name = ctx.registry.intern(local_canonical),
                    .kind = sym_kind,
                    .type = type,
                    .owner_module = current_module_id,
                    .is_public = false,
                    .const_value = const_val,
                    .global_index = global_idx,
                    .stack_offset = std::nullopt,
                    .ffi_index = std::nullopt,
                    .is_mut = is_mut,
                    .declaration = stmt_id};

                sym_id = ctx.registry.create_symbol(std::move(sym));
            }
        } else if (auto found = module.resolve_exported_symbol(name)) {
            sym_id = *found;
            ctx.registry.get_symbol(sym_id).type = type;
        }

        if (!sym_id.is_null()) {
            get_stmt<ast::Var_stmt>(ctx.tree, stmt_id).resolved_symbol = sym_id;
            variables.declare(name, type, is_mut, sym_id);
        }
    } else {
        declare(name, type, is_mut, loc);
    }

    // A binding initialized from a nested `fn` statement value keeps that
    // function from escaping through an alias; taint it identically.
    if (!initializer.is_null() && !initializer_failed && expr_references_nested_function(initializer)) {
        variables.mark_nested_function_value(name);
    }
}

void Semantic_checker::check_multi_var_stmt(ast::Stmt_id stmt_id)
{
    auto &stmt = get_stmt<ast::Multi_var_stmt>(ctx.tree, stmt_id);

    if (!stmt.type_inferred) {
        for (auto &type : stmt.types) {
            type = resolve_type_recursively(type, stmt.loc);
        }
    }

    const bool is_global = (stmt.kind == ast::Var_kind::Const || stmt.kind == ast::Var_kind::Static || stmt.kind == ast::Var_kind::Static_mut)
        || (ctx.repl_force_global && function_name_stack_.empty());
    const bool is_mut = (stmt.kind == ast::Var_kind::Mut || stmt.kind == ast::Var_kind::Static_mut);

    std::vector<types::Type_id> final_types(stmt.names.size(), ctx.tt.get_unknown());

    auto declare_name = [&](size_t index, types::Type_id type) {
        final_types[index] = type;
        if (is_global) {
            auto &module = ctx.workspace.get_module(current_module_id);
            Symbol_id sym_id = Symbol_id::null();

            if (!function_name_stack_.empty() || ctx.repl_force_global) {
                // Module-level Const/Static/Static_mut were already registered
                // by hoist_globals; reuse that symbol and adopt the resolved type.
                if (function_name_stack_.empty()
                    && (stmt.kind == ast::Var_kind::Const || stmt.kind == ast::Var_kind::Static || stmt.kind == ast::Var_kind::Static_mut)) {
                    if (auto found = module.resolve_exported_symbol(stmt.names[index])) {
                        sym_id = *found;
                        ctx.registry.get_symbol(sym_id).type = type;
                        ctx.registry.get_symbol(sym_id).is_mut = is_mut;
                    }
                }

                if (sym_id.is_null()) {
                    // Function-local static: shared global slot, lexically scoped.
                    std::string local_canonical =
                        (module.logical_namespace.empty() || module.logical_namespace == "main") ? "" : module.logical_namespace + "::";
                    for (const auto &fn_name : function_name_stack_) {
                        local_canonical += fn_name + "::";
                    }
                    local_canonical += stmt.names[index];

                    std::optional<Value> const_val = std::nullopt;
                    std::optional<uint32_t> global_idx = std::nullopt;
                    Symbol_kind sym_kind = Symbol_kind::Global_var;

                    if (stmt.kind == ast::Var_kind::Const) {
                        sym_kind = Symbol_kind::Phos_const;
                        if (stmt.initializers.size() == stmt.names.size()) {
                            if (auto *lit = std::get_if<ast::Literal_expr>(&ctx.tree.get(stmt.initializers[index]).node)) {
                                const_val = lit->value;
                            } else {
                                type_error(stmt.loc, "Constants must be initialized with primitive literals.");
                            }
                        } else {
                            type_error(stmt.loc, "Constants do not support destructuring from a runtime multi-value initializer.");
                        }
                    } else {
                        global_idx = ctx.registry.next_global_index++;
                    }

                    Symbol sym{
                        .id = Symbol_id{0},
                        .name = ctx.registry.intern(local_canonical),
                        .kind = sym_kind,
                        .type = type,
                        .owner_module = current_module_id,
                        .is_public = false,
                        .const_value = const_val,
                        .global_index = global_idx,
                        .stack_offset = std::nullopt,
                        .ffi_index = std::nullopt,
                        .is_mut = is_mut,
                        .declaration = stmt_id};

                    sym_id = ctx.registry.create_symbol(std::move(sym));
                }
            } else if (auto found = module.resolve_exported_symbol(stmt.names[index])) {
                sym_id = *found;
                ctx.registry.get_symbol(sym_id).type = type;
            }

            if (!sym_id.is_null()) {
                if (stmt.resolved_symbols.size() <= index) {
                    stmt.resolved_symbols.resize(index + 1, Symbol_id::null());
                }
                stmt.resolved_symbols[index] = sym_id;
                variables.declare(stmt.names[index], type, is_mut, sym_id);
            }
        } else {
            declare(stmt.names[index], type, is_mut, stmt.loc);
        }
    };

    // A bare multi-variable declaration with no initializer: synthesize a
    // default value for each declared type so only optionals start out nil.
    if (stmt.initializers.empty() && !stmt.type_inferred) {
        for (size_t i = 0; i < stmt.names.size(); ++i) {
            stmt.initializers.push_back(synthesize_default_initializer(stmt.types[i], stmt.loc));
        }
    }

    if (stmt.initializers.size() == stmt.names.size()) {
        for (size_t i = 0; i < stmt.names.size(); ++i) {
            auto init_type = check_expr(stmt.initializers[i], stmt.type_inferred ? std::nullopt : std::make_optional(stmt.types[i]));
            types::Type_id final_type = stmt.type_inferred ? init_type : stmt.types[i];

            if (stmt.type_inferred) {
                if (ctx.tt.is_array(init_type) && ctx.tt.get_array_elem(init_type) == ctx.tt.get_void()) {
                    type_error(stmt.loc, std::format("Cannot infer type of '{}' from an empty array initializer.", stmt.names[i]));
                }
            } else if (!is_compatible(stmt.types[i], init_type)) {
                report_type_mismatch(stmt.loc, stmt.types[i], init_type, "Initializer type mismatch.");
            }

            declare_name(i, final_type);

            if (expr_references_nested_function(stmt.initializers[i])) {
                variables.mark_nested_function_value(stmt.names[i]);
            }
        }
    } else if (stmt.initializers.size() == 1) {
        auto init_expr = stmt.initializers.front();
        auto source_type = check_expr(init_expr);
        auto &init_node = ctx.tree.get(init_expr).node;

        std::vector<types::Type_id> unpacked_types;
        bool is_valid_unpack = false;

        // 1. MULTI-RETURN FUNCTION UNPACKING
        if (auto *call_expr = std::get_if<ast::Call_expr>(&init_node)) {
            auto callee_type = ast::get_type(ctx.tree.get(call_expr->callee).node);
            if (ctx.tt.is_function(callee_type)) {
                unpacked_types = ctx.tt.get(callee_type).as<types::Function_type>().returns;
                is_valid_unpack = true;
            }
        }
        // 2. MODEL DESTRUCTURING
        else if (ctx.tt.is_model(source_type)) {
            const auto &model = ctx.tt.get(source_type).as<types::Model_type>();
            for (const auto &field : model.fields) {
                unpacked_types.push_back(field.second);
            }
            is_valid_unpack = true;
        }

        if (!is_valid_unpack) {
            type_error(stmt.loc, "A single initializer in a multi-variable declaration must be a multi-value model or a multi-return function call.");
            return;
        }

        if (unpacked_types.size() != stmt.names.size()) {
            type_error(
                stmt.loc,
                std::format("Destructuring arity mismatch. Expected {} value(s), got {}.", stmt.names.size(), unpacked_types.size()));
            return;
        }

        for (size_t i = 0; i < stmt.names.size(); ++i) {
            types::Type_id extracted_type = unpacked_types[i];
            types::Type_id final_type = stmt.type_inferred ? extracted_type : stmt.types[i];

            if (!stmt.type_inferred && !is_compatible(stmt.types[i], extracted_type)) {
                report_type_mismatch(stmt.loc, stmt.types[i], extracted_type, "Initializer type mismatch.");
            }
            declare_name(i, final_type);
        }
    } else if (stmt.initializers.empty()) {
        type_error(stmt.loc, "Multi-variable declarations require an initializer.");
        return;
    } else {
        type_error(stmt.loc, "Unsupported multi-variable declaration shape.");
        return;
    }

    stmt.types = std::move(final_types);
}

void Semantic_checker::check_model_stmt(ast::Stmt_id stmt_id)
{
    auto &model = get_stmt<ast::Model_stmt>(ctx.tree, stmt_id);
    validate_model_defaults(model);

    auto saved_model = current_model_type;

    if (model.resolved_symbol) {
        current_model_type = ctx.registry.get_symbol(*model.resolved_symbol).type;
    } else {
        type_error(model.loc, "Compiler Bug: Model lacks resolved_symbol");
    }

    for (auto &method : model.methods) {
        check_stmt(method);
    }

    current_model_type = saved_model;
}

void Semantic_checker::check_union_stmt(ast::Stmt_id stmt_id)
{
    validate_union_defaults(get_stmt<ast::Union_stmt>(ctx.tree, stmt_id));
}

void Semantic_checker::check_enum_stmt(ast::Stmt_id stmt_id)
{
    auto &en = get_stmt<ast::Enum_stmt>(ctx.tree, stmt_id);

    if (!en.resolved_symbol) {
        type_error(en.loc, "Compiler Bug: Enum lacks resolved_symbol");
        return;
    }

    std::string canonical_name = std::string(ctx.registry.resolve(ctx.registry.get_symbol(*en.resolved_symbol).name));
    auto enum_data_ptr = ctx.type_env.get_enum(canonical_name);

    if (!enum_data_ptr) {
        return;
    }

    std::unordered_set<std::string> used_names;
    std::unordered_set<int64_t> used_ints;
    std::unordered_set<std::string> used_strings;

    for (const auto &variant : en.variants) {
        if (!used_names.insert(variant.first).second) {
            type_error(en.loc, std::format("Duplicate enum variant name '{}'.", variant.first));
            continue;
        }

        auto val = enum_data_ptr->variants.at(variant.first);
        if (val.is_integer()) {
            int64_t i = val.as_int();
            if (!used_ints.insert(i).second) {
                type_error(en.loc, std::format("Duplicate enum value '{}' in variant '{}'. Enum values must be unique.", i, variant.first));
            }
        } else if (val.is_string()) {
            std::string s(val.as_string());
            if (!used_strings.insert(s).second) {
                type_error(en.loc, std::format("Duplicate enum value '\"{}\"' in variant '{}'. Enum values must be unique.", s, variant.first));
            }
        }
    }
}

void Semantic_checker::check_block_stmt(ast::Stmt_id stmt_id)
{
    auto statements = get_stmt<ast::Block_stmt>(ctx.tree, stmt_id).statements;
    variables.begin_scope();
    for (auto s : statements) {
        check_stmt(s);
    }
    variables.end_scope();
}

void Semantic_checker::check_expr_stmt(ast::Stmt_id stmt_id)
{
    auto expression = get_stmt<ast::Expr_stmt>(ctx.tree, stmt_id).expression;
    if (!expression.is_null()) {
        check_expr(expression);
    }
}

void Semantic_checker::check_if_stmt(ast::Stmt_id stmt_id)
{
    auto condition = get_stmt<ast::If_stmt>(ctx.tree, stmt_id).condition;
    auto then_branch = get_stmt<ast::If_stmt>(ctx.tree, stmt_id).then_branch;
    auto else_branch = get_stmt<ast::If_stmt>(ctx.tree, stmt_id).else_branch;

    auto condition_type = check_expr(condition);

    if (!ctx.tt.is_bool(condition_type) && !ctx.tt.is_optional(condition_type) && !ctx.tt.is_unknown(condition_type)) {
        type_error(ast::get_loc(ctx.tree.get(condition).node), "If condition must be a boolean or optional.");
    }

    std::unordered_set<Access_path, Access_path_hash> narrowed_in_then, narrowed_in_else;
    collect_nil_checked_vars_for_then(condition, narrowed_in_then);
    collect_nil_checked_vars_for_else(condition, narrowed_in_else);

    variables.begin_scope();
    m_nil_checked_vars_stack.emplace_back();

    for (const auto &path : narrowed_in_then) {
        m_nil_checked_vars_stack.back().insert(path);
    }

    if (!then_branch.is_null()) {
        check_stmt(then_branch);
    }

    m_nil_checked_vars_stack.pop_back();
    variables.end_scope();

    variables.begin_scope();
    m_nil_checked_vars_stack.emplace_back();
    for (const auto &path : narrowed_in_else) {
        m_nil_checked_vars_stack.back().insert(path);
    }

    if (!else_branch.is_null()) {
        check_stmt(else_branch);
    }

    m_nil_checked_vars_stack.pop_back();
    variables.end_scope();
}

void Semantic_checker::check_print_stmt(ast::Stmt_id stmt_id)
{
    auto expressions = get_stmt<ast::Print_stmt>(ctx.tree, stmt_id).expressions;
    for (auto ex : expressions) {
        check_expr(ex);
    }
}

bool Semantic_checker::expr_references_nested_function(ast::Expr_id expr_id, bool check_callees) const
{
    if (expr_id.is_null()) {
        return false;
    }

    // A name refers to a nested `fn` statement (declared inside a function
    // body) when the innermost binding with that name is such a function, or a
    // binding that was initialized/assigned from its value (tainted alias).
    // Binding lookups fall through the whole scope chain.
    auto is_nested = [&](const std::string &name) {
        if (name.empty()) {
            return false;
        }
        auto sym = variables.lookup(name);
        return sym.has_value() && (sym->is_nested_function || sym->nested_fn_value);
    };

    return std::visit(
        [&](const auto &node) -> bool {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, ast::Variable_expr>) {
                return is_nested(node.name);
            } else if constexpr (std::is_same_v<T, ast::Binary_expr>) {
                return expr_references_nested_function(node.left, check_callees) || expr_references_nested_function(node.right, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Unary_expr>) {
                return expr_references_nested_function(node.right, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Call_expr>) {
                // A call evaluated now (`ret()`) returns a normal value, so the
                // callee does not escape. Inside deferred contexts (closure
                // bodies) the call runs later, so the callee is captured.
                if (check_callees && expr_references_nested_function(node.callee, check_callees)) {
                    return true;
                }
                for (const auto &arg : node.arguments) {
                    if (expr_references_nested_function(arg.value, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Assignment_expr>) {
                return expr_references_nested_function(node.value, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Field_assignment_expr>) {
                return expr_references_nested_function(node.object, check_callees) || expr_references_nested_function(node.value, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>) {
                return expr_references_nested_function(node.array, check_callees) || expr_references_nested_function(node.index, check_callees)
                    || expr_references_nested_function(node.value, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Cast_expr>) {
                return expr_references_nested_function(node.expression, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Field_access_expr>) {
                return expr_references_nested_function(node.object, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Method_call_expr>) {
                if (expr_references_nested_function(node.object, check_callees)) {
                    return true;
                }
                for (const auto &arg : node.arguments) {
                    if (expr_references_nested_function(arg.value, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Model_literal_expr>) {
                for (const auto &field : node.fields) {
                    if (expr_references_nested_function(field.second, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Anon_model_literal_expr>) {
                for (const auto &field : node.fields) {
                    if (expr_references_nested_function(field.second, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Closure_expr>) {
                // A closure may smuggle a nested function out through its body. Its body
                // runs later, so even plain calls to a nested function count.
                return statement_references_nested_function(node.body, true);
            } else if constexpr (std::is_same_v<T, ast::Array_literal_expr>) {
                for (auto element : node.elements) {
                    if (expr_references_nested_function(element, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Array_access_expr>) {
                return expr_references_nested_function(node.array, check_callees) || expr_references_nested_function(node.index, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Static_path_expr>) {
                return expr_references_nested_function(node.base, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Range_expr>) {
                return expr_references_nested_function(node.start, check_callees) || expr_references_nested_function(node.end, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Spawn_expr>) {
                return expr_references_nested_function(node.call, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Await_expr>) {
                return expr_references_nested_function(node.thread, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Yield_expr>) {
                return expr_references_nested_function(node.value, check_callees);
            }
            return false;
        },
        ctx.tree.get(expr_id).node);
}

bool Semantic_checker::statement_references_nested_function(ast::Stmt_id stmt_id, bool check_callees) const
{
    if (stmt_id.is_null()) {
        return false;
    }

    return std::visit(
        [&](const auto &stmt) -> bool {
            using T = std::decay_t<decltype(stmt)>;
            if constexpr (std::is_same_v<T, ast::Return_stmt>) {
                for (auto expr : stmt.expressions) {
                    if (expr_references_nested_function(expr, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Expr_stmt>) {
                return expr_references_nested_function(stmt.expression, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Block_stmt>) {
                for (auto child : stmt.statements) {
                    if (statement_references_nested_function(child, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Var_stmt>) {
                return !stmt.initializer.is_null() && expr_references_nested_function(stmt.initializer, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Multi_var_stmt>) {
                for (auto init : stmt.initializers) {
                    if (expr_references_nested_function(init, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Print_stmt>) {
                for (auto expr : stmt.expressions) {
                    if (expr_references_nested_function(expr, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::If_stmt>) {
                return expr_references_nested_function(stmt.condition, check_callees)
                    || statement_references_nested_function(stmt.then_branch, check_callees)
                    || statement_references_nested_function(stmt.else_branch, check_callees);
            } else if constexpr (std::is_same_v<T, ast::While_stmt>) {
                return expr_references_nested_function(stmt.condition, check_callees)
                    || statement_references_nested_function(stmt.body, check_callees);
            } else if constexpr (std::is_same_v<T, ast::For_stmt>) {
                return statement_references_nested_function(stmt.initializer, check_callees)
                    || expr_references_nested_function(stmt.condition, check_callees)
                    || expr_references_nested_function(stmt.increment, check_callees)
                    || statement_references_nested_function(stmt.body, check_callees);
            } else if constexpr (std::is_same_v<T, ast::For_in_stmt>) {
                return expr_references_nested_function(stmt.iterable, check_callees)
                    || statement_references_nested_function(stmt.body, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Match_stmt>) {
                if (expr_references_nested_function(stmt.subject, check_callees)) {
                    return true;
                }
                for (const auto &arm : stmt.arms) {
                    if (expr_references_nested_function(arm.pattern, check_callees)
                        || statement_references_nested_function(arm.body, check_callees)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Defer_stmt>) {
                return statement_references_nested_function(stmt.call, check_callees);
            } else if constexpr (std::is_same_v<T, ast::Function_stmt>) {
                // A function *declared* inside a closure body is itself a nested
                // function; references inside it to an outer nested function are
                // captured through it.
                return statement_references_nested_function(stmt.body, check_callees);
            }
            return false;
        },
        ctx.tree.get(stmt_id).node);
}

void Semantic_checker::check_return_stmt(ast::Stmt_id stmt_id)
{
    auto expressions = get_stmt<ast::Return_stmt>(ctx.tree, stmt_id).expressions;
    auto loc = get_stmt<ast::Return_stmt>(ctx.tree, stmt_id).loc;

    if (!current_return_types) {
        type_error(loc, "Return statement used outside of a function");
        return;
    }

    // A named `fn` declared inside a function body can capture the enclosing
    // scope while it stays inside, but the function value itself cannot be
    // captured by anything that escapes (returned, aliased out, absorbed by a
    // returned closure). Reject any such escape and point the user at the
    // anonymous-closure form.
    for (auto expr : expressions) {
        if (expr_references_nested_function(expr)) {
            type_error(
                ast::get_loc(ctx.tree.get(expr).node),
                "A function declared inside another function cannot escape the enclosing function: it cannot be returned or captured "
                "by anything that is. Assign an anonymous function to a variable to return a closure with captures: "
                "`let x := fn(args) -> return_type { body }`.");
        }
    }

    auto expected_types = normalize_return_types(*current_return_types, ctx.tt);
    auto expected_value_type = effective_return_type(expected_types, ctx.tt);

    if (expressions.empty()) {
        if (expected_types.empty()) {
            return; // Void function, perfectly fine
        }

        if (current_return_params != nullptr) {
            bool all_have_defaults = true;
            for (size_t i = 0; i < current_return_params->size(); ++i) {
                const auto &ret = (*current_return_params)[i];
                if (ret.default_value.is_null()) {
                    std::string param_name = ret.name.empty() ? std::format("at index {}", i) : std::format("'{}'", ret.name);
                    type_error(loc, std::format("Naked return not allowed: Return parameter {} has no default value.", param_name));
                    all_have_defaults = false;
                }
            }
            if (all_have_defaults) {
                return; // All have defaults, naked return is safe!
            }
        } else {
            type_error(loc, "Function must return a value.");
        }
        return;
    }

    if (expected_types.empty()) {
        type_error(loc, "Void functions cannot return a value.");
        for (auto expr : expressions) {
            check_expr(expr);
        }
        return;
    }

    if (expressions.size() == 1) {
        auto val_type = check_expr(expressions.front(), expected_value_type);
        if (!is_compatible(expected_value_type, val_type)) {
            report_type_mismatch(loc, expected_value_type, val_type, "Return type mismatch.");
        }
        return;
    }

    if (expressions.size() != expected_types.size()) {
        type_error(loc, std::format("Return arity mismatch. Expected {} value(s), got {}.", expected_types.size(), expressions.size()));
        for (auto expr : expressions) {
            check_expr(expr);
        }
        return;
    }

    for (size_t i = 0; i < expressions.size(); ++i) {
        auto val_type = check_expr(expressions[i], expected_types[i]);
        if (!is_compatible(expected_types[i], val_type)) {
            auto expr_loc = ast::get_loc(ctx.tree.get(expressions[i]).node);
            report_type_mismatch(expr_loc, expected_types[i], val_type, "Return type mismatch.");
        }
    }
}

void Semantic_checker::check_stmt_node(const ast::Break_stmt &stmt)
{
    if (loop_label_stack_.empty()) {
        type_error(stmt.loc, "Cannot use 'break' outside of a loop.");
        return;
    }

    if (!stmt.target_label.empty()) {
        bool found = false;
        for (const auto &lbl : loop_label_stack_) {
            if (lbl == stmt.target_label) {
                found = true;
                break;
            }
        }
        if (!found) {
            type_error(stmt.loc, std::format("Cannot break to unknown loop label '@{}'.", stmt.target_label));
        }
    }
}

void Semantic_checker::check_stmt_node(const ast::Continue_stmt &stmt)
{
    if (loop_label_stack_.empty()) {
        type_error(stmt.loc, "Cannot use 'continue' outside of a loop.");
        return;
    }

    if (!stmt.target_label.empty()) {
        bool found = false;
        for (const auto &lbl : loop_label_stack_) {
            if (lbl == stmt.target_label) {
                found = true;
                break;
            }
        }
        if (!found) {
            type_error(stmt.loc, std::format("Cannot continue to unknown loop label '@{}'.", stmt.target_label));
        }
    }
}

void Semantic_checker::check_stmt_node(const ast::Defer_stmt &stmt)
{
    // Simply type-check the deferred statement exactly as if it were executed here.
    check_stmt(stmt.call);
}

void Semantic_checker::check_stmt_node(const ast::Label_stmt &stmt)
{
    if (!label_scopes_.empty()) {
        label_scopes_.back().defined_labels.insert(stmt.name);
    }
}

void Semantic_checker::check_stmt_node(const ast::Goto_stmt &stmt)
{
    if (!label_scopes_.empty()) {
        label_scopes_.back().gotos.push_back({stmt.target_label, stmt.loc});
    }
}

void Semantic_checker::check_while_stmt(ast::Stmt_id stmt_id)
{
    auto condition = get_stmt<ast::While_stmt>(ctx.tree, stmt_id).condition;
    auto body = get_stmt<ast::While_stmt>(ctx.tree, stmt_id).body;

    loop_label_stack_.push_back(get_stmt<ast::While_stmt>(ctx.tree, stmt_id).label);

    if (!condition.is_null()) {
        auto type = check_expr(condition);
        if (!ctx.tt.is_bool(type) && !ctx.tt.is_unknown(type)) {
            type_error(ast::get_loc(ctx.tree.get(condition).node), "Condition must be a boolean");
        }
    }
    if (!body.is_null()) {
        check_stmt(body);
    }
    loop_label_stack_.pop_back();
}

void Semantic_checker::check_for_stmt(ast::Stmt_id stmt_id)
{
    auto initializer = get_stmt<ast::For_stmt>(ctx.tree, stmt_id).initializer;
    auto condition = get_stmt<ast::For_stmt>(ctx.tree, stmt_id).condition;
    auto increment = get_stmt<ast::For_stmt>(ctx.tree, stmt_id).increment;
    auto body = get_stmt<ast::For_stmt>(ctx.tree, stmt_id).body;

    loop_label_stack_.push_back(get_stmt<ast::For_stmt>(ctx.tree, stmt_id).label);

    variables.begin_scope();
    if (!initializer.is_null()) {
        check_stmt(initializer);
    }
    if (!condition.is_null()) {
        auto type = check_expr(condition);
        if (!ctx.tt.is_bool(type) && !ctx.tt.is_unknown(type)) {
            type_error(ast::get_loc(ctx.tree.get(condition).node), "Condition must be boolean");
        }
    }
    if (!increment.is_null()) {
        check_expr(increment);
    }
    if (!body.is_null()) {
        check_stmt(body);
    }

    loop_label_stack_.pop_back();
    variables.end_scope();
}

void Semantic_checker::check_for_in_stmt(ast::Stmt_id stmt_id)
{
    auto iterable = get_stmt<ast::For_in_stmt>(ctx.tree, stmt_id).iterable;
    auto loc = get_stmt<ast::For_in_stmt>(ctx.tree, stmt_id).loc;
    auto var_name = get_stmt<ast::For_in_stmt>(ctx.tree, stmt_id).var_name;
    auto body = get_stmt<ast::For_in_stmt>(ctx.tree, stmt_id).body;

    // FIX: Use ast::For_in_stmt here!
    loop_label_stack_.push_back(get_stmt<ast::For_in_stmt>(ctx.tree, stmt_id).label);

    auto iterable_type = check_expr(iterable);
    if (ctx.tt.is_unknown(iterable_type) || ctx.tt.is_any(iterable_type)) {
        return;
    }

    auto iter_type = to_iterator_type(iterable_type);
    if (!is_iterator_protocol_type(iter_type)) {
        type_error(loc, "Value in 'for..in' loop is not iterable.");
        return;
    }
    types::Type_id var_type = iterator_element_type(iter_type);

    variables.begin_scope();
    declare(var_name, var_type, false, loc);
    if (!body.is_null()) {
        check_stmt(body);
    }

    loop_label_stack_.pop_back();
    variables.end_scope();
}

void Semantic_checker::check_match_stmt(ast::Stmt_id stmt_id)
{
    auto subject = get_stmt<ast::Match_stmt>(ctx.tree, stmt_id).subject;
    auto loc = get_stmt<ast::Match_stmt>(ctx.tree, stmt_id).loc;
    auto arms = get_stmt<ast::Match_stmt>(ctx.tree, stmt_id).arms;

    types::Type_id subject_type = check_expr(subject);
    if (ctx.tt.is_any(subject_type) || ctx.tt.is_unknown(subject_type)) {
        return;
    }

    bool is_union_subject = ctx.tt.is_union(subject_type);
    bool is_enum_subject = ctx.tt.is_enum(subject_type);
    bool seen_wildcard = false;
    std::unordered_set<std::string> seen_variants;

    auto arm_loc = [&](const ast::Match_arm &arm) -> ast::Source_location {
        if (!arm.pattern.is_null()) {
            return ast::get_loc(ctx.tree.get(arm.pattern).node);
        }
        if (!arm.body.is_null()) {
            return ast::get_loc(ctx.tree.get(arm.body).node);
        }
        return loc;
    };

    auto arm_variant_name = [&](const ast::Match_arm &arm) -> std::optional<std::string> {
        if (arm.pattern.is_null()) {
            return std::nullopt;
        }
        if (std::holds_alternative<ast::Static_path_expr>(ctx.tree.get(arm.pattern).node)) {
            return get_node<ast::Static_path_expr>(ctx.tree, arm.pattern).member.lexeme;
        }
        if (std::holds_alternative<ast::Enum_member_expr>(ctx.tree.get(arm.pattern).node)) {
            return get_node<ast::Enum_member_expr>(ctx.tree, arm.pattern).member_name;
        }
        return std::nullopt;
    };

    for (auto &arm : arms) {
        auto current_arm_loc = arm_loc(arm);
        if (seen_wildcard) {
            type_warning(current_arm_loc, "Unreachable match arm after wildcard '_'.");
        } else if ((is_union_subject || is_enum_subject) && !arm.is_wildcard) {
            if (auto variant_name = arm_variant_name(arm)) {
                if (!seen_variants.insert(*variant_name).second) {
                    type_warning(current_arm_loc, std::format("Unreachable duplicate match arm for variant '{}'.", *variant_name));
                }
            }
        }

        variables.begin_scope();
        if (!arm.is_wildcard) {
            if (is_union_subject
                && (std::holds_alternative<ast::Static_path_expr>(ctx.tree.get(arm.pattern).node)
                    || std::holds_alternative<ast::Enum_member_expr>(ctx.tree.get(arm.pattern).node))) {

                auto &union_t = std::get<types::Union_type>(ctx.tt.get(subject_type).data);
                std::string variant_name;
                ast::Source_location pattern_loc = loc;

                if (std::holds_alternative<ast::Static_path_expr>(ctx.tree.get(arm.pattern).node)) {
                    variant_name = get_node<ast::Static_path_expr>(ctx.tree, arm.pattern).member.lexeme;
                    pattern_loc = get_node<ast::Static_path_expr>(ctx.tree, arm.pattern).loc;
                    get_node<ast::Static_path_expr>(ctx.tree, arm.pattern).type = subject_type;
                } else if (std::holds_alternative<ast::Enum_member_expr>(ctx.tree.get(arm.pattern).node)) {
                    variant_name = get_node<ast::Enum_member_expr>(ctx.tree, arm.pattern).member_name;
                    pattern_loc = get_node<ast::Enum_member_expr>(ctx.tree, arm.pattern).loc;
                    get_node<ast::Enum_member_expr>(ctx.tree, arm.pattern).type = subject_type;
                }

                auto variant_it =
                    std::find_if(union_t.variants.begin(), union_t.variants.end(), [&](const auto &v) { return v.first == variant_name; });
                if (variant_it == union_t.variants.end()) {
                    type_error(pattern_loc, "Variant does not exist in union.");
                } else if (!arm.bind_name.empty()) {
                    if (variant_it->second == ctx.tt.get_void()) {
                        type_error(pattern_loc, "Variant does not hold a payload.");
                    } else {
                        declare(arm.bind_name, variant_it->second, false, loc);
                    }
                }
            } else {
                types::Type_id pattern_type = check_expr(arm.pattern, subject_type);
                if (ctx.tt.is_any(pattern_type) || ctx.tt.is_unknown(pattern_type)) {
                    variables.end_scope();
                    continue;
                }

                bool is_range_match = std::holds_alternative<ast::Range_expr>(ctx.tree.get(arm.pattern).node)
                    && ctx.tt.is_integer_primitive(subject_type);

                bool has_custom_match = false;
                if (ctx.tt.is_model(pattern_type)) {
                    if (auto *model_data = ctx.type_env.get_model(std::get<types::Model_type>(ctx.tt.get(pattern_type).data).name)) {
                        auto method_it = model_data->methods.find("__match__");
                        if (method_it != model_data->methods.end()) {
                            auto decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(method_it->second.declaration).node);
                            if (decl && decl->parameters.size() == 1 && is_compatible(decl->parameters[0].type, subject_type)) {
                                if (effective_return_type(declared_return_types(decl->returns, ctx.tt), ctx.tt) == ctx.tt.get_bool()) {
                                    has_custom_match = true;
                                } else {
                                    type_error(ast::get_loc(ctx.tree.get(arm.pattern).node), "__match__ method must return bool.");
                                }
                            } else {
                                type_error(
                                    ast::get_loc(ctx.tree.get(arm.pattern).node),
                                    "__match__ method must accept one argument of subject type.");
                            }
                        }
                    }
                }

                if (!is_compatible(subject_type, pattern_type) && !is_range_match && !has_custom_match) {
                    type_error(ast::get_loc(ctx.tree.get(arm.pattern).node), "Match pattern type mismatch.");
                }
            }
        }
        if (!arm.body.is_null()) {
            check_stmt(arm.body);
        }
        variables.end_scope();

        if (arm.is_wildcard) {
            seen_wildcard = true;
        }
    }

    bool has_wildcard = std::any_of(arms.begin(), arms.end(), [](const auto &arm) { return arm.is_wildcard; });
    if (has_wildcard) {
        return;
    }

    if (ctx.tt.is_enum(subject_type)) {
        auto &enum_t = std::get<types::Enum_type>(ctx.tt.get(subject_type).data);
        std::unordered_set<std::string> covered;
        for (const auto &arm : arms) {
            if (std::holds_alternative<ast::Static_path_expr>(ctx.tree.get(arm.pattern).node)) {
                covered.insert(get_node<ast::Static_path_expr>(ctx.tree, arm.pattern).member.lexeme);
            } else if (std::holds_alternative<ast::Enum_member_expr>(ctx.tree.get(arm.pattern).node)) {
                covered.insert(get_node<ast::Enum_member_expr>(ctx.tree, arm.pattern).member_name);
            }
        }
        std::vector<std::string> missing;
        auto enum_data_ptr = ctx.type_env.get_enum(enum_t.name);
        if (enum_data_ptr) {
            for (const auto &[name, _] : enum_data_ptr->variants) {
                if (!covered.count(name)) {
                    missing.push_back(name);
                }
            }
        }
        if (!missing.empty()) {
            type_error(loc, "Non-exhaustive match on enum.");
        }
        return;
    }

    if (is_union_subject) {
        auto &union_t = std::get<types::Union_type>(ctx.tt.get(subject_type).data);
        std::unordered_set<std::string> covered;
        for (const auto &arm : arms) {
            if (std::holds_alternative<ast::Static_path_expr>(ctx.tree.get(arm.pattern).node)) {
                covered.insert(get_node<ast::Static_path_expr>(ctx.tree, arm.pattern).member.lexeme);
            } else if (std::holds_alternative<ast::Enum_member_expr>(ctx.tree.get(arm.pattern).node)) {
                covered.insert(get_node<ast::Enum_member_expr>(ctx.tree, arm.pattern).member_name);
            }
        }
        std::vector<std::string> missing;
        for (const auto &v : union_t.variants) {
            if (!covered.count(v.first)) {
                missing.push_back(v.first);
            }
        }
        if (!missing.empty()) {
            type_error(loc, "Non-exhaustive match on union.");
        }
        return;
    }

    type_error(loc, "Non-exhaustive match. Add a wildcard '_' arm.");
}

void Semantic_checker::check_import_stmt(ast::Stmt_id stmt_id)
{
    auto &import_stmt = get_stmt<ast::Import_stmt>(ctx.tree, stmt_id);

    if (import_stmt.selectives.empty()) {
        return;
    }

    // 1. Get the alias for this module (e.g., "create")
    auto &current_module = ctx.workspace.get_module(current_module_id);
    std::string alias = import_stmt.local_alias.empty() ? import_stmt.path.back() : import_stmt.local_alias;

    // 2. Ask the Workspace what Module_id the Resolver assigned to this alias
    auto target_mod_id_opt = current_module.resolve_import(alias);
    if (!target_mod_id_opt) {
        type_error(import_stmt.loc, "Compiler Bug: Module alias '" + alias + "' not resolved by Module_resolver.");
        return;
    }

    auto &target_module = ctx.workspace.get_module(*target_mod_id_opt);

    // 3. Bind the specific requested symbols into the local scope
    for (const auto &sym_name : import_stmt.selectives) {
        auto exported_sym_id = target_module.resolve_exported_symbol(sym_name);
        if (!exported_sym_id) {
            type_error(import_stmt.loc, std::format("Module '{}' does not export symbol '{}'", target_module.logical_namespace, sym_name));
            continue;
        }

        if (variables.is_declared_locally(sym_name)) {
            type_error(import_stmt.loc, std::format("Import conflict: '{}' is already bound in this module.", sym_name));
            continue;
        }

        variables.declare(sym_name, resolve_symbol_type(ctx, *exported_sym_id, *this), false, *exported_sym_id);
    }
}

void Semantic_checker::bind_import_alias(const ast::Import_stmt &stmt, Module_id mod_id)
{
    (void)stmt;
    (void)mod_id;
}

// EXPRESSION VISITORS

} // namespace phos
