#include "semantic_checker.hpp"

#include "frontend/ast_utils.hpp"

#include <algorithm>
#include <cctype>
#include <format>

namespace phos {

types::Type_id Semantic_checker::resolve_type_recursively(types::Type_id type_id, const ast::Source_location &loc)
{
    if (ctx.tt.is_unresolved(type_id)) {
        auto type_name = ctx.tt.get(type_id).as<types::Unresolved_type>().name;

        if (ctx.type_env.is_type_defined(type_name)) {
            return *ctx.type_env.get_type(type_name);
        }

        if (!current_module_id.is_null()) {
            std::string current_ns = ctx.workspace.get_module(current_module_id).logical_namespace;
            if (!current_ns.empty() && current_ns != "main") {
                std::string local_canonical = current_ns + "::" + type_name;
                if (ctx.type_env.is_type_defined(local_canonical)) {
                    return *ctx.type_env.get_type(local_canonical);
                }
            }

            if (auto local_sym = variables.lookup(type_name)) {
                if (!local_sym->id.is_null()) {
                    auto &sym = ctx.registry.get_symbol(local_sym->id);
                    if (sym.kind == Symbol_kind::Model_def || sym.kind == Symbol_kind::Enum_def || sym.kind == Symbol_kind::Union_def) {
                        return sym.type;
                    }
                }
            }
        }

        size_t pos = type_name.find("::");
        if (pos != std::string::npos) {
            std::string alias = type_name.substr(0, pos);
            std::string member = type_name.substr(pos + 2);

            if (auto mod_id = imported_module_id(ctx, current_module_id, alias)) {
                auto &target_module = ctx.workspace.get_module(*mod_id);
                if (auto sym_id = target_module.resolve_exported_symbol(member)) {
                    auto &sym = ctx.registry.get_symbol(*sym_id);
                    if (sym.kind == Symbol_kind::Model_def || sym.kind == Symbol_kind::Enum_def || sym.kind == Symbol_kind::Union_def) {
                        return sym.type;
                    }
                }
            }
        }

        type_error(loc, "Unknown type '" + type_name + "'.");
        return ctx.tt.get_unknown();
    }

    if (ctx.tt.is_optional(type_id)) {
        types::Type_id base = ctx.tt.get_optional_base(type_id);
        types::Type_id resolved_base = resolve_type_recursively(base, loc);
        return ctx.tt.optional(resolved_base);
    }

    if (ctx.tt.is_array(type_id)) {
        types::Type_id base = ctx.tt.get_array_elem(type_id);
        types::Type_id resolved_base = resolve_type_recursively(base, loc);
        return ctx.tt.array(resolved_base);
    }

    if (ctx.tt.is_iterator(type_id)) {
        types::Type_id base = ctx.tt.get_iter_elem(type_id);
        types::Type_id resolved_base = resolve_type_recursively(base, loc);
        return ctx.tt.iterator(resolved_base);
    }

    if (ctx.tt.is_function(type_id)) {
        auto func = ctx.tt.get(type_id).as<types::Function_type>();

        for (auto &param_type : func.params) {
            param_type = resolve_type_recursively(param_type, loc);
        }
        for (auto &return_type : func.returns) {
            return_type = resolve_type_recursively(return_type, loc);
        }

        return ctx.tt.function(func.params, func.returns);
    }

    if (ctx.tt.is_model(type_id)) {
        const auto &model = ctx.tt.get(type_id).as<types::Model_type>();
        if (!model.name.empty()) {
            return type_id;
        }

        std::vector<std::pair<std::string, types::Type_id>> resolved_fields = model.fields;
        for (auto &[_, field_type] : resolved_fields) {
            field_type = resolve_type_recursively(field_type, loc);
        }

        return ctx.tt.model("", resolved_fields);
    }

    return type_id;
}

bool Semantic_checker::is_iterator_protocol_type(types::Type_id type) const
{
    if (ctx.tt.is_iterator(type)) {
        return true;
    }
    if (ctx.tt.is_model(type)) {
        const auto &model_name = ctx.tt.get(type).as<types::Model_type>().name;
        if (!model_name.empty()) {
            if (auto method = ctx.type_env.get_model_method(model_name, "next")) {
                if (method->declaration.is_null()) {
                    return false;
                }
                auto decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(method->declaration).node);
                bool valid_params = decl && (decl->parameters.size() == 1 || decl->parameters.size() == 2);
                return valid_params && ctx.tt.is_optional(effective_return_type(declared_return_types(decl->returns, ctx.tt), ctx.tt));
            }
        }
    }
    return false;
}

types::Type_id Semantic_checker::iterator_element_type(types::Type_id type) const
{
    if (ctx.tt.is_iterator(type)) {
        return ctx.tt.get_iter_elem(type);
    }
    if (ctx.tt.is_model(type)) {
        const auto &model_name = ctx.tt.get(type).as<types::Model_type>().name;
        if (!model_name.empty()) {
            if (auto method = ctx.type_env.get_model_method(model_name, "next")) {
                auto decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(method->declaration).node);
                auto ret_type = decl ? effective_return_type(declared_return_types(decl->returns, ctx.tt), ctx.tt) : ctx.tt.get_void();
                if (decl && ctx.tt.is_optional(ret_type)) {
                    return ctx.tt.get_optional_base(ret_type);
                }
            }
        }
    }
    return ctx.tt.get_void();
}

types::Type_id Semantic_checker::to_iterator_type(types::Type_id type) const
{
    if (is_iterator_protocol_type(type)) {
        return type;
    }
    if (ctx.tt.is_array(type)) {
        return ctx.tt.iterator(ctx.tt.get_array_elem(type));
    }
    if (ctx.tt.is_string(type)) {
        return ctx.tt.iterator(ctx.tt.get_string());
    }
    if (ctx.tt.is_optional(type)) {
        auto base = ctx.tt.get_optional_base(type);
        if (ctx.tt.is_array(base)) {
            return ctx.tt.iterator(ctx.tt.get_array_elem(base));
        }
        if (ctx.tt.is_string(base)) {
            return ctx.tt.iterator(ctx.tt.get_string());
        }
        return ctx.tt.iterator(base);
    }
    if (ctx.tt.is_nil(type)) {
        return ctx.tt.iterator(ctx.tt.get_any());
    }

    if (ctx.tt.is_model(type)) {
        const auto &model_name = ctx.tt.get(type).as<types::Model_type>().name;
        if (!model_name.empty()) {
            if (auto method = ctx.type_env.get_model_method(model_name, "iter")) {
                auto decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(method->declaration).node);
                auto ret_type = decl ? effective_return_type(declared_return_types(decl->returns, ctx.tt), ctx.tt) : ctx.tt.get_void();
                if (decl && decl->parameters.size() == 1 && is_iterator_protocol_type(ret_type)) {
                    return ret_type;
                }
            }
        }
    }
    return ctx.tt.iterator(type);
}

bool Semantic_checker::is_compatible(types::Type_id expected, types::Type_id actual) const
{
    if (expected == actual) {
        return true;
    }

    if (ctx.tt.is_unknown(expected) || ctx.tt.is_unknown(actual)) {
        return true;
    }

    // Same-class numeric widening is implicit: i8->i16->i32->i64,
    // u8->u16->u32->u64, f16->f32->f64. Narrowing, or moving between
    // classes (signed <-> unsigned, int <-> float), needs an explicit cast.
    if (ctx.tt.is_numeric_primitive(expected) && ctx.tt.is_numeric_primitive(actual)) {
        auto exp_kind = ctx.tt.get_primitive(expected);
        auto act_kind = ctx.tt.get_primitive(actual);
        if (types::is_float_primitive(exp_kind) && types::is_float_primitive(act_kind)) {
            return types::primitive_bit_width(exp_kind) >= types::primitive_bit_width(act_kind);
        }
        bool same_class = (types::is_signed_integer_primitive(exp_kind) && types::is_signed_integer_primitive(act_kind))
            || (types::is_unsigned_integer_primitive(exp_kind) && types::is_unsigned_integer_primitive(act_kind));
        return same_class && types::primitive_bit_width(exp_kind) >= types::primitive_bit_width(act_kind);
    }

    if (ctx.tt.is_function(expected) && ctx.tt.is_function(actual)) {
        const auto &exp_fn = ctx.tt.get(expected).as<types::Function_type>();
        const auto &act_fn = ctx.tt.get(actual).as<types::Function_type>();

        if (exp_fn.params.size() != act_fn.params.size() || exp_fn.returns.size() != act_fn.returns.size()) {
            return false;
        }

        for (size_t i = 0; i < exp_fn.params.size(); ++i) {
            if (!is_compatible(exp_fn.params[i], act_fn.params[i])) {
                return false;
            }
        }

        for (size_t i = 0; i < exp_fn.returns.size(); ++i) {
            if (!is_compatible(exp_fn.returns[i], act_fn.returns[i])) {
                return false;
            }
        }

        return true;
    }

    if (ctx.tt.is_model(expected) && ctx.tt.is_model(actual)) {
        auto &exp_model = std::get<types::Model_type>(ctx.tt.get(expected).data);
        auto &act_model = std::get<types::Model_type>(ctx.tt.get(actual).data);

        if (exp_model.fields.size() != act_model.fields.size()) {
            return false;
        }

        for (size_t i = 0; i < exp_model.fields.size(); ++i) {
            if (exp_model.fields[i].first != act_model.fields[i].first) {
                return false;
            }
            if (!is_compatible(exp_model.fields[i].second, act_model.fields[i].second)) {
                return false;
            }
        }

        return true;
    }

    if (ctx.tt.is_any(expected)) {
        return true;
    }

    if (ctx.tt.is_optional(expected)) {
        if (ctx.tt.is_nil(actual)) {
            return true;
        }
        if (ctx.tt.is_optional(actual)) {
            return is_compatible(ctx.tt.get_optional_base(expected), ctx.tt.get_optional_base(actual));
        }
        return is_compatible(ctx.tt.get_optional_base(expected), actual);
    }
    if (ctx.tt.is_optional(actual) || ctx.tt.is_nil(actual)) {
        return false;
    }

    if (ctx.tt.is_array(expected) && ctx.tt.is_array(actual)) {
        if (ctx.tt.is_any(ctx.tt.get_array_elem(expected))) {
            return true;
        }
        return is_compatible(ctx.tt.get_array_elem(expected), ctx.tt.get_array_elem(actual));
    }

    if (ctx.tt.is_iterator(expected) && ctx.tt.is_iterator(actual)) {
        types::Type_id exp_elem = ctx.tt.get_iter_elem(expected);
        types::Type_id act_elem = ctx.tt.get_iter_elem(actual);
        if (ctx.tt.is_any(exp_elem)) {
            return true;
        }
        return is_compatible(exp_elem, act_elem);
    }

    return false;
}

types::Type_id Semantic_checker::promote_numeric_type(types::Type_id left, types::Type_id right) const
{
    if (ctx.tt.is_unknown(left) || ctx.tt.is_unknown(right)) {
        return ctx.tt.get_unknown();
    }
    if (ctx.tt.is_any(left) || ctx.tt.is_any(right)) {
        return ctx.tt.get_any();
    }

    auto left_kind = ctx.tt.get_primitive(left);
    auto right_kind = ctx.tt.get_primitive(right);

    if (types::is_float_primitive(left_kind) || types::is_float_primitive(right_kind)) {
        if (types::is_float_primitive(left_kind) && types::is_float_primitive(right_kind)) {
            return ctx.tt.primitive(types::primitive_bit_width(left_kind) >= types::primitive_bit_width(right_kind) ? left_kind : right_kind);
        }
        return ctx.tt.primitive(types::is_float_primitive(left_kind) ? left_kind : right_kind);
    }

    bool same_signedness = (types::is_signed_integer_primitive(left_kind) && types::is_signed_integer_primitive(right_kind))
        || (types::is_unsigned_integer_primitive(left_kind) && types::is_unsigned_integer_primitive(right_kind));

    if (!same_signedness) {
        return ctx.tt.get_any();
    }

    return ctx.tt.primitive(types::primitive_bit_width(left_kind) >= types::primitive_bit_width(right_kind) ? left_kind : right_kind);
}

std::string Semantic_checker::numeric_cast_error_message(types::Type_id target, types::Type_id source) const
{
    return std::format(
        "Cannot implicitly convert '{}' to '{}'. Only widening within the same signedness is automatic "
        "(i8 < i16 < i32 < i64, u8 < u16 < u32 < u64, f16 < f32 < f64); you cannot go from signed to unsigned "
        "(e.g. i32 -> u64). Use an explicit cast: `(value as {})` (wrapping) or `(value sat {})` (saturating).",
        ctx.tt.to_string(source),
        ctx.tt.to_string(target),
        ctx.tt.to_string(target),
        ctx.tt.to_string(target));
}

void Semantic_checker::report_type_mismatch(
    const ast::Source_location &loc, types::Type_id expected, types::Type_id actual, std::string_view message)
{
    if (ctx.tt.is_numeric_primitive(expected) && ctx.tt.is_numeric_primitive(actual)) {
        type_error(loc, numeric_cast_error_message(expected, actual));
    } else {
        auto diagnostic = err::msg::error(diagnostics_.phase(), loc.l, loc.c, loc.file, "{}", message);
        diagnostic.expected_got(ctx.tt.to_string(expected), ctx.tt.to_string(actual));
        diagnostics_.push(std::move(diagnostic));
    }
}

bool Semantic_checker::default_expr_uses_forbidden_names(ast::Expr_id expr_id, const std::unordered_set<std::string> &forbidden_names) const
{
    if (expr_id.is_null()) {
        return false;
    }

    return std::visit(
        [&](const auto &node) -> bool {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, ast::Variable_expr>) {
                return forbidden_names.contains(node.name);
            } else if constexpr (std::is_same_v<T, ast::Binary_expr>) {
                return default_expr_uses_forbidden_names(node.left, forbidden_names)
                    || default_expr_uses_forbidden_names(node.right, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Unary_expr>) {
                return default_expr_uses_forbidden_names(node.right, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Call_expr>) {
                if (default_expr_uses_forbidden_names(node.callee, forbidden_names)) {
                    return true;
                }
                for (const auto &arg : node.arguments) {
                    if (default_expr_uses_forbidden_names(arg.value, forbidden_names)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Assignment_expr>) {
                return default_expr_uses_forbidden_names(node.value, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Field_assignment_expr>) {
                return default_expr_uses_forbidden_names(node.object, forbidden_names)
                    || default_expr_uses_forbidden_names(node.value, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>) {
                return default_expr_uses_forbidden_names(node.array, forbidden_names)
                    || default_expr_uses_forbidden_names(node.index, forbidden_names)
                    || default_expr_uses_forbidden_names(node.value, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Cast_expr>) {
                return default_expr_uses_forbidden_names(node.expression, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Field_access_expr>) {
                return default_expr_uses_forbidden_names(node.object, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Method_call_expr>) {
                if (default_expr_uses_forbidden_names(node.object, forbidden_names)) {
                    return true;
                }
                for (const auto &arg : node.arguments) {
                    if (default_expr_uses_forbidden_names(arg.value, forbidden_names)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Model_literal_expr>) {
                for (const auto &field : node.fields) {
                    if (default_expr_uses_forbidden_names(field.second, forbidden_names)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Array_literal_expr>) {
                for (auto element : node.elements) {
                    if (default_expr_uses_forbidden_names(element, forbidden_names)) {
                        return true;
                    }
                }
                return false;
            } else if constexpr (std::is_same_v<T, ast::Array_access_expr>) {
                return default_expr_uses_forbidden_names(node.array, forbidden_names)
                    || default_expr_uses_forbidden_names(node.index, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Static_path_expr>) {
                return default_expr_uses_forbidden_names(node.base, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Range_expr>) {
                return default_expr_uses_forbidden_names(node.start, forbidden_names) || default_expr_uses_forbidden_names(node.end, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Spawn_expr>) {
                return default_expr_uses_forbidden_names(node.call, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Await_expr>) {
                return default_expr_uses_forbidden_names(node.thread, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Yield_expr>) {
                return default_expr_uses_forbidden_names(node.value, forbidden_names);
            }
            return false;
        },
        ctx.tree.get(expr_id).node);
}

void Semantic_checker::validate_function_defaults(const ast::Function_stmt &stmt)
{
    std::unordered_set<std::string> forbidden_names = {"this"};
    for (const auto &param : stmt.parameters) {
        forbidden_names.insert(param.name);
    }
    for (const auto &param : stmt.parameters) {
        if (param.default_value.is_null()) {
            continue;
        }
        if (default_expr_uses_forbidden_names(param.default_value, forbidden_names)) {
            type_error(param.loc, std::format("Default argument for parameter '{}' cannot reference 'this' or another parameter.", param.name));
        } else {
            auto default_type = check_expr(param.default_value, param.type);
            if (!is_compatible(param.type, default_type)) {
                type_error(
                    param.loc,
                    std::format(
                        "Default argument mismatch in function.\n   Expected: '{}'\n   Got: '{}'",
                        ctx.tt.to_string(param.type),
                        ctx.tt.to_string(default_type)));
            }
        }
    }
}

void Semantic_checker::validate_model_defaults(const ast::Model_stmt &stmt)
{
    std::unordered_set<std::string> forbidden_names = {"this"};
    for (const auto &field : stmt.fields) {
        forbidden_names.insert(field.name);
    }
    for (const auto &field : stmt.fields) {
        if (field.default_value.is_null()) {
            if (field.is_static) {
                type_error(field.loc, std::format("Static field '{}' must have an initializer.", field.name));
            }
            continue;
        }
        if (default_expr_uses_forbidden_names(field.default_value, forbidden_names)) {
            type_error(field.loc, std::format("Default value for member '{}' cannot reference 'this' or another member.", field.name));
        } else if (!field.type_inferred) {
            auto default_type = check_expr(field.default_value, field.type);
            if (!is_compatible(field.type, default_type)) {
                type_error(
                    field.loc,
                    std::format(
                        "Default argument mismatch in model.\n   Expected: '{}'\n   Got: '{}'",
                        ctx.tt.to_string(field.type),
                        ctx.tt.to_string(default_type)));
            }
        }
    }
}

void Semantic_checker::validate_union_defaults(const ast::Union_stmt &stmt)
{
    std::unordered_set<std::string> forbidden_names = {"this"};
    for (const auto &variant : stmt.variants) {
        forbidden_names.insert(variant.name);
    }
    for (const auto &variant : stmt.variants) {
        if (variant.default_value.is_null()) {
            continue;
        }
        if (default_expr_uses_forbidden_names(variant.default_value, forbidden_names)) {
            type_error(variant.loc, std::format("Default value for variant '{}' cannot reference another member.", variant.name));
        }
        if (variant.type == ctx.tt.get_void()) {
            type_error(variant.loc, std::format("Variant '{}' does not take a payload, cannot declare default.", variant.name));
            continue;
        }
        auto default_type = check_expr(variant.default_value, variant.type);
        if (!is_compatible(variant.type, default_type)) {
            type_error(
                variant.loc,
                std::format(
                    "Default argument mismatch in model.\n   Expected: '{}'\n   Got: '{}'",
                    ctx.tt.to_string(variant.type),
                    ctx.tt.to_string(default_type)));
        }
    }
}

std::optional<Semantic_checker::Access_path> Semantic_checker::extract_access_path(ast::Expr_id expr_id) const
{
    if (expr_id.is_null()) {
        return std::nullopt;
    }

    auto &node = ctx.tree.get(expr_id).node;

    if (auto *v = std::get_if<ast::Variable_expr>(&node)) {
        return Access_path{v->name, {}};
    }
    if (auto *f = std::get_if<ast::Field_access_expr>(&node)) {
        auto base_path = extract_access_path(f->object);
        if (!base_path) {
            return std::nullopt;
        }

        base_path->projections.push_back(Projection{.kind = Projection_kind::Field, .name_val = f->field_name});
        return base_path;
    }
    if (auto *a = std::get_if<ast::Array_access_expr>(&node)) {
        auto base_path = extract_access_path(a->array);
        if (!base_path) {
            return std::nullopt;
        }

        auto &idx_node = ctx.tree.get(a->index).node;

        if (auto *index_var = std::get_if<ast::Variable_expr>(&idx_node)) {
            base_path->projections.push_back(Projection{.kind = Projection_kind::Var_Index, .name_val = index_var->name});
            return base_path;
        }
        if (auto *l = std::get_if<ast::Literal_expr>(&idx_node)) {
            if (l->value.is_integer()) {
                base_path->projections.push_back(Projection{.kind = Projection_kind::Int_Index, .name_val = "", .int_val = l->value.as_int()});
                return base_path;
            }
            if (l->value.is_u_integer()) {
                base_path->projections.push_back(Projection{.kind = Projection_kind::Uint_Index, .name_val = "", .uint_val = l->value.as_uint()});
                return base_path;
            }
        }
        return std::nullopt;
    }

    return std::nullopt;
}

void Semantic_checker::collect_nil_check_from_optional_method(
    const ast::Method_call_expr &expr, bool target_truthy_branch, std::unordered_set<Access_path, Access_path_hash> &out)
{
    if (!expr.arguments.empty()) {
        return;
    }
    if (!ctx.tt.is_optional(ast::get_type(ctx.tree.get(expr.object).node))) {
        return;
    }

    bool narrows_when_truthy = (expr.method_name == "has_val" || expr.method_name == "is_val");
    bool narrows_when_falsey = (expr.method_name == "is_nil");
    if ((target_truthy_branch && narrows_when_truthy) || (!target_truthy_branch && narrows_when_falsey)) {
        if (auto path = extract_access_path(expr.object)) {
            out.insert(*path);
        }
    }
}

void Semantic_checker::collect_nil_check_from_comparison(
    const ast::Binary_expr &expr, lex::TokenType target_op, std::unordered_set<Access_path, Access_path_hash> &out)
{
    if (expr.op != target_op) {
        return;
    }

    if (ctx.tt.is_nil(ast::get_type(ctx.tree.get(expr.right).node))) {
        if (auto path = extract_access_path(expr.left)) {
            out.insert(*path);
        }
    } else if (ctx.tt.is_nil(ast::get_type(ctx.tree.get(expr.left).node))) {
        if (auto path = extract_access_path(expr.right)) {
            out.insert(*path);
        }
    }
}

void Semantic_checker::collect_nil_checked_vars_for_then(ast::Expr_id expr_id, std::unordered_set<Access_path, Access_path_hash> &out)
{
    if (expr_id.is_null()) {
        return;
    }
    auto &node = ctx.tree.get(expr_id).node;

    if (auto *bin_expr = std::get_if<ast::Binary_expr>(&node)) {
        if (bin_expr->op == lex::TokenType::LogicalAnd) {
            collect_nil_checked_vars_for_then(bin_expr->left, out);
            collect_nil_checked_vars_for_then(bin_expr->right, out);
            return;
        }
        collect_nil_check_from_comparison(*bin_expr, lex::TokenType::NotEqual, out);
        return;
    }
    if (auto *unary_expr = std::get_if<ast::Unary_expr>(&node)) {
        if (unary_expr->op == lex::TokenType::LogicalNot) {
            collect_nil_checked_vars_for_else(unary_expr->right, out);
            return;
        }
    }
    if (auto *method_expr = std::get_if<ast::Method_call_expr>(&node)) {
        collect_nil_check_from_optional_method(*method_expr, true, out);
        return;
    }
    if (ctx.tt.is_optional(ast::get_type(node))) {
        if (auto path = extract_access_path(expr_id)) {
            out.insert(*path);
        }
    }
}

void Semantic_checker::collect_nil_checked_vars_for_else(ast::Expr_id expr_id, std::unordered_set<Access_path, Access_path_hash> &out)
{
    if (expr_id.is_null()) {
        return;
    }
    auto &node = ctx.tree.get(expr_id).node;

    if (auto *bin_expr = std::get_if<ast::Binary_expr>(&node)) {
        if (bin_expr->op == lex::TokenType::LogicalOr) {
            collect_nil_checked_vars_for_else(bin_expr->left, out);
            collect_nil_checked_vars_for_else(bin_expr->right, out);
            return;
        }
        collect_nil_check_from_comparison(*bin_expr, lex::TokenType::Equal, out);
        return;
    }
    if (auto *unary_expr = std::get_if<ast::Unary_expr>(&node)) {
        if (unary_expr->op == lex::TokenType::LogicalNot) {
            collect_nil_checked_vars_for_then(unary_expr->right, out);
            return;
        }
    }
    if (auto *method_expr = std::get_if<ast::Method_call_expr>(&node)) {
        collect_nil_check_from_optional_method(*method_expr, false, out);
    }
}

types::Type_id Semantic_checker::parse_type_string(std::string str, const std::unordered_map<std::string, types::Type_id> &generics) const
{
    str = trim_copy(str);

    if (str.length() == 1 && std::isupper(str[0]) && generics.contains(str)) {
        return generics.at(str);
    }

    if (str == "i8") {
        return ctx.tt.get_i8();
    }
    if (str == "i16") {
        return ctx.tt.get_i16();
    }
    if (str == "i32") {
        return ctx.tt.get_i32();
    }
    if (str == "i64") {
        return ctx.tt.get_i64();
    }
    if (str == "u8") {
        return ctx.tt.get_u8();
    }
    if (str == "u16") {
        return ctx.tt.get_u16();
    }
    if (str == "u32") {
        return ctx.tt.get_u32();
    }
    if (str == "u64") {
        return ctx.tt.get_u64();
    }
    if (str == "f16") {
        return ctx.tt.get_f16();
    }
    if (str == "f32") {
        return ctx.tt.get_f32();
    }
    if (str == "f64") {
        return ctx.tt.get_f64();
    }
    if (str == "bool") {
        return ctx.tt.get_bool();
    }
    if (str == "string") {
        return ctx.tt.get_string();
    }
    if (str == "void") {
        return ctx.tt.get_void();
    }
    if (str == "any") {
        return ctx.tt.get_any();
    }
    if (str == "nil") {
        return ctx.tt.get_nil();
    }
    if (str == "usize" || str == "ptr") {
        return ctx.tt.get_u64();
    }
    if (str == "isize") {
        return ctx.tt.get_i64();
    }
    if (str == "byte" || str == "char") {
        return ctx.tt.get_u8();
    }

    if (str.starts_with("fn(")) {
        size_t close_paren = 0;
        int depth = 0;
        bool found = false;
        for (size_t i = 2; i < str.size(); ++i) {
            if (str[i] == '(') {
                ++depth;
            } else if (str[i] == ')') {
                if (depth == 0) {
                    close_paren = i;
                    found = true;
                    break;
                }
                --depth;
            }
        }

        if (found) {
            auto tail = trim_copy(str.substr(close_paren + 1));
            if (tail.empty() || tail.starts_with("->")) {
                std::vector<types::Type_id> params;
                auto params_str = str.substr(3, close_paren - 3);
                for (const auto &param_str : split_top_level(params_str, ',')) {
                    params.push_back(parse_type_string(param_str, generics));
                }

                std::vector<types::Type_id> returns;
                if (tail.starts_with("->")) {
                    auto returns_str = trim_copy(tail.substr(2));
                    for (const auto &return_str : split_top_level(returns_str, ',')) {
                        returns.push_back(parse_type_string(return_str, generics));
                    }
                }

                return ctx.tt.function(params, returns);
            }
        }
    }

    if (str.starts_with("model {") && str.back() == '}') {
        auto body = str.substr(7, str.length() - 8);
        std::vector<std::pair<std::string, types::Type_id>> fields;

        for (const auto &field_str : split_top_level_fields(body)) {
            auto colon = field_str.find(':');
            if (colon == std::string::npos) {
                return ctx.tt.get_any();
            }

            std::string field_name = trim_copy(field_str.substr(0, colon));
            std::string field_type = trim_copy(field_str.substr(colon + 1));
            if (field_name.empty() || field_type.empty()) {
                return ctx.tt.get_any();
            }

            fields.push_back({field_name, parse_type_string(field_type, generics)});
        }

        return ctx.tt.model("", fields);
    }

    if (str.starts_with("iter<") && str.ends_with(">")) {
        auto base = parse_type_string(str.substr(5, str.length() - 6), generics);
        return ctx.tt.iterator(base);
    }
    if (str.ends_with("[]")) {
        auto base = parse_type_string(str.substr(0, str.length() - 2), generics);
        return ctx.tt.array(base);
    }
    if (str.ends_with("?")) {
        auto base = parse_type_string(str.substr(0, str.length() - 1), generics);
        return ctx.tt.optional(base);
    }

    if (ctx.type_env.is_type_defined(str)) {
        return *ctx.type_env.get_type(str);
    }

    return ctx.tt.get_any();
}

bool Semantic_checker::match_ffi_type(
    std::string expected_str, types::Type_id actual_type, std::unordered_map<std::string, types::Type_id> &generics) const
{
    expected_str = trim_copy(expected_str);

    if (ctx.tt.is_any(actual_type) || ctx.tt.is_unknown(actual_type)) {
        return true;
    }

    if (expected_str.find('|') != std::string::npos) {
        for (const auto &item : split_top_level(expected_str, '|')) {
            auto temp_generics = generics;
            if (match_ffi_type(item, actual_type, temp_generics)) {
                generics = temp_generics;
                return true;
            }
        }
        return false;
    }

    if (expected_str.starts_with("iter<") && expected_str.ends_with(">")) {
        if (!ctx.tt.is_iterator(actual_type)) {
            return false;
        }
        std::string base_str = expected_str.substr(5, expected_str.length() - 6);
        return match_ffi_type(base_str, ctx.tt.get_iter_elem(actual_type), generics);
    }

    if (expected_str.ends_with("[]")) {
        if (!ctx.tt.is_array(actual_type)) {
            return false;
        }
        std::string base_str = expected_str.substr(0, expected_str.length() - 2);
        return match_ffi_type(base_str, ctx.tt.get_array_elem(actual_type), generics);
    }

    if (expected_str.ends_with("?")) {
        if (ctx.tt.is_nil(actual_type)) {
            return true;
        }
        std::string base_str = expected_str.substr(0, expected_str.length() - 1);
        types::Type_id inner_actual = ctx.tt.is_optional(actual_type) ? ctx.tt.get_optional_base(actual_type) : actual_type;
        return match_ffi_type(base_str, inner_actual, generics);
    }

    if (expected_str.length() == 1 && std::isupper(expected_str[0])) {
        if (generics.contains(expected_str)) {
            return is_compatible(generics[expected_str], actual_type);
        }
        generics[expected_str] = actual_type;
        return true;
    }

    types::Type_id concrete_expected = parse_type_string(expected_str, generics);
    return is_compatible(concrete_expected, actual_type);
}

Semantic_checker::Bound_call_arguments Semantic_checker::bind_call_arguments(
    const std::vector<ast::Function_param> &parameters,
    const std::vector<ast::Call_argument> &arguments,
    const ast::Source_location &call_loc,
    const std::string &call_kind,
    const std::string &call_name)
{
    Bound_call_arguments result;
    result.ordered_arguments.resize(parameters.size());

    std::vector<bool> filled(parameters.size(), false);
    bool seen_named = false;
    size_t next_positional = 0;

    auto advance_to_next_positional = [&]() {
        while (next_positional < parameters.size() && filled[next_positional]) {
            ++next_positional;
        }
    };

    for (const auto &arg : arguments) {
        size_t target_index = parameters.size();

        if (!arg.name.empty()) {
            seen_named = true;
            auto it = std::find_if(parameters.begin(), parameters.end(), [&](const auto &param) { return param.name == arg.name; });
            if (it == parameters.end()) {
                type_error(arg.loc, std::format("{} '{}' has no parameter named '{}'.", call_kind, call_name, arg.name));
                result.ok = false;
                continue;
            }
            target_index = static_cast<size_t>(std::distance(parameters.begin(), it));
            if (filled[target_index]) {
                type_error(arg.loc, std::format("Parameter '{}' was provided more than once.", arg.name));
                result.ok = false;
                continue;
            }
        } else {
            if (seen_named) {
                type_error(arg.loc, "Positional arguments cannot appear after named arguments.");
                result.ok = false;
                continue;
            }
            advance_to_next_positional();
            if (next_positional >= parameters.size()) {
                type_error(call_loc, std::format("Too many arguments. Expected at most {}, got {}.", parameters.size(), arguments.size()));
                result.ok = false;
                continue;
            }
            target_index = next_positional++;
        }

        auto arg_type = check_expr(arg.value, parameters[target_index].type);
        if (!is_compatible(parameters[target_index].type, arg_type)) {
            if (ctx.tt.is_numeric_primitive(parameters[target_index].type) && ctx.tt.is_numeric_primitive(arg_type)) {
                type_error(arg.loc, numeric_cast_error_message(parameters[target_index].type, arg_type));
            } else {
                type_error(
                    arg.loc,
                    std::format(
                        "Argument type mismatch for parameter '{}'. Expected '{}' but got '{}'.",
                        parameters[target_index].name,
                        ctx.tt.to_string(parameters[target_index].type),
                        ctx.tt.to_string(arg_type)));
            }
            result.ok = false;
        }

        result.ordered_arguments[target_index] = ast::Call_argument{.name = "", .value = arg.value, .loc = arg.loc};
        filled[target_index] = true;
    }

    for (size_t i = 0; i < parameters.size(); ++i) {
        if (filled[i]) {
            continue;
        }
        if (!parameters[i].default_value.is_null()) {
            result.ordered_arguments[i] = ast::Call_argument{.name = "", .value = parameters[i].default_value, .loc = parameters[i].loc};
            continue;
        }
        type_error(call_loc, std::format("Missing required argument '{}'.", parameters[i].name));
        result.ok = false;
    }
    return result;
}

Semantic_checker::Bound_native_arguments Semantic_checker::try_bind_native_arguments(
    const std::vector<Native_param> &parameters, const std::vector<ast::Call_argument> &arguments, std::optional<types::Type_id> receiver_type)
{
    Bound_native_arguments result;
    size_t parameter_offset = receiver_type ? 1 : 0;

    if (receiver_type) {
        if (parameters.empty() || !match_ffi_type(parameters[0].type_str, *receiver_type, result.generics)) {
            return result;
        }
    }

    if (parameters.size() < parameter_offset) {
        return result;
    }

    const size_t available_parameter_count = parameters.size() - parameter_offset;
    const bool has_variadic = available_parameter_count > 0 && parameters.back().is_variadic;
    const size_t fixed_count = available_parameter_count - (has_variadic ? 1u : 0u);
    result.ordered_arguments.resize(fixed_count);

    std::vector<bool> filled(fixed_count, false);
    bool seen_named = false;
    size_t next_positional = 0;
    std::vector<ast::Call_argument> variadic_arguments;

    auto advance_to_next_positional = [&]() {
        while (next_positional < filled.size() && filled[next_positional]) {
            ++next_positional;
        }
    };

    for (const auto &arg : arguments) {
        size_t target_index = filled.size();

        if (!arg.name.empty()) {
            seen_named = true;
            for (size_t i = 0; i < fixed_count; ++i) {
                if (parameters[i + parameter_offset].name == arg.name) {
                    target_index = i;
                    break;
                }
            }
            if (target_index == fixed_count || filled[target_index]) {
                return result;
            }
        } else {
            if (seen_named) {
                return result;
            }
            advance_to_next_positional();
            if (next_positional >= fixed_count) {
                if (!has_variadic) {
                    return result;
                }

                std::string expected_type_str = parameters.back().type_str;
                std::optional<types::Type_id> expected_context = parse_type_string(expected_type_str, result.generics);
                auto arg_res = check_expr(arg.value, expected_context);
                if (!match_ffi_type(expected_type_str, arg_res, result.generics)) {
                    return result;
                }

                variadic_arguments.push_back(ast::Call_argument{.name = "", .value = arg.value, .loc = arg.loc});
                continue;
            }
            target_index = next_positional++;
        }

        std::string expected_type_str = parameters[target_index + parameter_offset].type_str;
        std::optional<types::Type_id> expected_context = parse_type_string(expected_type_str, result.generics);

        auto arg_res = check_expr(arg.value, expected_context);
        if (!match_ffi_type(expected_type_str, arg_res, result.generics)) {
            return result;
        }

        result.ordered_arguments[target_index] = ast::Call_argument{.name = "", .value = arg.value, .loc = arg.loc};
        filled[target_index] = true;
    }

    for (size_t i = 0; i < filled.size(); ++i) {
        if (filled[i]) {
            continue;
        }
        if (parameters[i + parameter_offset].default_value.has_value()) {
            result.ordered_arguments[i] = ast::Call_argument{.name = "", .value = ast::Expr_id::null(), .loc = {}};
            filled[i] = true;
            continue;
        }
        return result;
    }

    result.ordered_arguments.insert(result.ordered_arguments.end(), variadic_arguments.begin(), variadic_arguments.end());
    result.ok = true;
    return result;
}

} // namespace phos
