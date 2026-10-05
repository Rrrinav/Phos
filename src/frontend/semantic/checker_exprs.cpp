#include "semantic_checker.hpp"

#include "frontend/ast_utils.hpp"

#include <algorithm>
#include <cctype>
#include <format>

namespace phos {

types::Type_id Semantic_checker::check_model_literal_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto model_name = get_node<ast::Model_literal_expr>(ctx.tree, expr_id).model_name;
    auto loc = get_node<ast::Model_literal_expr>(ctx.tree, expr_id).loc;
    auto fields = get_node<ast::Model_literal_expr>(ctx.tree, expr_id).fields;

    types::Type_id resolved_type = resolve_type_recursively(ctx.tt.unresolved(model_name), loc);

    if (ctx.tt.is_unknown(resolved_type)) {
        return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    if (ctx.tt.is_union(resolved_type)) {
        auto &union_t = std::get<types::Union_type>(ctx.tt.get(resolved_type).data);
        auto union_data_ptr = ctx.type_env.get_union(union_t.name);

        if (fields.size() != 1) {
            type_error(loc, "Union literals must be initialized with exactly one variant field.");
            return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        std::string variant_name = fields[0].first;
        ast::Expr_id payload_expr = fields[0].second;

        if (variant_name.empty()) {
            if (!payload_expr.is_null()) {
                if (std::holds_alternative<ast::Variable_expr>(ctx.tree.get(payload_expr).node)) {
                    variant_name = get_node<ast::Variable_expr>(ctx.tree, payload_expr).name;
                    payload_expr = ast::Expr_id::null();
                    fields[0].first = variant_name;
                    fields[0].second = ast::Expr_id::null();
                }
            }
        }

        auto it = std::find_if(union_t.variants.begin(), union_t.variants.end(), [&](const auto &v) { return v.first == variant_name; });
        if (it == union_t.variants.end()) {
            type_error(loc, std::format("Union has no variant named '{}'.", variant_name));
            return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        auto expected_payload_type = it->second;

        if (payload_expr.is_null()) {
            if (expected_payload_type == ctx.tt.get_void()) {
                get_node<ast::Model_literal_expr>(ctx.tree, expr_id).fields = std::move(fields);
                return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = resolved_type;
            }

            if (union_data_ptr && union_data_ptr->variant_defaults.contains(variant_name)) {
                payload_expr = fields[0].second = union_data_ptr->variant_defaults.at(variant_name);
            } else {
                type_error(loc, "Variant requires a payload or default.");
                return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
        } else if (expected_payload_type == ctx.tt.get_void()) {
            type_error(ast::get_loc(ctx.tree.get(payload_expr).node), "Variant does not take a payload.");
            return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        auto actual_payload_type = check_expr(payload_expr, expected_payload_type);
        if (!is_compatible(expected_payload_type, actual_payload_type)) {
            type_error(ast::get_loc(ctx.tree.get(payload_expr).node), "Type mismatch for payload.");
        }

        get_node<ast::Model_literal_expr>(ctx.tree, expr_id).fields = std::move(fields);
        return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = resolved_type;
    }

    if (ctx.tt.is_model(resolved_type)) {
        auto &model_type = std::get<types::Model_type>(ctx.tt.get(resolved_type).data);
        auto model_data_ptr = ctx.type_env.get_model(model_type.name);

        if (fields.size() > model_type.fields.size()) {
            type_error(loc, "Too many fields provided for model.");
            return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        for (size_t i = 0; i < fields.size(); ++i) {
            if (fields[i].first.empty()) {
                fields[i].first = model_type.fields[i].first;
            }
        }

        std::unordered_set<std::string> provided_fields;
        for (const auto &field : fields) {
            if (provided_fields.contains(field.first)) {
                type_error(loc, "Field provided multiple times.");
            }
            provided_fields.insert(field.first);
        }

        for (const auto &expected_field : model_type.fields) {
            if (!provided_fields.count(expected_field.first)) {
                if (model_data_ptr && model_data_ptr->field_defaults.contains(expected_field.first)) {
                    fields.push_back({expected_field.first, model_data_ptr->field_defaults.at(expected_field.first)});
                    provided_fields.insert(expected_field.first);
                } else {
                    type_error(loc, std::format("Missing required field '{}'.", expected_field.first));
                }
            }
        }

        for (const auto &provided_field : fields) {
            const auto &field_name = provided_field.first;
            auto it = std::find_if(model_type.fields.begin(), model_type.fields.end(), [&](const auto &f) { return f.first == field_name; });
            if (it == model_type.fields.end()) {
                type_error(loc, std::format("Unknown field '{}'.", field_name));
                continue;
            }
            auto expected_type = it->second;
            auto actual_res = check_expr(provided_field.second, expected_type);
            if (!is_compatible(expected_type, actual_res)) {
                type_error(ast::get_loc(ctx.tree.get(provided_field.second).node), "Field type mismatch.");
            }
        }

        get_node<ast::Model_literal_expr>(ctx.tree, expr_id).fields = std::move(fields);
        return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = resolved_type;
    }

    type_error(loc, std::format("'{}' is not a model or a union.", model_name));
    return get_node<ast::Model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
}

types::Type_id Semantic_checker::check_anon_model_literal_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    auto loc = get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).loc;
    auto fields = get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).fields;

    if (!context_type) {
        type_error(loc, "Cannot infer type of anonymous literal. Context missing.");
        return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    types::Type_id expected_type = *context_type;

    if (ctx.tt.is_union(expected_type)) {
        auto &expected_union = std::get<types::Union_type>(ctx.tt.get(expected_type).data);
        auto union_data_ptr = ctx.type_env.get_union(expected_union.name);

        if (fields.size() != 1) {
            type_error(loc, "Anonymous union literals must have exactly one variant field.");
            return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        std::string variant_name = fields[0].first;
        ast::Expr_id payload_expr = fields[0].second;

        if (variant_name.empty()) {
            if (!payload_expr.is_null()) {
                if (std::holds_alternative<ast::Variable_expr>(ctx.tree.get(payload_expr).node)) {
                    variant_name = get_node<ast::Variable_expr>(ctx.tree, payload_expr).name;
                    payload_expr = ast::Expr_id::null();
                    fields[0].first = variant_name;
                    fields[0].second = ast::Expr_id::null();
                }
            }
        }

        auto it =
            std::find_if(expected_union.variants.begin(), expected_union.variants.end(), [&](const auto &v) { return v.first == variant_name; });
        if (it == expected_union.variants.end()) {
            type_error(loc, std::format("Union has no variant named '{}'.", variant_name));
            return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        auto expected_payload_type = it->second;
        if (payload_expr.is_null()) {
            if (expected_payload_type == ctx.tt.get_void()) {
                get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).fields = std::move(fields);
                return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = expected_type;
            }

            if (union_data_ptr && union_data_ptr->variant_defaults.contains(variant_name)) {
                payload_expr = fields[0].second = union_data_ptr->variant_defaults.at(variant_name);
            } else {
                type_error(loc, "Variant requires a value.");
                return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
        } else if (expected_payload_type == ctx.tt.get_void()) {
            type_error(ast::get_loc(ctx.tree.get(payload_expr).node), "Variant does not take a payload.");
            return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        auto actual_res = check_expr(payload_expr, expected_payload_type);
        if (!is_compatible(expected_payload_type, actual_res)) {
            type_error(ast::get_loc(ctx.tree.get(payload_expr).node), "Type mismatch for payload.");
        }

        get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).fields = std::move(fields);
        return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = expected_type;
    }

    if (ctx.tt.is_model(expected_type)) {
        auto &expected_model = std::get<types::Model_type>(ctx.tt.get(expected_type).data);
        auto model_data_ptr = ctx.type_env.get_model(expected_model.name);

        if (fields.size() > expected_model.fields.size()) {
            type_error(loc, "Too many fields for anonymous model.");
            return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        bool has_named = std::any_of(fields.begin(), fields.end(), [](const auto &f) { return !f.first.empty(); });
        if (has_named) {
            bool has_pos = std::any_of(fields.begin(), fields.end(), [](const auto &f) { return f.first.empty(); });
            if (has_pos) {
                type_error(loc, "Cannot mix positional and named fields.");
                return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
        } else {
            for (size_t i = 0; i < fields.size(); ++i) {
                fields[i].first = expected_model.fields[i].first;
            }
        }

        for (size_t i = 0; i < fields.size(); ++i) {
            auto it =
                std::find_if(expected_model.fields.begin(), expected_model.fields.end(), [&](const auto &f) { return f.first == fields[i].first; });
            if (it == expected_model.fields.end()) {
                continue;
            }

            auto act_field = check_expr(fields[i].second, it->second);
            if (!is_compatible(it->second, act_field)) {
                type_error(loc, "Type mismatch for field.");
            }
        }

        std::vector<std::pair<std::string, ast::Expr_id>> ordered;
        ordered.reserve(expected_model.fields.size());
        for (const auto &expected_field : expected_model.fields) {
            auto it = std::find_if(fields.begin(), fields.end(), [&](const auto &f) { return f.first == expected_field.first; });
            if (it != fields.end()) {
                ordered.push_back(*it);
                continue;
            }
            if (model_data_ptr && model_data_ptr->field_defaults.contains(expected_field.first)) {
                ordered.push_back({expected_field.first, model_data_ptr->field_defaults.at(expected_field.first)});
                continue;
            }
            type_error(loc, std::format("Missing required field '{}'.", expected_field.first));
        }

        get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).fields = std::move(ordered);
        return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = expected_type;
    }

    type_error(loc, "Context for anonymous literal is neither a model nor a union.");
    return get_node<ast::Anon_model_literal_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
}

types::Type_id Semantic_checker::check_assignment_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto name = get_node<ast::Assignment_expr>(ctx.tree, expr_id).name;
    auto loc = get_node<ast::Assignment_expr>(ctx.tree, expr_id).loc;
    auto value_id = get_node<ast::Assignment_expr>(ctx.tree, expr_id).value;

    auto var_info_res = lookup(name, loc);
    if (!var_info_res) {
        return get_node<ast::Assignment_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    if (!var_info_res->id.is_null()) {
        auto &sym = ctx.registry.get_symbol(var_info_res->id);

        if (sym.kind == Symbol_kind::Phos_const || sym.kind == Symbol_kind::Native_const) {
            type_error(loc, "Cannot mutate a constant.");
            return get_node<ast::Assignment_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }

        if (sym.kind == Symbol_kind::Global_var) {
            if (!ctx.repl_force_global && sym.owner_module != current_module_id) {
                type_error(loc, std::format("Cannot mutate foreign static variable '{}'. It is read-only outside its module.", ctx.registry.resolve(sym.name)));
                return get_node<ast::Assignment_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
            }
        }
    }

    if (!var_info_res->is_mut) {
        type_error(loc, "Cannot assign to an immutable variable. Use 'mut'.");
        return get_node<ast::Assignment_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    if (!var_info_res->id.is_null()) {
        get_node<ast::Assignment_expr>(ctx.tree, expr_id).resolved_symbol = var_info_res->id;
    }

    auto var_type = var_info_res->type;
    auto val_type_res = check_expr(value_id, var_type);

    // Reassigning a nested `fn` value keeps it from escaping through an alias.
    if (expr_references_nested_function(value_id)) {
        variables.mark_nested_function_value(name);
    }

    if (!is_compatible(var_type, val_type_res)) {
        auto diagnostic = err::msg::error(diagnostics_.phase(), loc.l, loc.c, loc.file, "Assignment type mismatch.");
        diagnostic.expected_got(ctx.tt.to_string(var_type), ctx.tt.to_string(val_type_res));
        diagnostics_.push(std::move(diagnostic));
    }
    return get_node<ast::Assignment_expr>(ctx.tree, expr_id).type = var_type;
}

types::Type_id Semantic_checker::check_variable_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;

    auto &expr = get_node<ast::Variable_expr>(ctx.tree, expr_id);
    auto name = expr.name;
    auto loc = expr.loc;

    if (name == "this") {
        if (!current_model_type) {
            type_error(loc, "Cannot use 'this' outside of a model method");
            return expr.type = ctx.tt.get_unknown();
        }
        return expr.type = *current_model_type;
    }

    if (auto local_sym = variables.lookup(name)) {
        if (!local_sym->id.is_null()) {
            expr.resolved_symbol = local_sym->id;
        } else {
            expr.resolved_symbol.reset();
        }
        return expr.type = local_sym->type;
    }

    if (auto module_id = imported_module_id(ctx, current_module_id, name)) {
        return expr.type = ctx.tt.get_any();
    }

    if (auto global_sym_id = ctx.registry.lookup_global(name)) {
        expr.resolved_symbol = *global_sym_id;

        auto &sym = ctx.registry.get_symbol(*global_sym_id);
        expr.name = std::string(ctx.registry.resolve(sym.name));
        return expr.type = resolve_symbol_type(ctx, *global_sym_id, *this);
    }

    auto type_res = lookup(name, loc);
    if (!type_res) {
        return expr.type = ctx.tt.get_unknown();
    }

    return expr.type = type_res->type;
}

types::Type_id Semantic_checker::check_binary_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto op = get_node<ast::Binary_expr>(ctx.tree, expr_id).op;
    auto loc = get_node<ast::Binary_expr>(ctx.tree, expr_id).loc;
    auto left_id = get_node<ast::Binary_expr>(ctx.tree, expr_id).left;
    auto right_id = get_node<ast::Binary_expr>(ctx.tree, expr_id).right;

    auto left_type = check_expr(left_id);
    auto right_type = check_expr(right_id);

    auto report_error = [&](const std::string &message) {
        type_error(loc, std::format("{} (left is '{}', right is '{}')", message, ctx.tt.to_string(left_type), ctx.tt.to_string(right_type)));
    };

    types::Type_id result = ctx.tt.get_unknown();

    switch (op) {
    case lex::TokenType::Pipe:
    case lex::TokenType::BitXor:
    case lex::TokenType::BitAnd:
    case lex::TokenType::BitLShift:
    case lex::TokenType::BitRshift:
        if (!ctx.tt.is_integer_primitive(left_type) || !ctx.tt.is_integer_primitive(right_type)) {
            report_error("Bitwise operators require integer operands.");
            result = ctx.tt.get_unknown();
        } else {
            result = promote_numeric_type(left_type, right_type);
            if (ctx.tt.is_any(result)) {
                report_error("Bitwise operators require integers from the same signedness family.");
                result = ctx.tt.get_unknown();
            }
        }
        break;

    case lex::TokenType::Plus:
        if (ctx.tt.is_string(left_type) && ctx.tt.is_string(right_type)) {
            result = ctx.tt.get_string();
        } else if (ctx.tt.is_numeric_primitive(left_type) && ctx.tt.is_numeric_primitive(right_type)) {
            result = promote_numeric_type(left_type, right_type);
            if (ctx.tt.is_any(result)) {
                report_error("Mixed signed and unsigned integer arithmetic requires an explicit cast.");
                result = ctx.tt.get_unknown();
            }
        } else {
            report_error("Operands must be two numbers or two strings for '+'");
            result = ctx.tt.get_unknown();
        }
        break;

    case lex::TokenType::Minus:
    case lex::TokenType::Star:
        if (!ctx.tt.is_numeric_primitive(left_type) || !ctx.tt.is_numeric_primitive(right_type)) {
            report_error("Operands must be two numbers for this operator.");
            result = ctx.tt.get_unknown();
        } else {
            result = promote_numeric_type(left_type, right_type);
            if (ctx.tt.is_any(result)) {
                report_error("Mixed signed and unsigned integer arithmetic requires an explicit cast.");
                result = ctx.tt.get_unknown();
            }
        }
        break;

    case lex::TokenType::Slash:
        if (!ctx.tt.is_numeric_primitive(left_type) || !ctx.tt.is_numeric_primitive(right_type)) {
            report_error("Operands for division must be numbers.");
            result = ctx.tt.get_unknown();
        } else {
            result = promote_numeric_type(left_type, right_type);
            if (ctx.tt.is_any(result) || ctx.tt.is_unknown(result)) {
                report_error("Mixed signed and unsigned division requires an explicit cast.");
                result = ctx.tt.get_unknown();
            }
        }
        break;

    case lex::TokenType::Percent: {
        bool both_ints = ctx.tt.is_integer_primitive(left_type) && ctx.tt.is_integer_primitive(right_type);
        bool both_floats = ctx.tt.is_float_primitive(left_type) && ctx.tt.is_float_primitive(right_type);

        if (!both_ints && !both_floats) {
            report_error("Operands for '%' must be either both integers or both floats.");
            result = ctx.tt.get_unknown();
        } else {
            result = promote_numeric_type(left_type, right_type);
            if (ctx.tt.is_any(result) || ctx.tt.is_unknown(result)) {
                report_error("Mixed signed and unsigned modulo requires an explicit cast.");
                result = ctx.tt.get_unknown();
            }
        }
        break;
    }

    case lex::TokenType::Greater:
    case lex::TokenType::GreaterEqual:
    case lex::TokenType::Less:
    case lex::TokenType::LessEqual:
        if (!ctx.tt.is_numeric_primitive(left_type) || !ctx.tt.is_numeric_primitive(right_type)) {
            report_error("Comparison operators require numeric operands.");
        } else if (ctx.tt.is_any(promote_numeric_type(left_type, right_type))) {
            report_error("Mixed signed and unsigned numeric comparison requires an explicit cast.");
        }
        result = ctx.tt.get_bool();
        break;

    case lex::TokenType::Equal:
    case lex::TokenType::NotEqual:
        if ((ctx.tt.is_optional(left_type) && ctx.tt.is_nil(right_type)) || (ctx.tt.is_nil(left_type) && ctx.tt.is_optional(right_type))) {
            result = ctx.tt.get_bool();
        } else {
            if (!is_compatible(left_type, right_type) && !is_compatible(right_type, left_type)) {
                report_error("Cannot compare incompatible types. Remember to use 'as' for enum casting.");
            }
            result = ctx.tt.get_bool();
        }
        break;

    case lex::TokenType::LogicalAnd:
    case lex::TokenType::LogicalOr:
        if (!ctx.tt.is_bool(left_type) || !ctx.tt.is_bool(right_type)) {
            report_error("Operands for logical operators must be booleans.");
        }
        result = ctx.tt.get_bool();
        break;

    default:
        type_error(loc, "Unsupported binary operator.");
        result = ctx.tt.get_unknown();
        break;
    }

    return get_node<ast::Binary_expr>(ctx.tree, expr_id).type = result;
}

types::Type_id Semantic_checker::check_cast_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto child_id = get_node<ast::Cast_expr>(ctx.tree, expr_id).expression;
    auto target_type =
        resolve_type_recursively(get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type, get_node<ast::Cast_expr>(ctx.tree, expr_id).loc);
    auto loc = get_node<ast::Cast_expr>(ctx.tree, expr_id).loc;
    get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type = target_type;

    auto original_type = check_expr(child_id);

    if (get_node<ast::Cast_expr>(ctx.tree, expr_id).is_saturating) {
        if (ctx.tt.is_unknown(original_type)) {
            return target_type;
        }
        if (!(ctx.tt.is_numeric_primitive(original_type) && ctx.tt.is_integer_primitive(target_type))) {
            type_error(
                loc,
                std::format(
                    "Saturating cast requires a numeric source and an integer target.\n   source: '{}'\n   target: '{}'",
                    ctx.tt.to_string(original_type),
                    ctx.tt.to_string(target_type)));
            get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type = ctx.tt.get_unknown();
        }
        return get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type;
    }

    bool is_str_to_byte_arr = ctx.tt.is_string(original_type) && ctx.tt.is_array(target_type)
        && (ctx.tt.get_array_elem(target_type) == ctx.tt.get_u8() || ctx.tt.get_array_elem(target_type) == ctx.tt.get_i8());

    bool is_byte_arr_to_str = ctx.tt.is_array(original_type) && ctx.tt.is_string(target_type)
        && (ctx.tt.get_array_elem(original_type) == ctx.tt.get_u8() || ctx.tt.get_array_elem(original_type) == ctx.tt.get_i8());

    if (is_str_to_byte_arr || is_byte_arr_to_str) {
        return get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type;
    }

    if (ctx.tt.is_unknown(original_type)) {
        return target_type;
    }

    if (is_compatible(target_type, original_type)) {
        return target_type;
    }
    if (ctx.tt.is_any(original_type) || ctx.tt.is_any(target_type)) {
        return target_type;
    }
    if (ctx.tt.is_numeric_primitive(original_type) && ctx.tt.is_numeric_primitive(target_type)) {
        return target_type;
    }
    if ((ctx.tt.is_bool(original_type) && ctx.tt.is_numeric_primitive(target_type))
        || (ctx.tt.is_numeric_primitive(original_type) && ctx.tt.is_bool(target_type))) {
        return target_type;
    }

    if (ctx.tt.is_array(target_type)) {
        auto t_elem = ctx.tt.get_array_elem(target_type);
        if (t_elem == ctx.tt.get_u8() || t_elem == ctx.tt.get_i8()) {
            if (ctx.tt.is_string(original_type)) {
                return target_type;
            }
            if (ctx.tt.is_array(original_type)) {
                auto s_elem = ctx.tt.get_array_elem(original_type);
                if (ctx.tt.is_bool(s_elem) || ctx.tt.is_numeric_primitive(s_elem)) {
                    return target_type;
                }
            }
            type_error(loc, "Can only cast strings or primitive arrays to byte buffers.");
            return get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type = ctx.tt.get_unknown();
        }
    }

    if (ctx.tt.is_enum(original_type) && target_type == std::get<types::Enum_type>(ctx.tt.get(original_type).data).base) {
        return target_type;
    }

    if (ctx.tt.is_enum(original_type)) {
        auto base = std::get<types::Enum_type>(ctx.tt.get(original_type).data).base;
        if (target_type == base || (ctx.tt.is_numeric_primitive(base) && ctx.tt.is_numeric_primitive(target_type))) {
            return target_type;
        }
    }

    if (ctx.tt.is_optional(original_type) && !ctx.tt.is_optional(target_type)) {
        auto base_type = ctx.tt.get_optional_base(original_type);

        if (auto path = extract_access_path(child_id)) {
            bool is_checked = false;
            for (const auto &scope : m_nil_checked_vars_stack) {
                if (scope.count(*path)) {
                    is_checked = true;
                    break;
                }
            }
            if (!is_checked) {
                type_error(loc, "Cannot cast optional type to non-optional type without a nil check.");
                return get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type = ctx.tt.get_unknown();
            }
            if (!is_compatible(target_type, base_type) && !(ctx.tt.is_numeric_primitive(base_type) && ctx.tt.is_numeric_primitive(target_type))) {
                type_error(
                    loc,
                    std::format(
                        "Cannot cast unwrapped payload.\n   optional base: '{}'\n   casted type: '{}'",
                        ctx.tt.to_string(base_type),
                        ctx.tt.to_string(target_type)));
                return get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type = ctx.tt.get_unknown();
            }
            return target_type;
        }

        type_error(
            loc,
            std::format(
                "Cannot explicitly cast a complex optional expression.\n   optional base: '{}'\n   casted type: '{}'",
                ctx.tt.to_string(base_type),
                ctx.tt.to_string(target_type)));
        return get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type = ctx.tt.get_unknown();
    }

    if (ctx.tt.is_nil(original_type) && !ctx.tt.is_optional(target_type)) {
        type_error(loc, "Cannot cast a 'nil' value to a non-optional type.");
        return get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type = ctx.tt.get_unknown();
    }

    type_error(loc, std::format("Invalid cast. Cannot cast from '{}' to '{}'.", ctx.tt.to_string(original_type), ctx.tt.to_string(target_type)));
    return get_node<ast::Cast_expr>(ctx.tree, expr_id).target_type = ctx.tt.get_unknown();
}

types::Type_id Semantic_checker::check_closure_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto &closure = get_node<ast::Closure_expr>(ctx.tree, expr_id);

    std::vector<types::Type_id> param_types;
    std::vector<types::Type_id> return_types;

    for (auto &param : closure.parameters) {
        param.type = resolve_type_recursively(param.type, param.loc);
        param_types.push_back(param.type);
    }
    for (auto &ret : closure.returns) {
        ret.type = resolve_type_recursively(ret.type, ret.loc);
        return_types.push_back(ret.type);
    }
    return_types = normalize_return_types(return_types, ctx.tt);

    closure.type = ctx.tt.function(param_types, return_types);

    auto saved_return_types = current_return_types;
    auto saved_return_params = current_return_params;
    current_return_types = return_types;
    current_return_params = &closure.returns;

    variables.begin_scope();

    for (const auto &p : closure.parameters) {
        declare(p.name, p.type, p.is_mut, p.loc);
    }

    for (const auto &ret : closure.returns) {
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

    if (!closure.body.is_null()) {
        check_stmt(closure.body);
    }

    variables.end_scope();
    current_return_types = saved_return_types;
    current_return_params = saved_return_params;

    return closure.type;
}

types::Type_id Semantic_checker::check_field_access_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto object_id = get_node<ast::Field_access_expr>(ctx.tree, expr_id).object;
    auto field_name = get_node<ast::Field_access_expr>(ctx.tree, expr_id).field_name;
    auto loc = get_node<ast::Field_access_expr>(ctx.tree, expr_id).loc;

    auto obj_type = check_expr(object_id);

    if (ctx.tt.is_optional(obj_type)) {
        type_error(loc, "Cannot access field on an optional type.");
        return get_node<ast::Field_access_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    if (ctx.tt.is_union(obj_type)) {
        auto &union_t = std::get<types::Union_type>(ctx.tt.get(obj_type).data);
        if (!contains_key(union_t.variants, field_name)) {
            type_error(loc, std::format("Union has no variant named '{}'", field_name));
            return get_node<ast::Field_access_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        auto ret = find_by_key(union_t.variants, field_name)->second;
        get_node<ast::Field_access_expr>(ctx.tree, expr_id).type = ret;
        return ret;
    }

    if (ctx.tt.is_model(obj_type)) {
        auto &model_t = std::get<types::Model_type>(ctx.tt.get(obj_type).data);
        if (contains_key(model_t.fields, field_name)) {
            auto ret = find_by_key(model_t.fields, field_name)->second;
            get_node<ast::Field_access_expr>(ctx.tree, expr_id).type = ret;
            return ret;
        }
        if (contains_key(model_t.methods, field_name)) {
            const auto &method = find_by_key(model_t.methods, field_name)->second;
            auto ret = ctx.tt.function(method.params, method.returns);
            get_node<ast::Field_access_expr>(ctx.tree, expr_id).type = ret;
            return ret;
        }
        type_error(loc, std::format("Model has no member '{}'", field_name));
    } else {
        type_error(loc, "Can only access fields on model instances");
    }

    return get_node<ast::Field_access_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
}

types::Type_id Semantic_checker::check_static_path_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto base_id = get_node<ast::Static_path_expr>(ctx.tree, expr_id).base;
    auto member_lexeme = get_node<ast::Static_path_expr>(ctx.tree, expr_id).member.lexeme;
    auto loc = get_node<ast::Static_path_expr>(ctx.tree, expr_id).loc;

    auto &expr = get_node<ast::Static_path_expr>(ctx.tree, expr_id);

    auto is_namespace_imported = [&](const std::string &alias) -> bool {
        if (current_module_id.is_null()) {
            return false;
        }
        auto &module = ctx.workspace.get_module(current_module_id);
        for (auto root_id : module.ast_roots) {
            if (root_id.is_null()) {
                continue;
            }
            if (auto *import_stmt = std::get_if<ast::Import_stmt>(&ctx.tree.get(root_id).node)) {
                std::string current_alias = import_stmt->local_alias.empty() ? import_stmt->path.back() : import_stmt->local_alias;
                if (current_alias == alias && import_stmt->selectives.empty()) {
                    return true;
                }
            }
        }
        return false;
    };

    if (auto *base_var = std::get_if<ast::Variable_expr>(&ctx.tree.get(expr.base).node)) {
        if (base_var->name == "env") {
            if (expr.member.lexeme == "line") {
                return ctx.tt.get_i64();
            }
            if (expr.member.lexeme == "file") {
                return ctx.tt.get_string();
            }
            if (expr.member.lexeme == "func") {
                return ctx.tt.get_string();
            }
            if (expr.member.lexeme == "module") {
                return ctx.tt.get_string();
            }
            if (expr.member.lexeme == "argv") {
                return ctx.tt.array(ctx.tt.get_string());
            }
        }

        std::string full_name = base_var->name + "::" + member_lexeme;
        if (auto sym_id = ctx.registry.lookup_global(full_name)) {
            // Intrinsic namespaces do not require explicit imports
            bool is_intrinsic_ns = (base_var->name == "Array" || base_var->name == "String" || base_var->name == "Optional");

            // NEW: Locally defined types (Models, Unions, Enums) bypass the import check!
            bool is_local_type = ctx.type_env.is_type_defined(base_var->name);

            if (is_intrinsic_ns || is_local_type || is_namespace_imported(base_var->name)) {
                expr.resolved_symbol = sym_id;
                if (auto native_t = ctx.type_env.get_native_type_str(full_name)) {
                    return expr.type = parse_type_string(*native_t, {});
                }
                return expr.type = resolve_symbol_type(ctx, *sym_id, *this);
            } else {
                type_error(
                    loc,
                    std::format("Namespace '{}' is not fully imported. Use 'import {};' to access '{}'.", base_var->name, base_var->name, full_name));
                return expr.type = ctx.tt.get_unknown();
            }
        }
    }

    std::optional<Module_id> base_module_id;

    if (auto *var_base = std::get_if<ast::Variable_expr>(&ctx.tree.get(base_id).node)) {
        if (is_namespace_imported(var_base->name)) {
            base_module_id = ctx.workspace.get_module(current_module_id).resolve_import(var_base->name);
        } else if (ctx.workspace.get_module(current_module_id).resolve_import(var_base->name)) {
            type_error(
                loc,
                std::format(
                    "Namespace '{}' is not fully imported. Use 'import {};' to access '{}::{}'.",
                    var_base->name,
                    var_base->name,
                    var_base->name,
                    member_lexeme));
            return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
    } else if (auto *path_base = std::get_if<ast::Static_path_expr>(&ctx.tree.get(base_id).node)) {
        check_expr(base_id);
        base_module_id = path_base->resolved_module;
    }

    if (base_module_id) {
        auto &target_module = ctx.workspace.get_module(*base_module_id);

        if (auto nested_module = target_module.resolve_import(member_lexeme)) {
            expr.resolved_module = *nested_module;
            expr.resolved_symbol.reset();
            return expr.type = ctx.tt.get_any();
        }

        if (auto sym_id = target_module.resolve_exported_symbol(member_lexeme)) {
            expr.resolved_module.reset();
            expr.resolved_symbol = *sym_id;
            auto &sym = ctx.registry.get_symbol(*sym_id);
            if (auto native_t = ctx.type_env.get_native_type_str(std::string(ctx.registry.resolve(sym.name)))) {
                return expr.type = parse_type_string(*native_t, {});
            }
            return expr.type = resolve_symbol_type(ctx, *sym_id, *this);
        }

        type_error(loc, std::format("Module does not export '{}'.", member_lexeme));
        return expr.type = ctx.tt.get_unknown();
    }

    types::Type_id base_type = ctx.tt.get_unknown();

    if (std::holds_alternative<ast::Variable_expr>(ctx.tree.get(base_id).node)) {
        auto vname = get_node<ast::Variable_expr>(ctx.tree, base_id).name;
        if (ctx.type_env.is_type_defined(vname) && !variables.lookup(vname)) {
            base_type = *ctx.type_env.get_type(vname);
            ast::get_type(ctx.tree.get(base_id).node) = base_type;
        } else {
            base_type = check_expr(base_id);
        }
    } else {
        base_type = check_expr(base_id);
    }

    if (ctx.tt.is_unknown(base_type)) {
        return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    if (ctx.tt.is_union(base_type)) {
        auto &union_t = std::get<types::Union_type>(ctx.tt.get(base_type).data);
        if (!contains_key(union_t.variants, member_lexeme)) {
            type_error(loc, std::format("Union has no variant '{}'.", member_lexeme));
            return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        auto variant_type = find_by_key(union_t.variants, member_lexeme)->second;
        if (variant_type != ctx.tt.get_void()) {
            type_error(loc, "Union payload variants are not first-class constructors.");
            return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = base_type;
    } else if (ctx.tt.is_model(base_type)) {
        auto &model_t = std::get<types::Model_type>(ctx.tt.get(base_type).data);
        if (const auto *model_data = ctx.type_env.get_model(model_t.name)) {
            if (model_data->static_field_types.contains(member_lexeme)) {
                auto field_type = model_data->static_field_types.at(member_lexeme);
                return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = field_type;
            }
            if (model_data->static_methods.contains(member_lexeme)) {
                auto method_decl_id = model_data->static_methods.at(member_lexeme).declaration;
                if (auto *decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(method_decl_id).node)) {
                    std::vector<types::Type_id> fparams;
                    fparams.reserve(decl->parameters.size());
                    for (const auto &param : decl->parameters) {
                        fparams.push_back(param.type);
                    }
                    auto ret = ctx.tt.function(fparams, declared_return_types(decl->returns, ctx.tt));
                    get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ret;
                    return ret;
                }
            }
            if (model_data->methods.contains(member_lexeme)) {
                auto method_decl_id = model_data->methods.at(member_lexeme).declaration;
                if (auto *decl = std::get_if<ast::Function_stmt>(&ctx.tree.get(method_decl_id).node)) {
                    std::vector<types::Type_id> fparams;
                    fparams.reserve(decl->parameters.size());
                    for (const auto &param : decl->parameters) {
                        fparams.push_back(param.type);
                    }
                    auto ret = ctx.tt.function(fparams, declared_return_types(decl->returns, ctx.tt));
                    get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ret;
                    return ret;
                }
            }
        }
        if (contains_key(model_t.static_methods, member_lexeme)) {
            auto sig = find_by_key(model_t.static_methods, member_lexeme)->second;
            auto ret = ctx.tt.function(sig.params, sig.returns);
            get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ret;
            return ret;
        }
        if (contains_key(model_t.methods, member_lexeme)) {
            auto sig = find_by_key(model_t.methods, member_lexeme)->second;
            std::vector<types::Type_id> fparams;
            fparams.push_back(base_type);
            fparams.insert(fparams.end(), sig.params.begin(), sig.params.end());
            auto ret = ctx.tt.function(fparams, sig.returns);
            get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ret;
            return ret;
        }
        type_error(loc, std::format("Model has no static member or method '{}'.", member_lexeme));
        return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    } else if (ctx.tt.is_enum(base_type)) {
        auto enum_data_ptr = ctx.type_env.get_enum(std::get<types::Enum_type>(ctx.tt.get(base_type).data).name);
        if (!enum_data_ptr->variants.contains(member_lexeme)) {
            type_error(loc, "Enum has no variant '" + member_lexeme + "'.");
            return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = base_type;
    }

    type_error(loc, "Scope resolution operator '::' is only supported for union, model, and enum.");
    return get_node<ast::Static_path_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
}

types::Type_id Semantic_checker::check_enum_member_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    auto member_name = get_node<ast::Enum_member_expr>(ctx.tree, expr_id).member_name;
    auto loc = get_node<ast::Enum_member_expr>(ctx.tree, expr_id).loc;

    if (!context_type) {
        type_error(loc, std::format("Cannot infer enum type for '{}.'. Context is missing.", member_name));
        return get_node<ast::Enum_member_expr>(ctx.tree, expr_id).type = ctx.type_env.tt.get_unknown();
    }

    types::Type_id expected_type = *context_type;
    if (ctx.type_env.tt.is_optional(expected_type)) {
        expected_type = ctx.type_env.tt.get_optional_base(expected_type);
    }

    if (!ctx.tt.is_enum(expected_type)) {
        type_error(loc, "Context is not an enum.");
        return get_node<ast::Enum_member_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    auto enum_data_ptr = ctx.type_env.get_enum(std::get<types::Enum_type>(ctx.tt.get(expected_type).data).name);
    if (!enum_data_ptr->variants.contains(member_name)) {
        type_error(loc, std::format("Enum has no variant '{}'.", member_name));
        return get_node<ast::Enum_member_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    return get_node<ast::Enum_member_expr>(ctx.tree, expr_id).type = *context_type;
}

types::Type_id Semantic_checker::check_field_assignment_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto object_id = get_node<ast::Field_assignment_expr>(ctx.tree, expr_id).object;
    auto field_name = get_node<ast::Field_assignment_expr>(ctx.tree, expr_id).field_name;
    auto loc = get_node<ast::Field_assignment_expr>(ctx.tree, expr_id).loc;
    auto value_id = get_node<ast::Field_assignment_expr>(ctx.tree, expr_id).value;

    auto obj_type = check_expr(object_id);
    types::Type_id field_type = ctx.tt.get_unknown();

    if (ctx.tt.is_optional(obj_type)) {
        type_error(loc, "Cannot access field on an optional type.");
    } else if (ctx.tt.is_union(obj_type)) {
        type_error(loc, "Cannot assign directly to union variant payload via field access.");
    } else if (ctx.tt.is_model(obj_type)) {
        auto &model_t = std::get<types::Model_type>(ctx.tt.get(obj_type).data);
        if (contains_key(model_t.fields, field_name)) {
            field_type = find_by_key(model_t.fields, field_name)->second;
        } else {
            type_error(loc, std::format("Model has no member '{}'", field_name));
        }
    } else {
        type_error(loc, "Can only access fields on model instances");
    }

    if (ctx.tt.is_unknown(field_type)) {
        return get_node<ast::Field_assignment_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    auto val_type = check_expr(value_id, field_type);
    if (!is_compatible(field_type, val_type)) {
        type_error(loc, "Assignment type mismatch for field");
    }
    return get_node<ast::Field_assignment_expr>(ctx.tree, expr_id).type = field_type;
}

types::Type_id Semantic_checker::check_literal_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    auto val = get_node<ast::Literal_expr>(ctx.tree, expr_id).value;
    auto base_type = get_node<ast::Literal_expr>(ctx.tree, expr_id).type;

    if (val.is_nil()) {
        return base_type;
    }

    if (context_type && ctx.tt.is_primitive(*context_type)) {
        auto target_kind = ctx.tt.get_primitive(*context_type);
        if (types::is_numeric_primitive(target_kind) && ctx.tt.is_numeric_primitive(base_type)) {
            auto coerced = coerce_numeric_literal(val, target_kind);
            if (coerced) {
                get_node<ast::Literal_expr>(ctx.tree, expr_id).value = coerced.value();
                return get_node<ast::Literal_expr>(ctx.tree, expr_id).type = *context_type;
            }
        }
    }
    return base_type;
}

types::Type_id Semantic_checker::check_unary_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto op = get_node<ast::Unary_expr>(ctx.tree, expr_id).op;
    auto loc = get_node<ast::Unary_expr>(ctx.tree, expr_id).loc;
    auto right_id = get_node<ast::Unary_expr>(ctx.tree, expr_id).right;

    auto right_type = check_expr(right_id);

    switch (op) {
    case lex::TokenType::Minus:
        if (!ctx.tt.is_numeric_primitive(right_type)) {
            type_error(loc, "Operand for '-' must be a number");
        } else if (ctx.tt.is_unsigned_integer_primitive(right_type)) {
            type_error(loc, "Operand for unary '-' cannot be unsigned.");
        }
        return get_node<ast::Unary_expr>(ctx.tree, expr_id).type = right_type;
    case lex::TokenType::LogicalNot:
        if (!ctx.tt.is_bool(right_type)) {
            type_error(loc, "Operand for '!' must be a boolean");
        }
        return get_node<ast::Unary_expr>(ctx.tree, expr_id).type = ctx.tt.get_bool();
    case lex::TokenType::BitNot:
        if (!ctx.tt.is_integer_primitive(right_type)) {
            type_error(loc, "Operand for '~' must be an integer.");
        } else if (ctx.tt.is_unsigned_integer_primitive(right_type)) {
            type_error(loc, "Operand for '~' cannot be unsigned.");
        }
        return get_node<ast::Unary_expr>(ctx.tree, expr_id).type = right_type;
    default:
        type_error(loc, "Unsupported unary operator");
        return get_node<ast::Unary_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }
}

types::Type_id Semantic_checker::check_array_literal_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    auto elements = get_node<ast::Array_literal_expr>(ctx.tree, expr_id).elements;
    auto loc = get_node<ast::Array_literal_expr>(ctx.tree, expr_id).loc;

    if (elements.empty()) {
        if (context_type && ctx.tt.is_array(*context_type)) {
            return get_node<ast::Array_literal_expr>(ctx.tree, expr_id).type = *context_type;
        }
        type_error(loc, "Cannot infer the element type of an empty array.");
        return get_node<ast::Array_literal_expr>(ctx.tree, expr_id).type = ctx.tt.array(ctx.tt.get_unknown());
    }

    types::Type_id common_type = ctx.tt.get_nil();
    uint8_t max_depth = 0;
    bool saw_nil_element = false;

    std::optional<types::Type_id> element_context = std::nullopt;
    types::Type_id hinted_base = ctx.tt.get_nil();
    uint8_t hinted_depth = 0;
    if (context_type && ctx.tt.is_array(*context_type)) {
        element_context = ctx.tt.get_array_elem(*context_type);
        hinted_base = *element_context;
        while (ctx.tt.is_optional(hinted_base)) {
            hinted_base = ctx.tt.get_optional_base(hinted_base);
            ++hinted_depth;
        }
    }

    for (const auto &elem_expr : elements) {
        auto elem_type = check_expr(elem_expr, element_context);

        auto *lit = std::get_if<ast::Literal_expr>(&ctx.tree.get(elem_expr).node);
        bool is_literal_nil = lit && lit->value.is_nil();

        uint8_t type_depth = 0;
        types::Type_id base = elem_type;
        while (ctx.tt.is_optional(base)) {
            type_depth++;
            base = ctx.tt.get_optional_base(base);
        }

        if (is_literal_nil || ctx.tt.is_nil(base)) {
            saw_nil_element = true;
            uint8_t current_depth = type_depth;
            if (current_depth == 0) {
                current_depth = std::max<uint8_t>(1, ctx.tree.get(elem_expr).auto_wrap_depth);
            }
            if (current_depth > max_depth) {
                max_depth = current_depth;
            }
            continue;
        }

        uint8_t current_depth = type_depth;
        if (current_depth > max_depth) {
            max_depth = current_depth;
        }

        if (ctx.tt.is_nil(common_type) && !ctx.tt.is_nil(hinted_base)) {
            common_type = hinted_base;
        }

        if (ctx.tt.is_nil(common_type)) {
            common_type = base;
        } else {
            if (!is_compatible(common_type, base) && !is_compatible(base, common_type)) {
                type_error(ast::get_loc(ctx.tree.get(elem_expr).node), "Array elements must have a consistent type.");
                return get_node<ast::Array_literal_expr>(ctx.tree, expr_id).type = ctx.tt.array(ctx.tt.get_unknown());
            }
        }
    }

    if (ctx.tt.is_nil(common_type) && !ctx.tt.is_nil(hinted_base)) {
        common_type = hinted_base;
        max_depth = std::max(max_depth, hinted_depth);
    }

    if (!ctx.tt.is_nil(common_type) && saw_nil_element) {
        max_depth = std::max<uint8_t>(max_depth, 1);
    }

    if (ctx.tt.is_nil(common_type)) {
        type_error(loc, "Cannot infer the element type of an all-nil array.");
        return get_node<ast::Array_literal_expr>(ctx.tree, expr_id).type = ctx.tt.array(ctx.tt.get_unknown());
    }

    for (auto elem_id : elements) {
        auto *lit = std::get_if<ast::Literal_expr>(&ctx.tree.get(elem_id).node);
        bool is_literal_nil = lit && lit->value.is_nil();

        types::Type_id elem_type = ast::get_type(ctx.tree.get(elem_id).node);
        uint8_t type_depth = 0;
        types::Type_id base = elem_type;
        while (ctx.tt.is_optional(base)) {
            type_depth++;
            base = ctx.tt.get_optional_base(base);
        }

        if (is_literal_nil || ctx.tt.is_nil(base)) {
            continue;
        }

        uint8_t logical_depth = type_depth;

        if (max_depth > logical_depth) {
            ctx.tree.get(elem_id).auto_wrap_depth += (max_depth - logical_depth);
        }
    }

    for (uint8_t i = 0; i < max_depth; ++i) {
        common_type = ctx.tt.optional(common_type);
    }

    return get_node<ast::Array_literal_expr>(ctx.tree, expr_id).type = ctx.tt.array(common_type);
}

types::Type_id Semantic_checker::check_array_access_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto array_id = get_node<ast::Array_access_expr>(ctx.tree, expr_id).array;
    auto index_id = get_node<ast::Array_access_expr>(ctx.tree, expr_id).index;
    auto loc = get_node<ast::Array_access_expr>(ctx.tree, expr_id).loc;

    auto collection_type = check_expr(array_id);

    auto index_type = check_expr(index_id);
    if (!ctx.tt.is_integer_primitive(index_type)) {
        type_error(ast::get_loc(ctx.tree.get(index_id).node), "Index must be an integer.");
    }

    if (ctx.tt.is_array(collection_type)) {
        return get_node<ast::Array_access_expr>(ctx.tree, expr_id).type = ctx.tt.get_array_elem(collection_type);
    } else if (ctx.tt.is_string(collection_type)) {
        return get_node<ast::Array_access_expr>(ctx.tree, expr_id).type = ctx.tt.get_string();
    } else {
        type_error(loc, "Subscript operator '[]' can only be used on arrays and strings.");
        return get_node<ast::Array_access_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }
}

types::Type_id Semantic_checker::check_array_assignment_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto array_id = get_node<ast::Array_assignment_expr>(ctx.tree, expr_id).array;
    auto index_id = get_node<ast::Array_assignment_expr>(ctx.tree, expr_id).index;
    auto value_id = get_node<ast::Array_assignment_expr>(ctx.tree, expr_id).value;
    auto loc = get_node<ast::Array_assignment_expr>(ctx.tree, expr_id).loc;

    auto array_type = check_expr(array_id);
    if (!ctx.tt.is_array(array_type)) {
        if (ctx.tt.is_string(array_type)) {
            type_error(loc, "Strings are immutable and you can't use '[]' on them.");
            return get_node<ast::Array_assignment_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
        }
        type_error(loc, "Subscript operator '[]' can only be used on arrays.");
        return get_node<ast::Array_assignment_expr>(ctx.tree, expr_id).type = ctx.tt.get_unknown();
    }

    auto index_type = check_expr(index_id);
    if (!ctx.tt.is_integer_primitive(index_type)) {
        type_error(ast::get_loc(ctx.tree.get(index_id).node), "Array index must be an integer.");
    }

    auto element_type = ctx.tt.get_array_elem(array_type);
    auto value_type = check_expr(value_id, element_type);

    if (!is_compatible(element_type, value_type)) {
        type_error(ast::get_loc(ctx.tree.get(value_id).node), "Type mismatch for array assignment.");
    }

    return get_node<ast::Array_assignment_expr>(ctx.tree, expr_id).type = value_type;
}

types::Type_id Semantic_checker::check_range_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto start_id = get_node<ast::Range_expr>(ctx.tree, expr_id).start;
    auto end_id = get_node<ast::Range_expr>(ctx.tree, expr_id).end;
    auto loc = get_node<ast::Range_expr>(ctx.tree, expr_id).loc;

    auto start_type = check_expr(start_id);
    auto end_type = check_expr(end_id);

    if (!ctx.tt.is_integer_primitive(start_type)) {
        type_error(loc, "Range start must be an integer.");
    }
    if (!ctx.tt.is_integer_primitive(end_type)) {
        type_error(loc, "Range end must be an integer.");
    }

    types::Type_id element_type = ctx.tt.get_i64();
    if (ctx.tt.is_integer_primitive(start_type) && ctx.tt.is_integer_primitive(end_type)) {
        element_type = promote_numeric_type(start_type, end_type);
        if (ctx.tt.is_any(element_type)) {
            type_error(loc, "Mixed signed and unsigned range bounds require an explicit cast.");
            element_type = ctx.tt.get_i64();
        }
    }

    return get_node<ast::Range_expr>(ctx.tree, expr_id).type = ctx.tt.iterator(element_type);
}

types::Type_id Semantic_checker::check_spawn_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto call_id = get_node<ast::Spawn_expr>(ctx.tree, expr_id).call;

    if (!call_id.is_null()) {
        std::ignore = check_expr(call_id);
    }
    return get_node<ast::Spawn_expr>(ctx.tree, expr_id).type = ctx.tt.get_any();
}

types::Type_id Semantic_checker::check_await_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto thread_id = get_node<ast::Await_expr>(ctx.tree, expr_id).thread;

    if (!thread_id.is_null()) {
        std::ignore = check_expr(thread_id);
    }
    return get_node<ast::Await_expr>(ctx.tree, expr_id).type = ctx.tt.get_any();
}

types::Type_id Semantic_checker::check_yield_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto value_id = get_node<ast::Yield_expr>(ctx.tree, expr_id).value;

    if (!value_id.is_null()) {
        auto ret = check_expr(value_id);
        return get_node<ast::Yield_expr>(ctx.tree, expr_id).type = ret;
    }
    return get_node<ast::Yield_expr>(ctx.tree, expr_id).type = ctx.tt.get_void();
}

} // namespace phos
