#include "semantic_checker.hpp"

#include <algorithm>
#include <cctype>
#include <format>
#include <sstream>

namespace phos {

template <typename Vec>
static auto find_by_key(const Vec &vec, const std::string &key)
{
    return std::find_if(vec.begin(), vec.end(), [&](const auto &pair) { return pair.first == key; });
}

template <typename Vec>
static bool contains_key(const Vec &vec, const std::string &key)
{
    return find_by_key(vec, key) != vec.end();
}

Semantic_checker::Semantic_checker(ast::Ast_tree &tree, Type_environment &env, mem::Arena& arena) : tree(tree), env(env), arena_(arena)
{}

void Semantic_checker::type_error(const ast::Source_location &loc, const std::string &message)
{
    errors.push_back(err::msg::error(this->phase, loc.l, loc.c, loc.file, "{}", message));
}

void Semantic_checker::type_warning(const ast::Source_location &loc, const std::string &message)
{
    errors.push_back(err::msg::warning(this->phase, loc.l, loc.c, loc.file, "{}", message));
}

void Semantic_checker::hoist_globals(const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {
        if (stmt_id.is_null()) {
            continue;
        }
        auto &node = tree.get(stmt_id).node;

        if (auto *fn = std::get_if<ast::Function_stmt>(&node)) {
            env.functions[fn->name] = types::Function_type_data{stmt_id};

        } else if (auto *model = std::get_if<ast::Model_stmt>(&node)) {
            std::vector<std::pair<std::string, types::Type_id>> tt_fields;
            for (const auto &field : model->fields) {
                tt_fields.push_back({field.name, field.type});
            }
            env.global_types[model->name] = env.tt.model(model->name, tt_fields);

            types::Model_type_data m_data;
            for (const auto &field : model->fields) {
                if (!field.default_value.is_null()) {
                    m_data.field_defaults[field.name] = field.default_value;
                }
            }
            env.model_data[model->name] = std::move(m_data);

            for (auto method_id : model->methods) {
                if (method_id.is_null()) {
                    continue;
                }
                auto &m_node = tree.get(method_id).node;

                if (auto *m_fn = std::get_if<ast::Function_stmt>(&m_node)) {
                    // 1. Inject 'this' parameter for non-static methods
                    if (!m_fn->is_static) {
                        ast::Function_param this_param;
                        this_param.name = "this";
                        this_param.type = env.global_types[model->name];
                        this_param.is_mut = false;
                        this_param.loc = m_fn->loc;
                        m_fn->parameters.insert(m_fn->parameters.begin(), this_param);
                    }

                    // 2. Rename globally to ModelName::MethodName
                    auto original_name = m_fn->name;
                    m_fn->name = model->name + "::" + m_fn->name;

                    // 3. Register in Model Data
                    if (m_fn->is_static) {
                        env.model_data[model->name].static_methods[original_name] = types::Function_type_data{method_id};
                    } else {
                        env.model_data[model->name].methods[original_name] = types::Function_type_data{method_id};
                    }

                    // 4. Register as a Global Function
                    env.functions[m_fn->name] = types::Function_type_data{method_id};
                }
            }
        } else if (auto *un = std::get_if<ast::Union_stmt>(&node)) {
            std::vector<std::pair<std::string, types::Type_id>> tt_variants;
            for (const auto &variant : un->variants) {
                tt_variants.push_back({variant.name, variant.type});
            }
            env.global_types[un->name] = env.tt.union_(un->name, tt_variants);

            types::Union_type_data u_data;
            for (const auto &variant : un->variants) {
                if (!variant.default_value.is_null()) {
                    u_data.variant_defaults[variant.name] = variant.default_value;
                }
            }
            env.union_data[un->name] = std::move(u_data);

        } else if (auto *en = std::get_if<ast::Enum_stmt>(&node)) {
            env.global_types[en->name] = env.tt.enum_(en->name, en->base_type);

            types::Enum_type_data e_data;

            bool is_string_enum = env.tt.is_string(en->base_type);
            int64_t current_val = 0;

            for (const auto &variant : en->variants) {
                if (variant.second.has_value()) {
                    if (!is_string_enum) {
                        current_val = variant.second->as_int();
                    }
                    e_data.variants[variant.first] = *variant.second;
                } else {
                    if (is_string_enum) {
                        e_data.variants[variant.first] = Value::make_string(arena_, variant.first);
                    } else {
                        e_data.variants[variant.first] = Value(current_val);
                    }
                }

                if (!is_string_enum) {
                    current_val++;
                }
            }

            env.enum_data[en->name] = std::move(e_data);
        }
    }
}

std::vector<err::msg> Semantic_checker::check(const std::vector<ast::Stmt_id> &statements)
{
    hoist_globals(statements);

    variables.begin_scope();
    m_nil_checked_vars_stack.emplace_back();

    for (auto stmt_id : statements) {
        check_stmt(stmt_id);
    }

    m_nil_checked_vars_stack.pop_back();
    variables.end_scope();
    return errors;
}

// SCOPING & LOOKUP
void Semantic_checker::declare(const std::string &name, types::Type_id type, bool is_mut, const ast::Source_location &loc)
{
    if (!variables.declare(name, type, is_mut)) {
        type_error(loc, std::format("Variable '{}' is already declared in this scope.", name));
    }
}

std::optional<Scope_symbol> Semantic_checker::lookup(const std::string &name, const ast::Source_location &loc)
{
    if (auto var_opt = variables.lookup(name)) {
        // 1. Check local scope
        return *var_opt;
    }

    // 2. Check FFI functions
    if (env.is_native_defined(name)) {
        types::Type_id dummy = env.tt.function({}, env.tt.get_any());
        return Scope_symbol{dummy, false, 0};
    }

    // 3. Check Global Functions
    if (auto func_data = env.get_function(name)) {
        const auto *decl = std::get_if<ast::Function_stmt>(&tree.get(func_data->declaration).node);
        std::vector<types::Type_id> params;
        for (const auto &p : decl->parameters) {
            params.push_back(p.type);
        }
        types::Type_id func_id = env.tt.function(params, decl->return_type);
        return Scope_symbol{func_id, false, 0};
    }

    type_error(loc, std::format("Undefined variable, function, or type '{}'", name));
    return std::nullopt;
}

// CORE UTILS
bool Semantic_checker::is_compatible(types::Type_id expected, types::Type_id actual) const
{
    if (expected == actual) {
        return true;
    }

    if (env.tt.is_unknown(expected) || env.tt.is_unknown(actual)) {
        return true;
    }
    if (env.tt.is_any(expected)) {
        return true;
    }

    if (env.tt.is_optional(expected)) {
        if (env.tt.is_nil(actual)) {
            return true;
        }
        if (env.tt.is_optional(actual)) {
            return is_compatible(env.tt.get_optional_base(expected), env.tt.get_optional_base(actual));
        }
        return is_compatible(env.tt.get_optional_base(expected), actual);
    }
    if (env.tt.is_optional(actual) || env.tt.is_nil(actual)) {
        return false;
    }

    if (env.tt.is_array(expected) && env.tt.is_array(actual)) {
        if (env.tt.is_any(env.tt.get_array_elem(expected))) {
            return true;
        }
        return is_compatible(env.tt.get_array_elem(expected), env.tt.get_array_elem(actual));
    }

    if (env.tt.is_iterator(expected) && env.tt.is_iterator(actual)) {
        types::Type_id exp_elem = env.tt.get_iter_elem(expected);
        types::Type_id act_elem = env.tt.get_iter_elem(actual);
        if (env.tt.is_any(exp_elem)) {
            return true;
        }
        return is_compatible(exp_elem, act_elem);
    }

    return false;
}

types::Type_id Semantic_checker::promote_numeric_type(types::Type_id left, types::Type_id right) const
{
    if (env.tt.is_unknown(left) || env.tt.is_unknown(right)) {
        return env.tt.get_unknown();
    }
    if (env.tt.is_any(left) || env.tt.is_any(right)) {
        return env.tt.get_any();
    }

    auto left_kind = env.tt.get_primitive(left);
    auto right_kind = env.tt.get_primitive(right);

    if (types::is_float_primitive(left_kind) || types::is_float_primitive(right_kind)) {
        if (types::is_float_primitive(left_kind) && types::is_float_primitive(right_kind)) {
            return env.tt.primitive(
                types::primitive_bit_width(left_kind) >= types::primitive_bit_width(right_kind) ? left_kind : right_kind);
        }
        return env.tt.primitive(types::is_float_primitive(left_kind) ? left_kind : right_kind);
    }

    bool same_signedness = (types::is_signed_integer_primitive(left_kind) && types::is_signed_integer_primitive(right_kind))
        || (types::is_unsigned_integer_primitive(left_kind) && types::is_unsigned_integer_primitive(right_kind));

    if (!same_signedness) {
        return env.tt.get_any();
    }

    return env.tt.primitive(types::primitive_bit_width(left_kind) >= types::primitive_bit_width(right_kind) ? left_kind : right_kind);
}

// Iterators
bool Semantic_checker::is_iterator_protocol_type(types::Type_id type) const
{
    if (env.tt.is_iterator(type)) {
        return true;
    }
    if (env.tt.is_model(type)) {
        const auto &model_name = env.tt.get(type).as<types::Model_type>().name;
        if (!model_name.empty()) {
            if (auto method = env.get_model_method(model_name, "next")) {
                if (method->declaration.is_null()) {
                    return false;
                }
                auto decl = std::get_if<ast::Function_stmt>(&tree.get(method->declaration).node);
                return decl && decl->parameters.empty() && env.tt.is_optional(decl->return_type);
            }
        }
    }
    return false;
}

types::Type_id Semantic_checker::iterator_element_type(types::Type_id type) const
{
    if (env.tt.is_iterator(type)) {
        return env.tt.get_iter_elem(type);
    }
    if (env.tt.is_model(type)) {
        const auto &model_name = env.tt.get(type).as<types::Model_type>().name;
        if (!model_name.empty()) {
            if (auto method = env.get_model_method(model_name, "next")) {
                auto decl = std::get_if<ast::Function_stmt>(&tree.get(method->declaration).node);
                if (decl && env.tt.is_optional(decl->return_type)) {
                    return env.tt.get_optional_base(decl->return_type);
                }
            }
        }
    }
    return env.tt.get_void();
}

types::Type_id Semantic_checker::to_iterator_type(types::Type_id type) const
{
    if (is_iterator_protocol_type(type)) {
        return type;
    }
    if (env.tt.is_array(type)) {
        return env.tt.iterator(env.tt.get_array_elem(type));
    }
    if (env.tt.is_string(type)) {
        return env.tt.iterator(env.tt.get_string());
    }
    if (env.tt.is_optional(type)) {
        return env.tt.iterator(env.tt.get_optional_base(type));
    }
    if (env.tt.is_nil(type)) {
        return env.tt.iterator(env.tt.get_any());
    }

    if (env.tt.is_model(type)) {
        const auto &model_name = env.tt.get(type).as<types::Model_type>().name;
        if (!model_name.empty()) {
            if (auto method = env.get_model_method(model_name, "iter")) {
                auto decl = std::get_if<ast::Function_stmt>(&tree.get(method->declaration).node);
                if (decl && decl->parameters.empty() && is_iterator_protocol_type(decl->return_type)) {
                    return decl->return_type;
                }
            }
        }
    }
    return env.tt.iterator(type);
}

// DEFAULTS VALIDATION
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
                return default_expr_uses_forbidden_names(node.start, forbidden_names)
                    || default_expr_uses_forbidden_names(node.end, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Spawn_expr>) {
                return default_expr_uses_forbidden_names(node.call, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Await_expr>) {
                return default_expr_uses_forbidden_names(node.thread, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Yield_expr>) {
                return default_expr_uses_forbidden_names(node.value, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Fstring_expr>) {
                for (auto part : node.interpolations) {
                    if (default_expr_uses_forbidden_names(part, forbidden_names)) {
                        return true;
                    }
                }
                return false;
            }
            return false;
        },
        tree.get(expr_id).node);
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
                    std::format("Default argument mismatch in function.\n   Expected: '{}'\n   Got: '{}'", env.tt.to_string(param.type), env.tt.to_string(default_type))
                );
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
            continue;
        }
        if (default_expr_uses_forbidden_names(field.default_value, forbidden_names)) {
            type_error(field.loc, std::format("Default value for field '{}' cannot reference 'this' or another member.", field.name));
        } else {
            auto default_type = check_expr(field.default_value, field.type);
            if (!is_compatible(field.type, default_type)) {
                type_error(
                    field.loc,
                    std::format("Default argument mismatch in model.\n   Expected: '{}'\n   Got: '{}'", env.tt.to_string(field.type), env.tt.to_string(default_type))
                );
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
        if (variant.type == env.tt.get_void()) {
            type_error(variant.loc, std::format("Variant '{}' does not take a payload, cannot declare default.", variant.name));
            continue;
        }
        auto default_type = check_expr(variant.default_value, variant.type);
        if (!is_compatible(variant.type, default_type)) {
            type_error(
                variant.loc,
                std::format("Default argument mismatch in model.\n   Expected: '{}'\n   Got: '{}'",
                    env.tt.to_string(variant.type),
                    env.tt.to_string(default_type)
                )
            );
        }
    }
}

// Nil Tracking
std::optional<Semantic_checker::Access_path> Semantic_checker::extract_access_path(ast::Expr_id expr_id) const
{
    if (expr_id.is_null()) {
        return std::nullopt;
    }

    auto &node = tree.get(expr_id).node;

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

        auto &idx_node = tree.get(a->index).node;

        // Semantic check 1: Is the index a variable like `i`?
        if (auto *index_var = std::get_if<ast::Variable_expr>(&idx_node)) {
            base_path->projections.push_back(Projection{.kind = Projection_kind::Var_Index, .name_val = index_var->name});
            return base_path;
        }
        // Semantic check 2: Is the index a hardcoded literal like `0`?
        if (auto *l = std::get_if<ast::Literal_expr>(&idx_node)) {
            if (l->value.is_integer()) {
                base_path->projections.push_back(
                    Projection{.kind = Projection_kind::Int_Index, .name_val = "", .int_val = l->value.as_int()}
                );
                return base_path;
            }
            if (l->value.is_u_integer()) {
                base_path->projections.push_back(
                    Projection{.kind = Projection_kind::Uint_Index, .name_val = "", .uint_val = l->value.as_uint()});
                return base_path;
            }
        }

        // If it's a complex math operation (like x[i+1]), it's unsafe to track.
        return std::nullopt;
    }

    return std::nullopt;
}

void Semantic_checker::collect_nil_check_from_optional_method(const ast::Method_call_expr &expr, bool target_truthy_branch, std::unordered_set<Access_path, Access_path_hash> &out)
{
    if (!expr.arguments.empty()) {
        return;
    }
    if (!env.tt.is_optional(ast::get_type(tree.get(expr.object).node))) {
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

void Semantic_checker::collect_nil_check_from_comparison(const ast::Binary_expr &expr, lex::TokenType target_op, std::unordered_set<Access_path, Access_path_hash> &out)
{
    if (expr.op != target_op) {
        return;
    }

    if (env.tt.is_nil(ast::get_type(tree.get(expr.right).node))) {
        if (auto path = extract_access_path(expr.left)) {
            out.insert(*path);
        }
    } else if (env.tt.is_nil(ast::get_type(tree.get(expr.left).node))) {
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
    auto &node = tree.get(expr_id).node;

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
    if (env.tt.is_optional(ast::get_type(node))) {
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
    auto &node = tree.get(expr_id).node;

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

// FFI Parsing & Matching
types::Type_id Semantic_checker::parse_type_string(std::string str, const std::unordered_map<std::string, types::Type_id> &generics) const
{
    str.erase(str.find_last_not_of(" \t\r\n") + 1);
    str.erase(0, str.find_first_not_of(" \t\r\n"));

    if (str.length() == 1 && std::isupper(str[0]) && generics.contains(str)) {
        return generics.at(str);
    }

    if (str == "i8")     { return env.tt.get_i8(); }
    if (str == "i16")    { return env.tt.get_i16(); }
    if (str == "i32")    { return env.tt.get_i32(); }
    if (str == "i64")    { return env.tt.get_i64(); }
    if (str == "u8")     { return env.tt.get_u8(); }
    if (str == "u16")    { return env.tt.get_u16(); }
    if (str == "u32")    { return env.tt.get_u32(); }
    if (str == "u64")    { return env.tt.get_u64(); }
    if (str == "f16")    { return env.tt.get_f16(); }
    if (str == "f32")    { return env.tt.get_f32(); }
    if (str == "f64")    { return env.tt.get_f64(); }
    if (str == "bool")   { return env.tt.get_bool(); }
    if (str == "string") { return env.tt.get_string(); }
    if (str == "void")   { return env.tt.get_void(); }
    if (str == "any")    { return env.tt.get_any(); }
    if (str == "nil")    { return env.tt.get_nil(); }

    if (str.starts_with("iter<") && str.ends_with(">")) {
        auto base = parse_type_string(str.substr(5, str.length() - 6), generics);
        return env.tt.iterator(base);
    }
    if (str.ends_with("[]")) {
        auto base = parse_type_string(str.substr(0, str.length() - 2), generics);
        return env.tt.array(base);
    }
    if (str.ends_with("?")) {
        auto base = parse_type_string(str.substr(0, str.length() - 1), generics);
        return env.tt.optional(base);
    }

    if (env.is_type_defined(str)) {
        return *env.get_type(str);
    }

    return env.tt.get_any();
}

bool Semantic_checker::match_ffi_type(
    std::string expected_str, types::Type_id actual_type, std::unordered_map<std::string, types::Type_id> &generics) const
{
    expected_str.erase(0, expected_str.find_first_not_of(" \t\r\n"));
    expected_str.erase(expected_str.find_last_not_of(" \t\r\n") + 1);

    if (env.tt.is_any(actual_type) || env.tt.is_unknown(actual_type)) {
        return true;
    }

    if (expected_str.find('|') != std::string::npos) {
        std::stringstream ss(expected_str);
        std::string item;
        while (std::getline(ss, item, '|')) {
            auto temp_generics = generics;
            if (match_ffi_type(item, actual_type, temp_generics)) {
                generics = temp_generics;
                return true;
            }
        }
        return false;
    }

    if (expected_str.starts_with("iter<") && expected_str.ends_with(">")) {
        if (!env.tt.is_iterator(actual_type)) {
            return false;
        }
        std::string base_str = expected_str.substr(5, expected_str.length() - 6);
        return match_ffi_type(base_str, env.tt.get_iter_elem(actual_type), generics);
    }

    if (expected_str.ends_with("[]")) {
        if (!env.tt.is_array(actual_type)) {
            return false;
        }
        std::string base_str = expected_str.substr(0, expected_str.length() - 2);
        return match_ffi_type(base_str, env.tt.get_array_elem(actual_type), generics);
    }

    if (expected_str.ends_with("?")) {
        if (env.tt.is_nil(actual_type)) {
            return true;
        }
        std::string base_str = expected_str.substr(0, expected_str.length() - 1);
        types::Type_id inner_actual = env.tt.is_optional(actual_type) ? env.tt.get_optional_base(actual_type) : actual_type;
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
            type_error(
                arg.loc,
                std::format(
                    "Argument type mismatch for parameter '{}'. Expected '{}' but got '{}'.",
                    parameters[target_index].name,
                    env.tt.to_string(parameters[target_index].type),
                    env.tt.to_string(arg_type)
                )
            );
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
    const std::vector<types::Native_param> &parameters,
    const std::vector<ast::Call_argument> &arguments,
    std::optional<types::Type_id> receiver_type)
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
    result.ordered_arguments.resize(parameters.size() - parameter_offset);

    std::vector<bool> filled(result.ordered_arguments.size(), false);
    bool seen_named = false;
    size_t next_positional = 0;

    auto advance_to_next_positional = [&]() {
        while (next_positional < filled.size() && filled[next_positional]) {
            ++next_positional;
        }
    };

    for (const auto &arg : arguments) {
        size_t target_index = filled.size();

        if (!arg.name.empty()) {
            seen_named = true;
            for (size_t i = 0; i < filled.size(); ++i) {
                if (parameters[i + parameter_offset].name == arg.name) {
                    target_index = i;
                    break;
                }
            }
            if (target_index == filled.size() || filled[target_index]) {
                return result;
            }
        } else {
            if (seen_named) {
                return result;
            }
            advance_to_next_positional();
            if (next_positional >= filled.size()) {
                return result;
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
    result.ok = true;
    return result;
}

// CORE AST TRAVERSAL
void Semantic_checker::check_stmt(ast::Stmt_id stmt_id)
{
    if (stmt_id.is_null()) {
        return;
    }
    std::visit([this](auto &s) { check_stmt_node(s); }, tree.get(stmt_id).node);
}

types::Type_id Semantic_checker::check_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    if (expr_id.is_null()) {
        return env.tt.get_void();
    }
    auto &expr = tree.get(expr_id);

    // 1. Explicit Optional Fallback Interception: .{ .val = ... }
    if (context_type && env.tt.is_optional(*context_type)) {
        if (auto *anon = std::get_if<ast::Anon_model_literal_expr>(&expr.node)) {
            if (anon->fields.size() == 1 && anon->fields[0].first == "val") {
                types::Type_id inner_context = env.tt.get_optional_base(*context_type);
                ast::Expr_id inner_expr = anon->fields[0].second;

                if (!inner_expr.is_null()) {
                    auto inner_res = check_expr(inner_expr, inner_context);
                    if (!is_compatible(inner_context, inner_res)) {
                        type_error(ast::get_loc(expr.node), "Type mismatch for explicit optional payload.");
                        return ast::get_type(expr.node) = env.tt.get_unknown();
                    }

                    uint8_t existing_wraps = tree.get(inner_expr).auto_wrap_depth;
                    expr.node = std::move(tree.get(inner_expr).node);
                    expr.auto_wrap_depth = existing_wraps + 1;
                    return ast::get_type(expr.node) = *context_type;
                }
            }
        }
    }

    // 2. Standard Expression Evaluation
    types::Type_id actual_type = std::visit([this, context_type](auto &e) -> types::Type_id { return check_expr_node(e, context_type); }, expr.node);

    // 3. Path-Based Nil Narrowing (Checks structurally!)
    if (env.tt.is_optional(actual_type)) {
        if (auto path = extract_access_path(expr_id)) {
            for (const auto &scope : m_nil_checked_vars_stack) {
                if (scope.count(*path)) {
                    actual_type = env.tt.get_optional_base(actual_type);
                    break;
                }
            }
        }
    }

    // 4. Auto-Lifting Logic
    if (context_type) {
        int expected_depth = env.tt.optional_depth(*context_type);
        int actual_depth = env.tt.optional_depth(actual_type);

        if (expected_depth > actual_depth) {
            types::Type_id expected_base = env.tt.optional_base(*context_type);
            types::Type_id actual_base   = env.tt.optional_base(actual_type);

            if (env.tt.is_nil(actual_base) || env.tt.is_unknown(actual_base)) {
                expr.auto_wrap_depth = 0;
                return ast::get_type(expr.node) = *context_type;
            }

            if (is_compatible(expected_base, actual_base)) {
                expr.auto_wrap_depth = expected_depth - actual_depth;
                return ast::get_type(expr.node) = *context_type;
            }
        }
    }

    return ast::get_type(expr.node) = actual_type;
}

// STATEMENT VISITORS
void Semantic_checker::check_stmt_node(ast::Function_stmt &stmt)
{
    auto saved_return = current_return_type;
    current_return_type = stmt.return_type;
    validate_function_defaults(stmt);

    variables.begin_scope();

    // Declare parameters in the local scope
    for (const auto &p : stmt.parameters) {
        declare(p.name, p.type, p.is_mut, stmt.loc);
    }

    // Check the body
    if (!stmt.body.is_null()) {
        check_stmt(stmt.body);
    }

    variables.end_scope();
    current_return_type = saved_return;
}

void Semantic_checker::check_stmt_node(ast::Var_stmt &stmt)
{
    if (!stmt.type_inferred && env.tt.is_unresolved(stmt.type)) {
        auto type_name = env.tt.get(stmt.type).as<types::Unresolved_type>().name;
        if (env.is_type_defined(type_name)) {
            stmt.type = *env.get_type(type_name);
        } else {
            type_error(stmt.loc, "Unknown type '" + type_name + "'.");
            stmt.type = env.tt.get_unknown();
        }
    }
    types::Type_id init_type = env.tt.get_void();
    bool initializer_failed = false;

    if (!stmt.initializer.is_null()) {
        init_type = check_expr(stmt.initializer, stmt.type_inferred ? std::nullopt : std::make_optional(stmt.type));
        if (env.tt.is_unknown(init_type)) {
            initializer_failed = true;
        }
    }

    if (stmt.type_inferred) {
        if (initializer_failed) {
            stmt.type = env.tt.get_unknown();
            return;
        }
        if (env.tt.is_array(init_type) && env.tt.get_array_elem(init_type) == env.tt.get_void()) {
            type_error(stmt.loc, "Cannot infer type of an empty array initializer.");
        }
        stmt.type = init_type;
    } else if (!stmt.initializer.is_null() && !initializer_failed && !is_compatible(stmt.type, init_type)) {
        const auto &init_node = tree.get(stmt.initializer).node;
        if (auto *lit = std::get_if<ast::Literal_expr>(&init_node);
            lit && env.tt.is_numeric_primitive(stmt.type) && env.tt.is_numeric_primitive(init_type)) {
            auto target_kind = env.tt.get_primitive(stmt.type);

            if ((lit->value.is_integer() && types::is_float_primitive(target_kind))
                || (lit->value.is_float() && !types::is_float_primitive(target_kind))) {
                type_error(
                    stmt.loc,
                    std::format(
                        "Numeric literal '{}' cannot be implicitly converted to '{}'; use an explicit cast.",
                        lit->value.to_string(),
                        env.tt.to_string(stmt.type)));
            } else {
                type_error(
                    stmt.loc,
                    std::format(
                        "Numeric literal '{}' does not fit in target type '{}'.",
                        lit->value.to_string(),
                        env.tt.to_string(stmt.type)));
            }
        } else {
            auto diagnostic = err::msg::error(this->phase, stmt.loc.l, stmt.loc.c, stmt.loc.file, "Initializer type mismatch.");
            diagnostic.expected_got(env.tt.to_string(stmt.type), env.tt.to_string(init_type));
            errors.push_back(std::move(diagnostic));
        }
    }
    declare(stmt.name, stmt.type, stmt.is_mut, stmt.loc);
}

void Semantic_checker::check_stmt_node(ast::Model_stmt &stmt)
{
    validate_model_defaults(stmt);
    auto saved_model = current_model_type;
    if (env.is_type_defined(stmt.name)) {
        current_model_type = env.get_type(stmt.name);
    }
    for (auto &method : stmt.methods) {
        check_stmt(method);
    }
    current_model_type = saved_model;
}
void Semantic_checker::check_stmt_node(ast::Union_stmt &stmt)
{
    validate_union_defaults(stmt);
}

void Semantic_checker::check_stmt_node(ast::Enum_stmt &stmt)
{
    auto enum_data_ptr = env.get_enum(stmt.name);
    if (!enum_data_ptr) {
        return;
    }

    std::unordered_set<std::string> used_names;
    std::unordered_set<int64_t> used_ints;
    std::unordered_set<std::string> used_strings;

    for (const auto &variant : stmt.variants) {
        if (!used_names.insert(variant.first).second) {
            type_error(stmt.loc, std::format("Duplicate enum variant name '{}'.", variant.first));
            continue;
        }

        auto val = enum_data_ptr->variants.at(variant.first);
        if (val.is_integer()) {
            int64_t i = val.as_int();
            if (!used_ints.insert(i).second) {
                type_error(
                    stmt.loc,
                    std::format("Duplicate enum value '{}' in variant '{}'. Enum values must be unique.", i, variant.first));
            }
        } else if (val.is_string()) {
            std::string s(val.as_string());
            if (!used_strings.insert(s).second) {
                type_error(
                    stmt.loc,
                    std::format("Duplicate enum value '\"{}\"' in variant '{}'. Enum values must be unique.", s, variant.first));
            }
        }
    }
}

void Semantic_checker::check_stmt_node(ast::Block_stmt &stmt)
{
    variables.begin_scope();
    for (auto s : stmt.statements) {
        check_stmt(s);
    }
    variables.end_scope();
}
void Semantic_checker::check_stmt_node(ast::Expr_stmt &stmt)
{
    if (!stmt.expression.is_null()) {
        check_expr(stmt.expression);
    }
}

void Semantic_checker::check_stmt_node(ast::If_stmt &stmt)
{
    auto condition_type = check_expr(stmt.condition);

    if (!env.tt.is_bool(condition_type) && !env.tt.is_optional(condition_type) && !env.tt.is_unknown(condition_type)) {
        type_error(ast::get_loc(tree.get(stmt.condition).node), "If condition must be a boolean or optional.");
    }

    std::unordered_set<Access_path, Access_path_hash> narrowed_in_then, narrowed_in_else;
    collect_nil_checked_vars_for_then(stmt.condition, narrowed_in_then);
    collect_nil_checked_vars_for_else(stmt.condition, narrowed_in_else);

    variables.begin_scope();
    m_nil_checked_vars_stack.emplace_back();

    for (const auto &path : narrowed_in_then) {
        m_nil_checked_vars_stack.back().insert(path);
    }

    if (!stmt.then_branch.is_null()) {
        check_stmt(stmt.then_branch);
    }

    m_nil_checked_vars_stack.pop_back();
    variables.end_scope();

    variables.begin_scope();
    m_nil_checked_vars_stack.emplace_back();
    for (const auto &path : narrowed_in_else) {
        m_nil_checked_vars_stack.back().insert(path);
    }

    if (!stmt.else_branch.is_null()) {
        check_stmt(stmt.else_branch);
    }

    m_nil_checked_vars_stack.pop_back();
    variables.end_scope();
}

void Semantic_checker::check_stmt_node(ast::Print_stmt &stmt)
{
    for (auto ex : stmt.expressions) {
        check_expr(ex);
    }
}

void Semantic_checker::check_stmt_node(ast::Return_stmt &stmt)
{
    if (!current_return_type) {
        type_error(stmt.loc, "Return statement used outside of a function");
        return;
    }
    if (!stmt.expression.is_null()) {
        auto val_type = check_expr(stmt.expression, current_return_type);
        if (!is_compatible(*current_return_type, val_type)) {
            auto diagnostic = err::msg::error(this->phase, stmt.loc.l, stmt.loc.c, stmt.loc.file, "Return type mismatch.");
            diagnostic.expected_got(env.tt.to_string(*current_return_type), env.tt.to_string(val_type));
            errors.push_back(std::move(diagnostic));
        }
    } else if (*current_return_type != env.tt.get_void()) {
        type_error(stmt.loc, "Function must return a value.");
    }
}

void Semantic_checker::check_stmt_node(ast::While_stmt &stmt)
{
    if (!stmt.condition.is_null()) {
        auto type = check_expr(stmt.condition);
        if (!env.tt.is_bool(type) && !env.tt.is_unknown(type)) {
            type_error(ast::get_loc(tree.get(stmt.condition).node), "Condition must be a boolean");
        }
    }
    if (!stmt.body.is_null()) {
        check_stmt(stmt.body);
    }
}

void Semantic_checker::check_stmt_node(ast::For_stmt &stmt)
{
    variables.begin_scope();
    if (!stmt.initializer.is_null()) {
        check_stmt(stmt.initializer);
    }
    if (!stmt.condition.is_null()) {
        auto type = check_expr(stmt.condition);
        if (!env.tt.is_bool(type) && !env.tt.is_unknown(type)) {
            type_error(ast::get_loc(tree.get(stmt.condition).node), "Condition must be boolean");
        }
    }
    if (!stmt.increment.is_null()) {
        check_expr(stmt.increment);
    }
    if (!stmt.body.is_null()) {
        check_stmt(stmt.body);
    }
    variables.end_scope();
}

void Semantic_checker::check_stmt_node(ast::For_in_stmt &stmt)
{
    auto iterable_type = check_expr(stmt.iterable);
    if (env.tt.is_unknown(iterable_type) || env.tt.is_any(iterable_type)) {
        return;
    }

    auto iter_type = to_iterator_type(iterable_type);
    if (!is_iterator_protocol_type(iter_type)) {
        type_error(stmt.loc, "Value in 'for..in' loop is not iterable.");
        return;
    }
    types::Type_id var_type = iterator_element_type(iter_type);

    variables.begin_scope();
    declare(stmt.var_name, var_type, false, stmt.loc);
    if (!stmt.body.is_null()) {
        check_stmt(stmt.body);
    }
    variables.end_scope();
}

void Semantic_checker::check_stmt_node(ast::Match_stmt &stmt)
{
    types::Type_id subject_type = check_expr(stmt.subject);
    if (env.tt.is_any(subject_type) || env.tt.is_unknown(subject_type)) {
        return;
    }

    bool is_union_subject = env.tt.is_union(subject_type);

    for (auto &arm : stmt.arms) {
        variables.begin_scope();
        if (!arm.is_wildcard) {
            auto &pattern_node = tree.get(arm.pattern).node;
            if (is_union_subject
                && (std::holds_alternative<ast::Static_path_expr>(pattern_node)
                    || std::holds_alternative<ast::Enum_member_expr>(pattern_node))) {
                auto &union_t = std::get<types::Union_type>(env.tt.get(subject_type).data);
                std::string variant_name;
                ast::Source_location pattern_loc = stmt.loc;

                if (auto *static_path = std::get_if<ast::Static_path_expr>(&pattern_node)) {
                    variant_name = static_path->member.lexeme;
                    pattern_loc = static_path->loc;
                    static_path->type = subject_type;
                } else if (auto *member = std::get_if<ast::Enum_member_expr>(&pattern_node)) {
                    variant_name = member->member_name;
                    pattern_loc = member->loc;
                    member->type = subject_type;
                }

                auto variant_it =
                    std::find_if(union_t.variants.begin(), union_t.variants.end(), [&](const auto &v) { return v.first == variant_name; });
                if (variant_it == union_t.variants.end()) {
                    type_error(pattern_loc, "Variant does not exist in union.");
                } else if (!arm.bind_name.empty()) {
                    if (variant_it->second == env.tt.get_void()) {
                        type_error(pattern_loc, "Variant does not hold a payload.");
                    } else {
                        declare(arm.bind_name, variant_it->second, false, stmt.loc);
                    }
                }
            } else {
                types::Type_id pattern_type = check_expr(arm.pattern, subject_type);
                if (env.tt.is_any(pattern_type) || env.tt.is_unknown(pattern_type)) {
                    variables.end_scope();
                    continue;
                }

                bool is_range_match = std::holds_alternative<ast::Range_expr>(pattern_node) && env.tt.is_integer_primitive(subject_type);

                bool has_custom_match = false;
                if (env.tt.is_model(pattern_type)) {
                    if (auto *model_data = env.get_model(std::get<types::Model_type>(env.tt.get(pattern_type).data).name)) {
                        auto method_it = model_data->methods.find("__match__");
                        if (method_it != model_data->methods.end()) {
                            auto decl = std::get_if<ast::Function_stmt>(&tree.get(method_it->second.declaration).node);
                            if (decl && decl->parameters.size() == 1 && is_compatible(decl->parameters[0].type, subject_type)) {
                                if (decl->return_type == env.tt.get_bool()) {
                                    has_custom_match = true;
                                } else {
                                    type_error(ast::get_loc(pattern_node), "__match__ method must return bool.");
                                }
                            } else {
                                type_error(ast::get_loc(pattern_node), "__match__ method must accept one argument of subject type.");
                            }
                        }
                    }
                }

                if (!is_compatible(subject_type, pattern_type) && !is_range_match && !has_custom_match) {
                    type_error(ast::get_loc(pattern_node), "Match pattern type mismatch.");
                }
            }
        }
        if (!arm.body.is_null()) {
            check_stmt(arm.body);
        }
        variables.end_scope();
    }

    bool has_wildcard = std::any_of(stmt.arms.begin(), stmt.arms.end(), [](const auto &arm) { return arm.is_wildcard; });
    if (has_wildcard) {
        return;
    }

    if (env.tt.is_enum(subject_type)) {
        auto &enum_t = std::get<types::Enum_type>(env.tt.get(subject_type).data);
        std::unordered_set<std::string> covered;
        for (const auto &arm : stmt.arms) {
            auto &pattern_node = tree.get(arm.pattern).node;
            if (auto *static_path = std::get_if<ast::Static_path_expr>(&pattern_node)) {
                covered.insert(static_path->member.lexeme);
            } else if (auto *member = std::get_if<ast::Enum_member_expr>(&pattern_node)) {
                covered.insert(member->member_name);
            }
        }
        std::vector<std::string> missing;
        auto enum_data_ptr = env.get_enum(enum_t.name);
        if (enum_data_ptr) {
            for (const auto &[name, _] : enum_data_ptr->variants) {
                if (!covered.count(name)) {
                    missing.push_back(name);
                }
            }
        }
        if (!missing.empty()) {
            type_error(stmt.loc, "Non-exhaustive match on enum.");
        }
        return;
    }

    if (is_union_subject) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(subject_type).data);
        std::unordered_set<std::string> covered;
        for (const auto &arm : stmt.arms) {
            auto &pattern_node = tree.get(arm.pattern).node;
            if (auto *static_path = std::get_if<ast::Static_path_expr>(&pattern_node)) {
                covered.insert(static_path->member.lexeme);
            } else if (auto *member = std::get_if<ast::Enum_member_expr>(&pattern_node)) {
                covered.insert(member->member_name);
            }
        }
        std::vector<std::string> missing;
        for (const auto &v : union_t.variants) {
            if (!covered.count(v.first)) {
                missing.push_back(v.first);
            }
        }
        if (!missing.empty()) {
            type_error(stmt.loc, "Non-exhaustive match on union.");
        }
        return;
    }

    type_error(stmt.loc, "Non-exhaustive match. Add a wildcard '_' arm.");
}

// EXPRESSION VISITORS
types::Type_id Semantic_checker::check_expr_node(ast::Model_literal_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    if (!env.is_type_defined(expr.model_name)) {
        type_error(expr.loc, "Unknown type '" + expr.model_name + "'.");
        return expr.type = env.tt.get_unknown();
    }

    types::Type_id resolved_type = *env.get_type(expr.model_name);

    if (env.tt.is_union(resolved_type)) {
        auto union_data_ptr = env.get_union(expr.model_name);
        auto &union_t = std::get<types::Union_type>(env.tt.get(resolved_type).data);

        if (expr.fields.size() != 1) {
            type_error(expr.loc, "Union literals must be initialized with exactly one variant field.");
            return expr.type = env.tt.get_unknown();
        }

        std::string variant_name = expr.fields[0].first;
        ast::Expr_id payload_expr = expr.fields[0].second;

        if (variant_name.empty()) {
            if (!payload_expr.is_null()) {
                if (auto *var_expr = std::get_if<ast::Variable_expr>(&tree.get(payload_expr).node)) {
                    variant_name = var_expr->name;
                    payload_expr = ast::Expr_id::null();
                    expr.fields[0].first = variant_name;
                    expr.fields[0].second = ast::Expr_id::null();
                }
            }
        }

        auto it = std::find_if(union_t.variants.begin(), union_t.variants.end(), [&](const auto &v) { return v.first == variant_name; });
        if (it == union_t.variants.end()) {
            type_error(expr.loc, std::format("Union has no variant named '{}'.", variant_name));
            return expr.type = env.tt.get_unknown();
        }

        auto expected_payload_type = it->second;

        if (payload_expr.is_null()) {
            if (expected_payload_type == env.tt.get_void()) {
                return expr.type = resolved_type;
            }

            if (union_data_ptr && union_data_ptr->variant_defaults.contains(variant_name)) {
                payload_expr = expr.fields[0].second = union_data_ptr->variant_defaults.at(variant_name);
            } else {
                type_error(expr.loc, "Variant requires a payload or default.");
                return expr.type = env.tt.get_unknown();
            }
        } else if (expected_payload_type == env.tt.get_void()) {
            type_error(ast::get_loc(tree.get(payload_expr).node), "Variant does not take a payload.");
            return expr.type = env.tt.get_unknown();
        }

        auto actual_payload_type = check_expr(payload_expr, expected_payload_type);
        if (!is_compatible(expected_payload_type, actual_payload_type)) {
            type_error(ast::get_loc(tree.get(payload_expr).node), "Type mismatch for payload.");
        }

        return expr.type = resolved_type;
    }

    if (env.tt.is_model(resolved_type)) {
        auto &model_type = std::get<types::Model_type>(env.tt.get(resolved_type).data);
        auto model_data_ptr = env.get_model(model_type.name);

        if (expr.fields.size() > model_type.fields.size()) {
            type_error(expr.loc, "Too many fields provided for model.");
            return expr.type = env.tt.get_unknown();
        }

        for (size_t i = 0; i < expr.fields.size(); ++i) {
            if (expr.fields[i].first.empty()) {
                expr.fields[i].first = model_type.fields[i].first;
            }
        }

        std::unordered_set<std::string> provided_fields;
        for (const auto &field : expr.fields) {
            if (provided_fields.contains(field.first)) {
                type_error(expr.loc, "Field provided multiple times.");
            }
            provided_fields.insert(field.first);
        }

        for (const auto &expected_field : model_type.fields) {
            if (!provided_fields.count(expected_field.first)) {
                if (model_data_ptr && model_data_ptr->field_defaults.contains(expected_field.first)) {
                    expr.fields.push_back({expected_field.first, model_data_ptr->field_defaults.at(expected_field.first)});
                    provided_fields.insert(expected_field.first);
                } else {
                    type_error(expr.loc, std::format("Missing required field '{}'.", expected_field.first));
                }
            }
        }

        for (const auto &provided_field : expr.fields) {
            const auto &field_name = provided_field.first;
            auto it =
                std::find_if(model_type.fields.begin(), model_type.fields.end(), [&](const auto &f) { return f.first == field_name; });
            if (it == model_type.fields.end()) {
                type_error(expr.loc, std::format("Unknown field '{}'.", field_name));
                continue;
            }
            auto expected_type = it->second;
            auto actual_res = check_expr(provided_field.second, expected_type);
            if (!is_compatible(expected_type, actual_res)) {
                type_error(ast::get_loc(tree.get(provided_field.second).node), "Field type mismatch.");
            }
        }
        return expr.type = resolved_type;
    }

    type_error(expr.loc, std::format("'{}' is not a model or a union.", expr.model_name));
    return expr.type = env.tt.get_unknown();
}

types::Type_id Semantic_checker::check_expr_node(ast::Anon_model_literal_expr &expr, std::optional<types::Type_id> context_type)
{
    if (!context_type) {
        type_error(expr.loc, "Cannot infer type of anonymous literal. Context missing.");
        return expr.type = env.tt.get_unknown();
    }

    types::Type_id expected_type = *context_type;
    bool is_ctx_optional = false;

    if (env.tt.is_optional(expected_type)) {
        expected_type = env.tt.get_optional_base(*context_type);
        is_ctx_optional = true;
    }

    if (env.tt.is_union(expected_type)) {
        auto &expected_union = std::get<types::Union_type>(env.tt.get(expected_type).data);
        auto union_data_ptr = env.get_union(expected_union.name);

        if (expr.fields.size() != 1) {
            type_error(expr.loc, "Anonymous union literals must have exactly one variant field.");
            return expr.type = env.tt.get_unknown();
        }

        std::string variant_name = expr.fields[0].first;
        ast::Expr_id payload_expr = expr.fields[0].second;

        if (variant_name.empty()) {
            if (!payload_expr.is_null()) {
                if (auto *var_expr = std::get_if<ast::Variable_expr>(&tree.get(payload_expr).node)) {
                    variant_name = var_expr->name;
                    payload_expr = ast::Expr_id::null();
                    expr.fields[0].first = variant_name;
                    expr.fields[0].second = ast::Expr_id::null();
                }
            }
        }

        auto it = std::find_if(expected_union.variants.begin(), expected_union.variants.end(), [&](const auto &v) {
            return v.first == variant_name;
        });
        if (it == expected_union.variants.end()) {
            type_error(expr.loc, std::format("Union has no variant named '{}'.", variant_name));
            return expr.type = env.tt.get_unknown();
        }

        auto expected_payload_type = it->second;
        if (payload_expr.is_null()) {
            if (expected_payload_type == env.tt.get_void()) {
                return expr.type = is_ctx_optional ? *context_type : expected_type;
            }

            if (union_data_ptr && union_data_ptr->variant_defaults.contains(variant_name)) {
                payload_expr = expr.fields[0].second = union_data_ptr->variant_defaults.at(variant_name);
            } else {
                type_error(expr.loc, "Variant requires a value.");
                return expr.type = env.tt.get_unknown();
            }
        } else if (expected_payload_type == env.tt.get_void()) {
            type_error(ast::get_loc(tree.get(payload_expr).node), "Variant does not take a payload.");
            return expr.type = env.tt.get_unknown();
        }

        auto actual_res = check_expr(payload_expr, expected_payload_type);
        if (!is_compatible(expected_payload_type, actual_res)) {
            type_error(ast::get_loc(tree.get(payload_expr).node), "Type mismatch for payload.");
        }

        return expr.type = is_ctx_optional ? *context_type : expected_type;
    }

    if (env.tt.is_model(expected_type)) {
        auto &expected_model = std::get<types::Model_type>(env.tt.get(expected_type).data);
        auto model_data_ptr = env.get_model(expected_model.name);

        if (expr.fields.size() > expected_model.fields.size()) {
            type_error(expr.loc, "Too many fields for anonymous model.");
            return expr.type = env.tt.get_unknown();
        }

        bool has_named = std::any_of(expr.fields.begin(), expr.fields.end(), [](const auto &f) { return !f.first.empty(); });
        if (has_named) {
            bool has_pos = std::any_of(expr.fields.begin(), expr.fields.end(), [](const auto &f) { return f.first.empty(); });
            if (has_pos) {
                type_error(expr.loc, "Cannot mix positional and named fields.");
                return expr.type = env.tt.get_unknown();
            }
        } else {
            for (size_t i = 0; i < expr.fields.size(); ++i) {
                if (expr.fields[i].first.empty()) {
                    expr.fields[i].first = expected_model.fields[i].first;
                }
            }
        }

        for (size_t i = 0; i < expr.fields.size(); ++i) {
            auto it = std::find_if(expected_model.fields.begin(), expected_model.fields.end(), [&](const auto &f) {
                return f.first == expr.fields[i].first;
            });
            if (it == expected_model.fields.end()) {
                continue;
            }

            auto act_field = check_expr(expr.fields[i].second, it->second);
            if (!is_compatible(it->second, act_field)) {
                type_error(expr.loc, "Type mismatch for field.");
            }
        }

        std::vector<std::pair<std::string, ast::Expr_id>> ordered;
        ordered.reserve(expected_model.fields.size());
        for (const auto &expected_field : expected_model.fields) {
            auto it = std::find_if(expr.fields.begin(), expr.fields.end(), [&](const auto &f) { return f.first == expected_field.first; });
            if (it != expr.fields.end()) {
                ordered.push_back(*it);
                continue;
            }
            if (model_data_ptr && model_data_ptr->field_defaults.contains(expected_field.first)) {
                ordered.push_back({expected_field.first, model_data_ptr->field_defaults.at(expected_field.first)});
                continue;
            }
            type_error(expr.loc, std::format("Missing required field '{}'.", expected_field.first));
        }
        expr.fields = std::move(ordered);
        return expr.type = is_ctx_optional ? *context_type : expected_type;
    }

    type_error(expr.loc, "Context for anonymous literal is neither a model nor a union.");
    return expr.type = env.tt.get_unknown();
}

types::Type_id Semantic_checker::check_expr_node(ast::Assignment_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto var_info_res = lookup(expr.name, expr.loc);
    if (!var_info_res) {
        return env.tt.get_unknown();
    }

    if (!var_info_res->is_mut) {
        type_error(expr.loc, "Cannot assign to an immutable/constant variable.");
    }

    auto var_type = var_info_res->type;
    auto val_type_res = check_expr(expr.value, var_type);

    if (!is_compatible(var_type, val_type_res)) {
        auto diagnostic = err::msg::error(this->phase, expr.loc.l, expr.loc.c, expr.loc.file, "Assignment type mismatch.");
        diagnostic.expected_got(env.tt.to_string(var_type), env.tt.to_string(val_type_res));
        errors.push_back(std::move(diagnostic));
    }
    return expr.type = var_type;
}

types::Type_id Semantic_checker::check_expr_node(ast::Variable_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    if (expr.name == "this") {
        if (!current_model_type) {
            type_error(expr.loc, "Cannot use 'this' outside of a model method");
            return expr.type = env.tt.get_unknown();
        }
        return expr.type = *current_model_type;
    }

    auto type_res = lookup(expr.name, expr.loc);
    if (!type_res) {
        return expr.type = env.tt.get_unknown();
    }

    return expr.type = type_res->type;
}

types::Type_id Semantic_checker::check_expr_node(ast::Binary_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto left_type = check_expr(expr.left);
    auto right_type = check_expr(expr.right);

    auto report_error = [&](const std::string &message) {
        type_error(
            expr.loc,
            std::format("{} (left is '{}', right is '{}')", message, env.tt.to_string(left_type), env.tt.to_string(right_type)));
    };

    switch (expr.op) {
    case lex::TokenType::Pipe:
    case lex::TokenType::BitXor:
    case lex::TokenType::BitAnd:
    case lex::TokenType::BitLShift:
    case lex::TokenType::BitRshift:
        if (!env.tt.is_integer_primitive(left_type) || !env.tt.is_integer_primitive(right_type)) {
            report_error("Bitwise operators require integer operands.");
            return expr.type = env.tt.get_unknown();
        }
        expr.type = promote_numeric_type(left_type, right_type);
        if (env.tt.is_any(expr.type)) {
            report_error("Bitwise operators require integers from the same signedness family.");
            return expr.type = env.tt.get_unknown();
        }
        return expr.type;

    case lex::TokenType::Plus:
        if (env.tt.is_string(left_type) && env.tt.is_string(right_type)) {
            return expr.type = env.tt.get_string();
        }
        if (env.tt.is_numeric_primitive(left_type) && env.tt.is_numeric_primitive(right_type)) {
            expr.type = promote_numeric_type(left_type, right_type);
            if (env.tt.is_any(expr.type)) {
                report_error("Mixed signed and unsigned integer arithmetic requires an explicit cast.");
                return expr.type = env.tt.get_unknown();
            }
            return expr.type;
        }
        report_error("Operands must be two numbers or two strings for '+'");
        return expr.type = env.tt.get_unknown();

    case lex::TokenType::Minus:
    case lex::TokenType::Star:
        if (!env.tt.is_numeric_primitive(left_type) || !env.tt.is_numeric_primitive(right_type)) {
            report_error("Operands must be two numbers for this operator.");
            return expr.type = env.tt.get_unknown();
        }
        expr.type = promote_numeric_type(left_type, right_type);
        if (env.tt.is_any(expr.type)) {
            report_error("Mixed signed and unsigned integer arithmetic requires an explicit cast.");
            return expr.type = env.tt.get_unknown();
        }
        return expr.type;

    case lex::TokenType::Slash:
        if (!env.tt.is_numeric_primitive(left_type) || !env.tt.is_numeric_primitive(right_type)) {
            report_error("Operands for division must be numbers.");
            return expr.type = env.tt.get_unknown();
        }
        expr.type = promote_numeric_type(left_type, right_type);
        if (env.tt.is_any(expr.type) || env.tt.is_unknown(expr.type)) {
            report_error("Mixed signed and unsigned division requires an explicit cast.");
            return expr.type = env.tt.get_unknown();
        }
        return expr.type;

    case lex::TokenType::Percent: {
        bool both_ints = env.tt.is_integer_primitive(left_type) && env.tt.is_integer_primitive(right_type);
        bool both_floats = env.tt.is_float_primitive(left_type) && env.tt.is_float_primitive(right_type);

        if (!both_ints && !both_floats) {
            report_error("Operands for '%' must be either both integers or both floats.");
            return expr.type = env.tt.get_unknown();
        }

        expr.type = promote_numeric_type(left_type, right_type);

        if (env.tt.is_any(expr.type) || env.tt.is_unknown(expr.type)) {
            report_error("Mixed signed and unsigned modulo requires an explicit cast.");
            return expr.type = env.tt.get_unknown();
        }

        return expr.type;
    }

    case lex::TokenType::Greater:
    case lex::TokenType::GreaterEqual:
    case lex::TokenType::Less:
    case lex::TokenType::LessEqual:
        if (!env.tt.is_numeric_primitive(left_type) || !env.tt.is_numeric_primitive(right_type)) {
            report_error("Comparison operators require numeric operands.");
        } else if (env.tt.is_any(promote_numeric_type(left_type, right_type))) {
            report_error("Mixed signed and unsigned numeric comparison requires an explicit cast.");
        }
        return expr.type = env.tt.get_bool();

    case lex::TokenType::Equal:
    case lex::TokenType::NotEqual:
        if ((env.tt.is_optional(left_type) && env.tt.is_nil(right_type)) || (env.tt.is_nil(left_type) && env.tt.is_optional(right_type))) {
            return expr.type = env.tt.get_bool();
        }
        if (!is_compatible(left_type, right_type) && !is_compatible(right_type, left_type)) {
            report_error("Cannot compare incompatible types. Remember to use 'as' for enum casting.");
        }
        return expr.type = env.tt.get_bool();

    case lex::TokenType::LogicalAnd:
    case lex::TokenType::LogicalOr:
        if (!env.tt.is_bool(left_type) || !env.tt.is_bool(right_type)) {
            report_error("Operands for logical operators must be booleans.");
        }
        return expr.type = env.tt.get_bool();

    default:
        type_error(expr.loc, "Unsupported binary operator.");
        return expr.type = env.tt.get_unknown();
    }
}

types::Type_id Semantic_checker::check_expr_node(ast::Call_expr &expr, std::optional<types::Type_id> context_type)
{

    (void)context_type;
    auto has_named_arguments = [&]() {
        return std::any_of(expr.arguments.begin(), expr.arguments.end(), [](const auto &arg) { return !arg.name.empty(); });
    };

    if (auto *var_callee = std::get_if<ast::Variable_expr>(&tree.get(expr.callee).node)) {
        if (var_callee->name == "len") {
            if (expr.arguments.size() != 1) {
                type_error(expr.loc, "len() expects exactly one argument.");
                return expr.type = env.tt.get_unknown();
            }
            if (!expr.arguments[0].name.empty()) {
                type_error(expr.arguments[0].loc, "len() does not support named arguments.");
                return expr.type = env.tt.get_unknown();
            }
            auto arg_type = check_expr(expr.arguments[0].value);
            if (!env.tt.is_string(arg_type) && !env.tt.is_array(arg_type)) {
                type_error(expr.loc, "len() expects a string or an array.");
            }
            return expr.type = env.tt.get_u64();
        }

        if (var_callee->name == "iter") {
            if (has_named_arguments()) {
                type_error(expr.loc, "iter() does not support named arguments.");
                return expr.type = env.tt.get_unknown();
            }
            if (expr.arguments.size() != 1) {
                type_error(expr.loc, "iter() expects exactly one argument.");
                return expr.type = env.tt.get_unknown();
            }
            auto arg_type_res = check_expr(expr.arguments[0].value);
            auto iter_type = to_iterator_type(arg_type_res);
            if (!is_iterator_protocol_type(iter_type)) {
                type_error(expr.loc, "Value passed to iter() is not iterable.");
                return expr.type = env.tt.get_unknown();
            }
            return expr.type = iter_type;
        }
        if (var_callee->name == "clone") {
            if (has_named_arguments()) {
                type_error(expr.loc, "clone() does not support named arguments.");
                return expr.type = env.tt.get_unknown();
            }
            if (expr.arguments.size() != 1) {
                type_error(expr.loc, "clone() expects exactly one argument.");
                return expr.type = env.tt.get_unknown();
            }
            return expr.type = check_expr(expr.arguments[0].value);
        }
    }

    // --- STEP 2: NATIVE FFI FUNCTIONS ---
    std::string ffi_callee_name;
    if (auto *var_callee = std::get_if<ast::Variable_expr>(&tree.get(expr.callee).node)) {
        ffi_callee_name = var_callee->name;
    } else if (auto *path_callee = std::get_if<ast::Static_path_expr>(&tree.get(expr.callee).node)) {
        if (auto *base_var = std::get_if<ast::Variable_expr>(&tree.get(path_callee->base).node)) {
            ffi_callee_name = base_var->name + "::" + path_callee->member.lexeme;
        }
    }

    if (!ffi_callee_name.empty() && env.is_native_defined(ffi_callee_name)) {
        const auto &signatures = *env.get_native_signatures(ffi_callee_name);

        for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
            auto bound = try_bind_native_arguments(signatures[sig_index].params, expr.arguments);
            if (bound.ok) {
                expr.arguments = std::move(bound.ordered_arguments);
                expr.native_signature_index = static_cast<int>(sig_index);
                return expr.type = parse_type_string(signatures[sig_index].ret_type_str, bound.generics);
            }
        }
        type_error(expr.loc, std::format("Arguments do not match any native signature for function '{}'.",ffi_callee_name));
        return expr.type = env.tt.get_unknown();
    }

    if (auto *static_path = std::get_if<ast::Static_path_expr>(&tree.get(expr.callee).node)) {

        // 1. Intercept Type-level base cleanly
        types::Type_id base_type_res = env.tt.get_unknown();
        if (auto *var_base = std::get_if<ast::Variable_expr>(&tree.get(static_path->base).node)) {
            if (env.is_type_defined(var_base->name) && !variables.lookup(var_base->name)) {
                base_type_res = *env.get_type(var_base->name);
                ast::get_type(tree.get(static_path->base).node) = base_type_res;
            } else {
                base_type_res = check_expr(static_path->base);
            }
        } else {
            base_type_res = check_expr(static_path->base);
        }

        if (env.tt.is_union(base_type_res)) {
            type_error(expr.loc, "Union construction via '::' is not supported.");
            return expr.type = env.tt.get_unknown();
        } else if (env.tt.is_model(base_type_res)) {
            auto &model_name = std::get<types::Model_type>(env.tt.get(base_type_res).data).name;
            auto model_data_ptr = env.get_model(model_name);

            if (model_data_ptr) {
                if (model_data_ptr->static_methods.contains(static_path->member.lexeme)) {
                    // STATIC METHOD CALL
                    auto method_decl_id = model_data_ptr->static_methods.at(static_path->member.lexeme).declaration;
                    auto decl = std::get_if<ast::Function_stmt>(&tree.get(method_decl_id).node);

                    auto bound =
                        bind_call_arguments(decl->parameters, expr.arguments, expr.loc, "static method", static_path->member.lexeme);
                    expr.arguments = std::move(bound.ordered_arguments);

                    std::vector<types::Type_id> fparams;
                    for (auto &p : decl->parameters) {
                        fparams.push_back(p.type);
                    }
                    static_path->type = env.tt.function(fparams, decl->return_type);

                    return expr.type = decl->return_type;

                } else if (model_data_ptr->methods.contains(static_path->member.lexeme)) {
                    // UFCS METHOD CALL
                    if (expr.arguments.empty()) {
                        type_error(expr.loc, "UFCS call requires an instance as the first argument.");
                        return expr.type = env.tt.get_unknown();
                    }
                    if (!expr.arguments[0].name.empty()) {
                        type_error(expr.arguments[0].loc, "First argument of UFCS cannot be named.");
                        return expr.type = env.tt.get_unknown();
                    }

                    auto receiver_type = check_expr(expr.arguments[0].value, base_type_res);
                    if (!is_compatible(base_type_res, receiver_type)) {
                        type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "UFCS receiver type mismatch.");
                        return expr.type = env.tt.get_unknown();
                    }

                    auto method_decl_id = model_data_ptr->methods.at(static_path->member.lexeme).declaration;
                    auto decl = std::get_if<ast::Function_stmt>(&tree.get(method_decl_id).node);

                    auto bound = bind_call_arguments(decl->parameters, expr.arguments, expr.loc, "method", static_path->member.lexeme);
                    expr.arguments = std::move(bound.ordered_arguments);

                    std::vector<types::Type_id> fparams;
                    for (auto &p : decl->parameters) {
                        fparams.push_back(p.type);
                    }
                    static_path->type = env.tt.function(fparams, decl->return_type);

                    return expr.type = decl->return_type;
                }
            }

            type_error(static_path->loc, std::format("Model '{}' has no method '{}'.", model_name, static_path->member.lexeme));
            return expr.type = env.tt.get_unknown();
        }
    }

    // STEP 4: Standard Function / Closure Calls
    auto callee_type = check_expr(expr.callee);
    if (env.tt.is_unknown(callee_type)) {
        return expr.type = env.tt.get_unknown();
    }

    if (!env.tt.is_function(callee_type)) {
        type_error(ast::get_loc(tree.get(expr.callee).node), "This expression cannot be called.");
        return expr.type = env.tt.get_unknown();
    }

    auto sig = env.tt.get(callee_type).as<types::Function_type>();
    const ast::Function_stmt *declaration = nullptr;
    if (auto *var_callee = std::get_if<ast::Variable_expr>(&tree.get(expr.callee).node)) {
        if (env.is_function_defined(var_callee->name)) {
            declaration = std::get_if<ast::Function_stmt>(&tree.get(env.get_function(var_callee->name)->declaration).node);
        }
    }

    if (declaration) {
        auto bound = bind_call_arguments(declaration->parameters, expr.arguments, expr.loc, "function", declaration->name);
        expr.arguments = std::move(bound.ordered_arguments);
    } else {
        if (has_named_arguments()) {
            type_error(expr.loc, "Named arguments only supported for direct static calls.");
            return expr.type = sig.ret;
        }
        if (expr.arguments.size() != sig.params.size()) {
            type_error(expr.loc, "Incorrect number of arguments.");
            return expr.type = sig.ret;
        }
        for (size_t i = 0; i < expr.arguments.size(); ++i) {
            auto arg_type = check_expr(expr.arguments[i].value, sig.params[i]);
            if (!is_compatible(sig.params[i], arg_type)) {
                auto loc = ast::get_loc(tree.get(expr.arguments[i].value).node);
                auto diagnostic = err::msg::error(this->phase, loc.l, loc.c, loc.file, "Argument type mismatch.");
                diagnostic.expected_got(env.tt.to_string(sig.params[i]), env.tt.to_string(arg_type));
                errors.push_back(std::move(diagnostic));
            }
        }
    }
    return expr.type = sig.ret;
}

types::Type_id Semantic_checker::check_expr_node(ast::Cast_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto original_type = check_expr(expr.expression);
    auto target_type = expr.target_type;

    bool is_str_to_byte_arr = env.tt.is_string(original_type) && env.tt.is_array(expr.target_type)
        && (env.tt.get_array_elem(expr.target_type) == env.tt.get_u8() || env.tt.get_array_elem(expr.target_type) == env.tt.get_i8());

    bool is_byte_arr_to_str = env.tt.is_array(original_type) && env.tt.is_string(expr.target_type)
        && (env.tt.get_array_elem(original_type) == env.tt.get_u8() || env.tt.get_array_elem(original_type) == env.tt.get_i8());

    if (is_str_to_byte_arr || is_byte_arr_to_str) {
        return expr.target_type;
    }

    if (env.tt.is_unknown(original_type)) {
        return target_type;
    }

    if (is_compatible(target_type, original_type)) {
        return target_type;
    }
    if (env.tt.is_any(original_type) || env.tt.is_any(target_type)) {
        return target_type;
    }
    if (env.tt.is_numeric_primitive(original_type) && env.tt.is_numeric_primitive(target_type)) {
        return target_type;
    }
    if ((env.tt.is_bool(original_type) && env.tt.is_numeric_primitive(target_type))
        || (env.tt.is_numeric_primitive(original_type) && env.tt.is_bool(target_type))) {
        return target_type;
    }

    if (env.tt.is_array(target_type)) {
        auto t_elem = env.tt.get_array_elem(target_type);
        if (t_elem == env.tt.get_u8() || t_elem == env.tt.get_i8()) {
            if (env.tt.is_string(original_type)) {
                return target_type;
            }
            if (env.tt.is_array(original_type)) {
                auto s_elem = env.tt.get_array_elem(original_type);
                if (env.tt.is_bool(s_elem) || env.tt.is_numeric_primitive(s_elem)) {
                    return target_type;
                }
            }
            type_error(expr.loc, "Can only cast strings or primitive arrays to byte buffers.");
            return expr.target_type = env.tt.get_unknown();
        }
    }

    if (env.tt.is_enum(original_type) && target_type == std::get<types::Enum_type>(env.tt.get(original_type).data).base) {
        return target_type;
    }

    if (env.tt.is_enum(original_type)) {
        auto base = std::get<types::Enum_type>(env.tt.get(original_type).data).base;
        if (target_type == base || (env.tt.is_numeric_primitive(base) && env.tt.is_numeric_primitive(target_type))) {
            return target_type;
        }
    }

    if (env.tt.is_optional(original_type) && !env.tt.is_optional(target_type)) {
        auto base_type = env.tt.get_optional_base(original_type);

        // Use the path extractor here!
        if (auto path = extract_access_path(expr.expression)) {
            bool is_checked = false;
            for (const auto &scope : m_nil_checked_vars_stack) {
                if (scope.count(*path)) {
                    is_checked = true;
                    break;
                }
            }
            if (!is_checked) {
                type_error(expr.loc, "Cannot cast optional type to non-optional type without a nil check.");
                return expr.target_type = env.tt.get_unknown();
            }
            if (!is_compatible(target_type, base_type)
                && !(env.tt.is_numeric_primitive(base_type) && env.tt.is_numeric_primitive(target_type))) {
                type_error(
                    expr.loc,
                    std::format(
                        "Cannot cast unwrapped payload.\n    optional base: '{}'\n    casted type: '{}'",
                        env.tt.to_string(base_type),
                        env.tt.to_string(target_type)));
                return expr.target_type = env.tt.get_unknown();
            }
            return target_type;
        }

        type_error(
            expr.loc,
            std::format(
                "Cannot explicitly cast a complex optional expression.\n    optional base: '{}'\n    casted type: '{}'",
                env.tt.to_string(base_type),
                env.tt.to_string(target_type)));
        return expr.target_type = env.tt.get_unknown();
    }

    if (env.tt.is_nil(original_type) && !env.tt.is_optional(target_type)) {
        type_error(expr.loc, "Cannot cast a 'nil' value to a non-optional type.");
        return expr.target_type = env.tt.get_unknown();
    }

    type_error(
        expr.loc,
        std::format("Invalid cast. Cannot cast from '{}' to '{}'.", env.tt.to_string(original_type), env.tt.to_string(target_type))
    );
    return expr.target_type = env.tt.get_unknown();
}

types::Type_id Semantic_checker::check_expr_node(ast::Closure_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto saved_ret = current_return_type;
    current_return_type = expr.return_type;

    variables.begin_scope();
    for (auto const &p : expr.parameters) {
        declare(p.name, p.type, !p.is_mut, expr.loc);
    }
    if (!expr.body.is_null()) {
        check_stmt(expr.body);
    }
    variables.end_scope();

    current_return_type = saved_ret;

    std::vector<types::Type_id> param_types;
    for (auto const &p : expr.parameters) {
        param_types.push_back(p.type);
    }

    return expr.type = env.tt.function(param_types, expr.return_type);
}

types::Type_id Semantic_checker::check_expr_node(ast::Field_access_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto obj_type = check_expr(expr.object);

    if (env.tt.is_optional(obj_type)) {
        type_error(expr.loc, "Cannot access field on an optional type.");
        return expr.type = env.tt.get_unknown();
    }

    if (env.tt.is_union(obj_type)) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(obj_type).data);
        if (!contains_key(union_t.variants, expr.field_name)) {
            type_error(expr.loc, std::format("Union has no variant named '{}'", expr.field_name));
            return expr.type = env.tt.get_unknown();
        }
        return expr.type = find_by_key(union_t.variants, expr.field_name)->second;
    }

    if (env.tt.is_model(obj_type)) {
        auto &model_t = std::get<types::Model_type>(env.tt.get(obj_type).data);
        if (contains_key(model_t.fields, expr.field_name)) {
            return expr.type = find_by_key(model_t.fields, expr.field_name)->second;
        }
        if (contains_key(model_t.methods, expr.field_name)) {
            const auto &method = find_by_key(model_t.methods, expr.field_name)->second;
            return expr.type = env.tt.function(method.params, method.ret);
        }
        type_error(expr.loc, std::format("Model has no member '{}'", expr.field_name));
    } else {
        type_error(expr.loc, "Can only access fields on model instances");
    }

    return expr.type = env.tt.get_unknown();
}

types::Type_id Semantic_checker::check_expr_node(ast::Static_path_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto base_type = check_expr(expr.base);

    if (auto *var_expr = std::get_if<ast::Variable_expr>(&tree.get(expr.base).node)) {
        if (env.is_type_defined(var_expr->name) && !variables.lookup(var_expr->name)) {
            base_type = *env.get_type(var_expr->name);
            ast::get_type(tree.get(expr.base).node) = base_type;
        } else {
            base_type = check_expr(expr.base);
        }
    } else {
        base_type = check_expr(expr.base);
    }

    if (env.tt.is_union(base_type)) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(base_type).data);
        if (!contains_key(union_t.variants, expr.member.lexeme)) {
            type_error(expr.loc, std::format("Union has no variant '{}'.", expr.member.lexeme));
            return expr.type = env.tt.get_unknown();
        }
        auto variant_type = find_by_key(union_t.variants, expr.member.lexeme)->second;
        if (variant_type != env.tt.get_void()) {
            type_error(expr.loc, "Union payload variants are not first-class constructors.");
            return expr.type = env.tt.get_unknown();
        }
        return expr.type = base_type;
    } else if (env.tt.is_model(base_type)) {
        auto &model_t = std::get<types::Model_type>(env.tt.get(base_type).data);
        if (contains_key(model_t.static_methods, expr.member.lexeme)) {
            auto sig = find_by_key(model_t.static_methods, expr.member.lexeme)->second;
            return expr.type = env.tt.function(sig.params, sig.ret);
        }
        if (contains_key(model_t.methods, expr.member.lexeme)) {
            auto sig = find_by_key(model_t.methods, expr.member.lexeme)->second;
            std::vector<types::Type_id> fparams;
            fparams.push_back(base_type);
            fparams.insert(fparams.end(), sig.params.begin(), sig.params.end());
            return expr.type = env.tt.function(fparams, sig.ret);
        }
        type_error(expr.loc, std::format("Model has no method '{}'.", expr.member.lexeme));
        return expr.type = env.tt.get_unknown();
    } else if (env.tt.is_enum(base_type)) {
        auto enum_data_ptr = env.get_enum(std::get<types::Enum_type>(env.tt.get(base_type).data).name);
        if (!enum_data_ptr->variants.contains(expr.member.lexeme)) {
            type_error(expr.loc, "Enum has no variant '" + expr.member.lexeme + "'.");
            return expr.type = env.tt.get_unknown();
        }
        return expr.type = base_type;
    }

    type_error(expr.loc, "Scope resolution operator '::' is only supported for union, model, and enum.");
    return expr.type = env.tt.get_unknown();
}

types::Type_id Semantic_checker::check_expr_node(ast::Enum_member_expr &expr, std::optional<types::Type_id> context_type)
{
    if (!context_type) {
        type_error(expr.loc, std::format("Cannot infer enum type for '{}.'. Context is missing.", expr.member_name));
        return expr.type = env.tt.get_unknown();
    }

    types::Type_id expected_type = *context_type;
    if (env.tt.is_optional(expected_type)) {
        expected_type = env.tt.get_optional_base(expected_type);
    }

    if (!env.tt.is_enum(expected_type)) {
        type_error(expr.loc, "Context is not an enum.");
        return expr.type = env.tt.get_unknown();
    }

    auto enum_data_ptr = env.get_enum(std::get<types::Enum_type>(env.tt.get(expected_type).data).name);
    if (!enum_data_ptr->variants.contains(expr.member_name)) {
        type_error(expr.loc, std::format("Enum has no variant '{}'.", expr.member_name));
        return expr.type = env.tt.get_unknown();
    }

    return expr.type = *context_type;
}

types::Type_id Semantic_checker::check_expr_node(ast::Field_assignment_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto field_access_temp = ast::Field_access_expr{expr.object, expr.field_name, env.tt.get_unknown(), expr.loc};
    auto field_type = check_expr_node(field_access_temp, std::nullopt);
    expr.object = field_access_temp.object;
    if (env.tt.is_unknown(field_type)) {
        return expr.type = env.tt.get_unknown();
    }

    auto val_type = check_expr(expr.value, field_type);
    if (!is_compatible(field_type, val_type)) {
        type_error(expr.loc, "Assignment type mismatch for field");
    }
    return expr.type = field_type;
}

types::Type_id Semantic_checker::check_expr_node(ast::Literal_expr &expr, std::optional<types::Type_id> context_type)
{
    if (expr.value.is_nil()) {
        return expr.type = env.tt.get_nil();
    }

    if (context_type && env.tt.is_primitive(*context_type)) {
        auto target_kind = env.tt.get_primitive(*context_type);
        if (types::is_numeric_primitive(target_kind) && env.tt.is_numeric_primitive(expr.type)) {
            auto coerced = coerce_numeric_literal(expr.value, target_kind);
            if (coerced) {
                expr.value = coerced.value();
                return expr.type = *context_type;
            }
        }
    }
    return expr.type;
}

types::Type_id Semantic_checker::check_expr_node(ast::Method_call_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto has_named_arguments = [&]() {
        return std::any_of(expr.arguments.begin(), expr.arguments.end(), [](const auto &arg) { return !arg.name.empty(); });
    };
    auto obj_type = check_expr(expr.object);

    if (env.tt.is_any(obj_type)) {
        for (auto &arg : expr.arguments) {
            if (!arg.value.is_null()) {
                std::ignore = check_expr(arg.value);
            }
        }
        return expr.type = env.tt.get_any();
    }

    if (env.tt.is_union(obj_type)) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(obj_type).data);

        if (expr.method_name == "has" || expr.method_name == "get") {
            if (expr.arguments.size() != 1) {
                type_error(expr.loc, std::format(".{}() expects exactly one argument.", expr.method_name));
                return expr.type = env.tt.get_unknown();
            }

            std::string variant_name;
            auto arg_expr_node = &tree.get(expr.arguments[0].value).node;

            if (auto *static_path = std::get_if<ast::Static_path_expr>(arg_expr_node)) {
                if (auto *var_expr = std::get_if<ast::Variable_expr>(&tree.get(static_path->base).node)) {
                    if (var_expr->name != union_t.name) {
                        type_error(
                            ast::get_loc(*arg_expr_node),
                            std::format("Argument to .{}() must be a variant of the same union type.", expr.method_name));
                    }
                    variant_name = static_path->member.lexeme;
                }
            } else if (auto *em = std::get_if<ast::Enum_member_expr>(arg_expr_node)) {
                variant_name = em->member_name;
            } else {
                type_error(ast::get_loc(*arg_expr_node), "Argument must be a static-like variant access.");
            }

            if (!variant_name.empty() && !contains_key(union_t.variants, variant_name)) {
                type_error(ast::get_loc(*arg_expr_node), std::format("Union has no variant named '{}'", variant_name));
            }

            if (expr.method_name == "has") {
                return expr.type = env.tt.get_bool();
            } else { // get
                auto it = find_by_key(union_t.variants, variant_name);
                if (it != union_t.variants.end()) {
                    return expr.type = it->second;
                }
                return expr.type = env.tt.get_unknown();
            }
        } else {
            type_error(expr.loc, "Union types only support the '.has()' and '.get()' methods.");
            return expr.type = env.tt.get_unknown();
        }
    }

    // FFI METHOD MATCHER
    std::string ffi_name;
    if (env.tt.is_array(obj_type)) {
        ffi_name = "Array::" + expr.method_name;
    }
    if (env.tt.is_string(obj_type)) {
        ffi_name = "string::" + expr.method_name;
    }
    if (env.tt.is_optional(obj_type)) {
        ffi_name = "Optional::" + expr.method_name;
    }
    if (env.tt.is_iterator(obj_type)) {
        ffi_name = "Iter::" + expr.method_name;
    }

    if (env.tt.is_optional(obj_type) && expr.method_name == "get") {
        auto base_type = env.tt.get_optional_base(obj_type);

        if (expr.arguments.empty()) {
            return expr.type = base_type;
        } else if (expr.arguments.size() == 1) {
            auto arg_type = check_expr(expr.arguments[0].value);

            if (env.tt.is_string(arg_type)) {
                return expr.type = base_type;
            } else if (env.tt.is_function(arg_type)) {
                auto c_type = env.tt.get(arg_type).as<types::Function_type>();
                if (!c_type.params.empty()) {
                    type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "Closure passed to 'get' must take no arguments.");
                }
                if (!is_compatible(base_type, c_type.ret)) {
                    type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "Closure return type mismatch.");
                }
                return expr.type = base_type;
            } else {
                type_error(expr.loc, "Argument to 'get' must be a string or a closure.");
                return expr.type = env.tt.get_unknown();
            }
        } else {
            type_error(expr.loc, ".get() expects 0 or 1 argument.");
            return expr.type = env.tt.get_unknown();
        }
    }

    if (env.tt.is_optional(obj_type) && expr.method_name == "or_else") {
        if (expr.arguments.size() != 1) {
            type_error(expr.loc, "or_else() expects exactly one closure argument.");
            return expr.type = env.tt.get_unknown();
        }
        auto closure_type = check_expr(expr.arguments[0].value);
        if (!env.tt.is_function(closure_type)) {
            type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "Argument to 'or_else' must be a closure.");
            return expr.type = env.tt.get_unknown();
        }
        auto c_type = env.tt.get(closure_type).as<types::Function_type>();
        auto base_type = env.tt.get_optional_base(obj_type);
        if (!c_type.params.empty()) {
            type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "Closure passed to 'or_else' must take no arguments.");
        }
        if (!is_compatible(base_type, c_type.ret)) {
            type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "Closure return type mismatch.");
        }
        return expr.type = base_type;
    }

    if (!ffi_name.empty() && env.is_native_defined(ffi_name)) {
        const auto &signatures = *env.get_native_signatures(ffi_name);
        for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
            auto bound = try_bind_native_arguments(signatures[sig_index].params, expr.arguments, obj_type);
            if (bound.ok) {
                expr.arguments = std::move(bound.ordered_arguments);
                expr.native_signature_index = static_cast<int>(sig_index);
                return expr.type = parse_type_string(signatures[sig_index].ret_type_str, bound.generics);
            }
        }
        type_error(expr.loc, std::format("Arguments do not match any native signature for method '{}'.", expr.method_name));
        return expr.type = env.tt.get_unknown();
    }

    if (expr.method_name == "map") {
        if (expr.arguments.size() != 1) {
            type_error(expr.loc, "Map must have one closure argument.");
            return expr.type = env.tt.get_unknown();
        }
        auto closure_type = check_expr(expr.arguments[0].value);
        if (!env.tt.is_function(closure_type)) {
            type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "Argument to 'map' must be a closure.");
            return expr.type = env.tt.get_unknown();
        }
        auto c_type = env.tt.get(closure_type).as<types::Function_type>();

        if (env.tt.is_array(obj_type)) {
            auto element_type = env.tt.get_array_elem(obj_type);
            if (c_type.params.size() != 1 || !is_compatible(c_type.params[0], element_type)) {
                type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "Closure parameter mismatch.");
            }
            return expr.type = env.tt.array(c_type.ret);
        }
        if (env.tt.is_optional(obj_type)) {
            auto base_type = env.tt.get_optional_base(obj_type);
            if (c_type.params.size() != 1 || !is_compatible(c_type.params[0], base_type)) {
                type_error(ast::get_loc(tree.get(expr.arguments[0].value).node), "Closure parameter mismatch.");
            }
            return expr.type = env.tt.optional(c_type.ret);
        }
        type_error(expr.loc, ".map() can only be called on arrays and optionals.");
        return expr.type = env.tt.get_unknown();
    }

    if (env.tt.is_optional(obj_type)) {
        type_error(expr.loc, "Cannot call method on an optional type. Unwrap first.");
        return expr.type = env.tt.get_unknown();
    }

    if (env.tt.is_model(obj_type)) {
        auto &model_name = std::get<types::Model_type>(env.tt.get(obj_type).data).name;
        auto model_data_ptr = env.get_model(model_name);

        if (model_data_ptr && model_data_ptr->methods.contains(expr.method_name)) {
            const auto &method_decl_id = model_data_ptr->methods.at(expr.method_name).declaration;
            auto decl = std::get_if<ast::Function_stmt>(&tree.get(method_decl_id).node);

            // UFCS argument builder: We push the object in as argument 0!
            std::vector<ast::Call_argument> ufcs_args;
            ufcs_args.push_back({"", expr.object, expr.loc});
            for (auto &arg : expr.arguments) {
                ufcs_args.push_back(arg);
            }

            auto bound = bind_call_arguments(decl->parameters, ufcs_args, expr.loc, "method", expr.method_name);
            expr.arguments = std::move(bound.ordered_arguments);

            if (!bound.ok) {
                return expr.type = decl->return_type;
            }

            return expr.type = decl->return_type;
        } else {
            // Fallback for closure fields
            auto &model_t = std::get<types::Model_type>(env.tt.get(obj_type).data);
            for (size_t i = 0; i < model_t.fields.size(); ++i) {
                if (model_t.fields[i].first == expr.method_name) {
                    auto field_type = model_t.fields[i].second;
                    if (env.tt.is_function(field_type)) {
                        expr.is_closure_field = true;
                        expr.field_index = static_cast<uint8_t>(i);
                        auto func_type = env.tt.get(field_type).as<types::Function_type>();

                        if (expr.arguments.size() != func_type.params.size()) {
                            type_error(expr.loc, "Incorrect number of arguments for closure field.");
                        } else {
                            for (size_t j = 0; j < expr.arguments.size(); ++j) {
                                auto arg_res = check_expr(expr.arguments[j].value, func_type.params[j]);
                                if (!is_compatible(func_type.params[j], arg_res)) {
                                    type_error(ast::get_loc(tree.get(expr.arguments[j].value).node), "Argument type mismatch.");
                                }
                            }
                        }
                        return expr.type = func_type.ret;
                    }
                }
            }
        }
    }

    type_error(expr.loc, std::format("No method or closure field named '{}'.", expr.method_name));
    return expr.type = env.tt.get_unknown();
}

types::Type_id Semantic_checker::check_expr_node(ast::Unary_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto right_type = check_expr(expr.right);

    switch (expr.op) {
    case lex::TokenType::Minus:
        if (!env.tt.is_numeric_primitive(right_type)) {
            type_error(expr.loc, "Operand for '-' must be a number");
        } else if (env.tt.is_unsigned_integer_primitive(right_type)) {
            type_error(expr.loc, "Operand for unary '-' cannot be unsigned.");
        }
        return expr.type = right_type;
    case lex::TokenType::LogicalNot:
        if (!env.tt.is_bool(right_type)) {
            type_error(expr.loc, "Operand for '!' must be a boolean");
        }
        return expr.type = env.tt.get_bool();
    case lex::TokenType::BitNot:
        if (!env.tt.is_integer_primitive(right_type)) {
            type_error(expr.loc, "Operand for '~' must be an integer.");
        }
        return expr.type = right_type;
    default:
        type_error(expr.loc, "Unsupported unary operator");
        return expr.type = env.tt.get_unknown();
    }
}

types::Type_id Semantic_checker::check_expr_node(ast::Array_literal_expr &expr, std::optional<types::Type_id> context_type)
{
    if (expr.elements.empty()) {
        if (context_type && env.tt.is_array(*context_type)) {
            return expr.type = *context_type;
        }
        type_error(expr.loc, "Cannot infer the element type of an empty array.");
        return expr.type = env.tt.array(env.tt.get_unknown());
    }

    types::Type_id common_type = env.tt.get_nil();
    bool has_nil = false;

    std::optional<types::Type_id> element_context = std::nullopt;
    if (context_type && env.tt.is_array(*context_type)) {
        element_context = env.tt.get_array_elem(*context_type);
    }

    for (const auto &elem_expr : expr.elements) {
        auto elem_type = check_expr(elem_expr, element_context);

        if (env.tt.is_nil(elem_type)) {
            has_nil = true;
        } else if (env.tt.is_nil(common_type)) {
            common_type = elem_type;
        } else {
            auto unwrapped_common = env.tt.is_optional(common_type) ? env.tt.get_optional_base(common_type) : common_type;
            auto unwrapped_elem = env.tt.is_optional(elem_type) ? env.tt.get_optional_base(elem_type) : elem_type;

            if (!is_compatible(unwrapped_common, unwrapped_elem) && !is_compatible(unwrapped_elem, unwrapped_common)) {
                type_error(ast::get_loc(tree.get(elem_expr).node), "Array elements must have a consistent type.");
                return expr.type = env.tt.array(env.tt.get_unknown());
            }

            if (env.tt.is_optional(elem_type) && !env.tt.is_optional(common_type)) {
                common_type = env.tt.optional(common_type);
            }
        }
    }

    if (env.tt.is_nil(common_type)) {
        if (context_type && env.tt.is_array(*context_type)) {
            return expr.type = *context_type;
        }
        type_error(expr.loc, "Cannot infer type of an array containing only 'nil'.");
        return expr.type = env.tt.array(env.tt.get_unknown());
    }

    if (has_nil && !env.tt.is_optional(common_type)) {
        common_type = env.tt.optional(common_type);
    }

    if (context_type && env.tt.is_array(*context_type)) {
        auto ctx_element_type = env.tt.get_array_elem(*context_type);
        if (env.tt.is_optional(ctx_element_type) && !env.tt.is_optional(common_type)) {
            if (is_compatible(ctx_element_type, common_type)) {
                common_type = ctx_element_type;
            }
        }
    }

    return expr.type = env.tt.array(common_type);
}

types::Type_id Semantic_checker::check_expr_node(ast::Array_access_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto collection_type = check_expr(expr.array);

    auto index_type = check_expr(expr.index);
    if (!env.tt.is_integer_primitive(index_type)) {
        type_error(ast::get_loc(tree.get(expr.index).node), "Index must be an integer.");
    }

    if (env.tt.is_array(collection_type)) {
        return expr.type = env.tt.get_array_elem(collection_type);
    } else if (env.tt.is_string(collection_type)) {
        return expr.type = env.tt.get_string();
    } else {
        type_error(expr.loc, "Subscript operator '[]' can only be used on arrays and strings.");
        return expr.type = env.tt.get_unknown();
    }
}

types::Type_id Semantic_checker::check_expr_node(ast::Array_assignment_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto array_type = check_expr(expr.array);
    if (!env.tt.is_array(array_type)) {
        if (env.tt.is_string(array_type)) {
            type_error(expr.loc, "Strings are immutable and you can't use '[]' on them.");
            return expr.type = env.tt.get_unknown();
        }
        type_error(expr.loc, "Subscript operator '[]' can only be used on arrays.");
        return expr.type = env.tt.get_unknown();
    }

    auto index_type = check_expr(expr.index);
    if (!env.tt.is_integer_primitive(index_type)) {
        type_error(ast::get_loc(tree.get(expr.index).node), "Array index must be an integer.");
    }

    auto element_type = env.tt.get_array_elem(array_type);
    auto value_type = check_expr(expr.value, element_type);

    if (!is_compatible(element_type, value_type)) {
        type_error(ast::get_loc(tree.get(expr.value).node), "Type mismatch for array assignment.");
    }

    return expr.type = value_type;
}

types::Type_id Semantic_checker::check_expr_node(ast::Range_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto start_type = check_expr(expr.start);
    auto end_type = check_expr(expr.end);

    if (!env.tt.is_integer_primitive(start_type)) {
        type_error(expr.loc, "Range start must be an integer.");
    }
    if (!env.tt.is_integer_primitive(end_type)) {
        type_error(expr.loc, "Range end must be an integer.");
    }

    types::Type_id element_type = env.tt.get_i64();
    if (env.tt.is_integer_primitive(start_type) && env.tt.is_integer_primitive(end_type)) {
        element_type = promote_numeric_type(start_type, end_type);
        if (env.tt.is_any(element_type)) {
            type_error(expr.loc, "Mixed signed and unsigned range bounds require an explicit cast.");
            element_type = env.tt.get_i64();
        }
    }

    return expr.type = env.tt.iterator(element_type);
}

types::Type_id Semantic_checker::check_expr_node(ast::Spawn_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    if (!expr.call.is_null()) {
        std::ignore = check_expr(expr.call);
    }
    return expr.type = env.tt.get_any();
}

types::Type_id Semantic_checker::check_expr_node(ast::Await_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    if (!expr.thread.is_null()) {
        std::ignore = check_expr(expr.thread);
    }
    return expr.type = env.tt.get_any();
}

types::Type_id Semantic_checker::check_expr_node(ast::Yield_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    if (!expr.value.is_null()) {
        return expr.type = check_expr(expr.value);
    }
    return expr.type = env.tt.get_void();
}

types::Type_id Semantic_checker::check_expr_node(ast::Fstring_expr &expr, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    for (auto interp : expr.interpolations) {
        if (!interp.is_null()) {
            std::ignore = check_expr(interp);
        }
    }
    return expr.type = env.tt.get_string();
}

} // namespace phos
