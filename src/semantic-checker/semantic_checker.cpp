#include "semantic_checker.hpp"

#include <algorithm>
#include <cctype>
#include <format>
#include <sstream>

namespace phos {

namespace {
template <typename T>
T &get_node(ast::Ast_tree &tree, ast::Expr_id id)
{
    return std::get<T>(tree.get(id).node);
}

template <typename T>
T &get_stmt(ast::Ast_tree &tree, ast::Stmt_id id)
{
    return std::get<T>(tree.get(id).node);
}
} // namespace

/*
 * [find_by_key]
 * `-- linear search in pair vector
 */
template <typename Vec>
static auto find_by_key(const Vec &vec, const std::string &key)
{
    return std::find_if(vec.begin(), vec.end(), [&](const auto &pair) { return pair.first == key; });
}

/*
 * [contains_key]
 * `-- checks existence using find_by_key
 */
template <typename Vec>
static bool contains_key(const Vec &vec, const std::string &key)
{
    return find_by_key(vec, key) != vec.end();
}

/*
 * [constructor]
 * `-- initialize checker state
 */
Semantic_checker::Semantic_checker(ast::Ast_tree &tree, Type_environment &env, mem::Arena &arena) : tree(tree), env(env), arena_(arena)
{}

/*
 * [type_error]
 * `-- emit error diagnostic
 */
void Semantic_checker::type_error(const ast::Source_location &loc, const std::string &message)
{
    errors.push_back(err::msg::error(this->phase, loc.l, loc.c, loc.file, "{}", message));
}

/*
 * [type_warning]
 * `-- emit warning diagnostic
 */
void Semantic_checker::type_warning(const ast::Source_location &loc, const std::string &message)
{
    errors.push_back(err::msg::warning(this->phase, loc.l, loc.c, loc.file, "{}", message));
}

/*
 * [resolve_type_recursively]
 * |-- unwrap optionals/arrays/iters
 * |-- resolve named type
 * `-- re-wrap containers
 */
types::Type_id Semantic_checker::resolve_type_recursively(types::Type_id type_id, const ast::Source_location &loc)
{
    if (env.tt.is_unresolved(type_id)) {
        auto type_name = env.tt.get(type_id).as<types::Unresolved_type>().name;
        if (env.is_type_defined(type_name)) {
            return *env.get_type(type_name);
        } else {
            type_error(loc, "Unknown type '" + type_name + "'.");
            return env.tt.get_unknown();
        }
    }

    if (env.tt.is_optional(type_id)) {
        types::Type_id base = env.tt.get_optional_base(type_id);
        types::Type_id resolved_base = resolve_type_recursively(base, loc);
        return env.tt.optional(resolved_base);
    }

    if (env.tt.is_array(type_id)) {
        types::Type_id base = env.tt.get_array_elem(type_id);
        types::Type_id resolved_base = resolve_type_recursively(base, loc);
        return env.tt.array(resolved_base);
    }

    if (env.tt.is_iterator(type_id)) {
        types::Type_id base = env.tt.get_iter_elem(type_id);
        types::Type_id resolved_base = resolve_type_recursively(base, loc);
        return env.tt.iterator(resolved_base);
    }

    return type_id;
}

/*
 * [is_iterator_protocol_type]
 * |-- check native iter
 * `-- check custom model for next()
 */
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
                bool valid_params = decl && (decl->parameters.size() == 1 || decl->parameters.size() == 2);
                return valid_params && env.tt.is_optional(decl->return_type);
            }
        }
    }
    return false;
}

/*
 * [iterator_element_type]
 * |-- extract from native iter
 * `-- extract from custom model next() return
 */
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

/*
 * [to_iterator_type]
 * |-- map collections to iter types
 * `-- resolve iter() method on custom models
 */
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
                if (decl && decl->parameters.size() == 1 && is_iterator_protocol_type(decl->return_type)) {
                    return decl->return_type;
                }
            }
        }
    }
    return env.tt.iterator(type);
}

/*
 * [hoist_globals]
 * |-- register functions
 * |-- register models and methods
 * |-- register unions
 * `-- register enums
 */
void Semantic_checker::hoist_globals(const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {
        if (stmt_id.is_null()) {
            continue;
        }

        if (std::holds_alternative<ast::Function_stmt>(tree.get(stmt_id).node)) {
            auto &fn = get_stmt<ast::Function_stmt>(tree, stmt_id);
            env.functions[fn.name] = types::Function_type_data{stmt_id};

        } else if (std::holds_alternative<ast::Model_stmt>(tree.get(stmt_id).node)) {
            auto &model = get_stmt<ast::Model_stmt>(tree, stmt_id);
            std::vector<std::pair<std::string, types::Type_id>> tt_fields;
            for (const auto &field : model.fields) {
                tt_fields.push_back({field.name, resolve_type_recursively(field.type, field.loc)});
            }
            env.global_types[model.name] = env.tt.model(model.name, tt_fields);

            types::Model_type_data m_data;
            for (const auto &field : model.fields) {
                if (!field.default_value.is_null()) {
                    m_data.field_defaults[field.name] = field.default_value;
                }
            }
            env.model_data[model.name] = std::move(m_data);

            for (auto method_id : model.methods) {
                if (method_id.is_null()) {
                    continue;
                }

                if (std::holds_alternative<ast::Function_stmt>(tree.get(method_id).node)) {
                    auto &m_fn = get_stmt<ast::Function_stmt>(tree, method_id);
                    if (!m_fn.is_static) {
                        ast::Function_param this_param;
                        this_param.name = "this";
                        this_param.type = env.global_types[model.name];
                        this_param.is_mut = true;
                        this_param.loc = m_fn.loc;
                        m_fn.parameters.insert(m_fn.parameters.begin(), this_param);
                    }

                    auto original_name = m_fn.name;
                    m_fn.name = model.name + "::" + m_fn.name;

                    if (m_fn.is_static) {
                        env.model_data[model.name].static_methods[original_name] = types::Function_type_data{method_id};
                    } else {
                        env.model_data[model.name].methods[original_name] = types::Function_type_data{method_id};
                    }

                    env.functions[m_fn.name] = types::Function_type_data{method_id};
                }
            }
        } else if (std::holds_alternative<ast::Union_stmt>(tree.get(stmt_id).node)) {
            auto &un = get_stmt<ast::Union_stmt>(tree, stmt_id);
            std::vector<std::pair<std::string, types::Type_id>> tt_variants;
            for (const auto &variant : un.variants) {
                tt_variants.push_back({variant.name, variant.type});
            }
            env.global_types[un.name] = env.tt.union_(un.name, tt_variants);

            types::Union_type_data u_data;
            for (const auto &variant : un.variants) {
                if (!variant.default_value.is_null()) {
                    u_data.variant_defaults[variant.name] = variant.default_value;
                }
            }
            env.union_data[un.name] = std::move(u_data);

        } else if (std::holds_alternative<ast::Enum_stmt>(tree.get(stmt_id).node)) {
            auto &en = get_stmt<ast::Enum_stmt>(tree, stmt_id);
            env.global_types[en.name] = env.tt.enum_(en.name, en.base_type);

            types::Enum_type_data e_data;

            bool is_string_enum = env.tt.is_string(en.base_type);
            int64_t current_val = 0;

            for (const auto &variant : en.variants) {
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

            env.enum_data[en.name] = std::move(e_data);
        }
    }
}

/*
 * [check]
 * |-- hoist globals
 * `-- verify all statements
 */
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

/*
 * [declare]
 * `-- register local scoped var
 */
void Semantic_checker::declare(const std::string &name, types::Type_id type, bool is_mut, const ast::Source_location &loc)
{
    if (!variables.declare(name, type, is_mut)) {
        type_error(loc, std::format("Variable '{}' is already declared in this scope.", name));
    }
}

/*
 * [lookup]
 * |-- locals
 * |-- natives
 * `-- globals
 */
std::optional<Scope_symbol> Semantic_checker::lookup(const std::string &name, const ast::Source_location &loc)
{
    if (auto var_opt = variables.lookup(name)) {
        return *var_opt;
    }

    if (env.is_native_defined(name)) {
        types::Type_id dummy = env.tt.function({}, env.tt.get_any());
        return Scope_symbol{dummy, false, 0};
    }

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

/*
 * [is_compatible]
 * |-- strict equality
 * |-- optional depth bounds
 * `-- structural matches
 */
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

/*
 * [promote_numeric_type]
 * `-- find safest numeric upcast
 */
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

/*
 * [default_expr_uses_forbidden_names]
 * `-- safety scan
 */
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

/*
 * [validate_function_defaults]
 * `-- safe evaluation
 */
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
            type_error(
                param.loc,
                std::format("Default argument for parameter '{}' cannot reference 'this' or another parameter.", param.name));
        } else {
            auto default_type = check_expr(param.default_value, param.type);
            if (!is_compatible(param.type, default_type)) {
                type_error(
                    param.loc,
                    std::format(
                        "Default argument mismatch in function.\n   Expected: '{}'\n   Got: '{}'",
                        env.tt.to_string(param.type),
                        env.tt.to_string(default_type)));
            }
        }
    }
}

/*
 * [validate_model_defaults]
 * `-- safe evaluation
 */
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
                    std::format(
                        "Default argument mismatch in model.\n   Expected: '{}'\n   Got: '{}'",
                        env.tt.to_string(field.type),
                        env.tt.to_string(default_type)));
            }
        }
    }
}

/*
 * [validate_union_defaults]
 * `-- safe evaluation
 */
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
                std::format(
                    "Default argument mismatch in model.\n   Expected: '{}'\n   Got: '{}'",
                    env.tt.to_string(variant.type),
                    env.tt.to_string(default_type)));
        }
    }
}

/*
 * [extract_access_path]
 * `-- build path sequence
 */
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

        if (auto *index_var = std::get_if<ast::Variable_expr>(&idx_node)) {
            base_path->projections.push_back(Projection{.kind = Projection_kind::Var_Index, .name_val = index_var->name});
            return base_path;
        }
        if (auto *l = std::get_if<ast::Literal_expr>(&idx_node)) {
            if (l->value.is_integer()) {
                base_path->projections.push_back(
                    Projection{.kind = Projection_kind::Int_Index, .name_val = "", .int_val = l->value.as_int()});
                return base_path;
            }
            if (l->value.is_u_integer()) {
                base_path->projections.push_back(
                    Projection{.kind = Projection_kind::Uint_Index, .name_val = "", .uint_val = l->value.as_uint()});
                return base_path;
            }
        }
        return std::nullopt;
    }

    return std::nullopt;
}

/*
 * [collect_nil_check_from_optional_method]
 * `-- optional method trace
 */
void Semantic_checker::collect_nil_check_from_optional_method(
    const ast::Method_call_expr &expr, bool target_truthy_branch, std::unordered_set<Access_path, Access_path_hash> &out)
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

/*
 * [collect_nil_check_from_comparison]
 * `-- binary nil trace
 */
void Semantic_checker::collect_nil_check_from_comparison(
    const ast::Binary_expr &expr, lex::TokenType target_op, std::unordered_set<Access_path, Access_path_hash> &out)
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

/*
 * [collect_nil_checked_vars_for_then]
 * `-- then branch tracker
 */
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

/*
 * [collect_nil_checked_vars_for_else]
 * `-- else branch tracker
 */
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

/*
 * [parse_type_string]
 * `-- build type from ffi signature
 */
types::Type_id Semantic_checker::parse_type_string(std::string str, const std::unordered_map<std::string, types::Type_id> &generics) const
{
    str.erase(str.find_last_not_of(" \t\r\n") + 1);
    str.erase(0, str.find_first_not_of(" \t\r\n"));

    if (str.length() == 1 && std::isupper(str[0]) && generics.contains(str)) {
        return generics.at(str);
    }

    if (str == "i8") {
        return env.tt.get_i8();
    }
    if (str == "i16") {
        return env.tt.get_i16();
    }
    if (str == "i32") {
        return env.tt.get_i32();
    }
    if (str == "i64") {
        return env.tt.get_i64();
    }
    if (str == "u8") {
        return env.tt.get_u8();
    }
    if (str == "u16") {
        return env.tt.get_u16();
    }
    if (str == "u32") {
        return env.tt.get_u32();
    }
    if (str == "u64") {
        return env.tt.get_u64();
    }
    if (str == "f16") {
        return env.tt.get_f16();
    }
    if (str == "f32") {
        return env.tt.get_f32();
    }
    if (str == "f64") {
        return env.tt.get_f64();
    }
    if (str == "bool") {
        return env.tt.get_bool();
    }
    if (str == "string") {
        return env.tt.get_string();
    }
    if (str == "void") {
        return env.tt.get_void();
    }
    if (str == "any") {
        return env.tt.get_any();
    }
    if (str == "nil") {
        return env.tt.get_nil();
    }

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

/*
 * [match_ffi_type]
 * `-- structurally matches target strings
 */
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

/*
 * [bind_call_arguments]
 * `-- match abi positional and named
 */
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
                    env.tt.to_string(arg_type)));
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

/*
 * [try_bind_native_arguments]
 * `-- ffi matching
 */
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

/*
 * [check_stmt]
 * `-- safe index routing dispatcher
 */
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
            } else if constexpr (std::is_same_v<T, ast::While_stmt>) {
                check_while_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::For_stmt>) {
                check_for_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::For_in_stmt>) {
                check_for_in_stmt(stmt_id);
            } else if constexpr (std::is_same_v<T, ast::Match_stmt>) {
                check_match_stmt(stmt_id);
            }
        },
        tree.get(stmt_id).node);
}

/*
 * [check_expr]
 * |-- peel hints
 * |-- route via ID
 * |-- auto lift
 * `-- save depths
 */
types::Type_id Semantic_checker::check_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    if (expr_id.is_null()) {
        return env.tt.get_void();
    }

    std::optional<types::Type_id> peeled_hint = context_type;
    uint8_t expected_depth = 0;

    if (peeled_hint) {
        while (env.tt.is_optional(*peeled_hint)) {
            peeled_hint = env.tt.get_optional_base(*peeled_hint);
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
            } else if constexpr (std::is_same_v<T, ast::Fstring_expr>) {
                return check_fstring_expr(expr_id, peeled_hint);
            }
            return env.tt.get_unknown();
        },
        tree.get(expr_id).node);

    if (env.tt.is_optional(actual_type)) {
        if (auto path = extract_access_path(expr_id)) {
            for (auto it = m_nil_checked_vars_stack.rbegin(); it != m_nil_checked_vars_stack.rend(); ++it) {
                if (it->count(*path)) {
                    actual_type = env.tt.get_optional_base(actual_type);
                    break;
                }
            }
        }
    }

    if (context_type) {
        uint8_t actual_depth = 0;
        types::Type_id actual_base = actual_type;
        while (env.tt.is_optional(actual_base)) {
            actual_base = env.tt.get_optional_base(actual_base);
            actual_depth++;
        }

        if (expected_depth > actual_depth) {
            types::Type_id expected_base = *peeled_hint;

            if (env.tt.is_nil(actual_base) || env.tt.is_unknown(actual_base)) {
                ast::get_type(tree.get(expr_id).node) = *context_type;
                return *context_type;
            }

            if (is_compatible(expected_base, actual_base)) {
                tree.get(expr_id).auto_wrap_depth += (expected_depth - actual_depth);
                ast::get_type(tree.get(expr_id).node) = *context_type;
                return *context_type;
            }
        }
    }

    ast::get_type(tree.get(expr_id).node) = actual_type;
    return actual_type;
}

// STATEMENT VISITORS

/*
 * [function_stmt]
 * |-- check defaults
 * |-- scope params
 * `-- check body
 */
void Semantic_checker::check_function_stmt(ast::Stmt_id stmt_id)
{
    // Grab by REFERENCE so we can mutate the AST node permanently!
    auto &fn_stmt = get_stmt<ast::Function_stmt>(tree, stmt_id);

    // 1. PERMANENTLY resolve return type and parameter types in the AST
    fn_stmt.return_type = resolve_type_recursively(fn_stmt.return_type, fn_stmt.loc);
    for (auto &param : fn_stmt.parameters) {
        param.type = resolve_type_recursively(param.type, param.loc);
    }

    auto saved_return = current_return_type;
    current_return_type = fn_stmt.return_type;

    validate_function_defaults(fn_stmt);

    variables.begin_scope();

    for (const auto &p : fn_stmt.parameters) {
        declare(p.name, p.type, p.is_mut, p.loc);
    }

    if (!fn_stmt.body.is_null()) {
        check_stmt(fn_stmt.body);
    }

    variables.end_scope();
    current_return_type = saved_return;
}

/*
 * [var_stmt]
 * |-- evaluate initializer
 * |-- infer type
 * `-- add to scope
 */
void Semantic_checker::check_var_stmt(ast::Stmt_id stmt_id)
{
    auto type_inferred = get_stmt<ast::Var_stmt>(tree, stmt_id).type_inferred;
    auto type = get_stmt<ast::Var_stmt>(tree, stmt_id).type;
    auto loc = get_stmt<ast::Var_stmt>(tree, stmt_id).loc;
    auto initializer = get_stmt<ast::Var_stmt>(tree, stmt_id).initializer;
    auto name = get_stmt<ast::Var_stmt>(tree, stmt_id).name;
    auto is_mut = get_stmt<ast::Var_stmt>(tree, stmt_id).is_mut;

    if (!type_inferred) {
        type = resolve_type_recursively(type, loc);
        get_stmt<ast::Var_stmt>(tree, stmt_id).type = type;
    }

    types::Type_id init_type = env.tt.get_void();
    bool initializer_failed = false;

    if (!initializer.is_null()) {
        init_type = check_expr(initializer, type_inferred ? std::nullopt : std::make_optional(type));
        if (env.tt.is_unknown(init_type)) {
            initializer_failed = true;
        }
    }

    if (type_inferred) {
        if (initializer_failed) {
            get_stmt<ast::Var_stmt>(tree, stmt_id).type = env.tt.get_unknown();
            return;
        }
        if (env.tt.is_array(init_type) && env.tt.get_array_elem(init_type) == env.tt.get_void()) {
            type_error(loc, "Cannot infer type of an empty array initializer.");
        }
        type = init_type;
        get_stmt<ast::Var_stmt>(tree, stmt_id).type = type;
    } else if (!initializer.is_null() && !initializer_failed && !is_compatible(type, init_type)) {
        if (std::holds_alternative<ast::Literal_expr>(tree.get(initializer).node)) {
            auto &lit = get_node<ast::Literal_expr>(tree, initializer);
            if (env.tt.is_numeric_primitive(type) && env.tt.is_numeric_primitive(init_type)) {
                auto target_kind = env.tt.get_primitive(type);

                if ((lit.value.is_integer() && types::is_float_primitive(target_kind))
                    || (lit.value.is_float() && !types::is_float_primitive(target_kind))) {
                    type_error(
                        loc,
                        std::format(
                            "Numeric literal '{}' cannot be implicitly converted to '{}'; use an explicit cast.",
                            lit.value.to_string(),
                            env.tt.to_string(type)));
                } else {
                    type_error(
                        loc,
                        std::format(
                            "Numeric literal '{}' does not fit in target type '{}'.",
                            lit.value.to_string(),
                            env.tt.to_string(type)));
                }
            } else {
                auto diagnostic = err::msg::error(this->phase, loc.l, loc.c, loc.file, "Initializer type mismatch.");
                diagnostic.expected_got(env.tt.to_string(type), env.tt.to_string(init_type));
                errors.push_back(std::move(diagnostic));
            }
        } else {
            auto diagnostic = err::msg::error(this->phase, loc.l, loc.c, loc.file, "Initializer type mismatch.");
            diagnostic.expected_got(env.tt.to_string(type), env.tt.to_string(init_type));
            errors.push_back(std::move(diagnostic));
        }
    }
    declare(name, type, is_mut, loc);
}

/*
 * [model_stmt]
 * |-- validate defaults
 * `-- check methods
 */
void Semantic_checker::check_model_stmt(ast::Stmt_id stmt_id)
{
    validate_model_defaults(get_stmt<ast::Model_stmt>(tree, stmt_id));
    auto name = get_stmt<ast::Model_stmt>(tree, stmt_id).name;
    auto methods = get_stmt<ast::Model_stmt>(tree, stmt_id).methods;

    auto saved_model = current_model_type;
    if (env.is_type_defined(name)) {
        current_model_type = env.get_type(name);
    }
    for (auto &method : methods) {
        check_stmt(method);
    }
    current_model_type = saved_model;
}

/*
 * [union_stmt]
 * `-- validate variants
 */
void Semantic_checker::check_union_stmt(ast::Stmt_id stmt_id)
{
    validate_union_defaults(get_stmt<ast::Union_stmt>(tree, stmt_id));
}

/*
 * [enum_stmt]
 * `-- validate uniqueness
 */
void Semantic_checker::check_enum_stmt(ast::Stmt_id stmt_id)
{
    auto name = get_stmt<ast::Enum_stmt>(tree, stmt_id).name;
    auto loc = get_stmt<ast::Enum_stmt>(tree, stmt_id).loc;
    auto variants = get_stmt<ast::Enum_stmt>(tree, stmt_id).variants;

    auto enum_data_ptr = env.get_enum(name);
    if (!enum_data_ptr) {
        return;
    }

    std::unordered_set<std::string> used_names;
    std::unordered_set<int64_t> used_ints;
    std::unordered_set<std::string> used_strings;

    for (const auto &variant : variants) {
        if (!used_names.insert(variant.first).second) {
            type_error(loc, std::format("Duplicate enum variant name '{}'.", variant.first));
            continue;
        }

        auto val = enum_data_ptr->variants.at(variant.first);
        if (val.is_integer()) {
            int64_t i = val.as_int();
            if (!used_ints.insert(i).second) {
                type_error(loc, std::format("Duplicate enum value '{}' in variant '{}'. Enum values must be unique.", i, variant.first));
            }
        } else if (val.is_string()) {
            std::string s(val.as_string());
            if (!used_strings.insert(s).second) {
                type_error(
                    loc,
                    std::format("Duplicate enum value '\"{}\"' in variant '{}'. Enum values must be unique.", s, variant.first));
            }
        }
    }
}

/*
 * [block_stmt]
 * |-- open scope
 * |-- evaluate children
 * `-- close scope
 */
void Semantic_checker::check_block_stmt(ast::Stmt_id stmt_id)
{
    auto statements = get_stmt<ast::Block_stmt>(tree, stmt_id).statements;
    variables.begin_scope();
    for (auto s : statements) {
        check_stmt(s);
    }
    variables.end_scope();
}

/*
 * [expr_stmt]
 * `-- pass through to expr
 */
void Semantic_checker::check_expr_stmt(ast::Stmt_id stmt_id)
{
    auto expression = get_stmt<ast::Expr_stmt>(tree, stmt_id).expression;
    if (!expression.is_null()) {
        check_expr(expression);
    }
}

/*
 * [if_stmt]
 * |-- check condition
 * |-- pass nil hints
 * `-- evaluate branches
 */
void Semantic_checker::check_if_stmt(ast::Stmt_id stmt_id)
{
    auto condition = get_stmt<ast::If_stmt>(tree, stmt_id).condition;
    auto then_branch = get_stmt<ast::If_stmt>(tree, stmt_id).then_branch;
    auto else_branch = get_stmt<ast::If_stmt>(tree, stmt_id).else_branch;

    auto condition_type = check_expr(condition);

    if (!env.tt.is_bool(condition_type) && !env.tt.is_optional(condition_type) && !env.tt.is_unknown(condition_type)) {
        type_error(ast::get_loc(tree.get(condition).node), "If condition must be a boolean or optional.");
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

/*
 * [print_stmt]
 * `-- force evaluate
 */
void Semantic_checker::check_print_stmt(ast::Stmt_id stmt_id)
{
    auto expressions = get_stmt<ast::Print_stmt>(tree, stmt_id).expressions;
    for (auto ex : expressions) {
        check_expr(ex);
    }
}

/*
 * [return_stmt]
 * `-- validate against function bound
 */
void Semantic_checker::check_return_stmt(ast::Stmt_id stmt_id)
{
    auto expression = get_stmt<ast::Return_stmt>(tree, stmt_id).expression;
    auto loc = get_stmt<ast::Return_stmt>(tree, stmt_id).loc;

    if (!current_return_type) {
        type_error(loc, "Return statement used outside of a function");
        return;
    }
    if (!expression.is_null()) {
        auto val_type = check_expr(expression, current_return_type);
        if (!is_compatible(*current_return_type, val_type)) {
            auto diagnostic = err::msg::error(this->phase, loc.l, loc.c, loc.file, "Return type mismatch.");
            diagnostic.expected_got(env.tt.to_string(*current_return_type), env.tt.to_string(val_type));
            errors.push_back(std::move(diagnostic));
        }
    } else if (*current_return_type != env.tt.get_void()) {
        type_error(loc, "Function must return a value.");
    }
}

/*
 * [while_stmt]
 * |-- assert condition
 * `-- check loop
 */
void Semantic_checker::check_while_stmt(ast::Stmt_id stmt_id)
{
    auto condition = get_stmt<ast::While_stmt>(tree, stmt_id).condition;
    auto body = get_stmt<ast::While_stmt>(tree, stmt_id).body;

    if (!condition.is_null()) {
        auto type = check_expr(condition);
        if (!env.tt.is_bool(type) && !env.tt.is_unknown(type)) {
            type_error(ast::get_loc(tree.get(condition).node), "Condition must be a boolean");
        }
    }
    if (!body.is_null()) {
        check_stmt(body);
    }
}

/*
 * [for_stmt]
 * |-- define init
 * |-- assert bool
 * |-- check increment
 * `-- body
 */
void Semantic_checker::check_for_stmt(ast::Stmt_id stmt_id)
{
    auto initializer = get_stmt<ast::For_stmt>(tree, stmt_id).initializer;
    auto condition = get_stmt<ast::For_stmt>(tree, stmt_id).condition;
    auto increment = get_stmt<ast::For_stmt>(tree, stmt_id).increment;
    auto body = get_stmt<ast::For_stmt>(tree, stmt_id).body;

    variables.begin_scope();
    if (!initializer.is_null()) {
        check_stmt(initializer);
    }
    if (!condition.is_null()) {
        auto type = check_expr(condition);
        if (!env.tt.is_bool(type) && !env.tt.is_unknown(type)) {
            type_error(ast::get_loc(tree.get(condition).node), "Condition must be boolean");
        }
    }
    if (!increment.is_null()) {
        check_expr(increment);
    }
    if (!body.is_null()) {
        check_stmt(body);
    }
    variables.end_scope();
}

/*
 * [for_in_stmt]
 * |-- bind protocol
 * |-- inject scope var
 * `-- body
 */
void Semantic_checker::check_for_in_stmt(ast::Stmt_id stmt_id)
{
    auto iterable = get_stmt<ast::For_in_stmt>(tree, stmt_id).iterable;
    auto loc = get_stmt<ast::For_in_stmt>(tree, stmt_id).loc;
    auto var_name = get_stmt<ast::For_in_stmt>(tree, stmt_id).var_name;
    auto body = get_stmt<ast::For_in_stmt>(tree, stmt_id).body;

    auto iterable_type = check_expr(iterable);
    if (env.tt.is_unknown(iterable_type) || env.tt.is_any(iterable_type)) {
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
    variables.end_scope();
}

/*
 * [match_stmt]
 * |-- assert exhaustive
 * |-- check unions
 * `-- check enums
 */
void Semantic_checker::check_match_stmt(ast::Stmt_id stmt_id)
{
    auto subject = get_stmt<ast::Match_stmt>(tree, stmt_id).subject;
    auto loc = get_stmt<ast::Match_stmt>(tree, stmt_id).loc;
    auto arms = get_stmt<ast::Match_stmt>(tree, stmt_id).arms;

    types::Type_id subject_type = check_expr(subject);
    if (env.tt.is_any(subject_type) || env.tt.is_unknown(subject_type)) {
        return;
    }

    bool is_union_subject = env.tt.is_union(subject_type);

    for (auto &arm : arms) {
        variables.begin_scope();
        if (!arm.is_wildcard) {
            if (is_union_subject
                && (std::holds_alternative<ast::Static_path_expr>(tree.get(arm.pattern).node)
                    || std::holds_alternative<ast::Enum_member_expr>(tree.get(arm.pattern).node))) {

                auto &union_t = std::get<types::Union_type>(env.tt.get(subject_type).data);
                std::string variant_name;
                ast::Source_location pattern_loc = loc;

                if (std::holds_alternative<ast::Static_path_expr>(tree.get(arm.pattern).node)) {
                    variant_name = get_node<ast::Static_path_expr>(tree, arm.pattern).member.lexeme;
                    pattern_loc = get_node<ast::Static_path_expr>(tree, arm.pattern).loc;
                    get_node<ast::Static_path_expr>(tree, arm.pattern).type = subject_type;
                } else if (std::holds_alternative<ast::Enum_member_expr>(tree.get(arm.pattern).node)) {
                    variant_name = get_node<ast::Enum_member_expr>(tree, arm.pattern).member_name;
                    pattern_loc = get_node<ast::Enum_member_expr>(tree, arm.pattern).loc;
                    get_node<ast::Enum_member_expr>(tree, arm.pattern).type = subject_type;
                }

                auto variant_it =
                    std::find_if(union_t.variants.begin(), union_t.variants.end(), [&](const auto &v) { return v.first == variant_name; });
                if (variant_it == union_t.variants.end()) {
                    type_error(pattern_loc, "Variant does not exist in union.");
                } else if (!arm.bind_name.empty()) {
                    if (variant_it->second == env.tt.get_void()) {
                        type_error(pattern_loc, "Variant does not hold a payload.");
                    } else {
                        declare(arm.bind_name, variant_it->second, false, loc);
                    }
                }
            } else {
                types::Type_id pattern_type = check_expr(arm.pattern, subject_type);
                if (env.tt.is_any(pattern_type) || env.tt.is_unknown(pattern_type)) {
                    variables.end_scope();
                    continue;
                }

                bool is_range_match = std::holds_alternative<ast::Range_expr>(tree.get(arm.pattern).node)
                    && env.tt.is_integer_primitive(subject_type);

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
                                    type_error(ast::get_loc(tree.get(arm.pattern).node), "__match__ method must return bool.");
                                }
                            } else {
                                type_error(
                                    ast::get_loc(tree.get(arm.pattern).node),
                                    "__match__ method must accept one argument of subject type.");
                            }
                        }
                    }
                }

                if (!is_compatible(subject_type, pattern_type) && !is_range_match && !has_custom_match) {
                    type_error(ast::get_loc(tree.get(arm.pattern).node), "Match pattern type mismatch.");
                }
            }
        }
        if (!arm.body.is_null()) {
            check_stmt(arm.body);
        }
        variables.end_scope();
    }

    bool has_wildcard = std::any_of(arms.begin(), arms.end(), [](const auto &arm) { return arm.is_wildcard; });
    if (has_wildcard) {
        return;
    }

    if (env.tt.is_enum(subject_type)) {
        auto &enum_t = std::get<types::Enum_type>(env.tt.get(subject_type).data);
        std::unordered_set<std::string> covered;
        for (const auto &arm : arms) {
            if (std::holds_alternative<ast::Static_path_expr>(tree.get(arm.pattern).node)) {
                covered.insert(get_node<ast::Static_path_expr>(tree, arm.pattern).member.lexeme);
            } else if (std::holds_alternative<ast::Enum_member_expr>(tree.get(arm.pattern).node)) {
                covered.insert(get_node<ast::Enum_member_expr>(tree, arm.pattern).member_name);
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
            type_error(loc, "Non-exhaustive match on enum.");
        }
        return;
    }

    if (is_union_subject) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(subject_type).data);
        std::unordered_set<std::string> covered;
        for (const auto &arm : arms) {
            if (std::holds_alternative<ast::Static_path_expr>(tree.get(arm.pattern).node)) {
                covered.insert(get_node<ast::Static_path_expr>(tree, arm.pattern).member.lexeme);
            } else if (std::holds_alternative<ast::Enum_member_expr>(tree.get(arm.pattern).node)) {
                covered.insert(get_node<ast::Enum_member_expr>(tree, arm.pattern).member_name);
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

// EXPRESSION VISITORS

/*
 * [model_literal_expr] -> Type
 * |-- assert variants if union
 * `-- populate defaults if model
 */
types::Type_id Semantic_checker::check_model_literal_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto model_name = get_node<ast::Model_literal_expr>(tree, expr_id).model_name;
    auto loc = get_node<ast::Model_literal_expr>(tree, expr_id).loc;
    auto fields = get_node<ast::Model_literal_expr>(tree, expr_id).fields;

    if (!env.is_type_defined(model_name)) {
        type_error(loc, "Unknown type '" + model_name + "'.");
        return get_node<ast::Model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    types::Type_id resolved_type = *env.get_type(model_name);

    if (env.tt.is_union(resolved_type)) {
        auto union_data_ptr = env.get_union(model_name);
        auto &union_t = std::get<types::Union_type>(env.tt.get(resolved_type).data);

        if (fields.size() != 1) {
            type_error(loc, "Union literals must be initialized with exactly one variant field.");
            return get_node<ast::Model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
        }

        std::string variant_name = fields[0].first;
        ast::Expr_id payload_expr = fields[0].second;

        if (variant_name.empty()) {
            if (!payload_expr.is_null()) {
                if (std::holds_alternative<ast::Variable_expr>(tree.get(payload_expr).node)) {
                    variant_name = get_node<ast::Variable_expr>(tree, payload_expr).name;
                    payload_expr = ast::Expr_id::null();
                    fields[0].first = variant_name;
                    fields[0].second = ast::Expr_id::null();
                }
            }
        }

        auto it = std::find_if(union_t.variants.begin(), union_t.variants.end(), [&](const auto &v) { return v.first == variant_name; });
        if (it == union_t.variants.end()) {
            type_error(loc, std::format("Union has no variant named '{}'.", variant_name));
            return get_node<ast::Model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
        }

        auto expected_payload_type = it->second;

        if (payload_expr.is_null()) {
            if (expected_payload_type == env.tt.get_void()) {
                get_node<ast::Model_literal_expr>(tree, expr_id).fields = std::move(fields);
                return get_node<ast::Model_literal_expr>(tree, expr_id).type = resolved_type;
            }

            if (union_data_ptr && union_data_ptr->variant_defaults.contains(variant_name)) {
                payload_expr = fields[0].second = union_data_ptr->variant_defaults.at(variant_name);
            } else {
                type_error(loc, "Variant requires a payload or default.");
                return get_node<ast::Model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
        } else if (expected_payload_type == env.tt.get_void()) {
            type_error(ast::get_loc(tree.get(payload_expr).node), "Variant does not take a payload.");
            return get_node<ast::Model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
        }

        auto actual_payload_type = check_expr(payload_expr, expected_payload_type);
        if (!is_compatible(expected_payload_type, actual_payload_type)) {
            type_error(ast::get_loc(tree.get(payload_expr).node), "Type mismatch for payload.");
        }

        get_node<ast::Model_literal_expr>(tree, expr_id).fields = std::move(fields);
        return get_node<ast::Model_literal_expr>(tree, expr_id).type = resolved_type;
    }

    if (env.tt.is_model(resolved_type)) {
        auto &model_type = std::get<types::Model_type>(env.tt.get(resolved_type).data);
        auto model_data_ptr = env.get_model(model_type.name);

        if (fields.size() > model_type.fields.size()) {
            type_error(loc, "Too many fields provided for model.");
            return get_node<ast::Model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
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
            auto it =
                std::find_if(model_type.fields.begin(), model_type.fields.end(), [&](const auto &f) { return f.first == field_name; });
            if (it == model_type.fields.end()) {
                type_error(loc, std::format("Unknown field '{}'.", field_name));
                continue;
            }
            auto expected_type = it->second;
            auto actual_res = check_expr(provided_field.second, expected_type);
            if (!is_compatible(expected_type, actual_res)) {
                type_error(ast::get_loc(tree.get(provided_field.second).node), "Field type mismatch.");
            }
        }

        get_node<ast::Model_literal_expr>(tree, expr_id).fields = std::move(fields);
        return get_node<ast::Model_literal_expr>(tree, expr_id).type = resolved_type;
    }

    type_error(loc, std::format("'{}' is not a model or a union.", model_name));
    return get_node<ast::Model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
}

/*
 * [anon_model_literal_expr] -> Type
 * |-- assert peeled target bounds
 * `-- structurally expand literal
 */
types::Type_id Semantic_checker::check_anon_model_literal_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    auto loc = get_node<ast::Anon_model_literal_expr>(tree, expr_id).loc;
    auto fields = get_node<ast::Anon_model_literal_expr>(tree, expr_id).fields;

    if (!context_type) {
        type_error(loc, "Cannot infer type of anonymous literal. Context missing.");
        return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    types::Type_id expected_type = *context_type;

    if (env.tt.is_union(expected_type)) {
        auto &expected_union = std::get<types::Union_type>(env.tt.get(expected_type).data);
        auto union_data_ptr = env.get_union(expected_union.name);

        if (fields.size() != 1) {
            type_error(loc, "Anonymous union literals must have exactly one variant field.");
            return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
        }

        std::string variant_name = fields[0].first;
        ast::Expr_id payload_expr = fields[0].second;

        if (variant_name.empty()) {
            if (!payload_expr.is_null()) {
                if (std::holds_alternative<ast::Variable_expr>(tree.get(payload_expr).node)) {
                    variant_name = get_node<ast::Variable_expr>(tree, payload_expr).name;
                    payload_expr = ast::Expr_id::null();
                    fields[0].first = variant_name;
                    fields[0].second = ast::Expr_id::null();
                }
            }
        }

        auto it = std::find_if(expected_union.variants.begin(), expected_union.variants.end(), [&](const auto &v) {
            return v.first == variant_name;
        });
        if (it == expected_union.variants.end()) {
            type_error(loc, std::format("Union has no variant named '{}'.", variant_name));
            return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
        }

        auto expected_payload_type = it->second;
        if (payload_expr.is_null()) {
            if (expected_payload_type == env.tt.get_void()) {
                get_node<ast::Anon_model_literal_expr>(tree, expr_id).fields = std::move(fields);
                return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = expected_type;
            }

            if (union_data_ptr && union_data_ptr->variant_defaults.contains(variant_name)) {
                payload_expr = fields[0].second = union_data_ptr->variant_defaults.at(variant_name);
            } else {
                type_error(loc, "Variant requires a value.");
                return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
        } else if (expected_payload_type == env.tt.get_void()) {
            type_error(ast::get_loc(tree.get(payload_expr).node), "Variant does not take a payload.");
            return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
        }

        auto actual_res = check_expr(payload_expr, expected_payload_type);
        if (!is_compatible(expected_payload_type, actual_res)) {
            type_error(ast::get_loc(tree.get(payload_expr).node), "Type mismatch for payload.");
        }

        get_node<ast::Anon_model_literal_expr>(tree, expr_id).fields = std::move(fields);
        return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = expected_type;
    }

    if (env.tt.is_model(expected_type)) {
        auto &expected_model = std::get<types::Model_type>(env.tt.get(expected_type).data);
        auto model_data_ptr = env.get_model(expected_model.name);

        if (fields.size() > expected_model.fields.size()) {
            type_error(loc, "Too many fields for anonymous model.");
            return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
        }

        bool has_named = std::any_of(fields.begin(), fields.end(), [](const auto &f) { return !f.first.empty(); });
        if (has_named) {
            bool has_pos = std::any_of(fields.begin(), fields.end(), [](const auto &f) { return f.first.empty(); });
            if (has_pos) {
                type_error(loc, "Cannot mix positional and named fields.");
                return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
        } else {
            for (size_t i = 0; i < fields.size(); ++i) {
                if (fields[i].first.empty()) {
                    auto expr_id = fields[i].second;
                    auto &node = tree.get(expr_id).node;

                    if (auto *v = std::get_if<ast::Variable_expr>(&node)) {
                        fields[i].first = v->name;
                    } else if (auto *f = std::get_if<ast::Field_access_expr>(&node)) {
                        fields[i].first = f->field_name; // Extracts "c" from "this.c"
                    } else {
                        fields[i].first = expected_model.fields[i].first; // Positional fallback
                    }
                }
            }
        }

        for (size_t i = 0; i < fields.size(); ++i) {
            auto it = std::find_if(expected_model.fields.begin(), expected_model.fields.end(), [&](const auto &f) {
                return f.first == fields[i].first;
            });
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

        get_node<ast::Anon_model_literal_expr>(tree, expr_id).fields = std::move(ordered);
        return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = expected_type;
    }

    type_error(loc, "Context for anonymous literal is neither a model nor a union.");
    return get_node<ast::Anon_model_literal_expr>(tree, expr_id).type = env.tt.get_unknown();
}

/*
 * [assignment_expr] -> Type
 * |-- target must be bound
 * |-- assert mut
 * `-- assert assignable
 */
types::Type_id Semantic_checker::check_assignment_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto name = get_node<ast::Assignment_expr>(tree, expr_id).name;
    auto loc = get_node<ast::Assignment_expr>(tree, expr_id).loc;
    auto value_id = get_node<ast::Assignment_expr>(tree, expr_id).value;

    auto var_info_res = lookup(name, loc);
    if (!var_info_res) {
        return get_node<ast::Assignment_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    if (!var_info_res->is_mut) {
        type_error(loc, "Cannot assign to an immutable/constant variable.");
    }

    auto var_type = var_info_res->type;
    auto val_type_res = check_expr(value_id, var_type);

    if (!is_compatible(var_type, val_type_res)) {
        auto diagnostic = err::msg::error(this->phase, loc.l, loc.c, loc.file, "Assignment type mismatch.");
        diagnostic.expected_got(env.tt.to_string(var_type), env.tt.to_string(val_type_res));
        errors.push_back(std::move(diagnostic));
    }
    return get_node<ast::Assignment_expr>(tree, expr_id).type = var_type;
}

/*
 * [variable_expr] -> Type
 * |-- replace self ref
 * `-- lookup bindings
 */
types::Type_id Semantic_checker::check_variable_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto name = get_node<ast::Variable_expr>(tree, expr_id).name;
    auto loc = get_node<ast::Variable_expr>(tree, expr_id).loc;

    if (name == "this") {
        if (!current_model_type) {
            type_error(loc, "Cannot use 'this' outside of a model method");
            return get_node<ast::Variable_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        return get_node<ast::Variable_expr>(tree, expr_id).type = *current_model_type;
    }

    auto type_res = lookup(name, loc);
    if (!type_res) {
        return get_node<ast::Variable_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    return get_node<ast::Variable_expr>(tree, expr_id).type = type_res->type;
}

/*
 * [binary_expr] -> Type
 * |-- extract left and right
 * `-- type match per op type
 */
types::Type_id Semantic_checker::check_binary_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto op = get_node<ast::Binary_expr>(tree, expr_id).op;
    auto loc = get_node<ast::Binary_expr>(tree, expr_id).loc;
    auto left_id = get_node<ast::Binary_expr>(tree, expr_id).left;
    auto right_id = get_node<ast::Binary_expr>(tree, expr_id).right;

    auto left_type = check_expr(left_id);
    auto right_type = check_expr(right_id);

    auto report_error = [&](const std::string &message) {
        type_error(
            loc,
            std::format("{} (left is '{}', right is '{}')", message, env.tt.to_string(left_type), env.tt.to_string(right_type)));
    };

    types::Type_id result = env.tt.get_unknown();

    switch (op) {
    case lex::TokenType::Pipe:
    case lex::TokenType::BitXor:
    case lex::TokenType::BitAnd:
    case lex::TokenType::BitLShift:
    case lex::TokenType::BitRshift:
        if (!env.tt.is_integer_primitive(left_type) || !env.tt.is_integer_primitive(right_type)) {
            report_error("Bitwise operators require integer operands.");
            result = env.tt.get_unknown();
        } else {
            result = promote_numeric_type(left_type, right_type);
            if (env.tt.is_any(result)) {
                report_error("Bitwise operators require integers from the same signedness family.");
                result = env.tt.get_unknown();
            }
        }
        break;

    case lex::TokenType::Plus:
        if (env.tt.is_string(left_type) && env.tt.is_string(right_type)) {
            result = env.tt.get_string();
        } else if (env.tt.is_numeric_primitive(left_type) && env.tt.is_numeric_primitive(right_type)) {
            result = promote_numeric_type(left_type, right_type);
            if (env.tt.is_any(result)) {
                report_error("Mixed signed and unsigned integer arithmetic requires an explicit cast.");
                result = env.tt.get_unknown();
            }
        } else {
            report_error("Operands must be two numbers or two strings for '+'");
            result = env.tt.get_unknown();
        }
        break;

    case lex::TokenType::Minus:
    case lex::TokenType::Star:
        if (!env.tt.is_numeric_primitive(left_type) || !env.tt.is_numeric_primitive(right_type)) {
            report_error("Operands must be two numbers for this operator.");
            result = env.tt.get_unknown();
        } else {
            result = promote_numeric_type(left_type, right_type);
            if (env.tt.is_any(result)) {
                report_error("Mixed signed and unsigned integer arithmetic requires an explicit cast.");
                result = env.tt.get_unknown();
            }
        }
        break;

    case lex::TokenType::Slash:
        if (!env.tt.is_numeric_primitive(left_type) || !env.tt.is_numeric_primitive(right_type)) {
            report_error("Operands for division must be numbers.");
            result = env.tt.get_unknown();
        } else {
            result = promote_numeric_type(left_type, right_type);
            if (env.tt.is_any(result) || env.tt.is_unknown(result)) {
                report_error("Mixed signed and unsigned division requires an explicit cast.");
                result = env.tt.get_unknown();
            }
        }
        break;

    case lex::TokenType::Percent: {
        bool both_ints = env.tt.is_integer_primitive(left_type) && env.tt.is_integer_primitive(right_type);
        bool both_floats = env.tt.is_float_primitive(left_type) && env.tt.is_float_primitive(right_type);

        if (!both_ints && !both_floats) {
            report_error("Operands for '%' must be either both integers or both floats.");
            result = env.tt.get_unknown();
        } else {
            result = promote_numeric_type(left_type, right_type);
            if (env.tt.is_any(result) || env.tt.is_unknown(result)) {
                report_error("Mixed signed and unsigned modulo requires an explicit cast.");
                result = env.tt.get_unknown();
            }
        }
        break;
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
        result = env.tt.get_bool();
        break;

    case lex::TokenType::Equal:
    case lex::TokenType::NotEqual:
        if ((env.tt.is_optional(left_type) && env.tt.is_nil(right_type)) || (env.tt.is_nil(left_type) && env.tt.is_optional(right_type))) {
            result = env.tt.get_bool();
        } else {
            if (!is_compatible(left_type, right_type) && !is_compatible(right_type, left_type)) {
                report_error("Cannot compare incompatible types. Remember to use 'as' for enum casting.");
            }
            result = env.tt.get_bool();
        }
        break;

    case lex::TokenType::LogicalAnd:
    case lex::TokenType::LogicalOr:
        if (!env.tt.is_bool(left_type) || !env.tt.is_bool(right_type)) {
            report_error("Operands for logical operators must be booleans.");
        }
        result = env.tt.get_bool();
        break;

    default:
        type_error(loc, "Unsupported binary operator.");
        result = env.tt.get_unknown();
        break;
    }

    return get_node<ast::Binary_expr>(tree, expr_id).type = result;
}

/*
 * [call_expr] -> Type
 * |-- handle variable vs method
 * |-- dispatch intrinsics
 * `-- perform call linkage
 */
types::Type_id Semantic_checker::check_call_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto loc = get_node<ast::Call_expr>(tree, expr_id).loc;
    auto callee_id = get_node<ast::Call_expr>(tree, expr_id).callee;
    auto arguments_copy = get_node<ast::Call_expr>(tree, expr_id).arguments;

    auto bind_intrinsic = [&](const std::vector<std::string> &param_names, const std::string &name) -> bool {
        std::vector<ast::Call_argument> ordered(param_names.size());
        std::vector<bool> filled(param_names.size(), false);
        bool seen_named = false;
        size_t next_pos = 0;

        for (const auto &arg : arguments_copy) {
            size_t target_idx = param_names.size();
            if (!arg.name.empty()) {
                seen_named = true;
                auto it = std::find(param_names.begin(), param_names.end(), arg.name);
                if (it == param_names.end()) {
                    type_error(arg.loc, std::format("Intrinsic '{}' has no parameter named '{}'.", name, arg.name));
                    return false;
                }
                target_idx = static_cast<size_t>(std::distance(param_names.begin(), it));
                if (filled[target_idx]) {
                    type_error(arg.loc, std::format("Parameter '{}' was provided more than once.", arg.name));
                    return false;
                }
            } else {
                if (seen_named) {
                    type_error(arg.loc, "Positional arguments cannot appear after named arguments.");
                    return false;
                }
                target_idx = next_pos++;
                if (target_idx >= param_names.size()) {
                    type_error(loc, std::format("Too many arguments for '{}'. Expected {}.", name, param_names.size()));
                    return false;
                }
            }
            ordered[target_idx] = arg;
            filled[target_idx] = true;
        }

        for (size_t i = 0; i < param_names.size(); ++i) {
            if (!filled[i]) {
                type_error(loc, std::format("Missing required argument '{}' for '{}'.", param_names[i], name));
                return false;
            }
        }
        arguments_copy = std::move(ordered);
        return true;
    };

    if (std::holds_alternative<ast::Variable_expr>(tree.get(callee_id).node)) {
        auto var_callee_name = get_node<ast::Variable_expr>(tree, callee_id).name;

        if (var_callee_name == "len") {
            if (!bind_intrinsic({"collection"}, "len")) {
                return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
            auto arg_type = check_expr(arguments_copy[0].value);
            if (!env.tt.is_string(arg_type) && !env.tt.is_array(arg_type)) {
                type_error(loc, "len() expects a string or an array.");
            }
            get_node<ast::Call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_u64();
        }

        if (var_callee_name == "iter") {
            if (!bind_intrinsic({"iterable"}, "iter")) {
                return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
            auto arg_type_res = check_expr(arguments_copy[0].value);
            auto iter_type = to_iterator_type(arg_type_res);
            if (!is_iterator_protocol_type(iter_type)) {
                type_error(loc, "Value passed to iter() is not iterable.");
                return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
            get_node<ast::Call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Call_expr>(tree, expr_id).type = iter_type;
        }

        if (var_callee_name == "clone") {
            if (!bind_intrinsic({"value"}, "clone")) {
                return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
            auto ret = check_expr(arguments_copy[0].value);
            get_node<ast::Call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Call_expr>(tree, expr_id).type = ret;
        }
    }

    std::string ffi_callee_name;

    if (std::holds_alternative<ast::Variable_expr>(tree.get(callee_id).node)) {
        ffi_callee_name = get_node<ast::Variable_expr>(tree, callee_id).name;
    } else if (std::holds_alternative<ast::Static_path_expr>(tree.get(callee_id).node)) {
        auto base_id = get_node<ast::Static_path_expr>(tree, callee_id).base;
        if (std::holds_alternative<ast::Variable_expr>(tree.get(base_id).node)) {
            ffi_callee_name = get_node<ast::Variable_expr>(tree, base_id).name
                + "::" + get_node<ast::Static_path_expr>(tree, callee_id).member.lexeme;
        }
    }

    if (!ffi_callee_name.empty() && env.is_native_defined(ffi_callee_name)) {
        const auto &signatures = *env.get_native_signatures(ffi_callee_name);

        for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
            auto bound = try_bind_native_arguments(signatures[sig_index].params, arguments_copy);
            if (bound.ok) {
                get_node<ast::Call_expr>(tree, expr_id).arguments = std::move(bound.ordered_arguments);
                get_node<ast::Call_expr>(tree, expr_id).native_signature_index = static_cast<int>(sig_index);
                auto ret = parse_type_string(signatures[sig_index].ret_type_str, bound.generics);
                return get_node<ast::Call_expr>(tree, expr_id).type = ret;
            }
        }

        auto diagnostic = err::msg::error(
            this->phase,
            loc.l,
            loc.c,
            loc.file,
            "Arguments do not match any native signature for function '{}'.",
            ffi_callee_name
        );

        // 1. Build string of ACTUAL argument types provided
        std::string actual_str = "";
        for (size_t i = 0; i < arguments_copy.size(); ++i) {
            actual_str += env.tt.to_string(check_expr(arguments_copy[i].value));
            if (i != arguments_copy.size() - 1) {
                actual_str += ", ";
            }
        }
        if (actual_str.empty()) {
            actual_str = "void";
        }

        // 2. Build string of EXPECTED argument types
        std::string expected_str = "";
        if (signatures.size() == 1) {
            for (size_t i = 0; i < signatures[0].params.size(); ++i) {
                expected_str += signatures[0].params[i].type_str;
                if (i != signatures[0].params.size() - 1) {
                    expected_str += ", ";
                }
            }
            if (expected_str.empty()) {
                expected_str = "void";
            }
        } else {
            // Format overloaded signatures like: (i32, i32) OR (f64, f64)
            for (size_t s = 0; s < signatures.size(); ++s) {
                expected_str += "(";
                for (size_t i = 0; i < signatures[s].params.size(); ++i) {
                    expected_str += signatures[s].params[i].type_str;
                    if (i != signatures[s].params.size() - 1) {
                        expected_str += ", ";
                    }
                }
                expected_str += ")";
                if (s != signatures.size() - 1) {
                    expected_str += " OR ";
                }
            }
        }

        // Output the beautiful error block
        diagnostic.expected_got(expected_str, actual_str);
        errors.push_back(std::move(diagnostic));

        return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    if (std::holds_alternative<ast::Static_path_expr>(tree.get(callee_id).node)) {
        types::Type_id base_type_res = env.tt.get_unknown();
        auto sp_base_id = get_node<ast::Static_path_expr>(tree, callee_id).base;
        auto member_name = get_node<ast::Static_path_expr>(tree, callee_id).member.lexeme;
        auto sp_loc = get_node<ast::Static_path_expr>(tree, callee_id).loc;

        if (std::holds_alternative<ast::Variable_expr>(tree.get(sp_base_id).node)) {
            auto vname = get_node<ast::Variable_expr>(tree, sp_base_id).name;
            if (env.is_type_defined(vname) && !variables.lookup(vname)) {
                base_type_res = *env.get_type(vname);
                ast::get_type(tree.get(sp_base_id).node) = base_type_res;
            } else {
                base_type_res = check_expr(sp_base_id);
            }
        } else {
            base_type_res = check_expr(sp_base_id);
        }

        if (env.tt.is_union(base_type_res)) {
            type_error(loc, "Union construction via '::' is not supported.");
            return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
        } else if (env.tt.is_model(base_type_res)) {
            auto &model_name = std::get<types::Model_type>(env.tt.get(base_type_res).data).name;
            auto model_data_ptr = env.get_model(model_name);

            if (model_data_ptr) {
                if (model_data_ptr->static_methods.contains(member_name)) {
                    auto method_decl_id = model_data_ptr->static_methods.at(member_name).declaration;
                    auto decl = std::get_if<ast::Function_stmt>(&tree.get(method_decl_id).node);

                    auto bound = bind_call_arguments(decl->parameters, arguments_copy, loc, "static method", member_name);
                    get_node<ast::Call_expr>(tree, expr_id).arguments = std::move(bound.ordered_arguments);

                    std::vector<types::Type_id> fparams;
                    for (auto &p : decl->parameters) {
                        fparams.push_back(p.type);
                    }
                    get_node<ast::Static_path_expr>(tree, callee_id).type = env.tt.function(fparams, decl->return_type);

                    return get_node<ast::Call_expr>(tree, expr_id).type = decl->return_type;

                } else if (model_data_ptr->methods.contains(member_name)) {
                    if (arguments_copy.empty()) {
                        type_error(loc, "UFCS call requires an instance as the first argument.");
                        return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
                    }
                    if (!arguments_copy[0].name.empty()) {
                        type_error(arguments_copy[0].loc, "First argument of UFCS cannot be named.");
                        return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
                    }

                    auto receiver_type = check_expr(arguments_copy[0].value, base_type_res);
                    if (!is_compatible(base_type_res, receiver_type)) {
                        type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "UFCS receiver type mismatch.");
                        return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
                    }

                    auto method_decl_id = model_data_ptr->methods.at(member_name).declaration;
                    auto decl = std::get_if<ast::Function_stmt>(&tree.get(method_decl_id).node);

                    auto bound = bind_call_arguments(decl->parameters, arguments_copy, loc, "method", member_name);
                    get_node<ast::Call_expr>(tree, expr_id).arguments = std::move(bound.ordered_arguments);

                    std::vector<types::Type_id> fparams;
                    for (auto &p : decl->parameters) {
                        fparams.push_back(p.type);
                    }
                    get_node<ast::Static_path_expr>(tree, callee_id).type = env.tt.function(fparams, decl->return_type);

                    return get_node<ast::Call_expr>(tree, expr_id).type = decl->return_type;
                }
            }

            type_error(sp_loc, std::format("Model '{}' has no method '{}'.", model_name, member_name));
            return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
    }

    auto callee_type = check_expr(callee_id);
    if (env.tt.is_unknown(callee_type)) {
        return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    if (!env.tt.is_function(callee_type)) {
        type_error(ast::get_loc(tree.get(callee_id).node), "This expression cannot be called.");
        return get_node<ast::Call_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    auto sig = env.tt.get(callee_type).as<types::Function_type>();
    const ast::Function_stmt *declaration = nullptr;
    if (std::holds_alternative<ast::Variable_expr>(tree.get(callee_id).node)) {
        auto vname = get_node<ast::Variable_expr>(tree, callee_id).name;
        if (env.is_function_defined(vname)) {
            declaration = std::get_if<ast::Function_stmt>(&tree.get(env.get_function(vname)->declaration).node);
        }
    }

    if (declaration) {
        auto bound = bind_call_arguments(declaration->parameters, arguments_copy, loc, "function", declaration->name);
        get_node<ast::Call_expr>(tree, expr_id).arguments = std::move(bound.ordered_arguments);
    } else {
        auto has_named_arguments = [&]() {
            return std::any_of(arguments_copy.begin(), arguments_copy.end(), [](const auto &arg) { return !arg.name.empty(); });
        };
        if (has_named_arguments()) {
            type_error(loc, "Named arguments only supported for direct static calls.");
            return get_node<ast::Call_expr>(tree, expr_id).type = sig.ret;
        }
        if (arguments_copy.size() != sig.params.size()) {
            type_error(loc, "Incorrect number of arguments.");
            return get_node<ast::Call_expr>(tree, expr_id).type = sig.ret;
        }
        for (size_t i = 0; i < arguments_copy.size(); ++i) {
            auto arg_type = check_expr(arguments_copy[i].value, sig.params[i]);
            if (!is_compatible(sig.params[i], arg_type)) {
                auto aloc = ast::get_loc(tree.get(arguments_copy[i].value).node);
                auto diagnostic = err::msg::error(this->phase, aloc.l, aloc.c, aloc.file, "Argument type mismatch.");
                diagnostic.expected_got(env.tt.to_string(sig.params[i]), env.tt.to_string(arg_type));
                errors.push_back(std::move(diagnostic));
            }
        }
    }
    return get_node<ast::Call_expr>(tree, expr_id).type = sig.ret;
}

/*
 * [cast_expr] -> Type
 * |-- eval source expression
 * `-- validate explicitly defined conversion
 */
types::Type_id Semantic_checker::check_cast_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto child_id = get_node<ast::Cast_expr>(tree, expr_id).expression;
    auto target_type = get_node<ast::Cast_expr>(tree, expr_id).target_type;
    auto loc = get_node<ast::Cast_expr>(tree, expr_id).loc;

    auto original_type = check_expr(child_id);

    bool is_str_to_byte_arr = env.tt.is_string(original_type) && env.tt.is_array(target_type)
        && (env.tt.get_array_elem(target_type) == env.tt.get_u8() || env.tt.get_array_elem(target_type) == env.tt.get_i8());

    bool is_byte_arr_to_str = env.tt.is_array(original_type) && env.tt.is_string(target_type)
        && (env.tt.get_array_elem(original_type) == env.tt.get_u8() || env.tt.get_array_elem(original_type) == env.tt.get_i8());

    if (is_str_to_byte_arr || is_byte_arr_to_str) {
        return get_node<ast::Cast_expr>(tree, expr_id).target_type;
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
            type_error(loc, "Can only cast strings or primitive arrays to byte buffers.");
            return get_node<ast::Cast_expr>(tree, expr_id).target_type = env.tt.get_unknown();
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
                return get_node<ast::Cast_expr>(tree, expr_id).target_type = env.tt.get_unknown();
            }
            if (!is_compatible(target_type, base_type)
                && !(env.tt.is_numeric_primitive(base_type) && env.tt.is_numeric_primitive(target_type))) {
                type_error(
                    loc,
                    std::format(
                        "Cannot cast unwrapped payload.\n   optional base: '{}'\n   casted type: '{}'",
                        env.tt.to_string(base_type),
                        env.tt.to_string(target_type)));
                return get_node<ast::Cast_expr>(tree, expr_id).target_type = env.tt.get_unknown();
            }
            return target_type;
        }

        type_error(
            loc,
            std::format(
                "Cannot explicitly cast a complex optional expression.\n   optional base: '{}'\n   casted type: '{}'",
                env.tt.to_string(base_type),
                env.tt.to_string(target_type)));
        return get_node<ast::Cast_expr>(tree, expr_id).target_type = env.tt.get_unknown();
    }

    if (env.tt.is_nil(original_type) && !env.tt.is_optional(target_type)) {
        type_error(loc, "Cannot cast a 'nil' value to a non-optional type.");
        return get_node<ast::Cast_expr>(tree, expr_id).target_type = env.tt.get_unknown();
    }

    type_error(
        loc,
        std::format("Invalid cast. Cannot cast from '{}' to '{}'.", env.tt.to_string(original_type), env.tt.to_string(target_type)));
    return get_node<ast::Cast_expr>(tree, expr_id).target_type = env.tt.get_unknown();
}

/*
 * [closure_expr] -> Type
 * |-- inject args into scope
 * `-- eval body mapping
 */
types::Type_id Semantic_checker::check_closure_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto return_type = get_node<ast::Closure_expr>(tree, expr_id).return_type;
    auto loc = get_node<ast::Closure_expr>(tree, expr_id).loc;
    auto parameters = get_node<ast::Closure_expr>(tree, expr_id).parameters;
    auto body = get_node<ast::Closure_expr>(tree, expr_id).body;

    auto saved_ret = current_return_type;
    current_return_type = return_type;

    variables.begin_scope();
    for (auto const &p : parameters) {
        declare(p.name, p.type, !p.is_mut, loc);
    }
    if (!body.is_null()) {
        check_stmt(body);
    }
    variables.end_scope();

    current_return_type = saved_ret;

    std::vector<types::Type_id> param_types;
    for (auto const &p : parameters) {
        param_types.push_back(p.type);
    }

    auto ret = env.tt.function(param_types, return_type);
    get_node<ast::Closure_expr>(tree, expr_id).type = ret;
    return ret;
}

/*
 * [field_access_expr] -> Type
 * |-- assert model instance
 * `-- fetch field type
 */
types::Type_id Semantic_checker::check_field_access_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto object_id = get_node<ast::Field_access_expr>(tree, expr_id).object;
    auto field_name = get_node<ast::Field_access_expr>(tree, expr_id).field_name;
    auto loc = get_node<ast::Field_access_expr>(tree, expr_id).loc;

    auto obj_type = check_expr(object_id);

    if (env.tt.is_optional(obj_type)) {
        type_error(loc, "Cannot access field on an optional type.");
        return get_node<ast::Field_access_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    if (env.tt.is_union(obj_type)) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(obj_type).data);
        if (!contains_key(union_t.variants, field_name)) {
            type_error(loc, std::format("Union has no variant named '{}'", field_name));
            return get_node<ast::Field_access_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        auto ret = find_by_key(union_t.variants, field_name)->second;
        get_node<ast::Field_access_expr>(tree, expr_id).type = ret;
        return ret;
    }

    if (env.tt.is_model(obj_type)) {
        auto &model_t = std::get<types::Model_type>(env.tt.get(obj_type).data);
        if (contains_key(model_t.fields, field_name)) {
            auto ret = find_by_key(model_t.fields, field_name)->second;
            get_node<ast::Field_access_expr>(tree, expr_id).type = ret;
            return ret;
        }
        if (contains_key(model_t.methods, field_name)) {
            const auto &method = find_by_key(model_t.methods, field_name)->second;
            auto ret = env.tt.function(method.params, method.ret);
            get_node<ast::Field_access_expr>(tree, expr_id).type = ret;
            return ret;
        }
        type_error(loc, std::format("Model has no member '{}'", field_name));
    } else {
        type_error(loc, "Can only access fields on model instances");
    }

    return get_node<ast::Field_access_expr>(tree, expr_id).type = env.tt.get_unknown();
}

/*
 * [static_path_expr] -> Type
 * |-- assert static receiver context
 * `-- bind variants or static methods
 */
types::Type_id Semantic_checker::check_static_path_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto base_id = get_node<ast::Static_path_expr>(tree, expr_id).base;
    auto member_lexeme = get_node<ast::Static_path_expr>(tree, expr_id).member.lexeme;
    auto loc = get_node<ast::Static_path_expr>(tree, expr_id).loc;

    types::Type_id base_type = env.tt.get_unknown();

    if (std::holds_alternative<ast::Variable_expr>(tree.get(base_id).node)) {
        auto vname = get_node<ast::Variable_expr>(tree, base_id).name;
        if (env.is_type_defined(vname) && !variables.lookup(vname)) {
            base_type = *env.get_type(vname);
            ast::get_type(tree.get(base_id).node) = base_type;
        } else {
            base_type = check_expr(base_id);
        }
    } else {
        base_type = check_expr(base_id);
    }

    if (env.tt.is_union(base_type)) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(base_type).data);
        if (!contains_key(union_t.variants, member_lexeme)) {
            type_error(loc, std::format("Union has no variant '{}'.", member_lexeme));
            return get_node<ast::Static_path_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        auto variant_type = find_by_key(union_t.variants, member_lexeme)->second;
        if (variant_type != env.tt.get_void()) {
            type_error(loc, "Union payload variants are not first-class constructors.");
            return get_node<ast::Static_path_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        return get_node<ast::Static_path_expr>(tree, expr_id).type = base_type;
    } else if (env.tt.is_model(base_type)) {
        auto &model_t = std::get<types::Model_type>(env.tt.get(base_type).data);
        if (contains_key(model_t.static_methods, member_lexeme)) {
            auto sig = find_by_key(model_t.static_methods, member_lexeme)->second;
            auto ret = env.tt.function(sig.params, sig.ret);
            get_node<ast::Static_path_expr>(tree, expr_id).type = ret;
            return ret;
        }
        if (contains_key(model_t.methods, member_lexeme)) {
            auto sig = find_by_key(model_t.methods, member_lexeme)->second;
            std::vector<types::Type_id> fparams;
            fparams.push_back(base_type);
            fparams.insert(fparams.end(), sig.params.begin(), sig.params.end());
            auto ret = env.tt.function(fparams, sig.ret);
            get_node<ast::Static_path_expr>(tree, expr_id).type = ret;
            return ret;
        }
        type_error(loc, std::format("Model has no method '{}'.", member_lexeme));
        return get_node<ast::Static_path_expr>(tree, expr_id).type = env.tt.get_unknown();
    } else if (env.tt.is_enum(base_type)) {
        auto enum_data_ptr = env.get_enum(std::get<types::Enum_type>(env.tt.get(base_type).data).name);
        if (!enum_data_ptr->variants.contains(member_lexeme)) {
            type_error(loc, "Enum has no variant '" + member_lexeme + "'.");
            return get_node<ast::Static_path_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        return get_node<ast::Static_path_expr>(tree, expr_id).type = base_type;
    }

    type_error(loc, "Scope resolution operator '::' is only supported for union, model, and enum.");
    return get_node<ast::Static_path_expr>(tree, expr_id).type = env.tt.get_unknown();
}

/*
 * [enum_member_expr] -> Type
 * |-- assert explicit context exists
 * `-- structurally map into context
 */
types::Type_id Semantic_checker::check_enum_member_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    auto member_name = get_node<ast::Enum_member_expr>(tree, expr_id).member_name;
    auto loc = get_node<ast::Enum_member_expr>(tree, expr_id).loc;

    if (!context_type) {
        type_error(loc, std::format("Cannot infer enum type for '{}.'. Context is missing.", member_name));
        return get_node<ast::Enum_member_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    types::Type_id expected_type = *context_type;
    if (env.tt.is_optional(expected_type)) {
        expected_type = env.tt.get_optional_base(expected_type);
    }

    if (!env.tt.is_enum(expected_type)) {
        type_error(loc, "Context is not an enum.");
        return get_node<ast::Enum_member_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    auto enum_data_ptr = env.get_enum(std::get<types::Enum_type>(env.tt.get(expected_type).data).name);
    if (!enum_data_ptr->variants.contains(member_name)) {
        type_error(loc, std::format("Enum has no variant '{}'.", member_name));
        return get_node<ast::Enum_member_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    return get_node<ast::Enum_member_expr>(tree, expr_id).type = *context_type;
}

/*
 * [field_assignment_expr] -> Type
 * |-- eval object locally
 * `-- assert compatible write bound
 */
types::Type_id Semantic_checker::check_field_assignment_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto object_id = get_node<ast::Field_assignment_expr>(tree, expr_id).object;
    auto field_name = get_node<ast::Field_assignment_expr>(tree, expr_id).field_name;
    auto loc = get_node<ast::Field_assignment_expr>(tree, expr_id).loc;
    auto value_id = get_node<ast::Field_assignment_expr>(tree, expr_id).value;

    auto obj_type = check_expr(object_id);
    types::Type_id field_type = env.tt.get_unknown();

    if (env.tt.is_optional(obj_type)) {
        type_error(loc, "Cannot access field on an optional type.");
    } else if (env.tt.is_union(obj_type)) {
        type_error(loc, "Cannot assign directly to union variant payload via field access.");
    } else if (env.tt.is_model(obj_type)) {
        auto &model_t = std::get<types::Model_type>(env.tt.get(obj_type).data);
        if (contains_key(model_t.fields, field_name)) {
            field_type = find_by_key(model_t.fields, field_name)->second;
        } else {
            type_error(loc, std::format("Model has no member '{}'", field_name));
        }
    } else {
        type_error(loc, "Can only access fields on model instances");
    }

    if (env.tt.is_unknown(field_type)) {
        return get_node<ast::Field_assignment_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    auto val_type = check_expr(value_id, field_type);
    if (!is_compatible(field_type, val_type)) {
        type_error(loc, "Assignment type mismatch for field");
    }
    return get_node<ast::Field_assignment_expr>(tree, expr_id).type = field_type;
}

/*
 * [literal_expr] -> Type
 * `-- raw value to primitive tag
 */
types::Type_id Semantic_checker::check_literal_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    auto val = get_node<ast::Literal_expr>(tree, expr_id).value;
    auto base_type = get_node<ast::Literal_expr>(tree, expr_id).type;

    if (val.is_nil()) {
        return base_type;
    }

    if (context_type && env.tt.is_primitive(*context_type)) {
        auto target_kind = env.tt.get_primitive(*context_type);
        if (types::is_numeric_primitive(target_kind) && env.tt.is_numeric_primitive(base_type)) {
            auto coerced = coerce_numeric_literal(val, target_kind);
            if (coerced) {
                get_node<ast::Literal_expr>(tree, expr_id).value = coerced.value();
                return get_node<ast::Literal_expr>(tree, expr_id).type = *context_type;
            }
        }
    }
    return base_type;
}

/*
 * [method_call_expr] -> Type
 * |-- evaluate base object
 * |-- map to intrinsic (iter, next, prev)
 * |-- route to UFCS or member closures
 * `-- fallback to FFI
 */
types::Type_id Semantic_checker::check_method_call_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;

    auto obj_id = get_node<ast::Method_call_expr>(tree, expr_id).object;
    auto method_name = get_node<ast::Method_call_expr>(tree, expr_id).method_name;
    auto loc = get_node<ast::Method_call_expr>(tree, expr_id).loc;
    auto arguments_copy = get_node<ast::Method_call_expr>(tree, expr_id).arguments;

    auto bind_intrinsic = [&](const std::vector<std::string> &param_names, const std::string &name) -> bool {
        std::vector<ast::Call_argument> ordered(param_names.size());
        std::vector<bool> filled(param_names.size(), false);
        bool seen_named = false;
        size_t next_pos = 0;

        for (const auto &arg : arguments_copy) {
            size_t target_idx = param_names.size();
            if (!arg.name.empty()) {
                seen_named = true;
                auto it = std::find(param_names.begin(), param_names.end(), arg.name);
                if (it == param_names.end()) {
                    type_error(arg.loc, std::format("Intrinsic '{}' has no parameter named '{}'.", name, arg.name));
                    return false;
                }
                target_idx = static_cast<size_t>(std::distance(param_names.begin(), it));
                if (filled[target_idx]) {
                    type_error(arg.loc, std::format("Parameter '{}' was provided more than once.", arg.name));
                    return false;
                }
            } else {
                if (seen_named) {
                    type_error(arg.loc, "Positional arguments cannot appear after named arguments.");
                    return false;
                }
                target_idx = next_pos++;
                if (target_idx >= param_names.size()) {
                    type_error(loc, std::format("Too many arguments for '{}'. Expected {}.", name, param_names.size()));
                    return false;
                }
            }
            ordered[target_idx] = arg;
            filled[target_idx] = true;
        }

        for (size_t i = 0; i < param_names.size(); ++i) {
            if (!filled[i]) {
                type_error(loc, std::format("Missing required argument '{}' for '{}'.", param_names[i], name));
                return false;
            }
        }
        arguments_copy = std::move(ordered);
        return true;
    };

    auto obj_type = check_expr(obj_id);

    if (env.tt.is_any(obj_type)) {
        for (auto &arg : arguments_copy) {
            if (!arg.value.is_null()) {
                std::ignore = check_expr(arg.value);
            }
        }
        get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_any();
    }

    if (env.tt.is_union(obj_type)) {
        auto &union_t = std::get<types::Union_type>(env.tt.get(obj_type).data);

        if (method_name == "has" || method_name == "get") {
            if (!bind_intrinsic({"variant"}, method_name)) {
                get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }

            std::string variant_name;
            auto arg_expr_node = &tree.get(arguments_copy[0].value).node;

            if (auto *static_path = std::get_if<ast::Static_path_expr>(arg_expr_node)) {
                if (auto *var_expr = std::get_if<ast::Variable_expr>(&tree.get(static_path->base).node)) {
                    if (var_expr->name != union_t.name) {
                        type_error(
                            ast::get_loc(*arg_expr_node),
                            std::format("Argument to .{}() must be a variant of the same union type.", method_name));
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

            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);

            if (method_name == "has") {
                return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_bool();
            } else { // get
                auto it = find_by_key(union_t.variants, variant_name);
                if (it != union_t.variants.end()) {
                    return get_node<ast::Method_call_expr>(tree, expr_id).type = it->second;
                }
                return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
        } else {
            type_error(loc, "Union types only support the '.has()' and '.get()' methods.");
            return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
    }

    if (method_name == "iter") {
        if (!bind_intrinsic({}, "iter")) {
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
        }

        auto iter_type = to_iterator_type(obj_type);
        if (!is_iterator_protocol_type(iter_type)) {
            type_error(ast::get_loc(tree.get(obj_id).node), "This type is not iterable.");
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(tree, expr_id).type = iter_type;
    }

    if (env.tt.is_iterator(obj_type)) {
        if (method_name == "next" || method_name == "prev") {

            if (arguments_copy.empty()) {
                ast::Literal_expr default_step{Value(static_cast<int64_t>(1)), 0};
                ast::Expr_id step_id = tree.add_expr(ast::Expr(default_step));
                ast::get_type(tree.get(step_id).node) = env.tt.get_i32();
                arguments_copy.push_back({"", step_id, loc});
            }

            if (!bind_intrinsic({"step"}, method_name)) {
                get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }

            auto step_type = check_expr(arguments_copy[0].value);
            if (!env.tt.is_integer_primitive(step_type)) {
                type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Step amount must be an integer.");
            }

            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            auto ret = env.tt.optional(env.tt.get_iter_elem(obj_type));
            return get_node<ast::Method_call_expr>(tree, expr_id).type = ret;
        }

        type_error(loc, std::format("Native Iterators have no method '{}'.", method_name));
        return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    std::string ffi_name;
    if (env.tt.is_array(obj_type)) {
        ffi_name = "Array::" + method_name;
    }
    if (env.tt.is_string(obj_type)) {
        ffi_name = "string::" + method_name;
    }
    if (env.tt.is_optional(obj_type)) {
        ffi_name = "Optional::" + method_name;
    }

    if (env.tt.is_optional(obj_type) && method_name == "get") {
        auto base_type = env.tt.get_optional_base(obj_type);

        if (arguments_copy.empty()) {
            return get_node<ast::Method_call_expr>(tree, expr_id).type = base_type;
        } else {
            if (!bind_intrinsic({"fallback"}, "get")) {
                get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
            auto arg_type = check_expr(arguments_copy[0].value);

            if (env.tt.is_string(arg_type)) {
                get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(tree, expr_id).type = base_type;
            } else if (env.tt.is_function(arg_type)) {
                auto c_type = env.tt.get(arg_type).as<types::Function_type>();
                if (!c_type.params.empty()) {
                    type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Closure passed to 'get' must take no arguments.");
                }
                if (!is_compatible(base_type, c_type.ret)) {
                    type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Closure return type mismatch.");
                }
                get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(tree, expr_id).type = base_type;
            } else {
                type_error(loc, "Argument to 'get' must be a string or a closure.");
                get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
                return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
            }
        }
    }

    if (env.tt.is_optional(obj_type) && method_name == "or_else") {
        if (!bind_intrinsic({"fallback"}, "or_else")) {
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        auto closure_type = check_expr(arguments_copy[0].value);
        if (!env.tt.is_function(closure_type)) {
            type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Argument to 'or_else' must be a closure.");
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        auto c_type = env.tt.get(closure_type).as<types::Function_type>();
        auto base_type = env.tt.get_optional_base(obj_type);
        if (!c_type.params.empty()) {
            type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Closure passed to 'or_else' must take no arguments.");
        }
        if (!is_compatible(base_type, c_type.ret)) {
            type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Closure return type mismatch.");
        }
        get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(tree, expr_id).type = base_type;
    }

    if (!ffi_name.empty() && env.is_native_defined(ffi_name)) {
        const auto &signatures = *env.get_native_signatures(ffi_name);
        for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
            auto bound = try_bind_native_arguments(signatures[sig_index].params, arguments_copy, obj_type);
            if (bound.ok) {
                get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(bound.ordered_arguments);
                get_node<ast::Method_call_expr>(tree, expr_id).native_signature_index = static_cast<int>(sig_index);
                auto ret = parse_type_string(signatures[sig_index].ret_type_str, bound.generics);
                return get_node<ast::Method_call_expr>(tree, expr_id).type = ret;
            }
        }
        type_error(loc, std::format("Arguments do not match any native signature for method '{}'.", method_name));
        get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    if (method_name == "map") {
        if (!bind_intrinsic({"func"}, "map")) {
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        auto closure_type = check_expr(arguments_copy[0].value);
        if (!env.tt.is_function(closure_type)) {
            type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Argument to 'map' must be a closure.");
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        auto c_type = env.tt.get(closure_type).as<types::Function_type>();

        if (env.tt.is_array(obj_type)) {
            auto element_type = env.tt.get_array_elem(obj_type);
            if (c_type.params.size() != 1 || !is_compatible(c_type.params[0], element_type)) {
                type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Closure parameter mismatch.");
            }
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            auto ret = env.tt.array(c_type.ret);
            return get_node<ast::Method_call_expr>(tree, expr_id).type = ret;
        }
        if (env.tt.is_optional(obj_type)) {
            auto base_type = env.tt.get_optional_base(obj_type);
            if (c_type.params.size() != 1 || !is_compatible(c_type.params[0], base_type)) {
                type_error(ast::get_loc(tree.get(arguments_copy[0].value).node), "Closure parameter mismatch.");
            }
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
            auto ret = env.tt.optional(c_type.ret);
            return get_node<ast::Method_call_expr>(tree, expr_id).type = ret;
        }
        type_error(loc, ".map() can only be called on arrays and optionals.");
        get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    if (env.tt.is_optional(obj_type)) {
        type_error(loc, "Cannot call method on an optional type. Unwrap first.");
        get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
        return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    if (env.tt.is_model(obj_type)) {
        auto &model_name = std::get<types::Model_type>(env.tt.get(obj_type).data).name;
        auto model_data_ptr = env.get_model(model_name);

        if (model_data_ptr && model_data_ptr->methods.contains(method_name)) {
            const auto &method_decl_id = model_data_ptr->methods.at(method_name).declaration;
            auto decl = std::get_if<ast::Function_stmt>(&tree.get(method_decl_id).node);

            std::vector<ast::Call_argument> ufcs_args;
            ufcs_args.push_back({"", obj_id, loc});
            for (auto &arg : arguments_copy) {
                ufcs_args.push_back(arg);
            }

            auto bound = bind_call_arguments(decl->parameters, ufcs_args, loc, "method", method_name);
            get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(bound.ordered_arguments);

            if (!bound.ok) {
                return get_node<ast::Method_call_expr>(tree, expr_id).type = decl->return_type;
            }

            return get_node<ast::Method_call_expr>(tree, expr_id).type = decl->return_type;
        } else {
            auto &model_t = std::get<types::Model_type>(env.tt.get(obj_type).data);
            for (size_t i = 0; i < model_t.fields.size(); ++i) {
                if (model_t.fields[i].first == method_name) {
                    auto field_type = model_t.fields[i].second;
                    if (env.tt.is_function(field_type)) {
                        get_node<ast::Method_call_expr>(tree, expr_id).is_closure_field = true;
                        get_node<ast::Method_call_expr>(tree, expr_id).field_index = static_cast<uint8_t>(i);
                        auto func_type = env.tt.get(field_type).as<types::Function_type>();

                        if (arguments_copy.size() != func_type.params.size()) {
                            type_error(loc, "Incorrect number of arguments for closure field.");
                        } else {
                            for (size_t j = 0; j < arguments_copy.size(); ++j) {
                                auto arg_res = check_expr(arguments_copy[j].value, func_type.params[j]);
                                if (!is_compatible(func_type.params[j], arg_res)) {
                                    type_error(ast::get_loc(tree.get(arguments_copy[j].value).node), "Argument type mismatch.");
                                }
                            }
                        }
                        get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
                        return get_node<ast::Method_call_expr>(tree, expr_id).type = func_type.ret;
                    }
                }
            }
        }
    }

    type_error(loc, std::format("No method or closure field named '{}' in {}", method_name, env.tt.to_string(obj_type)));
    get_node<ast::Method_call_expr>(tree, expr_id).arguments = std::move(arguments_copy);
    return get_node<ast::Method_call_expr>(tree, expr_id).type = env.tt.get_unknown();
}

/*
 * [unary_expr] -> Type
 * `-- sign flip or logical inverse
 */
types::Type_id Semantic_checker::check_unary_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto op = get_node<ast::Unary_expr>(tree, expr_id).op;
    auto loc = get_node<ast::Unary_expr>(tree, expr_id).loc;
    auto right_id = get_node<ast::Unary_expr>(tree, expr_id).right;

    auto right_type = check_expr(right_id);

    switch (op) {
    case lex::TokenType::Minus:
        if (!env.tt.is_numeric_primitive(right_type)) {
            type_error(loc, "Operand for '-' must be a number");
        } else if (env.tt.is_unsigned_integer_primitive(right_type)) {
            type_error(loc, "Operand for unary '-' cannot be unsigned.");
        }
        return get_node<ast::Unary_expr>(tree, expr_id).type = right_type;
    case lex::TokenType::LogicalNot:
        if (!env.tt.is_bool(right_type)) {
            type_error(loc, "Operand for '!' must be a boolean");
        }
        return get_node<ast::Unary_expr>(tree, expr_id).type = env.tt.get_bool();
    case lex::TokenType::BitNot:
        if (!env.tt.is_integer_primitive(right_type)) {
            type_error(loc, "Operand for '~' must be an integer.");
        }
        return get_node<ast::Unary_expr>(tree, expr_id).type = right_type;
    default:
        type_error(loc, "Unsupported unary operator");
        return get_node<ast::Unary_expr>(tree, expr_id).type = env.tt.get_unknown();
    }
}

/*
 * [array_literal_expr] -> Type
 * |-- align common types
 * |-- assert depth consistency
 * `-- upgrade array to wrap depth if nil is present
 */
types::Type_id Semantic_checker::check_array_literal_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    auto elements = get_node<ast::Array_literal_expr>(tree, expr_id).elements;
    auto loc = get_node<ast::Array_literal_expr>(tree, expr_id).loc;

    if (elements.empty()) {
        if (context_type && env.tt.is_array(*context_type)) {
            return get_node<ast::Array_literal_expr>(tree, expr_id).type = *context_type;
        }
        type_error(loc, "Cannot infer the element type of an empty array.");
        return get_node<ast::Array_literal_expr>(tree, expr_id).type = env.tt.array(env.tt.get_unknown());
    }

    types::Type_id common_type = env.tt.get_nil();
    uint8_t max_depth = 0;

    std::optional<types::Type_id> element_context = std::nullopt;
    if (context_type && env.tt.is_array(*context_type)) {
        element_context = env.tt.get_array_elem(*context_type);
    }

    // Pass 1: Find Common Type and Max Depth
    for (const auto &elem_expr : elements) {
        auto elem_type = check_expr(elem_expr, element_context);

        // Calculate depth: actual wrappers + auto_wrap_depth request
        uint8_t current_depth = tree.get(elem_expr).auto_wrap_depth;
        types::Type_id base = elem_type;
        while (env.tt.is_optional(base)) {
            current_depth++;
            base = env.tt.get_optional_base(base);
        }

        if (current_depth > max_depth) {
            max_depth = current_depth;
        }

        if (env.tt.is_nil(base)) {
            // Nil is the base, common type remains what it was
        } else if (env.tt.is_nil(common_type)) {
            common_type = base;
        } else {
            if (!is_compatible(common_type, base) && !is_compatible(base, common_type)) {
                type_error(ast::get_loc(tree.get(elem_expr).node), "Array elements must have a consistent type.");
                return get_node<ast::Array_literal_expr>(tree, expr_id).type = env.tt.array(env.tt.get_unknown());
            }
        }
    }

    // Pass 2: Standardize all elements to max_depth
    for (auto elem_id : elements) {
        types::Type_id elem_type = ast::get_type(tree.get(elem_id).node);
        uint8_t current_depth = tree.get(elem_id).auto_wrap_depth;
        types::Type_id base = elem_type;
        while (env.tt.is_optional(base)) {
            current_depth++;
            base = env.tt.get_optional_base(base);
        }

        // Lift everything to max_depth. No special exception for nil!
        if (max_depth > current_depth) {
            tree.get(elem_id).auto_wrap_depth += (max_depth - current_depth);
        }
    }

    // Re-wrap common type to the discovered max depth
    for (uint8_t i = 0; i < max_depth; ++i) {
        common_type = env.tt.optional(common_type);
    }

    return get_node<ast::Array_literal_expr>(tree, expr_id).type = env.tt.array(common_type);
}

/*
 * [array_access_expr] -> Type
 * |-- verify bounds lookup maps to int
 * `-- retrieve nested element type
 */
types::Type_id Semantic_checker::check_array_access_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto array_id = get_node<ast::Array_access_expr>(tree, expr_id).array;
    auto index_id = get_node<ast::Array_access_expr>(tree, expr_id).index;
    auto loc = get_node<ast::Array_access_expr>(tree, expr_id).loc;

    auto collection_type = check_expr(array_id);

    auto index_type = check_expr(index_id);
    if (!env.tt.is_integer_primitive(index_type)) {
        type_error(ast::get_loc(tree.get(index_id).node), "Index must be an integer.");
    }

    if (env.tt.is_array(collection_type)) {
        return get_node<ast::Array_access_expr>(tree, expr_id).type = env.tt.get_array_elem(collection_type);
    } else if (env.tt.is_string(collection_type)) {
        return get_node<ast::Array_access_expr>(tree, expr_id).type = env.tt.get_string();
    } else {
        type_error(loc, "Subscript operator '[]' can only be used on arrays and strings.");
        return get_node<ast::Array_access_expr>(tree, expr_id).type = env.tt.get_unknown();
    }
}

/*
 * [array_assignment_expr] -> Type
 * |-- assert target mutable structure
 * `-- assert compatible write bound
 */
types::Type_id Semantic_checker::check_array_assignment_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto array_id = get_node<ast::Array_assignment_expr>(tree, expr_id).array;
    auto index_id = get_node<ast::Array_assignment_expr>(tree, expr_id).index;
    auto value_id = get_node<ast::Array_assignment_expr>(tree, expr_id).value;
    auto loc = get_node<ast::Array_assignment_expr>(tree, expr_id).loc;

    auto array_type = check_expr(array_id);
    if (!env.tt.is_array(array_type)) {
        if (env.tt.is_string(array_type)) {
            type_error(loc, "Strings are immutable and you can't use '[]' on them.");
            return get_node<ast::Array_assignment_expr>(tree, expr_id).type = env.tt.get_unknown();
        }
        type_error(loc, "Subscript operator '[]' can only be used on arrays.");
        return get_node<ast::Array_assignment_expr>(tree, expr_id).type = env.tt.get_unknown();
    }

    auto index_type = check_expr(index_id);
    if (!env.tt.is_integer_primitive(index_type)) {
        type_error(ast::get_loc(tree.get(index_id).node), "Array index must be an integer.");
    }

    auto element_type = env.tt.get_array_elem(array_type);
    auto value_type = check_expr(value_id, element_type);

    if (!is_compatible(element_type, value_type)) {
        type_error(ast::get_loc(tree.get(value_id).node), "Type mismatch for array assignment.");
    }

    return get_node<ast::Array_assignment_expr>(tree, expr_id).type = value_type;
}

/*
 * [range_expr] -> Type
 * |-- promote edges
 * `-- wrap in iterator object
 */
types::Type_id Semantic_checker::check_range_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto start_id = get_node<ast::Range_expr>(tree, expr_id).start;
    auto end_id = get_node<ast::Range_expr>(tree, expr_id).end;
    auto loc = get_node<ast::Range_expr>(tree, expr_id).loc;

    auto start_type = check_expr(start_id);
    auto end_type = check_expr(end_id);

    if (!env.tt.is_integer_primitive(start_type)) {
        type_error(loc, "Range start must be an integer.");
    }
    if (!env.tt.is_integer_primitive(end_type)) {
        type_error(loc, "Range end must be an integer.");
    }

    types::Type_id element_type = env.tt.get_i64();
    if (env.tt.is_integer_primitive(start_type) && env.tt.is_integer_primitive(end_type)) {
        element_type = promote_numeric_type(start_type, end_type);
        if (env.tt.is_any(element_type)) {
            type_error(loc, "Mixed signed and unsigned range bounds require an explicit cast.");
            element_type = env.tt.get_i64();
        }
    }

    return get_node<ast::Range_expr>(tree, expr_id).type = env.tt.iterator(element_type);
}

/*
 * [spawn_expr] -> Type
 * `-- yield back to OS/scheduler
 */
types::Type_id Semantic_checker::check_spawn_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto call_id = get_node<ast::Spawn_expr>(tree, expr_id).call;

    if (!call_id.is_null()) {
        std::ignore = check_expr(call_id);
    }
    return get_node<ast::Spawn_expr>(tree, expr_id).type = env.tt.get_any();
}

/*
 * [await_expr] -> Type
 * `-- blocks on routine
 */
types::Type_id Semantic_checker::check_await_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto thread_id = get_node<ast::Await_expr>(tree, expr_id).thread;

    if (!thread_id.is_null()) {
        std::ignore = check_expr(thread_id);
    }
    return get_node<ast::Await_expr>(tree, expr_id).type = env.tt.get_any();
}

/*
 * [yield_expr] -> Type
 * `-- yield payload checking
 */
types::Type_id Semantic_checker::check_yield_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto value_id = get_node<ast::Yield_expr>(tree, expr_id).value;

    if (!value_id.is_null()) {
        auto ret = check_expr(value_id);
        return get_node<ast::Yield_expr>(tree, expr_id).type = ret;
    }
    return get_node<ast::Yield_expr>(tree, expr_id).type = env.tt.get_void();
}

/*
 * [fstring_expr] -> Type
 * `-- enforce string conversions
 */
types::Type_id Semantic_checker::check_fstring_expr(ast::Expr_id expr_id, std::optional<types::Type_id> context_type)
{
    (void)context_type;
    auto interpolations = get_node<ast::Fstring_expr>(tree, expr_id).interpolations;

    for (auto interp : interpolations) {
        if (!interp.is_null()) {
            std::ignore = check_expr(interp);
        }
    }
    return get_node<ast::Fstring_expr>(tree, expr_id).type = env.tt.get_string();
}

} // namespace phos
