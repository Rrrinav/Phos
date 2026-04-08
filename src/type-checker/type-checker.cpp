#include "type-checker.hpp"

#include <algorithm>
#include <cctype>
#include <format>
#include <sstream>
#include <unordered_set>

namespace phos {

// Helper functions for vector<pair<std::string, T>> lookups
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

static void collect_nil_checked_vars_for_then(const ast::Expr &expr, std::unordered_set<std::string> &out);
static void collect_nil_checked_vars_for_else(const ast::Expr &expr, std::unordered_set<std::string> &out);

static void
collect_nil_check_from_optional_method(const ast::Method_call_expr &expr, bool target_truthy_branch, std::unordered_set<std::string> &out)
{
    if (!expr.arguments.empty())
        return;

    auto *var_expr = std::get_if<ast::Variable_expr>(&expr.object->node);
    if (!var_expr || !types::is_optional(ast::get_type(expr.object->node)))
        return;

    bool narrows_when_truthy = (expr.method_name == "has_value" || expr.method_name == "is_some");
    bool narrows_when_falsey = (expr.method_name == "is_none");

    if ((target_truthy_branch && narrows_when_truthy) || (!target_truthy_branch && narrows_when_falsey))
        out.insert(var_expr->name);
}

static void collect_nil_check_from_comparison(const ast::Binary_expr &expr, lex::TokenType target_op, std::unordered_set<std::string> &out)
{
    const ast::Variable_expr *var_expr = nullptr;
    if (auto *left_var = std::get_if<ast::Variable_expr>(&expr.left->node)) {
        if (types::is_nil(ast::get_type(expr.right->node)))
            var_expr = left_var;
    } else if (auto *right_var = std::get_if<ast::Variable_expr>(&expr.right->node)) {
        if (types::is_nil(ast::get_type(expr.left->node)))
            var_expr = right_var;
    }

    if (var_expr && expr.op == target_op)
        out.insert(var_expr->name);
}

static void collect_nil_checked_vars_for_then(const ast::Expr &expr, std::unordered_set<std::string> &out)
{
    if (auto *bin_expr = std::get_if<ast::Binary_expr>(&expr.node)) {
        if (bin_expr->op == lex::TokenType::LogicalAnd) {
            collect_nil_checked_vars_for_then(*bin_expr->left, out);
            collect_nil_checked_vars_for_then(*bin_expr->right, out);
            return;
        }

        collect_nil_check_from_comparison(*bin_expr, lex::TokenType::NotEqual, out);
        return;
    }

    if (auto *unary_expr = std::get_if<ast::Unary_expr>(&expr.node)) {
        if (unary_expr->op == lex::TokenType::LogicalNot) {
            collect_nil_checked_vars_for_else(*unary_expr->right, out);
            return;
        }
    }

    if (auto *method_expr = std::get_if<ast::Method_call_expr>(&expr.node)) {
        collect_nil_check_from_optional_method(*method_expr, true, out);
        return;
    }

    if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr.node)) {
        if (types::is_optional(var_expr->type))
            out.insert(var_expr->name);
    }
}

static void collect_nil_checked_vars_for_else(const ast::Expr &expr, std::unordered_set<std::string> &out)
{
    if (auto *bin_expr = std::get_if<ast::Binary_expr>(&expr.node)) {
        if (bin_expr->op == lex::TokenType::LogicalOr) {
            collect_nil_checked_vars_for_else(*bin_expr->left, out);
            collect_nil_checked_vars_for_else(*bin_expr->right, out);
            return;
        }

        collect_nil_check_from_comparison(*bin_expr, lex::TokenType::Equal, out);
        return;
    }

    if (auto *unary_expr = std::get_if<ast::Unary_expr>(&expr.node)) {
        if (unary_expr->op == lex::TokenType::LogicalNot) {
            collect_nil_checked_vars_for_then(*unary_expr->right, out);
            return;
        }
    }

    if (auto *method_expr = std::get_if<ast::Method_call_expr>(&expr.node)) {
        collect_nil_check_from_optional_method(*method_expr, false, out);
    }
}

static const types::Function_type *find_model_method_signature(const types::Type &type, const std::string &name)
{
    if (!types::is_model(type))
        return nullptr;

    auto model_type = types::get_model_type(type);
    auto it = find_by_key(model_type->methods, name);
    if (it == model_type->methods.end())
        return nullptr;
    return &it->second;
}

bool Type_checker::default_expr_uses_forbidden_names(const ast::Expr &expr, const std::unordered_set<std::string> &forbidden_names) const
{
    return std::visit(
        [&](const auto &node) -> bool {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, ast::Variable_expr>) {
                return forbidden_names.contains(node.name);
            } else if constexpr (std::is_same_v<T, ast::Binary_expr>) {
                return default_expr_uses_forbidden_names(*node.left, forbidden_names)
                    || default_expr_uses_forbidden_names(*node.right, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Unary_expr>) {
                return default_expr_uses_forbidden_names(*node.right, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Call_expr>) {
                if (default_expr_uses_forbidden_names(*node.callee, forbidden_names))
                    return true;
                for (const auto &arg : node.arguments)
                    if (arg.value && default_expr_uses_forbidden_names(*arg.value, forbidden_names))
                        return true;
                return false;
            } else if constexpr (std::is_same_v<T, ast::Assignment_expr>) {
                return default_expr_uses_forbidden_names(*node.value, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Field_assignment_expr>) {
                return default_expr_uses_forbidden_names(*node.object, forbidden_names)
                    || default_expr_uses_forbidden_names(*node.value, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>) {
                return default_expr_uses_forbidden_names(*node.array, forbidden_names)
                    || default_expr_uses_forbidden_names(*node.index, forbidden_names)
                    || default_expr_uses_forbidden_names(*node.value, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Cast_expr>) {
                return default_expr_uses_forbidden_names(*node.expression, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Field_access_expr>) {
                return default_expr_uses_forbidden_names(*node.object, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Method_call_expr>) {
                if (default_expr_uses_forbidden_names(*node.object, forbidden_names))
                    return true;
                for (const auto &arg : node.arguments)
                    if (arg.value && default_expr_uses_forbidden_names(*arg.value, forbidden_names))
                        return true;
                return false;
            } else if constexpr (std::is_same_v<T, ast::Model_literal_expr>) {
                for (const auto &field : node.fields)
                    if (field.second && default_expr_uses_forbidden_names(*field.second, forbidden_names))
                        return true;
                return false;
            } else if constexpr (std::is_same_v<T, ast::Closure_expr>) {
                return false;
            } else if constexpr (std::is_same_v<T, ast::Array_literal_expr>) {
                for (auto *element : node.elements)
                    if (element && default_expr_uses_forbidden_names(*element, forbidden_names))
                        return true;
                return false;
            } else if constexpr (std::is_same_v<T, ast::Array_access_expr>) {
                return default_expr_uses_forbidden_names(*node.array, forbidden_names)
                    || default_expr_uses_forbidden_names(*node.index, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Static_path_expr>) {
                return default_expr_uses_forbidden_names(*node.base, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Enum_member_expr>) {
                return false;
            } else if constexpr (std::is_same_v<T, ast::Range_expr>) {
                bool bad = false;
                if (node.start)
                    bad = default_expr_uses_forbidden_names(*node.start, forbidden_names);
                if (!bad && node.end)
                    bad = default_expr_uses_forbidden_names(*node.end, forbidden_names);
                return bad;
            } else if constexpr (std::is_same_v<T, ast::Spawn_expr>) {
                return default_expr_uses_forbidden_names(*node.call, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Await_expr>) {
                return default_expr_uses_forbidden_names(*node.thread, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Yield_expr>) {
                return node.value && default_expr_uses_forbidden_names(*node.value, forbidden_names);
            } else if constexpr (std::is_same_v<T, ast::Fstring_expr>) {
                for (auto *part : node.interpolations)
                    if (part && default_expr_uses_forbidden_names(*part, forbidden_names))
                        return true;
                return false;
            } else {
                return false;
            }
        },
        expr.node);
}

void Type_checker::validate_function_defaults(const ast::Function_stmt &stmt)
{
    std::unordered_set<std::string> forbidden_names = {"this"};
    for (const auto &param : stmt.parameters)
        forbidden_names.insert(param.name);

    for (const auto &param : stmt.parameters) {
        if (!param.default_value)
            continue;

        bool uses_forbidden = default_expr_uses_forbidden_names(*param.default_value, forbidden_names);
        if (uses_forbidden)
            type_error(
                param.loc,
                "Default argument for parameter '" + param.name
                    + "' cannot reference 'this' or another parameter. Defaults are materialized at the call site.");

        if (!uses_forbidden) {
            auto default_type = check_expr(*param.default_value, param.type);
            if (default_type && !is_compatible(param.type, *default_type))
                type_error(
                    param.loc,
                    "Default argument for parameter '" + param.name + "' must have type '" + types::type_to_string(param.type)
                        + "', but got '" + types::type_to_string(*default_type) + "'.");
        }
    }
}

void Type_checker::validate_model_defaults(const ast::Model_stmt &stmt)
{
    std::unordered_set<std::string> forbidden_names = {"this"};
    for (const auto &field : stmt.fields)
        forbidden_names.insert(field.name);

    for (const auto &field : stmt.fields) {
        if (!field.default_value)
            continue;

        bool uses_forbidden = default_expr_uses_forbidden_names(*field.default_value, forbidden_names);
        if (uses_forbidden)
            type_error(
                field.loc,
                "Default value for field '" + field.name
                    + "' cannot reference 'this' or another member. Defaults are materialized at the literal site.");

        if (!uses_forbidden) {
            auto default_type = check_expr(*field.default_value, field.type);
            if (default_type && !is_compatible(field.type, *default_type))
                type_error(
                    field.loc,
                    "Default value for field '" + field.name + "' must have type '" + types::type_to_string(field.type) + "', but got '"
                        + types::type_to_string(*default_type) + "'.");
        }
    }
}

void Type_checker::validate_union_defaults(const ast::Union_stmt &stmt)
{
    std::unordered_set<std::string> forbidden_names = {"this"};
    for (const auto &variant : stmt.variants)
        forbidden_names.insert(variant.name);

    for (const auto &variant : stmt.variants) {
        if (!variant.default_value)
            continue;

        bool uses_forbidden = default_expr_uses_forbidden_names(*variant.default_value, forbidden_names);
        if (uses_forbidden)
            type_error(
                variant.loc,
                "Default value for variant '" + variant.name
                    + "' cannot reference another member. Defaults are materialized at the literal site.");

        if (variant.type == types::Type(types::Primitive_kind::Void)) {
            type_error(variant.loc, "Variant '" + variant.name + "' does not take a payload, so it cannot declare a default value.");
            continue;
        }

        if (!uses_forbidden) {
            auto default_type = check_expr(*variant.default_value, variant.type);
            if (default_type && !is_compatible(variant.type, *default_type))
                type_error(
                    variant.loc,
                    "Default value for variant '" + variant.name + "' must have type '" + types::type_to_string(variant.type)
                        + "', but got '" + types::type_to_string(*default_type) + "'.");
        }
    }
}

Type_checker::Bound_call_arguments Type_checker::bind_call_arguments(
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
        while (next_positional < parameters.size() && filled[next_positional])
            ++next_positional;
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
                type_error(arg.loc, std::format("Parameter '{}' was provided more than once in {} '{}'.", arg.name, call_kind, call_name));
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
                type_error(
                    call_loc,
                    std::format(
                        "Too many arguments for {} '{}'. Expected at most {}, but got {}.",
                        call_kind,
                        call_name,
                        parameters.size(),
                        arguments.size()));
                result.ok = false;
                continue;
            }

            target_index = next_positional++;
        }

        auto arg_type = check_expr(*arg.value, parameters[target_index].type);
        if (arg_type && !is_compatible(parameters[target_index].type, *arg_type)) {
            type_error(
                ast::get_loc(arg.value->node),
                std::format(
                    "Argument type mismatch for parameter '{}'. Expected '{}' but got '{}'.",
                    parameters[target_index].name,
                    types::type_to_string(parameters[target_index].type),
                    types::type_to_string(*arg_type)));
            result.ok = false;
        }

        result.ordered_arguments[target_index] = ast::Call_argument{
            .name = "",
            .value = arg.value,
            .loc = arg.loc,
        };
        filled[target_index] = true;
    }

    for (size_t i = 0; i < parameters.size(); ++i) {
        if (filled[i])
            continue;

        if (parameters[i].default_value) {
            result.ordered_arguments[i] = ast::Call_argument{
                .name = "",
                .value = parameters[i].default_value,
                .loc = parameters[i].loc,
            };
            continue;
        }

        type_error(call_loc, std::format("Missing required argument '{}' for {} '{}'.", parameters[i].name, call_kind, call_name));
        result.ok = false;
    }

    return result;
}

Type_checker::Bound_native_arguments Type_checker::try_bind_native_arguments(
    const std::vector<Native_param> &parameters, const std::vector<ast::Call_argument> &arguments, std::optional<types::Type> receiver_type)
{
    Bound_native_arguments result;
    size_t parameter_offset = receiver_type ? 1 : 0;

    if (receiver_type) {
        if (parameters.empty())
            return result;

        if (!match_ffi_type(parameters[0].type, *receiver_type, result.generics))
            return result;
    }

    if (parameters.size() < parameter_offset)
        return result;

    result.ordered_arguments.resize(parameters.size() - parameter_offset);

    std::vector<bool> filled(result.ordered_arguments.size(), false);
    bool seen_named = false;
    size_t next_positional = 0;

    auto advance_to_next_positional = [&]() {
        while (next_positional < filled.size() && filled[next_positional])
            ++next_positional;
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

            if (target_index == filled.size() || filled[target_index])
                return result;
        } else {
            if (seen_named)
                return result;

            advance_to_next_positional();
            if (next_positional >= filled.size())
                return result;

            target_index = next_positional++;
        }

        auto arg_res = check_expr(*arg.value);
        if (!arg_res || !match_ffi_type(parameters[target_index + parameter_offset].type, *arg_res, result.generics))
            return result;

        result.ordered_arguments[target_index] = ast::Call_argument{
            .name = "",
            .value = arg.value,
            .loc = arg.loc,
        };
        filled[target_index] = true;
    }

    for (size_t i = 0; i < filled.size(); ++i) {
        if (filled[i])
            continue;

        if (parameters[i + parameter_offset].default_value.has_value()) {
            result.ordered_arguments[i] = ast::Call_argument{
                .name = "",
                .value = nullptr,
                .loc = {},
            };
            filled[i] = true;
            continue;
        }

        return result;
    }

    result.ok = true;
    return result;
}

// FFI STRING PARSER
types::Type Type_checker::parse_type_string(std::string str) const
{
    str.erase(str.find_last_not_of(" \t\r\n") + 1);
    str.erase(0, str.find_first_not_of(" \t\r\n"));

    if (str == "i8")
        return types::Primitive_kind::I8;
    if (str == "i16")
        return types::Primitive_kind::I16;
    if (str == "i32")
        return types::Primitive_kind::I32;
    if (str == "i64")
        return types::Primitive_kind::I64;
    if (str == "u8")
        return types::Primitive_kind::U8;
    if (str == "u16")
        return types::Primitive_kind::U16;
    if (str == "u32")
        return types::Primitive_kind::U32;
    if (str == "u64")
        return types::Primitive_kind::U64;
    if (str == "f16")
        return types::Primitive_kind::F16;
    if (str == "f32")
        return types::Primitive_kind::F32;
    if (str == "f64")
        return types::Primitive_kind::F64;
    if (str == "bool")
        return types::Primitive_kind::Bool;
    if (str == "string")
        return types::Primitive_kind::String;
    if (str == "void")
        return types::Primitive_kind::Void;
    if (str == "any")
        return types::Primitive_kind::Any;
    if (str == "nil")
        return types::Primitive_kind::Nil;

    if (str.starts_with("iter<") && str.ends_with(">")) {
        auto base = parse_type_string(str.substr(5, str.length() - 6));
        return types::Type(mem::make_rc<types::Iterator_type>(base));
    }

    if (str.ends_with("[]")) {
        auto base = parse_type_string(str.substr(0, str.length() - 2));
        return types::Type(mem::make_rc<types::Array_type>(base));
    }
    if (str.ends_with("?")) {
        auto base = parse_type_string(str.substr(0, str.length() - 1));
        return types::Type(mem::make_rc<types::Optional_type>(base));
    }

    if (model_signatures.contains(str))
        return types::Type(model_signatures.at(str));
    if (m_union_signatures.contains(str))
        return types::Type(m_union_signatures.at(str));
    if (enum_signatures.contains(str))
        return types::Type(enum_signatures.at(str));

    return types::Primitive_kind::Any;
}

// ===================================================================
// TYPE RESOLVER
// ===================================================================
void TypeResolver::resolve(std::vector<ast::Stmt *> &statements)
{
    for (auto *stmt : statements)
        if (stmt)
            resolve_stmt(*stmt);
}

void TypeResolver::resolve_stmt(ast::Stmt &stmt)
{
    std::visit([this](auto &s) { visit(s); }, stmt.node);
}

void TypeResolver::resolve_expr(ast::Expr &expr)
{
    std::visit([this](auto &e) { visit(e); }, expr.node);
}

void TypeResolver::resolve_type(types::Type &type)
{
    if (auto *model_type_ptr = std::get_if<mem::rc_ptr<types::Model_type>>(&type)) {
        if ((*model_type_ptr)->name.empty())
            return;

        const std::string &name = (*model_type_ptr)->name;

        if (checker.model_signatures.count(name))
            type = checker.model_signatures.at(name);
        else if (checker.m_union_signatures.count(name))
            type = checker.m_union_signatures.at(name);
        else if (checker.enum_signatures.count(name))
            type = checker.enum_signatures.at(name);
        else
            checker.type_error({}, "Unknown type '" + name + "'.");
    } else if (auto *union_type_ptr = std::get_if<mem::rc_ptr<types::Union_type>>(&type)) {
        if ((*union_type_ptr)->name.empty())
            return;
        const std::string &name = (*union_type_ptr)->name;
        if (checker.m_union_signatures.count(name))
            type = checker.m_union_signatures.at(name);
        else if (checker.model_signatures.count(name))
            type = checker.model_signatures.at(name);
        else if (checker.enum_signatures.count(name))
            type = checker.enum_signatures.at(name);
        else
            checker.type_error({}, "Unknown type '" + name + "'.");
    } else if (auto *enum_type_ptr = std::get_if<mem::rc_ptr<types::Enum_type>>(&type)) {
        if ((*enum_type_ptr)->name.empty())
            return;
        const std::string &name = (*enum_type_ptr)->name;
        if (checker.enum_signatures.count(name))
            type = checker.enum_signatures.at(name);
        else if (checker.model_signatures.count(name))
            type = checker.model_signatures.at(name);
        else if (checker.m_union_signatures.count(name))
            type = checker.m_union_signatures.at(name);
        else
            checker.type_error({}, "Unknown type '" + name + "'.");
    } else if (auto *func_type_ptr = std::get_if<mem::rc_ptr<types::Function_type>>(&type)) {
        for (auto &param_type : (*func_type_ptr)->parameter_types)
            resolve_type(param_type);
        resolve_type((*func_type_ptr)->return_type);
    } else if (auto *array_type_ptr = std::get_if<mem::rc_ptr<types::Array_type>>(&type)) {
        if (*array_type_ptr)
            resolve_type((*array_type_ptr)->element_type);
    } else if (auto *optional_type_ptr = std::get_if<mem::rc_ptr<types::Optional_type>>(&type)) {
        if (*optional_type_ptr)
            resolve_type((*optional_type_ptr)->base_type);
    } else if (auto *iter_type_ptr = std::get_if<mem::rc_ptr<types::Iterator_type>>(&type)) {
        if (*iter_type_ptr)
            resolve_type((*iter_type_ptr)->element_type);
    }
}

void TypeResolver::visit(ast::Function_stmt &stmt)
{
    resolve_type(stmt.return_type);
    for (auto &param : stmt.parameters) {
        resolve_type(param.type);
        if (param.default_value)
            resolve_expr(*param.default_value);
    }
    if (stmt.body)
        resolve_stmt(*stmt.body);
}

void TypeResolver::visit(ast::Model_stmt &stmt)
{
    for (auto &field : stmt.fields) {
        resolve_type(field.type);
        if (field.default_value)
            resolve_expr(*field.default_value);
    }
    for (auto *method : stmt.methods)
        if (method)
            visit(*method);
}

void TypeResolver::visit(ast::Union_stmt &stmt)
{
    for (auto &variant : stmt.variants) {
        resolve_type(variant.type);
        if (variant.default_value)
            resolve_expr(*variant.default_value);
    }
}

void TypeResolver::visit(ast::Enum_stmt &stmt)
{
    (void)stmt;
}
void TypeResolver::visit(ast::Enum_member_expr &expr)
{
    (void)expr;
}

void TypeResolver::visit(ast::Block_stmt &stmt)
{
    for (auto *s : stmt.statements)
        if (s)
            resolve_stmt(*s);
}

void TypeResolver::visit(ast::Var_stmt &stmt)
{
    resolve_type(stmt.type);
    if (stmt.initializer)
        resolve_expr(*stmt.initializer);
}

void TypeResolver::visit(ast::Print_stmt &stmt)
{
    for (auto &ex : stmt.expressions)
        if (ex)
            resolve_expr(*ex);
}

void TypeResolver::visit(ast::Expr_stmt &stmt)
{
    if (stmt.expression)
        resolve_expr(*stmt.expression);
}

void TypeResolver::visit(ast::If_stmt &stmt)
{
    if (stmt.condition)
        resolve_expr(*stmt.condition);
    if (stmt.then_branch)
        resolve_stmt(*stmt.then_branch);
    if (stmt.else_branch)
        resolve_stmt(*stmt.else_branch);
}

void TypeResolver::visit(ast::While_stmt &stmt)
{
    if (stmt.condition)
        resolve_expr(*stmt.condition);
    if (stmt.body)
        resolve_stmt(*stmt.body);
}

void TypeResolver::visit(ast::For_stmt &stmt)
{
    if (stmt.initializer)
        resolve_stmt(*stmt.initializer);
    if (stmt.condition)
        resolve_expr(*stmt.condition);
    if (stmt.increment)
        resolve_expr(*stmt.increment);
    if (stmt.body)
        resolve_stmt(*stmt.body);
}

void TypeResolver::visit(ast::For_in_stmt &stmt)
{
    if (stmt.iterable)
        resolve_expr(*stmt.iterable);
    if (stmt.body)
        resolve_stmt(*stmt.body);
}

void TypeResolver::visit(ast::Match_stmt &stmt)
{
    if (stmt.subject)
        resolve_expr(*stmt.subject);
    for (auto &arm : stmt.arms)
        if (arm.body)
            resolve_stmt(*arm.body);
}

void TypeResolver::visit(ast::Return_stmt &stmt)
{
    if (stmt.expression)
        resolve_expr(*stmt.expression);
}

void TypeResolver::visit(ast::Assignment_expr &expr)
{
    if (expr.value)
        resolve_expr(*expr.value);
}

void TypeResolver::visit(ast::Binary_expr &expr)
{
    if (expr.left)
        resolve_expr(*expr.left);
    if (expr.right)
        resolve_expr(*expr.right);
}

void TypeResolver::visit(ast::Call_expr &expr)
{
    if (expr.callee)
        resolve_expr(*expr.callee);
    for (auto &arg : expr.arguments)
        if (arg.value)
            resolve_expr(*arg.value);
}

void TypeResolver::visit(ast::Cast_expr &expr)
{
    if (expr.expression)
        resolve_expr(*expr.expression);
    resolve_type(expr.target_type);
}

void TypeResolver::visit(ast::Closure_expr &expr)
{
    resolve_type(expr.return_type);
    for (auto &param : expr.parameters) {
        resolve_type(param.type);
        if (param.default_value)
            resolve_expr(*param.default_value);
    }
    if (expr.body)
        resolve_stmt(*expr.body);
}

void TypeResolver::visit(ast::Field_access_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
}

void TypeResolver::visit(ast::Field_assignment_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
    if (expr.value)
        resolve_expr(*expr.value);
}

void TypeResolver::visit(ast::Method_call_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
    for (auto &arg : expr.arguments)
        if (arg.value)
            resolve_expr(*arg.value);
}

void TypeResolver::visit(ast::Unary_expr &expr)
{
    if (expr.right)
        resolve_expr(*expr.right);
}

void TypeResolver::visit(ast::Array_literal_expr &expr)
{
    for (auto *element : expr.elements)
        if (element)
            resolve_expr(*element);
}

void TypeResolver::visit(ast::Static_path_expr &expr)
{
    resolve_expr(*expr.base);
}

void TypeResolver::visit(ast::Model_literal_expr &expr)
{
    for (auto &field : expr.fields)
        if (field.second)
            resolve_expr(*field.second);
}

void TypeResolver::visit(ast::Array_access_expr &expr)
{
    if (expr.array)
        resolve_expr(*expr.array);
    if (expr.index)
        resolve_expr(*expr.index);
}

void TypeResolver::visit(ast::Array_assignment_expr &expr)
{
    if (expr.array)
        resolve_expr(*expr.array);
    if (expr.index)
        resolve_expr(*expr.index);
    if (expr.value)
        resolve_expr(*expr.value);
}

void TypeResolver::visit(ast::Range_expr &expr)
{
    if (expr.start)
        resolve_expr(*expr.start);
    if (expr.end)
        resolve_expr(*expr.end);
}

void TypeResolver::visit(ast::Spawn_expr &expr)
{
    if (expr.call)
        resolve_expr(*expr.call);
}

void TypeResolver::visit(ast::Await_expr &expr)
{
    if (expr.thread)
        resolve_expr(*expr.thread);
}

void TypeResolver::visit(ast::Yield_expr &expr)
{
    if (expr.value)
        resolve_expr(*expr.value);
}

void TypeResolver::visit(ast::Fstring_expr &expr)
{
    for (auto *e : expr.interpolations)
        if (e)
            resolve_expr(*e);
}

// ===================================================================
// TYPE CHECKER IMPLEMENTATIONS
// ===================================================================
std::vector<err::msg> Type_checker::check(std::vector<ast::Stmt *> &statements)
{
    begin_scope();

    // First pass to find all type names
    collect_signatures(statements);

    // Use the collected names to resolve all placeholder types in the AST
    TypeResolver resolver(*this);
    resolver.resolve(statements);

    // Re-collect signatures
    functions.clear();
    model_data.clear();
    union_data.clear();
    model_signatures.clear();
    m_union_signatures.clear();
    enum_signatures.clear();

    collect_signatures(statements);

    for (auto &stmt : statements)
        if (stmt)
            check_stmt(*stmt);

    end_scope();
    return errors;
}

void Type_checker::collect_signatures(const std::vector<ast::Stmt *> &statements)
{
    for (const auto &stmt : statements) {
        if (const auto *func_stmt = std::get_if<ast::Function_stmt>(&stmt->node)) {
            if (functions.contains(func_stmt->name))
                type_error(func_stmt->loc, "Function '" + func_stmt->name + "' is already defined.");
            else
                functions[func_stmt->name] = {func_stmt};
        } else if (const auto *model_stmt = std::get_if<ast::Model_stmt>(&stmt->node)) {
            if (model_signatures.contains(model_stmt->name) || m_union_signatures.contains(model_stmt->name)
                || enum_signatures.contains(model_stmt->name)) {
                type_error(model_stmt->loc, "Type name '" + model_stmt->name + "' is already defined.");
            } else {
                auto model_type = mem::make_rc<types::Model_type>();
                ModelData data;
                model_type->name = model_stmt->name;
                data.signature = model_type;

                for (const auto &field : model_stmt->fields) {
                    model_type->fields.push_back({field.name, field.type});
                    if (field.default_value)
                        data.field_defaults[field.name] = field.default_value;
                }

                for (const auto &method_ast : model_stmt->methods) {
                    types::Function_type method_type;
                    for (const auto &param : method_ast->parameters)
                        method_type.parameter_types.push_back(param.type);
                    method_type.return_type = method_ast->return_type;

                    if (method_ast->is_static) {
                        data.static_methods[method_ast->name] = {method_ast};
                        model_type->static_methods.push_back({method_ast->name, method_type});
                    } else {
                        data.methods[method_ast->name] = {method_ast};
                        model_type->methods.push_back({method_ast->name, method_type});
                    }
                }
                model_signatures[model_stmt->name] = model_type;
                model_data[model_stmt->name] = std::move(data);
            }
        } else if (const auto *union_stmt = std::get_if<ast::Union_stmt>(&stmt->node)) {
            if (m_union_signatures.contains(union_stmt->name) || model_signatures.contains(union_stmt->name)
                || enum_signatures.contains(union_stmt->name)) {
                type_error(union_stmt->loc, "Type name '" + union_stmt->name + "' is already defined.");
            } else {
                auto union_type = mem::make_rc<types::Union_type>();
                UnionData data;
                union_type->name = union_stmt->name;
                for (const auto &variant : union_stmt->variants) {
                    if (contains_key(union_type->variants, variant.name))
                        type_error(union_stmt->loc, "Duplicate variant '" + variant.name + "' in union '" + union_stmt->name + "'.");
                    union_type->variants.push_back({variant.name, variant.type});
                    if (variant.default_value)
                        data.variant_defaults[variant.name] = variant.default_value;
                }
                data.signature = union_type;
                m_union_signatures[union_stmt->name] = union_type;
                union_data[union_stmt->name] = std::move(data);
            }
        } else if (const auto *enum_stmt = std::get_if<ast::Enum_stmt>(&stmt->node)) {
            if (enum_signatures.contains(enum_stmt->name) || model_signatures.contains(enum_stmt->name)
                || m_union_signatures.contains(enum_stmt->name)) {
                type_error(enum_stmt->loc, "Type name '" + enum_stmt->name + "' is already defined.");
            } else {
                auto enum_type = mem::make_rc<types::Enum_type>();
                enum_type->name = enum_stmt->name;
                enum_type->base_type = enum_stmt->base_type;
                enum_type->variants = mem::make_rc<Enum_variants>(); // Allocate the wrapper here

                std::int64_t current_signed_value = 0;
                std::uint64_t current_unsigned_value = 0;
                for (const auto &variant : enum_stmt->variants) {
                    Value val = Value(nullptr);
                    if (variant.second.has_value()) {
                        val = variant.second.value();
                        if (is_signed_integer(val))
                            current_signed_value = get_int(val) + 1;
                        else if (is_unsigned_integer(val))
                            current_unsigned_value = get_uint(val) + 1;
                    } else {
                        // Auto Derive String, or Auto Increment Int
                        if (types::is_primitive(enum_type->base_type)
                            && types::is_integer_primitive(types::get_primitive_kind(enum_type->base_type))) {
                            auto base_kind = types::get_primitive_kind(enum_type->base_type);
                            auto next_value = types::is_signed_integer_primitive(base_kind) ? Value(current_signed_value++)
                                                                                            : Value(current_unsigned_value++);
                            auto coerced = coerce_numeric_literal(next_value, base_kind);
                            if (!coerced) {
                                type_error(enum_stmt->loc, "Enum auto-increment value overflowed the enum base type.");
                                val = Value(nullptr);
                            } else {
                                val = coerced.value();
                            }
                        } else
                            val = Value(variant.first);
                    }

                    if (enum_type->variants->map.contains(variant.first))
                        type_error(enum_stmt->loc, "Duplicate variant '" + variant.first + "' in enum.");

                    for (const auto &[existing_name, existing_val] : enum_type->variants->map) {
                        if (existing_val == val) {
                            type_error(
                                enum_stmt->loc,
                                "Enum variant value " + value_to_string(val) + " is already used by variant '" + existing_name
                                    + "'. Variant values must be unique.");
                        }
                    }
                    enum_type->variants->map[variant.first] = val;
                }
                enum_signatures[enum_stmt->name] = enum_type;
            }
        }
    }
}

void Type_checker::begin_scope()
{
    scopes.emplace_back();
    m_nil_checked_vars_stack.emplace_back();
}

void Type_checker::end_scope()
{
    if (!scopes.empty())
        scopes.pop_back();
    if (!m_nil_checked_vars_stack.empty())
        m_nil_checked_vars_stack.pop_back();
}

void Type_checker::declare(const std::string &name, const types::Type &type, bool is_constant, const ast::Source_location &loc)
{
    if (!scopes.empty() && scopes.back().count(name))
        type_error(loc, "Variable '" + name + "' is already declared in this scope.");
    else
        scopes.back()[name] = {type, is_constant};
}

bool Type_checker::is_compatible(const types::Type &expected, const types::Type &actual) const
{
    if (is_any(expected))
        return true;

    if (is_optional(expected)) {
        if (is_nil(actual))
            return true;
        if (is_optional(actual))
            return is_compatible(types::get_optional_type(expected)->base_type, types::get_optional_type(actual)->base_type);
        return is_compatible(types::get_optional_type(expected)->base_type, actual);
    }
    if (is_optional(actual) || is_nil(actual))
        return false;

    if (auto *expected_array = std::get_if<mem::rc_ptr<types::Array_type>>(&expected)) {
        if (is_any((*expected_array)->element_type) && is_array(actual))
            return true;
    }
    if (const auto *a_array = std::get_if<mem::rc_ptr<types::Array_type>>(&expected)) {
        if (const auto *b_array = std::get_if<mem::rc_ptr<types::Array_type>>(&actual))
            return is_compatible((*a_array)->element_type, (*b_array)->element_type);
    }

    if (auto *expected_iter = std::get_if<mem::rc_ptr<types::Iterator_type>>(&expected)) {
        if (is_any((*expected_iter)->element_type) && is_iterator(actual))
            return true;
    }
    if (const auto *a_iter = std::get_if<mem::rc_ptr<types::Iterator_type>>(&expected)) {
        if (const auto *b_iter = std::get_if<mem::rc_ptr<types::Iterator_type>>(&actual))
            return is_compatible((*a_iter)->element_type, (*b_iter)->element_type);
    }

    // Function_type handles its own <=> equality gracefully
    return expected == actual;
}

bool Type_checker::is_argument_compatible(const std::vector<types::Type> &allowed_types, const types::Type &actual_type) const
{
    for (const auto &allowed : allowed_types)
        if (is_compatible(allowed, actual_type))
            return true;
    return false;
}

types::Type Type_checker::promote_numeric_type(const types::Type &left, const types::Type &right) const
{
    auto left_kind = std::get<types::Primitive_kind>(left);
    auto right_kind = std::get<types::Primitive_kind>(right);

    if (types::is_float_primitive(left_kind) || types::is_float_primitive(right_kind)) {
        if (types::is_float_primitive(left_kind) && types::is_float_primitive(right_kind))
            return types::primitive_bit_width(left_kind) >= types::primitive_bit_width(right_kind) ? left_kind : right_kind;

        return types::is_float_primitive(left_kind) ? left_kind : right_kind;
    }

    bool same_signedness = (types::is_signed_integer_primitive(left_kind) && types::is_signed_integer_primitive(right_kind))
        || (types::is_unsigned_integer_primitive(left_kind) && types::is_unsigned_integer_primitive(right_kind));

    if (!same_signedness)
        return types::Primitive_kind::Any;

    return types::primitive_bit_width(left_kind) >= types::primitive_bit_width(right_kind) ? left_kind : right_kind;
}

bool Type_checker::is_numeric(const types::Type &type) const
{
    if (const auto *prim = std::get_if<types::Primitive_kind>(&type))
        return types::is_numeric_primitive(*prim);
    return false;
}

bool Type_checker::is_boolean(const types::Type &type) const
{
    if (const auto *prim = std::get_if<types::Primitive_kind>(&type))
        return *prim == types::Primitive_kind::Bool;
    return false;
}

bool Type_checker::is_string(const types::Type &type) const
{
    if (const auto *prim = std::get_if<types::Primitive_kind>(&type))
        return *prim == types::Primitive_kind::String;
    return false;
}

bool Type_checker::is_array(const types::Type &type) const
{
    return std::holds_alternative<mem::rc_ptr<types::Array_type>>(type);
}
bool Type_checker::is_function(const types::Type &type) const
{
    return std::holds_alternative<mem::rc_ptr<types::Function_type>>(type);
}
bool Type_checker::is_model(const types::Type &type) const
{
    return std::holds_alternative<mem::rc_ptr<types::Model_type>>(type);
}
bool Type_checker::is_union(const types::Type &type) const
{
    return std::holds_alternative<mem::rc_ptr<types::Union_type>>(type);
}
bool Type_checker::is_enum(const types::Type &type) const
{
    return std::holds_alternative<mem::rc_ptr<types::Enum_type>>(type);
}
bool Type_checker::is_iterator(const types::Type &type) const
{
    return std::holds_alternative<mem::rc_ptr<types::Iterator_type>>(type);
}
bool Type_checker::is_any(const types::Type &type) const
{
    if (const auto *prim = std::get_if<types::Primitive_kind>(&type))
        return *prim == types::Primitive_kind::Any;
    return false;
}

bool Type_checker::is_nil(const types::Type &type) const
{
    return types::is_nil(type);
}
bool Type_checker::is_optional(const types::Type &type) const
{
    return types::is_optional(type);
}

bool Type_checker::is_iterator_protocol_type(const types::Type &type) const
{
    if (is_iterator(type))
        return true;

    auto next_method = find_model_method_signature(type, "next");
    if (!next_method)
        return false;
    if (!next_method->parameter_types.empty())
        return false;
    return is_optional(next_method->return_type);
}

types::Type Type_checker::iterator_element_type(const types::Type &type) const
{
    if (is_iterator(type))
        return types::get_iterator_type(type)->element_type;

    if (auto next_method = find_model_method_signature(type, "next")) {
        if (is_optional(next_method->return_type))
            return types::get_optional_type(next_method->return_type)->base_type;
    }

    return types::Primitive_kind::Void;
}

types::Type Type_checker::to_iterator_type(const types::Type &type) const
{
    if (is_iterator_protocol_type(type))
        return type;
    if (is_array(type))
        return types::Type(mem::make_rc<types::Iterator_type>(types::get_array_type(type)->element_type));
    if (is_string(type))
        return types::Type(mem::make_rc<types::Iterator_type>(types::Type(types::Primitive_kind::String)));
    if (is_optional(type))
        return types::Type(mem::make_rc<types::Iterator_type>(types::get_optional_type(type)->base_type));
    if (is_nil(type))
        return types::Type(mem::make_rc<types::Iterator_type>(types::Type(types::Primitive_kind::Any)));
    if (auto iter_method = find_model_method_signature(type, "iter")) {
        if (iter_method->parameter_types.empty() && is_iterator_protocol_type(iter_method->return_type))
            return iter_method->return_type;
    }
    if (type == types::Type(types::Primitive_kind::Void))
        return types::Type(types::Primitive_kind::Void);
    return types::Type(mem::make_rc<types::Iterator_type>(type));
}

Result<std::pair<types::Type, bool>> Type_checker::lookup_variable(const std::string &name, const ast::Source_location &loc)
{
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it)
        if (it->contains(name))
            return it->at(name);
    return std::unexpected(err::msg("", "", 0, 0));
}

Result<std::pair<types::Type, bool>> Type_checker::lookup(const std::string &name, const ast::Source_location &loc)
{
    auto var = lookup_variable(name, loc);
    if (var)
        return var;

    // --- CHECK FFI SIDE-TABLE ---
    if (native_signatures.contains(name)) {
        auto dummy_func = mem::make_rc<types::Function_type>();
        dummy_func->return_type = types::Primitive_kind::Any;
        return std::make_pair(types::Type(dummy_func), true);
    }
    // ----------------------------

    if (functions.contains(name)) {
        auto func_type = mem::make_rc<types::Function_type>();
        const auto &func_data = functions.at(name);
        for (const auto &param : func_data.declaration->parameters)
            func_type->parameter_types.push_back(param.type);
        func_type->return_type = func_data.declaration->return_type;
        return std::make_pair(types::Type(func_type), true);
    }

    if (model_signatures.count(name))
        return std::make_pair(model_signatures.at(name), true);
    if (m_union_signatures.count(name))
        return std::make_pair(m_union_signatures.at(name), true);
    if (enum_signatures.count(name))
        return std::make_pair(enum_signatures.at(name), true);

    type_error(loc, "Undefined variable, function, or type '" + name + "'.");
    return std::unexpected(err::msg("", "", 0, 0));
}

void Type_checker::check_stmt(ast::Stmt &stmt)
{
    std::visit([this](auto &s) { check_stmt_node(s); }, stmt.node);
}

Result<types::Type> Type_checker::check_expr(ast::Expr &expr, std::optional<types::Type> context_type)
{
    // 1. Explicit Optional Fallback Interception: .{ .val = ... }
    if (context_type && types::is_optional(*context_type)) {
        if (auto *anon = std::get_if<ast::Anon_model_literal_expr>(&expr.node)) {
            // If it looks EXACTLY like our explicit fallback
            if (anon->fields.size() == 1 && anon->fields[0].first == "val") {
                types::Type inner_context = types::get_optional_type(*context_type)->base_type;
                ast::Expr *inner_expr = anon->fields[0].second;

                if (inner_expr) {
                    auto inner_res = check_expr(*inner_expr, inner_context);
                    if (!inner_res)
                        return inner_res;

                    if (!is_compatible(inner_context, *inner_res)) {
                        type_error(ast::get_loc(expr.node), "Type mismatch for explicit optional payload.");
                        return ast::get_type(expr.node) = types::Primitive_kind::Void;
                    }

                    // REWRITE AST: Replace Anon_model with the inner expr, increment wrap depth
                    uint8_t existing_wraps = inner_expr->auto_wrap_depth;
                    expr.node = std::move(inner_expr->node);
                    expr.auto_wrap_depth = existing_wraps + 1;
                    return ast::get_type(expr.node) = *context_type;
                }
            }
        }
    }

    // 2. Standard Expression Evaluation
    auto res = std::visit([this, context_type](auto &e) -> Result<types::Type> { return check_expr_node(e, context_type); }, expr.node);
    if (!res)
        return res;

    types::Type actual_type = res.value();

    // 3. Auto-Lifting Logic
    if (context_type) {
        int expected_depth = get_optional_depth(*context_type);
        int actual_depth = get_optional_depth(actual_type);

        if (expected_depth > actual_depth) {
            types::Type expected_base = get_optional_base(*context_type);
            types::Type actual_base = get_optional_base(actual_type);

            // Base 'nil' never needs to be explicitly wrapped. The VM natively understands depth 0 nil.
            if (types::is_nil(actual_base)) {
                expr.auto_wrap_depth = 0;
                return ast::get_type(expr.node) = *context_type;
            }

            // If the underlying types match, calculate the gap and assign the tag!
            if (is_compatible(expected_base, actual_base)) {
                expr.auto_wrap_depth = expected_depth - actual_depth;
                return ast::get_type(expr.node) = *context_type;
            }
        }
    }

    return ast::get_type(expr.node) = actual_type;
}

void Type_checker::check_stmt_node(ast::Function_stmt &stmt)
{
    auto saved_return = current_return_type;
    current_return_type = stmt.return_type;
    validate_function_defaults(stmt);
    begin_scope();
    if (current_model_type && !stmt.is_static)
        declare("this", *current_model_type, true, stmt.loc);
    for (const auto &p : stmt.parameters)
        declare(p.name, p.type, !p.is_mut, stmt.loc);
    if (stmt.body)
        check_stmt(*stmt.body);
    end_scope();
    current_return_type = saved_return;
}

void Type_checker::check_stmt_node(ast::Var_stmt &stmt)
{
    types::Type init_type = types::Type(types::Primitive_kind::Void);
    bool initializer_failed = false;
    if (stmt.initializer) {
        auto res = check_expr(*stmt.initializer, stmt.type_inferred ? std::nullopt : std::make_optional(stmt.type));
        if (res)
            init_type = res.value();
        else
            return;

        if (init_type == types::Type(types::Primitive_kind::Void))
            initializer_failed = true;
    }

    if (stmt.type_inferred) {
        if (initializer_failed)
            return;

        if (is_array(init_type) && types::get_array_type(init_type)->element_type == types::Type(types::Primitive_kind::Void))
            type_error(stmt.loc, "Cannot infer type of an empty array initializer.");

        stmt.type = init_type;
    } else if (stmt.initializer && !initializer_failed && !is_compatible(stmt.type, init_type)) {
        type_error(
            stmt.loc,
            "Initializer type '" + types::type_to_string(init_type) + "' does not match variable's declared type '"
                + types::type_to_string(stmt.type) + "'.");
    }
    declare(stmt.name, stmt.type, !stmt.is_mut, stmt.loc);
}

void Type_checker::check_stmt_node(ast::Model_stmt &stmt)
{
    validate_model_defaults(stmt);
    auto saved_model = current_model_type;
    current_model_type = model_signatures.at(stmt.name);
    for (auto &method : stmt.methods)
        check_stmt_node(*method);
    current_model_type = saved_model;
}

void Type_checker::check_stmt_node(ast::Union_stmt &stmt)
{
    validate_union_defaults(stmt);
}
void Type_checker::check_stmt_node(ast::Enum_stmt &stmt)
{
    (void)stmt;
}

void Type_checker::check_stmt_node(ast::Block_stmt &stmt)
{
    begin_scope();
    for (auto &s : stmt.statements)
        if (s)
            check_stmt(*s);
    end_scope();
}

void Type_checker::check_stmt_node(ast::Expr_stmt &stmt)
{
    if (stmt.expression)
        std::ignore = check_expr(*stmt.expression);
}

void Type_checker::check_stmt_node(ast::If_stmt &stmt)
{
    auto condition_res = check_expr(*stmt.condition);
    if (!condition_res)
        return;
    auto condition_type = condition_res.value();
    if (!is_boolean(condition_type) && !is_optional(condition_type))
        type_error(ast::get_loc(stmt.condition->node), "If condition must be a boolean or an optional type.");

    std::unordered_set<std::string> narrowed_in_then;
    std::unordered_set<std::string> narrowed_in_else;
    collect_nil_checked_vars_for_then(*stmt.condition, narrowed_in_then);
    collect_nil_checked_vars_for_else(*stmt.condition, narrowed_in_else);

    begin_scope();
    for (const auto &name : narrowed_in_then)
        m_nil_checked_vars_stack.back().insert(name);
    if (stmt.then_branch)
        check_stmt(*stmt.then_branch);
    end_scope();

    begin_scope();
    for (const auto &name : narrowed_in_else)
        m_nil_checked_vars_stack.back().insert(name);
    if (stmt.else_branch)
        check_stmt(*stmt.else_branch);
    end_scope();
}

void Type_checker::check_stmt_node(ast::Print_stmt &stmt)
{
    for (auto &ex : stmt.expressions)
        if (ex)
            std::ignore = check_expr(*ex);
}

void Type_checker::check_stmt_node(ast::Return_stmt &stmt)
{
    if (!current_return_type) {
        type_error(stmt.loc, "Return statement used outside of a function");
        return;
    }
    if (stmt.expression) {
        if (auto val_res = check_expr(*stmt.expression, current_return_type);
            val_res && !is_compatible(current_return_type.value(), val_res.value()))
            type_error(
                stmt.loc,
                "Return value type does not match function's return type: expected: " + types::type_to_string(current_return_type.value())
                    + " got: " + types::type_to_string(val_res.value()));
    } else if (current_return_type.value() != types::Type(types::Primitive_kind::Void)) {
        type_error(stmt.loc, "Function with non-void return type must return a value");
    }
}

void Type_checker::check_stmt_node(ast::While_stmt &stmt)
{
    if (stmt.condition) {
        auto res = check_expr(*stmt.condition);
        if (res && !is_boolean(res.value()))
            type_error(ast::get_loc(stmt.condition->node), "While condition must be a boolean");
    }
    if (stmt.body)
        check_stmt(*stmt.body);
}

void Type_checker::check_stmt_node(ast::For_stmt &stmt)
{
    begin_scope();
    if (stmt.initializer)
        check_stmt(*stmt.initializer);
    if (stmt.condition) {
        auto res = check_expr(*stmt.condition);
        if (res && !is_boolean(res.value()))
            type_error(ast::get_loc(stmt.condition->node), "For loop condition must be a boolean");
    }
    if (stmt.increment)
        std::ignore = check_expr(*stmt.increment);
    if (stmt.body)
        check_stmt(*stmt.body);
    end_scope();
}

void Type_checker::check_stmt_node(ast::For_in_stmt &stmt)
{
    auto iterable_res = check_expr(*stmt.iterable);
    if (!iterable_res)
        return;

    auto iter_type = to_iterator_type(iterable_res.value());
    if (!is_iterator_protocol_type(iter_type)) {
        type_error(stmt.loc, "Value in 'for..in' loop is not iterable.");
        return;
    }
    types::Type var_type = iterator_element_type(iter_type);

    begin_scope();
    declare(stmt.var_name, var_type, true, stmt.loc);
    if (stmt.body)
        check_stmt(*stmt.body);
    end_scope();
}

void Type_checker::check_stmt_node(ast::Match_stmt &stmt)
{
    // 1. Resolve the subject we are matching against
    auto subject_res = check_expr(*stmt.subject);
    if (!subject_res)
        return;

    types::Type subject_type = subject_res.value();

    // We only treat static paths as special payload-binders if the subject is actually a Union!
    bool is_union_subject = is_union(subject_type);

    for (auto &arm : stmt.arms) {
        begin_scope();

        if (!arm.is_wildcard) {
            // --- UNION MATCHING ---
            if (is_union_subject
                && (std::holds_alternative<ast::Static_path_expr>(arm.pattern->node)
                    || std::holds_alternative<ast::Enum_member_expr>(arm.pattern->node))) {
                auto union_type = types::get_union_type(subject_type);
                std::string variant_name;
                ast::Source_location pattern_loc = stmt.loc;

                if (auto *static_path = std::get_if<ast::Static_path_expr>(&arm.pattern->node)) {
                    variant_name = static_path->member.lexeme;
                    pattern_loc = static_path->loc;
                    static_path->type = subject_type;
                } else if (auto *member = std::get_if<ast::Enum_member_expr>(&arm.pattern->node)) {
                    variant_name = member->member_name;
                    pattern_loc = member->loc;
                    member->type = subject_type;
                }

                auto variant_it = std::find_if(union_type->variants.begin(), union_type->variants.end(), [&](const auto &v) {
                    return v.first == variant_name;
                });

                if (variant_it == union_type->variants.end()) {
                    type_error(pattern_loc, "Variant '" + variant_name + "' does not exist in union '" + union_type->name + "'.");
                } else if (!arm.bind_name.empty()) {
                    if (variant_it->second == types::Type(types::Primitive_kind::Void))
                        type_error(pattern_loc, "Variant '" + variant_name + "' does not hold a payload to bind.");
                    else
                        declare(arm.bind_name, variant_it->second, true, stmt.loc);
                }
            }
            // --- GENERAL EXPRESSION MATCHING (Includes Enums!) ---
            else {
                auto pattern_res = check_expr(*arm.pattern, subject_type);
                if (!pattern_res) {
                    end_scope();
                    continue;
                }

                types::Type pattern_type = pattern_res.value();

                bool is_range_match = std::holds_alternative<ast::Range_expr>(arm.pattern->node) && types::is_primitive(subject_type)
                    && types::is_integer_primitive(types::get_primitive_kind(subject_type));

                // --- THE CUSTOM PROTOCOL CHECK ---
                bool has_custom_match = false;
                if (auto *model_t = std::get_if<mem::rc_ptr<types::Model_type>>(&pattern_type)) {
                    if (contains_key((*model_t)->methods, "__match__")) {
                        auto match_sig = find_by_key((*model_t)->methods, "__match__")->second;

                        // The method must take exactly 1 explicit argument (the subject)
                        if (match_sig.parameter_types.size() == 1 && is_compatible(match_sig.parameter_types[0], subject_type)) {
                            if (match_sig.return_type == types::Type(types::Primitive_kind::Bool))
                                has_custom_match = true;
                            else
                                type_error(ast::get_loc(arm.pattern->node), "The __match__ method must return a bool.");
                        } else {
                            type_error(
                                ast::get_loc(arm.pattern->node),
                                "The __match__ method must accept exactly one argument of the subject's type.");
                        }
                    }
                }

                if (!is_compatible(subject_type, pattern_type) && !is_range_match && !has_custom_match) {
                    type_error(
                        ast::get_loc(arm.pattern->node),
                        "Match arm pattern type must be compatible with subject type, or implement the __match__ protocol.");
                }
            }
        }

        if (arm.body)
            check_stmt(*arm.body);

        end_scope();
    }
    bool has_wildcard = std::any_of(stmt.arms.begin(), stmt.arms.end(), [](const auto &arm) { return arm.is_wildcard; });

    if (has_wildcard)
        return; // wildcards always make a match exhaustive, nothing more to check

    if (is_enum(subject_type)) {
        // Check that every variant is covered
        auto enum_t = types::get_enum_type(subject_type);
        std::unordered_set<std::string> covered;

        for (const auto &arm : stmt.arms)
            if (auto *static_path = std::get_if<ast::Static_path_expr>(&arm.pattern->node))
                covered.insert(static_path->member.lexeme);
            else if (auto *member = std::get_if<ast::Enum_member_expr>(&arm.pattern->node))
                covered.insert(member->member_name);

        std::vector<std::string> missing;
        for (const auto &[name, _] : enum_t->variants->map)
            if (!covered.count(name))
                missing.push_back(name);

        if (!missing.empty()) {
            std::string list;
            for (size_t i = 0; i < missing.size(); ++i) {
                if (i)
                    list += ", ";
                list += "'" + missing[i] + "'";
            }
            type_error(
                stmt.loc,
                "Non-exhaustive match on enum '" + enum_t->name + "'. Missing variants: " + list
                    + ". Add the missing arms or a wildcard '_'.");
        }
        return;
    }

    if (is_union(subject_type)) {
        auto union_t = types::get_union_type(subject_type);
        std::unordered_set<std::string> covered;

        for (const auto &arm : stmt.arms)
            if (auto *static_path = std::get_if<ast::Static_path_expr>(&arm.pattern->node))
                covered.insert(static_path->member.lexeme);
            else if (auto *member = std::get_if<ast::Enum_member_expr>(&arm.pattern->node))
                covered.insert(member->member_name);

        std::vector<std::string> missing;
        for (const auto &[name, _] : union_t->variants)
            if (!covered.count(name))
                missing.push_back(name);

        if (!missing.empty()) {
            std::string list;
            for (size_t i = 0; i < missing.size(); ++i) {
                if (i)
                    list += ", ";
                list += "'" + missing[i] + "'";
            }
            type_error(
                stmt.loc,
                "Non-exhaustive match on union '" + union_t->name + "'. Missing variants: " + list
                    + ". Add the missing arms or a wildcard '_'.");
        }
        return;
    }

    // Everything else: int, float, string, bool, model, custom protocol — infinite domain
    type_error(
        stmt.loc,
        "Non-exhaustive match on type '" + types::type_to_string(subject_type)
            + "'. This type has an infinite domain and requires a wildcard '_' arm.");
}

Result<types::Type> Type_checker::check_expr_node(ast::Assignment_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto var_info_res = lookup(expr.name, expr.loc);
    if (!var_info_res)
        return std::unexpected(err::msg("Variable not found", "", 0, 0));

    bool is_constant = var_info_res.value().second;
    if (is_constant)
        type_error(expr.loc, "Cannot assign to an immutable/constant variable '" + expr.name + "'.");

    auto var_type = var_info_res.value().first;
    auto val_type_res = check_expr(*expr.value, var_type);
    if (!val_type_res)
        return val_type_res;

    if (!is_compatible(var_type, val_type_res.value()))
        type_error(expr.loc, "Assignment type mismatch");
    return expr.type = var_type;
}

Result<types::Type> Type_checker::check_expr_node(ast::Variable_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    if (expr.name == "this") {
        if (!current_model_type) {
            type_error(expr.loc, "Cannot use 'this' outside of a model method");
            return expr.type = types::Primitive_kind::Void;
        }
        return expr.type = *current_model_type;
    }
    auto type_res = lookup(expr.name, expr.loc);
    if (!type_res)
        return expr.type = types::Primitive_kind::Void;

    auto actual_type = type_res.value().first;
    for (const auto &scope : m_nil_checked_vars_stack) {
        if (scope.count(expr.name)) {
            if (is_optional(actual_type))
                return expr.type = types::get_optional_type(actual_type)->base_type;
        }
    }
    return expr.type = actual_type;
}

Result<types::Type> Type_checker::check_expr_node(ast::Binary_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto left_res = check_expr(*expr.left);
    auto right_res = check_expr(*expr.right);
    if (!left_res || !right_res)
        return expr.type = types::Primitive_kind::Void;

    types::Type left_type = left_res.value();
    types::Type right_type = right_res.value();

    // FIX FOR THE "UNKNOWN" ERROR: Extract enum name locally if type_to_string doesn't know it!
    auto get_type_str = [](const types::Type &t) {
        if (std::holds_alternative<mem::rc_ptr<types::Enum_type>>(t))
            return std::get<mem::rc_ptr<types::Enum_type>>(t)->name;
        return types::type_to_string(t);
    };

    auto report_error = [&](const std::string &message) {
        type_error(expr.loc, std::format("{} (left is '{}', right is '{}')", message, get_type_str(left_type), get_type_str(right_type)));
    };

    switch (expr.op) {
    case lex::TokenType::Pipe:
    case lex::TokenType::BitXor:
    case lex::TokenType::BitAnd:
    case lex::TokenType::BitLShift:
    case lex::TokenType::BitRshift:
        if (!types::is_primitive(left_type) || !types::is_primitive(right_type)
            || !types::is_integer_primitive(types::get_primitive_kind(left_type))
            || !types::is_integer_primitive(types::get_primitive_kind(right_type))) {
            report_error("Bitwise operators require integer operands.");
            return expr.type = types::Primitive_kind::Void;
        }
        expr.type = promote_numeric_type(left_type, right_type);
        if (expr.type == types::Type(types::Primitive_kind::Any)) {
            report_error("Bitwise operators require integers from the same signedness family.");
            return expr.type = types::Primitive_kind::Void;
        }
        return expr.type;
    case lex::TokenType::Plus:
        if (is_string(left_type) && is_string(right_type))
            return expr.type = types::Primitive_kind::String;
        if (is_numeric(left_type) && is_numeric(right_type)) {
            expr.type = promote_numeric_type(left_type, right_type);
            if (expr.type == types::Type(types::Primitive_kind::Any)) {
                report_error("Mixed signed and unsigned integer arithmetic requires an explicit cast.");
                return expr.type = types::Primitive_kind::Void;
            }
            return expr.type;
        }

        report_error("Operands must be two numbers or two strings for '+'");
        return expr.type = types::Primitive_kind::Void;

    case lex::TokenType::Minus:
    case lex::TokenType::Star:
        if (!is_numeric(left_type) || !is_numeric(right_type)) {
            report_error("Operands must be two numbers for this operator.");
            return expr.type = types::Primitive_kind::Void;
        }
        expr.type = promote_numeric_type(left_type, right_type);
        if (expr.type == types::Type(types::Primitive_kind::Any)) {
            report_error("Mixed signed and unsigned integer arithmetic requires an explicit cast.");
            return expr.type = types::Primitive_kind::Void;
        }
        return expr.type;

    case lex::TokenType::Slash:
        if (!is_numeric(left_type) || !is_numeric(right_type)) {
            report_error("Operands for division must be numbers.");
            return expr.type = types::Primitive_kind::Void;
        }
        if (types::is_primitive(left_type) && types::is_primitive(right_type)) {
            auto left_kind = types::get_primitive_kind(left_type);
            auto right_kind = types::get_primitive_kind(right_type);

            if (types::is_float_primitive(left_kind) || types::is_float_primitive(right_kind))
                return expr.type = promote_numeric_type(left_type, right_type);
        }
        return expr.type = types::Primitive_kind::F64;

    case lex::TokenType::Percent:
        if (!types::is_primitive(left_type) || !types::is_primitive(right_type)
            || !types::is_integer_primitive(types::get_primitive_kind(left_type))
            || !types::is_integer_primitive(types::get_primitive_kind(right_type))) {
            report_error("Operands for '%' must be integers.");
            return expr.type = types::Primitive_kind::Void;
        }
        expr.type = promote_numeric_type(left_type, right_type);
        if (expr.type == types::Type(types::Primitive_kind::Any)) {
            report_error("Mixed signed and unsigned integer modulo requires an explicit cast.");
            return expr.type = types::Primitive_kind::Void;
        }
        return expr.type;
    case lex::TokenType::Greater:
    case lex::TokenType::GreaterEqual:
    case lex::TokenType::Less:
    case lex::TokenType::LessEqual:
        if (!is_numeric(left_type) || !is_numeric(right_type))
            report_error("Comparison operators require numeric operands.");
        else if (promote_numeric_type(left_type, right_type) == types::Type(types::Primitive_kind::Any))
            report_error("Mixed signed and unsigned numeric comparison requires an explicit cast.");
        return expr.type = types::Primitive_kind::Bool;
    case lex::TokenType::Equal:
    case lex::TokenType::NotEqual:
        if ((is_optional(left_type) && is_nil(right_type)) || (is_nil(left_type) && is_optional(right_type)))
            return expr.type = types::Primitive_kind::Bool;
        if (!is_compatible(left_type, right_type) && !is_compatible(right_type, left_type))
            report_error("Cannot compare incompatible types. Remember to use 'as' for enum casting.");
        return expr.type = types::Primitive_kind::Bool;
    case lex::TokenType::LogicalAnd:
    case lex::TokenType::LogicalOr:
        if (!is_boolean(left_type) || !is_boolean(right_type))
            report_error("Operands for logical operators must be booleans.");
        return expr.type = types::Primitive_kind::Bool;

    default:
        type_error(expr.loc, "Unsupported binary operator.");
        return expr.type = types::Primitive_kind::Void;
    }
}

Result<types::Type> Type_checker::check_expr_node(ast::Call_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto has_named_arguments = [&]() {
        return std::any_of(expr.arguments.begin(), expr.arguments.end(), [](const auto &arg) { return !arg.name.empty(); });
    };

    if (auto *static_path = std::get_if<ast::Static_path_expr>(&expr.callee->node)) {
        auto base_type_res = check_expr(*static_path->base);
        if (!base_type_res)
            return base_type_res;

        if (is_union(*base_type_res)) {
            auto union_type = types::get_union_type(*base_type_res);
            auto &variant_name = static_path->member.lexeme;

            if (!contains_key(union_type->variants, variant_name))
                type_error(static_path->loc, "Union '" + union_type->name + "' has no variant named '" + variant_name + "'.");

            for (const auto &arg : expr.arguments)
                if (arg.value)
                    std::ignore = check_expr(*arg.value);

            type_error(
                expr.loc,
                "Union construction via '::' is not supported. Use '" + union_type->name + "{ " + variant_name + ": ... }' or '.{ "
                    + variant_name + ": ... }' instead");
            return expr.type = types::Primitive_kind::Void;
        } else if (is_model(*base_type_res)) {
            auto model_type = types::get_model_type(*base_type_res);
            auto &method_name = static_path->member.lexeme;

            if (!contains_key(model_type->static_methods, method_name)) {
                type_error(static_path->loc, "Model '" + model_type->name + "' has no static method named '" + method_name + "'.");
                return expr.type = types::Primitive_kind::Void;
            }

            const auto &signature = find_by_key(model_type->static_methods, method_name)->second;
            const auto &method_decl = model_data.at(model_type->name).static_methods.at(method_name).declaration;
            auto bound = bind_call_arguments(method_decl->parameters, expr.arguments, expr.loc, "static method", method_name);
            expr.arguments = std::move(bound.ordered_arguments);

            if (!bound.ok)
                return expr.type = signature.return_type;

            if (expr.arguments.size() != signature.parameter_types.size()) {
                type_error(
                    expr.loc,
                    std::format(
                        "Incorrect number of arguments for static method '{}'. Expected {}, but got {}.",
                        method_name,
                        signature.parameter_types.size(),
                        expr.arguments.size()));
            } else {
                for (size_t i = 0; i < expr.arguments.size(); ++i) {
                    auto arg_type_res = check_expr(*expr.arguments[i].value, signature.parameter_types[i]);
                    if (arg_type_res && !is_compatible(signature.parameter_types[i], *arg_type_res))
                        type_error(ast::get_loc(expr.arguments[i].value->node), "Argument type mismatch for static method.");
                }
            }
            return expr.type = signature.return_type;
        }
    }

    if (auto *var_callee = std::get_if<ast::Variable_expr>(&expr.callee->node)) {
        if (var_callee->name == "iter") {
            if (has_named_arguments())
                type_error(expr.loc, "iter() does not support named arguments.");
            if (expr.arguments.size() != 1) {
                type_error(expr.loc, "iter() expects exactly one argument.");
                return expr.type = types::Primitive_kind::Void;
            }

            auto arg_type_res = check_expr(*expr.arguments[0].value);
            if (!arg_type_res)
                return expr.type = types::Primitive_kind::Void;

            auto iter_type = to_iterator_type(*arg_type_res);
            if (!is_iterator_protocol_type(iter_type)) {
                type_error(expr.loc, "Value passed to iter() is not iterable.");
                return expr.type = types::Primitive_kind::Void;
            }
            return expr.type = iter_type;
        }

        if (var_callee->name == "clone") {
            if (has_named_arguments())
                type_error(expr.loc, "clone() does not support named arguments.");
            if (expr.arguments.size() != 1) {
                type_error(expr.loc, "clone() expects exactly one argument.");
                return expr.type = types::Primitive_kind::Void;
            }

            auto arg_type_res = check_expr(*expr.arguments[0].value);
            if (!arg_type_res)
                return expr.type = types::Primitive_kind::Void;
            return expr.type = *arg_type_res;
        }
    }

    // --- FFI SIGNATURE MATCHER ---
    if (auto *var_callee = std::get_if<ast::Variable_expr>(&expr.callee->node)) {
        if (native_signatures.contains(var_callee->name)) {
            const auto &signatures = native_signatures.at(var_callee->name);

            for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
                const auto &sig = signatures[sig_index];
                auto bound = try_bind_native_arguments(sig.params, expr.arguments);
                if (bound.ok) {
                    expr.arguments = std::move(bound.ordered_arguments);
                    expr.native_signature_index = static_cast<int>(sig_index);

                    if (sig.ret_type.length() == 1 && std::isupper(sig.ret_type[0]) && bound.generics.contains(sig.ret_type))
                        return expr.type = bound.generics[sig.ret_type];
                    return expr.type = parse_type_string(sig.ret_type);
                }
            }

            // --- HIGH QUALITY ERROR REPORTING ---
            std::string expected_signatures;
            for (size_t i = 0; i < signatures.size(); ++i) {
                const auto &sig = signatures[i];
                expected_signatures += "(";

                // Start at 0 because standalone functions don't have a hidden 'this' parameter
                for (size_t j = 0; j < sig.params.size(); ++j) {
                    if (!sig.params[j].name.empty())
                        expected_signatures += sig.params[j].name + ": ";
                    expected_signatures += sig.params[j].type;
                    if (sig.params[j].default_value.has_value())
                        expected_signatures += " = ...";
                    if (j < sig.params.size() - 1)
                        expected_signatures += ", ";
                }
                expected_signatures += ")";

                if (i < signatures.size() - 1)
                    expected_signatures += " OR ";
            }

            std::string error_str = std::format(
                "Arguments do not match any native signature for function '{}'.\n    Expected: {}{}",
                var_callee->name,
                var_callee->name,
                expected_signatures);

            if (expected_signatures.find('T') != std::string::npos)
                error_str += "\n    (Note: 'T' represents the generic element type. For example, in an 'i64[]', T is 'i64'.)";

            type_error(expr.loc, error_str);
            return expr.type = types::Primitive_kind::Void;
            // ------------------------------------
        }
    }
    // -----------------------------

    auto callee_type_res = check_expr(*expr.callee);
    if (!callee_type_res)
        return std::unexpected(callee_type_res.error());

    types::Type callee_type = callee_type_res.value();
    const types::Function_type *signature = nullptr;
    const ast::Function_stmt *declaration = nullptr;

    if (types::is_function(callee_type))
        signature = types::get_function_type(callee_type).get();

    if (auto *var_callee = std::get_if<ast::Variable_expr>(&expr.callee->node)) {
        if (functions.contains(var_callee->name))
            declaration = functions.at(var_callee->name).declaration;
    }

    if (!signature) {
        type_error(
            ast::get_loc(expr.callee->node),
            "This expression has type '" + types::type_to_string(callee_type) + "' and cannot be called.");
        return expr.type = types::Primitive_kind::Void;
    }

    if (declaration) {
        auto bound = bind_call_arguments(declaration->parameters, expr.arguments, expr.loc, "function", declaration->name);
        expr.arguments = std::move(bound.ordered_arguments);
        if (!bound.ok)
            return expr.type = signature->return_type;
    } else if (has_named_arguments()) {
        type_error(expr.loc, "Named arguments are only supported for direct calls to declared functions and static methods.");
        return expr.type = signature->return_type;
    }

    if (expr.arguments.size() != signature->parameter_types.size()) {
        type_error(
            expr.loc,
            std::format(
                "Incorrect number of arguments. Expected {}, but got {}.",
                signature->parameter_types.size(),
                expr.arguments.size()));
    } else {
        for (size_t i = 0; i < expr.arguments.size(); ++i) {
            auto arg_type_res = check_expr(*expr.arguments[i].value, signature->parameter_types[i]);
            if (arg_type_res && !is_compatible(signature->parameter_types[i], arg_type_res.value())) {
                type_error(
                    ast::get_loc(expr.arguments[i].value->node),
                    std::format(
                        "Argument type mismatch. Expected '{}' but got '{}'.",
                        types::type_to_string(signature->parameter_types[i]),
                        types::type_to_string(arg_type_res.value())));
            }
        }
    }

    return expr.type = signature->return_type;
}

Result<types::Type> Type_checker::check_expr_node(ast::Cast_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto original_type_res = check_expr(*expr.expression);
    if (!original_type_res)
        return std::unexpected(original_type_res.error());

    auto original_type = original_type_res.value();
    auto &target_type = expr.target_type;

    if (auto *target_array = std::get_if<mem::rc_ptr<types::Array_type>>(&target_type)) {
        if ((*target_array)->element_type == types::Type(types::Primitive_kind::U8)) {
            bool ok = false;

            if (original_type == types::Type(types::Primitive_kind::String)) {
                ok = true;
            } else if (auto *source_array = std::get_if<mem::rc_ptr<types::Array_type>>(&original_type)) {
                auto elem = (*source_array)->element_type;
                if (elem == types::Type(types::Primitive_kind::Bool))
                    ok = true;
                else if (types::is_primitive(elem) && types::is_integer_primitive(types::get_primitive_kind(elem)))
                    ok = true;
            }

            if (!ok) {
                type_error(expr.loc, "Casting to 'u8[]' is only supported from 'string', integer arrays, bool arrays, or 'u8[]'.");
                return types::Primitive_kind::Void;
            }

            return expr.target_type;
        }
    }

    // --- NEW ENUM CASTING ---
    if (is_enum(original_type)) {
        auto enum_t = types::get_enum_type(original_type);
        if (target_type == enum_t->base_type)
            return expr.target_type;
    }
    if (is_enum(target_type)) {
        auto enum_t = types::get_enum_type(target_type);
        if (original_type == enum_t->base_type)
            return expr.target_type;
    }
    // ------------------------

    if (is_optional(original_type) && !is_optional(target_type)) {
        if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr.expression->node)) {
            bool is_checked = false;
            for (const auto &scope : m_nil_checked_vars_stack) {
                if (scope.count(var_expr->name)) {
                    is_checked = true;
                    break;
                }
            }
            if (!is_checked) {
                type_error(
                    expr.loc,
                    "Cannot cast optional type '" + types::type_to_string(original_type) + "' to non-optional type '"
                        + types::type_to_string(target_type) + "' without a nil check.");
            }
        } else {
            type_error(expr.loc, "Cannot cast an optional expression to a non-optional type. Use a temporary variable and an 'if' check.");
        }
    }
    if (is_nil(original_type) && !is_optional(target_type))
        type_error(expr.loc, "Cannot cast a 'nil' value to a non-optional type.");
    return expr.target_type;
}

Result<types::Type> Type_checker::check_expr_node(ast::Closure_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto saved_ret = current_return_type;
    current_return_type = expr.return_type;

    begin_scope();
    for (auto const &p : expr.parameters)
        declare(p.name, p.type, !p.is_mut, expr.loc);
    if (expr.body)
        check_stmt(*expr.body);
    end_scope();

    current_return_type = saved_ret;

    auto ct = mem::make_rc<types::Function_type>();
    for (auto const &p : expr.parameters)
        ct->parameter_types.push_back(p.type);
    ct->return_type = expr.return_type;

    return expr.type = types::Type(ct);
}

Result<types::Type> Type_checker::check_expr_node(ast::Field_access_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto obj_type_res = check_expr(*expr.object);
    if (!obj_type_res)
        return expr.type = types::Primitive_kind::Void;

    auto obj_type = obj_type_res.value();
    if (is_optional(obj_type)) {
        type_error(expr.loc, "Cannot access field on an optional type. Use an 'if' check to unwrap it first.");
        return expr.type = types::Primitive_kind::Void;
    }

    if (is_union(obj_type)) {
        auto union_type = types::get_union_type(obj_type);
        if (!contains_key(union_type->variants, expr.field_name)) {
            type_error(expr.loc, "Union '" + union_type->name + "' has no variant named '" + expr.field_name + "'");
            return expr.type = types::Primitive_kind::Void;
        }
        return expr.type = find_by_key(union_type->variants, expr.field_name)->second;
    }

    if (auto *mt = std::get_if<mem::rc_ptr<types::Model_type>>(&obj_type)) {
        if (contains_key((*mt)->fields, expr.field_name))
            return expr.type = find_by_key((*mt)->fields, expr.field_name)->second;

        if (contains_key((*mt)->methods, expr.field_name)) {
            const auto &method = find_by_key((*mt)->methods, expr.field_name)->second;
            auto ft = mem::make_rc<types::Function_type>();
            ft->parameter_types = method.parameter_types;
            ft->return_type = method.return_type;
            return expr.type = types::Type(ft);
        }
        type_error(expr.loc, "Model '" + (*mt)->name + "' has no member '" + expr.field_name + "'");
    } else
        type_error(expr.loc, "Can only access fields on model instances");

    return expr.type = types::Primitive_kind::Void;
}

Result<types::Type> Type_checker::check_expr_node(ast::Static_path_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto base_res = check_expr(*expr.base);
    if (!base_res)
        return base_res;

    if (is_union(*base_res)) {
        auto union_type = types::get_union_type(*base_res);
        if (!contains_key(union_type->variants, expr.member.lexeme)) {
            type_error(expr.loc, "Union '" + union_type->name + "' has no variant '" + expr.member.lexeme + "'.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto variant_type = find_by_key(union_type->variants, expr.member.lexeme)->second;
        if (variant_type != types::Type(types::Primitive_kind::Void)) {
            type_error(
                expr.loc,
                "Union payload variants are not first-class constructors. Use '" + union_type->name + "{ " + expr.member.lexeme
                    + ": ... }' or '.{ " + expr.member.lexeme + ": ... }' instead");
            return expr.type = types::Primitive_kind::Void;
        }

        return expr.type = *base_res;
    } else if (is_model(*base_res)) {
        auto model_type = types::get_model_type(*base_res);
        if (!contains_key(model_type->static_methods, expr.member.lexeme)) {
            type_error(expr.loc, "Model '" + model_type->name + "' has no static method '" + expr.member.lexeme + "'.");
            return expr.type = types::Primitive_kind::Void;
        }
        return expr.type = find_by_key(model_type->static_methods, expr.member.lexeme)->second.return_type;
    } else if (is_enum(*base_res)) {
        auto enum_type = types::get_enum_type(*base_res);
        if (!enum_type->variants->map.contains(expr.member.lexeme)) // Access via wrapper map
        {
            type_error(expr.loc, "Enum '" + enum_type->name + "' has no variant '" + expr.member.lexeme + "'.");
            return expr.type = types::Primitive_kind::Void;
        }
        return expr.type = types::Type(enum_type);
    }

    type_error(expr.loc, "Scope resolution operator '::' is only supported for union variants, static model methods, and enum variants.");
    return expr.type = types::Primitive_kind::Void;
}

Result<types::Type> Type_checker::check_expr_node(ast::Enum_member_expr &expr, std::optional<types::Type> context_type)
{
    if (!context_type) {
        type_error(expr.loc, "Cannot infer enum type for '." + expr.member_name + "'. Context is missing.");
        return expr.type = types::Primitive_kind::Void;
    }

    types::Type expected_type = *context_type;
    if (is_optional(expected_type))
        expected_type = types::get_optional_type(expected_type)->base_type;

    if (!is_enum(expected_type)) {
        type_error(expr.loc, "Cannot infer enum type for '." + expr.member_name + "'. Context is missing.");
        return expr.type = types::Primitive_kind::Void;
    }

    auto enum_type = types::get_enum_type(expected_type);

    if (!enum_type->variants->map.contains(expr.member_name)) // Access via wrapper map
    {
        type_error(expr.loc, "Enum '" + enum_type->name + "' has no variant '" + expr.member_name + "'.");
        return expr.type = types::Primitive_kind::Void;
    }

    return expr.type = context_type.value();
}

Result<types::Type> Type_checker::check_expr_node(ast::Field_assignment_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto field_access_temp = ast::Field_access_expr{expr.object, expr.field_name, {}, expr.loc};
    auto field_type_res = check_expr_node(field_access_temp, std::nullopt);
    expr.object = field_access_temp.object;
    if (!field_type_res)
        return expr.type = types::Primitive_kind::Void;

    auto val_type_res = check_expr(*expr.value, field_type_res.value());
    if (!val_type_res)
        return expr.type = types::Primitive_kind::Void;

    if (!is_compatible(field_type_res.value(), val_type_res.value()))
        type_error(expr.loc, "Assignment type mismatch for field");

    return expr.type = field_type_res.value();
}

Result<types::Type> Type_checker::check_expr_node(ast::Literal_expr &expr, std::optional<types::Type> context_type)
{
    if (std::holds_alternative<std::nullptr_t>(expr.value.payload))
        return expr.type = types::Primitive_kind::Nil;

    if (context_type && types::is_primitive(*context_type)) {
        auto target_kind = types::get_primitive_kind(*context_type);
        if (types::is_numeric_primitive(target_kind) && is_numeric(expr.type)) {
            auto coerced = coerce_numeric_literal(expr.value, target_kind);
            if (coerced) {
                expr.value = coerced.value();
                return expr.type = *context_type;
            }
        }
    }

    return expr.type;
}

Result<types::Type> Type_checker::check_expr_node(ast::Method_call_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto has_named_arguments = [&]() {
        return std::any_of(expr.arguments.begin(), expr.arguments.end(), [](const auto &arg) { return !arg.name.empty(); });
    };
    auto obj_type_res = check_expr(*expr.object);
    if (!obj_type_res)
        return expr.type = types::Primitive_kind::Void;
    auto obj_type = obj_type_res.value();

    if (is_union(obj_type)) {
        auto union_type = types::get_union_type(obj_type);
        if (expr.method_name == "has") {
            if (expr.arguments.size() != 1) {
                type_error(expr.loc, ".has() expects exactly one argument.");
                return expr.type = types::Primitive_kind::Bool;
            }

            auto *arg_expr = expr.arguments[0].value;
            if (auto *static_path = std::get_if<ast::Static_path_expr>(&arg_expr->node)) {
                if (auto *var_expr = std::get_if<ast::Variable_expr>(&static_path->base->node)) {
                    if (var_expr->name != union_type->name)
                        type_error(
                            ast::get_loc(arg_expr->node),
                            "Argument to .has() must be a variant of the same union type '" + union_type->name + "'");

                    if (!contains_key(union_type->variants, static_path->member.lexeme))
                        type_error(
                            ast::get_loc(arg_expr->node),
                            "Union '" + union_type->name + "' has no variant named '" + static_path->member.lexeme + "'");
                } else
                    type_error(ast::get_loc(arg_expr->node), "Argument to .has() must be a static-like variant access.");
            } else
                type_error(ast::get_loc(arg_expr->node), "Argument to .has() must be a static-like variant access.");

            return expr.type = types::Primitive_kind::Bool;
        } else {
            type_error(expr.loc, "Union types only support the '.has()' method.");
            return expr.type = types::Primitive_kind::Void;
        }
    }

    if (is_iterator(obj_type)) {
        auto element_type = types::get_iterator_type(obj_type)->element_type;
        auto optional_element = types::Type(mem::make_rc<types::Optional_type>(element_type));

        if (expr.method_name == "get") {
            if (!expr.arguments.empty())
                type_error(expr.loc, "'get()' does not take any arguments.");
            return expr.type = optional_element;
        }

        if (expr.method_name == "next" || expr.method_name == "prev") {
            if (expr.arguments.size() > 1) {
                type_error(expr.loc, "'" + expr.method_name + "()' accepts at most one integer step argument.");
            } else if (expr.arguments.size() == 1) {
                if (!expr.arguments[0].name.empty() && expr.arguments[0].name != "step")
                    type_error(
                        expr.arguments[0].loc,
                        "Unknown named argument '" + expr.arguments[0].name + "' for " + expr.method_name + "().");

                auto arg_type = check_expr(*expr.arguments[0].value);
                if (arg_type && (!types::is_primitive(*arg_type) || !types::is_integer_primitive(types::get_primitive_kind(*arg_type))))
                    type_error(ast::get_loc(expr.arguments[0].value->node), "Iterator step must be an integer.");
            }
            return expr.type = optional_element;
        }

        if (expr.method_name == "has_next" || expr.method_name == "has_prev") {
            if (expr.arguments.size() > 1) {
                type_error(expr.loc, "'" + expr.method_name + "()' accepts at most one integer step argument.");
            } else if (expr.arguments.size() == 1) {
                if (!expr.arguments[0].name.empty() && expr.arguments[0].name != "step")
                    type_error(
                        expr.arguments[0].loc,
                        "Unknown named argument '" + expr.arguments[0].name + "' for " + expr.method_name + "().");

                auto arg_type = check_expr(*expr.arguments[0].value);
                if (arg_type && (!types::is_primitive(*arg_type) || !types::is_integer_primitive(types::get_primitive_kind(*arg_type))))
                    type_error(ast::get_loc(expr.arguments[0].value->node), "Iterator step must be an integer.");
            }
            return expr.type = types::Primitive_kind::Bool;
        }

        type_error(expr.loc, "Iterator type has no method named '" + expr.method_name + "'.");
        return expr.type = types::Primitive_kind::Void;
    }

    // --- FFI METHOD MATCHER ---
    std::string ffi_name;
    if (is_array(obj_type))
        ffi_name = "Array::" + expr.method_name;
    if (is_string(obj_type))
        ffi_name = "string::" + expr.method_name;
    if (is_optional(obj_type))
        ffi_name = "Optional::" + expr.method_name;

    if (is_optional(obj_type) && expr.method_name == "or_else") {
        if (expr.arguments.size() != 1) {
            type_error(expr.loc, "or_else() expects exactly one closure argument.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto closure_type_res = check_expr(*expr.arguments[0].value);
        if (!closure_type_res || !is_function(*closure_type_res)) {
            type_error(ast::get_loc(expr.arguments[0].value->node), "Argument to 'or_else' must be a closure.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto closure_type = types::get_function_type(*closure_type_res);
        auto base_type = types::get_optional_type(obj_type)->base_type;
        if (!closure_type->parameter_types.empty())
            type_error(ast::get_loc(expr.arguments[0].value->node), "Closure passed to 'or_else' must take no arguments.");
        if (!is_compatible(base_type, closure_type->return_type))
            type_error(
                ast::get_loc(expr.arguments[0].value->node),
                "Closure return type for 'or_else' must be '" + types::type_to_string(base_type) + "'.");
        return expr.type = base_type;
    }

    if (!ffi_name.empty() && native_signatures.contains(ffi_name)) {
        const auto &signatures = native_signatures.at(ffi_name);

        for (size_t sig_index = 0; sig_index < signatures.size(); ++sig_index) {
            const auto &sig = signatures[sig_index];
            auto bound = try_bind_native_arguments(sig.params, expr.arguments, obj_type);
            if (bound.ok) {
                expr.arguments = std::move(bound.ordered_arguments);
                expr.native_signature_index = static_cast<int>(sig_index);

                if (sig.ret_type.length() == 1 && std::isupper(sig.ret_type[0]) && bound.generics.contains(sig.ret_type))
                    return expr.type = bound.generics[sig.ret_type];
                return expr.type = parse_type_string(sig.ret_type);
            }
        }

        // --- HIGH QUALITY ERROR FORMATTING ---
        std::string expected_signatures;
        for (size_t i = 0; i < signatures.size(); ++i) {
            const auto &sig = signatures[i];
            expected_signatures += "(";

            // Start at j = 1 to hide the implicit 'this' object from the user!
            for (size_t j = 1; j < sig.params.size(); ++j) {
                if (!sig.params[j].name.empty())
                    expected_signatures += sig.params[j].name + ": ";
                expected_signatures += sig.params[j].type;
                if (sig.params[j].default_value.has_value())
                    expected_signatures += " = ...";
                if (j < sig.params.size() - 1)
                    expected_signatures += ", ";
            }
            expected_signatures += ")";

            if (i < signatures.size() - 1)
                expected_signatures += " OR ";
        }

        std::string error_str = std::format(
            "Arguments do not match any native signature for method '{}'.\n    Expected: {}",
            expr.method_name,
            expected_signatures);

        if (expected_signatures.find('T') != std::string::npos)
            error_str += "\n    (Note: 'T' represents the generic element type. For example, in an 'i64[]', T is 'i64'.)";

        type_error(expr.loc, error_str);
        return expr.type = types::Primitive_kind::Void;
    }
    // --------------------------

    if (expr.method_name == "map") {
        if (expr.arguments.size() != 1) {
            type_error(expr.loc, "Map must have one and only one closure argument.");
            return expr.type = types::Primitive_kind::Void;
        }
        auto closure_type_res = check_expr(*expr.arguments[0].value);
        if (!closure_type_res || !is_function(closure_type_res.value())) {
            type_error(ast::get_loc(expr.arguments[0].value->node), "Argument to 'map' must be a closure.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto closure_type = types::get_function_type(closure_type_res.value());

        if (is_array(obj_type)) {
            auto element_type = types::get_array_type(obj_type)->element_type;
            if (closure_type->parameter_types.size() != 1 || !is_compatible(closure_type->parameter_types[0], element_type))
                type_error(
                    ast::get_loc(expr.arguments[0].value->node),
                    "Closure parameter type is not compatible with array element type.");
            return expr.type = types::Type(mem::make_rc<types::Array_type>(closure_type->return_type));
        }

        if (is_optional(obj_type)) {
            auto base_type = types::get_optional_type(obj_type)->base_type;
            if (closure_type->parameter_types.size() != 1 || !is_compatible(closure_type->parameter_types[0], base_type))
                type_error(
                    ast::get_loc(expr.arguments[0].value->node),
                    "Closure parameter type is not compatible with optional's base type.");
            return expr.type = types::Type(mem::make_rc<types::Optional_type>(closure_type->return_type));
        }

        type_error(expr.loc, ".map() can only be called on arrays and optionals.");
        return expr.type = types::Primitive_kind::Void;
    }

    if (is_optional(obj_type)) {
        type_error(expr.loc, "Cannot call method on an optional type. Use an 'if' check to unwrap it first.");
        return expr.type = types::Primitive_kind::Void;
    }

    if (auto *mt = std::get_if<mem::rc_ptr<types::Model_type>>(&obj_type)) {
        if (contains_key((*mt)->methods, expr.method_name)) {
            const auto &method_signature = find_by_key((*mt)->methods, expr.method_name)->second;
            const auto &method_decl = model_data.at((*mt)->name).methods.at(expr.method_name).declaration;
            auto bound = bind_call_arguments(method_decl->parameters, expr.arguments, expr.loc, "method", expr.method_name);
            expr.arguments = std::move(bound.ordered_arguments);

            if (!bound.ok)
                return expr.type = method_signature.return_type;

            if (expr.arguments.size() != method_signature.parameter_types.size()) {
                type_error(
                    expr.loc,
                    std::format(
                        "Incorrect number of arguments for method '{}'. Expected {}, but got {}.",
                        expr.method_name,
                        method_signature.parameter_types.size(),
                        expr.arguments.size()));
            } else {
                for (size_t i = 0; i < expr.arguments.size(); ++i) {
                    auto arg_res = check_expr(*expr.arguments[i].value, method_signature.parameter_types[i]);
                    if (arg_res && !is_compatible(method_signature.parameter_types[i], *arg_res))
                        type_error(
                            ast::get_loc(expr.arguments[i].value->node),
                            "Argument type mismatch for method '" + expr.method_name + "'.");
                }
            }
            return expr.type = method_signature.return_type;
        } else {
            for (size_t i = 0; i < (*mt)->fields.size(); ++i) {
                if ((*mt)->fields[i].first == expr.method_name) {
                    auto field_type = (*mt)->fields[i].second;

                    if (auto *func_type = std::get_if<mem::rc_ptr<types::Function_type>>(&field_type)) {
                        expr.is_closure_field = true;
                        expr.field_index = static_cast<uint8_t>(i);

                        if (expr.arguments.size() != (*func_type)->parameter_types.size()) {
                            type_error(
                                expr.loc,
                                std::format(
                                    "Incorrect number of arguments for closure field '{}'. Expected {}, but got {}.",
                                    expr.method_name,
                                    (*func_type)->parameter_types.size(),
                                    expr.arguments.size()));
                        } else {
                            for (size_t j = 0; j < expr.arguments.size(); ++j) {
                                auto arg_res = check_expr(*expr.arguments[j].value, (*func_type)->parameter_types[j]);
                                if (arg_res && !is_compatible((*func_type)->parameter_types[j], *arg_res))
                                    type_error(
                                        ast::get_loc(expr.arguments[j].value->node),
                                        "Argument type mismatch for closure field '" + expr.method_name + "'.");
                            }
                        }
                        return expr.type = (*func_type)->return_type;
                    }
                }
            }
        }
    }

    type_error(
        expr.loc,
        "No method or closure field named '" + expr.method_name + "' found for type '" + types::type_to_string(obj_type) + "'.");
    return expr.type = types::Primitive_kind::Void;
}

Result<types::Type> Type_checker::check_expr_node(ast::Unary_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto right_type_res = check_expr(*expr.right);
    if (!right_type_res)
        return expr.type = types::Primitive_kind::Void;
    auto right_type = right_type_res.value();

    switch (expr.op) {
    case lex::TokenType::Minus:
        if (!is_numeric(right_type))
            type_error(expr.loc, "Operand for '-' must be a number");
        else if (types::is_primitive(right_type) && types::is_unsigned_integer_primitive(types::get_primitive_kind(right_type)))
            type_error(expr.loc, "Operand for unary '-' cannot be an unsigned integer. Cast to a signed type first.");
        return expr.type = right_type;
    case lex::TokenType::LogicalNot:
        if (!is_boolean(right_type))
            type_error(expr.loc, "Operand for '!' must be a boolean");
        return expr.type = types::Primitive_kind::Bool;
    case lex::TokenType::BitNot:
        if (!types::is_primitive(right_type) || !types::is_integer_primitive(types::get_primitive_kind(right_type)))
            type_error(expr.loc, "Operand for '~' must be an integer.");
        return expr.type = right_type;
    default:
        type_error(expr.loc, "Unsupported unary operator");
        return expr.type = types::Primitive_kind::Void;
    }
}

Result<types::Type> Type_checker::check_expr_node(ast::Array_literal_expr &expr, std::optional<types::Type> context_type)
{
    if (expr.elements.empty()) {
        if (context_type && is_array(context_type.value()))
            return expr.type = context_type.value();
        type_error(
            expr.loc,
            "Cannot infer the element type of an empty array. Please provide an explicit type annotation (e.g., 'let arr: i64[] = []').");
        return expr.type = types::Type(mem::make_rc<types::Array_type>(types::Type(types::Primitive_kind::Void)));
    }

    types::Type common_type = types::Primitive_kind::Nil;
    bool has_nil = false;

    std::optional<types::Type> element_context = std::nullopt;
    if (context_type && is_array(context_type.value()))
        element_context = types::get_array_type(context_type.value())->element_type;

    for (const auto &elem_expr : expr.elements) {
        auto elem_type_res = check_expr(*elem_expr, element_context);
        if (!elem_type_res)
            continue;

        auto elem_type = elem_type_res.value();
        if (is_nil(elem_type)) {
            has_nil = true;
        } else if (is_nil(common_type)) {
            common_type = elem_type;
        } else {
            // Unwrap optionals for comparison
            auto unwrapped_common = is_optional(common_type) ? types::get_optional_type(common_type)->base_type : common_type;
            auto unwrapped_elem = is_optional(elem_type) ? types::get_optional_type(elem_type)->base_type : elem_type;

            if (!is_compatible(unwrapped_common, unwrapped_elem) && !is_compatible(unwrapped_elem, unwrapped_common)) {
                type_error(ast::get_loc(elem_expr->node), "Array elements must have a consistent type.");
                return expr.type = types::Type(mem::make_rc<types::Array_type>(types::Primitive_kind::Void));
            }

            // If either was optional, ensure the common type becomes optional
            if (is_optional(elem_type) && !is_optional(common_type))
                common_type = types::Type(mem::make_rc<types::Optional_type>(common_type));
        }
    }

    if (is_nil(common_type)) {
        if (context_type && is_array(context_type.value())) {
            return expr.type = context_type.value();
        } else {
            type_error(expr.loc, "Cannot infer type of an array containing only 'nil'. Please provide an explicit type annotation.");
            return expr.type = types::Type(mem::make_rc<types::Array_type>(types::Primitive_kind::Void));
        }
    }

    if (has_nil && !is_optional(common_type))
        common_type = types::Type(mem::make_rc<types::Optional_type>(common_type));

    if (context_type && is_array(context_type.value())) {
        auto ctx_element_type = types::get_array_type(context_type.value())->element_type;
        if (is_optional(ctx_element_type) && !is_optional(common_type)) {
            if (is_compatible(ctx_element_type, common_type))
                common_type = ctx_element_type;
        }
    }

    return expr.type = types::Type(mem::make_rc<types::Array_type>(common_type));
}

Result<types::Type> Type_checker::check_expr_node(ast::Array_access_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto array_type_res = check_expr(*expr.array);
    if (!array_type_res)
        return expr.type = types::Primitive_kind::Void;
    if (!is_array(array_type_res.value())) {
        type_error(expr.loc, "Subscript operator '[]' can only be used on arrays.");
        return expr.type = types::Primitive_kind::Void;
    }

    auto index_type_res = check_expr(*expr.index);
    if (index_type_res
        && (!types::is_primitive(index_type_res.value())
            || !types::is_integer_primitive(types::get_primitive_kind(index_type_res.value()))))
        type_error(ast::get_loc(expr.index->node), "Array index must be an integer.");

    const auto &array_type = std::get<mem::rc_ptr<types::Array_type>>(array_type_res.value());
    return expr.type = array_type->element_type;
}

Result<types::Type> Type_checker::check_expr_node(ast::Array_assignment_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto array_type_res = check_expr(*expr.array);
    if (!array_type_res)
        return expr.type = types::Primitive_kind::Void;
    if (!is_array(array_type_res.value())) {
        type_error(expr.loc, "Subscript operator '[]' can only be used on arrays for assignment.");
        return expr.type = types::Primitive_kind::Void;
    }

    auto index_type_res = check_expr(*expr.index);
    if (index_type_res
        && (!types::is_primitive(index_type_res.value())
            || !types::is_integer_primitive(types::get_primitive_kind(index_type_res.value()))))
        type_error(ast::get_loc(expr.index->node), "Array index must be an integer.");

    const auto &array_type = std::get<mem::rc_ptr<types::Array_type>>(array_type_res.value());

    auto value_type_res = check_expr(*expr.value, array_type->element_type);
    if (!value_type_res)
        return expr.type = types::Primitive_kind::Void;

    if (!is_compatible(array_type->element_type, value_type_res.value()))
        type_error(ast::get_loc(expr.value->node), "Type of value being assigned does not match array's element type.");

    return expr.type = value_type_res.value();
}

Result<types::Type> Type_checker::check_expr_node(ast::Model_literal_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type; // Unused parameter
    auto type_res = lookup(expr.model_name, expr.loc);
    if (!type_res)
        return expr.type = types::Primitive_kind::Void;

    auto resolved_type = type_res.value().first;

    // --- 1. UNION LITERAL SYNTAX ---
    if (auto *union_t = std::get_if<mem::rc_ptr<types::Union_type>>(&resolved_type)) {
        auto &union_defaults = union_data.at((*union_t)->name).variant_defaults;
        if (expr.fields.size() != 1) {
            type_error(expr.loc, "Union literals must be initialized with exactly one variant field.");
            return expr.type = types::Primitive_kind::Void;
        }

        std::string variant_name = expr.fields[0].first;
        ast::Expr *payload_expr = expr.fields[0].second;

        if (variant_name.empty()) {
            if (auto *var_expr = payload_expr ? std::get_if<ast::Variable_expr>(&payload_expr->node) : nullptr) {
                variant_name = var_expr->name;
                payload_expr = nullptr;
                expr.fields[0].first = variant_name;
                expr.fields[0].second = nullptr;
            }
        }

        if (!contains_key((*union_t)->variants, variant_name)) {
            type_error(expr.loc, "Union '" + (*union_t)->name + "' has no variant named '" + variant_name + "'.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto expected_payload_type = find_by_key((*union_t)->variants, variant_name)->second;
        if (!payload_expr) {
            if (expected_payload_type == types::Type(types::Primitive_kind::Void))
                return expr.type = resolved_type;

            if (auto it = union_defaults.find(variant_name); it != union_defaults.end())
                payload_expr = expr.fields[0].second = const_cast<ast::Expr *>(it->second);
            else {
                type_error(
                    expr.loc,
                    "Variant '" + variant_name + "' requires a value of type '" + types::type_to_string(expected_payload_type)
                        + "' or a declared default.");
                return expr.type = types::Primitive_kind::Void;
            }
        } else if (expected_payload_type == types::Type(types::Primitive_kind::Void)) {
            type_error(ast::get_loc(payload_expr->node), "Variant '" + variant_name + "' does not take a payload.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto actual_payload_type = check_expr(*payload_expr, expected_payload_type);
        if (actual_payload_type && !is_compatible(expected_payload_type, *actual_payload_type))
            type_error(ast::get_loc(payload_expr->node), "Type mismatch for union variant payload.");

        return expr.type = resolved_type;
    }

    // --- 2. MODEL LITERAL SYNTAX ---
    if (auto *model_t = std::get_if<mem::rc_ptr<types::Model_type>>(&resolved_type)) {
        auto model_type = *model_t;
        auto &field_defaults = model_data.at(model_type->name).field_defaults;

        if (expr.fields.size() > model_type->fields.size()) {
            type_error(expr.loc, "Too many fields provided for model '" + expr.model_name + "'.");
            return expr.type = types::Primitive_kind::Void;
        }

        for (size_t i = 0; i < expr.fields.size(); ++i) {
            if (expr.fields[i].first.empty()) {
                expr.fields[i].first = model_type->fields[i].first;
            }
        }

        std::unordered_set<std::string> provided_fields;
        for (const auto &field : expr.fields) {
            if (provided_fields.contains(field.first))
                type_error(expr.loc, "Field '" + field.first + "' was provided more than once in model literal '" + expr.model_name + "'.");
            provided_fields.insert(field.first);
        }

        for (const auto &expected_field : model_type->fields)
            if (!provided_fields.count(expected_field.first)) {
                if (auto it = field_defaults.find(expected_field.first); it != field_defaults.end()) {
                    expr.fields.push_back({expected_field.first, const_cast<ast::Expr *>(it->second)});
                    provided_fields.insert(expected_field.first);
                } else {
                    type_error(
                        expr.loc,
                        "Missing required field '" + expected_field.first + "' in model literal for '" + expr.model_name + "'");
                }
            }

        for (const auto &provided_field : expr.fields) {
            const auto &field_name = provided_field.first;
            if (!contains_key(model_type->fields, field_name)) {
                type_error(expr.loc, "Unknown field '" + field_name + "' in model '" + expr.model_name + "'");
                continue;
            }

            auto expected_type = find_by_key(model_type->fields, field_name)->second;
            auto actual_type_res = check_expr(*provided_field.second, expected_type);

            if (actual_type_res && !is_compatible(expected_type, actual_type_res.value())) {
                type_error(
                    ast::get_loc(provided_field.second->node),
                    "Field '" + field_name + "' type mismatch. Expected '" + types::type_to_string(expected_type) + "' but got '"
                        + types::type_to_string(actual_type_res.value()) + "'");
            }
        }

        return expr.type = resolved_type; // Safe to return the base resolved_type!
    }

    // --- 3. ERROR FALLBACK ---
    type_error(expr.loc, "'" + expr.model_name + "' is not a model or a union.");
    return expr.type = types::Primitive_kind::Void;
}

Result<types::Type> Type_checker::check_expr_node(ast::Range_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    auto start_res = check_expr(*expr.start);
    auto end_res = check_expr(*expr.end);
    if (start_res
        && (!types::is_primitive(start_res.value()) || !types::is_integer_primitive(types::get_primitive_kind(start_res.value()))))
        type_error(expr.loc, "Range start must be an integer.");
    if (end_res && (!types::is_primitive(end_res.value()) || !types::is_integer_primitive(types::get_primitive_kind(end_res.value()))))
        type_error(expr.loc, "Range end must be an integer.");

    types::Type element_type = types::Primitive_kind::I64;
    if (start_res && end_res && types::is_primitive(start_res.value()) && types::is_primitive(end_res.value())
        && types::is_integer_primitive(types::get_primitive_kind(start_res.value()))
        && types::is_integer_primitive(types::get_primitive_kind(end_res.value()))) {
        element_type = promote_numeric_type(start_res.value(), end_res.value());
        if (element_type == types::Type(types::Primitive_kind::Any)) {
            type_error(expr.loc, "Mixed signed and unsigned range bounds require an explicit cast.");
            element_type = types::Primitive_kind::I64;
        }
    }

    return expr.type = types::Type(mem::make_rc<types::Iterator_type>(element_type));
}

Result<types::Type> Type_checker::check_expr_node(ast::Spawn_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    if (expr.call)
        std::ignore = check_expr(*expr.call);
    return expr.type = types::Primitive_kind::Any;
}

Result<types::Type> Type_checker::check_expr_node(ast::Await_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    if (expr.thread)
        std::ignore = check_expr(*expr.thread);
    return expr.type = types::Primitive_kind::Any;
}

Result<types::Type> Type_checker::check_expr_node(ast::Yield_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    if (expr.value) {
        auto val_res = check_expr(*expr.value);
        return expr.type = val_res.value_or(types::Primitive_kind::Void);
    }
    return expr.type = types::Primitive_kind::Void;
}

Result<types::Type> Type_checker::check_expr_node(ast::Fstring_expr &expr, std::optional<types::Type> context_type)
{
    (void)context_type;
    for (auto *interp : expr.interpolations)
        if (interp)
            std::ignore = check_expr(*interp);
    return expr.type = types::Primitive_kind::String;
}

Result<types::Type> Type_checker::check_expr_node(ast::Anon_model_literal_expr &expr, std::optional<types::Type> context_type)
{
    // 1. Ensure we actually have a context to infer from
    if (!context_type) {
        type_error(expr.loc, "Cannot infer type of anonymous literal '.{}'. Context is missing.");
        return expr.type = types::Primitive_kind::Void;
    }

    types::Type expected_type = context_type.value();
    bool is_ctx_optional = false;

    if (is_optional(expected_type)) {
        expected_type = types::get_optional_type(*context_type)->base_type;
        is_ctx_optional = true;
    }

    // --- NEW: UNION ANONYMOUS LITERAL ---
    if (is_union(expected_type)) {
        auto expected_union = types::get_union_type(expected_type);
        auto &variant_defaults = union_data.at(expected_union->name).variant_defaults;

        if (expr.fields.size() != 1) {
            type_error(expr.loc, "Anonymous union literals must be initialized with exactly one variant field.");
            return expr.type = types::Primitive_kind::Void;
        }

        std::string variant_name = expr.fields[0].first;
        ast::Expr *payload_expr = expr.fields[0].second;

        if (variant_name.empty()) {
            if (auto *var_expr = payload_expr ? std::get_if<ast::Variable_expr>(&payload_expr->node) : nullptr) {
                variant_name = var_expr->name;
                payload_expr = nullptr;
                expr.fields[0].first = variant_name;
                expr.fields[0].second = nullptr;
            }
        }

        if (!contains_key(expected_union->variants, variant_name)) {
            type_error(expr.loc, "Union '" + expected_union->name + "' has no variant named '" + variant_name + "'.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto expected_payload_type = find_by_key(expected_union->variants, variant_name)->second;
        if (!payload_expr) {
            if (expected_payload_type == types::Type(types::Primitive_kind::Void))
                return expr.type = is_ctx_optional ? context_type.value() : expected_type;

            if (auto it = variant_defaults.find(variant_name); it != variant_defaults.end())
                payload_expr = expr.fields[0].second = const_cast<ast::Expr *>(it->second);
            else {
                type_error(
                    expr.loc,
                    "Variant '" + variant_name + "' requires a value of type '" + types::type_to_string(expected_payload_type)
                        + "' or a declared default.");
                return expr.type = types::Primitive_kind::Void;
            }
        } else if (expected_payload_type == types::Type(types::Primitive_kind::Void)) {
            type_error(ast::get_loc(payload_expr->node), "Variant '" + variant_name + "' does not take a payload.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto actual_payload_type = check_expr(*payload_expr, expected_payload_type);
        if (actual_payload_type && !is_compatible(expected_payload_type, actual_payload_type.value()))
            type_error(ast::get_loc(payload_expr->node), "Type mismatch for union variant payload.");

        return expr.type = is_ctx_optional ? context_type.value() : expected_type;
    }

    // --- MODEL ANONYMOUS LITERAL ---
    if (is_model(expected_type)) {
        auto expected_model = types::get_model_type(expected_type);
        static const std::unordered_map<std::string, const ast::Expr *> empty_field_defaults;
        const auto &field_defaults = (!expected_model->name.empty() && model_data.contains(expected_model->name))
            ? model_data.at(expected_model->name).field_defaults
            : empty_field_defaults;

        if (expr.fields.size() > expected_model->fields.size()) {
            type_error(
                expr.loc,
                "Anonymous model field count mismatch. Expected at most " + std::to_string(expected_model->fields.size()) + ", got "
                    + std::to_string(expr.fields.size()) + ".");
            return expr.type = types::Primitive_kind::Void;
        }

        bool has_named_fields = std::any_of(expr.fields.begin(), expr.fields.end(), [](const auto &field) { return !field.first.empty(); });

        if (has_named_fields) {
            bool has_positional_fields =
                std::any_of(expr.fields.begin(), expr.fields.end(), [](const auto &field) { return field.first.empty(); });
            if (has_positional_fields) {
                type_error(expr.loc, "Anonymous model literals cannot mix positional and named fields.");
                return expr.type = types::Primitive_kind::Void;
            }

            std::unordered_set<std::string> assigned_fields;

            for (auto &field : expr.fields) {
                if (!contains_key(expected_model->fields, field.first)) {
                    type_error(expr.loc, "Unknown field '" + field.first + "' in anonymous model literal.");
                    continue;
                }

                if (assigned_fields.contains(field.first)) {
                    type_error(expr.loc, "Field '" + field.first + "' was provided more than once in anonymous model literal.");
                    continue;
                }

                assigned_fields.insert(field.first);
            }
        } else {
            for (size_t i = 0; i < expr.fields.size(); ++i) {
                if (expr.fields[i].first.empty())
                    expr.fields[i].first = expected_model->fields[i].first;

                if (expr.fields[i].first != expected_model->fields[i].first) {
                    type_error(
                        expr.loc,
                        "Structural mismatch at field " + std::to_string(i) + ". Expected '" + expected_model->fields[i].first
                            + "', but got '" + expr.fields[i].first + "'. Order matters.");
                }
            }
        }

        for (size_t i = 0; i < expr.fields.size(); ++i) {
            if (!contains_key(expected_model->fields, expr.fields[i].first))
                continue;

            auto expected_field_type = find_by_key(expected_model->fields, expr.fields[i].first)->second;
            auto actual_field_type = check_expr(*expr.fields[i].second, expected_field_type);

            if (!actual_field_type || !is_compatible(expected_field_type, actual_field_type.value()))
                type_error(expr.loc, "Type mismatch for field '" + expr.fields[i].first + "'.");
        }

        std::vector<std::pair<std::string, ast::Expr *>> ordered_fields;
        ordered_fields.reserve(expected_model->fields.size());

        for (const auto &expected_field : expected_model->fields) {
            auto it = std::find_if(expr.fields.begin(), expr.fields.end(), [&](const auto &field) {
                return field.first == expected_field.first;
            });
            if (it != expr.fields.end()) {
                ordered_fields.push_back(*it);
                continue;
            }

            if (auto def_it = field_defaults.find(expected_field.first); def_it != field_defaults.end()) {
                ordered_fields.push_back({expected_field.first, const_cast<ast::Expr *>(def_it->second)});
                continue;
            }

            type_error(expr.loc, "Missing required field '" + expected_field.first + "' in anonymous model literal.");
        }

        expr.fields = std::move(ordered_fields);

        return expr.type = is_ctx_optional ? context_type.value() : expected_type;
    }

    type_error(expr.loc, "Context for anonymous literal '.{}' is neither a model nor a union type.");
    return expr.type = types::Primitive_kind::Void;
}

bool Type_checker::match_ffi_type(
    std::string expected_str, types::Type actual_type, std::unordered_map<std::string, types::Type> &generics) const
{
    // 1. Handle Unions ("string | i64")
    if (expected_str.find('|') != std::string::npos) {
        std::stringstream ss(expected_str);
        std::string item;
        while (std::getline(ss, item, '|')) {
            item.erase(0, item.find_first_not_of(" \t"));
            item.erase(item.find_last_not_of(" \t") + 1);

            auto temp_generics = generics; // Copy state so failed branches don't pollute
            if (match_ffi_type(item, actual_type, temp_generics)) {
                generics = temp_generics; // Commit the generics if successful
                return true;
            }
        }
        return false;
    }

    // 2. Handle Arrays ("T[]" or "string[]")
    if (expected_str.ends_with("[]")) {
        if (!is_array(actual_type))
            return false;
        std::string base_str = expected_str.substr(0, expected_str.length() - 2);
        return match_ffi_type(base_str, types::get_array_type(actual_type)->element_type, generics);
    }

    // 3. Handle Optionals ("T?")
    if (expected_str.ends_with("?")) {
        if (is_nil(actual_type))
            return true;
        std::string base_str = expected_str.substr(0, expected_str.length() - 1);
        types::Type inner_actual = actual_type;
        if (is_optional(actual_type))
            inner_actual = types::get_optional_type(actual_type)->base_type;
        return match_ffi_type(base_str, inner_actual, generics);
    }

    // 4. Handle Generics ("T", "U", "V")
    if (expected_str.length() == 1 && std::isupper(expected_str[0])) {
        if (generics.contains(expected_str)) {
            // If T is already bound, enforce the constraint!
            return is_compatible(generics[expected_str], actual_type);
        } else {
            // First time seeing T? Bind it to whatever actual_type is!
            generics[expected_str] = actual_type;
            return true;
        }
    }

    // 5. Handle Concrete Types ("i64", "string", "User")
    types::Type concrete_expected = parse_type_string(expected_str);
    return is_compatible(concrete_expected, actual_type);
}

} // namespace phos
