#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <optional>
#include <utility>

#include "../parser/ast.hpp"
#include "../error/err.hpp"
#include "../error/result.hpp"
#include "../value/type.hpp"
#include "../value/value.hpp"

namespace phos
{

class Type_checker;

// TYPE RESOLVER
class TypeResolver
{
public:
    TypeResolver(Type_checker &checker) : checker(checker) {}
    void resolve(std::vector<ast::Stmt *> &statements);

private:
    Type_checker &checker;
    void resolve_stmt(ast::Stmt &stmt);
    void resolve_expr(ast::Expr &expr);
    void resolve_type(types::Type &type);

    template <typename T>
    void visit(T &node)
    {
    }  // Default visitor
    void visit(ast::Function_stmt &stmt);
    void visit(ast::Model_stmt &stmt);
    void visit(ast::Union_stmt &stmt);
    void visit(ast::Enum_stmt &stmt);
    void visit(ast::Block_stmt &stmt);
    void visit(ast::Var_stmt &stmt);
    void visit(ast::Print_stmt &stmt);
    void visit(ast::Expr_stmt &stmt);
    void visit(ast::If_stmt &stmt);
    void visit(ast::While_stmt &stmt);
    void visit(ast::For_stmt &stmt);
    void visit(ast::For_in_stmt &stmt);
    void visit(ast::Match_stmt &stmt);
    void visit(ast::Return_stmt &stmt);
    void visit(ast::Assignment_expr &expr);
    void visit(ast::Binary_expr &expr);
    void visit(ast::Call_expr &expr);
    void visit(ast::Cast_expr &expr);
    void visit(ast::Closure_expr &expr);
    void visit(ast::Field_access_expr &expr);
    void visit(ast::Static_path_expr &expr);
    void visit(ast::Field_assignment_expr &expr);
    void visit(ast::Method_call_expr &expr);
    void visit(ast::Model_literal_expr &expr);
    void visit(ast::Unary_expr &expr);
    void visit(ast::Array_literal_expr &expr);
    void visit(ast::Array_access_expr &expr);
    void visit(ast::Array_assignment_expr &expr);
    void visit(ast::Range_expr &expr);
    void visit(ast::Spawn_expr &expr);
    void visit(ast::Await_expr &expr);
    void visit(ast::Yield_expr &expr);
    void visit(ast::Fstring_expr &expr);
    void visit(ast::Enum_member_expr &expr);
};

// TYPE CHECKER
class Type_checker
{
    friend class TypeResolver;

public:
    std::unordered_map<std::string, mem::rc_ptr<types::Model_type>> model_signatures;
    std::unordered_map<std::string, mem::rc_ptr<types::Union_type>> m_union_signatures;
    std::unordered_map<std::string, mem::rc_ptr<types::Enum_type>> enum_signatures;

    // --- FFI SIGNATURE REGISTRY ---
    struct Native_param
    {
        std::string          name;
        std::string          type;
        std::optional<Value> default_value;
    };

    struct Native_sig
    {
        std::vector<Native_param> params;
        std::string ret_type;
    };
    std::unordered_map<std::string, std::vector<Native_sig>> native_signatures;

    void define_native(const std::string &name, const std::vector<std::string> &params, const std::string &ret)
    {
        std::vector<Native_param> native_params;
        native_params.reserve(params.size());
        for (const auto &param : params)
            native_params.push_back({.name = "", .type = param, .default_value = std::nullopt});
        native_signatures[name].push_back({native_params, ret});
    }

    void define_native(const std::string &name, const std::vector<Native_param> &params, const std::string &ret)
    {
        native_signatures[name].push_back({params, ret});
    }

    void type_error(const ast::Source_location &loc, const std::string &message) { errors.push_back({message, this->phase, loc.l, loc.c}); }

    std::vector<err::msg> check(std::vector<ast::Stmt *> &statements);

    Type_checker() = default;

    types::Type parse_type_string(std::string str) const;
    bool match_ffi_type(std::string expected_str, types::Type actual_type, std::unordered_map<std::string, types::Type> &generics) const;

    struct FunctionData
    {
        const ast::Function_stmt *declaration;
    };

    struct ModelData
    {
        mem::rc_ptr<types::Model_type> signature;
        std::unordered_map<std::string, FunctionData> methods;
        std::unordered_map<std::string, FunctionData> static_methods;
    };

    using Scope = std::unordered_map<std::string, std::pair<types::Type, bool>>;
    std::vector<Scope> scopes;
    std::vector<std::unordered_set<std::string>> m_nil_checked_vars_stack;

    std::unordered_map<std::string, FunctionData> functions;
    std::unordered_map<std::string, ModelData> model_data;

    std::optional<types::Type> current_return_type;
    std::optional<mem::rc_ptr<types::Model_type>> current_model_type;
    std::vector<err::msg> errors;
    std::string phase = "type-checking";

    // --- Private Helper Methods ---
    bool is_compatible(const types::Type &expected, const types::Type &actual) const;
    bool is_argument_compatible(const std::vector<types::Type> &allowed_types, const types::Type &actual_type) const;
    void begin_scope();
    void end_scope();
    void declare(const std::string &name, const types::Type &type, bool is_constant, const ast::Source_location &loc);
    Result<std::pair<types::Type, bool>> lookup(const std::string &name, const ast::Source_location &loc);
    Result<std::pair<types::Type, bool>> lookup_variable(const std::string &name, const ast::Source_location &loc);
    types::Type promote_numeric_type(const types::Type &left, const types::Type &right) const;

    bool is_numeric(const types::Type &type) const;
    bool is_boolean(const types::Type &type) const;
    bool is_string(const types::Type &type) const;
    bool is_array(const types::Type &type) const;
    bool is_function(const types::Type &type) const;
    bool is_model(const types::Type &type) const;
    bool is_union(const types::Type &type) const;
    bool is_enum(const types::Type &type) const;
    bool is_iterator(const types::Type &type) const;
    bool is_any(const types::Type &type) const;
    bool is_nil(const types::Type &type) const;
    bool default_expr_uses_forbidden_names(const ast::Expr &expr, const std::unordered_set<std::string> &forbidden_names) const;
    void validate_function_defaults(const ast::Function_stmt &stmt);

    struct Bound_call_arguments
    {
        std::vector<ast::Call_argument> ordered_arguments;
        bool ok = true;
    };

    struct Bound_native_arguments
    {
        std::vector<ast::Call_argument> ordered_arguments;
        std::unordered_map<std::string, types::Type> generics;
        bool ok = false;
    };

    Bound_call_arguments bind_call_arguments(const std::vector<ast::Function_param> &parameters,
                                             const std::vector<ast::Call_argument> &arguments,
                                             const ast::Source_location &call_loc,
                                             const std::string &call_kind,
                                             const std::string &call_name);
    Bound_native_arguments try_bind_native_arguments(const std::vector<Native_param> &parameters,
                                                     const std::vector<ast::Call_argument> &arguments,
                                                     std::optional<types::Type> receiver_type = std::nullopt);
    bool is_optional(const types::Type &type) const;
    bool is_iterator_protocol_type(const types::Type &type) const;
    types::Type iterator_element_type(const types::Type &type) const;
    types::Type to_iterator_type(const types::Type &type) const;

    // --- Main Checking Logic ---
    void collect_signatures(const std::vector<ast::Stmt *> &statements);
    void check_stmt(ast::Stmt &stmt);
    Result<types::Type> check_expr(ast::Expr &expr, std::optional<types::Type> context_type = std::nullopt);

    // --- Node Visitors ---
    void check_stmt_node(ast::Function_stmt &stmt);
    void check_stmt_node(ast::Model_stmt &stmt);
    void check_stmt_node(ast::Union_stmt &stmt);
    void check_stmt_node(ast::Enum_stmt &stmt);
    void check_stmt_node(ast::Block_stmt &stmt);
    void check_stmt_node(ast::Expr_stmt &stmt);
    void check_stmt_node(ast::If_stmt &stmt);
    void check_stmt_node(ast::Print_stmt &stmt);
    void check_stmt_node(ast::Return_stmt &stmt);
    void check_stmt_node(ast::Var_stmt &stmt);
    void check_stmt_node(ast::While_stmt &stmt);
    void check_stmt_node(ast::For_stmt &stmt);
    void check_stmt_node(ast::For_in_stmt &stmt);
    void check_stmt_node(ast::Match_stmt &stmt);

    Result<types::Type> check_expr_node(ast::Assignment_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Binary_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Call_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Cast_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Closure_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Field_access_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Static_path_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Enum_member_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Field_assignment_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Literal_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Method_call_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Model_literal_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Unary_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Variable_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Array_literal_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Array_access_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Array_assignment_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Range_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Spawn_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Await_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Yield_expr &expr, std::optional<types::Type> context_type);
    Result<types::Type> check_expr_node(ast::Fstring_expr &expr, std::optional<types::Type> context_type);
};
}  // namespace phos
