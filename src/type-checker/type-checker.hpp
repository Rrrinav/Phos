#pragma once

#include <memory>
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
#include "../interpreter/native_signatures.hpp"

namespace phos
{

class Type_checker;

// ===================================================================
// TYPE RESOLVER
// ===================================================================
class TypeResolver
{
public:
    TypeResolver(Type_checker &checker) : checker(checker) {}
    void resolve(std::vector<ast::Stmt*> &statements);

private:
    Type_checker &checker;
    void resolve_stmt(ast::Stmt &stmt);
    void resolve_expr(ast::Expr &expr);
    void resolve_type(types::Type &type);

    template <typename T>
    void visit(T &node) { }
    void visit(ast::Function_stmt &stmt);
    void visit(ast::Model_stmt &stmt);
    void visit(ast::Block_stmt &stmt);
    void visit(ast::Var_stmt &stmt);
    void visit(ast::Print_stmt &stmt);
    void visit(ast::Expr_stmt &stmt);
    void visit(ast::If_stmt &stmt);
    void visit(ast::While_stmt &stmt);
    void visit(ast::For_stmt &stmt);
    void visit(ast::Return_stmt &stmt);
    void visit(ast::Assignment_expr &expr);
    void visit(ast::Binary_expr &expr);
    void visit(ast::Call_expr &expr);
    void visit(ast::Cast_expr &expr);
    void visit(ast::Closure_expr &expr);
    void visit(ast::Field_access_expr &expr);
    void visit(ast::Field_assignment_expr &expr);
    void visit(ast::Method_call_expr &expr);
    void visit(ast::Model_literal_expr &expr);
    void visit(ast::Unary_expr &expr);
    void visit(ast::Array_literal_expr &expr);
    void visit(ast::Array_access_expr &expr);
    void visit(ast::Array_assignment_expr &expr);
};

// ===================================================================
// TYPE CHECKER
// ===================================================================
class Type_checker
{
    friend class TypeResolver;

public:
    std::unordered_map<std::string, std::shared_ptr<types::Model_type>> model_signatures;

    void type_error(const ast::Source_location &loc, const std::string &message)
    {
        errors.push_back({message, this->phase, loc.line, loc.column});
    }

    std::vector<err::msg> check(std::vector<ast::Stmt*> &statements);

    Type_checker()
    {
        m_native_signatures = native::get_native_fn_signatures();
        m_native_methods = native::get_native_method_signatures();
    }

private:
    struct FunctionData
    {
        const ast::Function_stmt *declaration;
    };

    struct ModelData
    {
        std::shared_ptr<types::Model_type> signature;
        std::unordered_map<std::string, FunctionData> methods;
    };

    using Scope = std::unordered_map<std::string, std::pair<types::Type, bool>>;
    std::vector<Scope> scopes;
    std::vector<std::unordered_set<std::string>> m_nil_checked_vars_stack;

    std::unordered_map<std::string, FunctionData> functions;
    std::unordered_map<std::string, ModelData> model_data;
    std::unordered_map<std::string, native::Native_function_signature> m_native_signatures;
    std::unordered_map<std::string, native::Native_method_signature> m_native_methods;

    std::optional<types::Type> current_return_type;
    std::optional<std::shared_ptr<types::Model_type>> current_model_type;
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
    bool is_closure(const types::Type &type) const;
    bool is_model(const types::Type &type) const;
    bool is_any(const types::Type &type) const;
    bool is_nil(const types::Type &type) const;
    bool is_optional(const types::Type &type) const;

    // --- Main Checking Logic ---
    void collect_signatures(const std::vector<ast::Stmt*> &statements);
    void check_stmt(ast::Stmt &stmt);
    Result<types::Type> check_expr(ast::Expr &expr);

    // --- Node Visitors ---
    void check_stmt_node(ast::Function_stmt &stmt);
    void check_stmt_node(ast::Model_stmt &stmt);
    void check_stmt_node(ast::Block_stmt &stmt);
    void check_stmt_node(ast::Expr_stmt &stmt);
    void check_stmt_node(ast::If_stmt &stmt);
    void check_stmt_node(ast::Print_stmt &stmt);
    void check_stmt_node(ast::Return_stmt &stmt);
    void check_stmt_node(ast::Var_stmt &stmt);
    void check_stmt_node(ast::While_stmt &stmt);
    void check_stmt_node(ast::For_stmt &stmt);

    Result<types::Type> check_expr_node(ast::Assignment_expr &expr);
    Result<types::Type> check_expr_node(ast::Binary_expr &expr);
    Result<types::Type> check_expr_node(ast::Call_expr &expr);
    Result<types::Type> check_expr_node(ast::Cast_expr &expr);
    Result<types::Type> check_expr_node(ast::Closure_expr &expr);
    Result<types::Type> check_expr_node(ast::Field_access_expr &expr);
    Result<types::Type> check_expr_node(ast::Field_assignment_expr &expr);
    Result<types::Type> check_expr_node(ast::Literal_expr &expr);
    Result<types::Type> check_expr_node(ast::Method_call_expr &expr);
    Result<types::Type> check_expr_node(ast::Model_literal_expr &expr);
    Result<types::Type> check_expr_node(ast::Unary_expr &expr);
    Result<types::Type> check_expr_node(ast::Variable_expr &expr);
    Result<types::Type> check_expr_node(ast::Array_literal_expr &expr);
    Result<types::Type> check_expr_node(ast::Array_access_expr &expr);
    Result<types::Type> check_expr_node(ast::Array_assignment_expr &expr);
};
}  // namespace phos
