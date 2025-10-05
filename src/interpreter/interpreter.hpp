#pragma once

#include <cstddef>
#include <memory>
#include <unordered_map>
#include <vector>
#include <variant>
#include <string>

#include "../parser/ast.hpp"
#include "../value/type.hpp"
#include "../value/value.hpp"
#include "../error/result.hpp"
#include "./environment.hpp"

namespace phos
{
struct Return_value
{
    Value value;
    bool is_return;
    Return_value() : value(std::monostate{}), is_return(false) {}
    Return_value(const Value &v) : value(v), is_return(true) {}
};

class Interpreter
{
private:
    // Internal Data Structures
    struct FunctionData
    {
        types::Function_type signature;
        mem::rc_ptr<ast::Function_stmt> declaration;
        mem::rc_ptr<Environment> definition_environment;
    };
    struct ClosureData
    {
        std::size_t id;
        types::Closure_type signature;
        mem::rc_ptr<ast::Closure_expr> declaration;
        mem::rc_ptr<Environment> captured_environment;
    };
    struct ModelData
    {
        mem::rc_ptr<types::Model_type> signature;
        std::unordered_map<std::string, FunctionData> methods;
    };
    // Environment management
    mem::rc_ptr<Environment> globals;
    mem::rc_ptr<Environment> environment;
    // Code execution tables
    std::unordered_map<std::string, mem::rc_ptr<types::Union_type>> m_union_signatures;
    std::unordered_map<std::string, FunctionData> functions;
    std::unordered_map<size_t, ClosureData> closures;
    std::unordered_map<std::string, ModelData> model_data;
    using Native_method = std::function<Result<Value>(Value &, const std::vector<Value> &)>;
    std::unordered_map<std::string, std::unordered_map<std::string, Native_method>> native_methods;
    size_t next_closure_id = 0;

public:
    // ========================================================================
    // Constructor & Public Interface
    // ========================================================================
    Interpreter();
    Result<void> interpret(const std::vector<ast::Stmt*> &statements);
    void define_native(const std::string &name, int arity, types::Function_type signature,
                       std::vector<std::pair<std::string, types::Type>> parameters,
                       std::function<Result<Value>(const std::vector<Value> &)> code);

    void define_native_method(std::string type, const std::string &name, int arity, Native_method code);

private:
    // ========================================================================
    // Expression Evaluation
    // ========================================================================
    Result<Value> evaluate(const ast::Expr &expr);
    Result<Value> evaluate_visitor(const ast::Array_literal_expr& expr);
    Result<Value> evaluate_visitor(const ast::Array_access_expr& expr);
    Result<Value> evaluate_visitor(const ast::Array_assignment_expr& expr);
    Result<Value> evaluate_visitor(const ast::Assignment_expr & expr);
    Result<Value> evaluate_visitor(const ast::Binary_expr& expr);
    Result<Value> evaluate_visitor(const ast::Call_expr& expr);
    Result<Value> evaluate_visitor(const ast::Closure_expr& expr);
    Result<Value> evaluate_visitor(const ast::Cast_expr& expr);
    Result<Value> evaluate_visitor(const ast::Field_access_expr& expr);
    Result<Value> evaluate_visitor(const ast::Field_assignment_expr& expr);
    Result<Value> evaluate_visitor(const ast::Literal_expr& expr);
    Result<Value> evaluate_visitor(const ast::Variable_expr& expr);
    Result<Value> evaluate_visitor(const ast::Unary_expr& expr);
    Result<Value> evaluate_visitor(const ast::Method_call_expr& expr);
    Result<Value> evaluate_visitor(const ast::Model_literal_expr& expr);
    Result<Value> evaluate_visitor(const ast::Static_path_expr& expr);
    // ========================================================================
    // Statement Execution
    // ========================================================================
    Result<Return_value> execute(const ast::Stmt &stmt);

    Result<Return_value> execute_visitor(const ast::Return_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::Expr_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::Var_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::Function_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::Model_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::Block_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::If_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::While_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::Print_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::For_stmt& stmt);
    Result<Return_value> execute_visitor(const ast::Union_stmt& stmt);

    std::string unescape_string(const std::string &s);
    Result<Return_value> execute_block(const std::vector<ast::Stmt*> &statements, mem::rc_ptr<Environment> block_env);
    // ========================================================================
    // Function & Closure Calling
    // ========================================================================
    Result<Value> call(const Value& callee, const std::vector<Value>& arguments, const ast::Source_location& loc);
    Result<Value> call_function(const std::string &name, const std::vector<Value> &arguments, const ast::Source_location &loc);
    Result<Value> call_closure(size_t id, const std::vector<Value> &arguments, const ast::Source_location &loc);
    // ========================================================================
    // Arithmetic Operations
    // ========================================================================
    Result<Value> add(const Value &l, const Value &r);
    Result<Value> subtract(const Value &l, const Value &r);
    Result<Value> multiply(const Value &l, const Value &r);
    Result<Value> divide(const Value &l, const Value &r);
    Result<Value> modulo(const Value &l, const Value &r);
    Result<Value> negate(const Value &v);
    Result<Value> lessThan(const Value &l, const Value &r);
    Result<Value> less_than_or_equal(const Value &l, const Value &r);
    Result<Value> greater_than(const Value &l, const Value &r);
    Result<Value> greater_than_or_equal(const Value &l, const Value &r);
    bool equals(const Value &l, const Value &r);
    bool is_truthy(const Value &v);
    Result<Value> cast_value(const Value &v, const types::Type &t);
    Result<Value> map_array(Value &array_val, const Value &closure);
    Result<Value> map_optional(Value &optional_val, const Value &closure);
};
}  // namespace phos
