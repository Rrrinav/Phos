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
struct ReturnValue
{
    Value value;
    bool is_return;
    ReturnValue() : value(std::monostate{}), is_return(false) {}
    ReturnValue(const Value &v) : value(v), is_return(true) {}
};

class Interpreter
{
private:
    // Internal Data Structures
    struct FunctionData
    {
        types::Function_type signature;
        std::shared_ptr<ast::Function_stmt> declaration;
        std::shared_ptr<Environment> definition_environment;
    };
    struct ClosureData
    {
        std::size_t id;
        types::Closure_type signature;
        std::shared_ptr<ast::Closure_expr> declaration;
        std::shared_ptr<Environment> captured_environment;
    };
    struct ModelData
    {
        std::shared_ptr<types::Model_type> signature;
        std::unordered_map<std::string, FunctionData> methods;
    };
    // Environment management
    std::shared_ptr<Environment> globals;
    std::shared_ptr<Environment> environment;
    // Code execution tables
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
    // ========================================================================
    // Statement Execution
    // ========================================================================
    Result<ReturnValue> execute(const ast::Stmt &stmt);
    std::string unescape_string(const std::string &s);
    Result<ReturnValue> executeBlock(const std::vector<ast::Stmt*> &statements, std::shared_ptr<Environment> block_env);
    // ========================================================================
    // Function & Closure Calling
    // ========================================================================
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
    Result<Value> lessThanOrEqual(const Value &l, const Value &r);
    Result<Value> greaterThan(const Value &l, const Value &r);
    Result<Value> greaterThanOrEqual(const Value &l, const Value &r);
    bool equals(const Value &l, const Value &r);
    bool isTruthy(const Value &v);
    Result<Value> cast_value(const Value &v, const types::Type &t);
    Result<Value> map_array(Value &array_val, const Value &closure);
    Result<Value> map_optional(Value &optional_val, const Value &closure);
};
}  // namespace phos
