#pragma once

#include <cstddef>
#include <memory>
#include <unordered_map>
#include <vector>
#include <variant>
#include <iostream>
#include <string>

#include "../parser/ast.hpp"
#include "../value/type.hpp"
#include "../value/value.hpp"
#include "../error/err.hpp"
#include "../error/result.hpp"
#include "./native_globals.hpp"

// RAII guard to restore the environment automatically after scope exits
template <typename F>
struct Finally
{
    F func;
    ~Finally() { func(); }
};

template <typename F>
Finally<F> finally(F func)
{
    return {func};
}

namespace phos
{

struct ReturnValue
{
    Value value;
    bool is_return;

    ReturnValue() : value(std::monostate{}), is_return(false) {}
    ReturnValue(const Value &v) : value(v), is_return(true) {}
};

class Environment
{
private:
    std::unordered_map<std::string, Value> values;
    std::shared_ptr<Environment> enclosing;

public:
    Environment() : enclosing(nullptr) {}
    Environment(std::shared_ptr<Environment> enclosing) : enclosing(std::move(enclosing)) {}

    Result<void> define(const std::string &name, const Value &value)
    {
        values[name] = value;
        return {};
    }

    Result<Value> get(const std::string &name) const
    {
        if (values.contains(name))
            return values.at(name);
        if (enclosing != nullptr)
            return enclosing->get(name);
        return std::unexpected(err::msg("Undefined variable '" + name + "'", "interpreter", 0, 0));
    }

    Result<void> assign(const std::string &name, const Value &value)
    {
        if (values.contains(name))
        {
            values[name] = value;
            return {};
        }
        if (enclosing != nullptr)
            return enclosing->assign(name, value);
        return std::unexpected(err::msg("Undefined variable '" + name + "'", "interpreter", 0, 0));
    }
};

class Interpreter
{
private:
    // ========================================================================
    // Internal Data Structures
    // ========================================================================

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
    size_t next_closure_id = 0;

public:
    // ========================================================================
    // Constructor & Public Interface
    // ========================================================================

    Interpreter()
    {
        globals = std::make_shared<Environment>();
        environment = globals;
        auto native_functions = native::get_natives();
        for (auto & a: native_functions)
            this->define_native(a->name, a->arity, a->signature, a->parameters, a->code);
    }

    Result<void> interpret(const std::vector<std::unique_ptr<ast::Stmt>> &statements)
    {
        for (const auto &statement : statements)
        {
            auto result = execute(*statement);
            if (!result)
                return std::unexpected(result.error());
            if (result.value().is_return)
                return std::unexpected(err::msg("Cannot return from top-level code.", "interpreter", 0, 0));
        }
        return {};
    }

    void define_native(const std::string &name, int arity, types::Function_type signature,
                       std::vector<std::pair<std::string, types::Type>> parameters,
                       std::function<Result<Value>(const std::vector<Value> &)> code)
    {
        auto func_val = std::make_shared<Native_function_value>();
        func_val->name = name;
        func_val->arity = arity;
        func_val->signature = std::move(signature);
        func_val->parameters = std::move(parameters);
        func_val->code = std::move(code);

        std::ignore = globals->define(name, Value(func_val));
    }

private:
    // ========================================================================
    // Expression Evaluation
    // ========================================================================

    Result<Value> evaluate(const ast::Expr &expr)
    {
        return std::visit(
            [this](auto &&arg) -> Result<Value>
            {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, ast::Array_literal_expr>)
                {
                    auto array_val = std::make_shared<Array_value>();
                    for (const auto &elem_expr : arg.elements)
                    {
                        auto elem_result = evaluate(*elem_expr);
                        if (!elem_result)
                            return elem_result;
                        array_val->elements.push_back(elem_result.value());
                    }
                    return Value(array_val);
                }
                else if constexpr (std::is_same_v<T, ast::Array_access_expr>)
                {
                    auto array_res = evaluate(*arg.array);
                    if (!array_res)
                        return array_res;
                    auto index_res = evaluate(*arg.index);
                    if (!index_res)
                        return index_res;

                    if (!is_array(array_res.value()))
                        return std::unexpected(
                            err::msg("Can only use subscript '[]' on arrays.", "interpreter", arg.loc.line, arg.loc.column));
                    if (!is_int(index_res.value()))
                        return std::unexpected(err::msg("Array index must be an integer.", "interpreter", arg.loc.line, arg.loc.column));

                    auto array_val = get_array(array_res.value());
                    auto index = get_int(index_res.value());

                    if (index < 0 || static_cast<size_t>(index) >= array_val->elements.size())
                        return std::unexpected(err::msg("Array index out of bounds.", "interpreter", arg.loc.line, arg.loc.column));

                    return array_val->elements.at(index);
                }
                else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>)
                {
                    auto array_res = evaluate(*arg.array);
                    if (!array_res)
                        return array_res;
                    auto index_res = evaluate(*arg.index);
                    if (!index_res)
                        return index_res;
                    auto value_res = evaluate(*arg.value);
                    if (!value_res)
                        return value_res;

                    if (!is_array(array_res.value()))
                        return std::unexpected(
                            err::msg("Can only use subscript '[]' on arrays for assignment.", "interpreter", arg.loc.line, arg.loc.column));
                    if (!is_int(index_res.value()))
                        return std::unexpected(err::msg("Array index must be an integer.", "interpreter", arg.loc.line, arg.loc.column));

                    auto array_val = get_array(array_res.value());
                    auto index = get_int(index_res.value());

                    if (index < 0 || static_cast<size_t>(index) >= array_val->elements.size())
                        return std::unexpected(err::msg("Array index out of bounds.", "interpreter", arg.loc.line, arg.loc.column));

                    array_val->elements[index] = value_res.value();
                    return value_res.value();
                }
                if constexpr (std::is_same_v<T, ast::Assignment_expr>)
                {
                    auto value_result = evaluate(*arg.value);
                    if (!value_result)
                        return value_result;

                    auto assign_result = environment->assign(arg.name, value_result.value());
                    if (!assign_result)
                        return std::unexpected(assign_result.error());

                    return value_result.value();
                }

                else if constexpr (std::is_same_v<T, ast::Binary_expr>)
                {
                    auto left_result = evaluate(*arg.left);
                    if (!left_result)
                        return left_result;

                    auto right_result = evaluate(*arg.right);
                    if (!right_result)
                        return right_result;

                    const auto &left = left_result.value();
                    const auto &right = right_result.value();

                    switch (arg.op)
                    {
                        case lex::TokenType::Plus:
                            return add(left, right);
                        case lex::TokenType::Minus:
                            return subtract(left, right);
                        case lex::TokenType::Star:
                            return multiply(left, right);
                        case lex::TokenType::Slash:
                            return divide(left, right);
                        case lex::TokenType::Percent:
                            return modulo(left, right);
                        case lex::TokenType::Equal:
                            return Value(equals(left, right));
                        case lex::TokenType::NotEqual:
                            return Value(!equals(left, right));
                        case lex::TokenType::Less:
                            return lessThan(left, right);
                        case lex::TokenType::LessEqual:
                            return lessThanOrEqual(left, right);
                        case lex::TokenType::Greater:
                            return greaterThan(left, right);
                        case lex::TokenType::GreaterEqual:
                            return greaterThanOrEqual(left, right);
                        case lex::TokenType::LogicalAnd:
                            return Value(isTruthy(left) && isTruthy(right));
                        case lex::TokenType::LogicalOr:
                            return Value(isTruthy(left) || isTruthy(right));
                        default:
                            return std::unexpected(err::msg("Unknown binary operator.", "interpreter", arg.loc.line, arg.loc.column));
                    }
                }

                else if constexpr (std::is_same_v<T, ast::Call_expr>)
                {
                    auto callee_result = environment->get(arg.callee);
                    if (!callee_result)
                        return callee_result;

                    std::vector<Value> arguments;
                    for (const auto &argument : arg.arguments)
                    {
                        auto arg_result = evaluate(*argument);
                        if (!arg_result)
                            return arg_result;
                        arguments.push_back(arg_result.value());
                    }

                    if (is_function(callee_result.value()))
                        return call_function(arg.callee, arguments, arg.loc);
                    else if (is_closure(callee_result.value()))
                        return call_closure(get_closure(callee_result.value())->id, arguments, arg.loc);
                    else if (is_native_function(callee_result.value()))
                    {
                        auto native_fn = get_native_function(callee_result.value());
                        if (native_fn->arity != -1 && arguments.size() != native_fn->arity)
                        {
                            return std::unexpected(err::msg("'" + native_fn->name + "' expected " + std::to_string(native_fn->arity) +
                                                            " arguments but got " + std::to_string(arguments.size()) + ".",
                                                            "interpreter", arg.loc.line, arg.loc.column));
                        }
                        return native_fn->code(arguments);
                    }
                    else
                        return std::unexpected(
                            err::msg("Can only call functions and closures.", "interpreter", arg.loc.line, arg.loc.column));
                }

                else if constexpr (std::is_same_v<T, ast::Cast_expr>)
                {
                    auto value_result = evaluate(*arg.expression);
                    if (!value_result)
                        return value_result;
                    return cast_value(value_result.value(), arg.target_type);
                }

                else if constexpr (std::is_same_v<T, ast::Closure_expr>)
                {
                    size_t id = next_closure_id++;

                    types::Closure_type closure_type_info;
                    for (const auto &param : arg.parameters) closure_type_info.function_type.parameter_types.push_back(param.second);
                    closure_type_info.function_type.return_type = arg.return_type;

                    closures[id] = ClosureData{id, closure_type_info, std::make_shared<ast::Closure_expr>(arg), environment};

                    auto func_val = std::make_shared<Function_value>(closure_type_info.function_type, arg.parameters, environment);
                    auto closure_val = std::make_shared<Closure_value>(func_val, std::unordered_map<std::string, Value>{});
                    closure_val->id = id;
                    return Value(closure_val);
                }

                else if constexpr (std::is_same_v<T, ast::Field_access_expr>)
                {
                    auto object_result = evaluate(*arg.object);
                    if (!object_result)
                        return object_result;

                    if (!is_model(object_result.value()))
                        return std::unexpected(
                            err::msg("Can only access fields on model instances.", "interpreter", arg.loc.line, arg.loc.column));

                    auto model_val = get_model(object_result.value());
                    if (model_val->fields.contains(arg.field_name))
                        return model_val->fields.at(arg.field_name);

                    return std::unexpected(
                        err::msg("Field '" + arg.field_name + "' not found.", "interpreter", arg.loc.line, arg.loc.column));
                }

                else if constexpr (std::is_same_v<T, ast::Field_assignment_expr>)
                {
                    auto object_result = evaluate(*arg.object);
                    if (!object_result)
                        return object_result;

                    if (!is_model(object_result.value()))
                        return std::unexpected(
                            err::msg("Can only assign to fields of a model instance.", "interpreter", arg.loc.line, arg.loc.column));

                    auto value_res = evaluate(*arg.value);
                    if (!value_res)
                        return value_res;

                    get_model(object_result.value())->fields[arg.field_name] = value_res.value();
                    return value_res.value();
                }

                else if constexpr (std::is_same_v<T, ast::Literal_expr>)
                {
                    return arg.value;
                }

                else if constexpr (std::is_same_v<T, ast::Method_call_expr>)
                {
                    auto object_result = evaluate(*arg.object);
                    if (!object_result)
                        return object_result;

                    if (!is_model(object_result.value()))
                        return std::unexpected(
                            err::msg("Can only call methods on model instances.", "interpreter", arg.loc.line, arg.loc.column));

                    auto model_val = get_model(object_result.value());
                    const auto &model_name = model_val->signature.name;

                    if (!model_data.contains(model_name) || !model_data.at(model_name).methods.contains(arg.method_name))
                        return std::unexpected(
                            err::msg("Method '" + arg.method_name + "' not found.", "interpreter", arg.loc.line, arg.loc.column));

                    const auto &method_data = model_data.at(model_name).methods.at(arg.method_name);

                    std::vector<Value> arguments;
                    for (const auto &argument : arg.arguments)
                    {
                        auto arg_result = evaluate(*argument);
                        if (!arg_result)
                            return arg_result;
                        arguments.push_back(arg_result.value());
                    }

                    auto new_env = std::make_shared<Environment>(method_data.definition_environment);
                    new_env->define("this", object_result.value());

                    for (size_t i = 0; i < method_data.declaration->parameters.size(); ++i)
                        new_env->define(method_data.declaration->parameters[i].first, arguments[i]);

                    auto result = executeBlock(std::get<ast::Block_stmt>(method_data.declaration->body->node).statements, new_env);
                    if (!result)
                        return std::unexpected(result.error());
                    if (result.value().is_return)
                        return result.value().value;
                    return Value(std::monostate{});
                }

                else if constexpr (std::is_same_v<T, ast::Model_literal_expr>)
                {
                    if (!model_data.contains(arg.model_name))
                        return std::unexpected(
                            err::msg("Model type '" + arg.model_name + "' is not defined.", "interpreter", arg.loc.line, arg.loc.column));

                    auto model_type = model_data.at(arg.model_name).signature;
                    auto instance = std::make_shared<Model_value>(*model_type, std::unordered_map<std::string, Value>{});

                    for (const auto &[name, expr] : arg.fields)
                    {
                        auto val_res = evaluate(*expr);
                        if (!val_res)
                            return val_res;
                        instance->fields[name] = val_res.value();
                    }
                    return Value(instance);
                }

                else if constexpr (std::is_same_v<T, ast::Unary_expr>)
                {
                    auto right_result = evaluate(*arg.right);
                    if (!right_result)
                        return right_result;

                    switch (arg.op)
                    {
                        case lex::TokenType::Minus:
                            return negate(right_result.value());
                        case lex::TokenType::LogicalNot:
                            return Value(!isTruthy(right_result.value()));
                        default:
                            return std::unexpected(err::msg("Unknown unary operator.", "interpreter", arg.loc.line, arg.loc.column));
                    }
                }

                else if constexpr (std::is_same_v<T, ast::Variable_expr>)
                {
                    return environment->get(arg.name);
                }

                return std::unexpected(err::msg("Unhandled expression type", "interpreter", 0, 0));
            },
            expr.node);
    }

    // ========================================================================
    // Statement Execution
    // ========================================================================

    Result<ReturnValue> execute(const ast::Stmt &stmt)
    {
        return std::visit(
            [this](auto &&arg) -> Result<ReturnValue>
            {
                using T = std::decay_t<decltype(arg)>;

                if constexpr (std::is_same_v<T, ast::Block_stmt>)
                {
                    return executeBlock(arg.statements, std::make_shared<Environment>(environment));
                }

                else if constexpr (std::is_same_v<T, ast::Expr_stmt>)
                {
                    auto eval_result = evaluate(*arg.expression);
                    if (!eval_result)
                        return std::unexpected(eval_result.error());
                    return ReturnValue{};
                }

                else if constexpr (std::is_same_v<T, ast::Function_stmt>)
                {
                    types::Function_type func_type;
                    for (const auto &param : arg.parameters) func_type.parameter_types.push_back(param.second);
                    func_type.return_type = arg.return_type;

                    functions[arg.name] = {func_type, std::make_shared<ast::Function_stmt>(arg), environment};

                    auto func_val = std::make_shared<Function_value>(func_type, arg.parameters, environment);
                    auto define_result = environment->define(arg.name, Value(func_val));
                    if (!define_result)
                        return std::unexpected(define_result.error());
                    return ReturnValue{};
                }

                else if constexpr (std::is_same_v<T, ast::If_stmt>)
                {
                    auto condition_result = evaluate(*arg.condition);
                    if (!condition_result)
                        return std::unexpected(condition_result.error());

                    if (isTruthy(condition_result.value()))
                        return execute(*arg.then_branch);
                    else if (arg.else_branch != nullptr)
                        return execute(*arg.else_branch);
                    return ReturnValue{};
                }

                else if constexpr (std::is_same_v<T, ast::Model_stmt>)
                {
                    auto model_type = std::make_shared<types::Model_type>();
                    model_type->name = arg.name;

                    for (const auto &[name, type] : arg.fields) model_type->fields[name] = type;

                    ModelData data;
                    data.signature = model_type;

                    for (const auto &method_ast : arg.methods)
                    {
                        types::Function_type method_type;
                        for (const auto &param : method_ast.parameters) method_type.parameter_types.push_back(param.second);
                        method_type.return_type = method_ast.return_type;

                        data.methods[method_ast.name] = {method_type, std::make_shared<ast::Function_stmt>(method_ast), environment};
                        model_type->methods[method_ast.name] = method_type;
                    }

                    model_data[arg.name] = std::move(data);
                    return ReturnValue{};
                }

                else if constexpr (std::is_same_v<T, ast::Print_stmt>)
                {
                    auto value_result = evaluate(*arg.expression);
                    if (!value_result)
                        return std::unexpected(value_result.error());
                    std::cout << value_to_string(value_result.value()) << std::endl;
                    return ReturnValue{};
                }

                else if constexpr (std::is_same_v<T, ast::Return_stmt>)
                {
                    Value value;
                    if (arg.expression != nullptr)
                    {
                        auto value_result = evaluate(*arg.expression);
                        if (!value_result)
                            return std::unexpected(value_result.error());
                        value = value_result.value();
                    }
                    return ReturnValue(value);
                }

                else if constexpr (std::is_same_v<T, ast::Var_stmt>)
                {
                    Value value(std::monostate{});
                    if (arg.initializer != nullptr)
                    {
                        auto init_result = evaluate(*arg.initializer);
                        if (!init_result)
                            return std::unexpected(init_result.error());
                        value = init_result.value();
                    }
                    auto define_result = environment->define(arg.name, value);
                    if (!define_result)
                        return std::unexpected(define_result.error());
                    return ReturnValue{};
                }

                else if constexpr (std::is_same_v<T, ast::While_stmt>)
                {
                    while (true)
                    {
                        auto condition_result = evaluate(*arg.condition);
                        if (!condition_result)
                            return std::unexpected(condition_result.error());
                        if (!isTruthy(condition_result.value()))
                            break;

                        auto body_result = execute(*arg.body);
                        if (!body_result)
                            return body_result;
                        if (body_result.value().is_return)
                            return body_result;
                    }
                    return ReturnValue{};
                }

                else if constexpr (std::is_same_v<T, ast::For_stmt>)
                {
                    if (arg.initializer != nullptr)
                    {
                        auto init_result = execute(*arg.initializer);
                        if (!init_result)
                            return init_result;
                    }

                    while (true)
                    {
                        if (arg.condition != nullptr)
                        {
                            auto cond_result = evaluate(*arg.condition);
                            if (!cond_result)
                                return std::unexpected(cond_result.error());
                            if (!isTruthy(cond_result.value()))
                                break;
                        }

                        auto body_result = execute(*arg.body);
                        if (!body_result)
                            return body_result;
                        if (body_result.value().is_return)
                            return body_result;

                        if (arg.increment != nullptr)
                        {
                            auto inc_result = evaluate(*arg.increment);
                            if (!inc_result)
                                return std::unexpected(inc_result.error());
                        }
                    }
                    return ReturnValue{};
                }

                return std::unexpected(err::msg("Unhandled statement type", "interpreter", 0, 0));
            },
            stmt.node);
    }

    Result<ReturnValue> executeBlock(const std::vector<std::unique_ptr<ast::Stmt>> &statements, std::shared_ptr<Environment> block_env)
    {
        auto previous = this->environment;
        this->environment = block_env;
        auto guard = finally([this, previous]() { this->environment = previous; });

        for (const auto &statement : statements)
        {
            auto result = execute(*statement);
            if (!result || result.value().is_return)
                return result;
        }
        return ReturnValue{};
    }

    // ========================================================================
    // Function & Closure Calling
    // ========================================================================

    Result<Value> call_function(const std::string &name, const std::vector<Value> &arguments, const ast::Source_location &loc)
    {
        if (!functions.contains(name))
            return std::unexpected(err::msg("Undefined function '" + name + "'.", "interpreter", loc.line, loc.column));

        const auto &func_data = functions.at(name);
        auto new_env = std::make_shared<Environment>(func_data.definition_environment);

        for (size_t i = 0; i < func_data.declaration->parameters.size(); ++i)
            auto _ = new_env->define(func_data.declaration->parameters[i].first, arguments[i]);

        auto result = executeBlock(std::get<ast::Block_stmt>(func_data.declaration->body->node).statements, new_env);
        if (!result)
            return std::unexpected(result.error());
        if (result.value().is_return)
            return result.value().value;
        return Value(std::monostate{});
    }

    Result<Value> call_closure(size_t id, const std::vector<Value> &arguments, const ast::Source_location &loc)
    {
        if (!closures.contains(id))
            return std::unexpected(err::msg("Invalid or stale closure.", "interpreter", loc.line, loc.column));

        const auto &closure_data = closures.at(id);
        auto new_env = std::make_shared<Environment>(closure_data.captured_environment);

        for (size_t i = 0; i < closure_data.declaration->parameters.size(); ++i)
            std::ignore = new_env->define(closure_data.declaration->parameters[i].first, arguments[i]);

        auto result = executeBlock(std::get<ast::Block_stmt>(closure_data.declaration->body->node).statements, new_env);
        if (!result)
            return std::unexpected(result.error());
        if (result.value().is_return)
            return result.value().value;
        return Value(std::monostate{});
    }

    // ========================================================================
    // Arithmetic Operations
    // ========================================================================

    Result<Value> add(const Value &l, const Value &r)
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) + get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) + get_float(r));
        if (is_int(l) && is_float(r))
            return Value(static_cast<double>(get_int(l)) + get_float(r));
        if (is_float(l) && is_int(r))
            return Value(get_float(l) + static_cast<double>(get_int(r)));
        if (is_string(l) && is_string(r))
            return Value(get_string(l) + get_string(r));
        return std::unexpected(err::msg("Operands must be two numbers or two strings for '+'.", "interpreter", 0, 0));
    }

    Result<Value> subtract(const Value &l, const Value &r)
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) - get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) - get_float(r));
        if (is_int(l) && is_float(r))
            return Value(static_cast<double>(get_int(l)) - get_float(r));
        if (is_float(l) && is_int(r))
            return Value(get_float(l) - static_cast<double>(get_int(r)));
        return std::unexpected(err::msg("Operands must be numbers for '-'.", "interpreter", 0, 0));
    }

    Result<Value> multiply(const Value &l, const Value &r)
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) * get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) * get_float(r));
        if (is_int(l) && is_float(r))
            return Value(static_cast<double>(get_int(l)) * get_float(r));
        if (is_float(l) && is_int(r))
            return Value(get_float(l) * static_cast<double>(get_int(r)));
        return std::unexpected(err::msg("Operands must be numbers for '*'.", "interpreter", 0, 0));
    }

    Result<Value> divide(const Value &l, const Value &r)
    {
        if ((is_int(r) && get_int(r) == 0) || (is_float(r) && get_float(r) == 0.0))
            return std::unexpected(err::msg("Division by zero.", "interpreter", 0, 0));

        if (is_int(l) && is_int(r))
            return Value(static_cast<double>(get_int(l)) / static_cast<double>(get_int(r)));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) / get_float(r));
        if (is_int(l) && is_float(r))
            return Value(static_cast<double>(get_int(l)) / get_float(r));
        if (is_float(l) && is_int(r))
            return Value(get_float(l) / static_cast<double>(get_int(r)));
        return std::unexpected(err::msg("Operands must be numbers for '/'.", "interpreter", 0, 0));
    }

    Result<Value> modulo(const Value &l, const Value &r)
    {
        if (!is_int(l) || !is_int(r))
            return std::unexpected(err::msg("Operands must be integers for '%'.", "interpreter", 0, 0));
        if (get_int(r) == 0)
            return std::unexpected(err::msg("Division by zero.", "interpreter", 0, 0));
        return Value(get_int(l) % get_int(r));
    }

    Result<Value> negate(const Value &v)
    {
        if (is_int(v))
            return Value(-get_int(v));
        if (is_float(v))
            return Value(-get_float(v));
        return std::unexpected(err::msg("Operand must be a number for '-'.", "interpreter", 0, 0));
    }

    // ========================================================================
    // Comparison Operations
    // ========================================================================

    Result<Value> lessThan(const Value &l, const Value &r)
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) < get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) < get_float(r));
        return std::unexpected(err::msg("Operands must be numbers of same type for '<'. Even the comparison between f64 and i64 is not allowed", "interpreter", 0, 0));
    }

    Result<Value> lessThanOrEqual(const Value &l, const Value &r)
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) <= get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) <= get_float(r));

        return std::unexpected(err::msg("Operands must be numbers of same type for '<='. Even the comparison between f64 and i64 is not allowed", "interpreter", 0, 0));
    }

    Result<Value> greaterThan(const Value &l, const Value &r)
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) > get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) > get_float(r));
        return std::unexpected(err::msg("Operands must be numbers of same type for '>'. Even the comparison between f64 and i64 is not allowed", "interpreter", 0, 0));
    }

    Result<Value> greaterThanOrEqual(const Value &l, const Value &r)
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) >= get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) >= get_float(r));

        return std::unexpected(err::msg("Operands must be numbers of same type for '>='. Even the comparison between f64 and i64 is not allowed", "interpreter", 0, 0));
    }

    // ========================================================================
    // Utility Methods
    // ========================================================================

    bool equals(const Value &l, const Value &r) { return l == r; }

    bool isTruthy(const Value &v)
    {
        if (is_bool(v))
            return get_bool(v);
        if (is_void(v) || is_null(v))
            return false;
        if (is_int(v))
            return get_int(v) != 0;
        if (is_float(v))
            return get_float(v) != 0.0;
        return true;  // Models, functions, closures, and non-empty strings are truthy
    }

    Result<Value> cast_value(const Value &v, const types::Type &t)
    {
        if (types::is_primitive(t) && types::get_primitive_kind(t) == types::Primitive_kind::String)
            return Value(value_to_string(v));
        return v;
    }
};

}  // namespace phos
