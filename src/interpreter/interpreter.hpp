#pragma once

#include <cstddef>
#include <cstdio>
#include <print>
#include <string>
#include <variant>
#include <iostream>
#include <string>
#include <vector>
#include <variant>
#include <unordered_map>
#include <memory>
#include <expected>
#include "../utility/utility.hpp"
#include "environment.hpp"

#include "../parser/ast.hpp"

namespace phos
{

// Add a custom exception for return values
struct ReturnValue
{
    Value value;
    bool ok = true;
    std::string err = "";
    ReturnValue(Value val) : value(std::move(val)) {}
};

class Interpreter
{
private:
    Environment *environment = nullptr;
    std::unordered_map<std::string, const ast::Function_stmt *> functions;
    std::string filename;

public:
    Interpreter() { environment = new Environment(nullptr); }

    ~Interpreter()
    {
        Environment *env = environment;
        while (env)
        {
            Environment *p = env->parent;
            delete env;
            env = p;
        }
    }


    std::unordered_map<std::string, Value> get_variables()
    {
        return environment->vars;
    }

    std::unordered_map<std::string, const ast::Function_stmt *> get_functions()
    {
        return this->functions;
    }

    Result<void> interpret(const std::vector<std::unique_ptr<ast::Stmt>> &statements)
    {
        // Register all functions first
        for (const auto &stmt : statements)
            if (const auto *func_stmt = dynamic_cast<const ast::Function_stmt *>(stmt.get()))
                functions.emplace(func_stmt->name, func_stmt);

        // Execute non-function statements
        for (const auto &stmt : statements)
        {
            if (dynamic_cast<const ast::Function_stmt *>(stmt.get()))
                continue;

            auto result = execute(*stmt);
            if (!result)
                return std::unexpected(result.error());
        }
        return {};
    }

private:
    Result<void> declareVariable(const std::string &name, Value val)
    {
        environment->define(name, std::move(val));
        return {};
    }

    Result<bool> assignVariable(const std::string &name, Value val)
    {
        if (environment->assign(name, std::move(val)))
            return true;
        return std::unexpected(err::msg("Assignment to undefined variable", "runtime", 0, 0, filename));
    }

    Result<Value> lookupVariable(const std::string &name)
    {
        if (auto *val = environment->lookup(name))
            return *val;
        return std::unexpected(err::msg("Undefined variable '" + name + "'", "runtime", 0, 0, filename));
    }

    // ===== Statement execution =====
    Result<void> execute(const ast::Stmt &stmt)
    {
        if (const auto *var_stmt = dynamic_cast<const ast::Var_stmt *>(&stmt))
            return execute_var_stmt(*var_stmt);
        if (const auto *print_stmt = dynamic_cast<const ast::Print_stmt *>(&stmt))
            return execute_print_stmt(*print_stmt);
        if (const auto *expr_stmt = dynamic_cast<const ast::Expr_stmt *>(&stmt))
            return execute_expr_stmt(*expr_stmt);
        if (const auto *if_stmt = dynamic_cast<const ast::If_stmt *>(&stmt))
            return execute_if_stmt(*if_stmt);
        if (const auto *while_stmt = dynamic_cast<const ast::While_stmt *>(&stmt))
            return execute_while_stmt(*while_stmt);
        if (const auto *for_stmt = dynamic_cast<const ast::For_stmt *>(&stmt))
            return execute_for_stmt(*for_stmt);
        if (const auto *block_stmt = dynamic_cast<const ast::Block_stmt *>(&stmt))
            return execute_block_stmt(*block_stmt);
        if (const auto *return_stmt = dynamic_cast<const ast::Return_stmt *>(&stmt))
            return execute_return_stmt(*return_stmt);

        return std::unexpected(err::msg("Unknown statement type", "runtime", stmt.line, stmt.column));
    }

    Result<void> execute_var_stmt(const ast::Var_stmt &stmt)
    {
        Value value;
        if (stmt.initializer)
        {
            auto eval_res = evaluate(*stmt.initializer);
            if (!eval_res)
                return std::unexpected(eval_res.error());
            value = *eval_res;
        }
        else
        {
            // Default initialization based on type
            switch (stmt.type.kind_)
            {
                case types::kind::Int:
                    value = int64_t(0);
                    break;
                case types::kind::Float:
                    value = double(0.0);
                    break;
                case types::kind::Bool:
                    value = false;
                    break;
                case types::kind::String:
                    value = std::string("");
                    break;
                case types::kind::Void:
                    value = nullptr;
                    break;
                default:
                    return std::unexpected(err::msg("Cannot default-initialize variable", "runtime", stmt.line, stmt.column));
            }
        }
        return declareVariable(stmt.name, std::move(value));
    }

    Result<void> execute_print_stmt(const ast::Print_stmt &stmt)
    {
        auto stream = stmt.stream == ast::Print_stream::STDOUT ? stdout : stderr;

        auto eval_res = evaluate(*stmt.expression);
        if (!eval_res)
            return std::unexpected(eval_res.error());

        std::visit(
            [&](auto &&arg)
            {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, std::string>)
                    std::println(stream, "{}", arg);
                else if constexpr (std::is_same_v<T, bool>)
                    std::println(stream, "{}", arg ? "true" : "false");
                else if constexpr (std::is_same_v<T, std::nullptr_t>)
                    std::println("null");
                else if constexpr (std::is_same_v<T, std::monostate>)
                    std::println(stream,"monostate: uninitialized");
                else
                    std::println(stream,"{}", arg);
            },
        *eval_res);

        return {};
    }

    Result<void> execute_expr_stmt(const ast::Expr_stmt &stmt)
    {
        auto eval_res = evaluate(*stmt.expression);
        if (!eval_res)
            return std::unexpected(eval_res.error());
        return {};
    }

    Result<void> execute_if_stmt(const ast::If_stmt &stmt)
    {
        auto cond_res = evaluate(*stmt.condition);
        if (!cond_res)
            return std::unexpected(cond_res.error());

        if (!std::holds_alternative<bool>(*cond_res))
            return std::unexpected(err::msg("If condition must be bool", "runtime", stmt.condition->line, stmt.condition->column));

        bool cond_value = std::get<bool>(*cond_res);

        if (cond_value)
            return execute(*stmt.then_branch);
        else if (stmt.else_branch)
            return execute(*stmt.else_branch);

        return {};
    }

    Result<void> execute_while_stmt(const ast::While_stmt &stmt)
    {
        while (true)
        {
            auto cond_res = evaluate(*stmt.condition);
            if (!cond_res)
                return std::unexpected(cond_res.error());

            if (!std::holds_alternative<bool>(*cond_res))
                return std::unexpected(err::msg("While condition must be bool", "runtime", stmt.condition->line, stmt.condition->column));

            bool cond_value = std::get<bool>(*cond_res);
            if (!cond_value)
                break;

            auto body_res = execute(*stmt.body);
            if (!body_res)
                return std::unexpected(body_res.error());
        }
        return {};
    }

    Result<void> execute_for_stmt(const ast::For_stmt &stmt)
    {
        Environment *saved_env = environment;
        environment = new Environment(environment);

        if (stmt.initializer)
        {
            auto init_res = execute(*stmt.initializer);
            if (!init_res)
            {
                delete environment;
                environment = saved_env;
                return std::unexpected(init_res.error());
            }
        }

        Result<void> loop_result = {};
        while (true)
        {
            if (stmt.condition)
            {
                auto cond_res = evaluate(*stmt.condition);
                if (!cond_res)
                {
                    loop_result = std::unexpected(cond_res.error());
                    break;
                }

                if (!std::holds_alternative<bool>(*cond_res))
                {
                    loop_result =
                        std::unexpected(err::msg("For condition must be bool", "runtime", stmt.condition->line, stmt.condition->column));
                    break;
                }

                bool cond_value = std::get<bool>(*cond_res);
                if (!cond_value)
                    break;
            }

            auto body_res = execute(*stmt.body);
            if (!body_res)
            {
                loop_result = std::unexpected(body_res.error());
                break;
            }

            if (stmt.increment)
            {
                auto inc_res = evaluate(*stmt.increment);
                if (!inc_res)
                {
                    loop_result = std::unexpected(inc_res.error());
                    break;
                }
            }
        }

        delete environment;
        environment = saved_env;
        return loop_result;
    }

    Result<void> execute_block_stmt(const ast::Block_stmt &stmt)
    {
        Environment *saved_env = environment;
        environment = new Environment(environment);

        Result<void> block_result = {};
        for (const auto &s : stmt.statements)
        {
            auto res = execute(*s);
            if (!res)
            {
                block_result = std::unexpected(res.error());
                break;
            }
        }

        delete environment;
        environment = saved_env;
        return block_result;
    }

    Result<void> execute_return_stmt(const ast::Return_stmt &stmt)
    {
        Value value = std::monostate{};  // Default to monostate for void returns

        if (stmt.expression)
        {
            auto eval_res = evaluate(*stmt.expression);
            if (!eval_res)
                return std::unexpected(eval_res.error());
            value = *eval_res;
        }

        // Convert value to string for storage in error message
        std::string value_str = std::visit(
            [](auto &&arg) -> std::string
            {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, std::string>)
                    return arg;
                else if constexpr (std::is_same_v<T, bool>)
                    return arg ? "true" : "false";
                else if constexpr (std::is_same_v<T, std::nullptr_t>)
                    return "null";
                else if constexpr (std::is_same_v<T, std::monostate>)
                    return "void";
                else
                    return std::to_string(arg);
            },
            value);

        return std::unexpected(err::msg(value_str, "return", stmt.line, stmt.column));
    }

    // ===== Expression evaluation =====
    Result<Value> evaluate(const ast::Expr &expr)
    {
        if (const auto *lit = dynamic_cast<const ast::Literal_expr *>(&expr))
            return lit->value;

        if (const auto *var = dynamic_cast<const ast::Variable_expr *>(&expr))
            return lookupVariable(var->name);

        if (const auto *bin = dynamic_cast<const ast::Binary_expr *>(&expr))
            return evaluate_binary_expr(*bin);
        if (const auto *un = dynamic_cast<const ast::Unary_expr *>(&expr))
            return evaluate_unary_expr(*un);
        if (const auto *call = dynamic_cast<const ast::Call_expr *>(&expr))
            return evaluate_call_expr(*call);
        if (const auto *assign = dynamic_cast<const ast::Assignment_expr *>(&expr))
            return evaluate_assignment_expr(*assign);
        if (const auto *cast = dynamic_cast<const ast::Cast_expr *>(&expr))
            return evaluate_cast_expr(*cast);

        return std::unexpected(err::msg("Unknown expression type", "runtime", expr.line, expr.column));
    }

    Result<Value> evaluate_binary_expr(const ast::Binary_expr &expr)
    {
        auto left_res = evaluate(*expr.left);
        if (!left_res)
            return std::unexpected(left_res.error());

        auto right_res = evaluate(*expr.right);
        if (!right_res)
            return std::unexpected(right_res.error());

        return std::visit(
            [&](auto &&left_val, auto &&right_val) -> Result<Value>
            {
                using LeftT = std::decay_t<decltype(left_val)>;
                using RightT = std::decay_t<decltype(right_val)>;

                // Debug: Check what types we're dealing with
                if constexpr (std::is_same_v<LeftT, std::monostate> || std::is_same_v<RightT, std::monostate>)
                {
                    return std::unexpected(
                        err::msg("Cannot perform binary operation on uninitialized value", "runtime", expr.line, expr.column));
                }
                else if constexpr (std::is_same_v<LeftT, std::nullptr_t> || std::is_same_v<RightT, std::nullptr_t>)
                {
                    return std::unexpected(err::msg("Cannot perform binary operation on null value", "runtime", expr.line, expr.column));
                }
                else if constexpr (!util::AllowedType<LeftT> || !util::AllowedType<RightT>)
                {
                    return std::unexpected(err::msg("Invalid operand type for binary operator", "runtime", expr.line, expr.column));
                }
                else if constexpr (std::is_same_v<LeftT, RightT>)
                {
                    util::BinaryOperation<LeftT> op(left_val, right_val, expr.operator_, expr.line, expr.column);
                    return op.execute();
                }
                else
                {
                    return std::unexpected(err::msg("Type mismatch in binary expression", "runtime", expr.line, expr.column));
                }
            },
            *left_res, *right_res);
    }

    Result<Value> evaluate_unary_expr(const ast::Unary_expr &expr)
    {
        auto right_res = evaluate(*expr.right);
        if (!right_res)
            return std::unexpected(right_res.error());

        return std::visit(
            [&](auto &&right_val) -> Result<Value>
            {
                using RightT = std::decay_t<decltype(right_val)>;

                switch (expr.operator_)
                {
                    case lex::TokenType::Minus:
                        if constexpr (std::is_same_v<RightT, int64_t>)
                            return -right_val;
                        else if constexpr (std::is_same_v<RightT, double>)
                            return -right_val;
                        else
                            return std::unexpected(err::msg("Invalid operand for unary -", "runtime", expr.line, expr.column));

                    case lex::TokenType::LogicalNot:
                        if constexpr (std::is_same_v<RightT, bool>)
                            return !right_val;
                        else
                            return std::unexpected(err::msg("Invalid operand for !", "runtime", expr.line, expr.column));

                    default:
                        return std::unexpected(err::msg("Unknown unary operator", "runtime", expr.line, expr.column));
                }
            },
            *right_res);
    }

    Result<Value> evaluate_assignment_expr(const ast::Assignment_expr &expr)
    {
        auto val = evaluate(*expr.value);
        if (!val)
            return std::unexpected(val.error());

        auto assign_res = assignVariable(expr.name, *val);
        if (!assign_res)
            return std::unexpected(assign_res.error());

        return *val;
    }

    Result<Value> evaluate_call_expr(const ast::Call_expr &expr)
    {
        auto it = functions.find(expr.callee);
        if (it == functions.end())
            return std::unexpected(err::msg("Undefined function '" + expr.callee + "'", "runtime", expr.line, expr.column));

        const ast::Function_stmt *func = it->second;
        if (func->parameters.size() != expr.arguments.size())
            return std::unexpected(
                err::msg("Argument count mismatch for function '" + expr.callee + "'", "runtime", expr.line, expr.column));

        // Evaluate arguments in current environment
        std::vector<Value> arg_values;
        for (const auto &arg : expr.arguments)
        {
            auto val = evaluate(*arg);
            if (!val)
                return std::unexpected(val.error());
            arg_values.push_back(*val);
        }

        // Create new environment for function call
        Environment *saved_env = environment;
        environment = new Environment(environment);

        // Bind parameters
        for (size_t i = 0; i < func->parameters.size(); ++i) environment->define(func->parameters[i].first, arg_values[i]);

        // Execute function body
        Value return_value = nullptr;
        bool has_return = false;

        auto func_result = execute(*func->body);
        if (!func_result)
        {
            // Check if this is a return statement
            if (func_result.error().phase == "return")
            {
                has_return = true;
                // For void functions, return monostate instead of nullptr
                if (func->return_type.kind_ == types::kind::Void)
                {
                    return_value = std::monostate{};
                }
                else
                {
                    // Extract return value from error message
                    std::string return_str = func_result.error().message;

                    // Convert string back to appropriate type based on function return type
                    switch (func->return_type.kind_)
                    {
                        case types::kind::Int:
                            return_value = std::stoll(return_str);
                            break;
                        case types::kind::Float:
                            return_value = std::stod(return_str);
                            break;
                        case types::kind::Bool:
                            return_value = (return_str == "true");
                            break;
                        case types::kind::String:
                            return_value = return_str;
                            break;
                        default:
                            delete environment;
                            environment = saved_env;
                            return std::unexpected(err::msg("Invalid return type", "runtime", expr.line, expr.column));
                    }
                }
            }
            else
            {
                // Propagate other errors
                delete environment;
                environment = saved_env;
                return std::unexpected(func_result.error());
            }
        }

        // Check return requirements
        if (func->return_type.kind_ != types::kind::Void && !has_return)
        {
            delete environment;
            environment = saved_env;
            return std::unexpected(err::msg("Missing return statement in non-void function", "runtime", expr.line, expr.column));
        }

        // For non-void functions without explicit return, provide default value
        if (func->return_type.kind_ != types::kind::Void && !has_return)
        {
            switch (func->return_type.kind_)
            {
                case types::kind::Int:
                    return_value = int64_t(0);
                    break;
                case types::kind::Float:
                    return_value = double(0.0);
                    break;
                case types::kind::Bool:
                    return_value = false;
                    break;
                case types::kind::String:
                    return_value = std::string("");
                    break;
                default:
                    delete environment;
                    environment = saved_env;
                    return std::unexpected(err::msg("Invalid return type", "runtime", expr.line, expr.column));
            }
        }

        // For void functions, ensure we return monostate, not nullptr
        if (func->return_type.kind_ == types::kind::Void && !has_return)
            return_value = std::monostate{};

        // Restore environment
        delete environment;
        environment = saved_env;
        return return_value;
    }

    Result<Value> evaluate_cast_expr(const ast::Cast_expr &expr)
    {
        auto val = evaluate(*expr.expression);
        if (!val)
            return std::unexpected(val.error());

        return std::visit(
            [&](auto &&value) -> Result<Value>
            {
                using T = std::decay_t<decltype(value)>;

                switch (expr.target_type.kind_)
                {
                    case types::kind::Int:
                        if constexpr (std::is_same_v<T, int64_t>)
                            return value;
                        else if constexpr (std::is_same_v<T, double>)
                            return static_cast<int64_t>(value);
                        else if constexpr (std::is_same_v<T, bool>)
                            return static_cast<int64_t>(value);
                        else
                            return std::unexpected(err::msg("Cannot cast to int", "runtime", expr.line, expr.column));

                    case types::kind::Float:
                        if constexpr (std::is_same_v<T, int64_t>)
                            return static_cast<double>(value);
                        else if constexpr (std::is_same_v<T, double>)
                            return value;
                        else if constexpr (std::is_same_v<T, bool>)
                            return static_cast<double>(value);
                        else
                            return std::unexpected(err::msg("Cannot cast to float", "runtime", expr.line, expr.column));

                    case types::kind::Bool:
                        if constexpr (std::is_same_v<T, int64_t>)
                            return value != 0;
                        else if constexpr (std::is_same_v<T, double>)
                            return value != 0.0;
                        else if constexpr (std::is_same_v<T, bool>)
                            return value;
                        else
                            return std::unexpected(err::msg("Cannot cast to bool", "runtime", expr.line, expr.column));

                    case types::kind::String:
                        if constexpr (std::is_same_v<T, int64_t>)
                            return std::to_string(value);
                        else if constexpr (std::is_same_v<T, double>)
                            return std::to_string(value);
                        else if constexpr (std::is_same_v<T, bool>)
                            return value ? "true" : "false";
                        else if constexpr (std::is_same_v<T, std::string>)
                            return value;
                        else
                            return std::unexpected(err::msg("Cannot cast to string", "runtime", expr.line, expr.column));

                    default:
                        return std::unexpected(err::msg("Invalid cast target type", "runtime", expr.line, expr.column));
                }
            },
            *val);
    }
};

}  // namespace phos
