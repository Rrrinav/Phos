#pragma once

#include <cstddef>
#include <cstdio>
#include <print>
#include <string>
#include <variant>
#include <vector>
#include <unordered_map>
#include <memory>
#include <expected>

#include "../utility/utility.hpp"
#include "environment.hpp"
#include "../parser/ast.hpp"

namespace phos
{

class Interpreter
{
private:
    Environment *environment = nullptr;
    std::unordered_map<std::string, const ast::Function_stmt *> functions;
    std::string filename;

    // Structure to represent return values
    struct Value_result
    {
        Value value;
        bool has_return;

        Value_result() : value(std::monostate{}), has_return(false) {}
        explicit Value_result(Value val) : value(std::move(val)), has_return(true) {}

        explicit operator bool() const { return has_return; }
    };

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

    std::unordered_map<std::string, Value> get_variables() { return environment->vars; }

    std::unordered_map<std::string, const ast::Function_stmt *> get_functions() { return this->functions; }

    Result<void> interpret(const std::vector<std::unique_ptr<ast::Stmt>> &statements)
    {
        // Register all functions first
        for (const auto &stmt : statements)
            if (const auto *func_stmt = std::get_if<ast::Function_stmt>(&stmt->node))
                functions.emplace(func_stmt->name, func_stmt);

        // Execute non-function statements
        for (const auto &stmt : statements)
        {
            if (std::get_if<ast::Function_stmt>(&stmt->node))
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

    Result<Value> lookup_variable(const std::string &name, const ast::Source_location &loc)
    {
        if (auto *val = environment->lookup(name))
            return *val;
        return std::unexpected(err::msg("Undefined variable '" + name + "'", "runtime", loc.line, loc.column, filename));
    }

    // ===== Statement execution =====
    Result<Value_result> execute(const ast::Stmt &stmt)
    {
        return std::visit(
            [this, &stmt](const auto &s) -> Result<Value_result>
            {
                using T = std::decay_t<decltype(s)>;
                if constexpr (std::is_same_v<T, ast::Var_stmt>)
                    return execute_var_stmt(s);
                else if constexpr (std::is_same_v<T, ast::Print_stmt>)
                    return execute_print_stmt(s);
                else if constexpr (std::is_same_v<T, ast::Expr_stmt>)
                    return execute_expr_stmt(s);
                else if constexpr (std::is_same_v<T, ast::If_stmt>)
                    return execute_if_stmt(s);
                else if constexpr (std::is_same_v<T, ast::While_stmt>)
                    return execute_while_stmt(s);
                else if constexpr (std::is_same_v<T, ast::Block_stmt>)
                    return execute_block_stmt(s);
                else if constexpr (std::is_same_v<T, ast::Return_stmt>)
                    return execute_return_stmt(s);
                else if constexpr (std::is_same_v<T, ast::Function_stmt>)
                    return Value_result{};  // Functions are handled in interpret()
                else
                    return std::unexpected(err::msg("Unknown statement type", "runtime", ast::loc_copy(stmt.node).line, ast::loc_copy(stmt.node).column));
            },
            stmt.node);
    }

    Result<Value_result> execute_var_stmt(const ast::Var_stmt &stmt)
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
                    value = std::monostate{};
                    break;
                default:
                    return std::unexpected(err::msg("Cannot default-initialize variable", "runtime", stmt.loc.line, stmt.loc.column));
            }
        }

        auto declare_res = declareVariable(stmt.name, std::move(value));
        if (!declare_res)
            return std::unexpected(declare_res.error());

        return Value_result{};
    }

    Result<Value_result> execute_print_stmt(const ast::Print_stmt &stmt)
    {
        auto stream = (stmt.stream == ast::Print_stream::STDOUT) ? stdout : stderr;

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
                    std::println(stream, "null");
                else if constexpr (std::is_same_v<T, std::monostate>)
                    std::println(stream, "monostate: uninitialized");
                else
                    std::println(stream, "{}", arg);
            },
            *eval_res);

        return Value_result{};
    }

    Result<Value_result> execute_expr_stmt(const ast::Expr_stmt &stmt)
    {
        auto eval_res = evaluate(*stmt.expression);
        if (!eval_res)
            return std::unexpected(eval_res.error());
        return Value_result{};
    }

    Result<Value_result> execute_if_stmt(const ast::If_stmt &stmt)
    {
        auto cond_res = evaluate(*stmt.condition);
        if (!cond_res)
            return std::unexpected(cond_res.error());

        if (!std::holds_alternative<bool>(*cond_res))
            return std::unexpected(err::msg("If condition must be bool", "runtime", ast::loc(stmt.condition->node).line, ast::loc(stmt.condition->node).column));


        bool cond_value = std::get<bool>(*cond_res);

        if (cond_value)
            return execute(*stmt.then_branch);
        else if (stmt.else_branch)
            return execute(*stmt.else_branch);

        return Value_result{};
    }

    Result<Value_result> execute_while_stmt(const ast::While_stmt &stmt)
    {
        while (true)
        {
            auto cond_res = evaluate(*stmt.condition);
            if (!cond_res)
                return std::unexpected(cond_res.error());

            if (!std::holds_alternative<bool>(*cond_res))
                return std::unexpected(
                    err::msg("While condition must be bool", "runtime", ast::loc(stmt.condition->node).line, ast::loc(stmt.condition->node).column));

            bool cond_value = std::get<bool>(*cond_res);
            if (!cond_value)
                break;

            auto body_res = execute(*stmt.body);
            if (!body_res)
                return std::unexpected(body_res.error());

            // Check if return statement was encountered
            if (body_res->has_return)
                return *body_res;
        }
        return Value_result{};
    }

    Result<Value_result> execute_block_stmt(const ast::Block_stmt &stmt)
    {
        Environment *saved_env = environment;
        environment = new Environment(environment);

        Result<Value_result> block_result = Value_result{};
        for (const auto &s : stmt.statements)
        {
            auto res = execute(*s);
            if (!res)
            {
                block_result = std::unexpected(res.error());
                break;
            }

            // Check if return statement was encountered
            if (res->has_return)
            {
                block_result = *res;
                break;
            }
        }

        delete environment;
        environment = saved_env;
        return block_result;
    }

    Result<Value_result> execute_return_stmt(const ast::Return_stmt &stmt)
    {
        Value value = std::monostate{};  // Default to monostate for void returns

        if (stmt.expression)
        {
            auto eval_res = evaluate(*stmt.expression);
            if (!eval_res)
                return std::unexpected(eval_res.error());
            value = *eval_res;
        }

        return Value_result{value};
    }

    // ===== Expression evaluation =====
    Result<Value> evaluate(const ast::Expr &expr)
    {
        return std::visit(
            [this, &expr](const auto &e) -> Result<Value>
            {
                using T = std::decay_t<decltype(e)>;

                if constexpr (std::is_same_v<T, ast::Literal_expr>)
                    return e.value;
                else if constexpr (std::is_same_v<T, ast::Variable_expr>)
                    return lookup_variable(e.name, ast::loc_copy(expr.node));
                else if constexpr (std::is_same_v<T, ast::Binary_expr>)
                    return evaluate_binary_expr(e);
                else if constexpr (std::is_same_v<T, ast::Unary_expr>)
                    return evaluate_unary_expr(e);
                else if constexpr (std::is_same_v<T, ast::Call_expr>)
                    return evaluate_call_expr(e);
                else if constexpr (std::is_same_v<T, ast::Assignment_expr>)
                    return evaluate_assignment_expr(e);
                else if constexpr (std::is_same_v<T, ast::Cast_expr>)
                    return evaluate_cast_expr(e);
                else
                    return std::unexpected(err::msg("Unknown expression type", "runtime", ast::loc_copy(expr.node).line, ast::loc_copy(expr.node).column));
            },
            expr.node);
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

                if constexpr (std::is_same_v<LeftT, std::monostate>)
                    return std::unexpected(err::msg("Cannot perform binary operation on uninitialized value", "runtime",
                                                    ast::loc(expr.left->node).line, ast::loc(expr.left->node).column));
                if constexpr (std::is_same_v<RightT, std::monostate>)
                    return std::unexpected(err::msg("Cannot perform binary operation on uninitialized value", "runtime",
                                                    ast::loc(expr.right->node).line, ast::loc(expr.right->node).column));
                else if constexpr (std::is_same_v<LeftT, std::nullptr_t>)
                    return std::unexpected(
                        err::msg("Cannot perform binary operation on null value", "runtime", ast::loc(expr.left->node).line, ast::loc(expr.left->node).column));
                else if constexpr (std::is_same_v<RightT, std::nullptr_t>)
                    return std::unexpected(
                        err::msg("Cannot perform binary operation on null value", "runtime",ast::loc(expr.right->node).line, ast::loc(expr.right->node).column));
                else if constexpr (!util::AllowedType<LeftT>)
                    return std::unexpected(
                        err::msg("Invalid operand type for binary operator", "runtime", ast::loc(expr.left->node).line, ast::loc(expr.left->node).column));
                else if constexpr (!util::AllowedType<RightT>)
                    return std::unexpected(
                        err::msg("Invalid operand type for binary operator", "runtime", ast::loc(expr.right->node).line, ast::loc(expr.right->node).column));
                else if constexpr (std::is_same_v<LeftT, RightT>)
                    return util::BinaryOperation<LeftT>(left_val, right_val, expr.op, expr.loc.line, expr.loc.column).execute();
                else
                    return std::unexpected(err::msg("Type mismatch in binary expression", "runtime", expr.loc.line, expr.loc.column));
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

                switch (expr.op)
                {
                    case lex::TokenType::Minus:
                        if constexpr (std::is_same_v<RightT, int64_t>)
                            return -right_val;
                        else if constexpr (std::is_same_v<RightT, double>)
                            return -right_val;
                        else
                            return std::unexpected(
                                err::msg("Invalid operand for unary -", "runtime", ast::loc(expr.right->node).line, ast::loc(expr.right->node).column));

                    case lex::TokenType::LogicalNot:
                        if constexpr (std::is_same_v<RightT, bool>)
                            return !right_val;
                        else
                            return std::unexpected(
                                err::msg("Invalid operand for !", "runtime", ast::loc(expr.right->node).line, ast::loc(expr.right->node).column));

                    default:
                        return std::unexpected(err::msg("Unknown unary operator", "runtime", expr.loc.line, expr.loc.column));
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
            return std::unexpected(err::msg("Undefined function '" + expr.callee + "'", "runtime", expr.loc.line, expr.loc.column));

        const ast::Function_stmt *func = it->second;
        if (func->parameters.size() != expr.arguments.size())
            return std::unexpected(err::msg("Argument count mismatch for function '" + expr.callee + "'", "runtime", expr.loc.line, expr.loc.column));

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
        Value return_value = std::monostate{};
        bool has_return = false;

        auto func_result = execute(*func->body);
        if (!func_result)
        {
            delete environment;
            environment = saved_env;
            return std::unexpected(func_result.error());
        }

        // Check if function returned a value
        if (func_result->has_return)
        {
            return_value = func_result->value;
            has_return = true;
        }

        // Check return requirements
        if (func->return_type.kind_ != types::kind::Void && !has_return)
        {
            delete environment;
            environment = saved_env;
            return std::unexpected(err::msg("Missing return statement in non-void function", "runtime", ast::loc(func->body->node).line, ast::loc(func->body->node).column));
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
                    return std::unexpected(err::msg("Invalid return type", "runtime", expr.loc.line, expr.loc.column));
            }
        }

        // For void functions, ensure we return monostate
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

                switch (expr.type.kind_)
                {
                    case types::kind::Int:
                        if constexpr (std::is_same_v<T, int64_t>)
                            return value;
                        else if constexpr (std::is_same_v<T, double>)
                            return static_cast<int64_t>(value);
                        else if constexpr (std::is_same_v<T, bool>)
                            return static_cast<int64_t>(value);
                        else
                            return std::unexpected(err::msg("Cannot cast to int", "runtime", ast::loc(expr.expression->node).line, ast::loc(expr.expression->node).column));

                    case types::kind::Float:
                        if constexpr (std::is_same_v<T, int64_t>)
                            return static_cast<double>(value);
                        else if constexpr (std::is_same_v<T, double>)
                            return value;
                        else if constexpr (std::is_same_v<T, bool>)
                            return static_cast<double>(value);
                        else
                            return std::unexpected(err::msg("Cannot cast to float", "runtime", ast::loc(expr.expression->node).line, ast::loc(expr.expression->node).column));

                    case types::kind::Bool:
                        if constexpr (std::is_same_v<T, int64_t>)
                            return value != 0;
                        else if constexpr (std::is_same_v<T, double>)
                            return value != 0.0;
                        else if constexpr (std::is_same_v<T, bool>)
                            return value;
                        else
                            return std::unexpected(err::msg("Cannot cast to bool", "runtime", ast::loc(expr.expression->node).line, ast::loc(expr.expression->node).column));

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
                            return std::unexpected(err::msg("Cannot cast to string", "runtime", ast::loc(expr.expression->node).line, ast::loc(expr.expression->node).column));

                    default:
                        return std::unexpected(err::msg("Invalid cast target type", "runtime", ast::loc(expr.expression->node).line, ast::loc(expr.expression->node).column));
                }
            },
            *val);
    }
};

}  // namespace phos
