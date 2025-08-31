#pragma once

#include "../utility/includes.hpp"
#include "../utility/utility.hpp"

#include "../value/type.hpp"
#include "../error/result.hpp"
#include "../parser/ast.hpp"

namespace phos {
namespace types {

class TypeChecker
{
private:
    std::unordered_map<std::string, types::Type> variables;
    std::unordered_map<std::string, std::pair<std::vector<types::Type>, types::Type>> functions;
    types::Type current_function_return_type = types::Type(types::kind::Void);

public:
    Result<void> check(const std::vector<std::unique_ptr<ast::Stmt>> &statements)
    {
        for (const auto &stmt : statements)
        {
            if (const auto *func_stmt = dynamic_cast<const ast::Function_stmt *>(stmt.get()))
            {
                std::vector<types::Type> param_types;
                for (const auto &param : func_stmt->parameters) param_types.push_back(param.second);
                functions.emplace(func_stmt->name, std::make_pair(param_types, func_stmt->return_type));
            }
        }
        for (const auto &stmt : statements)
        {
            auto result = check_stmt(*stmt);
            if (!result)
                return std::unexpected(result.error());
        }
        return {};
    }

private:
    Result<void> check_stmt(const ast::Stmt &stmt)
    {
        if (const auto *var_stmt = dynamic_cast<const ast::Var_stmt *>(&stmt))
            return check_var_stmt(*var_stmt);
        if (const auto *print_stmt = dynamic_cast<const ast::Print_stmt *>(&stmt))
            return check_print_stmt(*print_stmt);
        if (const auto *expr_stmt = dynamic_cast<const ast::Expr_stmt *>(&stmt))
            return check_expr_stmt(*expr_stmt);
        if (const auto *if_stmt = dynamic_cast<const ast::If_stmt *>(&stmt))
            return check_if_stmt(*if_stmt);
        if (const auto *while_stmt = dynamic_cast<const ast::While_stmt *>(&stmt))
            return check_while_stmt(*while_stmt);
        if (const auto *for_stmt = dynamic_cast<const ast::For_stmt *>(&stmt))
            return check_for_stmt(*for_stmt);
        if (const auto *block_stmt = dynamic_cast<const ast::Block_stmt *>(&stmt))
            return check_block_stmt(*block_stmt);
        if (const auto *func_stmt = dynamic_cast<const ast::Function_stmt *>(&stmt))
            return check_func_stmt(*func_stmt);
        if (const auto *return_stmt = dynamic_cast<const ast::Return_stmt *>(&stmt))
            return check_return_stmt(*return_stmt);
        return std::unexpected(err::msg("Unknown statement type", "type_checker", stmt.line, stmt.column));
    }
    Result<void> check_var_stmt(const ast::Var_stmt &stmt)
    {
        if (stmt.initializer)
        {
            auto init_type = check_expr(*stmt.initializer);
            if (!init_type)
                return std::unexpected(init_type.error());
            if (*init_type != stmt.type)
                return std::unexpected(
                    err::msg(std::format("Type mismatch: cannot assign {} to {}", init_type->to_string(), stmt.type.to_string()),
                          "type_checker", stmt.line, stmt.column));
        }
        variables[stmt.name] = stmt.type;
        return {};
    }
    Result<void> check_print_stmt(const ast::Print_stmt &stmt)
    {
        auto expr_type = check_expr(*stmt.expression);
        if (!expr_type)
            return std::unexpected(expr_type.error());
        return {};
    }
    Result<void> check_expr_stmt(const ast::Expr_stmt &stmt)
    {
        auto expr_type = check_expr(*stmt.expression);
        if (!expr_type)
            return std::unexpected(expr_type.error());
        return {};
    }
    Result<void> check_if_stmt(const ast::If_stmt &stmt)
    {
        auto cond_type = check_expr(*stmt.condition);
        if (!cond_type)
            return std::unexpected(cond_type.error());
        if (cond_type->kind_ != types::kind::Bool)
            return std::unexpected(err::msg("If condition must be boolean", "type_checker", stmt.condition->line, stmt.condition->column));
        auto then_result = check_stmt(*stmt.then_branch);
        if (!then_result)
            return std::unexpected(then_result.error());
        if (stmt.else_branch)
        {
            auto else_result = check_stmt(*stmt.else_branch);
            if (!else_result)
                return std::unexpected(else_result.error());
        }
        return {};
    }
    Result<void> check_while_stmt(const ast::While_stmt &stmt)
    {
        auto cond_type = check_expr(*stmt.condition);
        if (!cond_type)
            return std::unexpected(cond_type.error());
        if (cond_type->kind_ != types::kind::Bool)
            return std::unexpected(err::msg("While condition must be boolean", "type_checker", stmt.condition->line, stmt.condition->column));
        auto body_result = check_stmt(*stmt.body);
        if (!body_result)
            return std::unexpected(body_result.error());
        return {};
    }
    Result<void> check_for_stmt(const ast::For_stmt &stmt)
    {
        auto saved_variables = variables;
        if (stmt.initializer)
        {
            auto init_result = check_stmt(*stmt.initializer);
            if (!init_result)
            {
                variables = saved_variables;
                return std::unexpected(init_result.error());
            }
        }
        if (stmt.condition)
        {
            auto cond_type = check_expr(*stmt.condition);
            if (!cond_type)
            {
                variables = saved_variables;
                return std::unexpected(cond_type.error());
            }
            if (cond_type->kind_ != types::kind::Bool)
            {
                variables = saved_variables;
                return std::unexpected(
                    err::msg("For condition must be boolean", "type_checker", stmt.condition->line, stmt.condition->column));
            }
        }
        if (stmt.increment)
        {
            auto inc_type = check_expr(*stmt.increment);
            if (!inc_type)
            {
                variables = saved_variables;
                return std::unexpected(inc_type.error());
            }
        }
        auto body_result = check_stmt(*stmt.body);
        if (!body_result)
        {
            variables = saved_variables;
            return std::unexpected(body_result.error());
        }
        variables = saved_variables;
        return {};
    }
    Result<void> check_block_stmt(const ast::Block_stmt &stmt)
    {
        auto saved_variables = variables;
        for (const auto &statement : stmt.statements)
        {
            auto result = check_stmt(*statement);
            if (!result)
            {
                variables = saved_variables;
                return std::unexpected(result.error());
            }
        }
        variables = saved_variables;
        return {};
    }
    Result<void> check_func_stmt(const ast::Function_stmt &stmt)
    {
        auto saved_variables = variables;
        auto saved_return_type = current_function_return_type;
        current_function_return_type = stmt.return_type;
        for (const auto &param : stmt.parameters) variables.emplace(param.first, param.second);
        auto body_result = check_stmt(*stmt.body);
        if (!body_result)
        {
            variables = saved_variables;
            current_function_return_type = saved_return_type;
            return std::unexpected(body_result.error());
        }
        variables = saved_variables;
        current_function_return_type = saved_return_type;
        return {};
    }
    Result<void> check_return_stmt(const ast::Return_stmt &stmt)
    {
        if (stmt.expression)
        {
            auto expr_type = check_expr(*stmt.expression);
            if (!expr_type)
                return std::unexpected(expr_type.error());
            if (*expr_type != current_function_return_type)
                return std::unexpected(err::msg(std::format("Return type mismatch: expected {}, got {}",
                                                         current_function_return_type.to_string(), expr_type->to_string()),
                                             "type_checker", stmt.line, stmt.column));
        }
        else
        {
            if (current_function_return_type.kind_ != types::kind::Void)
                return std::unexpected(
                    err::msg(std::format("Function expects return type {}, but got void return", current_function_return_type.to_string()),
                          "type_checker", stmt.line, stmt.column));
        }
        return {};
    }
    // Strict type checking for string concatenation
    Result<types::Type> check_expr(const ast::Expr &expr)
    {
        if (const auto *literal = dynamic_cast<const ast::Literal_expr *>(&expr))
            return literal->type;
        if (const auto *variable = dynamic_cast<const ast::Variable_expr *>(&expr))
        {
            auto it = variables.find(variable->name);
            if (it == variables.end())
                return std::unexpected(
                    err::msg(std::format("Undefined variable '{}'", variable->name), "type_checker", variable->line, variable->column));
            return it->second;
        }
        if (const auto *binary = dynamic_cast<const ast::Binary_expr *>(&expr))
            return check_binary_expr(*binary);
        if (const auto *unary = dynamic_cast<const ast::Unary_expr *>(&expr))
            return check_unary_expr(*unary);
        if (const auto *call = dynamic_cast<const ast::Call_expr *>(&expr))
            return check_call_expr(*call);
        if (const auto *assignment = dynamic_cast<const ast::Assignment_expr *>(&expr))
            return check_assignment_expr(*assignment);
        if (const auto *cast = dynamic_cast<const ast::Cast_expr *>(&expr))
            return check_cast_expr(*cast);
        return std::unexpected(err::msg("Unknown expression type", "type_checker", expr.line, expr.column));
    }
    Result<types::Type> check_binary_expr(const ast::Binary_expr &expr)
    {
        auto left_type = check_expr(*expr.left);
        if (!left_type)
            return std::unexpected(left_type.error());
        auto right_type = check_expr(*expr.right);
        if (!right_type)
            return std::unexpected(right_type.error());
        switch (expr.operator_)
        {
            case lex::TokenType::Plus:
                if (left_type->kind_ == types::kind::String || right_type->kind_ == types::kind::String)
                {
                    // Strict: Only string + string allowed for concatenation
                    if (left_type->kind_ == types::kind::String && right_type->kind_ == types::kind::String)
                        return types::Type(types::kind::String);
                    return std::unexpected(err::msg(std::format("Operator '+' requires both operands to be strings for concatenation"),
                                                 "type_checker", expr.line, expr.column));
                }
                if (left_type->kind_ == types::kind::Int && right_type->kind_ == types::kind::Int)
                    return types::Type(types::kind::Int);
                if (left_type->kind_ == types::kind::Float && right_type->kind_ == types::kind::Float)
                    return types::Type(types::kind::Float);
                return std::unexpected(err::msg(std::format("Operator '+' requires both operands to be numbers or strings"), "type_checker",
                                             expr.line, expr.column));
            case lex::TokenType::Minus:
            case lex::TokenType::Star:
            case lex::TokenType::Slash:
            case lex::TokenType::Percent:
                if (left_type->kind_ == types::kind::Int && right_type->kind_ == types::kind::Int)
                    return types::Type(types::kind::Int);
                if (left_type->kind_ == types::kind::Float && right_type->kind_ == types::kind::Float)
                    return types::Type(types::kind::Float);
                return std::unexpected(
                    err::msg(std::format("Operator '{}' requires both operands to be numbers", token_type_to_string(expr.operator_)),
                          "type_checker", expr.line, expr.column));
            case lex::TokenType::Equal:
            case lex::TokenType::NotEqual:
            case lex::TokenType::Less:
            case lex::TokenType::Greater:
            case lex::TokenType::LessEqual:
            case lex::TokenType::GreaterEqual:
                if (*left_type != *right_type)
                    return std::unexpected(
                        err::msg(std::format("Comparison operator '{}' requires operands of same type", token_type_to_string(expr.operator_)),
                              "type_checker", expr.line, expr.column));
                return types::Type(types::kind::Bool);
            case lex::TokenType::LogicalAnd:
            case lex::TokenType::LogicalOr:
                if (left_type->kind_ == types::kind::Bool && right_type->kind_ == types::kind::Bool)
                    return types::Type(types::kind::Bool);
                return std::unexpected(
                    err::msg(std::format("Operator '{}' requires both operands to be boolean", token_type_to_string(expr.operator_)),
                          "type_checker", expr.line, expr.column));
            default:
                return std::unexpected(err::msg(std::format("Invalid binary operator '{}'", token_type_to_string(expr.operator_)),
                                             "type_checker", expr.line, expr.column));
        }
    }
    Result<types::Type> check_unary_expr(const ast::Unary_expr &expr)
    {
        auto right_type = check_expr(*expr.right);
        if (!right_type)
            return std::unexpected(right_type.error());
        switch (expr.operator_)
        {
            case lex::TokenType::Minus:
                if (right_type->kind_ == types::kind::Int || right_type->kind_ == types::kind::Float)
                    return *right_type;
                return std::unexpected(err::msg("Unary minus requires numeric operand", "type_checker", expr.line, expr.column));
            case lex::TokenType::LogicalNot:
                if (right_type->kind_ == types::kind::Bool)
                    return types::Type(types::kind::Bool);
                return std::unexpected(err::msg("Logical not requires boolean operand", "type_checker", expr.line, expr.column));
            default:
                return std::unexpected(err::msg(std::format("Invalid unary operator '{}'", token_type_to_string(expr.operator_)),
                                             "type_checker", expr.line, expr.column));
        }
    }
    Result<types::Type> check_call_expr(const ast::Call_expr &expr)
    {
        auto func_it = functions.find(expr.callee);
        if (func_it == functions.end())
            return std::unexpected(err::msg(std::format("Undefined function '{}'", expr.callee), "type_checker", expr.line, expr.column));
        const auto &[expected_param_types, return_type] = func_it->second;
        if (expr.arguments.size() != expected_param_types.size())
            return std::unexpected(err::msg(std::format("Function '{}' expects {} arguments but got {}", expr.callee,
                                                     expected_param_types.size(), expr.arguments.size()),
                                         "type_checker", expr.line, expr.column));
        for (size_t i = 0; i < expr.arguments.size(); i++)
        {
            auto arg_type = check_expr(*expr.arguments[i]);
            if (!arg_type)
                return std::unexpected(arg_type.error());
            if (*arg_type != expected_param_types[i])
                return std::unexpected(err::msg(std::format("Argument {} type mismatch: expected {}, got {}", i + 1,
                                                         expected_param_types[i].to_string(), arg_type->to_string()),
                                             "type_checker", expr.arguments[i]->line, expr.arguments[i]->column));
        }
        return return_type;
    }
    Result<types::Type> check_assignment_expr(const ast::Assignment_expr &expr)
    {
        auto it = variables.find(expr.name);
        if (it == variables.end())
            return std::unexpected(err::msg(std::format("Undefined variable '{}'", expr.name), "type_checker", expr.line, expr.column));
        auto value_type = check_expr(*expr.value);
        if (!value_type)
            return std::unexpected(value_type.error());
        if (it->second != *value_type)
            return std::unexpected(err::msg(
                std::format("Cannot assign {} to variable '{}' of type {}", value_type->to_string(), expr.name, it->second.to_string()),
                "type_checker", expr.line, expr.column));
        return it->second;
    }

    Result<types::Type> check_cast_expr(const ast::Cast_expr &expr)
    {
        auto source_type = check_expr(*expr.expression);
        if (!source_type)
            return std::unexpected(source_type.error());
        if ((source_type->kind_ == types::kind::Int   && expr.target_type.kind_ == types::kind::Float)  ||
            (source_type->kind_ == types::kind::Float && expr.target_type.kind_ == types::kind::Int)    ||
            (source_type->kind_ == types::kind::Bool  && expr.target_type.kind_ == types::kind::Int)    ||
            (source_type->kind_ == types::kind::Int   && expr.target_type.kind_ == types::kind::Bool)   ||
            (source_type->kind_ == types::kind::Int   && expr.target_type.kind_ == types::kind::String) ||
            (source_type->kind_ == types::kind::Float && expr.target_type.kind_ == types::kind::String) ||
            (source_type->kind_ == types::kind::Bool  && expr.target_type.kind_ == types::kind::String))
        {
            return expr.target_type;
        }
        if (source_type->kind_ == expr.target_type.kind_)
            return expr.target_type;
        return std::unexpected(err::msg(std::format("Cannot cast from {} to {}", source_type->to_string(), expr.target_type.to_string()),
                                     "type_checker", expr.line, expr.column));
    }
    std::string token_type_to_string(lex::TokenType type) const
    {
        return util::operator_token_to_string(type);
    }
};

} // namespace types
} // namespace phos
