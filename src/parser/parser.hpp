#pragma once

#include <string>
#include <vector>
#include <variant>
#include <unordered_map>
#include <memory>
#include <expected>
#include <expected>

#include "../lexer/token.hpp"
#include "../value/type.hpp"
#include "../error/err.hpp"
#include "../error/result.hpp"
#include "ast.hpp"

namespace phos {

class Parser
{
    std::vector<lex::Token> tokens;
    size_t current = 0;
    std::unordered_map<std::string, types::Type> variable_types;
    std::unordered_map<std::string, types::Type> function_types;

public:
    explicit Parser(std::vector<lex::Token> t) : tokens(std::move(t)) {}

    void skip_newlines()
    {
        while (!is_at_end() && match({lex::TokenType::Newline})) {} // Keep consuming newline tokens
    }

    // Parses the entire program into a vector of statements
    Result<std::vector<std::unique_ptr<ast::Stmt>>> parse()
    {
        std::vector<std::unique_ptr<ast::Stmt>> statements;
        while (!is_at_end())
        {
            skip_newlines();
            if (is_at_end())
                break;

            auto decl_result = declaration();
            if (!decl_result)
            {
                if (decl_result.error().message.empty())
                {
                    // Skip this token and continue
                    advance();
                    continue;
                }
                return std::unexpected(decl_result.error());
            }

            if (decl_result.value())
            {
                statements.push_back(std::move(*decl_result.value()));
            }
        }
        return statements;
    }

private:
    bool is_at_end() const { return current >= tokens.size() || peek().type == lex::TokenType::Eof; }

    const lex::Token &peek() const
    {
        if (current >= tokens.size())
        {
            static lex::Token eof_token(lex::TokenType::Eof, "", std::monostate{}, 0, 0);
            return eof_token;
        }
        return tokens[current];
    }

    const lex::Token &previous() const
    {
        if (current == 0)
            return tokens[0];
        return tokens[current - 1];
    }

    lex::Token advance()
    {
        if (!is_at_end())
            current++;
        return previous();
    }

    bool check(lex::TokenType type) const { return !is_at_end() && peek().type == type; }

    bool match(std::initializer_list<lex::TokenType> types)
    {
        for (auto type : types)
        {
            if (check(type))
            {
                advance();
                return true;
            }
        }
        return false;
    }

    Result<lex::Token> consume(lex::TokenType type, const std::string &message)
    {
        if (check(type))
            return advance();
        return std::unexpected(err::msg(message, "parser", peek().line, peek().column));
    }

    err::msg create_error(const lex::Token &token, const std::string &message)
    {
        return err::msg(message, "parser", token.line, token.column);
    }

    void synchronize()
    {
        advance();
        while (!is_at_end())
        {
            if (previous().type == lex::TokenType::Semicolon)
                return;
            switch (peek().type)
            {
                case lex::TokenType::Fn:
                case lex::TokenType::Let:
                case lex::TokenType::For:
                case lex::TokenType::If:
                case lex::TokenType::While:
                case lex::TokenType::Print:
                case lex::TokenType::Return:
                    return;
                default:
                    advance();
            }
        }
    }

    Result<std::optional<std::unique_ptr<ast::Stmt>>> declaration()
    {
        skip_newlines();
        if (is_at_end())
            return std::optional<std::unique_ptr<ast::Stmt>>{std::nullopt};

        if (match({lex::TokenType::Fn}))
        {
            auto func_result = function_declaration();
            if (!func_result)
            {
                synchronize();
                return std::unexpected(func_result.error());
            }
            return std::optional<std::unique_ptr<ast::Stmt>>{std::move(func_result.value())};
        }

        if (match({lex::TokenType::Let}))
        {
            auto var_result = var_declaration();
            if (!var_result)
            {
                synchronize();
                return std::unexpected(var_result.error());
            }
            return std::optional<std::unique_ptr<ast::Stmt>>{std::move(var_result.value())};
        }

        auto stmt_result = statement();
        if (!stmt_result)
        {
            synchronize();
            return std::unexpected(stmt_result.error());
        }
        return std::optional<std::unique_ptr<ast::Stmt>>{std::move(stmt_result.value())};
    }

    Result<std::unique_ptr<ast::Stmt>> function_declaration()
    {
        auto name_result = consume(lex::TokenType::Identifier, "Expect function name");
        if (!name_result) return std::unexpected(name_result.error());
        lex::Token name = name_result.value();

        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after function name");
        if (!left_paren_result) return std::unexpected(left_paren_result.error());

        std::vector<std::pair<std::string, types::Type>> parameters;
        if (!check(lex::TokenType::RightParen))
        {
            do {
                auto param_name_result = consume(lex::TokenType::Identifier, "Expect parameter name");
                if (!param_name_result) return std::unexpected(param_name_result.error());

                auto colon_result = consume(lex::TokenType::Colon, "Expect ':' after parameter name");
                if (!colon_result) return std::unexpected(colon_result.error());

                auto param_type_result = parse_type();
                if (!param_type_result) return std::unexpected(param_type_result.error());

                parameters.emplace_back(param_name_result.value().lexeme, param_type_result.value());
            } while (match({lex::TokenType::Comma}));
        }

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after parameters");
        if (!right_paren_result) return std::unexpected(right_paren_result.error());

        types::Type return_type = types::Type(types::kind::Void);
        if (match({lex::TokenType::Arrow}))
        {
            auto return_type_result = parse_type();
            if (!return_type_result) return std::unexpected(return_type_result.error());
            return_type = return_type_result.value();
        }

        auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' before function body");
        if (!left_brace_result) return std::unexpected(left_brace_result.error());

        auto body_result = block_statement();
        if (!body_result) return std::unexpected(body_result.error());

        function_types.emplace(name.lexeme, return_type);
        return std::make_unique<ast::Function_stmt>(name.lexeme, parameters, return_type, std::move(body_result.value()), name.line, name.column);
    }

    Result<std::unique_ptr<ast::Stmt>> var_declaration()
    {
        auto name_result = consume(lex::TokenType::Identifier, "Expect variable name");
        if (!name_result)
            return std::unexpected(name_result.error());
        lex::Token name = name_result.value();

        types::Type var_type = types::Type(types::kind::Void);
        std::unique_ptr<ast::Expr> initializer;

        // Check for walrus operator (:=) for type inference
        if (match({lex::TokenType::Colon}))
        {
            if (match({lex::TokenType::Assign}))
            {
                // Walrus operator := - infer type from expression
                auto init_result = expression();
                if (!init_result)
                    return std::unexpected(init_result.error());
                initializer = std::move(init_result.value());

                // Infer type from the initializer expression
                var_type = initializer->get_type();
            }
            else
            {
                // Regular type annotation: let x: int
                auto type_result = parse_type();
                if (!type_result)
                    return std::unexpected(type_result.error());
                var_type = type_result.value();

                // Check for optional assignment after type
                if (match({lex::TokenType::Assign}))
                {
                    auto init_result = expression();
                    if (!init_result)
                        return std::unexpected(init_result.error());
                    initializer = std::move(init_result.value());
                }
            }
        }

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expected ';' after variable declaration");
        if (!semicolon_result)
            return std::unexpected(semicolon_result.error());

        // Add variable to type tracking
        variable_types[name.lexeme] = var_type;

        return std::make_unique<ast::Var_stmt>(name.lexeme, var_type, std::move(initializer), name.line, name.column);
    }

    Result<std::unique_ptr<ast::Stmt>> statement()
    {
        skip_newlines();
        if (is_at_end())
            return std::unexpected(create_error(peek(), "Unexpected end of file"));

        if (match({lex::TokenType::Print}))
            return print_statement();
        if (match({lex::TokenType::LeftBrace}))
            return block_statement();
        if (match({lex::TokenType::If}))
            return if_statement();
        if (match({lex::TokenType::While}))
            return while_statement();
        if (match({lex::TokenType::For}))
            return for_statement();
        if (match({lex::TokenType::Return}))
            return return_statement();
        return expression_statement();
    }

    Result<std::unique_ptr<ast::Stmt>> print_statement()
    {
        auto left_paren = consume(lex::TokenType::LeftParen, "Expected '(' after print statement");
        if (!left_paren) return std::unexpected(left_paren.error());

        auto expr_result = expression();
        if (!expr_result) return std::unexpected(expr_result.error());

        auto right_paren = consume(lex::TokenType::RightParen, "Expected ')' after value");
        if (!right_paren) return std::unexpected(right_paren.error());

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after print statement");
        if (!semicolon_result) return std::unexpected(semicolon_result.error());

        return std::make_unique<ast::Print_stmt>(std::move(expr_result.value()), previous().line, previous().column);
    }

    Result<std::unique_ptr<ast::Stmt>> block_statement()
    {
        std::vector<std::unique_ptr<ast::Stmt>> statements;
        size_t line = previous().line;
        size_t column = previous().column;

        // Save current variable types
        auto saved_variable_types = variable_types;

        while (!check(lex::TokenType::RightBrace) && !is_at_end())
        {
            auto stmt_result = declaration();
            if (!stmt_result)
            {
                variable_types = saved_variable_types;
                return std::unexpected(stmt_result.error());
            }

            if (stmt_result.value())
                statements.push_back(std::move(*stmt_result.value()));
        }

        auto right_brace_result = consume(lex::TokenType::RightBrace, "Expect '}' after block");
        if (!right_brace_result) 
        {
            variable_types = saved_variable_types;
            return std::unexpected(right_brace_result.error());
        }

        // Restore variable types (block creates a new scope)
        variable_types = saved_variable_types;

        return std::make_unique<ast::Block_stmt>(std::move(statements), line, column);
    }

    Result<std::unique_ptr<ast::Stmt>> if_statement()
    {
        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after 'if'");
        if (!left_paren_result) return std::unexpected(left_paren_result.error());

        auto condition_result = expression();
        if (!condition_result) return std::unexpected(condition_result.error());

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after if condition");
        if (!right_paren_result) return std::unexpected(right_paren_result.error());

        auto then_branch_result = statement();
        if (!then_branch_result) return std::unexpected(then_branch_result.error());

        std::unique_ptr<ast::Stmt> else_branch;
        if (match({lex::TokenType::Else}))
        {
            auto else_branch_result = statement();
            if (!else_branch_result) return std::unexpected(else_branch_result.error());
            else_branch = std::move(else_branch_result.value());
        }

        return std::make_unique<ast::If_stmt>(std::move(condition_result.value()), std::move(then_branch_result.value()), std::move(else_branch), previous().line, previous().column);
    }

    Result<std::unique_ptr<ast::Stmt>> while_statement()
    {
        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after 'while'");
        if (!left_paren_result) return std::unexpected(left_paren_result.error());

        auto condition_result = expression();
        if (!condition_result) return std::unexpected(condition_result.error());

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after condition");
        if (!right_paren_result) return std::unexpected(right_paren_result.error());

        auto body_result = statement();
        if (!body_result) return std::unexpected(body_result.error());

        return std::make_unique<ast::While_stmt>(std::move(condition_result.value()), std::move(body_result.value()), previous().line, previous().column);
    }

    Result<std::unique_ptr<ast::Stmt>> for_statement()
    {
        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after 'for'");
        if (!left_paren_result) return std::unexpected(left_paren_result.error());

        std::unique_ptr<ast::Stmt> initializer;
        if (match({lex::TokenType::Semicolon}))
        {
            // no initializer
        }
        else if (match({lex::TokenType::Let}))
        {
            auto init_result = var_declaration();
            if (!init_result) return std::unexpected(init_result.error());
            initializer = std::move(init_result.value());
        }
        else
        {
            auto init_result = expression_statement();
            if (!init_result) return std::unexpected(init_result.error());
            initializer = std::move(init_result.value());
        }

        std::unique_ptr<ast::Expr> condition;
        if (!check(lex::TokenType::Semicolon))
        {
            auto condition_result = expression();
            if (!condition_result) return std::unexpected(condition_result.error());
            condition = std::move(condition_result.value());
        }

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after loop condition");
        if (!semicolon_result) return std::unexpected(semicolon_result.error());

        std::unique_ptr<ast::Expr> increment;
        if (!check(lex::TokenType::RightParen))
        {
            auto increment_result = expression();
            if (!increment_result) return std::unexpected(increment_result.error());
            increment = std::move(increment_result.value());
        }

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after for clauses");
        if (!right_paren_result) return std::unexpected(right_paren_result.error());

        auto body_result = statement();
        if (!body_result) return std::unexpected(body_result.error());
        auto body = std::move(body_result.value());

        if (increment)
        {
            std::vector<std::unique_ptr<ast::Stmt>> body_stmts;
            body_stmts.push_back(std::move(body));
            body_stmts.push_back(std::make_unique<ast::Expr_stmt>(std::move(increment), 0, 0));
            body = std::make_unique<ast::Block_stmt>(std::move(body_stmts), 0, 0);
        }

        if (!condition)
            condition = std::make_unique<ast::Literal_expr>(true, types::Type(types::kind::Bool), 0, 0);

        auto while_loop = std::make_unique<ast::While_stmt>(std::move(condition), std::move(body), previous().line, previous().column);
        if (initializer)
        {
            std::vector<std::unique_ptr<ast::Stmt>> stmts;
            stmts.push_back(std::move(initializer));
            stmts.push_back(std::move(while_loop));
            return std::make_unique<ast::Block_stmt>(std::move(stmts), previous().line, previous().column);
        }
        return while_loop;
    }

    Result<std::unique_ptr<ast::Stmt>> return_statement()
    {
        size_t line = previous().line;
        size_t column = previous().column;
        std::unique_ptr<ast::Expr> value;
        
        if (!check(lex::TokenType::Semicolon))
        {
            auto value_result = expression();
            if (!value_result) return std::unexpected(value_result.error());
            value = std::move(value_result.value());
        }

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after return value");
        if (!semicolon_result) return std::unexpected(semicolon_result.error());

        return std::make_unique<ast::Return_stmt>(std::move(value), line, column);
    }

    Result<std::unique_ptr<ast::Stmt>> expression_statement()
    {
        auto expr_result = expression();
        if (!expr_result) return std::unexpected(expr_result.error());

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after expression");
        if (!semicolon_result) return std::unexpected(semicolon_result.error());

        return std::make_unique<ast::Expr_stmt>(std::move(expr_result.value()), previous().line, previous().column);
    }

    // Expression parsing with operator precedence
    Result<std::unique_ptr<ast::Expr>> expression() { return assignment(); }

    Result<std::unique_ptr<ast::Expr>> assignment()
    {
        auto expr_result = logical_or();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        if (match({lex::TokenType::Assign}))
        {
            lex::Token equals = previous();
            auto value_result = assignment();
            if (!value_result) return std::unexpected(value_result.error());

            if (auto *var_expr = dynamic_cast<ast::Variable_expr *>(expr.get()))
            {
                return std::make_unique<ast::Assignment_expr>(var_expr->name, std::move(value_result.value()), var_expr->type, equals.line, equals.column);
            }
            return std::unexpected(create_error(equals, "Invalid assignment target"));
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> logical_or()
    {
        auto expr_result = logical_and();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::LogicalOr}))
        {
            lex::Token op = previous();
            auto right_result = logical_and();
            if (!right_result) return std::unexpected(right_result.error());

            expr = std::make_unique<ast::Binary_expr>(std::move(expr), op.type, std::move(right_result.value()), types::Type(types::kind::Bool), op.line, op.column);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> logical_and()
    {
        auto expr_result = equality();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::LogicalAnd}))
        {
            lex::Token op = previous();
            auto right_result = equality();
            if (!right_result) return std::unexpected(right_result.error());

            expr = std::make_unique<ast::Binary_expr>(std::move(expr), op.type, std::move(right_result.value()), types::Type(types::kind::Bool), op.line, op.column);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> equality()
    {
        auto expr_result = comparison();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::Equal, lex::TokenType::NotEqual}))
        {
            lex::Token op = previous();
            auto right_result = comparison();
            if (!right_result) return std::unexpected(right_result.error());

            expr = std::make_unique<ast::Binary_expr>(std::move(expr), op.type, std::move(right_result.value()), types::Type(types::kind::Bool), op.line, op.column);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> comparison()
    {
        auto expr_result = term();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::Less, lex::TokenType::LessEqual, lex::TokenType::Greater, lex::TokenType::GreaterEqual}))
        {
            lex::Token op = previous();
            auto right_result = term();
            if (!right_result) return std::unexpected(right_result.error());

            expr = std::make_unique<ast::Binary_expr>(std::move(expr), op.type, std::move(right_result.value()), types::Type(types::kind::Bool), op.line, op.column);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> term()
    {
        auto expr_result = factor();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::Plus, lex::TokenType::Minus}))
        {
            lex::Token op = previous();
            auto right_result = factor();
            if (!right_result) return std::unexpected(right_result.error());

            types::Type result_type = expr->get_type();
            if (right_result.value()->get_type().kind_ == types::kind::Float && result_type.kind_ == types::kind::Int)
                result_type = types::Type(types::kind::Float);
            expr = std::make_unique<ast::Binary_expr>(std::move(expr), op.type, std::move(right_result.value()), result_type, op.line, op.column);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> factor()
    {
        auto expr_result = cast();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::Star, lex::TokenType::Slash, lex::TokenType::Percent}))
        {
            lex::Token op = previous();
            auto right_result = cast();
            if (!right_result) return std::unexpected(right_result.error());

            types::Type result_type = expr->get_type();
            if (right_result.value()->get_type().kind_ == types::kind::Float && result_type.kind_ == types::kind::Int)
                result_type = types::Type(types::kind::Float);
            expr = std::make_unique<ast::Binary_expr>(std::move(expr), op.type, std::move(right_result.value()), result_type, op.line, op.column);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> cast()
    {
        auto expr_result = unary();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::As}))
        {
            auto target_type_result = parse_type();
            if (!target_type_result) return std::unexpected(target_type_result.error());

            expr = std::make_unique<ast::Cast_expr>(std::move(expr), target_type_result.value());
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> unary()
    {
        if (match({lex::TokenType::LogicalNot, lex::TokenType::Minus}))
        {
            lex::Token op = previous();
            auto right_result = cast();
            if (!right_result) return std::unexpected(right_result.error());

            types::Type result_type = right_result.value()->get_type();
            if (op.type == lex::TokenType::LogicalNot)
                result_type = types::Type(types::kind::Bool);
            return std::make_unique<ast::Unary_expr>(op.type, std::move(right_result.value()), result_type, op.line, op.column);
        }
        return call();
    }

    Result<std::unique_ptr<ast::Expr>> call()
    {
        auto expr_result = primary();
        if (!expr_result) return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::LeftParen}))
        {
            std::vector<std::unique_ptr<ast::Expr>> arguments;
            if (!check(lex::TokenType::RightParen))
            {
                do {
                    auto arg_result = expression();
                    if (!arg_result) return std::unexpected(arg_result.error());
                    arguments.push_back(std::move(arg_result.value()));
                } while (match({lex::TokenType::Comma}));
            }

            auto paren_result = consume(lex::TokenType::RightParen, "Expect ')' after arguments");
            if (!paren_result) return std::unexpected(paren_result.error());
            lex::Token paren = paren_result.value();

            // Extract function name from the expression
            std::string function_name = "unknown";
            if (auto *var_expr = dynamic_cast<ast::Variable_expr *>(expr.get()))
                function_name = var_expr->name;
            types::Type return_type = types::Type(types::kind::Void);
            auto it = function_types.find(function_name);
            if (it != function_types.end())
                return_type = it->second;

            expr = std::make_unique<ast::Call_expr>(function_name, std::move(arguments), return_type, paren.line, paren.column);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> primary()
    {
        if (peek().type == lex::TokenType::Print)
            return std::unexpected(create_error(peek(), "print is a statement, not an expression. Did you forget a semicolon?"));

        if (match({lex::TokenType::Bool}))
        {
            bool value = std::get<bool>(previous().literal);
            return std::make_unique<ast::Literal_expr>(value, types::Type(types::kind::Bool), previous().line, previous().column);
        }

        if (match({lex::TokenType::Integer}))
        {
            return std::make_unique<ast::Literal_expr>(std::get<int64_t>(previous().literal), types::Type(types::kind::Int), previous().line, previous().column);
        }

        if (match({lex::TokenType::Float}))
        {
            return std::make_unique<ast::Literal_expr>(std::get<double>(previous().literal), types::Type(types::kind::Float), previous().line, previous().column);
        }

        if (match({lex::TokenType::String}))
        {
            return std::make_unique<ast::Literal_expr>(std::get<std::string>(previous().literal), types::Type(types::kind::String), previous().line, previous().column);
        }

        if (match({lex::TokenType::Identifier}))
        {
            std::string name = previous().lexeme;
            types::Type type = types::Type(types::kind::Void);

            // Look up variable type if it's already declared
            auto it = variable_types.find(name);
            if (it != variable_types.end())
                type = it->second;

            return std::make_unique<ast::Variable_expr>(name, type, previous().line, previous().column);
        }

        if (match({lex::TokenType::LeftParen}))
        {
            auto expr_result = expression();
            if (!expr_result)
                return std::unexpected(expr_result.error());

            auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after expression");
            if (!right_paren_result)
                return std::unexpected(right_paren_result.error());

            return std::move(expr_result.value());
        }

        return std::unexpected(create_error(peek(), "Expect expression. Found: " + peek().lexeme));
    }

    Result<types::Type> parse_type()
    {
        if (match({lex::TokenType::Identifier}))
        {
            std::string type_name = previous().lexeme;
            if (type_name == "int")
                return types::Type(types::kind::Int);
            if (type_name == "float")
                return types::Type(types::kind::Float);
            if (type_name == "bool")
                return types::Type(types::kind::Bool);
            if (type_name == "string")
                return types::Type(types::kind::String);
            if (type_name == "void")
                return types::Type(types::kind::Void);
            return std::unexpected(create_error(previous(), "Unknown type: " + type_name));
        }
        return std::unexpected(create_error(peek(), "Expect type"));
    }
};

} // namepsace phos
