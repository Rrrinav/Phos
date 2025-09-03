#pragma once

#include <string>
#include <utility>
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

        return std::make_unique<ast::Stmt>(
            (ast::Stmt){
                (ast::Function_stmt){ .name = name.lexeme, .parameters = parameters, .return_type = return_type, .body =  std::move(body_result.value())},
                {name.line, name.column}
        });
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
                var_type = initializer->type;
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

        return std::make_unique<ast::Stmt>( ast::Stmt{
            ast::Var_stmt{name.lexeme, var_type, std::move(initializer)},
            ast::Source_location{name.line, name.column}}
        );
    }

    Result<std::unique_ptr<ast::Stmt>> statement()
    {
        skip_newlines();
        if (is_at_end())
            return std::unexpected(create_error(peek(), "Unexpected end of file"));

        if (match({lex::TokenType::Print}))
            return print_statement();
        if (match({lex::TokenType::PrintErr}))
            return print_statement(ast::Print_stream::STDERR);
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

    Result<std::unique_ptr<ast::Stmt>> print_statement(ast::Print_stream stream = ast::Print_stream::STDOUT)
    {
        auto left_paren = consume(lex::TokenType::LeftParen, "Expected '(' after print statement");
        if (!left_paren) return std::unexpected(left_paren.error());

        auto expr_result = expression();
        if (!expr_result) return std::unexpected(expr_result.error());

        auto right_paren = consume(lex::TokenType::RightParen, "Expected ')' after value");
        if (!right_paren) return std::unexpected(right_paren.error());

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after print statement");
        if (!semicolon_result) return std::unexpected(semicolon_result.error());

        return std::make_unique<ast::Stmt>(ast::Stmt{
            ast::Print_stmt{stream, std::move(expr_result.value())},
            ast::Source_location{previous().line, previous().column}
        });
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

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::Block_stmt{std::move(statements)}, {line, column}});
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

        return std::make_unique<ast::Stmt>(ast::Stmt{
            ast::If_stmt{std::move(condition_result.value()), std::move(then_branch_result.value()), std::move(else_branch)},
            ast::Source_location{previous().line, previous().column}
        });
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

        return std::make_unique<ast::Stmt>(ast::Stmt{
            ast::While_stmt{std::move(condition_result.value()), std::move(body_result.value())}, 
            ast::Source_location{previous().line, previous().column}
        });
    }

    Result<std::unique_ptr<ast::Stmt>> for_statement()
    {
        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after 'for'");
        if (!left_paren_result)
            return std::unexpected(left_paren_result.error());

        std::unique_ptr<ast::Stmt> initializer;
        if (match({lex::TokenType::Semicolon}))
        {
            // no initializer
        }
        else if (match({lex::TokenType::Let}))
        {
            auto init_result = var_declaration();
            if (!init_result)
                return std::unexpected(init_result.error());
            initializer = std::move(init_result.value());
        }
        else
        {
            auto init_result = expression_statement();
            if (!init_result)
                return std::unexpected(init_result.error());
            initializer = std::move(init_result.value());
        }

        std::unique_ptr<ast::Expr> condition;
        if (!check(lex::TokenType::Semicolon))
        {
            auto condition_result = expression();
            if (!condition_result)
                return std::unexpected(condition_result.error());
            condition = std::move(condition_result.value());
        }

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after loop condition");
        if (!semicolon_result)
            return std::unexpected(semicolon_result.error());

        std::unique_ptr<ast::Expr> increment;
        if (!check(lex::TokenType::RightParen))
        {
            auto increment_result = expression();
            if (!increment_result)
                return std::unexpected(increment_result.error());
            increment = std::move(increment_result.value());
        }

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after for clauses");
        if (!right_paren_result)
            return std::unexpected(right_paren_result.error());

        auto body_result = statement();
        if (!body_result)
            return std::unexpected(body_result.error());
        auto body = std::move(body_result.value());

        // Convert for loop to while loop
        if (increment)
        {
            // Create block: { body; increment; }
            std::vector<std::unique_ptr<ast::Stmt>> body_stmts;
            body_stmts.push_back(std::move(body));

            ast::Expr_stmt increment_stmt{std::move(increment)};
            body_stmts.push_back(std::make_unique<ast::Stmt>(ast::Stmt{std::move(increment_stmt), {previous().line, previous().column}}));

            ast::Block_stmt block_node{std::move(body_stmts)};
            body = std::make_unique<ast::Stmt>(ast::Stmt{std::move(block_node), {previous().line, previous().column}});
        }

        // Default condition to true if not provided
        if (!condition)
        {
            ast::Literal_expr true_literal{Value{true}};
            condition = std::make_unique<ast::Expr>(ast::Expr{std::move(true_literal), types::Type(types::kind::Bool), {previous().line, previous().column}});
        }

        // Create the while loop
        ast::While_stmt while_node{std::move(condition), std::move(body)};
        auto while_loop = std::make_unique<ast::Stmt>(ast::Stmt{std::move(while_node), {previous().line, previous().column}});

        // If there's an initializer, create a block: { initializer; while_loop; }
        if (initializer)
        {
            std::vector<std::unique_ptr<ast::Stmt>> stmts;
            stmts.push_back(std::move(initializer));
            stmts.push_back(std::move(while_loop));

            ast::Block_stmt block_node{std::move(stmts)};
            return std::make_unique<ast::Stmt>(ast::Stmt{std::move(block_node), {previous().line, previous().column}});
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

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::Return_stmt{std::move(value)}, {line, column}});
    }

    Result<std::unique_ptr<ast::Stmt>> expression_statement()
    {
        auto expr_result = expression();
        if (!expr_result) return std::unexpected(expr_result.error());

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after expression");
        if (!semicolon_result) return std::unexpected(semicolon_result.error());

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::Expr_stmt{std::move(expr_result.value())}, {previous().line, previous().column}});
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

            if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr->node))
            {
                return std::make_unique<ast::Expr>(ast::Expr{
                    ast::Assignment_expr{var_expr->name, std::move(value_result.value())},
                    expr->type,
                    {equals.line, equals.column}
                });
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

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{
                .left = std::move(expr),
                .op = op.type,
                .right = std::move(right_result.value())
            };
            binary_expr->type = types::Type(types::kind::Bool);
            binary_expr->loc = {op.line, op.column};
            expr = std::move(binary_expr);
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

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{.left = std::move(expr), .op = op.type, .right = std::move(right_result.value())};
            binary_expr->type = types::Type(types::kind::Bool);
            binary_expr->loc = {op.line, op.column};
            expr = std::move(binary_expr);
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

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{
                .left = std::move(expr),
                .op = op.type,
                .right = std::move(right_result.value())
            };
            binary_expr->type = types::Type(types::kind::Bool);
            binary_expr->loc = {op.line, op.column};
            expr = std::move(binary_expr);
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

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{
                .left = std::move(expr),
                .op = op.type,
                .right = std::move(right_result.value())
            };
            binary_expr->type = types::Type(types::kind::Bool);
            binary_expr->loc = {op.line, op.column};
            expr = std::move(binary_expr);
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

            types::Type result_type = expr->type;
            if (right_result.value()->type.kind_ == types::kind::Float && result_type.kind_ == types::kind::Int)
                result_type = types::Type(types::kind::Float);

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{
                .left = std::move(expr),
                .op = op.type,
                .right = std::move(right_result.value())
            };
            binary_expr->type = result_type;
            binary_expr->loc = {op.line, op.column};
            expr = std::move(binary_expr);
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

            types::Type result_type = expr->type;
            if (right_result.value()->type.kind_ == types::kind::Float && result_type.kind_ == types::kind::Int)
                result_type = types::Type(types::kind::Float);

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{
                .left = std::move(expr),
                .op = op.type,
                .right = std::move(right_result.value())
            };
            binary_expr->type = result_type;
            binary_expr->loc = {op.line, op.column};
            expr = std::move(binary_expr);
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

            auto cast_expr = std::make_unique<ast::Expr>();
            cast_expr->node = ast::Cast_expr{.expression = std::move(expr)};
            cast_expr->type = target_type_result.value();
            cast_expr->loc = expr->loc;
            expr = std::move(cast_expr);
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

            types::Type result_type = right_result.value()->type;
            if (op.type == lex::TokenType::LogicalNot)
                result_type = types::Type(types::kind::Bool);

            auto unary_expr = std::make_unique<ast::Expr>();
            unary_expr->node = ast::Unary_expr{
                .op = op.type,
                .right = std::move(right_result.value())
            };
            unary_expr->type = result_type;
            unary_expr->loc = {op.line, op.column};
            return unary_expr;
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
            if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr->node))
                function_name = var_expr->name;

            types::Type return_type = types::Type(types::kind::Void);
            auto it = function_types.find(function_name);
            if (it != function_types.end())
                return_type = it->second;

            auto call_expr = std::make_unique<ast::Expr>();
            call_expr->node = ast::Call_expr{
                .callee = function_name,
                .arguments = std::move(arguments)
            };
            call_expr->type = return_type;
            call_expr->loc = {paren.line, paren.column};
            expr = std::move(call_expr);
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
            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Literal_expr{.value = Value(value)};
            expr->type = types::Type(types::kind::Bool);
            expr->loc = {previous().line, previous().column};
            return expr;
        }

        if (match({lex::TokenType::Integer64}))
        {
            int64_t value = std::get<int64_t>(previous().literal);
            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Literal_expr{.value = Value(value)};
            expr->type = types::Type(types::kind::Int);
            expr->loc = {previous().line, previous().column};
            return expr;
        }

        if (match({lex::TokenType::Float64}))
        {
            double value = std::get<double>(previous().literal);
            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Literal_expr{.value = Value(value)};
            expr->type = types::Type(types::kind::Float);
            expr->loc = {previous().line, previous().column};
            return expr;
        }

        if (match({lex::TokenType::String}))
        {
            std::string value = std::get<std::string>(previous().literal);
            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Literal_expr{.value = Value(value)};
            expr->type = types::Type(types::kind::String);
            expr->loc = {previous().line, previous().column};
            return expr;
        }

        if (match({lex::TokenType::Identifier}))
        {
            std::string name = previous().lexeme;
            types::Type type = types::Type(types::kind::Void);

            auto it = variable_types.find(name);
            if (it != variable_types.end())
                type = it->second;

            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Variable_expr{.name = name};
            expr->type = type;
            expr->loc = {previous().line, previous().column};
            return expr;
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
            if (type_name == "i64")
                return types::Type(types::kind::Int);
            if (type_name == "64")
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
