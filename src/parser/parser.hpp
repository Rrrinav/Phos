#pragma once

#include <string>
#include <utility>
#include <vector>
#include <variant>
#include <unordered_map>
#include <memory>
#include <expected>

#include "../lexer/token.hpp"
#include "../value/type.hpp"
#include "../error/err.hpp"
#include "../error/result.hpp"
#include "ast.hpp"

namespace phos
{

class Parser
{
    std::vector<lex::Token> tokens;
    size_t current = 0;
    std::unordered_map<std::string, types::Type> variable_types;
    std::unordered_map<std::string, types::Type> function_types;
    std::unordered_map<std::string, types::Type> model_types;
    std::string current_model;
    std::string stage = "parsing";

public:
    explicit Parser(std::vector<lex::Token> t) : tokens(std::move(t)) {}

    void skip_newlines()
    {
        while (!is_at_end() && match({lex::TokenType::Newline})) {}
    }

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
                    advance();
                    continue;
                }
                return std::unexpected(decl_result.error());
            }

            if (decl_result.value())
                statements.push_back(std::move(*decl_result.value()));
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
        return err::msg(message, this->stage, token.line, token.column);
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

        if (match({lex::TokenType::Model}))
        {
            auto model_result = model_declaration();
            if (!model_result)
            {
                synchronize();
                return std::unexpected(model_result.error());
            }
            return std::optional<std::unique_ptr<ast::Stmt>>{std::move(model_result.value())};
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
        if (!name_result)
            return std::unexpected(name_result.error());
        lex::Token name = name_result.value();

        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after function name");
        if (!left_paren_result)
            return std::unexpected(left_paren_result.error());

        std::vector<std::pair<std::string, types::Type>> parameters;
        if (!check(lex::TokenType::RightParen))
        {
            do {
                auto param_name_result = consume(lex::TokenType::Identifier, "Expect parameter name");
                if (!param_name_result)
                    return std::unexpected(param_name_result.error());

                auto colon_result = consume(lex::TokenType::Colon, "Expect ':' after parameter name");
                if (!colon_result)
                    return std::unexpected(colon_result.error());

                auto param_type_result = parse_type();
                if (!param_type_result)
                    return std::unexpected(param_type_result.error());

                parameters.emplace_back(param_name_result.value().lexeme, param_type_result.value());
            } while (match({lex::TokenType::Comma}));
        }

        auto saved_variable_types = this->variable_types;

        for (const auto &param : parameters) variable_types[param.first] = param.second;

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after parameters");
        if (!right_paren_result)
            return std::unexpected(right_paren_result.error());

        types::Type return_type = types::Type(types::Primitive_kind::Void);
        if (match({lex::TokenType::Arrow}))
        {
            auto return_type_result = parse_type();
            if (!return_type_result)
                return std::unexpected(return_type_result.error());
            return_type = return_type_result.value();
        }

        function_types.emplace(name.lexeme, return_type);
        auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' before function body");
        if (!left_brace_result)
            return std::unexpected(left_brace_result.error());

        auto body_result = block_statement();
        if (!body_result)
            return std::unexpected(body_result.error());

        this->variable_types = saved_variable_types;

        auto stmt = std::make_unique<ast::Stmt>();
        stmt->node = ast::Function_stmt{.name = name.lexeme,
                                        .parameters = std::move(parameters),
                                        .return_type = return_type,
                                        .body = std::shared_ptr<ast::Stmt>(std::move(body_result.value())),
                                        .loc = {name.line, name.column}};

        return stmt;
    }

    Result<std::unique_ptr<ast::Stmt>> model_declaration()
    {
        auto name_res = consume(lex::TokenType::Identifier, "Expected model name");
        if (!name_res)
            return std::unexpected(name_res.error());
        lex::Token name = name_res.value();

        current_model = name.lexeme;

        auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' after model name");
        if (!left_brace_result)
            return std::unexpected(left_brace_result.error());

        std::vector<std::pair<std::string, types::Type>> fields;
        std::vector<ast::Function_stmt> methods;

        bool parsing_fields = true;
        types::Model_type model_type;

        while (!check(lex::TokenType::RightBrace) && !is_at_end())
        {
            skip_newlines();
            if (check(lex::TokenType::RightBrace))
                break;

            if (peek().type == lex::TokenType::Fn)
            {
                if (parsing_fields)
                {
                    model_type.name = name.lexeme;
                    for (const auto &field : fields) model_type.fields[field.first] = field.second;
                    model_types[name.lexeme] = types::Type(std::make_shared<types::Model_type>(model_type));
                }
                parsing_fields = false;
                advance();
                auto method_result = parse_model_method();
                if (!method_result)
                    return std::unexpected(method_result.error());
                methods.push_back(std::move(method_result.value()));
            }
            else if (peek().type == lex::TokenType::Let)
            {
                if (!parsing_fields)
                    return std::unexpected(create_error(peek(), "Write all the fields before functions in the model"));
                advance();
                auto field_result = parse_model_field();
                if (!field_result)
                    return std::unexpected(field_result.error());
                fields.push_back(std::move(field_result.value()));
            }
            else
            {
                return std::unexpected(create_error(peek(), "Expect field or method declaration in model"));
            }
        }

        auto right_brace_result = consume(lex::TokenType::RightBrace, "Expect '}' after model body");
        if (!right_brace_result)
            return std::unexpected(right_brace_result.error());

        for (const auto &method : methods)
        {
            types::Function_type func_type;
            for (const auto &param : method.parameters) func_type.parameter_types.push_back(param.second);
            func_type.return_type = method.return_type;
            model_type.methods[method.name] = func_type;
        }

        current_model.clear();

        auto stmt = std::make_unique<ast::Stmt>();
        stmt->node = ast::Model_stmt{
            .name = name.lexeme, .fields = std::move(fields), .methods = std::move(methods), .loc = {name.line, name.column}};

        return stmt;
    }

    Result<std::pair<std::string, types::Type>> parse_model_field()
    {
        auto name_result = consume(lex::TokenType::Identifier, "Expect field name");
        if (!name_result)
            return std::unexpected(name_result.error());

        auto colon_result = consume(lex::TokenType::Colon, "Expect ':' after field name");
        if (!colon_result)
            return std::unexpected(colon_result.error());

        auto type_result = parse_type();
        if (!type_result)
            return std::unexpected(type_result.error());

        std::unique_ptr<ast::Expr> initializer;
        if (match({lex::TokenType::Assign}))
        {
            auto init_result = expression();
            if (!init_result)
                return std::unexpected(init_result.error());
            initializer = std::move(init_result.value());
        }

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after field declaration");
        if (!semicolon_result)
            return std::unexpected(semicolon_result.error());

        return std::make_pair(name_result.value().lexeme, type_result.value());
    }

    Result<ast::Function_stmt> parse_model_method()
    {
        auto name_result = consume(lex::TokenType::Identifier, "Expect method name");
        if (!name_result)
            return std::unexpected(name_result.error());
        lex::Token name = name_result.value();

        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after method name");
        if (!left_paren_result)
            return std::unexpected(left_paren_result.error());

        std::vector<std::pair<std::string, types::Type>> parameters;
        if (!check(lex::TokenType::RightParen))
        {
            do {
                auto param_name_result = consume(lex::TokenType::Identifier, "Expect parameter name");
                if (!param_name_result)
                    return std::unexpected(param_name_result.error());

                auto colon_result = consume(lex::TokenType::Colon, "Expect ':' after parameter name");
                if (!colon_result)
                    return std::unexpected(colon_result.error());

                auto param_type_result = parse_type();
                if (!param_type_result)
                    return std::unexpected(param_type_result.error());

                parameters.emplace_back(param_name_result.value().lexeme, param_type_result.value());
            } while (match({lex::TokenType::Comma}));
        }

        auto saved_variable_types = this->variable_types;
        variable_types["this"] = model_types[current_model];
        for (const auto &param : parameters) variable_types[param.first] = param.second;

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after parameters");
        if (!right_paren_result)
            return std::unexpected(right_paren_result.error());

        types::Type return_type = types::Type(types::Primitive_kind::Void);
        if (match({lex::TokenType::Arrow}))
        {
            auto return_type_result = parse_type();
            if (!return_type_result)
                return std::unexpected(return_type_result.error());
            return_type = return_type_result.value();
        }

        function_types.emplace(name.lexeme, return_type);
        auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' before method body");
        if (!left_brace_result)
            return std::unexpected(left_brace_result.error());

        auto body_result = block_statement();
        if (!body_result)
            return std::unexpected(body_result.error());

        this->variable_types = saved_variable_types;

        return ast::Function_stmt{.name = name.lexeme,
                                  .parameters = parameters,
                                  .return_type = return_type,
                                  .body = std::shared_ptr<ast::Stmt>(std::move(body_result.value())),
                                  .loc = {name.line, name.column}};
    }

    Result<std::unique_ptr<ast::Stmt>> var_declaration()
    {
        auto name_result = consume(lex::TokenType::Identifier, "Expect variable name");
        if (!name_result)
            return std::unexpected(name_result.error());
        lex::Token name = name_result.value();

        types::Type var_type = types::Type(types::Primitive_kind::Void);
        std::unique_ptr<ast::Expr> initializer;
        bool type_inferred = false;

        if (match({lex::TokenType::Colon}))
        {
            if (match({lex::TokenType::Assign}))
            {
                auto init_result = expression();
                if (!init_result)
                    return std::unexpected(init_result.error());
                initializer = std::move(init_result.value());
                var_type = ast::get_type(initializer->node);
                type_inferred = true;
            }
            else
            {
                auto type_result = parse_type();
                if (!type_result)
                    return std::unexpected(type_result.error());
                var_type = type_result.value();

                if (match({lex::TokenType::Assign}))
                {
                    Result<std::unique_ptr<ast::Expr>> init_result;
                    if (types::is_model(var_type) && check(lex::TokenType::LeftBrace))
                    {
                        // If we know the type is a model and we see a '{', parse a model literal.
                        init_result = parse_model_literal(types::get_model_type(var_type)->name);
                    }
                    else
                    {
                        // Otherwise, parse a normal expression.
                        init_result = expression();
                    }
                    if (!init_result)
                        return std::unexpected(init_result.error());
                    initializer = std::move(init_result.value());
                }
            }
        }

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expected ';' after variable declaration");
        if (!semicolon_result)
            return std::unexpected(semicolon_result.error());

        variable_types[name.lexeme] = var_type;

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::Var_stmt{.name = name.lexeme,
                                                                   .type = var_type,
                                                                   .initializer = std::move(initializer),
                                                                   .type_inferred = type_inferred,
                                                                   .loc = {name.line, name.column}}});
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
        if (!left_paren)
            return std::unexpected(left_paren.error());

        auto expr_result = expression();
        if (!expr_result)
            return std::unexpected(expr_result.error());

        auto right_paren = consume(lex::TokenType::RightParen, "Expected ')' after value");
        if (!right_paren)
            return std::unexpected(right_paren.error());

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after print statement");
        if (!semicolon_result)
            return std::unexpected(semicolon_result.error());

        return std::make_unique<ast::Stmt>(ast::Stmt{
            ast::Print_stmt{.stream = stream, .expression = std::move(expr_result.value()), .loc = {previous().line, previous().column}}});
    }

    Result<std::unique_ptr<ast::Stmt>> block_statement()
    {
        std::vector<std::unique_ptr<ast::Stmt>> statements;
        size_t line = previous().line;
        size_t column = previous().column;

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

        variable_types = saved_variable_types;

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::Block_stmt{std::move(statements), {line, column}}});
    }

    Result<std::unique_ptr<ast::Stmt>> if_statement()
    {
        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after 'if'");
        if (!left_paren_result)
            return std::unexpected(left_paren_result.error());

        auto condition_result = expression();
        if (!condition_result)
            return std::unexpected(condition_result.error());

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after if condition");
        if (!right_paren_result)
            return std::unexpected(right_paren_result.error());

        auto then_branch_result = statement();
        if (!then_branch_result)
            return std::unexpected(then_branch_result.error());

        std::unique_ptr<ast::Stmt> else_branch;
        if (match({lex::TokenType::Else}))
        {
            auto else_branch_result = statement();
            if (!else_branch_result)
                return std::unexpected(else_branch_result.error());
            else_branch = std::move(else_branch_result.value());
        }

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::If_stmt{.condition = std::move(condition_result.value()),
                                                                  .then_branch = std::move(then_branch_result.value()),
                                                                  .else_branch = std::move(else_branch),
                                                                  .loc = {previous().line, previous().column}}});
    }

    Result<std::unique_ptr<ast::Stmt>> while_statement()
    {
        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after 'while'");
        if (!left_paren_result)
            return std::unexpected(left_paren_result.error());

        auto condition_result = expression();
        if (!condition_result)
            return std::unexpected(condition_result.error());

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after condition");
        if (!right_paren_result)
            return std::unexpected(right_paren_result.error());

        auto body_result = statement();
        if (!body_result)
            return std::unexpected(body_result.error());

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::While_stmt{.condition = std::move(condition_result.value()),
                                                                     .body = std::move(body_result.value()),
                                                                     .loc = {previous().line, previous().column}}});
    }

    Result<std::unique_ptr<ast::Stmt>> for_statement()
    {
        auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after 'for'");
        if (!left_paren_result)
            return std::unexpected(left_paren_result.error());

        std::unique_ptr<ast::Stmt> initializer;
        if (match({lex::TokenType::Semicolon})) {}
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

        if (increment)
        {
            std::vector<std::unique_ptr<ast::Stmt>> body_stmts;
            body_stmts.push_back(std::move(body));

            ast::Expr_stmt increment_stmt{std::move(increment), {previous().line, previous().column}};
            body_stmts.push_back(std::make_unique<ast::Stmt>(ast::Stmt{std::move(increment_stmt)}));

            ast::Block_stmt block_node{std::move(body_stmts), {previous().line, previous().column}};
            body = std::make_unique<ast::Stmt>(ast::Stmt{std::move(block_node)});
        }

        if (!condition)
        {
            ast::Literal_expr true_literal{Value{true}, types::Type(types::Primitive_kind::Bool), {previous().line, previous().column}};
            condition = std::make_unique<ast::Expr>(ast::Expr{std::move(true_literal)});
        }

        ast::While_stmt while_node{std::move(condition), std::move(body), {previous().line, previous().column}};
        auto while_loop = std::make_unique<ast::Stmt>(ast::Stmt{std::move(while_node)});

        if (initializer)
        {
            std::vector<std::unique_ptr<ast::Stmt>> stmts;
            stmts.push_back(std::move(initializer));
            stmts.push_back(std::move(while_loop));

            ast::Block_stmt block_node{std::move(stmts), {previous().line, previous().column}};
            return std::make_unique<ast::Stmt>(ast::Stmt{std::move(block_node)});
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
            if (!value_result)
                return std::unexpected(value_result.error());
            value = std::move(value_result.value());
        }

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after return value");
        if (!semicolon_result)
            return std::unexpected(semicolon_result.error());

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::Return_stmt{std::move(value), {line, column}}});
    }

    Result<std::unique_ptr<ast::Stmt>> expression_statement()
    {
        auto expr_result = expression();
        if (!expr_result)
            return std::unexpected(expr_result.error());

        auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after expression");
        if (!semicolon_result)
            return std::unexpected(semicolon_result.error());

        return std::make_unique<ast::Stmt>(ast::Stmt{ast::Expr_stmt{std::move(expr_result.value()), {previous().line, previous().column}}});
    }

    Result<std::unique_ptr<ast::Expr>> expression() { return assignment(); }

    Result<std::unique_ptr<ast::Expr>> assignment()
    {
        auto expr_result = logical_or();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        if (match({lex::TokenType::Assign}))
        {
            lex::Token equals = previous();
            auto value_result = assignment();
            if (!value_result)
                return std::unexpected(value_result.error());

            if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr->node))
            {
                return std::make_unique<ast::Expr>(ast::Expr{
                    ast::Assignment_expr{var_expr->name, std::move(value_result.value()), var_expr->type, {equals.line, equals.column}}});
            }
            else if (auto *field_access_expr = std::get_if<ast::Field_access_expr>(&expr->node))
            {
                return std::make_unique<ast::Expr>(ast::Expr{ast::Field_assignment_expr{.object = std::move(field_access_expr->object),
                                                                                        .field_name = field_access_expr->field_name,
                                                                                        .value = std::move(value_result.value()),
                                                                                        .loc = {equals.line, equals.column}}});
            }
            else if (auto *array_access_expr = std::get_if<ast::Array_access_expr>(&expr->node))
            {
                return std::make_unique<ast::Expr>(ast::Expr{ast::Array_assignment_expr{.array = std::move(array_access_expr->array),
                                                                                        .index = std::move(array_access_expr->index),
                                                                                        .value = std::move(value_result.value()),
                                                                                        .type = types::Type(types::Primitive_kind::Void),
                                                                                        .loc = {equals.line, equals.column}}});
            }

            return std::unexpected(create_error(equals, "Invalid assignment target"));
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> logical_or()
    {
        auto expr_result = logical_and();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::LogicalOr}))
        {
            lex::Token op = previous();
            auto right_result = logical_and();
            if (!right_result)
                return std::unexpected(right_result.error());

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{.left = std::move(expr),
                                                 .op = op.type,
                                                 .right = std::move(right_result.value()),
                                                 .type = types::Type(types::Primitive_kind::Bool),
                                                 .loc = {op.line, op.column}};
            expr = std::move(binary_expr);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> logical_and()
    {
        auto expr_result = equality();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::LogicalAnd}))
        {
            lex::Token op = previous();
            auto right_result = equality();
            if (!right_result)
                return std::unexpected(right_result.error());

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{.left = std::move(expr),
                                                 .op = op.type,
                                                 .right = std::move(right_result.value()),
                                                 .type = types::Type(types::Primitive_kind::Bool),
                                                 .loc = {op.line, op.column}};
            expr = std::move(binary_expr);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> equality()
    {
        auto expr_result = comparison();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::Equal, lex::TokenType::NotEqual}))
        {
            lex::Token op = previous();
            auto right_result = comparison();
            if (!right_result)
                return std::unexpected(right_result.error());

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{.left = std::move(expr),
                                                 .op = op.type,
                                                 .right = std::move(right_result.value()),
                                                 .type = types::Type(types::Primitive_kind::Bool),
                                                 .loc = {op.line, op.column}};
            expr = std::move(binary_expr);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> comparison()
    {
        auto expr_result = term();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::Less, lex::TokenType::LessEqual, lex::TokenType::Greater, lex::TokenType::GreaterEqual}))
        {
            lex::Token op = previous();
            auto right_result = term();
            if (!right_result)
                return std::unexpected(right_result.error());

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{.left = std::move(expr),
                                                 .op = op.type,
                                                 .right = std::move(right_result.value()),
                                                 .type = types::Type(types::Primitive_kind::Bool),
                                                 .loc = {op.line, op.column}};
            expr = std::move(binary_expr);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> term()
    {
        auto expr_result = factor();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::Plus, lex::TokenType::Minus}))
        {
            lex::Token op = previous();
            auto right_result = factor();
            if (!right_result)
                return std::unexpected(right_result.error());

            types::Type result_type = ast::get_type(expr->node);
            if (ast::get_type(right_result.value()->node) == types::Type(types::Primitive_kind::Float) &&
                result_type == types::Type(types::Primitive_kind::Int))
            {
                result_type = types::Type(types::Primitive_kind::Float);
            }

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{.left = std::move(expr),
                                                 .op = op.type,
                                                 .right = std::move(right_result.value()),
                                                 .type = result_type,
                                                 .loc = {op.line, op.column}};
            expr = std::move(binary_expr);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> factor()
    {
        auto expr_result = cast();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::Star, lex::TokenType::Slash, lex::TokenType::Percent}))
        {
            lex::Token op = previous();
            auto right_result = cast();
            if (!right_result)
                return std::unexpected(right_result.error());

            types::Type result_type = ast::get_type(expr->node);
            if (ast::get_type(right_result.value()->node) == types::Type(types::Primitive_kind::Float) &&
                result_type == types::Type(types::Primitive_kind::Int))
            {
                result_type = types::Type(types::Primitive_kind::Float);
            }

            auto binary_expr = std::make_unique<ast::Expr>();
            binary_expr->node = ast::Binary_expr{.left = std::move(expr),
                                                 .op = op.type,
                                                 .right = std::move(right_result.value()),
                                                 .type = result_type,
                                                 .loc = {op.line, op.column}};
            expr = std::move(binary_expr);
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> cast()
    {
        auto expr_result = unary();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (match({lex::TokenType::As}))
        {
            auto target_type_result = parse_type();
            if (!target_type_result)
                return std::unexpected(target_type_result.error());

            auto loc = ast::get_loc(expr->node);
            auto cast_expr = std::make_unique<ast::Expr>();
            cast_expr->node = ast::Cast_expr{.expression = std::move(expr), .target_type = target_type_result.value(), .loc = loc};
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
            if (!right_result)
                return std::unexpected(right_result.error());

            types::Type result_type = ast::get_type(right_result.value()->node);
            if (op.type == lex::TokenType::LogicalNot)
                result_type = types::Type(types::Primitive_kind::Bool);

            auto unary_expr = std::make_unique<ast::Expr>();
            unary_expr->node =
                ast::Unary_expr{.op = op.type, .right = std::move(right_result.value()), .type = result_type, .loc = {op.line, op.column}};
            return unary_expr;
        }
        return call();
    }

    Result<std::unique_ptr<ast::Expr>> call()
    {
        auto expr_result = primary();
        if (!expr_result)
            return std::unexpected(expr_result.error());
        auto expr = std::move(expr_result.value());

        while (true)
        {
            if (match({lex::TokenType::LeftParen}))
            {
                std::vector<std::unique_ptr<ast::Expr>> arguments;
                if (!check(lex::TokenType::RightParen))
                {
                    do {
                        auto arg_result = expression();
                        if (!arg_result)
                            return std::unexpected(arg_result.error());
                        arguments.push_back(std::move(arg_result.value()));
                    } while (match({lex::TokenType::Comma}));
                }

                auto paren_result = consume(lex::TokenType::RightParen, "Expect ')' after arguments");
                if (!paren_result)
                    return std::unexpected(paren_result.error());
                lex::Token paren = paren_result.value();

                if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr->node))
                {
                    auto func_it = function_types.find(var_expr->name);
                    types::Type return_type =
                        (func_it != function_types.end()) ? func_it->second : types::Type(types::Primitive_kind::Void);

                    auto call_expr = std::make_unique<ast::Expr>();
                    call_expr->node = ast::Call_expr{.callee = var_expr->name,
                                                     .arguments = std::move(arguments),
                                                     .type = return_type,
                                                     .loc = {paren.line, paren.column}};
                    expr = std::move(call_expr);
                }
                else
                {
                    return std::unexpected(create_error(peek(), "Invalid function call target."));
                }
            }
            else if (match({lex::TokenType::LeftBracket}))
            {
                auto index_result = expression();
                if (!index_result)
                    return std::unexpected(index_result.error());

                auto right_bracket_result = consume(lex::TokenType::RightBracket, "Expect ']' after array index");
                if (!right_bracket_result)
                    return std::unexpected(right_bracket_result.error());

                auto array_access = std::make_unique<ast::Expr>();
                array_access->node =
                    ast::Array_access_expr{.array = std::move(expr),
                                           .index = std::move(index_result.value()),
                                           .type = types::Type(types::Primitive_kind::Void),
                                           .loc = {right_bracket_result.value().line, right_bracket_result.value().column}};
                expr = std::move(array_access);
            }
            else if (match({lex::TokenType::Dot}))
            {
                auto name_result = consume(lex::TokenType::Identifier, "Expect field or method name after '.'");
                if (!name_result)
                    return std::unexpected(name_result.error());

                if (check(lex::TokenType::LeftParen))
                {
                    // Method call
                    auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after method name");
                    if (!left_paren_result)
                        return std::unexpected(left_paren_result.error());

                    std::vector<std::unique_ptr<ast::Expr>> arguments;
                    if (!check(lex::TokenType::RightParen))
                    {
                        do {
                            auto arg_result = expression();
                            if (!arg_result)
                                return std::unexpected(arg_result.error());
                            arguments.push_back(std::move(arg_result.value()));
                        } while (match({lex::TokenType::Comma}));
                    }

                    auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after arguments");
                    if (!right_paren_result)
                        return std::unexpected(right_paren_result.error());

                    auto method_call = std::make_unique<ast::Expr>();
                    method_call->node = ast::Method_call_expr{.object = std::move(expr),
                                                              .method_name = name_result.value().lexeme,
                                                              .arguments = std::move(arguments),
                                                              .type = types::Type(types::Primitive_kind::Void),
                                                              .loc = {name_result.value().line, name_result.value().column}};
                    expr = std::move(method_call);
                }
                else
                {
                    // Field access
                    auto field_access = std::make_unique<ast::Expr>();
                    field_access->node = ast::Field_access_expr{.object = std::move(expr),
                                                                .field_name = name_result.value().lexeme,
                                                                .type = types::Type(types::Primitive_kind::Void),
                                                                .loc = {name_result.value().line, name_result.value().column}};
                    expr = std::move(field_access);
                }
            }
            else
            {
                break;
            }
        }
        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> primary()
    {
        if (peek().type == lex::TokenType::Print)
            return std::unexpected(create_error(peek(), "print is a statement, not an expression. Did you forget a semicolon?"));

        if (match({lex::TokenType::This}))
        {
            if (current_model.empty())
                return std::unexpected(create_error(previous(), "Cannot use 'this' outside of a model method"));

            auto model_it = model_types.find(current_model);
            if (model_it == model_types.end())
                return std::unexpected(create_error(previous(), "Unknown model type"));

            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Variable_expr{.name = "this", .type = model_it->second, .loc = {previous().line, previous().column}};
            return expr;
        }

        if (match({lex::TokenType::Pipe}))
            return parse_closure_expression();

        if (match({lex::TokenType::LeftBracket}))
            return parse_array_literal();

        if (match({lex::TokenType::Bool}))
        {
            bool value = std::get<bool>(previous().literal);
            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Literal_expr{
                .value = Value(value), .type = types::Type(types::Primitive_kind::Bool), .loc = {previous().line, previous().column}};
            return expr;
        }

        if (match({lex::TokenType::Integer64}))
        {
            int64_t value = std::get<int64_t>(previous().literal);
            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Literal_expr{
                .value = Value(value), .type = types::Type(types::Primitive_kind::Int), .loc = {previous().line, previous().column}};
            return expr;
        }

        if (match({lex::TokenType::Float64}))
        {
            double value = std::get<double>(previous().literal);
            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Literal_expr{
                .value = Value(value), .type = types::Type(types::Primitive_kind::Float), .loc = {previous().line, previous().column}};
            return expr;
        }

        if (match({lex::TokenType::String}))
        {
            std::string value = std::get<std::string>(previous().literal);
            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Literal_expr{
                .value = Value(value), .type = types::Type(types::Primitive_kind::String), .loc = {previous().line, previous().column}};
            return expr;
        }

        if (match({lex::TokenType::Identifier}))
        {
            std::string name = previous().lexeme;

            if (check(lex::TokenType::LeftBrace))
                return parse_model_literal(name);

            types::Type t = types::Type(types::Primitive_kind::Void);  // Default
            auto it = variable_types.find(name);
            if (it != variable_types.end())
                t = it->second;

            auto expr = std::make_unique<ast::Expr>();
            expr->node = ast::Variable_expr{.name = name, .type = t, .loc = {previous().line, previous().column}};
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

    Result<std::unique_ptr<ast::Expr>> parse_closure_expression()
    {
        size_t line = previous().line;
        size_t column = previous().column;

        std::vector<std::pair<std::string, types::Type>> parameters;

        if (!check(lex::TokenType::Pipe))
        {
            do {
                auto param_name_result = consume(lex::TokenType::Identifier, "Expect parameter name");
                if (!param_name_result)
                    return std::unexpected(param_name_result.error());

                auto colon_result = consume(lex::TokenType::Colon, "Expect ':' after parameter name");
                if (!colon_result)
                    return std::unexpected(colon_result.error());

                auto param_type_result = parse_type();
                if (!param_type_result)
                    return std::unexpected(param_type_result.error());

                parameters.emplace_back(param_name_result.value().lexeme, param_type_result.value());
            } while (match({lex::TokenType::Comma}));
        }

        auto right_pipe_result = consume(lex::TokenType::Pipe, "Expect '|' after closure parameters");
        if (!right_pipe_result)
            return std::unexpected(right_pipe_result.error());

        types::Type return_type = types::Type(types::Primitive_kind::Void);
        if (match({lex::TokenType::Arrow}))
        {
            if (peek().type != lex::TokenType::LeftBrace)
            {
                auto return_type_result = parse_type();
                if (!return_type_result)
                    return std::unexpected(return_type_result.error());
                return_type = return_type_result.value();
            }
        }

        auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' before closure body");
        if (!left_brace_result)
            return std::unexpected(left_brace_result.error());

        auto body_result = block_statement();
        if (!body_result)
            return std::unexpected(body_result.error());

        auto closure_type = std::make_shared<types::Closure_type>();
        auto func_type = std::make_shared<types::Function_type>();

        for (const auto &param : parameters) func_type->parameter_types.push_back(param.second);
        func_type->return_type = return_type;

        closure_type->function_type = *func_type;

        auto expr = std::make_unique<ast::Expr>();
        expr->node = ast::Closure_expr{.parameters = std::move(parameters),
                                       .return_type = return_type,
                                       .body = std::shared_ptr<ast::Stmt>(std::move(body_result.value())),
                                       .type = types::Type{closure_type},
                                       .loc = {line, column}};

        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> parse_array_literal()
    {
        size_t line = previous().line;
        size_t column = previous().column;

        std::vector<std::shared_ptr<ast::Expr>> elements;

        if (!check(lex::TokenType::RightBracket))
        {
            do {
                auto element_result = expression();
                if (!element_result)
                    return std::unexpected(element_result.error());
                elements.push_back(std::move(element_result.value()));
            } while (match({lex::TokenType::Comma}));
        }

        auto right_bracket_result = consume(lex::TokenType::RightBracket, "Expect ']' after array elements");
        if (!right_bracket_result)
            return std::unexpected(right_bracket_result.error());

        auto expr = std::make_unique<ast::Expr>();
        expr->node = ast::Array_literal_expr{
            .elements = std::move(elements), .type = types::Type(types::Primitive_kind::Void), .loc = {line, column}};

        return expr;
    }

    Result<std::unique_ptr<ast::Expr>> parse_model_literal(const std::string &model_name)
    {
        auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' after model name");
        if (!left_brace_result)
            return std::unexpected(left_brace_result.error());

        std::vector<std::pair<std::string, std::unique_ptr<ast::Expr>>> fields;

        if (!check(lex::TokenType::RightBrace))
        {
            do {
                auto dot_result = consume(lex::TokenType::Dot, "Expect '.' before field name");
                if (!dot_result)
                    return std::unexpected(dot_result.error());

                auto field_name_result = consume(lex::TokenType::Identifier, "Expect field name");
                if (!field_name_result)
                    return std::unexpected(field_name_result.error());

                auto assign_result = consume(lex::TokenType::Assign, "Expect '=' after field name");
                if (!assign_result)
                    return std::unexpected(assign_result.error());

                auto value_result = expression();
                if (!value_result)
                    return std::unexpected(value_result.error());

                fields.emplace_back(field_name_result.value().lexeme, std::move(value_result.value()));

                if (!check(lex::TokenType::RightBrace))
                {
                    auto comma_result = consume(lex::TokenType::Comma, "Expect ',' between field initializers");
                    if (!comma_result)
                        return std::unexpected(comma_result.error());
                }
            } while (!check(lex::TokenType::RightBrace) && !is_at_end());
        }

        auto right_brace_result = consume(lex::TokenType::RightBrace, "Expect '}' after field initializers");
        if (!right_brace_result)
            return std::unexpected(right_brace_result.error());

        auto model_it = model_types.find(model_name);
        types::Type model_type = (model_it != model_types.end()) ? model_it->second : types::Type(types::Primitive_kind::Void);

        auto expr = std::make_unique<ast::Expr>();
        expr->node = ast::Model_literal_expr{
            .model_name = model_name, .fields = std::move(fields), .type = model_type, .loc = {previous().line, previous().column}};

        return expr;
    }

    Result<types::Type> parse_type()
    {
        if (match({lex::TokenType::Pipe}))
        {
            std::vector<types::Type> parameter_types;

            if (!check(lex::TokenType::Pipe))
            {
                do {
                    auto param_type_result = parse_type();
                    if (!param_type_result)
                        return std::unexpected(param_type_result.error());
                    parameter_types.push_back(param_type_result.value());
                } while (match({lex::TokenType::Comma}));
            }

            auto right_pipe_result = consume(lex::TokenType::Pipe, "Expect '|' after closure type parameters");
            if (!right_pipe_result)
                return std::unexpected(right_pipe_result.error());

            auto arrow_result = consume(lex::TokenType::Arrow, "Expect '->' after closure parameters");
            if (!arrow_result)
                return std::unexpected(arrow_result.error());

            auto return_type_result = parse_type();
            if (!return_type_result)
                return std::unexpected(return_type_result.error());

            auto closure_type = std::make_shared<types::Closure_type>();
            auto func_type = std::make_shared<types::Function_type>();
            func_type->parameter_types = parameter_types;
            func_type->return_type = return_type_result.value();
            closure_type->function_type = *func_type;

            return types::Type{closure_type};
        }
        if (match({lex::TokenType::Identifier}))
        {
            std::string type_name = previous().lexeme;

            types::Type base_type;
            if (type_name == "i64")
                base_type = types::Type(types::Primitive_kind::Int);
            else if (type_name == "f64")
                base_type = types::Type(types::Primitive_kind::Float);
            else if (type_name == "bool")
                base_type = types::Type(types::Primitive_kind::Bool);
            else if (type_name == "string")
                base_type = types::Type(types::Primitive_kind::String);
            else if (type_name == "void")
                base_type = types::Type(types::Primitive_kind::Void);
            else
            {
                auto model_it = model_types.find(type_name);
                if (model_it != model_types.end())
                    base_type = model_it->second;
                else
                    return std::unexpected(create_error(previous(), "Unknown type: " + type_name));
            }

            if (match({lex::TokenType::LeftBracket}))
            {
                if (match({lex::TokenType::RightBracket}))
                    return types::Type(std::make_shared<types::Array_type>(base_type));
                else
                    return std::unexpected(create_error(peek(), "Fixed-size arrays not yet supported"));
            }

            return base_type;
        }

        return std::unexpected(create_error(peek(), "Expect type"));
    }
};

}  // namespace phos
