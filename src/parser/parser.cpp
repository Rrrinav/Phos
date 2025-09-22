#include "parser.hpp"

#include <cstddef>
#include <print>
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

namespace phos {

void Parser::skip_newlines()
{
    while (!is_at_end() && match({lex::TokenType::Newline})) {}
}

Result<std::vector<ast::Stmt*>> Parser::parse()
{
    std::vector<ast::Stmt*> statements;
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
            statements.push_back(*decl_result.value());
    }
    return statements;
}

const lex::Token &Parser::peek() const
{
    if (current_ >= tokens_.size())
    {
        static lex::Token eof_token(lex::TokenType::Eof, "", std::monostate{}, 0, 0);
        return eof_token;
    }
    return tokens_[current_];
}

const lex::Token &Parser::previous() const
{
    if (current_ == 0)
        return tokens_[0];
    return tokens_[current_ - 1];
}

lex::Token Parser::advance()
{
    if (!is_at_end())
        current_++;
    return previous();
}

bool Parser::match(std::initializer_list<lex::TokenType> types)
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

Result<lex::Token> Parser::consume(lex::TokenType type, const std::string &message)
{
    if (check(type))
        return advance();
    return std::unexpected(err::msg(message, "parser", peek().line, peek().column));
}

err::msg Parser::create_error(const lex::Token &token, const std::string &message)
{
    return err::msg(message, this->stage_, token.line, token.column);
}

void Parser::synchronize()
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

Result<std::optional<ast::Stmt*>> Parser::declaration()
{
    skip_newlines();
    if (is_at_end())
        return std::optional<ast::Stmt*>{std::nullopt};

    if (match({lex::TokenType::Fn}))
    {
        auto func_result = function_declaration();
        if (!func_result)
        {
            synchronize();
            return std::unexpected(func_result.error());
        }
        return std::optional<ast::Stmt*>{func_result.value()};
    }

    if (match({lex::TokenType::Model}))
    {
        auto model_result = model_declaration();
        if (!model_result)
        {
            synchronize();
            return std::unexpected(model_result.error());
        }
        return std::optional<ast::Stmt*>{model_result.value()};
    }

    if (match({lex::TokenType::Let}))
    {
        auto var_result = var_declaration();
        if (!var_result)
        {
            synchronize();
            return std::unexpected(var_result.error());
        }
        return std::optional<ast::Stmt*>{var_result.value()};
    }

    auto stmt_result = statement();
    if (!stmt_result)
    {
        synchronize();
        return std::unexpected(stmt_result.error());
    }
    return std::optional<ast::Stmt*>{stmt_result.value()};
}

Result<ast::Stmt*> Parser::function_declaration()
{
    auto name_result = consume(lex::TokenType::Identifier, "Expect function name");
    if (!name_result)
        return std::unexpected(name_result.error());
    lex::Token name = name_result.value();

    auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after function name");
    if (!left_paren_result)
        return std::unexpected(left_paren_result.error());

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen))
    {
        do {
            bool is_const = match({lex::TokenType::Const});
            auto param_name_result = consume(lex::TokenType::Identifier, "Expect parameter name");
            if (!param_name_result)
                return std::unexpected(param_name_result.error());

            auto colon_result = consume(lex::TokenType::Colon, "Expect ':' after parameter name");
            if (!colon_result)
                return std::unexpected(colon_result.error());

            auto param_type_result = parse_type();
            if (!param_type_result)
                return std::unexpected(param_type_result.error());

            parameters.emplace_back(ast::Function_param{param_name_result.value().lexeme, param_type_result.value(), is_const});
        } while (match({lex::TokenType::Comma}));
    }

    auto saved_variable_types = this->variable_types_;

    for (const auto &param : parameters) variable_types_[param.name] = param.type;

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

    function_types_.emplace(name.lexeme, return_type);
    auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' before function body");
    if (!left_brace_result)
        return std::unexpected(left_brace_result.error());

    auto body_result = block_statement();
    if (!body_result)
        return std::unexpected(body_result.error());

    this->variable_types_ = saved_variable_types;

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::Function_stmt {
            .name = name.lexeme,
            .parameters = parameters,
            .return_type = return_type,
            .body = body_result.value(),
            .loc = ast::Source_location{name.line, name.column}
        }
    });
}

Result<ast::Stmt*> Parser::model_declaration()
{
    auto name_res = consume(lex::TokenType::Identifier, "Expected model name");
    if (!name_res)
        return std::unexpected(name_res.error());
    lex::Token name = name_res.value();

    current_model_ = name.lexeme;

    auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' after model name");
    if (!left_brace_result)
        return std::unexpected(left_brace_result.error());

    std::vector<std::pair<std::string, types::Type>> fields;
    std::vector<ast::Function_stmt *> methods;

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
                model_types_[name.lexeme] = types::Type(std::make_shared<types::Model_type>(model_type));
            }
            parsing_fields = false;
            advance();
            auto method_result = parse_model_method();
            if (!method_result)
                return std::unexpected(method_result.error());
            methods.push_back(this->arena_.create<ast::Function_stmt>(method_result.value()));
        }
        else if (peek().type == lex::TokenType::Let)
        {
            if (!parsing_fields)
                return std::unexpected(create_error(peek(), "Write all the fields before functions in the model"));
            advance();
            auto field_result = parse_model_field();
            if (!field_result)
                return std::unexpected(field_result.error());
            fields.push_back(field_result.value());
        }
        else
    {
            return std::unexpected(create_error(peek(), "Expect field or method declaration in model"));
        }
    }

    auto right_brace_result = consume(lex::TokenType::RightBrace, "Expect '}' after model body");
    if (!right_brace_result)
        return std::unexpected(right_brace_result.error());

    if (parsing_fields)
    {
        model_type.name = name.lexeme;
        for (const auto &field : fields) model_type.fields[field.first] = field.second;
        model_types_[name.lexeme] = types::Type(std::make_shared<types::Model_type>(model_type));
    }

    for (const auto &method : methods)
    {
        types::Function_type func_type;
        for (const auto &param : method->parameters) func_type.parameter_types.push_back(param.type);
        func_type.return_type = method->return_type;
        // This is safe because model_type is now guaranteed to exist in the map
        std::get<std::shared_ptr<types::Model_type>>(model_types_[name.lexeme])->methods[method->name] = func_type;
    }

    current_model_.clear();


    auto m = mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::Model_stmt {
            .name = name.lexeme,
            .fields = fields,
            .methods = methods,
            .loc = {name.line, name.column}
        }
    });
    return m;
}

Result<std::pair<std::string, types::Type>> Parser::parse_model_field()
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

    ast::Expr* initializer = nullptr;
    if (match({lex::TokenType::Assign}))
    {
        auto init_result = expression();
        if (!init_result)
            return std::unexpected(init_result.error());
        initializer = init_result.value();
    }

    auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after field declaration");
    if (!semicolon_result)
        return std::unexpected(semicolon_result.error());

    return std::make_pair(name_result.value().lexeme, type_result.value());
}

Result<ast::Function_stmt> Parser::parse_model_method()
{
    auto name_result = consume(lex::TokenType::Identifier, "Expect method name");
    if (!name_result)
        return std::unexpected(name_result.error());
    lex::Token name = name_result.value();

    auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after method name");
    if (!left_paren_result)
        return std::unexpected(left_paren_result.error());

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen))
    {
        do {
            bool is_const = match({lex::TokenType::Const});
            auto param_name_result = consume(lex::TokenType::Identifier, "Expect parameter name");
            if (!param_name_result)
                return std::unexpected(param_name_result.error());

            auto colon_result = consume(lex::TokenType::Colon, "Expect ':' after parameter name");
            if (!colon_result)
                return std::unexpected(colon_result.error());

            auto param_type_result = parse_type();
            if (!param_type_result)
                return std::unexpected(param_type_result.error());

            parameters.emplace_back(ast::Function_param{param_name_result.value().lexeme, param_type_result.value(), is_const});
        } while (match({lex::TokenType::Comma}));
    }

    auto saved_variable_types = this->variable_types_;
    variable_types_["this"] = model_types_[current_model_];
    for (const auto &param : parameters) variable_types_[param.name] = param.type;

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

    function_types_.emplace(name.lexeme, return_type);
    auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' before method body");
    if (!left_brace_result)
        return std::unexpected(left_brace_result.error());

    auto body_result = block_statement();
    if (!body_result)
        return std::unexpected(body_result.error());

    this->variable_types_ = saved_variable_types;


    return ast::Function_stmt {
        .name = name.lexeme,
        .parameters = parameters,
        .return_type = return_type,
        .body = body_result.value(),
        .loc = {name.line, name.column}
    };
}

Result<ast::Stmt*> Parser::var_declaration()
{
    bool is_const = match({lex::TokenType::Const});
    auto name_result = consume(lex::TokenType::Identifier, "Expect variable name");
    if (!name_result)
        return std::unexpected(name_result.error());
    lex::Token name = name_result.value();

    types::Type var_type = types::Type(types::Primitive_kind::Void);
    ast::Expr* initializer = nullptr;
    bool type_inferred = false;

    if (match({lex::TokenType::Colon}))
    {
        if (match({lex::TokenType::Assign}))
        {
            auto init_result = expression();
            if (!init_result)
                return std::unexpected(init_result.error());
            initializer = init_result.value();
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
                Result<ast::Expr*> init_result;
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
                initializer = init_result.value();
            }
        }
    }

    auto semicolon_result = consume(lex::TokenType::Semicolon, "Expected ';' after variable declaration");
    if (!semicolon_result)
        return std::unexpected(semicolon_result.error());

    variable_types_[name.lexeme] = var_type;

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::Var_stmt {
            .is_const = is_const,
            .name = name.lexeme,
            .type = var_type,
            .initializer = initializer,
            .type_inferred = type_inferred,
            .loc = {name.line, name.column},
        }
    });
}

Result<ast::Stmt*> Parser::statement()
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

Result<ast::Stmt*> Parser::print_statement(ast::Print_stream stream)
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

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::Print_stmt {
            .stream = stream,
            .expression = expr_result.value(),
            .loc = {previous().line, previous().column}
        }
    });
}

Result<ast::Stmt*> Parser::block_statement()
{
    std::vector<ast::Stmt*> statements;
    size_t line = previous().line;
    size_t column = previous().column;

    auto saved_variable_types = variable_types_;

    while (!check(lex::TokenType::RightBrace) && !is_at_end())
    {
        auto stmt_result = declaration();
        if (!stmt_result)
        {
            variable_types_ = saved_variable_types;
            return std::unexpected(stmt_result.error());
        }

        if (stmt_result.value())
            statements.push_back(*stmt_result.value());
    }

    auto right_brace_result = consume(lex::TokenType::RightBrace, "Expect '}' after block");
    if (!right_brace_result)
    {
        variable_types_ = saved_variable_types;
        return std::unexpected(right_brace_result.error());
    }

    variable_types_ = saved_variable_types;

    return mem::Arena::alloc(this->arena_, ast::Stmt{
        ast::Block_stmt {
            .statements = statements,
            .loc = {line, column}
        }
    });
}

Result<ast::Stmt*> Parser::if_statement()
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

    ast::Stmt* else_branch = nullptr;
    if (match({lex::TokenType::Else}))
    {
        auto else_branch_result = statement();
        if (!else_branch_result)
            return std::unexpected(else_branch_result.error());
        else_branch = else_branch_result.value();
    }

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::If_stmt {
            .condition = condition_result.value(),
            .then_branch = then_branch_result.value(),
            .else_branch = else_branch,
            .loc = {previous().line, previous().column}
        }
    });
}

Result<ast::Stmt*> Parser::while_statement()
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

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::While_stmt {
            .condition = condition_result.value(),
            .body = body_result.value(),
            .loc = {previous().line, previous().column}
        }
    });
}

Result<ast::Stmt*> Parser::for_statement()
{
    auto left_paren_result = consume(lex::TokenType::LeftParen, "Expect '(' after 'for'");
    if (!left_paren_result)
        return std::unexpected(left_paren_result.error());

    ast::Stmt* initializer = nullptr;
    if (match({lex::TokenType::Semicolon})) {}
    else if (match({lex::TokenType::Let}))
    {
        auto init_result = var_declaration();
        if (!init_result)
            return std::unexpected(init_result.error());
        initializer = init_result.value();
    }
    else
    {
        auto init_result = expression_statement();
        if (!init_result)
            return std::unexpected(init_result.error());
        initializer = init_result.value();
    }

    ast::Expr* condition = nullptr;
    if (!check(lex::TokenType::Semicolon))
    {
        auto condition_result = expression();
        if (!condition_result)
            return std::unexpected(condition_result.error());
        condition = condition_result.value();
    }

    auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after loop condition");
    if (!semicolon_result)
        return std::unexpected(semicolon_result.error());

    ast::Expr* increment = nullptr;
    if (!check(lex::TokenType::RightParen))
    {
        auto increment_result = expression();
        if (!increment_result)
            return std::unexpected(increment_result.error());
        increment = increment_result.value();
    }

    auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after for clauses");
    if (!right_paren_result)
        return std::unexpected(right_paren_result.error());

    auto body_result = statement();
    if (!body_result)
        return std::unexpected(body_result.error());
    auto body = body_result.value();

    if (increment)
    {
        std::vector<ast::Stmt*> body_stmts;
        body_stmts.push_back(body);

        ast::Expr_stmt increment_stmt{increment, {previous().line, previous().column}};
        body_stmts.push_back(mem::Arena::alloc(this->arena_, ast::Stmt{increment_stmt}));

        ast::Block_stmt block_node{body_stmts, {previous().line, previous().column}};
        body = mem::Arena::alloc(this->arena_, ast::Stmt{block_node});
    }

    if (!condition)
    {
        ast::Literal_expr true_literal{Value{true}, types::Type(types::Primitive_kind::Bool), {previous().line, previous().column}};
        condition = mem::Arena::alloc(this->arena_, ast::Expr{true_literal});
    }

    ast::While_stmt while_node{condition, body, {previous().line, previous().column}};
    auto while_loop = mem::Arena::alloc(this->arena_, ast::Stmt{while_node});

    if (initializer)
    {
        std::vector<ast::Stmt*> stmts;
        stmts.push_back(initializer);
        stmts.push_back(while_loop);

        ast::Block_stmt block_node{stmts, {previous().line, previous().column}};
        return mem::Arena::alloc(this->arena_, ast::Stmt{block_node});
    }

    return while_loop;
}

Result<ast::Stmt*> Parser::return_statement()
{
    size_t line = previous().line;
    size_t column = previous().column;
    ast::Expr* value = nullptr;

    if (!check(lex::TokenType::Semicolon))
    {
        auto value_result = expression();
        if (!value_result)
            return std::unexpected(value_result.error());
        value = value_result.value();
    }

    auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after return value");
    if (!semicolon_result)
        return std::unexpected(semicolon_result.error());

    return mem::Arena::alloc(this->arena_, ast::Stmt{ast::Return_stmt{value, {line, column}}});
}

Result<ast::Stmt*> Parser::expression_statement()
{
    auto expr_result = expression();
    if (!expr_result)
        return std::unexpected(expr_result.error());

    auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after expression");
    if (!semicolon_result)
        return std::unexpected(semicolon_result.error());

    return mem::Arena::alloc(this->arena_, ast::Stmt{ast::Expr_stmt{expr_result.value(), {previous().line, previous().column}}});
}

Result<ast::Expr*> Parser::expression() { return assignment(); }

Result<ast::Expr*> Parser::assignment()
{
    auto expr_result = logical_or();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

    if (match({lex::TokenType::Assign}))
    {
        lex::Token equals = previous();
        auto value_result = assignment();
        if (!value_result)
            return std::unexpected(value_result.error());

        if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr->node))
        {
            return mem::Arena::alloc(this->arena_, ast::Expr{
                ast::Assignment_expr{var_expr->name, value_result.value(), var_expr->type, {equals.line, equals.column}}});
        }
        else if (auto *field_access_expr = std::get_if<ast::Field_access_expr>(&expr->node))
        {
            return mem::Arena::alloc(this->arena_, ast::Expr {
                ast::Field_assignment_expr{
                    .object = field_access_expr->object,
                    .field_name = field_access_expr->field_name,
                    .value = value_result.value(),
                    .loc = {equals.line, equals.column}
                }
            });
        }
        else if (auto *array_access_expr = std::get_if<ast::Array_access_expr>(&expr->node))
        {
            return mem::Arena::alloc(this->arena_, ast::Expr {
                ast::Array_assignment_expr {
                    .array = array_access_expr->array,
                    .index = array_access_expr->index,
                    .value = value_result.value(),
                    .type = types::Type(types::Primitive_kind::Void),
                    .loc = {equals.line, equals.column}
                }
            });
        }

        return std::unexpected(create_error(equals, "Invalid assignment target"));
    }
    return expr;
}

Result<ast::Expr*> Parser::logical_or()
{
    auto expr_result = logical_and();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

    while (match({lex::TokenType::LogicalOr}))
    {
        lex::Token op = previous();
        auto right_result = logical_and();
        if (!right_result)
            return std::unexpected(right_result.error());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right_result.value(),
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {op.line, op.column}
            }
        });
        expr = binary_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::logical_and()
{
    auto expr_result = equality();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

    while (match({lex::TokenType::LogicalAnd}))
    {
        lex::Token op = previous();
        auto right_result = equality();
        if (!right_result)
            return std::unexpected(right_result.error());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr {
                .left = expr,
                .op = op.type,
                .right = right_result.value(),
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {op.line, op.column}
            }
        });
        expr = binary_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::equality()
{
    auto expr_result = comparison();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

    while (match({lex::TokenType::Equal, lex::TokenType::NotEqual}))
    {
        lex::Token op = previous();
        auto right_result = comparison();
        if (!right_result)
            return std::unexpected(right_result.error());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right_result.value(),
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {op.line, op.column}
            }
        });
        expr = binary_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::comparison()
{
    auto expr_result = term();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

    while (match({lex::TokenType::Less, lex::TokenType::LessEqual, lex::TokenType::Greater, lex::TokenType::GreaterEqual}))
    {
        lex::Token op = previous();
        auto right_result = term();
        if (!right_result)
            return std::unexpected(right_result.error());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right_result.value(),
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {op.line, op.column}
            }
        });
        expr = binary_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::term()
{
    auto expr_result = factor();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

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

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr {
                .left = expr,
                .op = op.type,
                .right = right_result.value(),
                .type = result_type,
                .loc = {op.line, op.column}
            }
        });
        expr = binary_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::factor()
{
    auto expr_result = cast();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

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

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr {
                .left = expr,
                .op = op.type,
                .right = right_result.value(),
                .type = result_type,
                .loc = {op.line, op.column}
            }
        });
        expr = binary_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::cast()
{
    auto expr_result = unary();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

    while (match({lex::TokenType::As}))
    {
        auto target_type_result = parse_type();
        if (!target_type_result)
            return std::unexpected(target_type_result.error());

        auto loc = ast::get_loc(expr->node);
        auto cast_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Cast_expr {
                .expression = expr,
                .target_type = target_type_result.value(),
                .loc = loc
            }
        });
        expr = cast_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::unary()
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

        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Unary_expr{
                .op = op.type,
                .right = right_result.value(),
                .type = result_type,
                .loc = {op.line, op.column}
            }
        });
    }
    return call();
}

Result<ast::Expr*> Parser::call()
{
    auto expr_result = primary();
    if (!expr_result)
        return std::unexpected(expr_result.error());
    auto expr = expr_result.value();

    while (true)
    {
        if (match({lex::TokenType::LeftParen}))
        {
            std::vector<ast::Expr*> arguments;
            if (!check(lex::TokenType::RightParen))
            {
                do {
                    auto arg_result = expression();
                    if (!arg_result)
                        return std::unexpected(arg_result.error());
                    arguments.push_back(arg_result.value());
                } while (match({lex::TokenType::Comma}));
            }

            auto paren_result = consume(lex::TokenType::RightParen, "Expect ')' after arguments");
            if (!paren_result)
                return std::unexpected(paren_result.error());
            lex::Token paren = paren_result.value();

            if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr->node))
            {
                auto func_it = function_types_.find(var_expr->name);
                types::Type return_type =
                    (func_it != function_types_.end()) ? func_it->second : types::Type(types::Primitive_kind::Void);

                auto call_expr = mem::Arena::alloc(this->arena_, ast::Expr {
                    ast::Call_expr{
                        .callee = var_expr->name,
                        .arguments = arguments,
                        .type = return_type,
                        .loc = {paren.line, paren.column}
                    }
                });
                expr = call_expr;
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

            auto array_access = mem::Arena::alloc(this->arena_, ast::Expr {
                ast::Array_access_expr {
                    .array = expr,
                    .index = index_result.value(),
                    .type = types::Type(types::Primitive_kind::Void),
                    .loc = {right_bracket_result.value().line, right_bracket_result.value().column}
                }
            });
            expr = array_access;
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

                std::vector<ast::Expr*> arguments;
                if (!check(lex::TokenType::RightParen))
                {
                    do {
                        auto arg_result = expression();
                        if (!arg_result)
                            return std::unexpected(arg_result.error());
                        arguments.push_back(arg_result.value());
                    } while (match({lex::TokenType::Comma}));
                }

                auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after arguments");
                if (!right_paren_result)
                    return std::unexpected(right_paren_result.error());

                auto method_call = mem::Arena::alloc(this->arena_, ast::Expr {
                    ast::Method_call_expr{
                        .object = expr,
                        .method_name = name_result.value().lexeme,
                        .arguments = arguments,
                        .type = types::Type(types::Primitive_kind::Void),
                        .loc = {name_result.value().line, name_result.value().column}}
                });
                expr = method_call;
            }
            else
            {
                // Field access
                auto field_access = mem::Arena::alloc(this->arena_, ast::Expr{
                    ast::Field_access_expr{
                        .object = expr,
                        .field_name = name_result.value().lexeme,
                        .type = types::Type(types::Primitive_kind::Void),
                        .loc = {name_result.value().line, name_result.value().column}}
                });
                expr = field_access;
            }
        }
        else
        {
            break;
        }
    }
    return expr;
}

Result<ast::Expr*> Parser::primary()
{
    if (peek().type == lex::TokenType::Print)
        return std::unexpected(create_error(peek(), "print is a statement, not an expression. Did you forget a semicolon?"));

    if (match({lex::TokenType::This}))
    {
        if (current_model_.empty())
            return std::unexpected(create_error(previous(), "Cannot use 'this' outside of a model method"));

        auto model_it = model_types_.find(current_model_);
        if (model_it == model_types_.end())
            return std::unexpected(create_error(previous(), "Unknown model type"));

        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Variable_expr {
                .name = "this",
                .type = model_it->second,
                .loc = {previous().line, previous().column}
            }
        });
    }

    if (peek().type == lex::TokenType::Pipe || peek().type == lex::TokenType::LogicalOr)
        return parse_closure_expression();

    if (match({lex::TokenType::LeftBracket}))
        return parse_array_literal();

    if (match({lex::TokenType::Nil}))
    {
        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Literal_expr {
            .value = Value(nullptr),
            .type = types::Type(types::Primitive_kind::Nil),
            .loc = {previous().line, previous().column}}
        });
    }
    if (match({lex::TokenType::Bool}))
    {
        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Literal_expr {
                .value = Value(std::get<bool>(previous().literal)),
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {previous().line, previous().column}
            }
        });
    }

    if (match({lex::TokenType::Integer64}))
    {
        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Literal_expr {
                .value = Value(std::get<int64_t>(previous().literal)),
                .type = types::Type(types::Primitive_kind::Int),
                .loc = {previous().line, previous().column}
            }
        });
    }

    if (match({lex::TokenType::Float64}))
    {
        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Literal_expr{
                .value = Value(std::get<double>(previous().literal)),
                .type = types::Type(types::Primitive_kind::Float),
                .loc = {previous().line, previous().column}
            }
        });
    }

    if (match({lex::TokenType::String}))
    {
        return mem::Arena::alloc(this->arena_, ast::Expr{
            ast::Literal_expr{
                .value = Value(std::get<std::string>(previous().literal)),
                .type = types::Type(types::Primitive_kind::String),
                .loc = {previous().line, previous().column}
            }
        });
    }
    if (match({lex::TokenType::Nil}))
    {
        return mem::Arena::alloc(this->arena_, ast::Expr{
            ast::Literal_expr{
                .value = Value(nullptr),
                .type = types::Type(types::Primitive_kind::Nil),
                .loc = {previous().line, previous().column}
            }
        });
    }
    if (match({lex::TokenType::Identifier}))
    {
        std::string name = previous().lexeme;

        if (check(lex::TokenType::LeftBrace))
            return parse_model_literal(name);

        types::Type t = types::Type(types::Primitive_kind::Void);  // Default
        auto it = variable_types_.find(name);
        if (it != variable_types_.end())
            t = it->second;

        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Variable_expr {
                .name = name,
                .type = t,
                .loc = {previous().line, previous().column}
            }
        });
    }

    if (match({lex::TokenType::LeftParen}))
    {
        auto expr_result = expression();
        if (!expr_result)
            return std::unexpected(expr_result.error());

        auto right_paren_result = consume(lex::TokenType::RightParen, "Expect ')' after expression");
        if (!right_paren_result)
            return std::unexpected(right_paren_result.error());

        return expr_result.value();
    }

    return std::unexpected(create_error(peek(), "Expect expression. Found: " + peek().lexeme));
}

Result<ast::Expr*> Parser::parse_closure_expression()
{
    size_t line = peek().line;
    size_t column = peek().column;

    std::vector<ast::Function_param> parameters;

    // --- Parameter Parsing Logic ---

    // Case 1: Zero parameters, written as '||'
    if (match({lex::TokenType::LogicalOr}))
    {
        // The parameter list is empty. We are done with this part.
    }
    // Case 2: One or more parameters, or zero parameters as '| |'
    else if (match({lex::TokenType::Pipe}))
    {
        // Check for the empty '| |' case first.
        if (!check(lex::TokenType::Pipe))
        {
            // It's not empty, so we parse one or more parameters.
            do {
                bool is_const = match({lex::TokenType::Const});
                auto param_name_result = consume(lex::TokenType::Identifier, "Expect parameter name");
                if (!param_name_result)
                    return std::unexpected(param_name_result.error());

                consume(lex::TokenType::Colon, "Expect ':' after parameter name");

                auto param_type_result = parse_type();
                if (!param_type_result)
                    return std::unexpected(param_type_result.error());

                parameters.emplace_back(ast::Function_param{param_name_result.value().lexeme, param_type_result.value(), is_const});
            } while (match({lex::TokenType::Comma}));
        }
        consume(lex::TokenType::Pipe, "Expect '|' after closure parameters");
    }

    // --- Return Type and Body Parsing ---

    types::Type return_type = types::Type(types::Primitive_kind::Void);
    bool return_type_inferred = true;

    if (match({lex::TokenType::Arrow}))
    {
        return_type_inferred = false;
        auto return_type_result = parse_type();
        if (!return_type_result)
            return std::unexpected(return_type_result.error());
        return_type = return_type_result.value();
    }

    ast::Stmt* body = nullptr;

    if (peek().type == lex::TokenType::LeftBrace)
    {
        // Case A: Body is a regular block statement `{ ... }`
        consume(lex::TokenType::LeftBrace, "Expect '{' before closure body.");
        auto block_result = block_statement();
        if (!block_result)
            return std::unexpected(block_result.error());
        body = block_result.value();
    }
    else
    {
        // Case B: Body is a single expression with an implicit return
        auto expr_result = expression();
        if (!expr_result)
            return std::unexpected(expr_result.error());

        auto expr_loc = ast::get_loc(expr_result.value()->node);

        // 1. Create a synthetic Return_stmt from the expression
        auto return_stmt_node = ast::Return_stmt{expr_result.value(), expr_loc};
        auto return_stmt = mem::Arena::alloc(this->arena_, ast::Stmt{return_stmt_node});

        // 2. Create a synthetic Block_stmt to hold our new return statement
        std::vector<ast::Stmt*> statements;
        statements.push_back(return_stmt);

        auto block_stmt_node = ast::Block_stmt{statements, {line, column}};
        body = mem::Arena::alloc(this->arena_, ast::Stmt{block_stmt_node});

        // If the return type was not specified, we infer it from the expression
        if (return_type_inferred)
        {
            return_type =
                ast::get_type(std::get<ast::Return_stmt>(std::get<ast::Block_stmt>(body->node).statements[0]->node).expression->node);
        }
    }

    // --- AST Node Creation ---
    auto closure_type = std::make_shared<types::Closure_type>();
    for (const auto &param : parameters) closure_type->function_type.parameter_types.push_back(param.type);
    closure_type->function_type.return_type = return_type;

    return mem::Arena::alloc(this->arena_, ast::Expr {
        ast::Closure_expr {
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
            .type = types::Type{closure_type},
            .loc = {line, column}}
    });
}

Result<ast::Expr*> Parser::parse_array_literal()
{
    size_t line = previous().line;
    size_t column = previous().column;

    std::vector<ast::Expr*> elements;

    if (!check(lex::TokenType::RightBracket))
    {
        do {
            auto element_result = expression();
            if (!element_result)
                return std::unexpected(element_result.error());
            elements.push_back(element_result.value());
        } while (match({lex::TokenType::Comma}));
    }

    auto right_bracket_result = consume(lex::TokenType::RightBracket, "Expect ']' after array elements");
    if (!right_bracket_result)
        return std::unexpected(right_bracket_result.error());

    return mem::Arena::alloc(this->arena_, ast::Expr {
        ast::Array_literal_expr {
            .elements = elements,
            .type = types::Type(types::Primitive_kind::Void),
            .loc = {line, column}
        }
    });

}

Result<ast::Expr*> Parser::parse_model_literal(const std::string &model_name)
{
    auto left_brace_result = consume(lex::TokenType::LeftBrace, "Expect '{' after model name");
    if (!left_brace_result)
        return std::unexpected(left_brace_result.error());

    std::vector<std::pair<std::string, ast::Expr*>> fields;

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

            fields.emplace_back(field_name_result.value().lexeme, value_result.value());

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

    auto model_it = model_types_.find(model_name);
    types::Type model_type = (model_it != model_types_.end()) ? model_it->second : types::Type(types::Primitive_kind::Void);

    return mem::Arena::alloc(this->arena_, ast::Expr {
        ast::Model_literal_expr {
            .model_name = model_name,
            .fields = fields,
            .type = model_type,
            .loc = {previous().line, previous().column}
        }
    });
}

Result<types::Type> Parser::parse_type()
{
    types::Type current_type;

    // --- Step 1: Parse the core, non-postfix type component ---
    if (match({lex::TokenType::LeftParen}))
    {
        auto type_res = parse_type();  // Fully recursive call for parenthesized types
        if (!type_res)
            return type_res;
        consume(lex::TokenType::RightParen, "Expected ')' after type in parentheses.");
        current_type = type_res.value();
    }
    else if (peek().type == lex::TokenType::Pipe || peek().type == lex::TokenType::LogicalOr)
    {
        // This is a closure type, e.g., `|| -> i64` or `|string| -> void`
        std::vector<types::Type> parameter_types;

        if (match({lex::TokenType::LogicalOr}))
        {
            // This was the '||' case for zero arguments. No parameters to parse.
        }
        else if (match({lex::TokenType::Pipe}))
        {
            // This was the '|' case. It could be empty ('| |') or have parameters.
            if (!check(lex::TokenType::Pipe))
            {
                do {
                    auto param_type_result = parse_type();
                    if (!param_type_result)
                        return param_type_result;
                    parameter_types.push_back(param_type_result.value());
                } while (match({lex::TokenType::Comma}));
            }
            consume(lex::TokenType::Pipe, "Expect '|' after closure parameter types.");
        }

        consume(lex::TokenType::Arrow, "Expect '->' after closure parameters");

        auto return_type_result = parse_type();
        if (!return_type_result)
            return return_type_result;

        auto closure_t = std::make_shared<types::Closure_type>();
        closure_t->function_type.parameter_types = parameter_types;
        closure_t->function_type.return_type = return_type_result.value();
        current_type = types::Type(closure_t);
    }
    else if (match({lex::TokenType::Identifier}))
    {
        std::string type_name = previous().lexeme;
        if (type_name == "i64")
            current_type = types::Type(types::Primitive_kind::Int);
        else if (type_name == "f64")
            current_type = types::Type(types::Primitive_kind::Float);
        else if (type_name == "bool")
            current_type = types::Type(types::Primitive_kind::Bool);
        else if (type_name == "string")
            current_type = types::Type(types::Primitive_kind::String);
        else if (type_name == "void")
            current_type = types::Type(types::Primitive_kind::Void);
        else if (type_name == "any")
            current_type = types::Type(types::Primitive_kind::Any);
        else
        {  // Assume it's a model name
            auto model_type = std::make_shared<types::Model_type>();
            model_type->name = type_name;
            current_type = types::Type(model_type);
        }
    }
    else
    {
        return std::unexpected(create_error(peek(), "Expected a type name or '(' for a grouped type."));
    }

    // --- Step 2: Loop for any postfix operators (e.g., [], ?) ---
    while (true)
    {
        if (match({lex::TokenType::LeftBracket}))
        {
            consume(lex::TokenType::RightBracket, "Expected ']' to complete array type suffix.");
            current_type = types::Type(std::make_shared<types::Array_type>(current_type));
        }
        else if (match({lex::TokenType::Question}))
        {
            auto optional_type = std::make_shared<types::Optional_type>();
            optional_type->base_type = current_type;
            current_type = types::Type(optional_type);
        }
        else
    {
            break;  // No more suffixes found
        }
    }

    return current_type;
}
}; // namespace phos
