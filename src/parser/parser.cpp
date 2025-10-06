#include "parser.hpp"

#include <cstddef>
#include <print>
#include <string>
#include <utility>
#include <vector>
#include <variant>
#include <expected>

#include "../lexer/token.hpp"
#include "../value/type.hpp"
#include "../error/err.hpp"
#include "../error/result.hpp"
#include "../utility/try_res.hpp"

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
            if (!decl_result.error().message.empty()) std::println(stderr, "{}", decl_result.error().format());

            synchronize();
            continue; // Continue parsing to find more errors
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
            case lex::TokenType::Model:
            case lex::TokenType::Union:
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

    if (match({lex::TokenType::Fn})) return std::optional<ast::Stmt*>{__Try(function_declaration())};
    if (match({lex::TokenType::Model})) return std::optional<ast::Stmt*>{__Try(model_declaration())};
    if (match({lex::TokenType::Union})) return std::optional<ast::Stmt*>{__Try(union_declaration())};
    if (match({lex::TokenType::Let})) return std::optional<ast::Stmt*>{__Try(var_declaration())};

    return std::optional<ast::Stmt*>{__Try(statement())};
}

Result<ast::Stmt*> Parser::function_declaration()
{
    lex::Token name = __Try(consume(lex::TokenType::Identifier, "Expect function name"));
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after function name"));

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen))
    {
        do {
            bool is_const = match({lex::TokenType::Const});
            auto param_name_result = __Try(consume(lex::TokenType::Identifier, "Expect parameter name"));
            __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after parameter name"));
            auto param_type_result = __Try(parse_type());
            parameters.emplace_back(ast::Function_param{param_name_result.lexeme, param_type_result, is_const});
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after parameters"));
    types::Type return_type = types::Type(types::Primitive_kind::Void);

    if (match({lex::TokenType::Arrow}))
        return_type = __Try(parse_type());

    function_types_.emplace(name.lexeme, return_type);
    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before function body"));

    auto body = __Try(block_statement());

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::Function_stmt {
            .name = name.lexeme,
            .is_static = false,
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
            .loc = ast::Source_location{name.line, name.column}
        }
    });
}

Result<ast::Stmt*> Parser::model_declaration()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected model name"));
    m_known_model_names.insert(name.lexeme);
    current_model_ = name.lexeme;

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after model name"));

    std::vector<std::pair<std::string, types::Type>> fields;
    std::vector<ast::Function_stmt *> methods;

    while (!check(lex::TokenType::RightBrace) && !is_at_end())
    {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) break;

        if (match({lex::TokenType::Fn}) || (peek().type == lex::TokenType::Static && tokens_[current_ + 1].type == lex::TokenType::Fn))
        {
            auto method_result = __Try(parse_model_method());
            methods.push_back(this->arena_.create<ast::Function_stmt>(method_result));
        }
        else if (match({lex::TokenType::Let}))
        {
             auto field_result = __Try(parse_model_field());
             fields.push_back(field_result);
        }
        else
        {
            return std::unexpected(create_error(peek(), "Expect field ('let') or method ('fn') declaration in model"));
        }
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after model body"));
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

Result<ast::Stmt*> Parser::union_declaration()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected union name"));
    m_known_union_names.insert(name.lexeme);

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after union name"));

    std::vector<std::pair<std::string, types::Type>> variants;
    while (!check(lex::TokenType::RightBrace) && !is_at_end())
    {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) break;
        __Try(consume(lex::TokenType::Let, "Expect 'let' before vairant name"));

        auto variant_name = __Try(consume(lex::TokenType::Identifier, "Expect variant name"));

        __Try(consume(lex::TokenType::Colon, "Expect ':' after vairant name for type"));

        types::Type variant_type = types::Type(types::Primitive_kind::Void);
        variant_type = __Try(parse_type());

        variants.push_back({variant_name.lexeme, variant_type});

        if (!check(lex::TokenType::RightBrace))
        {
            __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after variant declaration"));
        }
        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after union body"));

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::Union_stmt {
            .name = name.lexeme,
            .variants = variants,
            .loc = {name.line, name.column}
        }
    });
}


Result<std::pair<std::string, types::Type>> Parser::parse_model_field()
{
    auto name_result = __Try(consume(lex::TokenType::Identifier, "Expect field name"));
    __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after field name"));
    auto type_result = __Try(parse_type());
    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after field declaration"));
    return std::make_pair(name_result.lexeme, type_result);
}

Result<ast::Function_stmt> Parser::parse_model_method()
{
    bool is_static = match({lex::TokenType::Static});
    if(is_static) __TryIgnore(consume(lex::TokenType::Fn, "Expect 'fn' after 'static'."));

    auto name = __Try(consume(lex::TokenType::Identifier, "Expect method name"));
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after method name"));

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen))
    {
        do {
            bool is_const = match({lex::TokenType::Const});
            auto param_name_result = __Try(consume(lex::TokenType::Identifier, "Expect parameter name"));
            __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after parameter name"));
            auto param_type_result = __Try(parse_type());
            parameters.emplace_back(ast::Function_param{param_name_result.lexeme, param_type_result, is_const});
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after parameters"));

    types::Type return_type = types::Type(types::Primitive_kind::Void);
    if (match({lex::TokenType::Arrow}))
        return_type = __Try(parse_type());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before method body"));

    auto body_result = __Try(block_statement());

    return ast::Function_stmt {
        .name = name.lexeme,
        .is_static = is_static,
        .parameters = parameters,
        .return_type = return_type,
        .body = body_result,
        .loc = {name.line, name.column}
    };
}

Result<ast::Stmt*> Parser::var_declaration()
{
    bool is_const = match({lex::TokenType::Const});
    auto name = __Try(consume(lex::TokenType::Identifier, "Expect variable name"));

    types::Type var_type = types::Type(types::Primitive_kind::Void);
    ast::Expr* initializer = nullptr;
    bool type_inferred = false;

    if (match({lex::TokenType::Colon}))
    {
        if (match({lex::TokenType::Assign})) // This is ':='
        {
            initializer = __Try(expression());
            type_inferred = true;
        }
        else // This is just ':'
        {
            var_type = __Try(parse_type());
            if (match({lex::TokenType::Assign}))
            {
                initializer = __Try(expression());
            }
        }
    }
    else
    {
        return std::unexpected(create_error(peek(), "Expect ':' or ':=' after variable name."));
    }

    __TryIgnore(consume(lex::TokenType::Semicolon, "Expected ';' after variable declaration"));

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
    auto expr_result = __Try(expression());
    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after print statement"));

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::Print_stmt {
            .stream = stream,
            .expression = expr_result,
            .loc = {previous().line, previous().column}
        }
    });
}

Result<ast::Stmt*> Parser::block_statement()
{
    std::vector<ast::Stmt*> statements;
    size_t line = previous().line;
    size_t column = previous().column;

    while (!check(lex::TokenType::RightBrace) && !is_at_end())
    {
        auto stmt_result = declaration();
        if (!stmt_result)
        {
            return std::unexpected(stmt_result.error());
        }

        if (stmt_result.value())
            statements.push_back(*stmt_result.value());
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after block"));

    return mem::Arena::alloc(this->arena_, ast::Stmt{
        ast::Block_stmt {
            .statements = statements,
            .loc = {line, column}
        }
    });
}

Result<ast::Stmt*> Parser::if_statement()
{
    auto condition_result = __Try(expression());
    auto then_branch_result = __Try(statement());

    ast::Stmt* else_branch = nullptr;
    if (match({lex::TokenType::Else}))
        else_branch = __TryOr(statement(), nullptr);

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::If_stmt {
            .condition = condition_result,
            .then_branch = then_branch_result,
            .else_branch = else_branch,
            .loc = {previous().line, previous().column}
        }
    });
}

Result<ast::Stmt*> Parser::while_statement()
{
    auto condition_result = __Try(expression());
    auto body_result = __Try(statement());

    return mem::Arena::alloc(this->arena_, ast::Stmt {
        ast::While_stmt {
            .condition = condition_result,
            .body = body_result,
            .loc = {previous().line, previous().column}
        }
    });
}

Result<ast::Stmt*> Parser::for_statement()
{
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after 'for'"));
    ast::Stmt* initializer = nullptr;
    ast::Expr* condition = nullptr;
    ast::Expr* increment = nullptr;

    if (match({lex::TokenType::Semicolon})) {}
    else if (match({lex::TokenType::Let}))
        initializer = __TryOr(var_declaration(), nullptr);
    else
        initializer = __TryOr(expression_statement(), nullptr);

    if (!check(lex::TokenType::Semicolon))
        condition = __TryOr(expression(), nullptr);

    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after loop condition"));

    if (!check(lex::TokenType::RightParen))
        increment = __TryOr(expression(), nullptr);

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after for clauses"));

    auto body = __Try(statement());

    return mem::Arena::alloc(this->arena_, ast::Stmt{
        ast::For_stmt{ initializer, condition, increment, body, {previous().line, previous().column} }
    });
}

Result<ast::Stmt*> Parser::return_statement()
{
    size_t line = previous().line;
    size_t column = previous().column;
    ast::Expr* value = nullptr;

    if (!check(lex::TokenType::Semicolon))
        value = __TryOr(expression(), nullptr);
    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after return value"));

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
    auto expr = __Try(logical_or());

    if (match({lex::TokenType::Assign}))
    {
        lex::Token equals = previous();
        auto value_result = __Try(assignment());

        if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr->node))
        {
            return mem::Arena::alloc(this->arena_, ast::Expr{
                ast::Assignment_expr{var_expr->name, value_result, var_expr->type, {equals.line, equals.column}}});
        }
        else if (auto *field_access_expr = std::get_if<ast::Field_access_expr>(&expr->node))
        {
            return mem::Arena::alloc(this->arena_, ast::Expr {
                ast::Field_assignment_expr{
                    .object = field_access_expr->object,
                    .field_name = field_access_expr->field_name,
                    .value = value_result,
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
                    .value = value_result,
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
    auto expr = __Try(logical_and());

    while (match({lex::TokenType::LogicalOr}))
    {
        lex::Token op = previous();
        auto right_result = __Try(logical_and());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right_result,
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
    auto expr = __Try(equality());

    while (match({lex::TokenType::LogicalAnd}))
    {
        lex::Token op = previous();
        auto right_result = __Try(equality());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr {
                .left = expr,
                .op = op.type,
                .right = right_result,
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
    auto expr = __Try(comparison());

    while (match({lex::TokenType::Equal, lex::TokenType::NotEqual}))
    {
        lex::Token op = previous();
        auto right = __Try(comparison());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
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
    auto expr = __Try(term());

    while (match({lex::TokenType::Less, lex::TokenType::LessEqual, lex::TokenType::Greater, lex::TokenType::GreaterEqual}))
    {
        lex::Token op = previous();
        auto right = __Try(term());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
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
    auto expr = __Try(factor());

    while (match({lex::TokenType::Plus, lex::TokenType::Minus}))
    {
        lex::Token op = previous();
        auto right = __Try(factor());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr {
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void), // Resolved by type checker
                .loc = {op.line, op.column}
            }
        });
        expr = binary_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::factor()
{
    auto expr = __Try(cast());

    while (match({lex::TokenType::Star, lex::TokenType::Slash, lex::TokenType::Percent}))
    {
        lex::Token op = previous();
        auto right = __Try(cast());

        auto binary_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Binary_expr {
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void), // Resolved by type checker
                .loc = {op.line, op.column}
            }
        });
        expr = binary_expr;
    }
    return expr;
}

Result<ast::Expr*> Parser::cast()
{
    auto expr = __Try(unary());

    while (match({lex::TokenType::As}))
    {
        auto target_type = __Try(parse_type());

        auto loc = ast::get_loc(expr->node);
        auto cast_expr = mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Cast_expr {
                .expression = expr,
                .target_type = target_type,
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
        auto right = __Try(cast());

        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Unary_expr{
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void), // Resolved by type checker
                .loc = {op.line, op.column}
            }
        });
    }
    return call();
}

Result<ast::Expr*> Parser::call()
{
    auto expr = __Try(primary());

    while (true)
    {
        if (match({lex::TokenType::LeftParen}))
        {
            std::vector<ast::Expr*> arguments;
            if (!check(lex::TokenType::RightParen))
            {
                do {
                    arguments.push_back(__Try(expression()));
                } while (match({lex::TokenType::Comma}));
            }
            auto paren = __Try(consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

            auto* call_node = mem::Arena::alloc(arena_, ast::Call_expr{
                expr,
                arguments,
                types::Type(types::Primitive_kind::Void), // Type checker resolves
                ast::Source_location{paren.line, paren.column}
            });

            expr = mem::Arena::alloc(arena_, ast::Expr{*call_node});
        }
        else if (match({lex::TokenType::LeftBracket}))
        {
            auto index_result = __Try(expression());
            auto right_bracket_result = __Try(consume(lex::TokenType::RightBracket, "Expect ']' after array index"));

            auto* array_access_node = mem::Arena::alloc(arena_, ast::Array_access_expr{
                expr,
                index_result,
                types::Type(types::Primitive_kind::Void),
                ast::Source_location{right_bracket_result.line, right_bracket_result.column}
            });
            expr = mem::Arena::alloc(arena_, ast::Expr{*array_access_node});
        }
        else if (match({lex::TokenType::Dot}))
        {
            auto name_result = __Try(consume(lex::TokenType::Identifier, "Expect member name after '.'"));

            // Re-introduce Method_call_expr parsing
            if (match({lex::TokenType::LeftParen})) 
            {
                std::vector<ast::Expr*> arguments;
                if (!check(lex::TokenType::RightParen)) {
                    do {
                        arguments.push_back(__Try(expression()));
                    } while (match({lex::TokenType::Comma}));
                }
                __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

                auto* method_call_node = mem::Arena::alloc(arena_, ast::Method_call_expr{
                    expr, name_result.lexeme, arguments,
                    types::Type(types::Primitive_kind::Void),
                    ast::Source_location{name_result.line, name_result.column}
                });
                expr = mem::Arena::alloc(arena_, ast::Expr{*method_call_node});
            }
            else // It's a field access
            {
                auto* field_access_node = mem::Arena::alloc(arena_, ast::Field_access_expr{
                    expr, name_result.lexeme, 
                    types::Type(types::Primitive_kind::Void),
                    ast::Source_location{name_result.line, name_result.column}
                });
                expr = mem::Arena::alloc(arena_, ast::Expr{*field_access_node});
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

        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Variable_expr {
                .name = "this",
                .type = types::Type(types::Primitive_kind::Void), // Resolved later
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

    if (match({lex::TokenType::Identifier}))
    {
        lex::Token id = previous();

        // FIX: Use ColonColon for scope resolution
        if (match({lex::TokenType::ColonColon}))
        {
            auto member = __Try(consume(lex::TokenType::Identifier, "Expect member name after '::'."));
            auto* base_var_expr = mem::Arena::alloc(arena_, ast::Expr{
                ast::Variable_expr{id.lexeme, types::Type(types::Primitive_kind::Void), {id.line, id.column}}
            });

            auto* static_path = mem::Arena::alloc(arena_, ast::Static_path_expr{
                .base = base_var_expr,
                .member = member,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {member.line, member.column}
            });
            return mem::Arena::alloc(arena_, ast::Expr{*static_path});
        }

        if (check(lex::TokenType::LeftBrace))
            return parse_model_literal(id.lexeme);

        return mem::Arena::alloc(this->arena_, ast::Expr {
            ast::Variable_expr {
                .name = id.lexeme,
                .type = types::Type(types::Primitive_kind::Void), // Type checker resolves
                .loc = {id.line, id.column}
            }
        });
    }

    if (match({lex::TokenType::LeftParen}))
    {
        auto expr_result = __Try(expression());
        __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after expression"));
        return expr_result;
    }

    return std::unexpected(create_error(peek(), "Expect expression. Found: " + peek().lexeme));
}

Result<ast::Expr*> Parser::parse_closure_expression()
{
    size_t line = peek().line;
    size_t column = peek().column;

    std::vector<ast::Function_param> parameters;

    if (match({lex::TokenType::LogicalOr})) {}
    else if (match({lex::TokenType::Pipe}))
    {
        if (!check(lex::TokenType::Pipe))
        {
            do {
                bool is_const = match({lex::TokenType::Const});
                auto param_name = __Try(consume(lex::TokenType::Identifier, "Expect parameter name"));
                __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after parameter name"));
                auto param_type = __Try(parse_type());
                parameters.emplace_back(ast::Function_param{param_name.lexeme, param_type, is_const});
            } while (match({lex::TokenType::Comma}));
        }
        __TryIgnore(consume(lex::TokenType::Pipe, "Expect '|' after closure parameters"));
    }

    types::Type return_type = types::Type(types::Primitive_kind::Void);
    if (match({lex::TokenType::Arrow}))
    {
        return_type = __Try(parse_type());
    }

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before closure body."));
    auto body = __Try(block_statement());

    auto closure_type = mem::make_rc<types::Closure_type>();
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
            auto element = __Try(expression());
            elements.push_back(element);
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightBracket, "Expect ']' after array elements"));

    return mem::Arena::alloc(this->arena_, ast::Expr {
        ast::Array_literal_expr {
            .elements = elements,
            .type = types::Type(types::Primitive_kind::Void), // Type checker resolves
            .loc = {line, column}
        }
    });

}

Result<ast::Expr*> Parser::parse_model_literal(const std::string& model_name)
{
    auto brace = __Try(consume(lex::TokenType::LeftBrace, "Expect '{' for model literal."));

    std::vector<std::pair<std::string, ast::Expr*>> fields;

    if (!check(lex::TokenType::RightBrace))
    {
        do
        {
            __TryIgnore(consume(lex::TokenType::Dot, "Expect '.' before field name."));
            auto field_name = __Try(consume(lex::TokenType::Identifier, "Expect field name."));
            __TryIgnore(consume(lex::TokenType::Assign, "Expect '=' after field name."));
            auto value = __Try(expression());
            fields.push_back({field_name.lexeme, value});
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after model fields."));

    return mem::Arena::alloc(this->arena_, ast::Expr{
        ast::Model_literal_expr {
            .model_name = model_name,
            .fields = fields,
            .type = types::Type(types::Primitive_kind::Void), // Type checker resolves
            .loc = {brace.line, brace.column}
        }
    });
}


Result<types::Type> Parser::parse_type()
{
    types::Type current_type;

    if (match({lex::TokenType::LeftParen}))
    {
        current_type = __Try(parse_type());
        __TryIgnore(consume(lex::TokenType::RightParen, "Expected ')' after type in parentheses."));
    }
    else if (peek().type == lex::TokenType::Pipe || peek().type == lex::TokenType::LogicalOr)
    {
        std::vector<types::Type> parameter_types;

        if (match({lex::TokenType::LogicalOr})) {}
        else if (match({lex::TokenType::Pipe}))
        {
            if (!check(lex::TokenType::Pipe))
            {
                do {
                    auto param_type_result = __Try(parse_type());
                    parameter_types.push_back(param_type_result);
                } while (match({lex::TokenType::Comma}));
            }
            __TryIgnore(consume(lex::TokenType::Pipe, "Expect '|' after closure parameter types."));
        }

        __TryIgnore(consume(lex::TokenType::Arrow, "Expect '->' after closure parameters"));
        auto return_type_result = __Try(parse_type());

        auto closure_t = mem::make_rc<types::Closure_type>();
        closure_t->function_type.parameter_types = parameter_types;
        closure_t->function_type.return_type = return_type_result;
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
        else if (m_known_union_names.count(type_name))
        {
            auto union_type = mem::make_rc<types::Union_type>();
            union_type->name = type_name;
            current_type = types::Type(union_type);
        }
        else // Defaults to model, including known models and forward-declarations
        {
            auto model_type = mem::make_rc<types::Model_type>();
            model_type->name = type_name;
            current_type = types::Type(model_type);
        }
    }
    else
    {
        return std::unexpected(create_error(peek(), "Expected a type name or '(' for a grouped type."));
    }

    while (true)
    {
        if (match({lex::TokenType::LeftBracket}))
        {
            __TryIgnore(consume(lex::TokenType::RightBracket, "Expected ']' to complete array type suffix."));
            current_type = types::Type(mem::make_rc<types::Array_type>(current_type));
        }
        else if (match({lex::TokenType::Question}))
        {
            auto optional_type = mem::make_rc<types::Optional_type>();
            optional_type->base_type = current_type;
            current_type = types::Type(optional_type);
        }
        else
        {
            break;
        }
    }

    return current_type;
}
}; // namespace phos
