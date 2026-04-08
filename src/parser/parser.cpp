#include "parser.hpp"

#include "../error/err.hpp"
#include "../error/result.hpp"
#include "../lexer/lexer.hpp"
#include "../lexer/token.hpp"
#include "../utility/try_res.hpp"
#include "../value/type.hpp"

#include <cstddef>
#include <expected>
#include <print>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace phos {

// --- utility -----------------------------------------------------------------

void Parser::skip_newlines()
{
    while (!is_at_end() && match({lex::TokenType::Newline})) {}
}

const lex::Token &Parser::peek() const
{
    if (current_ >= tokens_.size()) {
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
    for (auto type : types) {
        if (check(type)) {
            advance();
            return true;
        }
    }
    return false;
}

bool Parser::check_next(lex::TokenType type) const
{
    if (current_ + 1 >= tokens_.size())
        return false;
    return tokens_[current_ + 1].type == type;
}

// consume -> advance if current token matches type, else return error
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

// Skip tokens until we reach a point where we can resume parsing cleanly.
void Parser::synchronize()
{
    advance();
    while (!is_at_end()) {
        if (previous().type == lex::TokenType::Semicolon)
            return;
        switch (peek().type) {
        case lex::TokenType::Fn:
        case lex::TokenType::Let:
        case lex::TokenType::For:
        case lex::TokenType::If:
        case lex::TokenType::While:
        case lex::TokenType::Match:
        case lex::TokenType::Print:
        case lex::TokenType::Return:
        case lex::TokenType::Model:
        case lex::TokenType::Bind:
        case lex::TokenType::Union:
        case lex::TokenType::Enum:
            return;
        default:
            advance();
        }
    }
}

// --- top-level ---------------------------------------------------------------

// program -> declaration* EOF
Result<std::vector<ast::Stmt *>> Parser::parse()
{
    std::vector<ast::Stmt *> statements;
    bool had_error = false; // Add an error flag!

    while (!is_at_end()) {
        skip_newlines();
        if (is_at_end())
            break;

        auto decl_result = declaration();
        if (!decl_result) {
            if (!decl_result.error().message.empty())
                std::println(stderr, "{}", decl_result.error().format());

            had_error = true;
            synchronize();
            continue;
        }

        if (decl_result.value())
            statements.push_back(*decl_result.value());
    }
    if (had_error)
        return std::unexpected(err::msg("Compilation halted due to syntax errors.", "parser", 0, 0));

    return statements;
}

// --- declarations ------------------------------------------------------------

Result<ast::Function_param> Parser::parse_function_parameter(bool allow_default_value)
{
    bool is_mut = match({lex::TokenType::Mut});
    auto param_name = __Try(consume(lex::TokenType::Identifier, "Expect parameter name"));
    __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after parameter name"));
    auto param_type = __Try(parse_type());

    ast::Expr *default_value = nullptr;
    if (allow_default_value && match({lex::TokenType::Assign}))
        default_value = __Try(expression());

    return ast::Function_param{
        .name = param_name.lexeme,
        .type = param_type,
        .is_mut = is_mut,
        .default_value = default_value,
        .loc = {param_name.line, param_name.column},
    };
}

Result<std::vector<ast::Call_argument>> Parser::parse_call_arguments()
{
    std::vector<ast::Call_argument> arguments;
    if (check(lex::TokenType::RightParen))
        return arguments;

    do {
        if (check(lex::TokenType::RightParen))
            break;
        if (check(lex::TokenType::Identifier) && check_next(lex::TokenType::Assign)) {
            auto name = advance();
            __TryIgnore(consume(lex::TokenType::Assign, "Expect '=' after argument name"));
            auto *value = __Try(expression());
            arguments.push_back(
                ast::Call_argument{
                    .name = name.lexeme,
                    .value = value,
                    .loc = {name.line, name.column},
                });
        } else {
            auto *value = __Try(expression());
            arguments.push_back(
                ast::Call_argument{
                    .name = "",
                    .value = value,
                    .loc = ast::get_loc(value->node),
                });
        }
    } while (match({lex::TokenType::Comma}));

    return arguments;
}

// declaration -> fn_decl | model_decl | union_decl | enum_decl | var_decl | statement
Result<std::optional<ast::Stmt *>> Parser::declaration()
{
    skip_newlines();
    if (is_at_end())
        return std::optional<ast::Stmt *>{std::nullopt};

    if (match({lex::TokenType::Fn}))
        return std::optional<ast::Stmt *>{__Try(function_declaration())};
    if (match({lex::TokenType::Model}))
        return std::optional<ast::Stmt *>{__Try(model_declaration())};
    if (match({lex::TokenType::Bind})) {
        __Try(parse_bind_statement());
        return std::optional<ast::Stmt *>{std::nullopt};
    }
    if (match({lex::TokenType::Union}))
        return std::optional<ast::Stmt *>{__Try(union_declaration())};
    if (match({lex::TokenType::Enum}))
        return std::optional<ast::Stmt *>{__Try(enum_declaration())};
    if (match({lex::TokenType::Let}))
        return std::optional<ast::Stmt *>{__Try(var_declaration())};

    return std::optional<ast::Stmt *>{__Try(statement())};
}

// fn_decl -> "fn" IDENT "(" param* ")" ("->" type)? block
// param   -> "mut"? IDENT ":" type
Result<ast::Stmt *> Parser::function_declaration()
{
    lex::Token name = __Try(consume(lex::TokenType::Identifier, "Expect function name"));
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after function name"));

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen)) {
        do {
            if (check(lex::TokenType::RightParen))
                break;
            parameters.push_back(__Try(parse_function_parameter(true)));
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after parameters"));

    types::Type return_type = types::Type(types::Primitive_kind::Void);
    if (match({lex::TokenType::Arrow}))
        return_type = __Try(parse_type());

    function_types_.emplace(name.lexeme, return_type);
    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before function body"));
    auto body = __Try(block_statement());

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::Function_stmt{
            .name = name.lexeme,
            .is_static = false,
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
            .loc = {name.line, name.column},
        }});
}

// model_decl -> "model" IDENT "{" field_decl* "}"
Result<ast::Stmt *> Parser::model_declaration()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected model name"));
    m_known_model_names.insert(name.lexeme);
    current_model_ = name.lexeme;

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after model name"));

    std::vector<ast::Typed_member_decl> fields;
    std::vector<ast::Function_stmt *> methods;

    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace))
            break;

        fields.push_back(__Try(parse_model_field()));
        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '};' after model body"));
    current_model_.clear();

    auto stmt = mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::Model_stmt{
            .name = name.lexeme,
            .fields = fields,
            .methods = methods,
            .loc = {name.line, name.column},
        }});

    // Save a direct pointer to the Model AST node so 'bind' can find it later!
    parsed_models[name.lexeme] = std::get_if<ast::Model_stmt>(&stmt->node);

    return stmt;
}

// bind_stmt -> "bind" IDENT "{" method_decl* "}"
Result<ast::Stmt *> Parser::parse_bind_statement()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected model name to bind to"));

    if (!parsed_models.count(name.lexeme)) {
        return std::unexpected(
            create_error(name, "Cannot bind methods to undeclared model '" + name.lexeme + "'. Please declare the model first."));
    }

    auto target_model = parsed_models[name.lexeme];
    current_model_ = name.lexeme;

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before bind body"));

    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace))
            break;

        auto method_ast = this->arena_.create<ast::Function_stmt>(__Try(parse_model_method()));
        target_model->methods.push_back(method_ast);

        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after bind body"));
    current_model_.clear();

    return nullptr;
}

// union_decl -> "union" IDENT "{" (IDENT ":" type ";")* "}"
Result<ast::Stmt *> Parser::union_declaration()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected union name"));
    m_known_union_names.insert(name.lexeme);

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after union name"));

    std::vector<ast::Typed_member_decl> variants;
    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace))
            break;

        auto variant_name = __Try(consume(lex::TokenType::Identifier, "Expect variant name"));
        __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after variant name for type"));

        types::Type variant_type = types::Type(types::Primitive_kind::Void);
        variant_type = __Try(parse_type());
        ast::Expr *default_value = nullptr;
        if (match({lex::TokenType::Assign}))
            default_value = __Try(expression());

        variants.push_back(
            ast::Typed_member_decl{
                .name = variant_name.lexeme,
                .type = variant_type,
                .default_value = default_value,
                .loc = {variant_name.line, variant_name.column},
            });

        if (!check(lex::TokenType::RightBrace))
            __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after variant declaration"));

        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after union body"));

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::Union_stmt{
            .name = name.lexeme,
            .variants = variants,
            .loc = {name.line, name.column},
        }});
}

// enum_decl -> "enum" IDENT (":" type)? "{" (IDENT ("=" INT | STRING)? ","?)* "}"
Result<ast::Stmt *> Parser::enum_declaration()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected enum name"));
    m_known_enum_names.insert(name.lexeme);

    types::Type base_type = types::Primitive_kind::I64;
    if (match({lex::TokenType::Colon})) {
        base_type = __Try(parse_type());
        if (!(types::is_primitive(base_type)
              && (types::is_integer_primitive(types::get_primitive_kind(base_type))
                  || types::get_primitive_kind(base_type) == types::Primitive_kind::String))) {
            return std::unexpected(create_error(previous(), "Enum base type must be an integer type or 'string'."));
        }
    }

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after enum name"));

    std::vector<std::pair<std::string, std::optional<Value>>> variants;
    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace))
            break;

        auto variant_name = __Try(consume(lex::TokenType::Identifier, "Expect enum variant name"));

        std::optional<Value> value = std::nullopt;
        if (match({lex::TokenType::Assign})) {
            if (types::is_primitive(base_type) && types::is_integer_primitive(types::get_primitive_kind(base_type))) {
                auto val_tok = __Try(consume(lex::TokenType::Integer32, "Expect integer value after '=' in integer enum"));
                auto coerced = coerce_numeric_literal(val_tok.literal, types::get_primitive_kind(base_type));
                if (!coerced)
                    return std::unexpected(create_error(val_tok, "Enum variant value does not fit the enum base type."));
                value = coerced.value();
            } else {
                auto val_tok = __Try(consume(lex::TokenType::String, "Expect string value after '=' in string enum"));
                value = Value(std::get<std::string>(val_tok.literal.payload));
            }
        }

        variants.push_back({variant_name.lexeme, value});

        match({lex::TokenType::Comma}); // Optional trailing comma
        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after enum body"));

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::Enum_stmt{
            .name = name.lexeme,
            .base_type = base_type,
            .variants = variants,
            .loc = {name.line, name.column},
        }});
}

// field_decl -> IDENT ":" type ";"
Result<ast::Typed_member_decl> Parser::parse_model_field()
{
    auto name_result = __Try(consume(lex::TokenType::Identifier, "Expect field name"));
    __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after field name"));
    auto type_result = __Try(parse_type());
    ast::Expr *default_value = nullptr;
    if (match({lex::TokenType::Assign}))
        default_value = __Try(expression());
    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after field declaration"));
    return ast::Typed_member_decl{
        .name = name_result.lexeme,
        .type = type_result,
        .default_value = default_value,
        .loc = {name_result.line, name_result.column},
    };
}

// method_decl -> "static"? "fn" IDENT "(" param* ")" ("->" type)? block
Result<ast::Function_stmt> Parser::parse_model_method()
{
    bool is_static = match({lex::TokenType::Static});

    // We consume 'fn' here now so the parser correctly expects it inside the bind block
    __TryIgnore(consume(lex::TokenType::Fn, is_static ? "Expect 'fn' after 'static'" : "Expect 'fn' keyword for method"));

    auto name = __Try(consume(lex::TokenType::Identifier, "Expect method name"));
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after method name"));

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen)) {
        do {
            if (check(lex::TokenType::RightParen))
                break;
            parameters.push_back(__Try(parse_function_parameter(true)));
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after parameters"));

    types::Type return_type = types::Type(types::Primitive_kind::Void);
    if (match({lex::TokenType::Arrow}))
        return_type = __Try(parse_type());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before method body"));
    auto body_result = __Try(block_statement());

    return ast::Function_stmt{
        .name = name.lexeme,
        .is_static = is_static,
        .parameters = parameters,
        .return_type = return_type,
        .body = body_result,
        .loc = {name.line, name.column},
    };
}

// var_decl -> "let" ("mut" | "const")? IDENT (":=" expr | ":" type ("=" expr)?) ";"
Result<ast::Stmt *> Parser::var_declaration()
{
    bool is_mut = match({lex::TokenType::Mut});
    bool is_const = !is_mut && match({lex::TokenType::Const});

    auto name = __Try(consume(lex::TokenType::Identifier, "Expect variable name"));

    types::Type var_type = types::Type(types::Primitive_kind::Void);
    ast::Expr *initializer = nullptr;
    bool type_inferred = false;

    if (match({lex::TokenType::Colon})) {
        if (match({lex::TokenType::Assign})) {
            // let x := expr  -- inferred type
            initializer = __Try(expression());
            type_inferred = true;
        } else {
            // let x: T = expr  -- explicit type, optional initializer
            var_type = __Try(parse_type());
            if (match({lex::TokenType::Assign}))
                initializer = __Try(expression());
        }
    } else {
        return std::unexpected(create_error(peek(), "Expect ':' or ':=' after variable name."));
    }

    __TryIgnore(consume(lex::TokenType::Semicolon, "Expected ';' after variable declaration"));

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::Var_stmt{
            .is_mut = is_mut,
            .is_const = is_const,
            .name = name.lexeme,
            .type = var_type,
            .initializer = initializer,
            .type_inferred = type_inferred,
            .loc = {name.line, name.column},
        }});
}

// --- statements --------------------------------------------------------------

// statement -> print_stmt | block | if_stmt | while_stmt | for_stmt
//            | match_stmt | return_stmt | expr_stmt
Result<ast::Stmt *> Parser::statement()
{
    skip_newlines();
    if (is_at_end())
        return std::unexpected(create_error(peek(), "Unexpected end of file"));

    if (match({lex::TokenType::Print}))
        return print_statement(ast::Print_stream::STDOUT);
    if (match({lex::TokenType::PrintErr}))
        return print_statement(ast::Print_stream::STDERR);
    if (match({lex::TokenType::LeftBrace}))
        return block_statement();
    if (match({lex::TokenType::If}))
        return if_statement();
    if (match({lex::TokenType::While}))
        return while_statement();
    if (match({lex::TokenType::Match}))
        return match_statement();
    if (match({lex::TokenType::Return}))
        return return_statement();

    if (match({lex::TokenType::For})) {
        // "for" "(" ...  ->  C-style for loop
        // "for" IDENT "in" ...  ->  for-in loop
        if (check(lex::TokenType::LeftParen))
            return for_statement();
        return for_in_statement();
    }

    return expression_statement();
}

// print_stmt -> ("print" | "eprint") "(" (expr ("," expr)*)? ("," "sep" "=" expr)? ("," "end" "=" expr)? ")" ";"
Result<ast::Stmt *> Parser::print_statement(ast::Print_stream stream)
{
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after 'print'."));

    std::vector<ast::Expr *> expressions;
    std::string sep = "";
    std::string end = "\n";

    // Empty print() -> just print the end character (newline by default)
    if (check(lex::TokenType::RightParen)) {
        advance();
        __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after print statement."));
        return mem::Arena::alloc(
            this->arena_,
            ast::Stmt{ast::Print_stmt{
                .stream = stream,
                .expressions = {},
                .sep = "",
                .end = "\n",
                .loc = {previous().line, previous().column},
            }});
    }

    bool seen_named = false;

    do {
        // Allow trailing comma: "print(a, b,)"
        if (check(lex::TokenType::RightParen))
            break;

        // Peek for named args: sep= or end=
        if (check(lex::TokenType::Identifier) && check_next(lex::TokenType::Assign)) {
            std::string name = peek().lexeme;
            if (name == "sep" || name == "end") {
                seen_named = true;
                advance(); // consume identifier
                advance(); // consume '='

                auto val_result = __Try(expression());

                auto *lit = std::get_if<ast::Literal_expr>(&val_result->node);
                if (!lit || !std::holds_alternative<std::string>(lit->value.payload))
                    return std::unexpected(create_error(previous(), "'sep' and 'end' must be string literals."));

                if (name == "sep")
                    sep = std::get<std::string>(lit->value.payload);
                else
                    end = std::get<std::string>(lit->value.payload);
            } else {
                if (seen_named)
                    return std::unexpected(create_error(peek(), "Positional arguments cannot appear after named arguments in print()."));

                auto expr_result = __Try(expression());
                expressions.push_back(expr_result);
            }
        } else {
            if (seen_named)
                return std::unexpected(create_error(peek(), "Positional arguments cannot appear after named arguments in print()."));

            auto expr_result = __Try(expression());
            expressions.push_back(expr_result);
        }
    } while (match({lex::TokenType::Comma}));

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after print arguments."));
    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after print statement."));

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::Print_stmt{
            .stream = stream,
            .expressions = std::move(expressions),
            .sep = sep,
            .end = end,
            .loc = {previous().line, previous().column},
        }});
}

// block -> "{" declaration* "}"
Result<ast::Stmt *> Parser::block_statement()
{
    std::vector<ast::Stmt *> statements;
    size_t line = previous().line;
    size_t column = previous().column;

    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        auto stmt_result = declaration();
        if (!stmt_result)
            return std::unexpected(stmt_result.error());
        if (stmt_result.value())
            statements.push_back(*stmt_result.value());
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after block"));

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::Block_stmt{
            .statements = statements,
            .loc = {line, column},
        }});
}

// if_stmt -> "if" expr "{" declaration* "}" ("else" (if_stmt | "{" declaration* "}"))?
Result<ast::Stmt *> Parser::if_statement()
{
    auto condition_result = __Try(expression());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after if condition."));
    auto then_branch_result = __Try(block_statement());

    ast::Stmt *else_branch = nullptr;
    if (match({lex::TokenType::Else})) {
        if (match({lex::TokenType::If})) {
            else_branch = __TryOr(if_statement(), nullptr);
        } else {
            __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after 'else'."));
            else_branch = __TryOr(block_statement(), nullptr);
        }
    }

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::If_stmt{
            .condition = condition_result,
            .then_branch = then_branch_result,
            .else_branch = else_branch,
            .loc = {previous().line, previous().column},
        }});
}

// while_stmt -> "while" expr "{" declaration* "}"
Result<ast::Stmt *> Parser::while_statement()
{
    auto condition_result = __Try(expression());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after while condition."));
    auto body_result = __Try(block_statement());

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::While_stmt{
            .condition = condition_result,
            .body = body_result,
            .loc = {previous().line, previous().column},
        }});
}

Result<ast::Stmt *> Parser::for_statement()
{
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after 'for'"));

    ast::Stmt *initializer = nullptr;
    ast::Expr *condition = nullptr;
    ast::Expr *increment = nullptr;

    if (match({lex::TokenType::Semicolon})) {
    } else if (match({lex::TokenType::Let}))
        initializer = __TryOr(var_declaration(), nullptr);
    else
        initializer = __TryOr(expression_statement(), nullptr);

    if (!check(lex::TokenType::Semicolon))
        condition = __TryOr(expression(), nullptr);
    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after loop condition"));

    if (!check(lex::TokenType::RightParen))
        increment = __TryOr(expression(), nullptr);
    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after for clauses"));

    // FORCE BLOCK HERE
    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after for clauses"));
    auto body = __Try(block_statement());

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::For_stmt{initializer, condition, increment, body, {previous().line, previous().column}}});
}

// for_in_stmt
Result<ast::Stmt *> Parser::for_in_statement()
{
    auto var_name = __Try(consume(lex::TokenType::Identifier, "Expect loop variable name after 'for'"));
    __TryIgnore(consume(lex::TokenType::In, "Expect 'in' after loop variable"));

    auto iterable = __Try(expression());

    // FORCE BLOCK HERE
    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after for-in iterable."));
    auto body = __Try(block_statement());

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::For_in_stmt{
            .var_name = var_name.lexeme,
            .iterable = iterable,
            .body = body,
            .loc = {var_name.line, var_name.column},
        }});
}

// match_stmt -> "match" expr "{" match_arm* "}"
// match_arm  -> (expr ("(" IDENT ")")? | "_") "=>" statement ","?
Result<ast::Stmt *> Parser::match_statement()
{
    ast::Source_location loc{peek().line, peek().column};
    auto subject = __Try(expression());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after match subject"));

    std::vector<ast::Match_arm> arms;
    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace))
            break;

        ast::Match_arm arm;

        if (match({lex::TokenType::Underscore})) {
            arm.is_wildcard = true;
        } else {
            // Parse the pattern as a standard expression
            arm.pattern = __Try(expression());

            // If the user typed `Result::Ok(data)`, the expression parser
            // naturally consumed it as a Call_expr. We unpack that into the
            // actual pattern + the binding name!
            if (auto *call_node = std::get_if<ast::Call_expr>(&arm.pattern->node)) {
                // 1. The actual pattern is just the callee (e.g., Result::Ok)
                arm.pattern = call_node->callee;

                // 2. Validate and extract the payload binding name
                if (call_node->arguments.size() != 1)
                    return std::unexpected(err::msg("Match union payloads can only bind exactly one variable.", "parser", loc.l, loc.c));

                if (auto *var_node = std::get_if<ast::Variable_expr>(&call_node->arguments[0].value->node))
                    arm.bind_name = var_node->name;
                else
                    return std::unexpected(err::msg("Match binding payload must be a simple identifier.", "parser", loc.l, loc.c));
            }
        }

        // 5. Fat Arrow
        __TryIgnore(consume(lex::TokenType::FatArrow, "Expect '=>' after match pattern"));

        arm.body = __Try(statement());
        arms.push_back(std::move(arm));

        match({lex::TokenType::Comma});
        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after match arms"));

    return mem::Arena::alloc(
        this->arena_,
        ast::Stmt{ast::Match_stmt{
            .subject = subject,
            .arms = std::move(arms),
            .loc = loc,
        }});
}

// return_stmt -> "return" expr? ";"
Result<ast::Stmt *> Parser::return_statement()
{
    size_t line = previous().line;
    size_t column = previous().column;

    ast::Expr *value = nullptr;
    if (!check(lex::TokenType::Semicolon))
        value = __TryOr(expression(), nullptr);

    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after return value"));

    return mem::Arena::alloc(this->arena_, ast::Stmt{ast::Return_stmt{value, {line, column}}});
}

// expr_stmt -> expr ";"
Result<ast::Stmt *> Parser::expression_statement()
{
    auto expr_result = expression();
    if (!expr_result)
        return std::unexpected(expr_result.error());

    auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after expression");
    if (!semicolon_result)
        return std::unexpected(semicolon_result.error());

    return mem::Arena::alloc(this->arena_, ast::Stmt{ast::Expr_stmt{expr_result.value(), {previous().line, previous().column}}});
}

// --- expressions -------------------------------------------------------------

// expression -> assignment
Result<ast::Expr *> Parser::expression()
{
    return assignment();
}

// assignment -> range_expr ("=" assignment)?
// lvalue rewrites: Variable -> Assignment_expr
//                  Field_access -> Field_assignment_expr
//                  Array_access -> Array_assignment_expr
Result<ast::Expr *> Parser::assignment()
{
    auto expr = __Try(range_expr());

    if (match({lex::TokenType::Assign})) {
        lex::Token equals = previous();
        auto value_result = __Try(assignment());

        if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr->node)) {
            return mem::Arena::alloc(
                this->arena_,
                ast::Expr{ast::Assignment_expr{var_expr->name, value_result, var_expr->type, {equals.line, equals.column}}});
        } else if (auto *field_access_expr = std::get_if<ast::Field_access_expr>(&expr->node)) {
            return mem::Arena::alloc(
                this->arena_,
                ast::Expr{ast::Field_assignment_expr{
                    .object = field_access_expr->object,
                    .field_name = field_access_expr->field_name,
                    .value = value_result,
                    .type = types::Type(types::Primitive_kind::Void),
                    .loc = {equals.line, equals.column},
                }});
        } else if (auto *array_access_expr = std::get_if<ast::Array_access_expr>(&expr->node)) {
            return mem::Arena::alloc(
                this->arena_,
                ast::Expr{ast::Array_assignment_expr{
                    .array = array_access_expr->array,
                    .index = array_access_expr->index,
                    .value = value_result,
                    .type = types::Type(types::Primitive_kind::Void),
                    .loc = {equals.line, equals.column},
                }});
        }

        return std::unexpected(create_error(equals, "Invalid assignment target"));
    }
    return expr;
}

// range_expr -> logical_or (".." logical_or)?
Result<ast::Expr *> Parser::range_expr()
{
    auto expr = __Try(logical_or());

    if (match({lex::TokenType::DotDot, lex::TokenType::DotDotEq})) {
        bool inclusive = (previous().type == lex::TokenType::DotDotEq);
        lex::Token op = previous();
        auto end_expr = __Try(logical_or());

        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Range_expr{
                .start = expr,
                .end = end_expr,
                .inclusive = inclusive,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {op.line, op.column},
            }});
    }

    return expr;
}

// logical_or -> logical_and ("||" logical_and)*
Result<ast::Expr *> Parser::logical_or()
{
    auto expr = __Try(logical_and());

    while (match({lex::TokenType::LogicalOr})) {
        lex::Token op = previous();
        auto right = __Try(logical_and());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// logical_and -> bitwise_or ("&&" bitwise_or)*
Result<ast::Expr *> Parser::logical_and()
{
    auto expr = __Try(bitwise_or());

    while (match({lex::TokenType::LogicalAnd})) {
        lex::Token op = previous();
        auto right = __Try(bitwise_or());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// bitwise_or -> bitwise_xor ("|" bitwise_xor)*
Result<ast::Expr *> Parser::bitwise_or()
{
    auto expr = __Try(bitwise_xor());

    while (match({lex::TokenType::Pipe})) {
        lex::Token op = previous();
        auto right = __Try(bitwise_xor());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// bitwise_xor -> bitwise_and ("^" bitwise_and)*
Result<ast::Expr *> Parser::bitwise_xor()
{
    auto expr = __Try(bitwise_and());

    while (match({lex::TokenType::BitXor})) {
        lex::Token op = previous();
        auto right = __Try(bitwise_and());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// bitwise_and -> equality ("&" equality)*
Result<ast::Expr *> Parser::bitwise_and()
{
    auto expr = __Try(equality());

    while (match({lex::TokenType::BitAnd})) {
        lex::Token op = previous();
        auto right = __Try(equality());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// equality -> comparison (("==" | "!=") comparison)*
Result<ast::Expr *> Parser::equality()
{
    auto expr = __Try(comparison());

    while (match({lex::TokenType::Equal, lex::TokenType::NotEqual})) {
        lex::Token op = previous();
        auto right = __Try(comparison());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// comparison -> bitwise_shift (("<" | "<=" | ">" | ">=") bitwise_shift)*
Result<ast::Expr *> Parser::comparison()
{
    auto expr = __Try(bitwise_shift());

    while (match({lex::TokenType::Less, lex::TokenType::LessEqual, lex::TokenType::Greater, lex::TokenType::GreaterEqual})) {
        lex::Token op = previous();
        auto right = __Try(bitwise_shift());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// bitwise_shift -> term (("<<" | ">>") term)*
Result<ast::Expr *> Parser::bitwise_shift()
{
    auto expr = __Try(term());

    while (match({lex::TokenType::BitLShift, lex::TokenType::BitRshift})) {
        lex::Token op = previous();
        auto right = __Try(term());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// term -> factor (("+" | "-") factor)*
Result<ast::Expr *> Parser::term()
{
    auto expr = __Try(factor());

    while (match({lex::TokenType::Plus, lex::TokenType::Minus})) {
        lex::Token op = previous();
        auto right = __Try(factor());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// factor -> cast (("*" | "/" | "%") cast)*
Result<ast::Expr *> Parser::factor()
{
    auto expr = __Try(cast());

    while (match({lex::TokenType::Star, lex::TokenType::Slash, lex::TokenType::Percent})) {
        lex::Token op = previous();
        auto right = __Try(cast());
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// cast -> unary ("as" type)*
Result<ast::Expr *> Parser::cast()
{
    auto expr = __Try(unary());

    while (match({lex::TokenType::As})) {
        auto target_type = __Try(parse_type());
        auto loc = ast::get_loc(expr->node);
        expr = mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Cast_expr{
                .expression = expr,
                .target_type = target_type,
                .loc = loc,
            }});
    }
    return expr;
}

// unary -> ("!" | "-" | "~") cast | call
Result<ast::Expr *> Parser::unary()
{
    if (match({lex::TokenType::LogicalNot, lex::TokenType::Minus, lex::TokenType::BitNot})) {
        lex::Token op = previous();
        auto right = __Try(cast());
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Unary_expr{
                .op = op.type,
                .right = right,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {op.line, op.column},
            }});
    }
    return call();
}

// call -> primary ( "(" args? ")" | "[" expr "]" | "." IDENT ("(" args? ")")? )*
Result<ast::Expr *> Parser::call()
{
    auto expr = __Try(primary());

    while (true) {
        if (match({lex::TokenType::LeftParen})) {
            auto arguments = __Try(parse_call_arguments());
            auto paren = __Try(consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

            auto *call_node = mem::Arena::alloc(
                arena_,
                ast::Call_expr{expr, arguments, types::Type(types::Primitive_kind::Void), ast::Source_location{paren.line, paren.column}});
            expr = mem::Arena::alloc(arena_, ast::Expr{*call_node});
        } else if (match({lex::TokenType::LeftBracket})) {
            auto index_result = __Try(expression());
            auto right_bracket_result = __Try(consume(lex::TokenType::RightBracket, "Expect ']' after array index"));

            auto *array_access_node = mem::Arena::alloc(
                arena_,
                ast::Array_access_expr{
                    expr,
                    index_result,
                    types::Type(types::Primitive_kind::Void),
                    ast::Source_location{right_bracket_result.line, right_bracket_result.column}});
            expr = mem::Arena::alloc(arena_, ast::Expr{*array_access_node});
        } else if (match({lex::TokenType::Dot})) {
            auto name_result = __Try(consume(lex::TokenType::Identifier, "Expect member name after '.'"));

            if (match({lex::TokenType::LeftParen})) {
                auto arguments = __Try(parse_call_arguments());
                __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

                auto *method_call_node = mem::Arena::alloc(
                    arena_,
                    ast::Method_call_expr{
                        expr,
                        name_result.lexeme,
                        arguments,
                        types::Type(types::Primitive_kind::Void),
                        ast::Source_location{name_result.line, name_result.column}});
                expr = mem::Arena::alloc(arena_, ast::Expr{*method_call_node});
            } else {
                auto *field_access_node = mem::Arena::alloc(
                    arena_,
                    ast::Field_access_expr{
                        expr,
                        name_result.lexeme,
                        types::Type(types::Primitive_kind::Void),
                        ast::Source_location{name_result.line, name_result.column}});
                expr = mem::Arena::alloc(arena_, ast::Expr{*field_access_node});
            }
        } else {
            break;
        }
    }
    return expr;
}

// primary -> INT | FLOAT | STRING | BOOL | NIL
//          | "this" | closure | "[" array_literal "]" | FSTRING
//          | "spawn" expr | "await" expr | "yield" expr?
//          | IDENT ("::" IDENT | "{" model_literal "}")?
//          | "." IDENT
//          | "(" expr ")"
Result<ast::Expr *> Parser::primary()
{
    if (peek().type == lex::TokenType::Print)
        return std::unexpected(create_error(peek(), "print is a statement, not an expression. Did you forget a semicolon?"));

    // implicit enum member expression or anonymous model literal
    if (match({lex::TokenType::Dot})) {
        ast::Source_location loc{previous().line, previous().column};

        if (match({lex::TokenType::LeftBrace})) {
            std::vector<std::pair<std::string, ast::Expr *>> fields;

            if (!check(lex::TokenType::RightBrace)) {
                do {
                    if (check(lex::TokenType::RightBrace))
                        break; // handle trailing comma gracefully

                    std::string field_name;
                    if (check(lex::TokenType::Identifier) && check_next(lex::TokenType::Colon)) {
                        field_name = advance().lexeme;
                        advance(); // Consume ':'
                    }
                    ast::Expr *field_val = __Try(expression());

                    fields.push_back({field_name, field_val});
                } while (match({lex::TokenType::Comma}));
            }

            __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after anonymous model fields."));

            return mem::Arena::alloc(
                this->arena_,
                ast::Expr{ast::Anon_model_literal_expr{
                    .fields = std::move(fields),
                    .type = types::Type(types::Primitive_kind::Void),
                    .loc = loc,
                }});
        }

        auto member = __Try(consume(lex::TokenType::Identifier, "Expect enum variant name after '.'"));
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Enum_member_expr{
                .member_name = member.lexeme,
                .loc = loc,
                .type = types::Type(types::Primitive_kind::Void),
            }});
    }

    // concurrency

    if (match({lex::TokenType::Spawn})) {
        ast::Source_location loc{previous().line, previous().column};
        auto call_expr = __Try(expression());
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Spawn_expr{
                .call = call_expr,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = loc,
            }});
    }

    if (match({lex::TokenType::Await})) {
        ast::Source_location loc{previous().line, previous().column};
        auto thread = __Try(expression());
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Await_expr{
                .thread = thread,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = loc,
            }});
    }

    if (match({lex::TokenType::Yield})) {
        ast::Source_location loc{previous().line, previous().column};
        ast::Expr *value = nullptr;
        if (!check(lex::TokenType::Semicolon) && !check(lex::TokenType::RightBrace))
            value = __Try(expression());
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Yield_expr{
                .value = value,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = loc,
            }});
    }

    // this

    if (match({lex::TokenType::This})) {
        if (current_model_.empty())
            return std::unexpected(create_error(previous(), "Cannot use 'this' outside of a model method"));
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Variable_expr{
                .name = "this",
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {previous().line, previous().column},
            }});
    }

    // closure

    if (peek().type == lex::TokenType::Pipe || peek().type == lex::TokenType::LogicalOr)
        return parse_closure_expression();

    // array literal

    if (match({lex::TokenType::LeftBracket}))
        return parse_array_literal();

    // f-string

    if (match({lex::TokenType::Fstring}))
        return parse_fstring(previous());

    // nil
    if (match({lex::TokenType::Nil})) {
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Literal_expr{
                .value = Value(nullptr),
                .type = types::Type(types::Primitive_kind::Nil),
                .loc = {previous().line, previous().column},
            }});
    }

    // bool
    if (match({lex::TokenType::Bool})) {
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Literal_expr{
                .value = Value(std::get<bool>(previous().literal.payload)),
                .type = types::Type(types::Primitive_kind::Bool),
                .loc = {previous().line, previous().column},
            }});
    }

    // integer
    if (match({lex::TokenType::Integer32})) {
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Literal_expr{
                .value = previous().literal,
                .type = types::Type(numeric_type_of(previous().literal)),
                .loc = {previous().line, previous().column},
            }});
    }

    // float
    if (match({lex::TokenType::Float64})) {
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Literal_expr{
                .value = previous().literal,
                .type = types::Type(numeric_type_of(previous().literal)),
                .loc = {previous().line, previous().column},
            }});
    }

    // string
    if (match({lex::TokenType::String})) {
        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Literal_expr{
                .value = Value(std::get<std::string>(previous().literal.payload)),
                .type = types::Type(types::Primitive_kind::String),
                .loc = {previous().line, previous().column},
            }});
    }

    // identifier, scope resolution, model literal
    if (match({lex::TokenType::Identifier})) {
        lex::Token id = previous();

        if (match({lex::TokenType::ColonColon})) {
            // Type::Member  or  Union::Variant or Enum::Variant
            auto member = __Try(consume(lex::TokenType::Identifier, "Expect member name after '::'"));
            auto *base_var_expr = mem::Arena::alloc(
                arena_,
                ast::Expr{ast::Variable_expr{id.lexeme, types::Type(types::Primitive_kind::Void), {id.line, id.column}}});
            auto *static_path = mem::Arena::alloc(
                arena_,
                ast::Static_path_expr{
                    .base = base_var_expr,
                    .member = member,
                    .type = types::Type(types::Primitive_kind::Void),
                    .loc = {member.line, member.column},
                });
            return mem::Arena::alloc(arena_, ast::Expr{*static_path});
        }

        // Only parse as a literal if the identifier is actually a registered type!
        // (Enums are purposely excluded from model `{}` literal instantiation)
        bool is_known_type = m_known_model_names.count(id.lexeme) || m_known_union_names.count(id.lexeme);

        if (check(lex::TokenType::LeftBrace) && is_known_type)
            return parse_model_literal(id.lexeme);

        return mem::Arena::alloc(
            this->arena_,
            ast::Expr{ast::Variable_expr{
                .name = id.lexeme,
                .type = types::Type(types::Primitive_kind::Void),
                .loc = {id.line, id.column},
            }});
    }

    // grouped expression

    if (match({lex::TokenType::LeftParen})) {
        auto expr_result = __Try(expression());
        __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after expression"));
        return expr_result;
    }

    return std::unexpected(create_error(peek(), "Expect expression. Found: " + peek().lexeme));
}

// --- specific sub-parsers ----------------------------------------------------

// closure -> ("|" param* "|" | "||") ("->" type)? block
// param   -> "mut"? IDENT ":" type
Result<ast::Expr *> Parser::parse_closure_expression()
{
    size_t line = peek().line;
    size_t column = peek().column;

    std::vector<ast::Function_param> parameters;

    if (match({lex::TokenType::LogicalOr})) {
        // zero parameters
    } else if (match({lex::TokenType::Pipe})) {
        if (!check(lex::TokenType::Pipe)) {
            do {
                if (check(lex::TokenType::Pipe))
                    break;
                parameters.push_back(__Try(parse_function_parameter(false)));
            } while (match({lex::TokenType::Comma}));
        }
        __TryIgnore(consume(lex::TokenType::Pipe, "Expect '|' after closure parameters"));
    }

    types::Type return_type = types::Type(types::Primitive_kind::Void);
    if (match({lex::TokenType::Arrow}))
        return_type = __Try(parse_type());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before closure body"));
    auto body = __Try(block_statement());

    auto closure_type = mem::make_rc<types::Function_type>();
    for (const auto &param : parameters)
        closure_type->parameter_types.push_back(param.type);
    closure_type->return_type = return_type;

    return mem::Arena::alloc(
        this->arena_,
        ast::Expr{ast::Closure_expr{
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
            .type = types::Type{closure_type},
            .loc = {line, column},
        }});
}

// array_literal -> "[" (expr ("," expr)*)? "]"
Result<ast::Expr *> Parser::parse_array_literal()
{
    size_t line = previous().line;
    size_t column = previous().column;

    std::vector<ast::Expr *> elements;
    if (!check(lex::TokenType::RightBracket)) {
        do {
            if (check(lex::TokenType::RightBracket))
                break;
            elements.push_back(__Try(expression()));
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightBracket, "Expect ']' after array elements"));

    return mem::Arena::alloc(
        this->arena_,
        ast::Expr{ast::Array_literal_expr{
            .elements = elements,
            .type = types::Type(types::Primitive_kind::Void),
            .loc = {line, column},
        }});
}

// model_literal -> IDENT "{" ( (IDENT ":")? expr ("," (IDENT ":")? expr)* ","? )? "}"
Result<ast::Expr *> Parser::parse_model_literal(const std::string &model_name)
{
    auto brace = __Try(consume(lex::TokenType::LeftBrace, "Expect '{' for model literal"));

    std::vector<std::pair<std::string, ast::Expr *>> fields;
    if (!check(lex::TokenType::RightBrace)) {
        do {
            if (check(lex::TokenType::RightBrace))
                break; // trailing comma support

            std::string field_name;
            if (check(lex::TokenType::Identifier) && check_next(lex::TokenType::Colon)) {
                field_name = advance().lexeme;
                advance(); // Consume ':'
            }

            auto value = __Try(expression());
            fields.push_back({field_name, value});
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after model fields"));

    return mem::Arena::alloc(
        this->arena_,
        ast::Expr{ast::Model_literal_expr{
            .model_name = model_name,
            .fields = fields,
            .type = types::Type(types::Primitive_kind::Void),
            .loc = {brace.line, brace.column},
        }});
}

// fstring -> split raw template on "{" "}" pairs, re-lex each interpolation,
// and preserve the full construct as a dedicated AST node.
Result<ast::Expr *> Parser::parse_fstring(const lex::Token &tok)
{
    ast::Source_location loc{tok.line, tok.column};
    const std::string &raw = std::get<std::string>(tok.literal.payload);

    std::vector<ast::Expr *> interpolations;

    size_t i = 0;
    while (i < raw.size()) {
        if (raw[i] == '{') {
            // ESCAPE CHECK FOR OPENING BRACE
            if (i + 1 < raw.size() && raw[i + 1] == '{') {
                i += 2; // Leapfrog over the '{{'
                continue;
            }

            size_t start = ++i;
            int depth = 1;
            while (i < raw.size() && depth > 0) {
                if (raw[i] == '{')
                    depth++;
                else if (raw[i] == '}')
                    depth--;
                if (depth > 0)
                    i++;
            }
            std::string inner = raw.substr(start, i - start);
            i++; // Skip the closing '}'

            lex::Lexer inner_lexer(inner);
            auto inner_tokens = inner_lexer.tokenize();
            Parser inner_parser(std::move(inner_tokens), arena_);

            inner_parser.current_model_      = current_model_;
            inner_parser.m_known_model_names = m_known_model_names;
            inner_parser.m_known_union_names = m_known_union_names;
            inner_parser.m_known_enum_names  = m_known_enum_names;
            inner_parser.function_types_     = function_types_;
            inner_parser.parsed_models       = parsed_models;

            auto inner_expr = inner_parser.expression();
            if (!inner_expr)
                return std::unexpected(create_error(tok, "Invalid expression in f-string: " + inner));

            inner_parser.skip_newlines();
            if (!inner_parser.is_at_end())
                return std::unexpected(create_error(tok, "Unexpected trailing tokens in f-string expression: " + inner));

            interpolations.push_back(*inner_expr);
        } else if (raw[i] == '}') {
            // ESCAPE CHECK FOR CLOSING BRACE
            if (i + 1 < raw.size() && raw[i + 1] == '}') {
                i += 2; // Leapfrog over the '}}'
                continue;
            } else {
                // If it's just a single '}', it's a syntax error!
                return std::unexpected(create_error(tok, "Unmatched closing brace '}' in f-string. Use '}}' to escape."));
            }
        } else {
            i++;
        }
    }

    return mem::Arena::alloc(
        arena_,
        ast::Expr{ast::Fstring_expr{
            .raw_template = raw,
            .interpolations = std::move(interpolations),
            .type = types::Type(types::Primitive_kind::String),
            .loc = loc,
        }}
    );
}

// --- type parser -------------------------------------------------------------

// type        -> "(" type ")"
//              | ("|" param_types "|" | "||") "->" type    closure type
//              | "model" "{" (IDENT ":" type (";" IDENT ":" type)*)? ";"? "}"
//              | type_name type_suffix*
// type_suffix -> "[" "]"                                   array type
//              | "?"                                       optional type
// type_name   -> primitive_kw | known_union_name | IDENT   (defaults to model)
Result<types::Type> Parser::parse_type()
{
    types::Type current_type;

    if (match({lex::TokenType::LeftParen})) {
        current_type = __Try(parse_type());
        __TryIgnore(consume(lex::TokenType::RightParen, "Expected ')' after type in parentheses"));
    } else if (peek().type == lex::TokenType::Pipe || peek().type == lex::TokenType::LogicalOr) {
        std::vector<types::Type> parameter_types;

        if (match({lex::TokenType::LogicalOr})) {
            // zero parameters: "||"
        } else if (match({lex::TokenType::Pipe})) {
            if (!check(lex::TokenType::Pipe)) {
                do {
                    if (check(lex::TokenType::Pipe))
                        break;
                    parameter_types.push_back(__Try(parse_type()));
                } while (match({lex::TokenType::Comma}));
            }
            __TryIgnore(consume(lex::TokenType::Pipe, "Expect '|' after closure parameter types"));
        }

        __TryIgnore(consume(lex::TokenType::Arrow, "Expect '->' after closure parameters"));
        auto return_type_result = __Try(parse_type());

        auto closure_t = mem::make_rc<types::Function_type>();
        closure_t->parameter_types = parameter_types;
        closure_t->return_type = return_type_result;
        current_type = types::Type(closure_t);
    } else if (match({lex::TokenType::TInt64}))
        current_type = types::Type(types::Primitive_kind::I64);
    else if (match({lex::TokenType::TInt32}))
        current_type = types::Type(types::Primitive_kind::I32);
    else if (match({lex::TokenType::TInt16}))
        current_type = types::Type(types::Primitive_kind::I16);
    else if (match({lex::TokenType::TInt8}))
        current_type = types::Type(types::Primitive_kind::I8);
    else if (match({lex::TokenType::TUInt64}))
        current_type = types::Type(types::Primitive_kind::U64);
    else if (match({lex::TokenType::TUInt32}))
        current_type = types::Type(types::Primitive_kind::U32);
    else if (match({lex::TokenType::TUInt16}))
        current_type = types::Type(types::Primitive_kind::U16);
    else if (match({lex::TokenType::TUInt8}))
        current_type = types::Type(types::Primitive_kind::U8);
    else if (match({lex::TokenType::TFloat64}))
        current_type = types::Type(types::Primitive_kind::F64);
    else if (match({lex::TokenType::TFloat32}))
        current_type = types::Type(types::Primitive_kind::F32);
    else if (match({lex::TokenType::TFloat16}))
        current_type = types::Type(types::Primitive_kind::F16);
    else if (match({lex::TokenType::TBool}))
        current_type = types::Type(types::Primitive_kind::Bool);
    else if (match({lex::TokenType::TString}))
        current_type = types::Type(types::Primitive_kind::String);
    else if (match({lex::TokenType::TVoid}))
        current_type = types::Type(types::Primitive_kind::Void);
    else if (match({lex::TokenType::TAny}))
        current_type = types::Type(types::Primitive_kind::Any);
    else if (match({lex::TokenType::Model})) {
        auto model_type = mem::make_rc<types::Model_type>();
        model_type->name = ""; // Empty string name means it's an anonymous structural model!

        __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after 'model' in anonymous type"));

        while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
            auto field_name = __Try(consume(lex::TokenType::Identifier, "Expect field name in anonymous model type"));
            __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after field name"));
            auto field_type = __Try(parse_type());
            model_type->fields.push_back({field_name.lexeme, field_type});

            // Note: Support parsing semicolons to separate structural fields
            if (check(lex::TokenType::Semicolon)) {
                advance(); // consume ';'
            } else if (check(lex::TokenType::Comma)) {
                advance(); // gracefully allow comma fallback
            }
        }

        __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after anonymous model fields"));
        current_type = types::Type(model_type);
    } else if (match({lex::TokenType::Identifier})) {
        std::string type_name = previous().lexeme;
        if (m_known_union_names.count(type_name)) {
            auto union_type = mem::make_rc<types::Union_type>();
            union_type->name = type_name;
            current_type = types::Type(union_type);
        } else if (m_known_enum_names.count(type_name)) {
            auto enum_type = mem::make_rc<types::Enum_type>();
            enum_type->name = type_name;
            current_type = types::Type(enum_type);
        } else {
            // forward-declared or known model
            auto model_type = mem::make_rc<types::Model_type>();
            model_type->name = type_name;
            current_type = types::Type(model_type);
        }
    } else {
        return std::unexpected(create_error(peek(), "Expected a type name or '(' for a grouped type"));
    }

    // type suffixes: "[]" for array, "?" for optional
    while (true) {
        if (match({lex::TokenType::LeftBracket})) {
            __TryIgnore(consume(lex::TokenType::RightBracket, "Expected ']' to complete array type suffix"));
            current_type = types::Type(mem::make_rc<types::Array_type>(current_type));
        } else if (match({lex::TokenType::Question})) {
            auto optional_type = mem::make_rc<types::Optional_type>();
            optional_type->base_type = current_type;
            current_type = types::Type(optional_type);
        } else {
            break;
        }
    }

    return current_type;
}

} // namespace phos
