#include "parser.hpp"

#include "../error/err.hpp"
#include "../error/result.hpp"
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

static types::Type_id map_token_to_type(lex::TokenType token_type, types::Type_table& tt) {
    switch (token_type) {
        case lex::TokenType::TInt8:      return tt.get_i8();
        case lex::TokenType::TInt16:     return tt.get_i16();
        case lex::TokenType::Integer32:  // Deault to i32
        case lex::TokenType::TInt32:     return tt.get_i32();
        case lex::TokenType::TInt64:     return tt.get_i64();

        case lex::TokenType::TUInt8:     return tt.get_u8();
        case lex::TokenType::TUInt16:    return tt.get_u16();
        case lex::TokenType::TUInt32:    return tt.get_u32();
        case lex::TokenType::TUInt64:    return tt.get_u64();
        case lex::TokenType::TFloat16:   return tt.get_f16();
        case lex::TokenType::TFloat32:   return tt.get_f32();
        case lex::TokenType::Float64:    // Deault to f64
        case lex::TokenType::TFloat64:   return tt.get_f64();

        case lex::TokenType::Bool:       return tt.get_bool();
        case lex::TokenType::String:     return tt.get_string();
        default:                         return tt.get_unknown();
    }
}

// Utility
void Parser::skip_newlines()
{
    while (!is_at_end() && match({lex::TokenType::Newline})) {}
}

const lex::Token &Parser::peek() const
{
    if (current_ >= tokens_.size()) {
        static lex::Token eof_token(lex::TokenType::Eof, "", Value(), 0, 0);
        return eof_token;
    }
    return tokens_[current_];
}

const lex::Token &Parser::previous() const
{
    if (current_ == 0) {
        return tokens_[0];
    }
    return tokens_[current_ - 1];
}

lex::Token Parser::advance()
{
    if (!is_at_end()) {
        current_++;
    }
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
    if (current_ + 1 >= tokens_.size()) {
        return false;
    }
    return tokens_[current_ + 1].type == type;
}

// consume -> advance if current token matches type, else return error
Result<lex::Token> Parser::consume(lex::TokenType type, const std::string &message)
{
    if (check(type)) {
        return advance();
    }
    return std::unexpected(err::msg::error("parser", peek().line, peek().column, source_name_, "{}", message));
}

err::msg Parser::create_error(const lex::Token &token, const std::string &message)
{
    return err::msg::error(this->stage_, token.line, token.column, source_name_, "{}", message);
}

// Skip tokens until we reach a point where we can resume parsing cleanly.
void Parser::synchronize()
{
    advance();
    while (!is_at_end()) {
        if (previous().type == lex::TokenType::Semicolon) {
            return;
        }

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

// Top-Level
// program -> declaration* EOF
Result<std::vector<ast::Stmt_id>> Parser::parse()
{
    std::vector<ast::Stmt_id> statements;
    bool had_error = false;

    while (!is_at_end()) {
        skip_newlines();
        if (is_at_end()) {
            break;
        }

        auto decl_result = declaration();
        if (!decl_result) {
            if (!decl_result.error().summary.empty()) {
                std::println(stderr, "{}", decl_result.error().format());
            }

            had_error = true;
            synchronize();
            continue;
        }

        if (decl_result.value()) {
            statements.push_back(*decl_result.value());
        }
    }

    if (had_error) {
        return std::unexpected(err::msg::error("parser", 0, 0, source_name_, "Compilation halted due to syntax errors"));
    }

    stamp_statement_locations(statements);
    return statements;
}

void Parser::stamp_loc(ast::Source_location &loc)
{
    if (loc.file.empty()) {
        loc.file = source_name_;
    }
}

void Parser::stamp_expr(ast::Expr_id expr_id)
{
    if (expr_id.is_null()) {
        return;
    }

    auto &expr = tree_.get(expr_id).node;
    stamp_loc(ast::get_loc(expr));

    std::visit(
        [this](auto &e) {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, ast::Binary_expr>) {
                stamp_expr(e.left);
                stamp_expr(e.right);
            } else if constexpr (std::is_same_v<T, ast::Unary_expr>) {
                stamp_expr(e.right);
            } else if constexpr (std::is_same_v<T, ast::Call_expr>) {
                stamp_expr(e.callee);
                for (auto &arg : e.arguments) {
                    stamp_loc(arg.loc);
                    stamp_expr(arg.value);
                }
            } else if constexpr (std::is_same_v<T, ast::Assignment_expr>) {
                stamp_expr(e.value);
            } else if constexpr (std::is_same_v<T, ast::Field_assignment_expr>) {
                stamp_expr(e.object);
                stamp_expr(e.value);
            } else if constexpr (std::is_same_v<T, ast::Array_assignment_expr>) {
                stamp_expr(e.array);
                stamp_expr(e.index);
                stamp_expr(e.value);
            } else if constexpr (std::is_same_v<T, ast::Cast_expr>) {
                stamp_expr(e.expression);
            } else if constexpr (std::is_same_v<T, ast::Field_access_expr>) {
                stamp_expr(e.object);
            } else if constexpr (std::is_same_v<T, ast::Method_call_expr>) {
                stamp_expr(e.object);
                for (auto &arg : e.arguments) {
                    stamp_loc(arg.loc);
                    stamp_expr(arg.value);
                }
            } else if constexpr (std::is_same_v<T, ast::Model_literal_expr>) {
                for (auto &[_, field_expr] : e.fields) {
                    stamp_expr(field_expr);
                }
            } else if constexpr (std::is_same_v<T, ast::Closure_expr>) {
                for (auto &param : e.parameters) {
                    stamp_loc(param.loc);
                    stamp_expr(param.default_value);
                }
                stamp_stmt(e.body);
            } else if constexpr (std::is_same_v<T, ast::Array_literal_expr>) {
                for (auto elem : e.elements) {
                    stamp_expr(elem);
                }
            } else if constexpr (std::is_same_v<T, ast::Array_access_expr>) {
                stamp_expr(e.array);
                stamp_expr(e.index);
            } else if constexpr (std::is_same_v<T, ast::Static_path_expr>) {
                stamp_expr(e.base);
            } else if constexpr (std::is_same_v<T, ast::Range_expr>) {
                stamp_expr(e.start);
                stamp_expr(e.end);
            } else if constexpr (std::is_same_v<T, ast::Spawn_expr>) {
                stamp_expr(e.call);
            } else if constexpr (std::is_same_v<T, ast::Await_expr>) {
                stamp_expr(e.thread);
            } else if constexpr (std::is_same_v<T, ast::Yield_expr>) {
                stamp_expr(e.value);
            } else if constexpr (std::is_same_v<T, ast::Anon_model_literal_expr>) {
                for (auto &[_, field_expr] : e.fields) {
                    stamp_expr(field_expr);
                }
            }
        },
        expr);
}

void Parser::stamp_stmt(ast::Stmt_id stmt_id)
{
    if (stmt_id.is_null()) {
        return;
    }

    auto &stmt = tree_.get(stmt_id).node;
    stamp_loc(ast::get_loc(stmt));

    std::visit(
        [this](auto &s) {
            using T = std::decay_t<decltype(s)>;

            if constexpr (std::is_same_v<T, ast::Return_stmt>) {
                stamp_expr(s.expression);
            } else if constexpr (std::is_same_v<T, ast::Function_stmt>) {
                for (auto &param : s.parameters) {
                    stamp_loc(param.loc);
                    stamp_expr(param.default_value);
                }
                stamp_stmt(s.body);
            } else if constexpr (std::is_same_v<T, ast::Model_stmt>) {
                for (auto &field : s.fields) {
                    stamp_loc(field.loc);
                    stamp_expr(field.default_value);
                }
                for (auto method : s.methods) {
                    stamp_stmt(method);
                }
            } else if constexpr (std::is_same_v<T, ast::Var_stmt>) {
                stamp_expr(s.initializer);
            } else if constexpr (std::is_same_v<T, ast::Print_stmt>) {
                for (auto expr : s.expressions) {
                    stamp_expr(expr);
                }
            } else if constexpr (std::is_same_v<T, ast::Expr_stmt>) {
                stamp_expr(s.expression);
            } else if constexpr (std::is_same_v<T, ast::Block_stmt>) {
                for (auto child : s.statements) {
                    stamp_stmt(child);
                }
            } else if constexpr (std::is_same_v<T, ast::If_stmt>) {
                stamp_expr(s.condition);
                stamp_stmt(s.then_branch);
                stamp_stmt(s.else_branch);
            } else if constexpr (std::is_same_v<T, ast::While_stmt>) {
                stamp_expr(s.condition);
                stamp_stmt(s.body);
            } else if constexpr (std::is_same_v<T, ast::For_stmt>) {
                stamp_stmt(s.initializer);
                stamp_expr(s.condition);
                stamp_expr(s.increment);
                stamp_stmt(s.body);
            } else if constexpr (std::is_same_v<T, ast::For_in_stmt>) {
                stamp_expr(s.iterable);
                stamp_stmt(s.body);
            } else if constexpr (std::is_same_v<T, ast::Union_stmt>) {
                for (auto &variant : s.variants) {
                    stamp_loc(variant.loc);
                    stamp_expr(variant.default_value);
                }
            } else if constexpr (std::is_same_v<T, ast::Match_stmt>) {
                stamp_expr(s.subject);
                for (auto &arm : s.arms) {
                    stamp_expr(arm.pattern);
                    stamp_stmt(arm.body);
                }
            }
        },
        stmt);
}

void Parser::stamp_statement_locations(const std::vector<ast::Stmt_id> &statements)
{
    for (auto stmt_id : statements) {
        stamp_stmt(stmt_id);
    }
}

// Declarations
Result<ast::Function_param> Parser::parse_function_parameter(bool allow_default_value)
{
    bool is_mut = match({lex::TokenType::Mut});
    auto param_name = __Try(consume(lex::TokenType::Identifier, "Expect parameter name"));
    __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after parameter name"));

    auto param_type = __Try(parse_type());

    ast::Expr_id default_value = ast::Expr_id::null();
    if (allow_default_value && match({lex::TokenType::Assign})) {
        default_value = __Try(expression());
    }

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

    if (check(lex::TokenType::RightParen)) {
        return arguments;
    }

    do {
        if (check(lex::TokenType::RightParen)) {
            break;
        }

        if (check(lex::TokenType::Identifier) && check_next(lex::TokenType::Assign)) {
            auto name = advance();
            __TryIgnore(consume(lex::TokenType::Assign, "Expect '=' after argument name"));
            auto value = __Try(expression());

            arguments.push_back(
                ast::Call_argument{
                    .name = name.lexeme,
                    .value = value,
                    .loc = {name.line, name.column},
                });
        } else {
            auto value = __Try(expression());

            arguments.push_back(
                ast::Call_argument{
                    .name = "",
                    .value = value,
                    .loc = ast::get_loc(tree_.get(value).node),
                }
            );
        }
    } while (match({lex::TokenType::Comma}));

    return arguments;
}

// declaration -> fn_decl | model_decl | union_decl | enum_decl | var_decl | statement
Result<std::optional<ast::Stmt_id>> Parser::declaration()
{
    skip_newlines();
    if (is_at_end()) {
        return std::optional<ast::Stmt_id>{std::nullopt};
    }

    if (match({lex::TokenType::Fn})) {
        return std::optional<ast::Stmt_id>{__Try(function_declaration())};
    }
    if (match({lex::TokenType::Model})) {
        return std::optional<ast::Stmt_id>{__Try(model_declaration())};
    }
    if (match({lex::TokenType::Bind})) {
        __Try(parse_bind_statement());
        return std::optional<ast::Stmt_id>{std::nullopt};
    }
    if (match({lex::TokenType::Union})) {
        return std::optional<ast::Stmt_id>{__Try(union_declaration())};
    }
    if (match({lex::TokenType::Enum})) {
        return std::optional<ast::Stmt_id>{__Try(enum_declaration())};
    }
    if (match({lex::TokenType::Let})) {
        return std::optional<ast::Stmt_id>{__Try(var_declaration(false))};
    }
    if (match({lex::TokenType::Const})) {
        return std::optional<ast::Stmt_id>{__Try(var_declaration(true))};
    }

    return std::optional<ast::Stmt_id>{__Try(statement())};
}

// fn_decl -> "fn" IDENT "(" param* ")" ("->" type)? block
Result<ast::Stmt_id> Parser::function_declaration()
{
    lex::Token name = __Try(consume(lex::TokenType::Identifier, "Expect function name"));
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after function name"));

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen)) {
        do {
            if (check(lex::TokenType::RightParen)) {
                break;
            }
            parameters.push_back(__Try(parse_function_parameter(true)));
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after parameters"));

    types::Type_id return_type{};

    if (match({lex::TokenType::Arrow})) {
        return_type = __Try(parse_type());
    }

    function_types_.emplace(name.lexeme, return_type);

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before function body"));
    auto body = __Try(block_statement());

    return tree_.add_stmt(
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
Result<ast::Stmt_id> Parser::model_declaration()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected model name"));
    current_model_ = name.lexeme;

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after model name"));

    std::vector<ast::Typed_member_decl> fields;
    std::vector<ast::Stmt_id> methods;

    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        fields.push_back(__Try(parse_model_field()));
        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '};' after model body"));
    current_model_.clear();

    ast::Stmt_id stmt_id = tree_.add_stmt(
        ast::Stmt{ast::Model_stmt{
            .name = name.lexeme,
            .fields = fields,
            .methods = methods,
            .loc = {name.line, name.column},
        }});

    // Save a direct ID to the Model AST node so 'bind' can find it later!
    parsed_models[name.lexeme] = stmt_id;

    return stmt_id;
}

// bind_stmt -> "bind" IDENT "{" method_decl* "}"
Result<ast::Stmt_id> Parser::parse_bind_statement()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected model name to bind to"));

    if (!parsed_models.count(name.lexeme)) {
        return std::unexpected(
            create_error(name, "Cannot bind methods to undeclared model '" + name.lexeme + "'. Please declare the model first"));
    }

    ast::Stmt_id target_model_id = parsed_models[name.lexeme];
    current_model_ = name.lexeme;

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before bind body"));

    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        auto method_ast = __Try(parse_model_method());
        ast::Stmt_id method_id = tree_.add_stmt(ast::Stmt{method_ast});

        // Re-fetch pointer AFTER adding to tree to prevent vector reallocation invalidation!
        auto *target_model = std::get_if<ast::Model_stmt>(&tree_.get(target_model_id).node);
        target_model->methods.push_back(method_id);

        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after bind body"));
    current_model_.clear();

    return ast::Stmt_id::null(); // Bind statement doesn't create a new standalone block
}

// union_decl -> "union" IDENT "{" (IDENT ":" type ";")* "}"
Result<ast::Stmt_id> Parser::union_declaration()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected union name"));
    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after union name"));

    std::vector<ast::Typed_member_decl> variants;
    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        auto variant_name = __Try(consume(lex::TokenType::Identifier, "Expect variant name"));
        __TryIgnore(consume(lex::TokenType::Colon, "Expect ':' after variant name for type"));

        types::Type_id variant_type = __Try(parse_type());
        ast::Expr_id default_value = ast::Expr_id::null();

        if (match({lex::TokenType::Assign})) {
            default_value = __Try(expression());
        }

        variants.push_back(
            ast::Typed_member_decl{
                .name = variant_name.lexeme,
                .type = variant_type,
                .default_value = default_value,
                .loc = {variant_name.line, variant_name.column},
            });

        if (!check(lex::TokenType::RightBrace)) {
            __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after variant declaration"));
        }

        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after union body"));

    return tree_.add_stmt(
        ast::Stmt{ast::Union_stmt{
            .name = name.lexeme,
            .variants = variants,
            .loc = {name.line, name.column},
        }});
}

// enum_decl -> "enum" IDENT (":" type)? "{" (IDENT ("=" INT | STRING)? ","?)* "}"
Result<ast::Stmt_id> Parser::enum_declaration()
{
    auto name = __Try(consume(lex::TokenType::Identifier, "Expected enum name"));
    types::Type_id base_type{};

    if (match({lex::TokenType::Colon})) {
        base_type = __Try(parse_type());
        if (!(type_table_.is_primitive(base_type) && (type_table_.is_integer_primitive(base_type) || type_table_.is_string(base_type)))) {
            return std::unexpected(create_error(previous(), "Enum base type must be an integer type or 'string'"));
        }
    } else {
        // NEW: Default to a 64-bit integer if no base type is explicitly provided!
        base_type = type_table_.get_i64();
    }

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after enum name"));

    std::vector<std::pair<std::string, std::optional<Value>>> variants;
    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        auto variant_name = __Try(consume(lex::TokenType::Identifier, "Expect enum variant name"));
        std::optional<Value> value = std::nullopt;

        if (match({lex::TokenType::Assign})) {
            if (type_table_.is_primitive(base_type) && type_table_.is_integer_primitive(base_type)) {
                auto val_tok = __Try(consume(lex::TokenType::Integer32, "Expect integer value after '=' in integer enum"));
                auto coerced = coerce_numeric_literal(val_tok.literal, type_table_.get_primitive(base_type));

                if (!coerced) {
                    return std::unexpected(create_error(val_tok, "Enum variant value does not fit the enum base type"));
                }

                value = coerced.value();
            } else {
                auto val_tok = __Try(consume(lex::TokenType::String, "Expect string value after '=' in string enum"));
                value = val_tok.literal;
            }
        }

        variants.push_back({variant_name.lexeme, value});

        match({lex::TokenType::Comma}); // Optional trailing comma
        skip_newlines();
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after enum body"));

    return tree_.add_stmt(ast::Stmt{ast::Enum_stmt{
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

    ast::Expr_id default_value = ast::Expr_id::null();
    if (match({lex::TokenType::Assign})) {
        default_value = __Try(expression());
    }

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
            if (check(lex::TokenType::RightParen)) {
                break;
            }
            parameters.push_back(__Try(parse_function_parameter(true)));
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after parameters"));

    types::Type_id return_type = type_table_.get_void();
    if (match({lex::TokenType::Arrow})) {
        return_type = __Try(parse_type());
    }

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

// var_decl   -> "let" "mut"? IDENT (":=" expr | ":" type ("=" expr)?) ";"
// const_decl -> "const" IDENT (":=" expr | ":" type ("=" expr)?) ";"
Result<ast::Stmt_id> Parser::var_declaration(bool is_const)
{
    bool is_mut = false;

    // Only check for 'mut' if we are in a 'let' declaration
    if (!is_const) {
        is_mut = match({lex::TokenType::Mut});
    }

    auto name = __Try(consume(lex::TokenType::Identifier, "Expect variable or constant name"));

    types::Type_id var_type{};
    ast::Expr_id initializer = ast::Expr_id::null();
    bool type_inferred = false;

    if (match({lex::TokenType::Colon})) {
        if (match({lex::TokenType::Assign})) {
            // let x := expr  OR  const x := expr
            initializer = __Try(expression());
            type_inferred = true;
        } else {
            // let x: T = expr  OR  const x: T = expr
            var_type = __Try(parse_type());

            if (match({lex::TokenType::Assign})) {
                initializer = __Try(expression());
            } else if (is_const) {
                // Semantic enforcement: Constants must have an initializer
                return std::unexpected(create_error(peek(), "Constants must be initialized"));
            }
        }
    } else {
        return std::unexpected(create_error(peek(), "Expect ':' or ':=' after name"));
    }

    __TryIgnore(consume(lex::TokenType::Semicolon, "Expected ';' after declaration"));

    return tree_.add_stmt(ast::Stmt{ast::Var_stmt{
        .is_mut = is_mut,
        .is_const = is_const,
        .name = name.lexeme,
        .type = var_type,
        .initializer = initializer,
        .type_inferred = type_inferred,
        .loc = {name.line, name.column},
    }});
}

// =============================================================================
// Statements
// =============================================================================

// statement -> print_stmt | block | if_stmt | while_stmt | for_stmt
//            | match_stmt | return_stmt | expr_stmt
Result<ast::Stmt_id> Parser::statement()
{
    skip_newlines();
    if (is_at_end()) {
        return std::unexpected(create_error(peek(), "Unexpected end of file"));
    }

    if (match({lex::TokenType::Print})) {
        return print_statement(ast::Print_stream::STDOUT);
    }
    if (match({lex::TokenType::PrintErr})) {
        return print_statement(ast::Print_stream::STDERR);
    }
    if (match({lex::TokenType::LeftBrace})) {
        return block_statement();
    }
    if (match({lex::TokenType::If})) {
        return if_statement();
    }
    if (match({lex::TokenType::While})) {
        return while_statement();
    }
    if (match({lex::TokenType::Match})) {
        return match_statement();
    }
    if (match({lex::TokenType::Return})) {
        return return_statement();
    }

    if (match({lex::TokenType::For})) {
        // "for" "(" ...  ->  C-style for loop
        // "for" IDENT "in" ...  ->  for-in loop
        if (check(lex::TokenType::LeftParen)) {
            return for_statement();
        }
        return for_in_statement();
    }

    return expression_statement();
}

// print_stmt -> ("print" | "eprint") "(" (expr ("," expr)*)? ("," "sep" "=" expr)? ("," "end" "=" expr)? ")" ";"
Result<ast::Stmt_id> Parser::print_statement(ast::Print_stream stream)
{
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after 'print'"));

    std::vector<ast::Expr_id> expressions;
    std::string sep = "";
    std::string end = "\n";

    // Empty print() -> just print the end character (newline by default)
    if (check(lex::TokenType::RightParen)) {
        advance();
        __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after print statement"));
        return tree_.add_stmt(
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
        if (check(lex::TokenType::RightParen)) {
            break;
        }

        // Peek for named args: sep= or end=
        if (check(lex::TokenType::Identifier) && check_next(lex::TokenType::Assign)) {
            std::string name = peek().lexeme;
            if (name == "sep" || name == "end") {
                seen_named = true;
                advance(); // consume identifier
                advance(); // consume '='

                auto val_result = __Try(expression());

                auto *lit = std::get_if<ast::Literal_expr>(&tree_.get(val_result).node);
                if (!lit || !lit->value.is_string()) {
                    return std::unexpected(create_error(previous(), "'sep' and 'end' must be string literals"));
                }

                if (name == "sep") {
                    sep = lit->value.as_string();
                } else {
                    end = lit->value.as_string();
                }
            } else {
                if (seen_named) {
                    return std::unexpected(create_error(peek(), "Positional arguments cannot appear after named arguments in print()"));
                }

                auto expr_result = __Try(expression());
                expressions.push_back(expr_result);
            }
        } else {
            if (seen_named) {
                return std::unexpected(create_error(peek(), "Positional arguments cannot appear after named arguments in print()"));
            }

            auto expr_result = __Try(expression());
            expressions.push_back(expr_result);
        }
    } while (match({lex::TokenType::Comma}));

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after print arguments"));
    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after print statement"));

    return tree_.add_stmt(
        ast::Stmt{ast::Print_stmt{
            .stream = stream,
            .expressions = std::move(expressions),
            .sep = sep,
            .end = end,
            .loc = {previous().line, previous().column},
        }});
}

// block -> "{" declaration* "}"
Result<ast::Stmt_id> Parser::block_statement()
{
    std::vector<ast::Stmt_id> statements;
    size_t line = previous().line;
    size_t column = previous().column;

    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        auto stmt_result = declaration();
        if (!stmt_result) {
            return std::unexpected(stmt_result.error());
        }
        if (stmt_result.value()) {
            statements.push_back(*stmt_result.value());
        }
    }

    __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after block"));

    return tree_.add_stmt(
        ast::Stmt{ast::Block_stmt{
            .statements = statements,
            .loc = {line, column},
        }});
}

// if_stmt -> "if" expr "{" declaration* "}" ("else" (if_stmt | "{" declaration* "}"))?
Result<ast::Stmt_id> Parser::if_statement()
{
    auto condition_result = __Try(expression());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after if condition"));
    auto then_branch_result = __Try(block_statement());

    ast::Stmt_id else_branch = ast::Stmt_id::null();
    if (match({lex::TokenType::Else})) {
        if (match({lex::TokenType::If})) {
            else_branch = __TryOr(if_statement(), ast::Stmt_id::null());
        } else {
            __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after 'else'"));
            else_branch = __TryOr(block_statement(), ast::Stmt_id::null());
        }
    }

    return tree_.add_stmt(
        ast::Stmt{ast::If_stmt{
            .condition = condition_result,
            .then_branch = then_branch_result,
            .else_branch = else_branch,
            .loc = {previous().line, previous().column},
        }});
}

// while_stmt -> "while" expr "{" declaration* "}"
Result<ast::Stmt_id> Parser::while_statement()
{
    auto condition_result = __Try(expression());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after while condition"));
    auto body_result = __Try(block_statement());

    return tree_.add_stmt(
        ast::Stmt{ast::While_stmt{
            .condition = condition_result,
            .body = body_result,
            .loc = {previous().line, previous().column},
        }});
}

// for_stmt -> "for" "(" (var_decl | const_decl | expr_stmt | ";") expr? ";" expr? ")" block
Result<ast::Stmt_id> Parser::for_statement()
{
    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after 'for'"));

    ast::Stmt_id initializer = ast::Stmt_id::null();
    ast::Expr_id condition = ast::Expr_id::null();
    ast::Expr_id increment = ast::Expr_id::null();

    if (match({lex::TokenType::Semicolon})) {
        // empty initializer, do nothing
    } else if (match({lex::TokenType::Let})) {
        initializer = __TryOr(var_declaration(false), ast::Stmt_id::null());
    } else if (match({lex::TokenType::Const})) {
        initializer = __TryOr(var_declaration(true), ast::Stmt_id::null());
    } else {
        initializer = __TryOr(expression_statement(), ast::Stmt_id::null());
    }

    if (!check(lex::TokenType::Semicolon)) {
        condition = __TryOr(expression(), ast::Expr_id::null());
    }
    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after loop condition"));

    if (!check(lex::TokenType::RightParen)) {
        increment = __TryOr(expression(), ast::Expr_id::null());
    }
    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after for clauses"));

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after for clauses"));
    auto body = __Try(block_statement());

    return tree_.add_stmt(ast::Stmt{ast::For_stmt{initializer, condition, increment, body, {previous().line, previous().column}}});
}

// for_in_stmt
Result<ast::Stmt_id> Parser::for_in_statement()
{
    auto var_name = __Try(consume(lex::TokenType::Identifier, "Expect loop variable name after 'for'"));
    __TryIgnore(consume(lex::TokenType::In, "Expect 'in' after loop variable"));

    auto iterable = __Try(expression());

    // FORCE BLOCK HERE
    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after for-in iterable"));
    auto body = __Try(block_statement());

    return tree_.add_stmt(
        ast::Stmt{ast::For_in_stmt{
            .var_name = var_name.lexeme,
            .iterable = iterable,
            .body = body,
            .loc = {var_name.line, var_name.column},
        }});
}

// match_stmt -> "match" expr "{" match_arm* "}"
// match_arm  -> (expr ("(" IDENT ")")? | "_") "=>" statement ","?
Result<ast::Stmt_id> Parser::match_statement()
{
    ast::Source_location loc{peek().line, peek().column};
    auto subject = __Try(expression());

    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after match subject"));

    std::vector<ast::Match_arm> arms;
    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        ast::Match_arm arm;

        if (match({lex::TokenType::Underscore})) {
            arm.is_wildcard = true;
        } else {
            // Parse the pattern as a standard expression
            arm.pattern = __Try(expression());

            // If the user typed `Result::Ok(data)`, the expression parser
            // naturally consumed it as a Call_expr. We unpack that into the
            // actual pattern + the binding name!
            if (auto *call_node = std::get_if<ast::Call_expr>(&tree_.get(arm.pattern).node)) {
                // 1. The actual pattern is just the callee (e.g., Result::Ok)
                arm.pattern = call_node->callee;

                // 2. Validate and extract the payload binding name
                if (call_node->arguments.size() != 1) {
                    return std::unexpected(
                        err::msg::error("parser", loc.l, loc.c, source_name_, "Match union payloads can only bind exactly one variable"));
                }

                if (auto *var_node = std::get_if<ast::Variable_expr>(&tree_.get(call_node->arguments[0].value).node)) {
                    arm.bind_name = var_node->name;
                } else {
                    return std::unexpected(
                        err::msg::error("parser", loc.l, loc.c, source_name_, "Match binding payload must be a simple identifier"));
                }
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

    return tree_.add_stmt(
        ast::Stmt{ast::Match_stmt{
            .subject = subject,
            .arms = std::move(arms),
            .loc = loc,
        }});
}

// return_stmt -> "return" expr? ";"
Result<ast::Stmt_id> Parser::return_statement()
{
    size_t line = previous().line;
    size_t column = previous().column;

    ast::Expr_id value = ast::Expr_id::null();
    if (!check(lex::TokenType::Semicolon)) {
        value = __TryOr(expression(), ast::Expr_id::null());
    }

    __TryIgnore(consume(lex::TokenType::Semicolon, "Expect ';' after return value"));

    return tree_.add_stmt(ast::Stmt{ast::Return_stmt{value, {line, column}}});
}

// expr_stmt -> expr ";"
Result<ast::Stmt_id> Parser::expression_statement()
{
    auto expr_result = expression();
    if (!expr_result) {
        return std::unexpected(expr_result.error());
    }

    auto semicolon_result = consume(lex::TokenType::Semicolon, "Expect ';' after expression");
    if (!semicolon_result) {
        return std::unexpected(semicolon_result.error());
    }

    return tree_.add_stmt(ast::Stmt{ast::Expr_stmt{expr_result.value(), {previous().line, previous().column}}});
}

// =============================================================================
// Expressions (Precedence: Low -> High)
// =============================================================================

Result<ast::Expr_id> Parser::expression()
{
    return assignment();
}

// assignment -> range_expr ("=" assignment)?
// lvalue rewrites: Variable -> Assignment_expr
//                  Field_access -> Field_assignment_expr
//                  Array_access -> Array_assignment_expr
Result<ast::Expr_id> Parser::assignment()
{
    auto expr = __Try(range_expr());

    if (match({lex::TokenType::Assign})) {
        lex::Token equals = previous();
        auto value_result = __Try(assignment());

        // Vector invalidation safety: MUST get the reference after __Try(assignment()) returns!
        auto &expr_ref = tree_.get(expr);

        if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr_ref.node)) {
            return tree_.add_expr(
                ast::Expr{ast::Assignment_expr{var_expr->name, value_result, var_expr->type, {equals.line, equals.column}}});
        } else if (auto *field_access_expr = std::get_if<ast::Field_access_expr>(&expr_ref.node)) {
            return tree_.add_expr(
                ast::Expr{ast::Field_assignment_expr{
                    .object = field_access_expr->object,
                    .field_name = field_access_expr->field_name,
                    .value = value_result,
                    .type = type_table_.get_unknown(),
                    .loc = {equals.line, equals.column},
                }});
        } else if (auto *array_access_expr = std::get_if<ast::Array_access_expr>(&expr_ref.node)) {
            return tree_.add_expr(
                ast::Expr{ast::Array_assignment_expr{
                    .array = array_access_expr->array,
                    .index = array_access_expr->index,
                    .value = value_result,
                    .type = type_table_.get_unknown(),
                    .loc = {equals.line, equals.column},
                }});
        }

        return std::unexpected(create_error(equals, "Invalid assignment target"));
    }
    return expr;
}

// range_expr -> logical_or (".." logical_or)?
Result<ast::Expr_id> Parser::range_expr()
{
    auto expr = __Try(logical_or());

    if (match({lex::TokenType::DotDot, lex::TokenType::DotDotEq})) {
        bool inclusive = (previous().type == lex::TokenType::DotDotEq);
        lex::Token op = previous();
        auto end_expr = __Try(logical_or());

        return tree_.add_expr(
            ast::Expr{ast::Range_expr{
                .start = expr,
                .end = end_expr,
                .inclusive = inclusive,
                .type = type_table_.get_unknown(),
                .loc = {op.line, op.column},
            }});
    }

    return expr;
}

// logical_or -> logical_and ("||" logical_and)*
Result<ast::Expr_id> Parser::logical_or()
{
    auto expr = __Try(logical_and());

    while (match({lex::TokenType::LogicalOr})) {
        lex::Token op = previous();
        auto right = __Try(logical_and());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_bool(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// logical_and -> bitwise_or ("&&" bitwise_or)*
Result<ast::Expr_id> Parser::logical_and()
{
    auto expr = __Try(bitwise_or());

    while (match({lex::TokenType::LogicalAnd})) {
        lex::Token op = previous();
        auto right = __Try(bitwise_or());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_bool(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// bitwise_or -> bitwise_xor ("|" bitwise_xor)*
Result<ast::Expr_id> Parser::bitwise_or()
{
    auto expr = __Try(bitwise_xor());

    while (match({lex::TokenType::Pipe})) {
        lex::Token op = previous();
        auto right = __Try(bitwise_xor());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_bool(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// bitwise_xor -> bitwise_and ("^" bitwise_and)*
Result<ast::Expr_id> Parser::bitwise_xor()
{
    auto expr = __Try(bitwise_and());

    while (match({lex::TokenType::BitXor})) {
        lex::Token op = previous();
        auto right = __Try(bitwise_and());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_bool(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// bitwise_and -> equality ("&" equality)*
Result<ast::Expr_id> Parser::bitwise_and()
{
    auto expr = __Try(equality());

    while (match({lex::TokenType::BitAnd})) {
        lex::Token op = previous();
        auto right = __Try(equality());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_unknown(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// equality -> comparison (("==" | "!=") comparison)*
Result<ast::Expr_id> Parser::equality()
{
    auto expr = __Try(comparison());

    while (match({lex::TokenType::Equal, lex::TokenType::NotEqual})) {
        lex::Token op = previous();
        auto right = __Try(comparison());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_bool(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// comparison -> bitwise_shift (("<" | "<=" | ">" | ">=") bitwise_shift)*
Result<ast::Expr_id> Parser::comparison()
{
    auto expr = __Try(bitwise_shift());

    while (match({lex::TokenType::Less, lex::TokenType::LessEqual, lex::TokenType::Greater, lex::TokenType::GreaterEqual})) {
        lex::Token op = previous();
        auto right = __Try(bitwise_shift());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_bool(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// bitwise_shift -> term (("<<" | ">>") term)*
Result<ast::Expr_id> Parser::bitwise_shift()
{
    auto expr = __Try(term());

    while (match({lex::TokenType::BitLShift, lex::TokenType::BitRshift})) {
        lex::Token op = previous();
        auto right = __Try(term());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_unknown(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// term -> factor (("+" | "-") factor)*
Result<ast::Expr_id> Parser::term()
{
    auto expr = __Try(factor());

    while (match({lex::TokenType::Plus, lex::TokenType::Minus})) {
        lex::Token op = previous();
        auto right = __Try(factor());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_unknown(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// factor -> cast (("*" | "/" | "%") cast)*
Result<ast::Expr_id> Parser::factor()
{
    auto expr = __Try(cast());

    while (match({lex::TokenType::Star, lex::TokenType::Slash, lex::TokenType::Percent})) {
        lex::Token op = previous();
        auto right = __Try(cast());
        expr = tree_.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = type_table_.get_unknown(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// cast -> unary ("as" type)*
Result<ast::Expr_id> Parser::cast()
{
    auto expr = __Try(unary());

    while (match({lex::TokenType::As})) {
        auto target_type = __Try(parse_type());
        auto loc = ast::get_loc(tree_.get(expr).node);
        expr = tree_.add_expr(
            ast::Expr{ast::Cast_expr{
                .expression = expr,
                .target_type = target_type,
                .loc = loc,
            }});
    }
    return expr;
}

// unary -> ("!" | "-" | "~") cast | call
Result<ast::Expr_id> Parser::unary()
{
    if (match({lex::TokenType::LogicalNot, lex::TokenType::Minus, lex::TokenType::BitNot})) {
        lex::Token op = previous();
        auto right = __Try(cast());
        return tree_.add_expr(
            ast::Expr{ast::Unary_expr{
                .op = op.type,
                .right = right,
                .type = type_table_.get_unknown(),
                .loc = {op.line, op.column},
            }});
    }
    return call();
}

// call -> primary ( "(" args? ")" | "[" expr "]" | "." IDENT ("(" args? ")")? )*
Result<ast::Expr_id> Parser::call()
{
    auto expr = __Try(primary());

    while (true) {
        if (match({lex::TokenType::LeftParen})) {
            auto arguments = __Try(parse_call_arguments());
            auto paren = __Try(consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

            expr = tree_.add_expr(
                ast::Expr{ast::Call_expr{expr, arguments, type_table_.get_unknown(), ast::Source_location{paren.line, paren.column}}});

        } else if (match({lex::TokenType::LeftBracket})) {
            auto index_result = __Try(expression());
            auto right_bracket_result = __Try(consume(lex::TokenType::RightBracket, "Expect ']' after array index"));

            expr = tree_.add_expr(
                ast::Expr{ast::Array_access_expr{
                    expr,
                    index_result,
                    type_table_.get_unknown(),
                    ast::Source_location{right_bracket_result.line, right_bracket_result.column}}});

        } else if (match({lex::TokenType::Dot})) {
            auto name_result = __Try(consume(lex::TokenType::Identifier, "Expect member name after '.'"));

            if (match({lex::TokenType::LeftParen})) {
                auto arguments = __Try(parse_call_arguments());
                __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

                expr = tree_.add_expr(
                    ast::Expr{ast::Method_call_expr{
                        expr,
                        name_result.lexeme,
                        arguments,
                        type_table_.get_unknown(),
                        ast::Source_location{name_result.line, name_result.column}}});
            } else {
                expr = tree_.add_expr(
                    ast::Expr{ast::Field_access_expr{
                        expr,
                        name_result.lexeme,
                        type_table_.get_unknown(),
                        ast::Source_location{name_result.line, name_result.column}}});
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
Result<ast::Expr_id> Parser::primary()
{
    if (peek().type == lex::TokenType::Print) {
        return std::unexpected(create_error(peek(), "print is a statement, not an expression. Did you forget a semicolon?"));
    }

    // implicit enum member expression or anonymous model literal
    if (match({lex::TokenType::Dot})) {
        ast::Source_location loc{previous().line, previous().column};

        if (match({lex::TokenType::LeftBrace})) {
            std::vector<std::pair<std::string, ast::Expr_id>> fields;

            if (!check(lex::TokenType::RightBrace)) {
                do {
                    if (check(lex::TokenType::RightBrace)) {
                        break; // handle trailing comma gracefully
                    }

                    std::string field_name;
                    if (check(lex::TokenType::Identifier) && check_next(lex::TokenType::Colon)) {
                        field_name = advance().lexeme;
                        advance(); // Consume ':'
                    }
                    ast::Expr_id field_val = __Try(expression());

                    fields.push_back({field_name, field_val});
                } while (match({lex::TokenType::Comma}));
            }

            __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}' after anonymous model fields"));

            return tree_.add_expr(
                ast::Expr{ast::Anon_model_literal_expr{
                    .fields = std::move(fields),
                    .type = type_table_.get_unknown(),
                    .loc = loc,
                }});
        }

        auto member = __Try(consume(lex::TokenType::Identifier, "Expect enum variant name after '.'"));
        return tree_.add_expr(
            ast::Expr{ast::Enum_member_expr{
                .member_name = member.lexeme,
                .loc = loc,
                .type = type_table_.get_unknown(),
            }});
    }

    // concurrency
    if (match({lex::TokenType::Spawn})) {
        ast::Source_location loc{previous().line, previous().column};
        auto call_expr = __Try(expression());
        return tree_.add_expr(
            ast::Expr{ast::Spawn_expr{
                .call = call_expr,
                .type = type_table_.get_unknown(),
                .loc = loc,
            }});
    }

    if (match({lex::TokenType::Await})) {
        ast::Source_location loc{previous().line, previous().column};
        auto thread = __Try(expression());
        return tree_.add_expr(
            ast::Expr{ast::Await_expr{
                .thread = thread,
                .type = type_table_.get_unknown(),
                .loc = loc,
            }});
    }

    if (match({lex::TokenType::Yield})) {
        ast::Source_location loc{previous().line, previous().column};
        ast::Expr_id value = ast::Expr_id::null();
        if (!check(lex::TokenType::Semicolon) && !check(lex::TokenType::RightBrace)) {
            value = __Try(expression());
        }

        return tree_.add_expr(
            ast::Expr{ast::Yield_expr{
                .value = value,
                .type = type_table_.get_unknown(),
                .loc = loc,
            }});
    }

    // this
    if (match({lex::TokenType::This})) {
        if (current_model_.empty()) {
            return std::unexpected(create_error(previous(), "Cannot use 'this' outside of a model method"));
        }

        return tree_.add_expr(
            ast::Expr{ast::Variable_expr{
                .name = "this",
                .type = type_table_.get_unknown(),
                .loc = {previous().line, previous().column},
            }});
    }

    // closure
    if (match({lex::TokenType::Fn})) {
        return parse_closure_expression();
    }

    // array literal
    if (match({lex::TokenType::LeftBracket})) {
        return parse_array_literal();
    }

    // primitives
    if (match({lex::TokenType::Nil})) {
        ast::Expr_id expr = tree_.add_expr(ast::Expr{ast::Literal_expr{
            .value = Value(nullptr),
            .type = type_table_.get_nil(),
            .loc = {previous().line, previous().column},
        }});

        // 'nil' is an empty container, inherently Depth 1
        tree_.get(expr).auto_wrap_depth = 1;

        // Support nil?, nil??, nil???
        while (match({lex::TokenType::Question})) {
            tree_.get(expr).auto_wrap_depth++;
        }

        return expr;
    }

    if (match({lex::TokenType::Bool})) {
        return tree_.add_expr(
            ast::Expr{ast::Literal_expr{
                .value = previous().literal,
                .type = type_table_.get_bool(),
                .loc = {previous().line, previous().column},
            }});
    }

    auto is_numeric_literal = [](const lex::Token &t) {
        if (t.type == lex::TokenType::Integer32 || t.type == lex::TokenType::Float64) {
            return true;
        }

        bool is_sized_type = (
                t.type == lex::TokenType::TInt8    || t.type == lex::TokenType::TInt16  || t.type == lex::TokenType::TInt32
             || t.type == lex::TokenType::TInt64   || t.type == lex::TokenType::TUInt8  || t.type == lex::TokenType::TUInt16
             || t.type == lex::TokenType::TUInt32  || t.type == lex::TokenType::TUInt64 || t.type == lex::TokenType::TFloat16
             || t.type == lex::TokenType::TFloat32 || t.type == lex::TokenType::TFloat64
        );

        return is_sized_type && (t.literal.is_integer() || t.literal.is_float() || t.literal.is_u_integer());
    };

    if (is_numeric_literal(peek())) {
        lex::Token tok = advance();
        return tree_.add_expr(ast::Expr{ast::Literal_expr{
            .value = tok.literal,
            .type = map_token_to_type(tok.type, type_table_),
            .loc = {tok.line, tok.column},
        }});
    }

    if (match({lex::TokenType::String})) {
        return tree_.add_expr(
            ast::Expr{ast::Literal_expr{
                .value = previous().literal,
                .type = type_table_.get_string(),
                .loc = {previous().line, previous().column},
            }});
    }

    // identifier, primitive type namespaces (like string::), scope resolution, model literal
    if (match(
            {lex::TokenType::Identifier,
             lex::TokenType::TString,
             lex::TokenType::TInt8,
             lex::TokenType::TInt16,
             lex::TokenType::TInt32,
             lex::TokenType::TInt64,
             lex::TokenType::TUInt8,
             lex::TokenType::TUInt16,
             lex::TokenType::TUInt32,
             lex::TokenType::TUInt64,
             lex::TokenType::TFloat16,
             lex::TokenType::TFloat32,
             lex::TokenType::TFloat64,
             lex::TokenType::TBool})) {
        lex::Token id = previous();

        if (match({lex::TokenType::ColonColon})) {
            // Type::Member  or  Union::Variant or Enum::Variant
            auto member = __Try(consume(lex::TokenType::Identifier, "Expect member name after '::'"));
            auto base_var_expr = tree_.add_expr(ast::Expr{ast::Variable_expr{id.lexeme, type_table_.get_unknown(), {id.line, id.column}}});
            return tree_.add_expr(
                ast::Expr{ast::Static_path_expr{
                    .base = base_var_expr,
                    .member = member,
                    .type = type_table_.get_unknown(),
                    .loc = {member.line, member.column},
                }});
        }

        if (check(lex::TokenType::LeftBrace)) {
            return parse_model_literal(id.lexeme);
        }

        // Otherwise, it's just a standard variable expression!
        return tree_.add_expr(
            ast::Expr{ast::Variable_expr{
                .name = id.lexeme,
                .type = type_table_.get_unknown(),
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

// Specific Sub-Parsers
// closure -> "fn" "(" param* ")" ("->" type)? block
Result<ast::Expr_id> Parser::parse_closure_expression()
{
    size_t line = peek().line;
    size_t column = peek().column;

    std::vector<ast::Function_param> parameters;

    __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after 'fn' for closure parameters"));

    // params
    if (!check(lex::TokenType::RightParen)) {
        do {
            if (check(lex::TokenType::RightParen)) {
                break;
            }
            parameters.push_back(__Try(parse_function_parameter(false)));
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after closure parameters"));

    // return type
    types::Type_id return_type = type_table_.get_void();

    if (match({lex::TokenType::Arrow})) {
        return_type = __Try(parse_type());
    }

    // body
    __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' before closure body"));
    auto body = __Try(block_statement());

    std::vector<types::Type_id> param_types;
    param_types.reserve(parameters.size());

    for (const auto &param : parameters) {
        param_types.push_back(param.type);
    }

    types::Type_id closure_type = type_table_.function(param_types, return_type);

    return tree_.add_expr(ast::Expr{ast::Closure_expr{
        .parameters = parameters,
        .return_type = return_type,
        .body = body,
        .type = closure_type,
        .loc = {line, column},
    }});
}

// array_literal -> "[" (expr ("," expr)*)? "]"
Result<ast::Expr_id> Parser::parse_array_literal()
{
    size_t line = previous().line;
    size_t column = previous().column;

    std::vector<ast::Expr_id> elements;
    if (!check(lex::TokenType::RightBracket)) {
        do {
            if (check(lex::TokenType::RightBracket)) {
                break;
            }
            elements.push_back(__Try(expression()));
        } while (match({lex::TokenType::Comma}));
    }

    __TryIgnore(consume(lex::TokenType::RightBracket, "Expect ']' after array elements"));

    return tree_.add_expr(
        ast::Expr{ast::Array_literal_expr{
            .elements = elements,
            .type = type_table_.get_unknown(),
            .loc = {line, column},
        }});
}

// model_literal -> IDENT "{" ( (IDENT ":")? expr ("," (IDENT ":")? expr)* ","? )? "}"
Result<ast::Expr_id> Parser::parse_model_literal(const std::string &model_name)
{
    auto brace = __Try(consume(lex::TokenType::LeftBrace, "Expect '{' for model literal"));

    std::vector<std::pair<std::string, ast::Expr_id>> fields;
    if (!check(lex::TokenType::RightBrace)) {
        do {
            if (check(lex::TokenType::RightBrace)) {
                break; // trailing comma support
            }

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

    return tree_.add_expr(
        ast::Expr{ast::Model_literal_expr{
            .model_name = model_name,
            .fields = fields,
            .type = type_table_.get_unknown(),
            .loc = {brace.line, brace.column},
        }});
}

// =============================================================================
// Type Parser
// =============================================================================

// type        -> "(" type ")"
//              | "fn" "(" param_types? ")" "->" type    closure type
//              | "model" "{" (IDENT ":" type (";" IDENT ":" type)*)? ";"? "}"
//              | type_name type_suffix*
// type_suffix -> "[" "]"                                array type
//              | "?"                                    optional type
// type_name   -> primitive_kw | IDENT                   (defaults to unresolved)
Result<types::Type_id> Parser::parse_type()
{
    using namespace types;

    Type_id current;

    // grouped
    if (match({lex::TokenType::LeftParen})) {
        current = __Try(parse_type());
        __TryIgnore(consume(lex::TokenType::RightParen, "Expected ')' after type"));
    } else if (match({lex::TokenType::Fn})) {
        // function type
        std::vector<Type_id> params;

        __TryIgnore(consume(lex::TokenType::LeftParen, "Expect '(' after 'fn' for function type"));

        if (!check(lex::TokenType::RightParen)) {
            do {
                if (check(lex::TokenType::RightParen)) {
                    break;
                }
                params.push_back(__Try(parse_type()));
            } while (match({lex::TokenType::Comma}));
        }

        __TryIgnore(consume(lex::TokenType::RightParen, "Expect ')' after function type parameters"));
        __TryIgnore(consume(lex::TokenType::Arrow, "Expect '->' after function type parameters"));
        Type_id ret = __Try(parse_type());

        current = type_table_.function(params, ret);
    }
    // Primitives
    else if (match({lex::TokenType::TInt64})) {
        current = type_table_.get_i64();
    } else if (match({lex::TokenType::TInt32})) {
        current = type_table_.get_i32();
    } else if (match({lex::TokenType::TInt16})) {
        current = type_table_.get_i16();
    } else if (match({lex::TokenType::TInt8})) {
        current = type_table_.get_i8();
    } else if (match({lex::TokenType::TUInt64})) {
        current = type_table_.get_u64();
    } else if (match({lex::TokenType::TUInt32})) {
        current = type_table_.get_u32();
    } else if (match({lex::TokenType::TUInt16})) {
        current = type_table_.get_u16();
    } else if (match({lex::TokenType::TUInt8})) {
        current = type_table_.get_u8();
    } else if (match({lex::TokenType::TFloat64})) {
        current = type_table_.get_f64();
    } else if (match({lex::TokenType::TFloat32})) {
        current = type_table_.get_f32();
    } else if (match({lex::TokenType::TFloat16})) {
        current = type_table_.get_f16();
    } else if (match({lex::TokenType::TBool})) {
        current = type_table_.get_bool();
    } else if (match({lex::TokenType::TString})) {
        current = type_table_.get_string();
    } else if (match({lex::TokenType::TVoid})) {
        current = type_table_.get_void();
    } else if (match({lex::TokenType::TAny})) {
        current = type_table_.get_any();
    } else if (match({lex::TokenType::Model})) {

        std::vector<std::pair<std::string, Type_id>> fields;

        __TryIgnore(consume(lex::TokenType::LeftBrace, "Expect '{' after model"));

        while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
            auto name = __Try(consume(lex::TokenType::Identifier, "Expect field name"));
            __TryIgnore(consume(lex::TokenType::Colon, "Expect ':'"));

            auto ty = __Try(parse_type());
            fields.push_back({name.lexeme, ty});

            if (check(lex::TokenType::Semicolon) || check(lex::TokenType::Comma)) {
                advance();
            }
        }

        __TryIgnore(consume(lex::TokenType::RightBrace, "Expect '}'"));
        current = type_table_.model("", fields); // anonymous
    } else if (match({lex::TokenType::Identifier})) {
        std::string name = previous().lexeme;
        if (name == "any") {
            return std::unexpected(create_error(peek(), "Expected a type, 'any' is not one."));
        }
        // The parser doesn't attempt to resolve if this is a Model, Union, or Enum!
        // It simply creates an Unresolved_type placeholder. The Type Checker maps it later.
        current = type_table_.unresolved(name);
    } else {
        return std::unexpected(create_error(peek(), "Expected a type"));
    }

    // suffixes (Arrays and Optionals)
    while (true) {
        if (match({lex::TokenType::LeftBracket})) {
            __TryIgnore(consume(lex::TokenType::RightBracket, "Expected ']'"));
            current = type_table_.array(current);
        } else if (match({lex::TokenType::Question})) {
            current = type_table_.optional(current);
        } else {
            break;
        }
    }

    return current;
}

} // namespace phos
