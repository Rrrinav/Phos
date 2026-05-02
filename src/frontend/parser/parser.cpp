#include "parser.hpp"

#include "core/error/err.hpp"
#include "core/error/result.hpp"
#include "core/value/type.hpp"
#include "frontend/lexer/token.hpp"
#include "frontend/parser/ast.hpp"

#include <cstddef>
#include <expected>
#include <print>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#define PHOS_PARSER_CONCAT_IMPL(x, y) x##y
#define PHOS_PARSER_CONCAT(x, y) PHOS_PARSER_CONCAT_IMPL(x, y)

#define ASSIGN_OR_RETURN(var, expr)                                                                                                                  \
    auto PHOS_PARSER_CONCAT(_res_, __LINE__) = (expr);                                                                                               \
    if (!PHOS_PARSER_CONCAT(_res_, __LINE__))                                                                                                        \
        return std::unexpected(PHOS_PARSER_CONCAT(_res_, __LINE__).error());                                                                         \
    var = std::move(PHOS_PARSER_CONCAT(_res_, __LINE__).value())

#define DECL_OR_RETURN(var, expr)                                                                                                                    \
    auto PHOS_PARSER_CONCAT(_res_, __LINE__) = (expr);                                                                                               \
    if (!PHOS_PARSER_CONCAT(_res_, __LINE__))                                                                                                        \
        return std::unexpected(PHOS_PARSER_CONCAT(_res_, __LINE__).error());                                                                         \
    auto var = std::move(PHOS_PARSER_CONCAT(_res_, __LINE__).value())

#define TRY_IGNORE(expr)                                                                                                                             \
    do {                                                                                                                                             \
        auto PHOS_PARSER_CONCAT(_res_, __LINE__) = (expr);                                                                                           \
        if (!PHOS_PARSER_CONCAT(_res_, __LINE__))                                                                                                    \
            return std::unexpected(PHOS_PARSER_CONCAT(_res_, __LINE__).error());                                                                     \
    } while (0)

namespace phos {

static types::Type_id map_token_to_type(lex::TokenType token_type, types::Type_table &tt)
{
    switch (token_type) {
    case lex::TokenType::TInt8:
        return tt.get_i8();
    case lex::TokenType::TInt16:
        return tt.get_i16();
    case lex::TokenType::Integer32:
    case lex::TokenType::TInt32:
        return tt.get_i32();
    case lex::TokenType::TInt64:
        return tt.get_i64();

    case lex::TokenType::TUInt8:
        return tt.get_u8();
    case lex::TokenType::TUInt16:
        return tt.get_u16();
    case lex::TokenType::TUInt32:
        return tt.get_u32();
    case lex::TokenType::TUInt64:
        return tt.get_u64();
    case lex::TokenType::TFloat16:
        return tt.get_f16();
    case lex::TokenType::TFloat32:
        return tt.get_f32();
    case lex::TokenType::Float64:
    case lex::TokenType::TFloat64:
        return tt.get_f64();

    case lex::TokenType::Bool:
        return tt.get_bool();
    case lex::TokenType::String:
        return tt.get_string();
    default:
        return tt.get_unknown();
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
Parser::Parse_result Parser::parse()
{
    diagnostics_ = err::Engine{stage_};
    Parse_result result;

    while (!is_at_end()) {
        skip_newlines();
        if (is_at_end()) {
            break;
        }

        auto decl_result = declaration();
        if (!decl_result) {
            if (!decl_result.error().summary.empty()) {
                diagnostics_.push(decl_result.error());
            }

            synchronize();
            continue;
        }

        if (decl_result.value()) {
            result.statements.push_back(*decl_result.value());
        }
    }

    if (diagnostics_.has_errors()) {
        diagnostics_.error(0, 0, source_name_, "Compilation halted due to syntax errors");
    }

    stamp_statement_locations(result.statements);
    result.diagnostics = diagnostics_;
    return result;
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

    auto &expr = ctx_.tree.get(expr_id).node;
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

    auto &stmt = ctx_.tree.get(stmt_id).node;
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
    DECL_OR_RETURN(param_name, consume(lex::TokenType::Identifier, "Expect parameter name"));
    TRY_IGNORE(consume(lex::TokenType::Colon, "Expect ':' after parameter name"));

    DECL_OR_RETURN(param_type, parse_type());

    ast::Expr_id default_value = ast::Expr_id::null();
    if (allow_default_value && match({lex::TokenType::Assign})) {
        ASSIGN_OR_RETURN(default_value, expression());
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
            TRY_IGNORE(consume(lex::TokenType::Assign, "Expect '=' after argument name"));
            DECL_OR_RETURN(value, expression());

            arguments.push_back(ast::Call_argument{
                .name = name.lexeme,
                .value = value,
                .loc = {name.line, name.column},
            });
        } else {
            DECL_OR_RETURN(value, expression());

            arguments.push_back(ast::Call_argument{
                .name = "",
                .value = value,
                .loc = ast::get_loc(ctx_.tree.get(value).node),
            });
        }
    } while (match({lex::TokenType::Comma}));

    return arguments;
}

// declaration -> fn_decl | model_decl | union_decl | enum_decl | var_decl | statement | import_stmt
Result<std::optional<ast::Stmt_id>> Parser::declaration()
{
    skip_newlines();
    if (is_at_end()) {
        return std::optional<ast::Stmt_id>{std::nullopt};
    }

    if (match({lex::TokenType::Fn})) {
        DECL_OR_RETURN(stmt, function_declaration());
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Model})) {
        DECL_OR_RETURN(stmt, model_declaration());
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Bind})) {
        TRY_IGNORE(parse_bind_statement());
        return std::optional<ast::Stmt_id>{std::nullopt};
    }
    if (match({lex::TokenType::Union})) {
        DECL_OR_RETURN(stmt, union_declaration());
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Enum})) {
        DECL_OR_RETURN(stmt, enum_declaration());
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Let})) {
        DECL_OR_RETURN(stmt, var_declaration(false));
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Const})) {
        DECL_OR_RETURN(stmt, var_declaration(true));
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Import})) {
        DECL_OR_RETURN(stmt, import_statement());
        return std::optional<ast::Stmt_id>{stmt};
    }

    DECL_OR_RETURN(stmt, statement());
    return std::optional<ast::Stmt_id>{stmt};
}

// fn_decl -> "fn" IDENT "(" param* ")" ("->" type)? block
Result<ast::Stmt_id> Parser::function_declaration()
{
    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect function name"));
    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after function name"));

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen)) {
        do {
            if (check(lex::TokenType::RightParen)) {
                break;
            }
            DECL_OR_RETURN(param, parse_function_parameter(true));
            parameters.push_back(param);
        } while (match({lex::TokenType::Comma}));
    }

    TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after parameters"));

    types::Type_id return_type{};

    if (match({lex::TokenType::Arrow})) {
        ASSIGN_OR_RETURN(return_type, parse_type());
    }

    function_types_.emplace(name.lexeme, return_type);

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' before function body"));
    DECL_OR_RETURN(body, block_statement());

    return ctx_.tree.add_stmt(ast::Stmt{ast::Function_stmt{
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
    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expected model name"));
    current_model_ = name.lexeme;

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after model name"));

    std::vector<ast::Typed_member_decl> fields;
    std::vector<ast::Stmt_id> methods;

    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        DECL_OR_RETURN(field, parse_model_field());
        fields.push_back(field);
        skip_newlines();
    }

    TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '};' after model body"));
    current_model_.clear();

    ast::Stmt_id stmt_id = ctx_.tree.add_stmt(ast::Stmt{ast::Model_stmt{
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
    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expected model name to bind to"));

    if (!parsed_models.count(name.lexeme)) {
        return std::unexpected(create_error(name, "Cannot bind methods to undeclared model '" + name.lexeme + "'. Please declare the model first"));
    }

    ast::Stmt_id target_model_id = parsed_models[name.lexeme];
    current_model_ = name.lexeme;

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' before bind body"));

    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        DECL_OR_RETURN(method_ast, parse_model_method());
        ast::Stmt_id method_id = ctx_.tree.add_stmt(ast::Stmt{method_ast});

        // Re-fetch pointer AFTER adding to tree to prevent vector reallocation invalidation!
        auto *target_model = std::get_if<ast::Model_stmt>(&ctx_.tree.get(target_model_id).node);
        target_model->methods.push_back(method_id);

        skip_newlines();
    }

    TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}' after bind body"));
    current_model_.clear();

    return ast::Stmt_id::null(); // Bind statement doesn't create a new standalone block
}

// union_decl -> "union" IDENT "{" (IDENT ":" type ";")* "}"
Result<ast::Stmt_id> Parser::union_declaration()
{
    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expected union name"));
    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after union name"));

    std::vector<ast::Typed_member_decl> variants;
    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        DECL_OR_RETURN(variant_name, consume(lex::TokenType::Identifier, "Expect variant name"));
        TRY_IGNORE(consume(lex::TokenType::Colon, "Expect ':' after variant name for type"));

        DECL_OR_RETURN(variant_type, parse_type());
        ast::Expr_id default_value = ast::Expr_id::null();

        if (match({lex::TokenType::Assign})) {
            ASSIGN_OR_RETURN(default_value, expression());
        }

        variants.push_back(ast::Typed_member_decl{
            .name = variant_name.lexeme,
            .type = variant_type,
            .default_value = default_value,
            .loc = {variant_name.line, variant_name.column},
        });

        if (!check(lex::TokenType::RightBrace)) {
            TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after variant declaration"));
        }

        skip_newlines();
    }

    TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}' after union body"));

    return ctx_.tree.add_stmt(ast::Stmt{ast::Union_stmt{
        .name = name.lexeme,
        .variants = variants,
        .loc = {name.line, name.column},
    }});
}

// enum_decl -> "enum" IDENT (":" type)? "{" (IDENT ("=" INT | STRING)? ","?)* "}"
Result<ast::Stmt_id> Parser::enum_declaration()
{
    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expected enum name"));
    types::Type_id base_type{};

    if (match({lex::TokenType::Colon})) {
        ASSIGN_OR_RETURN(base_type, parse_type());
        if (!(ctx_.tt.is_primitive(base_type) && (ctx_.tt.is_integer_primitive(base_type) || ctx_.tt.is_string(base_type)))) {
            return std::unexpected(create_error(previous(), "Enum base type must be an integer type or 'string'"));
        }
    } else {
        // NEW: Default to a 64-bit integer if no base type is explicitly provided!
        base_type = ctx_.tt.get_i64();
    }

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after enum name"));

    std::vector<std::pair<std::string, std::optional<Value>>> variants;
    while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
        skip_newlines();
        if (check(lex::TokenType::RightBrace)) {
            break;
        }

        DECL_OR_RETURN(variant_name, consume(lex::TokenType::Identifier, "Expect enum variant name"));
        std::optional<Value> value = std::nullopt;

        if (match({lex::TokenType::Assign})) {
            if (ctx_.tt.is_primitive(base_type) && ctx_.tt.is_integer_primitive(base_type)) {
                DECL_OR_RETURN(val_tok, consume(lex::TokenType::Integer32, "Expect integer value after '=' in integer enum"));
                auto coerced = coerce_numeric_literal(val_tok.literal, ctx_.tt.get_primitive(base_type));

                if (!coerced) {
                    return std::unexpected(create_error(val_tok, "Enum variant value does not fit the enum base type"));
                }

                value = coerced.value();
            } else {
                DECL_OR_RETURN(val_tok, consume(lex::TokenType::String, "Expect string value after '=' in string enum"));
                value = val_tok.literal;
            }
        }

        variants.push_back({variant_name.lexeme, value});

        match({lex::TokenType::Comma}); // Optional trailing comma
        skip_newlines();
    }

    TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}' after enum body"));

    return ctx_.tree.add_stmt(ast::Stmt{ast::Enum_stmt{
        .name = name.lexeme,
        .base_type = base_type,
        .variants = variants,
        .loc = {name.line, name.column},
    }});
}

// field_decl -> IDENT ":" type ";"
Result<ast::Typed_member_decl> Parser::parse_model_field()
{
    bool is_static = match({lex::TokenType::Static});
    DECL_OR_RETURN(name_result, consume(lex::TokenType::Identifier, "Expect field name"));
    TRY_IGNORE(consume(lex::TokenType::Colon, "Expect ':' after field name"));
    DECL_OR_RETURN(type_result, parse_type());

    ast::Expr_id default_value = ast::Expr_id::null();
    if (match({lex::TokenType::Assign})) {
        ASSIGN_OR_RETURN(default_value, expression());
    }

    TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after field declaration"));

    return ast::Typed_member_decl{
        .name = name_result.lexeme,
        .type = type_result,
        .is_static = is_static,
        .default_value = default_value,
        .loc = {name_result.line, name_result.column},
    };
}

// method_decl -> "static"? "fn" IDENT "(" param* ")" ("->" type)? block
Result<ast::Function_stmt> Parser::parse_model_method()
{
    bool is_static = match({lex::TokenType::Static});

    // We consume 'fn' here now so the parser correctly expects it inside the bind block
    TRY_IGNORE(consume(lex::TokenType::Fn, is_static ? "Expect 'fn' after 'static'" : "Expect 'fn' keyword for method"));

    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect method name"));
    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after method name"));

    std::vector<ast::Function_param> parameters;
    if (!check(lex::TokenType::RightParen)) {
        do {
            if (check(lex::TokenType::RightParen)) {
                break;
            }
            DECL_OR_RETURN(param, parse_function_parameter(true));
            parameters.push_back(param);
        } while (match({lex::TokenType::Comma}));
    }

    TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after parameters"));

    types::Type_id return_type = ctx_.tt.get_void();
    if (match({lex::TokenType::Arrow})) {
        ASSIGN_OR_RETURN(return_type, parse_type());
    }

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' before method body"));
    DECL_OR_RETURN(body_result, block_statement());

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

    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect variable or constant name"));

    types::Type_id var_type{};
    ast::Expr_id initializer = ast::Expr_id::null();
    bool type_inferred = false;

    if (match({lex::TokenType::Colon})) {
        if (match({lex::TokenType::Assign})) {
            // let x := expr  OR  const x := expr
            ASSIGN_OR_RETURN(initializer, expression());
            type_inferred = true;
        } else {
            // let x: T = expr  OR  const x: T = expr
            ASSIGN_OR_RETURN(var_type, parse_type());

            if (match({lex::TokenType::Assign})) {
                ASSIGN_OR_RETURN(initializer, expression());
            } else if (is_const) {
                // Semantic enforcement: Constants must have an initializer
                return std::unexpected(create_error(peek(), "Constants must be initialized"));
            }
        }
    } else {
        return std::unexpected(create_error(peek(), "Expect ':' or ':=' after name"));
    }

    TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expected ';' after declaration"));

    return ctx_.tree.add_stmt(ast::Stmt{ast::Var_stmt{
        .is_mut = is_mut,
        .is_const = is_const,
        .name = name.lexeme,
        .type = var_type,
        .initializer = initializer,
        .type_inferred = type_inferred,
        .loc = {name.line, name.column},
    }});
}

// import_stmt       ::= "import" module_path ( "::" symbol_extraction )? ";"
// module_path       ::= IDENTIFIER ( "." IDENTIFIER )*
// symbol_extraction ::= IDENTIFIER | "{" IDENTIFIER ( "," IDENTIFIER )* "}"
Result<ast::Stmt_id> Parser::import_statement()
{
    // Use the location of the "import" keyword we just passed
    size_t start_line = previous().line;
    size_t start_column = previous().column;
    ast::Source_location loc{start_line, start_column, source_name_};

    ast::Import_stmt stmt;
    stmt.loc = loc;

    // 1. Parse the path segments (e.g. std.math)
    do {
        auto path_res = consume(lex::TokenType::Identifier, "Expected module path identifier.");
        if (!path_res) {
            return std::unexpected(path_res.error());
        }
        stmt.path.push_back(path_res.value().lexeme);
    } while (match({lex::TokenType::Dot}));

    // 2. Parse selective imports (e.g. ::sin or ::{sin, cos})
    if (match({lex::TokenType::ColonColon})) {
        if (match({lex::TokenType::LeftBrace})) {
            if (!check(lex::TokenType::RightBrace)) {
                do {
                    auto sym_res = consume(lex::TokenType::Identifier, "Expected symbol name.");
                    if (!sym_res) {
                        return std::unexpected(sym_res.error());
                    }
                    stmt.selectives.push_back(sym_res.value().lexeme);
                } while (match({lex::TokenType::Comma}));
            }

            auto rbrace_res = consume(lex::TokenType::RightBrace, "Expected '}' after selectives.");
            if (!rbrace_res) {
                return std::unexpected(rbrace_res.error());
            }
        } else {
            auto sym_res = consume(lex::TokenType::Identifier, "Expected imported symbol name after '::'.");
            if (!sym_res) {
                return std::unexpected(sym_res.error());
            }
            stmt.selectives.push_back(sym_res.value().lexeme);
        }
    }

    // 3. Parse local alias (e.g. as m)
    if (match({lex::TokenType::As})) {
        auto alias_res = consume(lex::TokenType::Identifier, "Expected alias identifier.");
        if (!alias_res) {
            return std::unexpected(alias_res.error());
        }
        stmt.local_alias = alias_res.value().lexeme;
    }

    auto semi_res = consume(lex::TokenType::Semicolon, "Expected ';' after import statement.");
    if (!semi_res) {
        return std::unexpected(semi_res.error());
    }

    return ctx_.tree.add_stmt(ast::Stmt(std::move(stmt)));
}

// Statements

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
    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after 'print'"));

    std::vector<ast::Expr_id> expressions;
    std::string sep = "";
    std::string end = "\n";

    // Empty print() -> just print the end character (newline by default)
    if (check(lex::TokenType::RightParen)) {
        advance();
        TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after print statement"));
        return ctx_.tree.add_stmt(ast::Stmt{ast::Print_stmt{
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

                DECL_OR_RETURN(val_result, expression());

                auto *lit = std::get_if<ast::Literal_expr>(&ctx_.tree.get(val_result).node);
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

                DECL_OR_RETURN(expr_result, expression());
                expressions.push_back(expr_result);
            }
        } else {
            if (seen_named) {
                return std::unexpected(create_error(peek(), "Positional arguments cannot appear after named arguments in print()"));
            }

            DECL_OR_RETURN(expr_result, expression());
            expressions.push_back(expr_result);
        }
    } while (match({lex::TokenType::Comma}));

    TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after print arguments"));
    TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after print statement"));

    return ctx_.tree.add_stmt(ast::Stmt{ast::Print_stmt{
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

    TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}' after block"));

    return ctx_.tree.add_stmt(ast::Stmt{ast::Block_stmt{
        .statements = statements,
        .loc = {line, column},
    }});
}

// if_stmt -> "if" expr "{" declaration* "}" ("else" (if_stmt | "{" declaration* "}"))?
Result<ast::Stmt_id> Parser::if_statement()
{
    DECL_OR_RETURN(condition_result, expression());

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after if condition"));
    DECL_OR_RETURN(then_branch_result, block_statement());

    ast::Stmt_id else_branch = ast::Stmt_id::null();
    if (match({lex::TokenType::Else})) {
        if (match({lex::TokenType::If})) {
            else_branch = if_statement().value_or(ast::Stmt_id::null());
        } else {
            TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after 'else'"));
            else_branch = block_statement().value_or(ast::Stmt_id::null());
        }
    }

    return ctx_.tree.add_stmt(ast::Stmt{ast::If_stmt{
        .condition = condition_result,
        .then_branch = then_branch_result,
        .else_branch = else_branch,
        .loc = {previous().line, previous().column},
    }});
}

// while_stmt -> "while" expr "{" declaration* "}"
Result<ast::Stmt_id> Parser::while_statement()
{
    DECL_OR_RETURN(condition_result, expression());

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after while condition"));
    DECL_OR_RETURN(body_result, block_statement());

    return ctx_.tree.add_stmt(ast::Stmt{ast::While_stmt{
        .condition = condition_result,
        .body = body_result,
        .loc = {previous().line, previous().column},
    }});
}

// for_stmt -> "for" "(" (var_decl | const_decl | expr_stmt | ";") expr? ";" expr? ")" block
Result<ast::Stmt_id> Parser::for_statement()
{
    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after 'for'"));

    ast::Stmt_id initializer = ast::Stmt_id::null();
    ast::Expr_id condition = ast::Expr_id::null();
    ast::Expr_id increment = ast::Expr_id::null();

    if (match({lex::TokenType::Semicolon})) {
        // empty initializer, do nothing
    } else if (match({lex::TokenType::Let})) {
        initializer = var_declaration(false).value_or(ast::Stmt_id::null());
    } else if (match({lex::TokenType::Const})) {
        initializer = var_declaration(true).value_or(ast::Stmt_id::null());
    } else {
        initializer = expression_statement().value_or(ast::Stmt_id::null());
    }

    if (!check(lex::TokenType::Semicolon)) {
        condition = expression().value_or(ast::Expr_id::null());
    }
    TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after loop condition"));

    if (!check(lex::TokenType::RightParen)) {
        increment = expression().value_or(ast::Expr_id::null());
    }
    TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after for clauses"));

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after for clauses"));
    DECL_OR_RETURN(body, block_statement());

    return ctx_.tree.add_stmt(ast::Stmt{ast::For_stmt{initializer, condition, increment, body, {previous().line, previous().column}}});
}

// for_in_stmt
Result<ast::Stmt_id> Parser::for_in_statement()
{
    DECL_OR_RETURN(var_name, consume(lex::TokenType::Identifier, "Expect loop variable name after 'for'"));
    TRY_IGNORE(consume(lex::TokenType::In, "Expect 'in' after loop variable"));

    DECL_OR_RETURN(iterable, expression());

    // FORCE BLOCK HERE
    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after for-in iterable"));
    DECL_OR_RETURN(body, block_statement());

    return ctx_.tree.add_stmt(ast::Stmt{ast::For_in_stmt{
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
    DECL_OR_RETURN(subject, expression());

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after match subject"));

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
            ASSIGN_OR_RETURN(arm.pattern, expression());

            // If the user typed `Result::Ok(data)`, the expression parser
            // naturally consumed it as a Call_expr. We unpack that into the
            // actual pattern + the binding name!
            if (auto *call_node = std::get_if<ast::Call_expr>(&ctx_.tree.get(arm.pattern).node)) {
                // 1. The actual pattern is just the callee (e.g., Result::Ok)
                arm.pattern = call_node->callee;

                // 2. Validate and extract the payload binding name
                if (call_node->arguments.size() != 1) {
                    return std::unexpected(create_error(previous(), "Match union payloads can only bind exactly one variable"));
                }

                if (auto *var_node = std::get_if<ast::Variable_expr>(&ctx_.tree.get(call_node->arguments[0].value).node)) {
                    arm.bind_name = var_node->name;
                } else {
                    return std::unexpected(create_error(previous(), "Match binding payload must be a simple identifier"));
                }
            }
        }

        // 5. Fat Arrow
        TRY_IGNORE(consume(lex::TokenType::FatArrow, "Expect '=>' after match pattern"));

        ASSIGN_OR_RETURN(arm.body, statement());
        arms.push_back(std::move(arm));

        match({lex::TokenType::Comma});
        skip_newlines();
    }

    TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}' after match arms"));

    return ctx_.tree.add_stmt(ast::Stmt{ast::Match_stmt{
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
        value = expression().value_or(ast::Expr_id::null());
    }

    TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after return value"));

    return ctx_.tree.add_stmt(ast::Stmt{ast::Return_stmt{value, {line, column}}});
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

    return ctx_.tree.add_stmt(ast::Stmt{ast::Expr_stmt{expr_result.value(), {previous().line, previous().column}}});
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
    DECL_OR_RETURN(expr, range_expr());

    if (match({lex::TokenType::Assign})) {
        lex::Token equals = previous();
        DECL_OR_RETURN(value_result, assignment());

        // Vector invalidation safety: MUST get the reference after assignment() returns!
        auto &expr_ref = ctx_.tree.get(expr);

        if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr_ref.node)) {
            return ctx_.tree.add_expr(ast::Expr{ast::Assignment_expr{var_expr->name, value_result, var_expr->type, {equals.line, equals.column}}});
        } else if (auto *field_access_expr = std::get_if<ast::Field_access_expr>(&expr_ref.node)) {
            return ctx_.tree.add_expr(ast::Expr{ast::Field_assignment_expr{
                .object = field_access_expr->object,
                .field_name = field_access_expr->field_name,
                .value = value_result,
                .type = ctx_.tt.get_unknown(),
                .loc = {equals.line, equals.column},
            }});
        } else if (auto *array_access_expr = std::get_if<ast::Array_access_expr>(&expr_ref.node)) {
            return ctx_.tree.add_expr(ast::Expr{ast::Array_assignment_expr{
                .array = array_access_expr->array,
                .index = array_access_expr->index,
                .value = value_result,
                .type = ctx_.tt.get_unknown(),
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
    DECL_OR_RETURN(expr, logical_or());

    if (match({lex::TokenType::DotDot, lex::TokenType::DotDotEq})) {
        bool inclusive = (previous().type == lex::TokenType::DotDotEq);
        lex::Token op = previous();
        DECL_OR_RETURN(end_expr, logical_or());

        return ctx_.tree.add_expr(ast::Expr{ast::Range_expr{
            .start = expr,
            .end = end_expr,
            .inclusive = inclusive,
            .type = ctx_.tt.get_unknown(),
            .loc = {op.line, op.column},
        }});
    }

    return expr;
}

// logical_or -> logical_and ("||" logical_and)*
Result<ast::Expr_id> Parser::logical_or()
{
    DECL_OR_RETURN(expr, logical_and());

    while (match({lex::TokenType::LogicalOr})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, logical_and());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_bool(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// logical_and -> bitwise_or ("&&" bitwise_or)*
Result<ast::Expr_id> Parser::logical_and()
{
    DECL_OR_RETURN(expr, bitwise_or());

    while (match({lex::TokenType::LogicalAnd})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, bitwise_or());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_bool(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// bitwise_or -> bitwise_xor ("|" bitwise_xor)*
Result<ast::Expr_id> Parser::bitwise_or()
{
    DECL_OR_RETURN(expr, bitwise_xor());

    while (match({lex::TokenType::Pipe})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, bitwise_xor());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_bool(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// bitwise_xor -> bitwise_and ("^" bitwise_and)*
Result<ast::Expr_id> Parser::bitwise_xor()
{
    DECL_OR_RETURN(expr, bitwise_and());

    while (match({lex::TokenType::BitXor})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, bitwise_and());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_bool(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// bitwise_and -> equality ("&" equality)*
Result<ast::Expr_id> Parser::bitwise_and()
{
    DECL_OR_RETURN(expr, equality());

    while (match({lex::TokenType::BitAnd})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, equality());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_unknown(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// equality -> comparison (("==" | "!=") comparison)*
Result<ast::Expr_id> Parser::equality()
{
    DECL_OR_RETURN(expr, comparison());

    while (match({lex::TokenType::Equal, lex::TokenType::NotEqual})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, comparison());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_bool(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// comparison -> bitwise_shift (("<" | "<=" | ">" | ">=") bitwise_shift)*
Result<ast::Expr_id> Parser::comparison()
{
    DECL_OR_RETURN(expr, bitwise_shift());

    while (match({lex::TokenType::Less, lex::TokenType::LessEqual, lex::TokenType::Greater, lex::TokenType::GreaterEqual})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, bitwise_shift());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_bool(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// bitwise_shift -> term (("<<" | ">>") term)*
Result<ast::Expr_id> Parser::bitwise_shift()
{
    DECL_OR_RETURN(expr, term());

    while (match({lex::TokenType::BitLShift, lex::TokenType::BitRshift})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, term());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_unknown(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// term -> factor (("+" | "-") factor)*
Result<ast::Expr_id> Parser::term()
{
    DECL_OR_RETURN(expr, factor());

    while (match({lex::TokenType::Plus, lex::TokenType::Minus})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, factor());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_unknown(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// factor -> cast (("*" | "/" | "%") cast)*
Result<ast::Expr_id> Parser::factor()
{
    DECL_OR_RETURN(expr, cast());

    while (match({lex::TokenType::Star, lex::TokenType::Slash, lex::TokenType::Percent})) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, cast());
        expr = ctx_.tree.add_expr(ast::Expr{ast::Binary_expr{
            .left = expr,
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_unknown(),
            .loc = {op.line, op.column},
        }});
    }
    return expr;
}

// cast -> unary ("as" type)*
Result<ast::Expr_id> Parser::cast()
{
    DECL_OR_RETURN(expr, unary());

    while (match({lex::TokenType::As})) {
        DECL_OR_RETURN(target_type, parse_type());
        auto loc = ast::get_loc(ctx_.tree.get(expr).node);
        expr = ctx_.tree.add_expr(ast::Expr{ast::Cast_expr{
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
        DECL_OR_RETURN(right, cast());
        return ctx_.tree.add_expr(ast::Expr{ast::Unary_expr{
            .op = op.type,
            .right = right,
            .type = ctx_.tt.get_unknown(),
            .loc = {op.line, op.column},
        }});
    }
    return call();
}

// call -> primary ( "(" args? ")" | "[" expr "]" | "." IDENT ("(" args? ")")? | "::" IDENT )*
Result<ast::Expr_id> Parser::call()
{
    DECL_OR_RETURN(expr, primary());

    while (true) {
        if (match({lex::TokenType::LeftParen})) {
            DECL_OR_RETURN(arguments, parse_call_arguments());
            DECL_OR_RETURN(paren, consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

            expr =
                ctx_.tree.add_expr(ast::Expr{ast::Call_expr{expr, arguments, ctx_.tt.get_unknown(), ast::Source_location{paren.line, paren.column}}});

        } else if (match({lex::TokenType::LeftBracket})) {
            DECL_OR_RETURN(index_result, expression());
            DECL_OR_RETURN(right_bracket_result, consume(lex::TokenType::RightBracket, "Expect ']' after array index"));

            expr = ctx_.tree.add_expr(ast::Expr{ast::Array_access_expr{
                expr,
                index_result,
                ctx_.tt.get_unknown(),
                ast::Source_location{right_bracket_result.line, right_bracket_result.column}}});

        } else if (match({lex::TokenType::ColonColon})) {
            DECL_OR_RETURN(member_result, consume(lex::TokenType::Identifier, "Expect member name after '::'"));

            expr = ctx_.tree.add_expr(ast::Expr{ast::Static_path_expr{
                .base = expr,
                .member = member_result,
                .type = ctx_.tt.get_unknown(),
                .loc = {member_result.line, member_result.column},
            }});

        } else if (match({lex::TokenType::Dot})) {
            DECL_OR_RETURN(name_result, consume(lex::TokenType::Identifier, "Expect member name after '.'"));

            if (match({lex::TokenType::LeftParen})) {
                DECL_OR_RETURN(arguments, parse_call_arguments());
                TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

                expr = ctx_.tree.add_expr(ast::Expr{ast::Method_call_expr{
                    expr,
                    name_result.lexeme,
                    arguments,
                    ctx_.tt.get_unknown(),
                    ast::Source_location{name_result.line, name_result.column}}});
            } else {
                expr = ctx_.tree.add_expr(ast::Expr{ast::Field_access_expr{
                    expr,
                    name_result.lexeme,
                    ctx_.tt.get_unknown(),
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
                    DECL_OR_RETURN(field_val, expression());

                    fields.push_back({field_name, field_val});
                } while (match({lex::TokenType::Comma}));
            }

            TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}' after anonymous model fields"));

            return ctx_.tree.add_expr(ast::Expr{ast::Anon_model_literal_expr{
                .fields = std::move(fields),
                .type = ctx_.tt.get_unknown(),
                .loc = loc,
            }});
        }

        DECL_OR_RETURN(member, consume(lex::TokenType::Identifier, "Expect enum variant name after '.'"));
        return ctx_.tree.add_expr(ast::Expr{ast::Enum_member_expr{
            .member_name = member.lexeme,
            .loc = loc,
            .type = ctx_.tt.get_unknown(),
        }});
    }

    // concurrency
    if (match({lex::TokenType::Spawn})) {
        ast::Source_location loc{previous().line, previous().column};
        DECL_OR_RETURN(call_expr, expression());
        return ctx_.tree.add_expr(ast::Expr{ast::Spawn_expr{
            .call = call_expr,
            .type = ctx_.tt.get_unknown(),
            .loc = loc,
        }});
    }

    if (match({lex::TokenType::Await})) {
        ast::Source_location loc{previous().line, previous().column};
        DECL_OR_RETURN(thread, expression());
        return ctx_.tree.add_expr(ast::Expr{ast::Await_expr{
            .thread = thread,
            .type = ctx_.tt.get_unknown(),
            .loc = loc,
        }});
    }

    if (match({lex::TokenType::Yield})) {
        ast::Source_location loc{previous().line, previous().column};
        ast::Expr_id value = ast::Expr_id::null();
        if (!check(lex::TokenType::Semicolon) && !check(lex::TokenType::RightBrace)) {
            ASSIGN_OR_RETURN(value, expression());
        }

        return ctx_.tree.add_expr(ast::Expr{ast::Yield_expr{
            .value = value,
            .type = ctx_.tt.get_unknown(),
            .loc = loc,
        }});
    }

    // this
    if (match({lex::TokenType::This})) {
        if (current_model_.empty()) {
            return std::unexpected(create_error(previous(), "Cannot use 'this' outside of a model method"));
        }

        return ctx_.tree.add_expr(ast::Expr{ast::Variable_expr{
            .name = "this",
            .type = ctx_.tt.get_unknown(),
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
        ast::Expr_id expr = ctx_.tree.add_expr(ast::Expr{ast::Literal_expr{
            .value = Value(nullptr),
            .type = ctx_.tt.get_nil(),
            .loc = {previous().line, previous().column},
        }});

        // 'nil' is an empty container, inherently Depth 1
        ctx_.tree.get(expr).auto_wrap_depth = 1;

        // Support nil?, nil??, nil???
        while (match({lex::TokenType::Question})) {
            ctx_.tree.get(expr).auto_wrap_depth++;
        }

        return expr;
    }

    if (match({lex::TokenType::Bool})) {
        return ctx_.tree.add_expr(ast::Expr{ast::Literal_expr{
            .value = previous().literal,
            .type = ctx_.tt.get_bool(),
            .loc = {previous().line, previous().column},
        }});
    }

    auto is_numeric_literal = [](const lex::Token &t) {
        if (t.type == lex::TokenType::Integer32 || t.type == lex::TokenType::Float64) {
            return true;
        }

        bool is_sized_type =
            (t.type == lex::TokenType::TInt8 || t.type == lex::TokenType::TInt16 || t.type == lex::TokenType::TInt32
             || t.type == lex::TokenType::TInt64 || t.type == lex::TokenType::TUInt8 || t.type == lex::TokenType::TUInt16
             || t.type == lex::TokenType::TUInt32 || t.type == lex::TokenType::TUInt64 || t.type == lex::TokenType::TFloat16
             || t.type == lex::TokenType::TFloat32 || t.type == lex::TokenType::TFloat64);

        return is_sized_type && (t.literal.is_integer() || t.literal.is_float() || t.literal.is_u_integer());
    };

    if (is_numeric_literal(peek())) {
        lex::Token tok = advance();
        return ctx_.tree.add_expr(ast::Expr{ast::Literal_expr{
            .value = tok.literal,
            .type = map_token_to_type(tok.type, ctx_.tt),
            .loc = {tok.line, tok.column},
        }});
    }

    if (match({lex::TokenType::String})) {
        return ctx_.tree.add_expr(ast::Expr{ast::Literal_expr{
            .value = previous().literal,
            .type = ctx_.tt.get_string(),
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
            DECL_OR_RETURN(member, consume(lex::TokenType::Identifier, "Expect member name after '::'"));
            auto base_var_expr = ctx_.tree.add_expr(ast::Expr{ast::Variable_expr{id.lexeme, ctx_.tt.get_unknown(), {id.line, id.column}}});
            return ctx_.tree.add_expr(ast::Expr{ast::Static_path_expr{
                .base = base_var_expr,
                .member = member,
                .type = ctx_.tt.get_unknown(),
                .loc = {member.line, member.column},
            }});
        }

        if (check(lex::TokenType::Dot) && current_ + 1 < tokens_.size() && tokens_[current_ + 1].type == lex::TokenType::LeftBrace) {
            advance();
            return parse_model_literal(id.lexeme);
        }

        // Otherwise, it's just a standard variable expression!
        return ctx_.tree.add_expr(ast::Expr{ast::Variable_expr{
            .name = id.lexeme,
            .type = ctx_.tt.get_unknown(),
            .loc = {id.line, id.column},
        }});
    }

    // grouped expression
    if (match({lex::TokenType::LeftParen})) {
        DECL_OR_RETURN(expr_result, expression());
        TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after expression"));
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

    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after 'fn' for closure parameters"));

    // params
    if (!check(lex::TokenType::RightParen)) {
        do {
            if (check(lex::TokenType::RightParen)) {
                break;
            }
            DECL_OR_RETURN(param, parse_function_parameter(false));
            parameters.push_back(param);
        } while (match({lex::TokenType::Comma}));
    }

    TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after closure parameters"));

    // return type
    types::Type_id return_type = ctx_.tt.get_void();

    if (match({lex::TokenType::Arrow})) {
        ASSIGN_OR_RETURN(return_type, parse_type());
    }

    // body
    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' before closure body"));
    DECL_OR_RETURN(body, block_statement());

    std::vector<types::Type_id> param_types;
    param_types.reserve(parameters.size());

    for (const auto &param : parameters) {
        param_types.push_back(param.type);
    }

    types::Type_id closure_type = ctx_.tt.function(param_types, return_type);

    return ctx_.tree.add_expr(ast::Expr{ast::Closure_expr{
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
            DECL_OR_RETURN(elem, expression());
            elements.push_back(elem);
        } while (match({lex::TokenType::Comma}));
    }

    TRY_IGNORE(consume(lex::TokenType::RightBracket, "Expect ']' after array elements"));

    return ctx_.tree.add_expr(ast::Expr{ast::Array_literal_expr{
        .elements = elements,
        .type = ctx_.tt.get_unknown(),
        .loc = {line, column},
    }});
}

// model_literal -> IDENT "{" ( (IDENT ":")? expr ("," (IDENT ":")? expr)* ","? )? "}"
Result<ast::Expr_id> Parser::parse_model_literal(const std::string &model_name)
{
    DECL_OR_RETURN(brace, consume(lex::TokenType::LeftBrace, "Expect '{' for model literal"));

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

            DECL_OR_RETURN(value, expression());
            fields.push_back({field_name, value});
        } while (match({lex::TokenType::Comma}));
    }

    TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}' after model fields"));

    return ctx_.tree.add_expr(ast::Expr{ast::Model_literal_expr{
        .model_name = model_name,
        .fields = fields,
        .type = ctx_.tt.get_unknown(),
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
        ASSIGN_OR_RETURN(current, parse_type());
        TRY_IGNORE(consume(lex::TokenType::RightParen, "Expected ')' after type"));
    } else if (match({lex::TokenType::Fn})) {
        // function type
        std::vector<Type_id> params;

        TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after 'fn' for function type"));

        if (!check(lex::TokenType::RightParen)) {
            do {
                if (check(lex::TokenType::RightParen)) {
                    break;
                }
                DECL_OR_RETURN(param_type, parse_type());
                params.push_back(param_type);
            } while (match({lex::TokenType::Comma}));
        }

        TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after function type parameters"));
        TRY_IGNORE(consume(lex::TokenType::Arrow, "Expect '->' after function type parameters"));
        DECL_OR_RETURN(ret, parse_type());

        current = ctx_.tt.function(params, ret);
    }
    // Primitives
    else if (match({lex::TokenType::TInt64})) {
        current = ctx_.tt.get_i64();
    } else if (match({lex::TokenType::TInt32})) {
        current = ctx_.tt.get_i32();
    } else if (match({lex::TokenType::TInt16})) {
        current = ctx_.tt.get_i16();
    } else if (match({lex::TokenType::TInt8})) {
        current = ctx_.tt.get_i8();
    } else if (match({lex::TokenType::TUInt64})) {
        current = ctx_.tt.get_u64();
    } else if (match({lex::TokenType::TUInt32})) {
        current = ctx_.tt.get_u32();
    } else if (match({lex::TokenType::TUInt16})) {
        current = ctx_.tt.get_u16();
    } else if (match({lex::TokenType::TUInt8})) {
        current = ctx_.tt.get_u8();
    } else if (match({lex::TokenType::TFloat64})) {
        current = ctx_.tt.get_f64();
    } else if (match({lex::TokenType::TFloat32})) {
        current = ctx_.tt.get_f32();
    } else if (match({lex::TokenType::TFloat16})) {
        current = ctx_.tt.get_f16();
    } else if (match({lex::TokenType::TBool})) {
        current = ctx_.tt.get_bool();
    } else if (match({lex::TokenType::TString})) {
        current = ctx_.tt.get_string();
    } else if (match({lex::TokenType::TVoid})) {
        current = ctx_.tt.get_void();
    } else if (match({lex::TokenType::TAny})) {
        current = ctx_.tt.get_any();
    } else if (match({lex::TokenType::Model})) {

        std::vector<std::pair<std::string, Type_id>> fields;

        TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after model"));

        while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
            DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect field name"));
            TRY_IGNORE(consume(lex::TokenType::Colon, "Expect ':'"));

            DECL_OR_RETURN(ty, parse_type());
            fields.push_back({name.lexeme, ty});

            if (check(lex::TokenType::Semicolon) || check(lex::TokenType::Comma)) {
                advance();
            }
        }

        TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}'"));
        current = ctx_.tt.model("", fields); // anonymous
    } else if (match({lex::TokenType::Identifier})) {
        std::string name = previous().lexeme;
        if (name == "any") {
            return std::unexpected(create_error(peek(), "Expected a type, 'any' is not one."));
        }
        // The parser doesn't attempt to resolve if this is a Model, Union, or Enum!
        // It simply creates an Unresolved_type placeholder. The Type Checker maps it later.
        current = ctx_.tt.unresolved(name);
    } else {
        return std::unexpected(create_error(peek(), "Expected a type"));
    }

    // suffixes (Arrays and Optionals)
    while (true) {
        if (match({lex::TokenType::LeftBracket})) {
            TRY_IGNORE(consume(lex::TokenType::RightBracket, "Expected ']'"));
            current = ctx_.tt.array(current);
        } else if (match({lex::TokenType::Question})) {
            current = ctx_.tt.optional(current);
        } else {
            break;
        }
    }

    return current;
}

} // namespace phos

#undef TRY_IGNORE
#undef DECL_OR_RETURN
#undef ASSIGN_OR_RETURN
#undef PHOS_PARSER_CONCAT
#undef PHOS_PARSER_CONCAT_IMPL
