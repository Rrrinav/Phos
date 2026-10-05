#include "parser.hpp"

#include "core/error/err.hpp"
#include "core/error/result.hpp"
#include "core/value/type.hpp"
#include "frontend/lexer/token.hpp"
#include "frontend/parser/ast.hpp"
#include "frontend/parser/parser_internal.hpp"

#include <cassert>
#include <cstddef>
#include <expected>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace phos {

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

            arguments.push_back(
                ast::Call_argument{
                    .name = name.lexeme,
                    .value = value,
                    .loc = {name.line, name.column},
                });
        } else {
            DECL_OR_RETURN(value, expression());

            arguments.push_back(
                ast::Call_argument{
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

    if (match({lex::TokenType::Fn, lex::TokenType::Proc})) {
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
        DECL_OR_RETURN(stmt, var_declaration(ast::Var_kind::Let));
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Const})) {
        DECL_OR_RETURN(stmt, var_declaration(ast::Var_kind::Const));
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Static})) {
        DECL_OR_RETURN(stmt, var_declaration(ast::Var_kind::Static));
        return std::optional<ast::Stmt_id>{stmt};
    }
    if (match({lex::TokenType::Import})) {
        DECL_OR_RETURN(stmt, import_statement());
        return std::optional<ast::Stmt_id>{stmt};
    }

    DECL_OR_RETURN(stmt, statement());
    return std::optional<ast::Stmt_id>{stmt};
}

// ret_param -> IDENT ":" type ("=" expr)? | type
Result<ast::Function_param> Parser::parse_return_parameter()
{
    // Check for "name :" pattern using lookahead
    if (check(lex::TokenType::Identifier) && check_next(lex::TokenType::Colon)) {
        // Case: Named return (e.g., -> x: i32 = 0)
        DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect return parameter name"));
        TRY_IGNORE(consume(lex::TokenType::Colon, "Expect ':' after return parameter name"));

        DECL_OR_RETURN(type, parse_type());

        ast::Expr_id default_val = ast::Expr_id::null();
        if (match({lex::TokenType::Assign})) {
            ASSIGN_OR_RETURN(default_val, expression());
        }

        return ast::Function_param{.name = name.lexeme, .type = type, .is_mut = true, .default_value = default_val, .loc = {name.line, name.column}};
    }

    // Case: Anonymous return (e.g., -> i32)
    lex::Token type_start = peek();
    DECL_OR_RETURN(type, parse_type());

    return ast::Function_param{
        .name = "",
        .type = type,
        .is_mut = false,
        .default_value = ast::Expr_id::null(),
        .loc = {type_start.line, type_start.column}};
}

// fn_decl -> ("fn" | "proc") IDENT "(" param* ")" ("->" ret_param ("," ret_param)*)? block
Result<ast::Stmt_id> Parser::function_declaration()
{
    // Purity is determined by the starting keyword: fn (pure) vs proc (impure)
    bool is_pure = previous().type == lex::TokenType::Fn;

    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect function name"));
    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after function name"));

    // 1. Parse Input Parameters (Names mandatory)
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

    // 2. Parse Return Parameters (Comma-separated list after '->')
    std::vector<ast::Function_param> returns;
    if (match({lex::TokenType::Arrow})) {
        do {
            DECL_OR_RETURN(ret_param, parse_return_parameter());
            returns.push_back(ret_param);
        } while (match({lex::TokenType::Comma}));
    }

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' before function body"));
    DECL_OR_RETURN(body, block_statement());

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::Function_stmt{
            .name = name.lexeme,
            .is_static = false,
            .is_pure = is_pure,
            .parameters = std::move(parameters),
            .returns = std::move(returns),
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

    ast::Stmt_id stmt_id = ctx_.tree.add_stmt(
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

        variants.push_back(
            ast::Typed_member_decl{
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

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::Union_stmt{
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

    return ctx_.tree.add_stmt(
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
    bool is_static = match({lex::TokenType::Static});
    DECL_OR_RETURN(name_result, consume(lex::TokenType::Identifier, "Expect field name"));
    TRY_IGNORE(consume(lex::TokenType::Colon, "Expect ':' after field name"));

    types::Type_id type_result = ctx_.tt.get_unknown();
    bool type_inferred = false;
    ast::Expr_id default_value = ast::Expr_id::null();

    if (match({lex::TokenType::Assign})) {
        type_inferred = true;
        ASSIGN_OR_RETURN(default_value, expression());
    } else {
        ASSIGN_OR_RETURN(type_result, parse_type());
        if (match({lex::TokenType::Assign})) {
            ASSIGN_OR_RETURN(default_value, expression());
        }
    }

    TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after field declaration"));

    return ast::Typed_member_decl{
        .name = name_result.lexeme,
        .type = type_result,
        .type_inferred = type_inferred,
        .is_static = is_static,
        .default_value = default_value,
        .loc = {name_result.line, name_result.column},
    };
}

// method_decl -> "static"? ("fn" | "proc") IDENT "(" param* ")" ("->" ret_param ("," ret_param)*)? block
Result<ast::Function_stmt> Parser::parse_model_method()
{
    bool is_static = match({lex::TokenType::Static});

    if (!match({lex::TokenType::Fn, lex::TokenType::Proc})) {
        return std::unexpected(create_error(peek(), "Expect 'fn' or 'proc' for method declaration"));
    }
    bool is_pure = previous().type == lex::TokenType::Fn;

    DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect method name"));
    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after method name"));

    // Input parameters
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

    // Multi-return list
    std::vector<ast::Function_param> returns;
    if (match({lex::TokenType::Arrow})) {
        do {
            DECL_OR_RETURN(ret_param, parse_return_parameter());
            returns.push_back(ret_param);
        } while (match({lex::TokenType::Comma}));
    }

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' before method body"));
    DECL_OR_RETURN(body_result, block_statement());

    return ast::Function_stmt{
        .name = name.lexeme,
        .is_static = is_static,
        .is_pure = is_pure,
        .parameters = std::move(parameters),
        .returns = std::move(returns),
        .body = body_result,
        .loc = {name.line, name.column},
    };
}

// var_decl    -> "let" "mut"? IDENT (":=" expr | ":" type ("=" expr)?) ";"
// const_decl  -> "const" IDENT (":=" expr | ":" type ("=" expr)?) ";"
// static_decl -> "static" "mut"? IDENT (":=" expr | ":" type ("=" expr)?) ";"
Result<ast::Stmt_id> Parser::var_declaration(ast::Var_kind kind)
{
    assert(kind == ast::Var_kind::Let || kind == ast::Var_kind::Static || kind == ast::Var_kind::Const);

    bool is_mut = false;
    if (kind != ast::Var_kind::Const) {
        is_mut = match({lex::TokenType::Mut});
    }

    std::vector<lex::Token> names;
    DECL_OR_RETURN(first_name, consume(lex::TokenType::Identifier, "Expect variable or constant name"));
    names.push_back(first_name);
    while (match({lex::TokenType::Comma})) {
        DECL_OR_RETURN(next_name, consume(lex::TokenType::Identifier, "Expect variable or constant name"));
        names.push_back(next_name);
    }

    std::vector<types::Type_id> declared_types;
    std::vector<ast::Expr_id> initializers;
    bool type_inferred = false;

    if (match({lex::TokenType::Colon})) {
        if (match({lex::TokenType::Assign})) {
            type_inferred = true;
            do {
                DECL_OR_RETURN(init_expr, expression());
                initializers.push_back(init_expr);
            } while (match({lex::TokenType::Comma}));
        } else {
            do {
                DECL_OR_RETURN(declared_type, parse_type());
                declared_types.push_back(declared_type);
            } while (match({lex::TokenType::Comma}));

            if (match({lex::TokenType::Assign})) {
                do {
                    DECL_OR_RETURN(init_expr, expression());
                    initializers.push_back(init_expr);
                } while (match({lex::TokenType::Comma}));
            } else if (kind == ast::Var_kind::Const) {
                return std::unexpected(create_error(peek(), "Constants must be initialized"));
            }
        }
    } else {
        return std::unexpected(create_error(peek(), "Expect ':' or ':=' after name"));
    }

    TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expected ';' after declaration"));

    ast::Var_kind var_kind{kind};
    if (is_mut) {
        if (kind == ast::Var_kind::Let) {
            var_kind = ast::Var_kind::Mut;
        } else {
            var_kind = ast::Var_kind::Static_mut;
        }
    }

    if (!type_inferred && declared_types.size() != names.size()) {
        return std::unexpected(create_error(first_name, "Variable count and type count must match in multi-variable declarations"));
    }
    if (initializers.size() > 1 && initializers.size() != names.size()) {
        return std::unexpected(
            create_error(first_name, "Variable count and initializer count must match unless using a single multi-value initializer"));
    }

    if (names.size() == 1) {
        return ctx_.tree.add_stmt(
            ast::Stmt{ast::Var_stmt{
                .kind = var_kind,
                .name = first_name.lexeme,
                .type = type_inferred ? ctx_.tt.get_unknown() : declared_types.front(),
                .initializer = initializers.empty() ? ast::Expr_id::null() : initializers.front(),
                .type_inferred = type_inferred,
                .loc = {first_name.line, first_name.column},
            }});
    }

    std::vector<std::string> var_names;
    var_names.reserve(names.size());
    for (const auto &name : names) {
        var_names.push_back(name.lexeme);
    }

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::Multi_var_stmt{
            .kind = var_kind,
            .names = std::move(var_names),
            .types = std::move(declared_types),
            .initializers = std::move(initializers),
            .type_inferred = type_inferred,
            .loc = {first_name.line, first_name.column},
            .resolved_symbols = {},
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
//            | match_stmt | return_stmt | expr_stmt | break_stmt
//            | continue_stmt | goto_stmt | defer_stmt | label_stmt
} // namespace phos
