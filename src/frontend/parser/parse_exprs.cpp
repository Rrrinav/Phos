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
            return ctx_.tree.add_expr(
                ast::Expr{ast::Field_assignment_expr{
                    .object = field_access_expr->object,
                    .field_name = field_access_expr->field_name,
                    .value = value_result,
                    .type = ctx_.tt.get_unknown(),
                    .loc = {equals.line, equals.column},
                }});
        } else if (auto *array_access_expr = std::get_if<ast::Array_access_expr>(&expr_ref.node)) {
            return ctx_.tree.add_expr(
                ast::Expr{ast::Array_assignment_expr{
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

        return ctx_.tree.add_expr(
            ast::Expr{ast::Range_expr{
                .start = expr,
                .end = end_expr,
                .inclusive = inclusive,
                .type = ctx_.tt.get_unknown(),
                .loc = {op.line, op.column},
            }});
    }

    return expr;
}

Result<ast::Expr_id> Parser::parse_left_assoc(
    Result<ast::Expr_id> (Parser::*next)(), std::initializer_list<lex::TokenType> ops, bool bool_result)
{
    DECL_OR_RETURN(expr, (this->*next)());

    while (match(ops)) {
        lex::Token op = previous();
        DECL_OR_RETURN(right, (this->*next)());
        expr = ctx_.tree.add_expr(
            ast::Expr{ast::Binary_expr{
                .left = expr,
                .op = op.type,
                .right = right,
                .type = bool_result ? ctx_.tt.get_bool() : ctx_.tt.get_unknown(),
                .loc = {op.line, op.column},
            }});
    }
    return expr;
}

// logical_or -> logical_and ("||" logical_and)*
Result<ast::Expr_id> Parser::logical_or()
{
    return parse_left_assoc(&Parser::logical_and, {lex::TokenType::LogicalOr}, true);
}

// logical_and -> bitwise_or ("&&" bitwise_or)*
Result<ast::Expr_id> Parser::logical_and()
{
    return parse_left_assoc(&Parser::bitwise_or, {lex::TokenType::LogicalAnd}, true);
}

// bitwise_or -> bitwise_xor ("|" bitwise_xor)*
Result<ast::Expr_id> Parser::bitwise_or()
{
    return parse_left_assoc(&Parser::bitwise_xor, {lex::TokenType::Pipe}, true);
}

// bitwise_xor -> bitwise_and ("^" bitwise_and)*
Result<ast::Expr_id> Parser::bitwise_xor()
{
    return parse_left_assoc(&Parser::bitwise_and, {lex::TokenType::BitXor}, true);
}

// bitwise_and -> equality ("&" equality)*
Result<ast::Expr_id> Parser::bitwise_and()
{
    return parse_left_assoc(&Parser::equality, {lex::TokenType::BitAnd}, false);
}

// equality -> comparison (("==" | "!=") comparison)*
Result<ast::Expr_id> Parser::equality()
{
    return parse_left_assoc(&Parser::comparison, {lex::TokenType::Equal, lex::TokenType::NotEqual}, true);
}

// comparison -> bitwise_shift (("<" | "<=" | ">" | ">=") bitwise_shift)*
Result<ast::Expr_id> Parser::comparison()
{
    return parse_left_assoc(
        &Parser::bitwise_shift, {lex::TokenType::Less, lex::TokenType::LessEqual, lex::TokenType::Greater, lex::TokenType::GreaterEqual}, true);
}

// bitwise_shift -> term (("<<" | ">>") term)*
Result<ast::Expr_id> Parser::bitwise_shift()
{
    return parse_left_assoc(&Parser::term, {lex::TokenType::BitLShift, lex::TokenType::BitRshift}, false);
}

// term -> factor (("+" | "-") factor)*
Result<ast::Expr_id> Parser::term()
{
    return parse_left_assoc(&Parser::factor, {lex::TokenType::Plus, lex::TokenType::Minus}, false);
}

// factor -> cast (("*" | "/" | "%") cast)*
Result<ast::Expr_id> Parser::factor()
{
    return parse_left_assoc(&Parser::cast, {lex::TokenType::Star, lex::TokenType::Slash, lex::TokenType::Percent}, false);
}

// cast -> unary (("as" | "sat") type)*
Result<ast::Expr_id> Parser::cast()
{
    DECL_OR_RETURN(expr, unary());

    while (match({lex::TokenType::As, lex::TokenType::Sat})) {
        bool is_saturating = previous().type == lex::TokenType::Sat;
        DECL_OR_RETURN(target_type, parse_type());
        auto loc = ast::get_loc(ctx_.tree.get(expr).node);
        expr = ctx_.tree.add_expr(
            ast::Expr{ast::Cast_expr{
                .expression = expr,
                .target_type = target_type,
                .loc = loc,
                .is_saturating = is_saturating,
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
        return ctx_.tree.add_expr(
            ast::Expr{ast::Unary_expr{
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

            expr = ctx_.tree.add_expr(
                ast::Expr{ast::Array_access_expr{
                    expr,
                    index_result,
                    ctx_.tt.get_unknown(),
                    ast::Source_location{right_bracket_result.line, right_bracket_result.column}}});

        } else if (match({lex::TokenType::ColonColon})) {
            DECL_OR_RETURN(member_result, consume(lex::TokenType::Identifier, "Expect member name after '::'"));

            expr = ctx_.tree.add_expr(
                ast::Expr{ast::Static_path_expr{
                    .base = expr,
                    .member = member_result,
                    .type = ctx_.tt.get_unknown(),
                    .loc = {member_result.line, member_result.column},
                }});

        } else if (match({lex::TokenType::Dot})) {
            // NEW: Intercept Model Literal construction precisely!
            if (check(lex::TokenType::LeftBrace)) {
                // A safe recursive lambda to build string representations out of arbitrarily deep static paths.
                auto get_path_string = [&](ast::Expr_id id) -> std::string {
                    auto helper = [&](auto &self, ast::Expr_id curr_id) -> std::string {
                        const auto &node = ctx_.tree.get(curr_id).node;
                        if (const auto *var = std::get_if<ast::Variable_expr>(&node)) {
                            return var->name;
                        }
                        if (const auto *path = std::get_if<ast::Static_path_expr>(&node)) {
                            std::string base_str = self(self, path->base);
                            if (!base_str.empty()) {
                                return base_str + "::" + path->member.lexeme;
                            }
                        }
                        return "";
                    };
                    return helper(helper, id);
                };

                std::string path_name = get_path_string(expr);
                if (path_name.empty()) {
                    return std::unexpected(create_error(previous(), "Invalid left-hand side for model literal instantiation. Expected a type path."));
                }

                DECL_OR_RETURN(model_lit, parse_model_literal(path_name));
                expr = model_lit;
                continue;
            }

            DECL_OR_RETURN(name_result, consume(lex::TokenType::Identifier, "Expect member name after '.'"));

            if (match({lex::TokenType::LeftParen})) {
                DECL_OR_RETURN(arguments, parse_call_arguments());
                TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after arguments"));

                expr = ctx_.tree.add_expr(
                    ast::Expr{ast::Method_call_expr{
                        expr,
                        name_result.lexeme,
                        arguments,
                        ctx_.tt.get_unknown(),
                        ast::Source_location{name_result.line, name_result.column}}});
            } else {
                expr = ctx_.tree.add_expr(
                    ast::Expr{ast::Field_access_expr{
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

            return ctx_.tree.add_expr(
                ast::Expr{ast::Anon_model_literal_expr{
                    .fields = std::move(fields),
                    .type = ctx_.tt.get_unknown(),
                    .loc = loc,
                }});
        }

        DECL_OR_RETURN(member, consume(lex::TokenType::Identifier, "Expect enum variant name after '.'"));
        return ctx_.tree.add_expr(
            ast::Expr{ast::Enum_member_expr{
                .member_name = member.lexeme,
                .loc = loc,
                .type = ctx_.tt.get_unknown(),
            }});
    }

    // concurrency
    if (match({lex::TokenType::Spawn})) {
        ast::Source_location loc{previous().line, previous().column};
        DECL_OR_RETURN(call_expr, expression());
        return ctx_.tree.add_expr(
            ast::Expr{ast::Spawn_expr{
                .call = call_expr,
                .type = ctx_.tt.get_unknown(),
                .loc = loc,
            }});
    }

    if (match({lex::TokenType::Await})) {
        ast::Source_location loc{previous().line, previous().column};
        DECL_OR_RETURN(thread, expression());
        return ctx_.tree.add_expr(
            ast::Expr{ast::Await_expr{
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

        return ctx_.tree.add_expr(
            ast::Expr{ast::Yield_expr{
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

        return ctx_.tree.add_expr(
            ast::Expr{ast::Variable_expr{
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
        ast::Expr_id expr = ctx_.tree.add_expr(
            ast::Expr{ast::Literal_expr{
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
        return ctx_.tree.add_expr(
            ast::Expr{ast::Literal_expr{
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
        return ctx_.tree.add_expr(
            ast::Expr{ast::Literal_expr{
                .value = tok.literal,
                .type = map_token_to_type(tok.type, ctx_.tt),
                .loc = {tok.line, tok.column},
            }});
    }

    if (match({lex::TokenType::String})) {
        return ctx_.tree.add_expr(
            ast::Expr{ast::Literal_expr{
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
            DECL_OR_RETURN(member, consume(lex::TokenType::Identifier, "Expect type name after '::'"));
            auto base_var_expr = ctx_.tree.add_expr(ast::Expr{ast::Variable_expr{id.lexeme, ctx_.tt.get_unknown(), {id.line, id.column}}});
            return ctx_.tree.add_expr(
                ast::Expr{ast::Static_path_expr{
                    .base = base_var_expr,
                    .member = member,
                    .type = ctx_.tt.get_unknown(),
                    .loc = {member.line, member.column},
                }});
        }

        // Otherwise, it's just a standard variable expression!
        return ctx_.tree.add_expr(
            ast::Expr{ast::Variable_expr{
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

// closure_expr -> ("fn") "(" param* ")" ("->" ret_param ("," ret_param)*)? block
Result<ast::Expr_id> Parser::parse_closure_expression()
{
    size_t line = previous().line;
    size_t column = previous().column;

    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after closure keyword"));

    // 1. Parameters
    std::vector<ast::Function_param> parameters;
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

    // 2. Returns (Multi-return support)
    std::vector<ast::Function_param> returns;
    if (match({lex::TokenType::Arrow})) {
        do {
            DECL_OR_RETURN(ret_param, parse_return_parameter());
            returns.push_back(ret_param);
        } while (match({lex::TokenType::Comma}));
    }

    // 3. Body
    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' before closure body"));
    DECL_OR_RETURN(body, block_statement());

    return ctx_.tree.add_expr(
        ast::Expr{ast::Closure_expr{
            .parameters = std::move(parameters),
            .returns = std::move(returns),
            .body = body,
            .type = ctx_.tt.get_unknown(),
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

    return ctx_.tree.add_expr(
        ast::Expr{ast::Array_literal_expr{
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

    return ctx_.tree.add_expr(
        ast::Expr{ast::Model_literal_expr{
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
} // namespace phos
