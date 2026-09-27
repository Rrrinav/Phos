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

Result<ast::Stmt_id> Parser::statement()
{
    skip_newlines();
    if (is_at_end()) {
        return std::unexpected(create_error(peek(), "Unexpected end of file"));
    }

    // 1. Handle Labels and Loop Prefixes
    if (match({lex::TokenType::At})) {
        ast::Source_location loc{previous().line, previous().column, source_name_};
        DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect label or attribute name after '@'."));

        if (match({lex::TokenType::Colon})) {
            // It is a Label! Check if a loop follows it immediately
            if (match({lex::TokenType::While})) {
                return while_statement(name.lexeme);
            }
            if (match({lex::TokenType::For})) {
                if (check(lex::TokenType::LeftParen)) {
                    return for_statement(name.lexeme);
                }
                return for_in_statement(name.lexeme);
            }

            // Otherwise, it's a standalone label (for goto)
            return ctx_.tree.add_stmt(ast::Stmt{ast::Label_stmt{.name = name.lexeme, .loc = loc}});
        } else {
            return std::unexpected(create_error(previous(), "Attributes not yet implemented."));
        }
    }

    // 2. Control Flow: Break, Continue, Goto, Defer
    if (match({lex::TokenType::Break})) {
        ast::Source_location loc{previous().line, previous().column, source_name_};
        std::string target = "";
        if (match({lex::TokenType::At})) {
            DECL_OR_RETURN(label_name, consume(lex::TokenType::Identifier, "Expect label name after '@'."));
            target = label_name.lexeme;
        }
        TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after break."));
        return ctx_.tree.add_stmt(ast::Stmt{ast::Break_stmt{.target_label = target, .loc = loc}});
    }

    if (match({lex::TokenType::Continue})) {
        ast::Source_location loc{previous().line, previous().column, source_name_};
        std::string target = "";
        if (match({lex::TokenType::At})) {
            DECL_OR_RETURN(label_name, consume(lex::TokenType::Identifier, "Expect label name after '@'."));
            target = label_name.lexeme;
        }
        TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after continue."));
        return ctx_.tree.add_stmt(ast::Stmt{ast::Continue_stmt{.target_label = target, .loc = loc}});
    }

    if (match({lex::TokenType::Goto})) {
        ast::Source_location loc{previous().line, previous().column, source_name_};
        TRY_IGNORE(consume(lex::TokenType::At, "Expect '@' before goto label."));
        DECL_OR_RETURN(label_name, consume(lex::TokenType::Identifier, "Expect label name."));
        TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after goto statement."));
        return ctx_.tree.add_stmt(ast::Stmt{ast::Goto_stmt{.target_label = label_name.lexeme, .loc = loc}});
    }

    if (match({lex::TokenType::Defer})) {
        ast::Source_location loc{previous().line, previous().column, source_name_};
        ASSIGN_OR_RETURN(auto deferred_stmt, statement());
        return ctx_.tree.add_stmt(ast::Stmt{ast::Defer_stmt{.call = deferred_stmt, .is_function_scoped = true, .loc = loc}});
    }

    if (match({lex::TokenType::Defer_local})) {
        ast::Source_location loc{previous().line, previous().column, source_name_};
        ASSIGN_OR_RETURN(auto deferred_stmt, statement());
        return ctx_.tree.add_stmt(ast::Stmt{ast::Defer_stmt{.call = deferred_stmt, .is_function_scoped = false, .loc = loc}});
    }

    // 3. Existing Statements
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
        return while_statement("");
    }
    if (match({lex::TokenType::Match})) {
        return match_statement();
    }
    if (match({lex::TokenType::Return})) {
        return return_statement();
    }

    if (match({lex::TokenType::For})) {
        if (check(lex::TokenType::LeftParen)) {
            return for_statement("");
        }
        return for_in_statement("");
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
        return ctx_.tree.add_stmt(
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

    return ctx_.tree.add_stmt(
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

    TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}' after block"));

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::Block_stmt{
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

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::If_stmt{
            .condition = condition_result,
            .then_branch = then_branch_result,
            .else_branch = else_branch,
            .loc = {previous().line, previous().column},
        }});
}

// while_stmt -> "while" expr "{" declaration* "}"
Result<ast::Stmt_id> Parser::while_statement(const std::string &label)
{
    DECL_OR_RETURN(condition_result, expression());

    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after while condition"));
    DECL_OR_RETURN(body_result, block_statement());

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::While_stmt{
            .label = label,
            .condition = condition_result,
            .body = body_result,
            .loc = {previous().line, previous().column},
        }});
}

// for_stmt -> "for" "(" (var_decl | const_decl | expr_stmt | ";") expr? ";" expr? ")" block
Result<ast::Stmt_id> Parser::for_statement(const std::string &label)
{
    TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after 'for'"));

    ast::Stmt_id initializer = ast::Stmt_id::null();
    ast::Expr_id condition = ast::Expr_id::null();
    ast::Expr_id increment = ast::Expr_id::null();

    if (match({lex::TokenType::Semicolon})) {
    } else if (match({lex::TokenType::Let})) {
        initializer = var_declaration(ast::Var_kind::Let).value_or(ast::Stmt_id::null());
    } else if (match({lex::TokenType::Const})) {
        initializer = var_declaration(ast::Var_kind::Const).value_or(ast::Stmt_id::null());
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

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::For_stmt{
            .label = label,
            .initializer = initializer,
            .condition = condition,
            .increment = increment,
            .body = body,
            .loc = {previous().line, previous().column}}});
}

// for_in_stmt
Result<ast::Stmt_id> Parser::for_in_statement(const std::string &label)
{
    DECL_OR_RETURN(var_name, consume(lex::TokenType::Identifier, "Expect loop variable name after 'for'"));
    TRY_IGNORE(consume(lex::TokenType::In, "Expect 'in' after loop variable"));

    DECL_OR_RETURN(iterable, expression());

    // FORCE BLOCK HERE
    TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after for-in iterable"));
    DECL_OR_RETURN(body, block_statement());

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::For_in_stmt{
            .label = label,
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

    return ctx_.tree.add_stmt(
        ast::Stmt{ast::Match_stmt{
            .subject = subject,
            .arms = std::move(arms),
            .loc = loc,
        }});
}

// return_stmt -> "return" (expr ("," expr)*)? ";"
Result<ast::Stmt_id> Parser::return_statement()
{
    lex::Token keyword = previous();
    std::vector<ast::Expr_id> values;

    if (!check(lex::TokenType::Semicolon)) {
        do {
            DECL_OR_RETURN(expr, expression());
            values.push_back(expr);
        } while (match({lex::TokenType::Comma}));
    }

    TRY_IGNORE(consume(lex::TokenType::Semicolon, "Expect ';' after return value"));

    return ctx_.tree.add_stmt(ast::Stmt{ast::Return_stmt{.expressions = std::move(values), .loc = {keyword.line, keyword.column}}});
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

} // namespace phos
