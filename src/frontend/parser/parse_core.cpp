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
                for (auto &e : s.expressions) {
                    stamp_expr(e);
                }
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
            } else if constexpr (std::is_same_v<T, ast::Multi_var_stmt>) {
                for (auto expr : s.initializers) {
                    stamp_expr(expr);
                }
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
} // namespace phos
