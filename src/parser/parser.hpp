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

    void skip_newlines();

    Result<std::vector<std::unique_ptr<ast::Stmt>>> parse();

private:
    bool is_at_end() const { return current >= tokens.size() || peek().type == lex::TokenType::Eof; }

    const lex::Token &peek() const;

    const lex::Token &previous() const;

    lex::Token advance();

    bool check(lex::TokenType type) const { return !is_at_end() && peek().type == type; }

    bool match(std::initializer_list<lex::TokenType> types);

    Result<lex::Token> consume(lex::TokenType type, const std::string &message);

    err::msg create_error(const lex::Token &token, const std::string &message);

    void synchronize();

    Result<std::optional<std::unique_ptr<ast::Stmt>>> declaration();
    Result<std::unique_ptr<ast::Stmt>>                function_declaration();
    Result<std::unique_ptr<ast::Stmt>>                model_declaration();
    Result<std::pair<std::string, types::Type>>       parse_model_field();
    Result<ast::Function_stmt>                        parse_model_method();
    Result<std::unique_ptr<ast::Stmt>>                var_declaration();
    Result<std::unique_ptr<ast::Stmt>>                statement();
    Result<std::unique_ptr<ast::Stmt>>                print_statement(ast::Print_stream stream = ast::Print_stream::STDOUT);
    Result<std::unique_ptr<ast::Stmt>>                block_statement();
    Result<std::unique_ptr<ast::Stmt>>                if_statement();
    Result<std::unique_ptr<ast::Stmt>>                while_statement();
    Result<std::unique_ptr<ast::Stmt>>                for_statement();
    Result<std::unique_ptr<ast::Stmt>>                return_statement();
    Result<std::unique_ptr<ast::Stmt>>                expression_statement();

    Result<std::unique_ptr<ast::Expr>> expression();
    Result<std::unique_ptr<ast::Expr>> assignment();
    Result<std::unique_ptr<ast::Expr>> logical_or();
    Result<std::unique_ptr<ast::Expr>> logical_and();
    Result<std::unique_ptr<ast::Expr>> equality();
    Result<std::unique_ptr<ast::Expr>> comparison();
    Result<std::unique_ptr<ast::Expr>> term();
    Result<std::unique_ptr<ast::Expr>> factor();
    Result<std::unique_ptr<ast::Expr>> cast();
    Result<std::unique_ptr<ast::Expr>> unary();
    Result<std::unique_ptr<ast::Expr>> call();
    Result<std::unique_ptr<ast::Expr>> primary();
    Result<std::unique_ptr<ast::Expr>> parse_closure_expression();
    Result<std::unique_ptr<ast::Expr>> parse_array_literal();
    Result<std::unique_ptr<ast::Expr>> parse_model_literal(const std::string &model_name);

    Result<types::Type> parse_type();
};

}  // namespace phos
