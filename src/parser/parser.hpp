#pragma once

#include <string>
#include <utility>
#include <vector>
#include <unordered_map>
#include <memory>

#include "../lexer/token.hpp"
#include "../value/type.hpp"
#include "../error/err.hpp"
#include "../error/result.hpp"
#include "./ast.hpp"
#include "../memory/arena.hpp"

namespace phos
{

class Parser
{
    mem::Arena& arena_;
    std::vector<lex::Token> tokens_;
    size_t current_ = 0;
    std::unordered_map<std::string, types::Type> variable_types_;
    std::unordered_map<std::string, types::Type> function_types_;
    std::unordered_map<std::string, types::Type> model_types_;
    std::string current_model_;
    std::string stage_ = "parsing";

public:
    explicit Parser(std::vector<lex::Token> t, mem::Arena &arena) : tokens_(std::move(t)), arena_(arena) {}
    Result<std::vector<ast::Stmt*>> parse();

private:
    void skip_newlines();
    bool is_at_end() const { return current_ >= tokens_.size() || peek().type == lex::TokenType::Eof; }
    const lex::Token &peek() const;
    const lex::Token &previous() const;
    lex::Token advance();
    bool check(lex::TokenType type) const { return !is_at_end() && peek().type == type; }
    bool match(std::initializer_list<lex::TokenType> types);
    Result<lex::Token> consume(lex::TokenType type, const std::string &message);
    err::msg create_error(const lex::Token &token, const std::string &message);

    void synchronize();

    Result<std::optional<ast::Stmt*>> declaration();

    Result<ast::Stmt*> function_declaration();
    Result<ast::Stmt*> var_declaration();
    Result<ast::Stmt*> statement();
    Result<ast::Stmt*> print_statement(ast::Print_stream stream = ast::Print_stream::STDOUT);
    Result<ast::Stmt*> block_statement();
    Result<ast::Stmt*> if_statement();
    Result<ast::Stmt*> while_statement();
    Result<ast::Stmt*> for_statement();
    Result<ast::Stmt*> return_statement();
    Result<ast::Stmt*> expression_statement();
    Result<ast::Stmt*> model_declaration();

    Result<std::pair<std::string, types::Type>> parse_model_field();
    Result<ast::Function_stmt> parse_model_method();

    Result<ast::Expr*> expression();
    Result<ast::Expr*> assignment();
    Result<ast::Expr*> logical_or();
    Result<ast::Expr*> logical_and();
    Result<ast::Expr*> equality();
    Result<ast::Expr*> comparison();
    Result<ast::Expr*> term();
    Result<ast::Expr*> factor();
    Result<ast::Expr*> cast();
    Result<ast::Expr*> unary();
    Result<ast::Expr*> call();
    Result<ast::Expr*> primary();
    Result<ast::Expr*> parse_closure_expression();
    Result<ast::Expr*> parse_array_literal();
    Result<ast::Expr*> parse_model_literal(const std::string &model_name);

    Result<types::Type> parse_type();
};

}  // namespace phos
