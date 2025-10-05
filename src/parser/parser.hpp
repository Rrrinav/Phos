#pragma once

#include <vector>
#include <string>
#include <optional>
#include <utility>
#include <unordered_set>

#include "../lexer/token.hpp"
#include "../error/result.hpp"
#include "../error/err.hpp"
#include "ast.hpp"
#include "../memory/arena.hpp"

namespace phos
{

class Parser
{
public:
    explicit Parser(std::vector<lex::Token> tokens, mem::Arena& arena) : tokens_(std::move(tokens)), arena_(arena) {}

    Result<std::vector<ast::Stmt*>> parse();

private:
    mem::Arena& arena_;
    std::vector<lex::Token> tokens_;
    size_t current_ = 0;
    std::string stage_ = "parser";

    std::string current_model_;

    // These are used by your parser's parse_type function
    std::unordered_set<std::string> m_known_model_names;
    std::unordered_set<std::string> m_known_union_names;
    // This is used by your parser's function_declaration
    std::unordered_map<std::string, types::Type> function_types_;


    // --- Utility Methods ---
    void skip_newlines();
    const lex::Token &peek() const;
    const lex::Token &previous() const;
    lex::Token advance();
    bool match(std::initializer_list<lex::TokenType> types);
    Result<lex::Token> consume(lex::TokenType type, const std::string &message);
    bool check(lex::TokenType type) const { return peek().type == type; }
    bool is_at_end() const { return peek().type == lex::TokenType::Eof; }
    err::msg create_error(const lex::Token &token, const std::string &message);
    void synchronize();
    Result<types::Type> parse_type();

    // --- Declaration Parsers ---
    Result<std::optional<ast::Stmt*>> declaration();
    Result<ast::Stmt*> function_declaration();
    Result<ast::Stmt*> model_declaration();
    Result<ast::Stmt*> union_declaration();
    Result<ast::Stmt*> var_declaration();
    Result<std::pair<std::string, types::Type>> parse_model_field();
    Result<ast::Function_stmt> parse_model_method();

    // --- Statement Parsers ---
    Result<ast::Stmt*> statement();
    Result<ast::Stmt*> print_statement(ast::Print_stream stream = ast::Print_stream::STDOUT);
    Result<ast::Stmt*> block_statement();
    Result<ast::Stmt*> if_statement();
    Result<ast::Stmt*> while_statement();
    Result<ast::Stmt*> for_statement();
    Result<ast::Stmt*> return_statement();
    Result<ast::Stmt*> expression_statement();

    // --- Expression Parsers (by precedence) ---
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

    // --- Specific Expression Parsers ---
    Result<ast::Expr*> parse_closure_expression();
    Result<ast::Expr*> parse_array_literal();
    Result<ast::Expr*> parse_model_literal(const std::string& model_name);
};

} // namespace phos
