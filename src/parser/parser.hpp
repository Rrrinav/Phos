#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "../error/err.hpp"
#include "../error/result.hpp"
#include "../lexer/token.hpp"
#include "../memory/arena.hpp"
#include "ast.hpp"

namespace phos
{

class Parser
{
public:
    explicit Parser(std::vector<lex::Token> tokens, mem::Arena& arena)
        : arena_(arena), tokens_(std::move(tokens)) {}

    Result<std::vector<ast::Stmt*>> parse();

private:
    mem::Arena&             arena_;
    std::vector<lex::Token> tokens_;
    size_t                  current_ = 0;
    std::string             stage_   = "parser";

    // set while parsing inside a model body so 'this' is valid
    std::string current_model_;

    // populated during parse so parse_type() can resolve union vs model names
    std::unordered_set<std::string> m_known_model_names;
    std::unordered_set<std::string> m_known_union_names;

    // return types of top-level functions, used downstream by the type checker
    std::unordered_map<std::string, types::Type> function_types_;

    void               skip_newlines();
    const lex::Token&  peek()     const;
    const lex::Token&  previous() const;
    lex::Token         advance();
    bool               match(std::initializer_list<lex::TokenType> types);
    bool               check(lex::TokenType type) const { return peek().type == type; }
    bool               is_at_end()               const { return peek().type == lex::TokenType::Eof; }
    Result<lex::Token> consume(lex::TokenType type, const std::string& message);
    err::msg           create_error(const lex::Token& token, const std::string& message);
    void               synchronize();

    Result<types::Type> parse_type();

    Result<std::optional<ast::Stmt*>>           declaration();
    Result<ast::Stmt*>                          function_declaration();
    Result<ast::Stmt*>                          model_declaration();
    Result<ast::Stmt*>                          union_declaration();
    Result<ast::Stmt*>                          var_declaration();
    Result<std::pair<std::string, types::Type>> parse_model_field();
    Result<ast::Function_stmt>                  parse_model_method();

    Result<ast::Stmt*> statement();
    Result<ast::Stmt*> print_statement(ast::Print_stream stream = ast::Print_stream::STDOUT);
    Result<ast::Stmt*> block_statement();
    Result<ast::Stmt*> if_statement();
    Result<ast::Stmt*> while_statement();
    Result<ast::Stmt*> for_statement();
    Result<ast::Stmt*> for_in_statement();
    Result<ast::Stmt*> match_statement();
    Result<ast::Stmt*> return_statement();
    Result<ast::Stmt*> expression_statement();

    // --- expressions (precedence: low -> high) ---

    Result<ast::Expr*> expression();
    Result<ast::Expr*> assignment();
    Result<ast::Expr*> range_expr();
    Result<ast::Expr*> logical_or();
    Result<ast::Expr*> logical_and();
    Result<ast::Expr*> bitwise_or();
    Result<ast::Expr*> bitwise_xor();
    Result<ast::Expr*> bitwise_and();
    Result<ast::Expr*> equality();
    Result<ast::Expr*> comparison();
    Result<ast::Expr*> bitwise_shift();
    Result<ast::Expr*> term();
    Result<ast::Expr*> factor();
    Result<ast::Expr*> cast();
    Result<ast::Expr*> unary();
    Result<ast::Expr*> call();
    Result<ast::Expr*> primary();

    // --- specific sub-parsers ---

    Result<ast::Expr*> parse_closure_expression();
    Result<ast::Expr*> parse_array_literal();
    Result<ast::Expr*> parse_model_literal(const std::string& model_name);
    Result<ast::Expr*> parse_fstring(const lex::Token& tok);
};

} // namespace phos
