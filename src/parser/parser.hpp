#pragma once

#include "../error/err.hpp"
#include "../error/result.hpp"
#include "../lexer/token.hpp"
#include "ast.hpp"
#include "../memory/arena.hpp"

#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace phos {

class Parser
{
public:
    struct Parse_result
    {
        std::vector<ast::Stmt_id> statements;
        err::Engine diagnostics{"parser"};
    };

    explicit Parser(
        std::vector<lex::Token> tokens,
        phos::types::Type_table &type_table,
        ast::Ast_tree &tree,
        phos::mem::Arena &arena,
        std::string source_name = "<input>")
        : tree_(tree), type_table_(type_table), tokens_(std::move(tokens)), arena_(arena), source_name_(std::move(source_name))
    {}

    Parse_result parse();

private:
    ast::Ast_tree &tree_;
    phos::types::Type_table &type_table_;
    std::vector<lex::Token> tokens_;
    phos::mem::Arena& arena_;
    std::string source_name_;
    size_t current_ = 0;
    std::string stage_ = "parser";
    err::Engine diagnostics_{"parser"};

    // set while parsing inside a model body so 'this' is valid
    std::string current_model_;

    // return types of top-level functions, used downstream by the type checker
    std::unordered_map<std::string, types::Type_id> function_types_;

    // Memory of parsed models so the 'bind' block can find them (Data-Oriented!)
    std::unordered_map<std::string, ast::Stmt_id> parsed_models;

    void skip_newlines();
    const lex::Token &peek() const;
    const lex::Token &previous() const;
    lex::Token advance();
    bool match(std::initializer_list<lex::TokenType> types);
    bool check(lex::TokenType type) const
    {
        return peek().type == type;
    }
    bool check_next(lex::TokenType type) const;
    bool is_at_end() const
    {
        return peek().type == lex::TokenType::Eof;
    }
    Result<lex::Token> consume(lex::TokenType type, const std::string &message);
    err::msg create_error(const lex::Token &token, const std::string &message);
    void synchronize();

    Result<types::Type_id> parse_type();
    Result<ast::Function_param> parse_function_parameter(bool allow_default_value);
    Result<std::vector<ast::Call_argument>> parse_call_arguments();

    Result<std::optional<ast::Stmt_id>> declaration();
    Result<ast::Stmt_id> function_declaration();
    Result<ast::Stmt_id> model_declaration();
    Result<ast::Stmt_id> parse_bind_statement();
    Result<ast::Stmt_id> union_declaration();
    Result<ast::Stmt_id> enum_declaration();
    Result<ast::Stmt_id> var_declaration(bool is_const);

    Result<ast::Typed_member_decl> parse_model_field();
    Result<ast::Function_stmt> parse_model_method();

    Result<ast::Stmt_id> statement();
    Result<ast::Stmt_id> import_statement();
    Result<ast::Stmt_id> print_statement(ast::Print_stream stream = ast::Print_stream::STDOUT);
    Result<ast::Stmt_id> block_statement();
    Result<ast::Stmt_id> if_statement();
    Result<ast::Stmt_id> while_statement();
    Result<ast::Stmt_id> for_statement();
    Result<ast::Stmt_id> for_in_statement();
    Result<ast::Stmt_id> match_statement();
    Result<ast::Stmt_id> return_statement();
    Result<ast::Stmt_id> expression_statement();

    Result<ast::Expr_id> expression();
    Result<ast::Expr_id> assignment();
    Result<ast::Expr_id> range_expr();
    Result<ast::Expr_id> logical_or();
    Result<ast::Expr_id> logical_and();
    Result<ast::Expr_id> bitwise_or();
    Result<ast::Expr_id> bitwise_xor();
    Result<ast::Expr_id> bitwise_and();
    Result<ast::Expr_id> equality();
    Result<ast::Expr_id> comparison();
    Result<ast::Expr_id> bitwise_shift();
    Result<ast::Expr_id> term();
    Result<ast::Expr_id> factor();
    Result<ast::Expr_id> cast();
    Result<ast::Expr_id> unary();
    Result<ast::Expr_id> call();
    Result<ast::Expr_id> primary();

    Result<ast::Expr_id> parse_closure_expression();
    Result<ast::Expr_id> parse_array_literal();
    Result<ast::Expr_id> parse_model_literal(const std::string &model_name);

    void stamp_loc(ast::Source_location &loc);
    void stamp_expr(ast::Expr_id expr);
    void stamp_stmt(ast::Stmt_id stmt);
    void stamp_statement_locations(const std::vector<ast::Stmt_id> &statements);
};

} // namespace phos
