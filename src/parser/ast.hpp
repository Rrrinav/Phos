#pragma once

#include <cstddef>

#include "../value/type.hpp"
#include "../value/value.hpp"
#include "../lexer/token.hpp"

namespace phos::ast
{

// =================================
// Source location (common for all)
// =================================
struct Source_location
{
    size_t line = 0;
    size_t column = 0;
};

// =================================
// Expression node types
// =================================
struct Literal_expr
{
    Value value;
};

struct Variable_expr
{
    std::string name;
};

struct Binary_expr
{
    std::unique_ptr<struct Expr> left;
    lex::TokenType op;
    std::unique_ptr<struct Expr> right;
};

struct Unary_expr
{
    lex::TokenType op;
    std::unique_ptr<struct Expr> right;
};

struct Call_expr
{
    std::string callee;
    std::vector<std::unique_ptr<struct Expr>> arguments;
};

struct Assignment_expr
{
    std::string name;
    std::unique_ptr<struct Expr> value;
};

struct Cast_expr
{
    std::unique_ptr<struct Expr> expression;
};

// =================================
// Unified expression wrapper
// =================================
struct Expr
{
    using Node = std::variant<Literal_expr, Variable_expr, Binary_expr, Unary_expr, Call_expr, Assignment_expr, Cast_expr>;

    Node node;
    types::Type type;
    Source_location loc;
};

// =================================
// Statement node types
// =================================
struct Return_stmt
{
    std::unique_ptr<Expr> expression;  // may be null
};

struct Function_stmt
{
    std::string name;
    std::vector<std::pair<std::string, types::Type>> parameters;
    types::Type return_type;
    std::unique_ptr<struct Stmt> body;
};

struct Var_stmt
{
    std::string name;
    types::Type type;
    std::unique_ptr<Expr> initializer;  // may be null
};

enum class Print_stream
{
    STDOUT,
    STDERR
};

struct Print_stmt
{
    Print_stream stream;
    std::unique_ptr<Expr> expression;
};

struct Expr_stmt
{
    std::unique_ptr<Expr> expression;
};

struct Block_stmt
{
    std::vector<std::unique_ptr<struct Stmt>> statements;
};

struct If_stmt
{
    std::unique_ptr<Expr> condition;
    std::unique_ptr<struct Stmt> then_branch;
    std::unique_ptr<struct Stmt> else_branch;  // may be null
};

struct While_stmt
{
    std::unique_ptr<Expr> condition;
    std::unique_ptr<struct Stmt> body;
};

struct For_stmt
{
    std::unique_ptr<struct Stmt> initializer;  // may be null
    std::unique_ptr<Expr>        condition;    // may be null
    std::unique_ptr<Expr>        increment;    // may be null
    std::unique_ptr<struct Stmt> body;
};

// =================================
// Unified statement wrapper
// =================================
struct Stmt
{
    using Node = std::variant<Return_stmt, Function_stmt, Var_stmt, Print_stmt, Expr_stmt, Block_stmt, If_stmt, While_stmt, For_stmt>;

    Node node;
    Source_location loc;
};

}  // namespace phos::ast
