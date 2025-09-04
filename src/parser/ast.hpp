#pragma once

#include <cstddef>
#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "../value/type.hpp"
#include "../value/value.hpp"
#include "../lexer/token.hpp"

namespace phos::ast
{

// =================================
// Common metadata
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
    types::Type type;
    Source_location loc;
};

struct Variable_expr
{
    std::string name;
    types::Type type;
    Source_location loc;
};

struct Binary_expr
{
    std::unique_ptr<struct Expr> left;
    lex::TokenType op;
    std::unique_ptr<struct Expr> right;

    types::Type type;
    Source_location loc;
};

struct Unary_expr
{
    lex::TokenType op;
    std::unique_ptr<struct Expr> right;

    types::Type type;
    Source_location loc;
};

struct Call_expr
{
    std::string callee;
    std::vector<std::unique_ptr<struct Expr>> arguments;

    types::Type type;
    Source_location loc;
};

struct Assignment_expr
{
    std::string name;
    std::unique_ptr<struct Expr> value;

    types::Type type;
    Source_location loc;
};

struct Cast_expr
{
    std::unique_ptr<struct Expr> expression;

    types::Type type;  // the cast target
    Source_location loc;
};

// =================================
// Unified expression wrapper
// =================================
using Expr_node = std::variant<
    Literal_expr,
    Variable_expr,
    Binary_expr,
    Unary_expr,
    Call_expr,
    Assignment_expr,
    Cast_expr
>;

struct Expr
{
    Expr_node node;
};

// =================================
// Statement node types
// =================================
struct Return_stmt
{
    std::unique_ptr<Expr> expression;  // may be null
    Source_location loc;
};

struct Function_stmt
{
    std::string name;
    std::vector<std::pair<std::string, types::Type>> parameters;
    types::Type return_type;
    std::unique_ptr<struct Stmt> body;

    Source_location loc;
    types::Type function_type;
};

struct Var_stmt
{
    std::string name;
    types::Type type;
    std::unique_ptr<Expr> initializer;  // may be null
    Source_location loc;
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
    Source_location loc;
};

struct Expr_stmt
{
    std::unique_ptr<Expr> expression;
    Source_location loc;
};

struct Block_stmt
{
    std::vector<std::unique_ptr<struct Stmt>> statements;
    Source_location loc;
};

struct If_stmt
{
    std::unique_ptr<Expr> condition;
    std::unique_ptr<struct Stmt> then_branch;
    std::unique_ptr<struct Stmt> else_branch;  // may be null
    Source_location loc;
};

struct While_stmt
{
    std::unique_ptr<Expr> condition;
    std::unique_ptr<struct Stmt> body;
    Source_location loc;
};

// =================================
// Unified statement wrapper
// =================================
using Stmt_node = std::variant<
    Return_stmt,
    Function_stmt,
    Var_stmt,
    Print_stmt,
    Expr_stmt,
    Block_stmt,
    If_stmt,
    While_stmt
>;

struct Stmt
{
    Stmt_node node;
};

types::Type &get_type(Expr_node &node)
{
    return std::visit([](auto &n) -> types::Type &{ return n.type; }, node);
}

types::Type get_type_copy(const Expr_node &node)
{
    return std::visit([](auto &n) -> types::Type { return n.type; }, node);
}

Source_location &loc(Stmt_node &node)
{
    return std::visit([](auto &n) -> Source_location &{ return n.loc; }, node);
}
Source_location &loc(Expr_node &node)
{
    return std::visit([](auto &n) -> Source_location &{ return n.loc; }, node);
}
Source_location loc_copy(const Stmt_node &node)
{
    return std::visit([](auto &n) -> Source_location { return n.loc; }, node);
}
Source_location loc_copy(const Expr_node &node)
{
    return std::visit([](auto &n) -> Source_location { return n.loc; }, node);
}
}  // namespace phos::ast
