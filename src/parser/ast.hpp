#pragma once

#include "../utility/includes.hpp"

#include "../value/type.hpp"
#include "../value/value.hpp"
#include "../lexer/token.hpp"

namespace phos {
namespace ast {

struct Expr
{
    size_t line = 0, column = 0;
    virtual ~Expr() = default;
    virtual types::Type get_type() const = 0;
};

struct Stmt
{
    size_t line = 0, column = 0;
    virtual ~Stmt() = default;
};

struct Literal_expr : Expr
{
    Value value;
    types::Type type;
    Literal_expr(Value v, types::Type t, size_t l = 0, size_t c = 0) : value(std::move(v)), type(t)
    {
        line = l;
        column = c;
    }
    types::Type get_type() const override { return type; }
};

struct Variable_expr : Expr
{
    std::string name;
    types::Type type;
    explicit Variable_expr(std::string n, types::Type t, size_t l = 0, size_t c = 0) : name(std::move(n)), type(t)
    {
        line = l;
        column = c;
    }
    types::Type get_type() const override { return type; }
};

struct Binary_expr : Expr
{
    std::unique_ptr<Expr> left;
    lex::TokenType operator_;
    std::unique_ptr<Expr> right;
    types::Type type;
    Binary_expr(std::unique_ptr<Expr> l, lex::TokenType op, std::unique_ptr<Expr> r, types::Type t, size_t ln = 0, size_t c = 0)
        : left(std::move(l)), operator_(op), right(std::move(r)), type(t)
    {
        line = ln;
        column = c;
    }
    types::Type get_type() const override { return type; }
};

struct Unary_expr : Expr
{
    lex::TokenType operator_;
    std::unique_ptr<Expr> right;
    types::Type type;
    Unary_expr(lex::TokenType op, std::unique_ptr<Expr> r, types::Type t, size_t l = 0, size_t c = 0) : operator_(op), right(std::move(r)), type(t)
    {
        line = l;
        column = c;
    }
    types::Type get_type() const override { return type; }
};

struct Call_expr : Expr
{
    std::string callee;
    std::vector<std::unique_ptr<Expr>> arguments;
    types::Type type;
    Call_expr(std::string c, std::vector<std::unique_ptr<Expr>> args, types::Type t, size_t l = 0, size_t col = 0)
        : callee(std::move(c)), arguments(std::move(args)), type(t)
    {
        line = l;
        column = col;
    }
    types::Type get_type() const override { return type; }
};

struct Assignment_expr : Expr
{
    std::string name;
    std::unique_ptr<Expr> value;
    types::Type type;
    Assignment_expr(std::string n, std::unique_ptr<Expr> val, types::Type t, size_t l = 0, size_t c = 0)
        : name(std::move(n)), value(std::move(val)), type(t)
    {
        line = l;
        column = c;
    }
    types::Type get_type() const override { return type; }
};

struct Cast_expr : Expr
{
    std::unique_ptr<Expr> expression;
    types::Type target_type;
    Cast_expr(std::unique_ptr<Expr> expr, types::Type t, size_t l = 0, size_t c = 0) : expression(std::move(expr)), target_type(t)
    {
        line = l;
        column = c;
    }
    types::Type get_type() const override { return target_type; }
};

struct Return_stmt : Stmt
{
    std::unique_ptr<Expr> expression;
    explicit Return_stmt(std::unique_ptr<Expr> expr, size_t l = 0, size_t c = 0) : expression(std::move(expr))
    {
        line = l;
        column = c;
    }
};

struct Function_stmt : Stmt
{
    std::string name;
    std::vector<std::pair<std::string, types::Type>> parameters;
    types::Type return_type;
    std::shared_ptr<Stmt> body;
    Function_stmt(std::string n, std::vector<std::pair<std::string, types::Type>> params, types::Type ret, std::shared_ptr<Stmt> b, size_t l = 0,
                 size_t c = 0)
        : name(std::move(n)), parameters(std::move(params)), return_type(ret), body(std::move(b))
    {
        line = l;
        column = c;
    }
};

struct Var_stmt : Stmt
{
    std::string name;
    types::Type type;
    std::unique_ptr<Expr> initializer;
    Var_stmt(std::string n, types::Type t, std::unique_ptr<Expr> init, size_t l = 0, size_t c = 0)
        : name(std::move(n)), type(t), initializer(std::move(init))
    {
        line = l;
        column = c;
    }
};

enum class Print_stream
{
    STDOUT, STDERR
};

struct Print_stmt : Stmt
{
    Print_stream stream;
    std::unique_ptr<Expr> expression;
    explicit Print_stmt(std::unique_ptr<Expr> expr, size_t l = 0, size_t c = 0) : expression(std::move(expr))
    {
        line = l;
        column = c;
    }
};

struct Expr_stmt : Stmt
{
    std::unique_ptr<Expr> expression;
    explicit Expr_stmt(std::unique_ptr<Expr> expr, size_t l = 0, size_t c = 0) : expression(std::move(expr))
    {
        line = l;
        column = c;
    }
};

struct Block_stmt : Stmt
{
    std::vector<std::unique_ptr<Stmt>> statements;
    explicit Block_stmt(std::vector<std::unique_ptr<Stmt>> stmts, size_t l = 0, size_t c = 0) : statements(std::move(stmts))
    {
        line = l;
        column = c;
    }
};

struct If_stmt : Stmt
{
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> then_branch;
    std::unique_ptr<Stmt> else_branch;
    If_stmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> then_stmt, std::unique_ptr<Stmt> else_stmt = nullptr, size_t l = 0,
           size_t c = 0)
        : condition(std::move(cond)), then_branch(std::move(then_stmt)), else_branch(std::move(else_stmt))
    {
        line = l;
        column = c;
    }
};

struct While_stmt : Stmt
{
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> body;
    While_stmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> body_stmt, size_t l = 0, size_t c = 0)
        : condition(std::move(cond)), body(std::move(body_stmt))
    {
        line = l;
        column = c;
    }
};

struct For_stmt : Stmt
{
    std::unique_ptr<Stmt> initializer;
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Expr> increment;
    std::unique_ptr<Stmt> body;
    For_stmt(std::unique_ptr<Stmt> init, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> inc, std::unique_ptr<Stmt> body_stmt,
            size_t l = 0, size_t c = 0)
        : initializer(std::move(init)), condition(std::move(cond)), increment(std::move(inc)), body(std::move(body_stmt))
    {
        line = l;
        column = c;
    }
};

} // namespace ast
} // namespace phos
