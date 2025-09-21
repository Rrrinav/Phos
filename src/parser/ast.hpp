#pragma once

#include <cstddef>
#include <memory>
#include <string>
#include <vector>
#include <variant>

#include "../value/type.hpp"
#include "../value/value.hpp"
#include "../lexer/token.hpp"

namespace phos::ast
{

struct Source_location
{
    size_t line = 0;
    size_t column = 0;
};

struct Function_param
{
    std::string name;
    types::Type type;
    bool is_const;
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
    struct Expr* left;
    lex::TokenType op;
    struct Expr* right;
    types::Type type;
    Source_location loc;
};

struct Unary_expr
{
    lex::TokenType op;
    struct Expr* right;
    types::Type type;
    Source_location loc;
};

struct Call_expr
{
    std::string callee;
    std::vector<struct Expr*> arguments;
    types::Type type;
    Source_location loc;
};

struct Assignment_expr
{
    std::string name;
    struct Expr* value;
    types::Type type;
    Source_location loc;
};

struct Cast_expr
{
    struct Expr* expression;
    types::Type target_type;
    Source_location loc;
};

struct Field_access_expr
{
    struct Expr* object;
    std::string field_name;
    types::Type type;
    Source_location loc;
};

struct Field_assignment_expr
{
    Expr* object;
    std::string field_name;
    Expr* value;
    types::Type type;
    Source_location loc;
};

struct Method_call_expr
{
    struct Expr* object;
    std::string method_name;
    std::vector<struct Expr*> arguments;
    types::Type type;
    Source_location loc;
};

struct Model_literal_expr
{
    std::string model_name;
    std::vector<std::pair<std::string, struct Expr*>> fields;
    types::Type type;
    Source_location loc;
};

struct Closure_expr
{
    std::vector<Function_param> parameters;
    types::Type return_type;
    struct Stmt* body;
    types::Type type;
    Source_location loc;
};

struct Array_literal_expr
{
    std::vector<Expr*> elements;
    types::Type type;
    Source_location loc;
};

struct Array_access_expr
{
    Expr* array;
    Expr* index;
    types::Type type;
    Source_location loc;
};

struct Array_assignment_expr
{
    Expr* array;
    Expr* index;
    Expr* value;
    types::Type type;
    Source_location loc;
};

// =================================
// Unified expression wrapper
// =================================
struct Expr
{
    using Node = std::variant<Literal_expr, Variable_expr, Binary_expr, Unary_expr, Call_expr, Assignment_expr, Cast_expr,
                              Field_access_expr, Method_call_expr, Model_literal_expr, Closure_expr, Field_assignment_expr,
                              Array_literal_expr, Array_assignment_expr, Array_access_expr>;

    Node node;
};

// =================================
// Statement node types
// =================================
struct Return_stmt
{
    Expr* expression;
    Source_location loc;
};

struct Function_stmt
{
    std::string name;
    std::vector<Function_param> parameters;
    types::Type return_type;
    struct Stmt* body;
    Source_location loc;
};

struct Model_stmt
{
    std::string name;
    std::vector<std::pair<std::string, types::Type>> fields;
    std::vector<Function_stmt> methods;
    Source_location loc;
};

struct Var_stmt
{
    bool is_const;
    std::string name;
    types::Type type;
    Expr* initializer;
    bool type_inferred = false;
    Source_location loc;
};

enum class Print_stream { STDOUT, STDERR };

struct Print_stmt
{
    Print_stream stream;
    Expr* expression;
    Source_location loc;
};

struct Expr_stmt
{
    Expr* expression;
    Source_location loc;
};

struct Block_stmt
{
    std::vector<struct Stmt*> statements;
    Source_location loc;
};

struct If_stmt
{
    Expr* condition;
    struct Stmt* then_branch;
    struct Stmt* else_branch;
    Source_location loc;
};

struct While_stmt
{
    Expr* condition;
    struct Stmt* body;
    Source_location loc;
};

struct For_stmt
{
    struct Stmt* initializer;
    Expr* condition;
    Expr* increment;
    struct Stmt* body;
    Source_location loc;
};

// =================================
// Unified statement wrapper
// =================================
struct Stmt
{
    using Node = std::variant<Return_stmt, Function_stmt, Model_stmt, Var_stmt, Print_stmt, Expr_stmt, Block_stmt, If_stmt, While_stmt,
                              For_stmt>;

    Node node;
};

inline types::Type &get_type(Expr::Node &node)
{
    return std::visit(
        [](auto &expr) -> types::Type &
        {
            using T = std::decay_t<decltype(expr)>;
            if constexpr (std::is_same_v<T, Cast_expr>)
                return expr.target_type;
            else
                return expr.type;
        },
        node);
}

// const version
inline const types::Type &get_type(const Expr::Node &node)
{
    return std::visit(
        [](auto const &expr) -> const types::Type &
        {
            using T = std::decay_t<decltype(expr)>;
            if constexpr (std::is_same_v<T, Cast_expr>)
                return expr.target_type;
            else
                return expr.type;
        },
        node);
}

inline Source_location &get_loc(Expr::Node &node)
{
    return std::visit([](auto &expr) -> Source_location & { return expr.loc; }, node);
}

inline Source_location &loc_copy(Expr::Node node)
{
    return std::visit([](auto &expr) -> Source_location & { return expr.loc; }, node);
}

inline Source_location &get_loc(Stmt::Node &node)
{
    return std::visit([](auto &stmt) -> Source_location & { return stmt.loc; }, node);
}

inline Source_location loc_copy(const Stmt::Node &node)
{
    return std::visit([](auto &stmt) -> Source_location { return stmt.loc; }, node);
}


inline types::Type get_type(const Stmt::Node &node)
{
    return std::visit(
        [](auto const &stmt) -> types::Type
        {
            using T = std::decay_t<decltype(stmt)>;
            if constexpr (std::is_same_v<T, ast::Var_stmt>)
            {
                return stmt.type;
            }
            else if constexpr (std::is_same_v<T, ast::Return_stmt>)
            {
                if (stmt.expression)
                    return get_type(stmt.expression->node);
                return types::Primitive_kind::Void;
            }
            else if constexpr (std::is_same_v<T, ast::Expr_stmt>)
            {
                return get_type(stmt.expression->node);
            }
            else
            {
                // For Block, If, While, For, etc., the "type" is void.
                return types::Primitive_kind::Void;
            }
        },
        node);
}
}  // namespace phos::ast
