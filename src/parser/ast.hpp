#pragma once

#include <cstddef>
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
    size_t l = 0;
    size_t c = 0;
};

struct Function_param
{
    std::string  name;
    types::Type  type;
    bool         is_mut   = false;
    struct Expr* default_value = nullptr;
    Source_location loc;
};

struct Call_argument
{
    std::string   name;
    struct Expr*  value = nullptr;
    Source_location loc;
};

struct Literal_expr
{
    Value           value;
    types::Type     type;
    Source_location loc;
};

struct Variable_expr
{
    std::string     name;
    types::Type     type;
    Source_location loc;
};

struct Binary_expr
{
    struct Expr*    left;
    lex::TokenType  op;
    struct Expr*    right;
    types::Type     type;
    Source_location loc;
};

struct Unary_expr
{
    lex::TokenType  op;
    struct Expr*    right;
    types::Type     type;
    Source_location loc;
};

struct Call_expr
{
    struct Expr*               callee;
    std::vector<Call_argument> arguments;
    types::Type                type;
    Source_location            loc;
    int                        native_signature_index = -1;
};

// Simple variable assignment:  name = value
struct Assignment_expr
{
    std::string     name;
    struct Expr*    value;
    types::Type     type;
    Source_location loc;
};

// Field mutation:  obj.field = value
struct Field_assignment_expr
{
    struct Expr*    object;
    std::string     field_name;
    struct Expr*    value;
    types::Type     type;
    Source_location loc;
};

// Array element mutation:  arr[idx] = value
struct Array_assignment_expr
{
    struct Expr*    array;
    struct Expr*    index;
    struct Expr*    value;
    types::Type     type;
    Source_location loc;
};

struct Cast_expr
{
    struct Expr*    expression;
    types::Type     target_type;
    Source_location loc;
};

struct Field_access_expr
{
    struct Expr*    object;
    std::string     field_name;
    types::Type     type;
    Source_location loc;
};

struct Method_call_expr
{
    struct Expr*               object;
    std::string                method_name;
    std::vector<Call_argument> arguments;
    types::Type                type;
    Source_location            loc;

    bool                      is_closure_field = false;
    uint8_t                   field_index = 0;
    int                       native_signature_index = -1;
};

struct Model_literal_expr
{
    std::string                                        model_name;
    std::vector<std::pair<std::string, struct Expr*>>  fields;
    types::Type                                        type;
    Source_location                                    loc;
};

struct Closure_expr
{
    std::vector<Function_param> parameters;
    types::Type                 return_type;
    struct Stmt*                body;
    types::Type                 type;
    Source_location             loc;
};

struct Array_literal_expr
{
    std::vector<struct Expr*> elements;
    types::Type               type;
    Source_location           loc;
};

struct Array_access_expr
{
    struct Expr*    array;
    struct Expr*    index;
    types::Type     type;
    Source_location loc;
};

// Type::Member  or  Union::Variant
struct Static_path_expr
{
    struct Expr*    base;
    lex::Token      member;
    types::Type     type;
    Source_location loc;
};

// start..end  or  start..=end
struct Range_expr
{
    struct Expr*    start;
    struct Expr*    end;
    bool            inclusive; // true => ..=   false => ..
    types::Type     type;
    Source_location loc;
};

// spawn fn(args)  =>  Thread<T>
struct Spawn_expr
{
    struct Expr*    call;
    types::Type     type;
    Source_location loc;
};

// await thread_val  =>  T
struct Await_expr
{
    struct Expr*    thread;
    types::Type     type;
    Source_location loc;
};

// yield  /  yield value
struct Yield_expr
{
    struct Expr*    value; // nullptr for bare yield
    types::Type     type;
    Source_location loc;
};

// f"hello {name}, result {x + 1}"
// The parser expands this into a Binary_expr concat tree at parse time.
// This node is kept only for the AST printer.
struct Fstring_expr
{
    std::string               raw_template;    // e.g. "hello {name}"
    std::vector<struct Expr*> interpolations;  // exprs extracted from {}
    types::Type               type;            // always String
    Source_location           loc;
};

// =============================================================================
// Unified expression wrapper
// =============================================================================

struct Expr
{
    using Node = std::variant<
        Literal_expr,
        Variable_expr,
        Binary_expr,
        Unary_expr,
        Call_expr,
        Assignment_expr,
        Field_assignment_expr,
        Array_assignment_expr,
        Cast_expr,
        Field_access_expr,
        Method_call_expr,
        Model_literal_expr,
        Closure_expr,
        Array_literal_expr,
        Array_access_expr,
        Static_path_expr,
        Range_expr,
        Spawn_expr,
        Await_expr,
        Yield_expr,
        Fstring_expr
    >;

    Node node;
};

// =============================================================================
// Statement node types
// =============================================================================

struct Return_stmt
{
    struct Expr*    expression; // nullptr for bare return
    Source_location loc;
};

struct Function_stmt
{
    std::string                 name;
    bool                        is_static;
    std::vector<Function_param> parameters;
    types::Type                 return_type;
    struct Stmt*                body;
    Source_location             loc;
};

struct Model_stmt
{
    std::string                                  name;
    std::vector<std::pair<std::string, types::Type>> fields;
    std::vector<Function_stmt*>                  methods;
    Source_location                              loc;
};

struct Union_stmt
{
    std::string                                      name;
    std::vector<std::pair<std::string, types::Type>> variants;
    Source_location                                  loc;
};

struct Var_stmt
{
    bool            is_mut          = false; // let mut  => reassignable
    bool            is_const        = false; // const    => compile-time inline
    std::string     name;
    types::Type     type;
    struct Expr*    initializer     = nullptr;
    bool            type_inferred   = false;
    Source_location loc;
};

enum class Print_stream { STDOUT, STDERR };

struct Print_stmt
{
    Print_stream    stream;
    struct Expr*    expression;
    Source_location loc;
};

struct Expr_stmt
{
    struct Expr*    expression;
    Source_location loc;
};

struct Block_stmt
{
    std::vector<struct Stmt*> statements;
    Source_location           loc;
};

struct If_stmt
{
    struct Expr*    condition;
    struct Stmt*    then_branch;
    struct Stmt*    else_branch; // nullptr if no else
    Source_location loc;
};

struct While_stmt
{
    struct Expr*    condition;
    struct Stmt*    body;
    Source_location loc;
};

// C-style for  (kept for compatibility)
struct For_stmt
{
    struct Stmt*    initializer;
    struct Expr*    condition;
    struct Expr*    increment;
    struct Stmt*    body;
    Source_location loc;
};

// for x in iterable { ... }
struct For_in_stmt
{
    std::string     var_name;  // always immutable inside body
    struct Expr*    iterable;  // Range_expr or array expr
    struct Stmt*    body;
    Source_location loc;
};

// One arm of a match statement
struct Match_arm
{
    std::string  union_name;   // e.g. "Result"
    std::string  variant_name; // e.g. "Ok"
    std::string  bind_name;    // e.g. "s" (Empty if no payload bound)
    bool         is_wildcard = false;
    struct Stmt* body        = nullptr;
};

struct Match_stmt
{
    struct Expr*            subject;
    std::vector<Match_arm>  arms;
    Source_location         loc;
};

// =============================================================================
// Unified statement wrapper
// =============================================================================

struct Stmt
{
    using Node = std::variant<
        Return_stmt,
        Function_stmt,
        Model_stmt,
        Var_stmt,
        Print_stmt,
        Expr_stmt,
        Block_stmt,
        If_stmt,
        While_stmt,
        For_stmt,
        For_in_stmt,
        Union_stmt,
        Match_stmt
    >;

    Node node;
};

// =============================================================================
// Inline helpers
// =============================================================================

inline types::Type& get_type(Expr::Node& node)
{
    return std::visit(
        [](auto& expr) -> types::Type&
        {
            using T = std::decay_t<decltype(expr)>;
            if constexpr (std::is_same_v<T, Cast_expr>)
                return expr.target_type;
            else
                return expr.type;
        },
        node);
}

inline const types::Type& get_type(const Expr::Node& node)
{
    return std::visit(
        [](const auto& expr) -> const types::Type&
        {
            using T = std::decay_t<decltype(expr)>;
            if constexpr (std::is_same_v<T, Cast_expr>)
                return expr.target_type;
            else
                return expr.type;
        },
        node);
}

inline Source_location& get_loc(Expr::Node& node)
{
    return std::visit([](auto& expr) -> Source_location& { return expr.loc; }, node);
}

inline Source_location get_loc(const Expr::Node& node)
{
    return std::visit([](const auto& expr) -> Source_location { return expr.loc; }, node);
}

inline Source_location& get_loc(Stmt::Node& node)
{
    return std::visit([](auto& stmt) -> Source_location& { return stmt.loc; }, node);
}

inline Source_location get_loc(const Stmt::Node& node)
{
    return std::visit([](const auto& stmt) -> Source_location { return stmt.loc; }, node);
}

inline types::Type get_type(const Stmt::Node& node)
{
    return std::visit(
        [](const auto& stmt) -> types::Type
        {
            using T = std::decay_t<decltype(stmt)>;
            if constexpr (std::is_same_v<T, Var_stmt>)
                return stmt.type;
            else if constexpr (std::is_same_v<T, Return_stmt>)
                return stmt.expression ? get_type(stmt.expression->node)
                                       : types::Type(types::Primitive_kind::Void);
            else if constexpr (std::is_same_v<T, Expr_stmt>)
                return get_type(stmt.expression->node);
            else
                return types::Type(types::Primitive_kind::Void);
        },
        node);
}

} // namespace phos::ast
