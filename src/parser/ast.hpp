#pragma once

#include "../lexer/token.hpp"
#include "../value/type.hpp"
#include "../value/value.hpp"
#include "../semantic-checker/symbol.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>
#include <deque>

namespace phos::ast {

// Strongly-Typed AST Indices
struct Expr_id
{
    uint32_t value;
    constexpr bool operator==(const Expr_id &o) const noexcept
    {
        return value == o.value;
    }
    constexpr bool operator!=(const Expr_id &o) const noexcept
    {
        return value != o.value;
    }
    constexpr bool is_null() const noexcept
    {
        return value == 0xFFFFFFFF;
    }
    static constexpr Expr_id null() noexcept
    {
        return {0xFFFFFFFF};
    }
};

struct Stmt_id
{
    uint32_t value;
    constexpr bool operator==(const Stmt_id &o) const noexcept
    {
        return value == o.value;
    }
    constexpr bool operator!=(const Stmt_id &o) const noexcept
    {
        return value != o.value;
    }
    constexpr bool is_null() const noexcept
    {
        return value == 0xFFFFFFFF;
    }
    static constexpr Stmt_id null() noexcept
    {
        return {0xFFFFFFFF};
    }
};

struct Source_location
{
    size_t l = 0;
    size_t c = 0;
    std::string file{""};
};

struct Function_param
{
    std::string name;
    types::Type_id type;
    bool is_mut = false;
    Expr_id default_value = Expr_id::null();
    Source_location loc;
};

struct Typed_member_decl
{
    std::string name;
    types::Type_id type;
    bool is_static = false;
    Expr_id default_value = Expr_id::null();
    Source_location loc;
};

struct Call_argument
{
    std::string name;
    Expr_id value = Expr_id::null();
    Source_location loc;
};

// Expression Nodes
struct Literal_expr
{
    Value value;
    types::Type_id type;
    Source_location loc;
};

struct Variable_expr
{
    std::string name;
    types::Type_id type;
    Source_location loc;

    std::optional<Symbol_id> resolved_symbol = std::nullopt;
};

struct Binary_expr
{
    Expr_id left;
    lex::TokenType op;
    Expr_id right;
    types::Type_id type;
    Source_location loc;
};

struct Unary_expr
{
    lex::TokenType op;
    Expr_id right;
    types::Type_id type;
    Source_location loc;
};
struct Call_expr
{
    Expr_id callee;
    std::vector<Call_argument> arguments;
    types::Type_id type;
    Source_location loc;
    int native_signature_index = -1;
};

struct Assignment_expr
{
    std::string name;
    Expr_id value;
    types::Type_id type;
    Source_location loc;
};

struct Field_assignment_expr
{
    Expr_id object;
    std::string field_name;
    Expr_id value;
    types::Type_id type;
    Source_location loc;
};

struct Array_assignment_expr
{
    Expr_id array;
    Expr_id index;
    Expr_id value;
    types::Type_id type;
    Source_location loc;
};

struct Cast_expr
{
    Expr_id expression;
    types::Type_id target_type;
    Source_location loc;
};

struct Field_access_expr
{
    Expr_id object;
    std::string field_name;
    types::Type_id type;
    Source_location loc;
};

struct Method_call_expr
{
    Expr_id object;
    std::string method_name;
    std::vector<Call_argument> arguments;
    types::Type_id type;
    Source_location loc;
    bool is_closure_field = false;
    uint8_t field_index = 0;
    int native_signature_index = -1;
};

struct Model_literal_expr
{
    std::string model_name;
    std::vector<std::pair<std::string, Expr_id>> fields;
    types::Type_id type;
    Source_location loc;
};

struct Closure_expr
{
    std::vector<Function_param> parameters;
    types::Type_id return_type;
    Stmt_id body;
    types::Type_id type;
    Source_location loc;
};

struct Array_literal_expr
{
    std::vector<Expr_id> elements;
    types::Type_id type;
    Source_location loc;
};

struct Array_access_expr
{
    Expr_id array;
    Expr_id index;
    types::Type_id type;
    Source_location loc;
};

struct Static_path_expr
{
    Expr_id base;
    lex::Token member;
    types::Type_id type;
    Source_location loc;

    std::optional<Symbol_id> resolved_symbol = std::nullopt;
};

struct Range_expr
{
    Expr_id start;
    Expr_id end;
    bool inclusive;
    types::Type_id type;
    Source_location loc;
};

struct Spawn_expr
{
    Expr_id call;
    types::Type_id type;
    Source_location loc;
};

struct Await_expr
{
    Expr_id thread;
    types::Type_id type;
    Source_location loc;
};

struct Yield_expr
{
    Expr_id value;
    types::Type_id type;
    Source_location loc;
};

struct Enum_member_expr
{
    std::string member_name;
    Source_location loc;
    types::Type_id type{};
};

struct Anon_model_literal_expr
{
    std::vector<std::pair<std::string, Expr_id>> fields;
    types::Type_id type{};
    Source_location loc;
};

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
        Range_expr, //
        Spawn_expr,
        Await_expr,
        Yield_expr,
        Enum_member_expr,
        Anon_model_literal_expr>;

    Node node;
    uint8_t auto_wrap_depth = 0;
};

// Statement Nodes
struct Return_stmt
{
    Expr_id expression = Expr_id::null();
    Source_location loc;
};

struct Function_stmt
{
    std::string name;
    bool is_static;
    std::vector<Function_param> parameters;
    types::Type_id return_type;
    Stmt_id body;
    Source_location loc;
};

struct Model_stmt
{
    std::string name;
    std::vector<Typed_member_decl> fields;
    std::vector<Stmt_id> methods;
    Source_location loc;
};

struct Union_stmt
{
    std::string name;
    std::vector<Typed_member_decl> variants;
    Source_location loc;
};

struct Var_stmt
{
    bool is_mut = false;
    bool is_const = false;
    std::string name;
    types::Type_id type;
    Expr_id initializer = Expr_id::null();
    bool type_inferred = false;
    Source_location loc;
};

enum class Print_stream { STDOUT, STDERR };
struct Print_stmt
{
    Print_stream stream{Print_stream::STDOUT};
    std::vector<Expr_id> expressions{};
    std::string sep = " ";
    std::string end = "\n";
    Source_location loc;
};

struct Expr_stmt
{
    Expr_id expression;
    Source_location loc;
};

struct Block_stmt
{
    std::vector<Stmt_id> statements;
    Source_location loc;
};

struct If_stmt
{
    Expr_id condition;
    Stmt_id then_branch;
    Stmt_id else_branch = Stmt_id::null();
    Source_location loc;
};

struct While_stmt
{
    Expr_id condition;
    Stmt_id body;
    Source_location loc;
};

struct For_stmt
{
    Stmt_id initializer;
    Expr_id condition;
    Expr_id increment;
    Stmt_id body;
    Source_location loc;
};

struct For_in_stmt
{
    std::string var_name;
    Expr_id iterable;
    Stmt_id body;
    Source_location loc;
};

struct Match_arm
{
    Expr_id pattern = Expr_id::null();
    std::string bind_name;
    bool is_wildcard = false;
    Stmt_id body = Stmt_id::null();
};

struct Match_stmt
{
    Expr_id subject;
    std::vector<Match_arm> arms;
    Source_location loc;
};

struct Enum_stmt
{
    std::string name;
    types::Type_id base_type;
    std::vector<std::pair<std::string, std::optional<Value>>> variants;
    Source_location loc;
};

struct Import_stmt
{
    std::vector<std::string> path{};
    std::vector<std::string> selectives{};
    std::string local_alias{};
    bool is_first_class{};

    Source_location loc{};
};

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
        Match_stmt,
        Enum_stmt,
        Import_stmt
    >;

    Node node;
};

// The Abstract Syntax Tree (Central Storage)
class Ast_tree
{
public:
    std::vector<Expr> expressions;
    std::vector<Stmt> statements;

    Expr_id add_expr(Expr expr)
    {
        Expr_id id = {static_cast<uint32_t>(expressions.size())};
        expressions.push_back(std::move(expr));
        return id;
    }

    Stmt_id add_stmt(Stmt stmt)
    {
        Stmt_id id = {static_cast<uint32_t>(statements.size())};
        statements.push_back(std::move(stmt));
        return id;
    }

    Expr &get(Expr_id id)
    {
        return expressions[id.value];
    }
    const Expr &get(Expr_id id) const
    {
        return expressions[id.value];
    }

    Stmt &get(Stmt_id id)
    {
        return statements[id.value];
    }
    const Stmt &get(Stmt_id id) const
    {
        return statements[id.value];
    }
};

// Inline Extractors
inline types::Type_id &get_type(Expr::Node &node)
{
    return std::visit(
        [](auto &expr) -> types::Type_id & {
            using T = std::decay_t<decltype(expr)>;
            if constexpr (std::is_same_v<T, Cast_expr>) {
                return expr.target_type;
            } else {
                return expr.type;
            }
        },
        node);
}

inline const types::Type_id &get_type(const Expr::Node &node)
{
    return std::visit(
        [](const auto &expr) -> const types::Type_id & {
            using T = std::decay_t<decltype(expr)>;
            if constexpr (std::is_same_v<T, Cast_expr>) {
                return expr.target_type;
            } else {
                return expr.type;
            }
        },
        node);
}

inline Source_location &get_loc(Expr::Node &node)
{
    return std::visit([](auto &expr) -> Source_location & { return expr.loc; }, node);
}

inline Source_location get_loc(const Expr::Node &node)
{
    return std::visit([](const auto &expr) -> Source_location { return expr.loc; }, node);
}

inline Source_location &get_loc(Stmt::Node &node)
{
    return std::visit([](auto &stmt) -> Source_location & { return stmt.loc; }, node);
}

inline Source_location get_loc(const Stmt::Node &node)
{
    return std::visit([](const auto &stmt) -> Source_location { return stmt.loc; }, node);
}

} // namespace phos::ast
