#pragma once

#include "../error/err.hpp"
#include "../parser/ast.hpp"
#include "../value/type.hpp"
#include "../memory/arena.hpp"
#include "scope_tracker.hpp"
#include "type_environment.hpp"

#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

namespace phos {

// Enforces type rules and logic utilizing the fully populated Type_environment.
// Strictly decoupled from declaration tracking.
class Semantic_checker
{
    enum class Projection_kind { Field, Var_Index, Int_Index, Uint_Index };

    struct Projection
    {
        Projection_kind kind;
        std::string name_val; // Used for Field names and Variable indices (e.g. "i")
        std::int64_t int_val = 0;
        std::uint64_t uint_val = 0;

        bool operator==(const Projection &o) const
        {
            if (kind != o.kind) {
                return false;
            }
            if (kind == Projection_kind::Field || kind == Projection_kind::Var_Index) {
                return name_val == o.name_val;
            }
            if (kind == Projection_kind::Int_Index) {
                return int_val == o.int_val;
            }
            return uint_val == o.uint_val;
        }
    };

    struct Access_path
    {
        std::string base_variable;
        std::vector<Projection> projections;

        bool operator==(const Access_path &o) const
        {
            return base_variable == o.base_variable && projections == o.projections;
        }
    };

    struct Access_path_hash
    {
        size_t operator()(const Access_path &p) const
        {
            size_t h = std::hash<std::string>{}(p.base_variable);
            for (const auto &proj : p.projections) {
                h ^= std::hash<int>{}(static_cast<int>(proj.kind)) + 0x9e3779b9 + (h << 6) + (h >> 2);
                if (proj.kind == Projection_kind::Field || proj.kind == Projection_kind::Var_Index) {
                    h ^= std::hash<std::string>{}(proj.name_val) + 0x9e3779b9 + (h << 6) + (h >> 2);
                } else if (proj.kind == Projection_kind::Int_Index) {
                    h ^= std::hash<std::int64_t>{}(proj.int_val) + 0x9e3779b9 + (h << 6) + (h >> 2);
                } else {
                    h ^= std::hash<std::uint64_t>{}(proj.uint_val) + 0x9e3779b9 + (h << 6) + (h >> 2);
                }
            }
            return h;
        }
    };

    ast::Ast_tree &tree;
    Type_environment &env;
    phos::mem::Arena& arena_;
public:
    Semantic_checker(ast::Ast_tree &tree, Type_environment &env, mem::Arena& arena);

    std::vector<err::msg> check(const std::vector<ast::Stmt_id> &statements);

    Scope_tracker variables;

    std::vector<std::unordered_set<Access_path, Access_path_hash>> m_nil_checked_vars_stack;
    std::optional<types::Type_id> current_return_type;
    std::optional<types::Type_id> current_model_type;

    std::vector<err::msg> errors;
    std::string phase = "semantic-checking";

    void type_error(const ast::Source_location &loc, const std::string &message);
    void type_warning(const ast::Source_location &loc, const std::string &message);

    // --- Core Logic ---
    bool is_compatible(types::Type_id expected, types::Type_id actual) const;
    types::Type_id promote_numeric_type(types::Type_id left, types::Type_id right) const;

    void declare(const std::string &name, types::Type_id type, bool is_mut, const ast::Source_location &loc);
    std::optional<Scope_symbol> lookup(const std::string &name, const ast::Source_location &loc);

    void hoist_globals(const std::vector<ast::Stmt_id> &statements);
    // --- Defaults & Nil Tracking ---
    bool default_expr_uses_forbidden_names(ast::Expr_id expr, const std::unordered_set<std::string> &forbidden_names) const;
    void validate_function_defaults(const ast::Function_stmt &stmt);
    void validate_model_defaults(const ast::Model_stmt &stmt);
    void validate_union_defaults(const ast::Union_stmt &stmt);

    std::optional<Access_path> extract_access_path(ast::Expr_id expr_id) const;

    void collect_nil_check_from_comparison(const ast::Binary_expr &expr, lex::TokenType target_op, std::unordered_set<Access_path, Access_path_hash> &out);
    void collect_nil_check_from_optional_method(const ast::Method_call_expr &expr, bool target_truthy_branch, std::unordered_set<Access_path, Access_path_hash> &out);
    void collect_nil_checked_vars_for_then(ast::Expr_id expr, std::unordered_set<Access_path, Access_path_hash> &out);
    void collect_nil_checked_vars_for_else(ast::Expr_id expr, std::unordered_set<Access_path, Access_path_hash> &out);

    // --- Binders & FFI ---
    struct Bound_call_arguments
    {
        std::vector<ast::Call_argument> ordered_arguments;
        bool ok = true;
    };

    struct Bound_native_arguments
    {
        std::vector<ast::Call_argument> ordered_arguments;
        std::unordered_map<std::string, types::Type_id> generics;
        bool ok = false;
    };

    Bound_call_arguments bind_call_arguments(
        const std::vector<ast::Function_param> &parameters,
        const std::vector<ast::Call_argument> &arguments,
        const ast::Source_location &call_loc,
        const std::string &call_kind,
        const std::string &call_name
    );

    Bound_native_arguments try_bind_native_arguments(
        const std::vector<types::Native_param> &parameters,
        const std::vector<ast::Call_argument> &arguments,
        std::optional<types::Type_id> receiver_type = std::nullopt);

    types::Type_id parse_type_string(std::string str, const std::unordered_map<std::string, types::Type_id> &generics) const;
    bool match_ffi_type(std::string expected_str, types::Type_id actual_type, std::unordered_map<std::string, types::Type_id> &generics) const;

    // --- Iterator Protocol ---
    bool is_iterator_protocol_type(types::Type_id type) const;
    types::Type_id iterator_element_type(types::Type_id type) const;
    types::Type_id to_iterator_type(types::Type_id type) const;

    // --- AST Walkers ---
    void check_stmt(ast::Stmt_id stmt);
    types::Type_id check_expr(ast::Expr_id expr, std::optional<types::Type_id> context_type = std::nullopt);

    void check_stmt_node(ast::Function_stmt &stmt);
    void check_stmt_node(ast::Model_stmt &stmt);
    void check_stmt_node(ast::Union_stmt &stmt);
    void check_stmt_node(ast::Enum_stmt &stmt);
    void check_stmt_node(ast::Block_stmt &stmt);
    void check_stmt_node(ast::Expr_stmt &stmt);
    void check_stmt_node(ast::If_stmt &stmt);
    void check_stmt_node(ast::Print_stmt &stmt);
    void check_stmt_node(ast::Return_stmt &stmt);
    void check_stmt_node(ast::Var_stmt &stmt);
    void check_stmt_node(ast::While_stmt &stmt);
    void check_stmt_node(ast::For_stmt &stmt);
    void check_stmt_node(ast::For_in_stmt &stmt);
    void check_stmt_node(ast::Match_stmt &stmt);

    types::Type_id check_expr_node(ast::Assignment_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Binary_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Call_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Cast_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Closure_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Field_access_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Static_path_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Enum_member_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Field_assignment_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Literal_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Method_call_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Model_literal_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Unary_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Variable_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Array_literal_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Array_access_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Array_assignment_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Range_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Spawn_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Await_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Yield_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Fstring_expr &expr, std::optional<types::Type_id> context_type);
    types::Type_id check_expr_node(ast::Anon_model_literal_expr &expr, std::optional<types::Type_id> context_type);
};

} // namespace phos
