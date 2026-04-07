#pragma once

#include "ast.hpp"

#include <string>
#include <vector>

namespace phos::ast {

// =============================================================================
// AST Printer (Tree representation)
// =============================================================================
class AstPrinter
{
public:
    // Added `simple_mode`. If true, prints clean indentation instead of tree lines.
    explicit AstPrinter(bool unicode = true, bool simple_mode = false);
    void print_statements(const std::vector<Stmt *> &statements);

    bool use_unicode;
    bool simple_mode;

private:
    std::vector<bool> branch_stack;

    void print_expr_ptr(const Expr *expr);
    void print_stmt_ptr(const Stmt *stmt);
    void print_expr(const Expr &expr);
    void print_stmt(const Stmt &stmt);

    // --- Expression Visitors ---
    void print_expr_node(const Literal_expr &node);
    void print_expr_node(const Variable_expr &node);
    void print_expr_node(const Binary_expr &node);
    void print_expr_node(const Unary_expr &node);
    void print_expr_node(const Call_expr &node);
    void print_expr_node(const Assignment_expr &node);
    void print_expr_node(const Field_assignment_expr &node);
    void print_expr_node(const Field_access_expr &node);
    void print_expr_node(const Method_call_expr &node);
    void print_expr_node(const Model_literal_expr &node);
    void print_expr_node(const Closure_expr &node);
    void print_expr_node(const Cast_expr &node);
    void print_expr_node(const Array_literal_expr &node);
    void print_expr_node(const Array_access_expr &node);
    void print_expr_node(const Array_assignment_expr &node);
    void print_expr_node(const Static_path_expr &node);
    void print_expr_node(const Range_expr &node);
    void print_expr_node(const Spawn_expr &node);
    void print_expr_node(const Await_expr &node);
    void print_expr_node(const Yield_expr &node);
    void print_expr_node(const Fstring_expr &node);
    void print_expr_node(const Enum_member_expr &node);
    void print_expr_node(const Anon_model_literal_expr &node);

    // --- Statement Visitors ---
    void print_stmt_node(const Return_stmt &node);
    void print_stmt_node(const Function_stmt &node);
    void print_stmt_node(const Model_stmt &node);
    void print_stmt_node(const Var_stmt &node);
    void print_stmt_node(const Print_stmt &node);
    void print_stmt_node(const Expr_stmt &node);
    void print_stmt_node(const Block_stmt &node);
    void print_stmt_node(const If_stmt &node);
    void print_stmt_node(const While_stmt &node);
    void print_stmt_node(const For_stmt &node);
    void print_stmt_node(const For_in_stmt &node);
    void print_stmt_node(const Union_stmt &node);
    void print_stmt_node(const Match_stmt &node);
    void print_stmt_node(const Enum_stmt &node);

    // --- Helper Methods ---
    std::string branch_sym(bool has_next) const;
    std::string vertical_sym(bool has_next) const;
    void indent() const;
    void print_str(const std::string &s) const;

    template <typename F>
    void with_child(bool has_next, F f)
    {
        branch_stack.push_back(has_next);
        f();
        branch_stack.pop_back();
    }
};

// =============================================================================
// Source Printer (Reconstructs source code from AST)
// =============================================================================
class Source_printer
{
public:
    Source_printer() = default;

    std::string print_statements(const std::vector<Stmt *> &statements);
    std::string print_expr_ptr(const Expr *expr);
    std::string print_stmt_ptr(const Stmt *stmt);

private:
    int indent_level = 0;
    std::string get_indent() const;

    // --- Expression Visitors ---
    std::string print_node(const Literal_expr &node);
    std::string print_node(const Variable_expr &node);
    std::string print_node(const Binary_expr &node);
    std::string print_node(const Unary_expr &node);
    std::string print_node(const Call_expr &node);
    std::string print_node(const Assignment_expr &node);
    std::string print_node(const Field_assignment_expr &node);
    std::string print_node(const Field_access_expr &node);
    std::string print_node(const Method_call_expr &node);
    std::string print_node(const Model_literal_expr &node);
    std::string print_node(const Closure_expr &node);
    std::string print_node(const Cast_expr &node);
    std::string print_node(const Array_literal_expr &node);
    std::string print_node(const Array_access_expr &node);
    std::string print_node(const Array_assignment_expr &node);
    std::string print_node(const Static_path_expr &node);
    std::string print_node(const Range_expr &node);
    std::string print_node(const Spawn_expr &node);
    std::string print_node(const Await_expr &node);
    std::string print_node(const Yield_expr &node);
    std::string print_node(const Fstring_expr &node);
    std::string print_node(const Enum_member_expr &node);
    std::string print_node(const Anon_model_literal_expr &node);

    // --- Statement Visitors ---
    std::string print_node(const Return_stmt &node);
    std::string print_node(const Function_stmt &node);
    std::string print_node(const Model_stmt &node);
    std::string print_node(const Var_stmt &node);
    std::string print_node(const Print_stmt &node);
    std::string print_node(const Expr_stmt &node);
    std::string print_node(const Block_stmt &node);
    std::string print_node(const If_stmt &node);
    std::string print_node(const While_stmt &node);
    std::string print_node(const For_stmt &node);
    std::string print_node(const For_in_stmt &node);
    std::string print_node(const Union_stmt &node);
    std::string print_node(const Match_stmt &node);
    std::string print_node(const Enum_stmt &node);
};

} // namespace phos::ast
