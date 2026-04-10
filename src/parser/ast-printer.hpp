#pragma once

#include "ast.hpp"

#include <string>
#include <vector>

namespace phos::ast {

// =============================================================================
// AST Tree Printer (Visual branch representation)
// =============================================================================
class Tree_printer
{
public:
    explicit Tree_printer(const Ast_tree &tree, const types::Type_table &tt, bool unicode = true);

    std::string print_statements(const std::vector<Stmt_id> &statements);
    std::string print_expr(Expr_id expr);
    std::string print_stmt(Stmt_id stmt);

    bool use_unicode;

private:
    const Ast_tree &tree;
    const types::Type_table &tt;

    std::vector<bool> branch_stack;
    std::string output;

    void visit(Expr_id expr);
    void visit(Stmt_id stmt);

    // --- Expression Visitors ---
    void print_node(const Literal_expr &node);
    void print_node(const Variable_expr &node);
    void print_node(const Binary_expr &node);
    void print_node(const Unary_expr &node);
    void print_node(const Call_expr &node);
    void print_node(const Assignment_expr &node);
    void print_node(const Field_assignment_expr &node);
    void print_node(const Array_assignment_expr &node);
    void print_node(const Cast_expr &node);
    void print_node(const Field_access_expr &node);
    void print_node(const Method_call_expr &node);
    void print_node(const Model_literal_expr &node);
    void print_node(const Closure_expr &node);
    void print_node(const Array_literal_expr &node);
    void print_node(const Array_access_expr &node);
    void print_node(const Static_path_expr &node);
    void print_node(const Range_expr &node);
    void print_node(const Spawn_expr &node);
    void print_node(const Await_expr &node);
    void print_node(const Yield_expr &node);
    void print_node(const Fstring_expr &node);
    void print_node(const Enum_member_expr &node);
    void print_node(const Anon_model_literal_expr &node);

    // --- Statement Visitors ---
    void print_node(const Return_stmt &node);
    void print_node(const Function_stmt &node);
    void print_node(const Model_stmt &node);
    void print_node(const Union_stmt &node);
    void print_node(const Var_stmt &node);
    void print_node(const Print_stmt &node);
    void print_node(const Expr_stmt &node);
    void print_node(const Block_stmt &node);
    void print_node(const If_stmt &node);
    void print_node(const While_stmt &node);
    void print_node(const For_stmt &node);
    void print_node(const For_in_stmt &node);
    void print_node(const Match_stmt &node);
    void print_node(const Enum_stmt &node);

    // --- Helper Methods ---
    std::string branch_sym(bool has_next) const;
    std::string vertical_sym(bool has_next) const;
    void indent();
    void print_str(const std::string &s);

    template <typename F>
    void with_child(bool has_next, F f)
    {
        branch_stack.push_back(has_next);
        f();
        branch_stack.pop_back();
    }
};

// =============================================================================
// AST S-Expression Printer (Lisp-like representation)
// =============================================================================
class Sexpr_printer
{
public:
    explicit Sexpr_printer(const Ast_tree &tree, const types::Type_table &tt);

    std::string print_statements(const std::vector<Stmt_id> &statements);
    std::string print_expr(Expr_id expr);
    std::string print_stmt(Stmt_id stmt);

private:
    const Ast_tree &tree;
    const types::Type_table &tt;

    // --- Expression Visitors ---
    std::string print_node(const Literal_expr &node);
    std::string print_node(const Variable_expr &node);
    std::string print_node(const Binary_expr &node);
    std::string print_node(const Unary_expr &node);
    std::string print_node(const Call_expr &node);
    std::string print_node(const Assignment_expr &node);
    std::string print_node(const Field_assignment_expr &node);
    std::string print_node(const Array_assignment_expr &node);
    std::string print_node(const Cast_expr &node);
    std::string print_node(const Field_access_expr &node);
    std::string print_node(const Method_call_expr &node);
    std::string print_node(const Model_literal_expr &node);
    std::string print_node(const Closure_expr &node);
    std::string print_node(const Array_literal_expr &node);
    std::string print_node(const Array_access_expr &node);
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
    std::string print_node(const Union_stmt &node);
    std::string print_node(const Var_stmt &node);
    std::string print_node(const Print_stmt &node);
    std::string print_node(const Expr_stmt &node);
    std::string print_node(const Block_stmt &node);
    std::string print_node(const If_stmt &node);
    std::string print_node(const While_stmt &node);
    std::string print_node(const For_stmt &node);
    std::string print_node(const For_in_stmt &node);
    std::string print_node(const Match_stmt &node);
    std::string print_node(const Enum_stmt &node);
};

} // namespace phos::ast
