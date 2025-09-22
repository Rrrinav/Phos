#pragma once

#include <memory>
#include <string>
#include <vector>

#include "../parser/ast.hpp"

namespace phos::ast {

class AstPrinter {
    std::vector<bool> branch_stack;  // true = there are further siblings at this depth

    std::string branch_sym(bool has_next) const;
    std::string vertical_sym(bool has_next) const;
    void indent() const;

    template <typename Fn>
    void with_child(bool has_next, Fn &&fn)
    {
        branch_stack.push_back(has_next);
        fn();
        branch_stack.pop_back();
    }

    void print_str(const std::string& s = "") const;

public:
    bool use_unicode = true;
    explicit AstPrinter(bool unicode = true);

    void print_statements(const std::vector<Stmt*>& statements);

    void print_expr_ptr(const Expr* expr);
    void print_stmt_ptr(const Stmt* stmt);

    void print_expr(const Expr& expr);
    void print_stmt(const Stmt& stmt);

private:
    // --- Expr visitors ---
    void print_expr_node(const Literal_expr& node);
    void print_expr_node(const Variable_expr& node);
    void print_expr_node(const Binary_expr& node);
    void print_expr_node(const Unary_expr& node);
    void print_expr_node(const Call_expr& node);
    void print_expr_node(const Assignment_expr& node);
    void print_expr_node(const Field_assignment_expr& node);
    void print_expr_node(const Field_access_expr& node);
    void print_expr_node(const Method_call_expr& node);
    void print_expr_node(const Model_literal_expr& node);
    void print_expr_node(const Closure_expr& node);
    void print_expr_node(const Cast_expr& node);
    void print_expr_node(const Array_literal_expr& node);
    void print_expr_node(const Array_access_expr& node);
    void print_expr_node(const Array_assignment_expr& node);

    // --- Stmt visitors ---
    void print_stmt_node(const Return_stmt& node);
    void print_stmt_node(const Function_stmt& node);
    void print_stmt_node(const Model_stmt& node);
    void print_stmt_node(const Var_stmt& node);
    void print_stmt_node(const Print_stmt& node);
    void print_stmt_node(const Expr_stmt& node);
    void print_stmt_node(const Field_assignment_expr& node);
    void print_stmt_node(const Block_stmt& node);
    void print_stmt_node(const If_stmt& node);
    void print_stmt_node(const While_stmt& node);
    void print_stmt_node(const For_stmt& node);
};

}  // namespace phos::ast
