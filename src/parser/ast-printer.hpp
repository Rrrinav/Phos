#pragma once
#include <print>
#include <string>
#include <variant>
#include <memory>
#include "../parser/ast.hpp"
#include "../utility/utility.hpp"

namespace phos::ast
{

class AstPrinter
{
    bool use_unicode;                 // whether to use Unicode tree chars
    std::vector<bool> indent_guides;  // tracks which branches continue

    std::string branch(bool has_next) const
    {
        if (use_unicode)
            return has_next ? "├── " : "└── ";
        else
            return has_next ? "|-- " : "\\-- ";
    }

    std::string vertical(bool has_next) const
    {
        if (use_unicode)
            return has_next ? "│   " : "    ";
        else
            return has_next ? "|   " : "    ";
    }

    void print_indent() const
    {
        for (size_t i = 0; i < indent_guides.size(); ++i)
            if (i + 1 == indent_guides.size())
                std::print("{}", branch(indent_guides[i]));
            else
                std::print("{}", vertical(indent_guides[i]));
    }

    void println(const std::string &s) const { std::println("{}", s); }

public:
    explicit AstPrinter(bool unicode = true) : use_unicode(unicode) {}

    void print_statements(const std::vector<std::unique_ptr<Stmt>> &statements)
    {
        for (size_t i = 0; i < statements.size(); ++i)
        {
            indent_guides.clear();
            print_stmt_ptr(statements[i], i + 1 < statements.size());
            std::println();
        }
    }

    void print_expr_ptr(const std::unique_ptr<Expr> &expr, bool has_next = false)
    {
        if (expr)
            print_expr(*expr, has_next);
        else
        {
            print_indent();
            println("null");
        }
    }

    void print_stmt_ptr(const std::unique_ptr<Stmt> &stmt, bool has_next = false)
    {
        if (stmt)
            print_stmt(*stmt, has_next);
        else
        {
            print_indent();
            println("null");
        }
    }

    void print_expr(const Expr &expr, bool has_next = false)
    {
        indent_guides.push_back(has_next);
        std::visit([this](auto &&node) { print_expr_node(node); }, expr.node);
        indent_guides.pop_back();
    }

    void print_stmt(const Stmt &stmt, bool has_next = false)
    {
        indent_guides.push_back(has_next);
        std::visit([this](auto &&node) { print_stmt_node(node); }, stmt.node);
        indent_guides.pop_back();
    }

private:
    // --- Expressions ---
    void print_expr_node(const Literal_expr &node)
    {
        print_indent();
        println("Literal: " + util::value_to_string(node.value));
        indent_guides.push_back(false);
        print_indent();
        println("Type: " + node.type.to_string());
        indent_guides.pop_back();
    }

    void print_expr_node(const Variable_expr &node)
    {
        print_indent();
        println("Variable: " + node.name);
        indent_guides.push_back(false);
        print_indent();
        println("Type: " + node.type.to_string());
        indent_guides.pop_back();
    }

    void print_expr_node(const Binary_expr &node)
    {
        print_indent();
        println("Binary_expr");
        indent_guides.push_back(true);
        print_indent();
        println("Operator: " + std::to_string(static_cast<int>(node.op)));
        print_indent();
        println("Type: " + node.type.to_string());
        indent_guides.back() = true;
        print_expr_ptr(node.left, true);
        indent_guides.back() = false;
        print_expr_ptr(node.right, false);
        indent_guides.pop_back();
    }

    void print_expr_node(const Call_expr &node)
    {
        print_indent();
        println("Call: " + node.callee);
        indent_guides.push_back(!node.arguments.empty());
        print_indent();
        println("Type: " + node.type.to_string());
        indent_guides.pop_back();

        // Print arguments with proper indentation:
        for (size_t i = 0; i < node.arguments.size(); ++i) print_expr_ptr(node.arguments[i], i + 1 < node.arguments.size());
    }

    void print_expr_node(const Assignment_expr &node)
    {
        print_indent();
        println("Assign: " + node.name);
        indent_guides.push_back(true);
        print_indent();
        println("Type: " + node.type.to_string());
        indent_guides.back() = false;
        print_expr_ptr(node.value, false);
        indent_guides.pop_back();
    }

    void print_expr_node(const Cast_expr &node)
    {
        print_indent();
        println("Cast_expr");
        indent_guides.push_back(true);
        print_indent();
        println("Type: " + node.type.to_string());
        indent_guides.back() = false;
        print_expr_ptr(node.expression, false);
        indent_guides.pop_back();
    }

    // Also fix the Unary expression:
    void print_expr_node(const Unary_expr &node)
    {
        print_indent();
        println("Unary_expr");
        indent_guides.push_back(true);
        print_indent();
        println("Type: " + node.type.to_string());
        indent_guides.back() = false;
        print_expr_ptr(node.right, false);
        indent_guides.pop_back();
    }
    // --- Statements ---
    void print_stmt_node(const Return_stmt &node)
    {
        print_indent();
        println("Return");
        if (node.expression)
            print_expr_ptr(node.expression, false);
    }

    void print_stmt_node(const Function_stmt &node)
    {
        print_indent();
        println("Function: " + node.name);
        for (size_t i = 0; i < node.parameters.size(); ++i)
        {
            print_indent();
            println("Param: " + node.parameters[i].first + " : " + node.parameters[i].second.to_string());
        }
        print_indent();
        println("Return type: " + node.return_type.to_string());
        print_stmt_ptr(node.body, false);
    }

    void print_stmt_node(const Var_stmt &node)
    {
        print_indent();
        println("Var: " + node.name + " : " + node.type.to_string());
        if (node.initializer)
            print_expr_ptr(node.initializer, false);
    }

    void print_stmt_node(const Print_stmt &node)
    {
        print_indent();
        println(std::string("Print(") + (node.stream == Print_stream::STDOUT ? "stdout)" : "stderr)"));
        print_expr_ptr(node.expression, false);
    }

    void print_stmt_node(const Expr_stmt &node)
    {
        print_indent();
        println("Expr_stmt");
        print_expr_ptr(node.expression, false);
    }

    void print_stmt_node(const Block_stmt &node)
    {
        print_indent();
        println("Block");
        for (size_t i = 0; i < node.statements.size(); ++i) print_stmt_ptr(node.statements[i], i + 1 < node.statements.size());
    }

    void print_stmt_node(const If_stmt &node)
    {
        print_indent();
        println("If");
        print_expr_ptr(node.condition, true);
        print_stmt_ptr(node.then_branch, node.else_branch != nullptr);
        if (node.else_branch)
            print_stmt_ptr(node.else_branch, false);
    }

    void print_stmt_node(const While_stmt &node)
    {
        print_indent();
        println("While");
        print_expr_ptr(node.condition, true);
        print_stmt_ptr(node.body, false);
    }
};

}  // namespace phos::ast
