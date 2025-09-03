#pragma once
#include <print>
#include <string>
#include <variant>
#include <memory>
#include "../parser/ast.hpp"

#include "../utility/utility.hpp"

namespace phos
{
namespace ast
{

class AstPrinter
{
    int indent_level = 0;

    void indent() const
    {
        for (int i = 0; i < indent_level; ++i) std::print(" ");
    }

    void print_str(const std::string &s = "") const { std::println("{}", s); }

public:
    void print_statements(const std::vector<std::unique_ptr<Stmt>> &statements)
    {
        AstPrinter printer;
        for (const auto &stmt : statements)
        {
            printer.print_stmt_ptr(stmt);
            std::println();
        }
    }
    void print_expr_ptr(const std::unique_ptr<Expr> &expr)
    {
        if (expr)
            print_expr(*expr);
        else
            print_str("null");
    }

    void print_stmt_ptr(const std::unique_ptr<Stmt> &stmt)
    {
        if (stmt)
            print_stmt(*stmt);
        else
            print_str("null");
    }

    void print_expr(const Expr &expr)
    {
        std::visit([this](auto &&node) { print_expr_node(node); }, expr.node);
    }

    void print_stmt(const Stmt &stmt)
    {
        std::visit([this](auto &&node) { print_stmt_node(node); }, stmt.node);
    }

private:
    void print_expr_node(const Literal_expr &node)
    {
        indent();
        print_str("Literal_expr: " + util::value_to_string(node.value));
    }

    void print_expr_node(const Variable_expr &node)
    {
        indent();
        print_str("Variable_expr: " + node.name);
    }

    void print_expr_node(const Binary_expr &node)
    {
        indent();
        print_str("Binary_expr:");
        ++indent_level;
        indent();
        print_str("Operator: " + std::to_string(static_cast<int>(node.op)));
        indent();
        print_str("Left:");
        ++indent_level;
        print_expr_ptr(node.left);
        --indent_level;
        indent();
        print_str("Right:");
        ++indent_level;
        print_expr_ptr(node.right);
        --indent_level;
        --indent_level;
    }

    void print_expr_node(const Unary_expr &node)
    {
        indent();
        print_str("Unary_expr:");
        ++indent_level;
        indent();
        print_str("Operator: " + std::to_string(static_cast<int>(node.op)));
        indent();
        print_str("Right:");
        ++indent_level;
        print_expr_ptr(node.right);
        --indent_level;
        --indent_level;
    }

    void print_expr_node(const Call_expr &node)
    {
        indent();
        print_str("Call_expr: " + node.callee);
        ++indent_level;
        indent();
        print_str("Arguments:");
        ++indent_level;
        for (const auto &arg : node.arguments) print_expr_ptr(arg);
        --indent_level;
        --indent_level;
    }

    void print_expr_node(const Assignment_expr &node)
    {
        indent();
        print_str("Assignment_expr: " + node.name);
        ++indent_level;
        indent();
        print_str("Value:");
        ++indent_level;
        print_expr_ptr(node.value);
        --indent_level;
        --indent_level;
    }

    void print_expr_node(const Cast_expr &node)
    {
        indent();
        print_str("Cast_expr:");
        ++indent_level;
        indent();
        print_str("Expression:");
        ++indent_level;
        print_expr_ptr(node.expression);
        --indent_level;
        --indent_level;
    }

    void print_stmt_node(const Return_stmt &node)
    {
        indent();
        print_str("Return_stmt:");
        ++indent_level;
        indent();
        print_str("Expression:");
        ++indent_level;
        print_expr_ptr(node.expression);
        --indent_level;
        --indent_level;
    }

    void print_stmt_node(const Function_stmt &node)
    {
        indent();
        print_str("Function_stmt: " + node.name);
        ++indent_level;
        indent();
        print_str("Parameters:");
        ++indent_level;
        for (const auto &[name, type] : node.parameters)
        {
            indent();
            print_str(name + ": " + type.to_string());
        }
        --indent_level;
        indent();
        print_str("Return type: " + node.return_type.to_string());
        indent();
        print_str("Body:");
        ++indent_level;
        print_stmt_ptr(node.body);
        --indent_level;
        --indent_level;
    }

    void print_stmt_node(const Var_stmt &node)
    {
        indent();
        print_str("Var_stmt: " + node.name + " : " + node.type.to_string());
        if (node.initializer)
        {
            ++indent_level;
            indent();
            print_str("Initializer:");
            ++indent_level;
            print_expr_ptr(node.initializer);
            --indent_level;
            --indent_level;
        }
    }

    void print_stmt_node(const Print_stmt &node)
    {
        indent();
        print_str(std::string("Print_stmt: ") + std::string((node.stream == Print_stream::STDOUT ? "STDOUT" : "STDERR")));
        ++indent_level;
        print_expr_ptr(node.expression);
        --indent_level;
    }

    void print_stmt_node(const Expr_stmt &node)
    {
        indent();
        print_str("Expr_stmt:");
        ++indent_level;
        print_expr_ptr(node.expression);
        --indent_level;
    }

    void print_stmt_node(const Block_stmt &node)
    {
        indent();
        print_str("Block_stmt:");
        ++indent_level;
        for (const auto &stmt : node.statements) print_stmt_ptr(stmt);
        --indent_level;
    }

    void print_stmt_node(const If_stmt &node)
    {
        indent();
        print_str("If_stmt:");
        ++indent_level;
        indent();
        print_str("Condition:");
        ++indent_level;
        print_expr_ptr(node.condition);
        --indent_level;
        indent();
        print_str("Then branch:");
        ++indent_level;
        print_stmt_ptr(node.then_branch);
        --indent_level;
        if (node.else_branch)
        {
            indent();
            print_str("Else branch:");
            ++indent_level;
            print_stmt_ptr(node.else_branch);
            --indent_level;
        }
        --indent_level;
    }

    void print_stmt_node(const While_stmt &node)
    {
        indent();
        print_str("While_stmt:");
        ++indent_level;
        indent();
        print_str("Condition:");
        ++indent_level;
        print_expr_ptr(node.condition);
        --indent_level;
        indent();
        print_str("Body:");
        ++indent_level;
        print_stmt_ptr(node.body);
        --indent_level;
        --indent_level;
    }

    void print_stmt_node(const For_stmt &node)
    {
        indent();
        print_str("For_stmt:");
        ++indent_level;
        indent();
        print_str("Initializer:");
        ++indent_level;
        print_stmt_ptr(node.initializer);
        --indent_level;
        indent();
        print_str("Condition:");
        ++indent_level;
        print_expr_ptr(node.condition);
        --indent_level;
        indent();
        print_str("Increment:");
        ++indent_level;
        print_expr_ptr(node.increment);
        --indent_level;
        indent();
        print_str("Body:");
        ++indent_level;
        print_stmt_ptr(node.body);
        --indent_level;
        --indent_level;
    }
};

}  // namespace ast
}  // namespace phos
