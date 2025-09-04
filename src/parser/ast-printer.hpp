#pragma once
#include <print>
#include <string>
#include <variant>
#include <memory>
#include <vector>
#include "../parser/ast.hpp"
#include "../utility/utility.hpp"

namespace phos::ast {

class AstPrinter {
    std::vector<bool> branch_stack; // true = there are further siblings at this depth

    // Symbols (unicode or ascii)
    std::string branch_sym(bool has_next) const { return use_unicode ? (has_next ? "├── " : "└── ") : (has_next ? "+-- " : "`-- "); }
    std::string vertical_sym(bool has_next) const { return use_unicode ? (has_next ? "│   " : "    ") : (has_next ? "|   " : "    "); }

    // Print the indentation prefix according to branch_stack
    void indent() const {
        for (size_t i = 0; i + 1 < branch_stack.size(); ++i)
            std::print("{}", vertical_sym(branch_stack[i]));
        if (!branch_stack.empty())
            std::print("{}", branch_sym(branch_stack.back()));
    }

    // Utility to run a printing lambda with a pushed branch flag
    template <typename Fn>
    void with_child(bool has_next, Fn &&fn) {
        branch_stack.push_back(has_next);
        fn();
        branch_stack.pop_back();
    }

    void print_str(const std::string &s = "") const { std::println("{}", s); }

public:
    bool use_unicode = true;
    explicit AstPrinter(bool unicode = true) : use_unicode(unicode) {}

    // Top-level: print a list of statements
    void print_statements(const std::vector<std::unique_ptr<Stmt>> &statements) {
        for (size_t i = 0; i < statements.size(); ++i) {
            with_child(i + 1 < statements.size(), [&] { print_stmt_ptr(statements[i]); });
        }
    }

    // Pointer helpers (do NOT manipulate branch_stack here; caller decides sibling flags)
    void print_expr_ptr(const std::unique_ptr<Expr> &expr) {
        if (expr) print_expr(*expr);
        else {
            indent();
            print_str("null");
        }
    }

    void print_stmt_ptr(const std::unique_ptr<Stmt> &stmt) {
        if (stmt) print_stmt(*stmt);
        else {
            indent();
            print_str("null");
        }
    }

    // Visit the variant node
    void print_expr(const Expr &expr) {
        std::visit([this](auto &&node) { print_expr_node(node); }, expr.node);
    }
    void print_stmt(const Stmt &stmt) {
        std::visit([this](auto &&node) { print_stmt_node(node); }, stmt.node);
    }

private:
    // ---------- Expressions (all print Type where applicable) ----------
    void print_expr_node(const Literal_expr &node) {
        indent(); print_str("Literal: " + util::value_to_string(node.value));
        // single child => Type
        with_child(false, [&] {
            indent(); print_str("Type: " + node.type.to_string());
        });
    }

    void print_expr_node(const Variable_expr &node) {
        indent(); print_str("Variable: " + node.name);
        with_child(false, [&] {
            indent(); print_str("Type: " + node.type.to_string());
        });
    }

    void print_expr_node(const Binary_expr &node) {
        indent(); print_str("Binary_expr");

        // children: Operator, Type, Left, Right
        with_child(true, [&] {
            indent(); print_str("Operator: " + util::operator_token_to_string(node.op));
        });

        with_child(true, [&] {
            indent(); print_str("Type: " + node.type.to_string());
        });

        // Left (has more siblings after it => true)
        with_child(true, [&] {
            indent(); print_str("Left");
            // actual left expression (single child of "Left")
            with_child(false, [&] { print_expr_ptr(node.left); });
        });

        // Right (last child => false)
        with_child(false, [&] {
            indent(); print_str("Right");
            with_child(false, [&] { print_expr_ptr(node.right); });
        });
    }

    void print_expr_node(const Unary_expr &node) {
        indent(); print_str("Unary_expr");

        // children: Operator, Type, Right
        with_child(true, [&] {
            indent(); print_str("Operator: " + util::operator_token_to_string(node.op));
        });

        with_child(true, [&] {
            indent(); print_str("Type: " + node.type.to_string());
        });

        with_child(false, [&] {
            indent(); print_str("Right");
            with_child(false, [&] { print_expr_ptr(node.right); });
        });
    }

    void print_expr_node(const Call_expr &node)
    {
        indent();
        print_str("Call: " + node.callee);

        bool has_args = !node.arguments.empty();

        // First child: Type
        with_child(has_args,
                   [&]
                   {
                       indent();
                       print_str("Type: " + node.type.to_string());
                   });

        // Second child: Arguments (only if not empty)
        if (has_args)
        {
            with_child(false,
                       [&]
                       {
                           indent();
                           print_str("Arguments");
                           for (size_t i = 0; i < node.arguments.size(); ++i)
                               with_child(i + 1 < node.arguments.size(), [&] { print_expr_ptr(node.arguments[i]); });
                       });
        }
    }

    void print_expr_node(const Assignment_expr &node) {
        indent(); print_str("Assign: " + node.name);
        // children: Type, Value
        with_child(true, [&] {
            indent(); print_str("Type: " + node.type.to_string());
        });
        with_child(false, [&] {
            indent(); print_str("Value");
            with_child(false, [&] { print_expr_ptr(node.value); });
        });
    }

    void print_expr_node(const Cast_expr &node) {
        indent(); print_str("Cast");
        with_child(true, [&] {
            indent(); print_str("Type: " + node.type.to_string());
        });
        with_child(false, [&] {
            indent(); print_str("Expression");
            with_child(false, [&] { print_expr_ptr(node.expression); });
        });
    }

    // ---------- Statements ----------
    void print_stmt_node(const Return_stmt &node) {
        indent(); print_str("Return");
        if (node.expression) {
            with_child(false, [&] { print_expr_ptr(node.expression); });
        }
    }

    void print_stmt_node(const Function_stmt &node) {
        indent(); print_str("Function: " + node.name + " ( " + node.function_type.to_string() + " ) ");

        // Params (may be empty)
        with_child(true, [&] {
            indent(); print_str("Params");
            for (size_t i = 0; i < node.parameters.size(); ++i) {
                with_child(i + 1 < node.parameters.size(), [&] {
                    indent();
                    print_str(node.parameters[i].first + " : " + node.parameters[i].second.to_string());
                });
            }
        });

        // Return type
        with_child(true, [&] {
            indent(); print_str("Return type: " + node.return_type.to_string());
        });

        // Body
        with_child(false, [&] {
            indent(); print_str("Body");
            with_child(false, [&] { print_stmt_ptr(node.body); });
        });
    }

    void print_stmt_node(const Var_stmt &node) {
        indent(); print_str("Var: " + node.name + " : " + node.type.to_string());
        if (node.initializer) {
            with_child(false, [&] {
                indent(); print_str("Initializer");
                with_child(false, [&] { print_expr_ptr(node.initializer); });
            });
        }
    }

    void print_stmt_node(const Print_stmt &node) {
        indent();
        print_str(std::string("Print(") + (node.stream == Print_stream::STDOUT ? "stdout" : "stderr") + ")");
        with_child(false, [&] { print_expr_ptr(node.expression); });
    }

    void print_stmt_node(const Expr_stmt &node) {
        indent(); print_str("Expr_stmt");
        with_child(false, [&] { print_expr_ptr(node.expression); });
    }

    void print_stmt_node(const Block_stmt &node) {
        indent(); print_str("Block");
        for (size_t i = 0; i < node.statements.size(); ++i)
            with_child(i + 1 < node.statements.size(), [&] { print_stmt_ptr(node.statements[i]); });
    }

    void print_stmt_node(const If_stmt &node) {
        indent(); print_str("If");
        // Cond
        with_child(true, [&] {
            indent(); print_str("Cond");
            with_child(false, [&] { print_expr_ptr(node.condition); });
        });
        // Then
        with_child(node.else_branch != nullptr, [&] {
            indent(); print_str("Then");
            with_child(false, [&] { print_stmt_ptr(node.then_branch); });
        });
        // Else
        if (node.else_branch) {
            with_child(false, [&] {
                indent(); print_str("Else");
                with_child(false, [&] { print_stmt_ptr(node.else_branch); });
            });
        }
    }

    void print_stmt_node(const While_stmt &node) {
        indent(); print_str("While");
        with_child(true, [&] {
            indent(); print_str("Cond");
            with_child(false, [&] { print_expr_ptr(node.condition); });
        });
        with_child(false, [&] {
            indent(); print_str("Body");
            with_child(false, [&] { print_stmt_ptr(node.body); });
        });
    }
};

} // namespace phos::ast

