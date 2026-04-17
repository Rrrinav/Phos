#include "ast-printer.hpp"

#include "../utility/utility.hpp"

#include <format>
#include <string>
#include <ranges>

static auto escape_view(std::string_view input)
{
    return input | std::views::transform([](char c) {
        switch (c) {
        case '\n': return std::string("\\n");
        case '\t': return std::string("\\t");
        case '\r': return std::string("\\r");
        case '\\': return std::string("\\\\");
        case '\"': return std::string("\\\"");
        default:   return std::string(1, c);
        }
    });
}

static std::string escape(std::string_view input)
{
    std::string out;
    for (auto &&s : escape_view(input)) {
        out += s;
    }
    return out;
}

namespace phos::ast {

// =============================================================================
// Tree_printer Implementation
// =============================================================================

Tree_printer::Tree_printer(const Ast_tree &tree, const types::Type_table &tt, bool unicode) : tree(tree), tt(tt), use_unicode(unicode)
{}

std::string Tree_printer::branch_sym(bool has_next) const
{
    return use_unicode ? (has_next ? "├── " : "└── ") : (has_next ? "+-- " : "\\-- ");
}

std::string Tree_printer::vertical_sym(bool has_next) const
{
    return use_unicode ? (has_next ? "│   " : "    ") : (has_next ? "|   " : "    ");
}

void Tree_printer::indent()
{
    for (size_t i = 0; i + 1 < branch_stack.size(); ++i) {
        output += vertical_sym(branch_stack[i]);
    }
    if (!branch_stack.empty()) {
        output += branch_sym(branch_stack.back());
    }
}

void Tree_printer::print_str(const std::string &s)
{
    output += s + "\n";
}

std::string Tree_printer::print_statements(const std::vector<Stmt_id> &statements)
{
    output.clear();
    for (size_t i = 0; i < statements.size(); ++i) {
        with_child(i + 1 < statements.size(), [&] { visit(statements[i]); });
    }
    return output;
}

std::string Tree_printer::print_expr(Expr_id expr)
{
    output.clear();
    visit(expr);
    return output;
}

std::string Tree_printer::print_stmt(Stmt_id stmt)
{
    output.clear();
    visit(stmt);
    return output;
}

void Tree_printer::visit(Expr_id expr)
{
    if (expr.is_null()) {
        print_str("<null>");
        return;
    }
    std::visit([this](auto const &e) { this->print_node(e); }, tree.get(expr).node);
}

void Tree_printer::visit(Stmt_id stmt)
{
    if (stmt.is_null()) {
        print_str("<null>");
        return;
    }
    std::visit([this](auto const &s) { this->print_node(s); }, tree.get(stmt).node);
}

// ---------- Tree Expressions ----------

void Tree_printer::print_node(const Literal_expr &node)
{
    indent();
    print_str("Literal: " + node.value.to_string());
    with_child(false, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
}

void Tree_printer::print_node(const Variable_expr &node)
{
    indent();
    print_str("Variable: " + node.name);
    with_child(false, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
}

void Tree_printer::print_node(const Binary_expr &node)
{
    indent();
    print_str("Binary_expr");
    with_child(true, [&] {
        indent();
        print_str("Operator: " + util::operator_token_to_string(node.op));
    });
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(true, [&] {
        indent();
        print_str("Left");
        with_child(false, [&] { visit(node.left); });
    });
    with_child(false, [&] {
        indent();
        print_str("Right");
        with_child(false, [&] { visit(node.right); });
    });
}

void Tree_printer::print_node(const Unary_expr &node)
{
    indent();
    print_str("Unary_expr");
    with_child(true, [&] {
        indent();
        print_str("Operator: " + util::operator_token_to_string(node.op));
    });
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(false, [&] {
        indent();
        print_str("Right");
        with_child(false, [&] { visit(node.right); });
    });
}

void Tree_printer::print_node(const Call_expr &node)
{
    indent();
    print_str("Call Expression");
    bool has_args = !node.arguments.empty();
    with_child(true, [&] {
        indent();
        print_str("Callee");
        with_child(false, [&] { visit(node.callee); });
    });
    with_child(!has_args, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    if (has_args) {
        with_child(false, [&] {
            indent();
            print_str("Arguments");
            for (size_t i = 0; i < node.arguments.size(); ++i) {
                with_child(i + 1 < node.arguments.size(), [&] {
                    if (!node.arguments[i].name.empty()) {
                        indent();
                        print_str("Named: " + node.arguments[i].name);
                        with_child(false, [&] { visit(node.arguments[i].value); });
                    } else {
                        visit(node.arguments[i].value);
                    }
                });
            }
        });
    }
}

void Tree_printer::print_node(const Assignment_expr &node)
{
    indent();
    print_str("Assign: " + node.name);
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(false, [&] {
        indent();
        print_str("Value");
        with_child(false, [&] { visit(node.value); });
    });
}

void Tree_printer::print_node(const Field_assignment_expr &node)
{
    indent();
    print_str("Field Assign: " + node.field_name);
    with_child(true, [&] {
        indent();
        print_str("Object");
        with_child(false, [&] { visit(node.object); });
    });
    with_child(false, [&] {
        indent();
        print_str("Value");
        with_child(false, [&] { visit(node.value); });
    });
}

void Tree_printer::print_node(const Array_assignment_expr &node)
{
    indent();
    print_str("Array Assignment");
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(true, [&] {
        indent();
        print_str("Array");
        with_child(false, [&] { visit(node.array); });
    });
    with_child(true, [&] {
        indent();
        print_str("Index");
        with_child(false, [&] { visit(node.index); });
    });
    with_child(false, [&] {
        indent();
        print_str("Value");
        with_child(false, [&] { visit(node.value); });
    });
}

void Tree_printer::print_node(const Cast_expr &node)
{
    indent();
    print_str("Cast");
    with_child(true, [&] {
        indent();
        print_str("Target Type: " + tt.to_string(node.target_type));
    });
    with_child(false, [&] {
        indent();
        print_str("Expression");
        with_child(false, [&] { visit(node.expression); });
    });
}

void Tree_printer::print_node(const Field_access_expr &node)
{
    indent();
    print_str("Field Access: " + node.field_name);
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(false, [&] {
        indent();
        print_str("Object");
        with_child(false, [&] { visit(node.object); });
    });
}

void Tree_printer::print_node(const Method_call_expr &node)
{
    indent();
    print_str("Method Call: " + node.method_name);
    bool has_args = !node.arguments.empty();
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(has_args, [&] {
        indent();
        print_str("Object");
        with_child(false, [&] { visit(node.object); });
    });
    if (has_args) {
        with_child(false, [&] {
            indent();
            print_str("Arguments");
            for (size_t i = 0; i < node.arguments.size(); ++i) {
                with_child(i + 1 < node.arguments.size(), [&] {
                    if (!node.arguments[i].name.empty()) {
                        indent();
                        print_str("Named: " + node.arguments[i].name);
                        with_child(false, [&] { visit(node.arguments[i].value); });
                    } else {
                        visit(node.arguments[i].value);
                    }
                });
            }
        });
    }
}

void Tree_printer::print_node(const Model_literal_expr &node)
{
    indent();
    print_str("Model Literal: " + node.model_name);
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(false, [&] {
        indent();
        print_str("Fields");
        for (size_t i = 0; i < node.fields.size(); ++i) {
            with_child(i + 1 < node.fields.size(), [&] {
                indent();
                print_str("." + node.fields[i].first);
                if (!node.fields[i].second.is_null()) {
                    with_child(false, [&] { visit(node.fields[i].second); });
                }
            });
        }
    });
}

void Tree_printer::print_node(const Closure_expr &node)
{
    indent();
    print_str("Closure");
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(true, [&] {
        indent();
        print_str("Return Type: " + tt.to_string(node.return_type));
    });
    with_child(true, [&] {
        indent();
        print_str("Parameters");
        for (size_t i = 0; i < node.parameters.size(); ++i) {
            with_child(i + 1 < node.parameters.size(), [&] {
                indent();
                std::string text = (node.parameters[i].is_mut ? "mut " : "") + node.parameters[i].name + " : "
                    + tt.to_string(node.parameters[i].type);
                if (!node.parameters[i].default_value.is_null()) {
                    Sexpr_printer inline_printer(tree, tt);
                    text += " = " + inline_printer.print_expr(node.parameters[i].default_value);
                }
                print_str(text);
            });
        }
    });
    with_child(false, [&] {
        indent();
        print_str("Body");
        with_child(false, [&] { visit(node.body); });
    });
}

void Tree_printer::print_node(const Array_literal_expr &node)
{
    indent();
    print_str("Array Literal");
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(false, [&] {
        indent();
        print_str("Elements");
        for (size_t i = 0; i < node.elements.size(); ++i) {
            with_child(i + 1 < node.elements.size(), [&] { visit(node.elements[i]); });
        }
    });
}

void Tree_printer::print_node(const Array_access_expr &node)
{
    indent();
    print_str("Array Access");
    with_child(true, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
    with_child(true, [&] {
        indent();
        print_str("Array");
        with_child(false, [&] { visit(node.array); });
    });
    with_child(false, [&] {
        indent();
        print_str("Index");
        with_child(false, [&] { visit(node.index); });
    });
}

void Tree_printer::print_node(const Static_path_expr &node)
{
    indent();
    std::string base_name = "<unknown>";
    if (auto *var_expr = std::get_if<Variable_expr>(&tree.get(node.base).node)) {
        base_name = var_expr->name;
    }

    print_str("Static Path: " + base_name + "::" + node.member.lexeme);
    with_child(false, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
}

void Tree_printer::print_node(const Range_expr &node)
{
    indent();
    print_str("Range Expression (" + std::string(node.inclusive ? "..=" : "..") + ")");
    with_child(true, [&] {
        indent();
        print_str("Start");
        with_child(false, [&] { visit(node.start); });
    });
    with_child(false, [&] {
        indent();
        print_str("End");
        with_child(false, [&] { visit(node.end); });
    });
}

void Tree_printer::print_node(const Spawn_expr &node)
{
    indent();
    print_str("Spawn Thread");
    with_child(false, [&] {
        indent();
        print_str("Call");
        with_child(false, [&] { visit(node.call); });
    });
}

void Tree_printer::print_node(const Await_expr &node)
{
    indent();
    print_str("Await Thread");
    with_child(false, [&] {
        indent();
        print_str("Target");
        with_child(false, [&] { visit(node.thread); });
    });
}

void Tree_printer::print_node(const Yield_expr &node)
{
    indent();
    print_str("Yield");
    if (!node.value.is_null()) {
        with_child(false, [&] {
            indent();
            print_str("Value");
            with_child(false, [&] { visit(node.value); });
        });
    }
}

void Tree_printer::print_node(const Fstring_expr &node)
{
    indent();
    print_str("F-String: " + node.raw_template);
    with_child(false, [&] {
        indent();
        print_str("Interpolations");
        for (size_t i = 0; i < node.interpolations.size(); ++i) {
            with_child(i + 1 < node.interpolations.size(), [&] { visit(node.interpolations[i]); });
        }
    });
}

void Tree_printer::print_node(const Enum_member_expr &node)
{
    indent();
    print_str("Enum Member: ." + node.member_name);
    with_child(false, [&] {
        indent();
        print_str("Type: " + tt.to_string(node.type));
    });
}

void Tree_printer::print_node(const Anon_model_literal_expr &node)
{
    indent();
    print_str("Anonymous Model Literal");
    with_child(true, [&] {
        indent();
        print_str("Inferred Type: " + (tt.to_string(node.type)));
    });
    with_child(false, [&] {
        indent();
        print_str("Fields");
        for (size_t i = 0; i < node.fields.size(); ++i) {
            with_child(i + 1 < node.fields.size(), [&] {
                indent();
                print_str("." + node.fields[i].first);
                if (!node.fields[i].second.is_null()) {
                    with_child(false, [&] { visit(node.fields[i].second); });
                }
            });
        }
    });
}

// ---------- Tree Statements ----------

void Tree_printer::print_node(const Return_stmt &node)
{
    indent();
    print_str("Return");
    if (!node.expression.is_null()) {
        with_child(false, [&] { visit(node.expression); });
    }
}

void Tree_printer::print_node(const Function_stmt &node)
{
    indent();
    print_str(std::format("{} Function: {}", node.is_static ? "static" : "", node.name));
    with_child(true, [&] {
        indent();
        print_str("Parameters");
        for (size_t i = 0; i < node.parameters.size(); ++i) {
            with_child(i + 1 < node.parameters.size(), [&] {
                indent();
                std::string text = (node.parameters[i].is_mut ? "mut " : "") + node.parameters[i].name + " : "
                    + tt.to_string(node.parameters[i].type);
                if (!node.parameters[i].default_value.is_null()) {
                    Sexpr_printer inline_printer(tree, tt);
                    text += " = " + inline_printer.print_expr(node.parameters[i].default_value);
                }
                print_str(text);
            });
        }
    });
    with_child(true, [&] {
        indent();
        print_str("Return type: " + tt.to_string(node.return_type));
    });
    with_child(false, [&] {
        indent();
        print_str("Body");
        with_child(false, [&] { visit(node.body); });
    });
}

void Tree_printer::print_node(const Model_stmt &node)
{
    indent();
    print_str("Model: " + node.name);
    with_child(true, [&] {
        indent();
        print_str("Fields");
        if (node.fields.empty()) {
            with_child(false, [&] {
                indent();
                print_str("<none>");
            });
        } else {
            for (size_t i = 0; i < node.fields.size(); ++i) {
                with_child(i + 1 < node.fields.size(), [&] {
                    indent();
                    std::string text = node.fields[i].name + " : " + tt.to_string(node.fields[i].type);
                    if (!node.fields[i].default_value.is_null()) {
                        Sexpr_printer inline_printer(tree, tt);
                        text += " = " + inline_printer.print_expr(node.fields[i].default_value);
                    }
                    print_str(text);
                });
            }
        }
    });
    with_child(false, [&] {
        indent();
        print_str("Methods");
        if (node.methods.empty()) {
            with_child(false, [&] {
                indent();
                print_str("<none>");
            });
        } else {
            for (size_t i = 0; i < node.methods.size(); ++i) {
                with_child(i + 1 < node.methods.size(), [&] { visit(node.methods[i]); });
            }
        }
    });
}

void Tree_printer::print_node(const Union_stmt &node)
{
    indent();
    print_str("Union: " + node.name);
    with_child(false, [&] {
        indent();
        print_str("Variants");
        if (node.variants.empty()) {
            with_child(false, [&] {
                indent();
                print_str("<none>");
            });
        } else {
            for (size_t i = 0; i < node.variants.size(); ++i) {
                with_child(i + 1 < node.variants.size(), [&] {
                    indent();
                    std::string text = node.variants[i].name + " : " + tt.to_string(node.variants[i].type);
                    if (!node.variants[i].default_value.is_null()) {
                        Sexpr_printer inline_printer(tree, tt);
                        text += " = " + inline_printer.print_expr(node.variants[i].default_value);
                    }
                    print_str(text);
                });
            }
        }
    });
}

void Tree_printer::print_node(const Enum_stmt &node)
{
    indent();
    print_str("Enum: " + node.name);
    with_child(false, [&] {
        indent();
        print_str("Variants");
        if (node.variants.empty()) {
            with_child(false, [&] {
                indent();
                print_str("<none>");
            });
        } else {
            for (size_t i = 0; i < node.variants.size(); ++i) {
                with_child(i + 1 < node.variants.size(), [&] {
                    indent();
                    std::string text = node.variants[i].first;
                    if (node.variants[i].second.has_value()) {
                        text += " = " + node.variants[i].second.value().to_string();
                    }
                    print_str(text);
                });
            }
        }
    });
}

void Tree_printer::print_node(const Var_stmt &node)
{
    indent();
    print_str("Var: " + node.name + " : " + tt.to_string(node.type));
    if (!node.initializer.is_null()) {
        with_child(false, [&] {
            indent();
            print_str("Initializer");
            with_child(false, [&] { visit(node.initializer); });
        });
    }
}

void Tree_printer::print_node(const Print_stmt &node)
{
    indent();
    print_str(std::string(
        "Print(") + (node.stream == Print_stream::STDOUT ? "stdout" : "stderr") +
        ", sep=\"" + escape(node.sep) + "\", end=\"" + escape(node.end) + "\")"
    );

    with_child(false, [&] {
        for (const auto &exp : node.expressions) {
            visit(exp);
        }
    });
}

void Tree_printer::print_node(const Expr_stmt &node)
{
    indent();
    print_str("Expr_stmt");
    with_child(false, [&] { visit(node.expression); });
}

void Tree_printer::print_node(const Block_stmt &node)
{
    indent();
    print_str("Block");
    for (size_t i = 0; i < node.statements.size(); ++i) {
        with_child(i + 1 < node.statements.size(), [&] { visit(node.statements[i]); });
    }
}

void Tree_printer::print_node(const If_stmt &node)
{
    indent();
    print_str("If");
    with_child(true, [&] {
        indent();
        print_str("Cond");
        with_child(false, [&] { visit(node.condition); });
    });
    with_child(!node.else_branch.is_null(), [&] {
        indent();
        print_str("Then");
        with_child(false, [&] { visit(node.then_branch); });
    });
    if (!node.else_branch.is_null()) {
        with_child(false, [&] {
            indent();
            print_str("Else");
            with_child(false, [&] { visit(node.else_branch); });
        });
    }
}

void Tree_printer::print_node(const While_stmt &node)
{
    indent();
    print_str("While");
    with_child(true, [&] {
        indent();
        print_str("Cond");
        with_child(false, [&] { visit(node.condition); });
    });
    with_child(false, [&] {
        indent();
        print_str("Body");
        with_child(false, [&] { visit(node.body); });
    });
}

void Tree_printer::print_node(const For_stmt &node)
{
    indent();
    print_str("For");
    with_child(true, [&] {
        indent();
        print_str("Initializer");
        with_child(false, [&] { visit(node.initializer); });
    });
    with_child(true, [&] {
        indent();
        print_str("Condition");
        with_child(false, [&] { visit(node.condition); });
    });
    with_child(true, [&] {
        indent();
        print_str("Increment");
        with_child(false, [&] { visit(node.increment); });
    });
    with_child(false, [&] {
        indent();
        print_str("Body");
        with_child(false, [&] { visit(node.body); });
    });
}

void Tree_printer::print_node(const For_in_stmt &node)
{
    indent();
    print_str("For-In: " + node.var_name);
    with_child(true, [&] {
        indent();
        print_str("Iterable");
        with_child(false, [&] { visit(node.iterable); });
    });
    with_child(false, [&] {
        indent();
        print_str("Body");
        with_child(false, [&] { visit(node.body); });
    });
}

void Tree_printer::print_node(const Match_stmt &node)
{
    indent();
    print_str("Match");
    with_child(true, [&] {
        indent();
        print_str("Subject");
        with_child(false, [&] { visit(node.subject); });
    });
    with_child(false, [&] {
        indent();
        print_str("Arms");
        for (size_t i = 0; i < node.arms.size(); ++i) {
            with_child(i + 1 < node.arms.size(), [&] {
                if (node.arms[i].is_wildcard) {
                    indent();
                    print_str("Wildcard (_)");
                } else {
                    indent();
                    std::string bind_str = node.arms[i].bind_name.empty() ? "" : " (binds: " + node.arms[i].bind_name + ")";
                    print_str("Pattern" + bind_str);
                    with_child(true, [&] { visit(node.arms[i].pattern); });
                }
                indent();
                print_str("Body");
                with_child(false, [&] { visit(node.arms[i].body); });
            });
        }
    });
}

// =============================================================================
// Sexpr_printer Implementation (Lisp-like syntax)
// =============================================================================

Sexpr_printer::Sexpr_printer(const Ast_tree &tree, const types::Type_table &tt) : tree(tree), tt(tt)
{}

std::string Sexpr_printer::print_statements(const std::vector<Stmt_id> &statements)
{
    std::string res;
    for (size_t i = 0; i < statements.size(); ++i) {
        res += print_stmt(statements[i]);
        if (i + 1 < statements.size()) {
            res += "\n";
        }
    }
    return res;
}

std::string Sexpr_printer::print_expr(Expr_id expr)
{
    if (expr.is_null()) {
        return "()";
    }
    return std::visit([this](auto const &e) { return this->print_node(e); }, tree.get(expr).node);
}

std::string Sexpr_printer::print_stmt(Stmt_id stmt)
{
    if (stmt.is_null()) {
        return "()";
    }
    return std::visit([this](auto const &s) { return this->print_node(s); }, tree.get(stmt).node);
}

// ---------- S-Expressions ----------

std::string Sexpr_printer::print_node(const Literal_expr &node)
{
    if (node.value.is_string()) {
        return "\"" + std::string(node.value.as_string()) + "\"";
    }
    return std::string(node.value.as_string());
}

std::string Sexpr_printer::print_node(const Variable_expr &node)
{
    return node.name;
}

std::string Sexpr_printer::print_node(const Binary_expr &node)
{
    return "(" + util::operator_token_to_string(node.op) + " " + print_expr(node.left) + " " + print_expr(node.right) + ")";
}

std::string Sexpr_printer::print_node(const Unary_expr &node)
{
    return "(" + util::operator_token_to_string(node.op) + " " + print_expr(node.right) + ")";
}

std::string Sexpr_printer::print_node(const Call_expr &node)
{
    std::string res = "(call " + print_expr(node.callee);
    for (const auto &arg : node.arguments) {
        res += " ";
        if (!arg.name.empty()) {
            res += ":" + arg.name + " ";
        }
        res += print_expr(arg.value);
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const Assignment_expr &node)
{
    return "(= " + node.name + " " + print_expr(node.value) + ")";
}

std::string Sexpr_printer::print_node(const Field_assignment_expr &node)
{
    return "(= (. " + print_expr(node.object) + " " + node.field_name + ") " + print_expr(node.value) + ")";
}

std::string Sexpr_printer::print_node(const Array_assignment_expr &node)
{
    return "(= (idx " + print_expr(node.array) + " " + print_expr(node.index) + ") " + print_expr(node.value) + ")";
}

std::string Sexpr_printer::print_node(const Cast_expr &node)
{
    return "(as " + print_expr(node.expression) + " " + tt.to_string(node.target_type) + ")";
}

std::string Sexpr_printer::print_node(const Field_access_expr &node)
{
    return "(. " + print_expr(node.object) + " " + node.field_name + ")";
}

std::string Sexpr_printer::print_node(const Method_call_expr &node)
{
    std::string res = "(mcall " + print_expr(node.object) + " " + node.method_name;
    for (const auto &arg : node.arguments) {
        res += " ";
        if (!arg.name.empty()) {
            res += ":" + arg.name + " ";
        }
        res += print_expr(arg.value);
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const Model_literal_expr &node)
{
    std::string res = "(init " + node.model_name;
    for (const auto &field : node.fields) {
        res += " (:" + field.first + " " + (field.second.is_null() ? "default" : print_expr(field.second)) + ")";
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const Closure_expr &node)
{
    std::string res = "(closure (";
    for (size_t i = 0; i < node.parameters.size(); ++i) {
        res += node.parameters[i].name;
        if (i + 1 < node.parameters.size()) {
            res += " ";
        }
    }
    res += ") " + print_stmt(node.body) + ")";
    return res;
}

std::string Sexpr_printer::print_node(const Array_literal_expr &node)
{
    std::string res = "(list";
    for (const auto &elem : node.elements) {
        res += " " + print_expr(elem);
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const Array_access_expr &node)
{
    return "(idx " + print_expr(node.array) + " " + print_expr(node.index) + ")";
}

std::string Sexpr_printer::print_node(const Static_path_expr &node)
{
    return "(:: " + print_expr(node.base) + " " + node.member.lexeme + ")";
}

std::string Sexpr_printer::print_node(const Range_expr &node)
{
    std::string op = node.inclusive ? "..=" : "..";
    return "(" + op + " " + print_expr(node.start) + " " + print_expr(node.end) + ")";
}

std::string Sexpr_printer::print_node(const Spawn_expr &node)
{
    return "(spawn " + print_expr(node.call) + ")";
}

std::string Sexpr_printer::print_node(const Await_expr &node)
{
    return "(await " + print_expr(node.thread) + ")";
}

std::string Sexpr_printer::print_node(const Yield_expr &node)
{
    return "(yield" + std::string(node.value.is_null() ? "" : " " + print_expr(node.value)) + ")";
}

std::string Sexpr_printer::print_node(const Fstring_expr &node)
{
    std::string res = "(fstring \"" + node.raw_template + "\"";
    for (auto interp : node.interpolations) {
        res += " " + print_expr(interp);
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const Enum_member_expr &node)
{
    return "(. " + node.member_name + ")";
}

std::string Sexpr_printer::print_node(const Anon_model_literal_expr &node)
{
    std::string res = "(anon_init";
    for (const auto &field : node.fields) {
        res += " (:" + field.first + " " + (field.second.is_null() ? "default" : print_expr(field.second)) + ")";
    }
    return res + ")";
}

// ---------- S-Statements ----------

// TODO: Add types everywhere.
std::string Sexpr_printer::print_node(const Return_stmt &node)
{
    if (!node.expression.is_null()) {
        return "(return " + print_expr(node.expression) + ")";
    }
    return "(return)";
}

std::string Sexpr_printer::print_node(const Function_stmt &node)
{
    std::string res = "(fn " + node.name + " (";
    for (size_t i = 0; i < node.parameters.size(); ++i) {
        res += node.parameters[i].name + ": ";
        res += tt.to_string(node.parameters[i].type);
        if (i + 1 < node.parameters.size()) {
            res += " ";
        }
    }
    res += ") " + print_stmt(node.body) + ")";
    return res;
}

std::string Sexpr_printer::print_node(const Model_stmt &node)
{
    std::string res = "(model " + node.name + " (fields";
    for (const auto &f : node.fields) {
        res += " " + f.name;
    }
    res += ") (methods";
    for (const auto &m : node.methods) {
        res += " " + print_stmt(m);
    }
    return res + "))";
}

std::string Sexpr_printer::print_node(const Union_stmt &node)
{
    std::string res = "(union " + node.name;
    for (const auto &v : node.variants) {
        res += " " + v.name;
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const Var_stmt &node)
{
    std::string kw = node.is_const ? "const" : (node.is_mut ? "mut" : "let");
    if (!node.initializer.is_null()) {
        return "(" + kw + " " + node.name + " " + print_expr(node.initializer) + ")";
    }
    return "(" + kw + " " + node.name + ")";
}

std::string Sexpr_printer::print_node(const Print_stmt &node)
{
    std::string kw = (node.stream == Print_stream::STDOUT) ? "print" : "eprint";
    std::string res = "(" + kw;
    for (const auto &exp : node.expressions) {
        res += " " + print_expr(exp);
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const Expr_stmt &node)
{
    return print_expr(node.expression);
}

std::string Sexpr_printer::print_node(const Block_stmt &node)
{
    std::string res = "(block";
    for (const auto s : node.statements) {
        res += " " + print_stmt(s);
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const If_stmt &node)
{
    std::string res = "(if " + print_expr(node.condition) + " " + print_stmt(node.then_branch);
    if (!node.else_branch.is_null()) {
        res += " " + print_stmt(node.else_branch);
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const While_stmt &node)
{
    return "(while " + print_expr(node.condition) + " " + print_stmt(node.body) + ")";
}

std::string Sexpr_printer::print_node(const For_stmt &node)
{
    return "(for " + print_stmt(node.initializer) + " " + print_expr(node.condition) + " " + print_expr(node.increment) + " "
        + print_stmt(node.body) + ")";
}

std::string Sexpr_printer::print_node(const For_in_stmt &node)
{
    return "(for_in " + node.var_name + " " + print_expr(node.iterable) + " " + print_stmt(node.body) + ")";
}

std::string Sexpr_printer::print_node(const Match_stmt &node)
{
    std::string res = "(match " + print_expr(node.subject);
    for (const auto &arm : node.arms) {
        res += " (" + (arm.is_wildcard ? "_" : print_expr(arm.pattern));
        if (!arm.bind_name.empty()) {
            res += " :bind " + arm.bind_name;
        }
        res += " " + print_stmt(arm.body) + ")";
    }
    return res + ")";
}

std::string Sexpr_printer::print_node(const Enum_stmt &node)
{
    std::string res = "(enum " + node.name;
    for (const auto &var : node.variants) {
        res += " " + var.first;
    }
    return res + ")";
}

} // namespace phos::ast
