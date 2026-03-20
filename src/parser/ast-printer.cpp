#include "ast-printer.hpp"
#include "../utility/utility.hpp"

#include <format>
#include <iostream>

namespace phos::ast
{

// =============================================================================
// AstPrinter Implementation
// =============================================================================

AstPrinter::AstPrinter(bool unicode, bool simple) : use_unicode(unicode), simple_mode(simple) {}

std::string AstPrinter::branch_sym(bool has_next) const
{
    return use_unicode ? (has_next ? "├── " : "└── ") : (has_next ? "+-- " : "\\-- ");
}

std::string AstPrinter::vertical_sym(bool has_next) const
{
    return use_unicode ? (has_next ? "│   " : "    ") : (has_next ? "|   " : "    ");
}

void AstPrinter::indent() const
{
    // The magic logic: clean indentation for large files
    if (simple_mode)
    {
        std::cout << std::string(branch_stack.size() * 2, ' ');
        return;
    }

    for (size_t i = 0; i + 1 < branch_stack.size(); ++i) std::cout << vertical_sym(branch_stack[i]);
    if (!branch_stack.empty())
        std::cout << branch_sym(branch_stack.back());
}

void AstPrinter::print_str(const std::string &s) const { std::cout << s << "\n"; }

void AstPrinter::print_statements(const std::vector<Stmt *> &statements)
{
    for (size_t i = 0; i < statements.size(); ++i) with_child(i + 1 < statements.size(), [&] { print_stmt_ptr(statements[i]); });
}

void AstPrinter::print_expr_ptr(const Expr *expr)
{
    if (!expr)
    {
        print_str("<nullptr>");
        return;
    }
    std::visit([this](auto const &e) { this->print_expr_node(e); }, expr->node);
}

void AstPrinter::print_stmt_ptr(const Stmt *stmt)
{
    if (!stmt)
    {
        print_str("<nullptr>");
        return;
    }
    std::visit([this](auto const &s) { this->print_stmt_node(s); }, stmt->node);
}

void AstPrinter::print_expr(const Expr &expr)
{
    std::visit([this](auto &&node) { print_expr_node(node); }, expr.node);
}

void AstPrinter::print_stmt(const Stmt &stmt)
{
    std::visit([this](auto &&node) { print_stmt_node(node); }, stmt.node);
}

// ---------- Expressions ----------

void AstPrinter::print_expr_node(const Literal_expr &node)
{
    indent();
    print_str("Literal: " + value_to_string(node.value));
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
}

void AstPrinter::print_expr_node(const Variable_expr &node)
{
    indent();
    print_str("Variable: " + node.name);
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
}

void AstPrinter::print_expr_node(const Binary_expr &node)
{
    indent();
    print_str("Binary_expr");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Operator: " + util::operator_token_to_string(node.op));
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Left");
                   with_child(false, [&] { print_expr_ptr(node.left); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Right");
                   with_child(false, [&] { print_expr_ptr(node.right); });
               });
}

void AstPrinter::print_expr_node(const Unary_expr &node)
{
    indent();
    print_str("Unary_expr");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Operator: " + util::operator_token_to_string(node.op));
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Right");
                   with_child(false, [&] { print_expr_ptr(node.right); });
               });
}

void AstPrinter::print_expr_node(const Call_expr &node)
{
    indent();
    print_str("Call Expression");
    bool has_args = !node.arguments.empty();
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Callee");
                   with_child(false, [&] { print_expr_ptr(node.callee); });
               });
    with_child(!has_args,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
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

void AstPrinter::print_expr_node(const Assignment_expr &node)
{
    indent();
    print_str("Assign: " + node.name);
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Value");
                   with_child(false, [&] { print_expr_ptr(node.value); });
               });
}

void AstPrinter::print_expr_node(const Field_assignment_expr &node)
{
    indent();
    print_str("Field Assign: " + node.field_name);
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Object");
                   with_child(false, [&] { print_expr_ptr(node.object); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Value");
                   with_child(false, [&] { print_expr_ptr(node.value); });
               });
}

void AstPrinter::print_expr_node(const Field_access_expr &node)
{
    indent();
    print_str("Field Access: " + node.field_name);
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Object");
                   with_child(false, [&] { print_expr_ptr(node.object); });
               });
}

void AstPrinter::print_expr_node(const Method_call_expr &node)
{
    indent();
    print_str("Method Call: " + node.method_name);
    bool has_args = !node.arguments.empty();
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(has_args,
               [&]
               {
                   indent();
                   print_str("Object");
                   with_child(false, [&] { print_expr_ptr(node.object); });
               });
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

void AstPrinter::print_expr_node(const Model_literal_expr &node)
{
    indent();
    print_str("Model Literal: " + node.model_name);
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Fields");
                   for (size_t i = 0; i < node.fields.size(); ++i)
                   {
                       with_child(i + 1 < node.fields.size(),
                                  [&]
                                  {
                                      indent();
                                      print_str("." + node.fields[i].first);
                                      with_child(false, [&] { print_expr_ptr(node.fields[i].second); });
                                  });
                   }
               });
}

void AstPrinter::print_expr_node(const Closure_expr &node)
{
    indent();
    print_str("Closure");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Return Type: " + types::type_to_string(node.return_type));
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Parameters");
                   for (size_t i = 0; i < node.parameters.size(); ++i)
                   {
                       with_child(i + 1 < node.parameters.size(),
                                  [&]
                                  {
                                      indent();
                                      print_str((node.parameters[i].is_mut ? "mut " : "") + node.parameters[i].name + " : " +
                                                types::type_to_string(node.parameters[i].type));
                                  });
                   }
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Body");
                   with_child(false, [&] { print_stmt_ptr(node.body); });
               });
}

void AstPrinter::print_expr_node(const Cast_expr &node)
{
    indent();
    print_str("Cast");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Target Type: " + types::type_to_string(node.target_type));
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Expression");
                   with_child(false, [&] { print_expr_ptr(node.expression); });
               });
}

void AstPrinter::print_expr_node(const Array_literal_expr &node)
{
    indent();
    print_str("Array Literal");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Elements");
                   for (size_t i = 0; i < node.elements.size(); ++i)
                       with_child(i + 1 < node.elements.size(), [&] { print_expr_ptr(node.elements[i]); });
               });
}

void AstPrinter::print_expr_node(const Array_access_expr &node)
{
    indent();
    print_str("Array Access");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Array");
                   with_child(false, [&] { print_expr_ptr(node.array); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Index");
                   with_child(false, [&] { print_expr_ptr(node.index); });
               });
}

void AstPrinter::print_expr_node(const Array_assignment_expr &node)
{
    indent();
    print_str("Array Assignment");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Array");
                   with_child(false, [&] { print_expr_ptr(node.array); });
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Index");
                   with_child(false, [&] { print_expr_ptr(node.index); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Value");
                   with_child(false, [&] { print_expr_ptr(node.value); });
               });
}

void AstPrinter::print_expr_node(const Static_path_expr &node)
{
    indent();
    std::string base_name = "<unknown>";
    if (auto *var_expr = std::get_if<Variable_expr>(&node.base->node))
        base_name = var_expr->name;
    print_str("Static Path: " + base_name + "::" + node.member.lexeme);
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Type: " + types::type_to_string(node.type));
               });
}

void AstPrinter::print_expr_node(const Range_expr &node)
{
    indent();
    print_str("Range Expression (" + std::string(node.inclusive ? "..=" : "..") + ")");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Start");
                   with_child(false, [&] { print_expr_ptr(node.start); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("End");
                   with_child(false, [&] { print_expr_ptr(node.end); });
               });
}

void AstPrinter::print_expr_node(const Spawn_expr &node)
{
    indent();
    print_str("Spawn Thread");
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Call");
                   with_child(false, [&] { print_expr_ptr(node.call); });
               });
}

void AstPrinter::print_expr_node(const Await_expr &node)
{
    indent();
    print_str("Await Thread");
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Target");
                   with_child(false, [&] { print_expr_ptr(node.thread); });
               });
}

void AstPrinter::print_expr_node(const Yield_expr &node)
{
    indent();
    print_str("Yield");
    if (node.value)
        with_child(false,
                   [&]
                   {
                       indent();
                       print_str("Value");
                       with_child(false, [&] { print_expr_ptr(node.value); });
                   });
}

void AstPrinter::print_expr_node(const Fstring_expr &node)
{
    indent();
    print_str("F-String: " + node.raw_template);
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Interpolations");
                   for (size_t i = 0; i < node.interpolations.size(); ++i)
                       with_child(i + 1 < node.interpolations.size(), [&] { print_expr_ptr(node.interpolations[i]); });
               });
}

// ---------- Statements ----------

void AstPrinter::print_stmt_node(const Return_stmt &node)
{
    indent();
    print_str("Return");
    if (node.expression)
        with_child(false, [&] { print_expr_ptr(node.expression); });
}

void AstPrinter::print_stmt_node(const Function_stmt &node)
{
    indent();
    print_str(std::format("{} Function: {}", node.is_static ? "static" : "", node.name));
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Parameters");
                   for (size_t i = 0; i < node.parameters.size(); ++i)
                   {
                       with_child(i + 1 < node.parameters.size(),
                                  [&]
                                  {
                                      indent();
                                      print_str((node.parameters[i].is_mut ? "mut " : "") + node.parameters[i].name + " : " +
                                                types::type_to_string(node.parameters[i].type));
                                  });
                   }
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Return type: " + types::type_to_string(node.return_type));
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Body");
                   with_child(false, [&] { print_stmt_ptr(node.body); });
               });
}

void AstPrinter::print_stmt_node(const Model_stmt &node)
{
    indent();
    print_str("Model: " + node.name);
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Fields");
                   if (node.fields.empty())
                   {
                       with_child(false,
                                  [&]
                                  {
                                      indent();
                                      print_str("<none>");
                                  });
                   }
                   else
                   {
                       for (size_t i = 0; i < node.fields.size(); ++i)
                       {
                           with_child(i + 1 < node.fields.size(),
                                      [&]
                                      {
                                          indent();
                                          print_str(node.fields[i].first + " : " + types::type_to_string(node.fields[i].second));
                                      });
                       }
                   }
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Methods");
                   if (node.methods.empty())
                   {
                       with_child(false,
                                  [&]
                                  {
                                      indent();
                                      print_str("<none>");
                                  });
                   }
                   else
                   {
                       for (size_t i = 0; i < node.methods.size(); ++i)
                           with_child(i + 1 < node.methods.size(), [&] { print_stmt_node(*node.methods[i]); });
                   }
               });
}

void AstPrinter::print_stmt_node(const Var_stmt &node)
{
    indent();
    print_str("Var: " + node.name + " : " + types::type_to_string(node.type));
    if (node.initializer)
    {
        with_child(false,
                   [&]
                   {
                       indent();
                       print_str("Initializer");
                       with_child(false, [&] { print_expr_ptr(node.initializer); });
                   });
    }
}

void AstPrinter::print_stmt_node(const Print_stmt &node)
{
    indent();
    print_str(std::string("Print(") + (node.stream == Print_stream::STDOUT ? "stdout" : "stderr") + ")");
    with_child(false, [&] { print_expr_ptr(node.expression); });
}

void AstPrinter::print_stmt_node(const Expr_stmt &node)
{
    indent();
    print_str("Expr_stmt");
    with_child(false, [&] { print_expr_ptr(node.expression); });
}

void AstPrinter::print_stmt_node(const Block_stmt &node)
{
    indent();
    print_str("Block");
    for (size_t i = 0; i < node.statements.size(); ++i)
        with_child(i + 1 < node.statements.size(), [&] { print_stmt_ptr(node.statements[i]); });
}

void AstPrinter::print_stmt_node(const If_stmt &node)
{
    indent();
    print_str("If");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Cond");
                   with_child(false, [&] { print_expr_ptr(node.condition); });
               });
    with_child(node.else_branch != nullptr,
               [&]
               {
                   indent();
                   print_str("Then");
                   with_child(false, [&] { print_stmt_ptr(node.then_branch); });
               });
    if (node.else_branch)
    {
        with_child(false,
                   [&]
                   {
                       indent();
                       print_str("Else");
                       with_child(false, [&] { print_stmt_ptr(node.else_branch); });
                   });
    }
}

void AstPrinter::print_stmt_node(const While_stmt &node)
{
    indent();
    print_str("While");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Cond");
                   with_child(false, [&] { print_expr_ptr(node.condition); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Body");
                   with_child(false, [&] { print_stmt_ptr(node.body); });
               });
}

void AstPrinter::print_stmt_node(const For_stmt &node)
{
    indent();
    print_str("For");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Initializer");
                   with_child(false, [&] { print_stmt_ptr(node.initializer); });
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Condition");
                   with_child(false, [&] { print_expr_ptr(node.condition); });
               });
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Increment");
                   with_child(false, [&] { print_expr_ptr(node.increment); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Body");
                   with_child(false, [&] { print_stmt_ptr(node.body); });
               });
}

void AstPrinter::print_stmt_node(const For_in_stmt &node)
{
    indent();
    print_str("For-In: " + node.var_name);
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Iterable");
                   with_child(false, [&] { print_expr_ptr(node.iterable); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Body");
                   with_child(false, [&] { print_stmt_ptr(node.body); });
               });
}

void AstPrinter::print_stmt_node(const Union_stmt &node)
{
    indent();
    print_str("Union: " + node.name);
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Variants");
                   if (node.variants.empty())
                   {
                       with_child(false,
                                  [&]
                                  {
                                      indent();
                                      print_str("<none>");
                                  });
                   }
                   else
                   {
                       for (size_t i = 0; i < node.variants.size(); ++i)
                       {
                           with_child(i + 1 < node.variants.size(),
                                      [&]
                                      {
                                          indent();
                                          print_str(node.variants[i].first + " : " + types::type_to_string(node.variants[i].second));
                                      });
                       }
                   }
               });
}

void AstPrinter::print_stmt_node(const Match_stmt &node)
{
    indent();
    print_str("Match");
    with_child(true,
               [&]
               {
                   indent();
                   print_str("Subject");
                   with_child(false, [&] { print_expr_ptr(node.subject); });
               });
    with_child(false,
               [&]
               {
                   indent();
                   print_str("Arms");
                   for (size_t i = 0; i < node.arms.size(); ++i)
                   {
                       with_child(i + 1 < node.arms.size(),
                                  [&]
                                  {
                                      indent();
                                      if (node.arms[i].is_wildcard)
                                          print_str("_:");
                                      else
                                          print_str(node.arms[i].union_name +
                                                    (node.arms[i].bind_name.empty() ? "" : "(" + node.arms[i].bind_name + ")") + ":");
                                      with_child(false, [&] { print_stmt_ptr(node.arms[i].body); });
                                  });
                   }
               });
}

// =============================================================================
// Source_printer Implementation
// =============================================================================

std::string Source_printer::get_indent() const { return std::string(indent_level * 4, ' '); }

std::string Source_printer::print_statements(const std::vector<Stmt *> &statements)
{
    std::string result;
    for (const auto *stmt : statements) result += print_stmt_ptr(stmt) + "\n";
    return result;
}

std::string Source_printer::print_expr_ptr(const Expr *expr)
{
    if (!expr)
        return "";
    return std::visit([this](auto const &e) { return this->print_node(e); }, expr->node);
}

std::string Source_printer::print_stmt_ptr(const Stmt *stmt)
{
    if (!stmt)
        return "";
    return std::visit([this](auto const &s) { return this->print_node(s); }, stmt->node);
}

// --- Expressions ---

std::string Source_printer::print_node(const Literal_expr &node)
{
    if (is_string(node.value))
        return "\"" + get_string(node.value) + "\"";
    return value_to_string(node.value);
}

std::string Source_printer::print_node(const Variable_expr &node) { return node.name; }

std::string Source_printer::print_node(const Binary_expr &node)
{
    return "(" + print_expr_ptr(node.left) + " " + util::operator_token_to_string(node.op) + " " + print_expr_ptr(node.right) + ")";
}

std::string Source_printer::print_node(const Unary_expr &node)
{
    return "(" + util::operator_token_to_string(node.op) + print_expr_ptr(node.right) + ")";
}

std::string Source_printer::print_node(const Call_expr &node)
{
    std::string res = print_expr_ptr(node.callee) + "(";
    for (size_t i = 0; i < node.arguments.size(); ++i)
    {
        res += print_expr_ptr(node.arguments[i]);
        if (i + 1 < node.arguments.size())
            res += ", ";
    }
    return res + ")";
}

std::string Source_printer::print_node(const Assignment_expr &node) { return "(" + node.name + " = " + print_expr_ptr(node.value) + ")"; }

std::string Source_printer::print_node(const Field_assignment_expr &node)
{
    return "(" + print_expr_ptr(node.object) + "." + node.field_name + " = " + print_expr_ptr(node.value) + ")";
}

std::string Source_printer::print_node(const Field_access_expr &node) { return print_expr_ptr(node.object) + "." + node.field_name; }

std::string Source_printer::print_node(const Method_call_expr &node)
{
    std::string res = print_expr_ptr(node.object) + "." + node.method_name + "(";
    for (size_t i = 0; i < node.arguments.size(); ++i)
    {
        res += print_expr_ptr(node.arguments[i]);
        if (i + 1 < node.arguments.size())
            res += ", ";
    }
    return res + ")";
}

std::string Source_printer::print_node(const Model_literal_expr &node)
{
    std::string res = node.model_name + " { ";
    for (size_t i = 0; i < node.fields.size(); ++i)
    {
        res += node.fields[i].first + ": " + print_expr_ptr(node.fields[i].second);
        if (i + 1 < node.fields.size())
            res += ", ";
    }
    return res + " }";
}

std::string Source_printer::print_node(const Closure_expr &node)
{
    std::string res = "|";
    for (size_t i = 0; i < node.parameters.size(); ++i)
    {
        if (node.parameters[i].is_mut)
            res += "mut ";
        res += node.parameters[i].name + ": " + types::type_to_string(node.parameters[i].type);
        if (i + 1 < node.parameters.size())
            res += ", ";
    }
    res += "| -> " + types::type_to_string(node.return_type) + " ";
    res += print_stmt_ptr(node.body);
    return res;
}

std::string Source_printer::print_node(const Cast_expr &node)
{
    return "(" + print_expr_ptr(node.expression) + " as " + types::type_to_string(node.target_type) + ")";
}

std::string Source_printer::print_node(const Array_literal_expr &node)
{
    std::string res = "[";
    for (size_t i = 0; i < node.elements.size(); ++i)
    {
        res += print_expr_ptr(node.elements[i]);
        if (i + 1 < node.elements.size())
            res += ", ";
    }
    return res + "]";
}

std::string Source_printer::print_node(const Array_access_expr &node)
{
    return print_expr_ptr(node.array) + "[" + print_expr_ptr(node.index) + "]";
}

std::string Source_printer::print_node(const Array_assignment_expr &node)
{
    return "(" + print_expr_ptr(node.array) + "[" + print_expr_ptr(node.index) + "] = " + print_expr_ptr(node.value) + ")";
}

std::string Source_printer::print_node(const Static_path_expr &node) { return print_expr_ptr(node.base) + "::" + node.member.lexeme; }

std::string Source_printer::print_node(const Range_expr &node)
{
    std::string op = node.inclusive ? "..=" : "..";
    return "(" + (node.start ? print_expr_ptr(node.start) : "") + op + (node.end ? print_expr_ptr(node.end) : "") + ")";
}

std::string Source_printer::print_node(const Spawn_expr &node) { return "spawn " + print_expr_ptr(node.call); }

std::string Source_printer::print_node(const Await_expr &node) { return "await " + print_expr_ptr(node.thread); }

std::string Source_printer::print_node(const Yield_expr &node)
{
    if (node.value)
        return "yield " + print_expr_ptr(node.value);
    return "yield";
}

std::string Source_printer::print_node(const Fstring_expr &node)
{
    std::string res = "f\"";
    for (auto *e : node.interpolations) res += "{" + print_expr_ptr(e) + "}";
    return res + "\"";
}

// --- Statements ---

std::string Source_printer::print_node(const Return_stmt &node)
{
    if (node.expression)
        return get_indent() + "return " + print_expr_ptr(node.expression) + ";";
    return get_indent() + "return;";
}

std::string Source_printer::print_node(const Function_stmt &node)
{
    std::string res = get_indent();
    if (node.is_static)
        res += "static ";
    res += "fn " + node.name + "(";
    for (size_t i = 0; i < node.parameters.size(); ++i)
    {
        if (node.parameters[i].is_mut)
            res += "mut ";
        res += node.parameters[i].name + ": " + types::type_to_string(node.parameters[i].type);
        if (i + 1 < node.parameters.size())
            res += ", ";
    }
    res += ") -> " + types::type_to_string(node.return_type) + "\n";
    res += print_stmt_ptr(node.body);
    return res;
}

std::string Source_printer::print_node(const Model_stmt &node)
{
    std::string res = get_indent() + "model " + node.name + " {\n";
    indent_level++;
    for (const auto &field : node.fields) res += get_indent() + "let " + field.first + ": " + types::type_to_string(field.second) + ";\n";
    for (const auto *method : node.methods)
        res += print_node(*method) + "\n";
    indent_level--;
    res += get_indent() + "}";
    return res;
}

std::string Source_printer::print_node(const Var_stmt &node)
{
    std::string res = get_indent() + "let ";
    if (node.is_mut)
        res += "mut ";
    if (node.is_const)
        res += "const ";
    res += node.name;

    if (!node.type_inferred)
    {
        res += ": " + types::type_to_string(node.type);
        if (node.initializer)
            res += " = " + print_expr_ptr(node.initializer);
    }
    else
    {
        res += " := " + print_expr_ptr(node.initializer);
    }
    return res + ";";
}

std::string Source_printer::print_node(const Print_stmt &node)
{
    std::string kw = (node.stream == Print_stream::STDOUT) ? "print" : "print_err";
    return get_indent() + kw + "(" + print_expr_ptr(node.expression) + ");";
}

std::string Source_printer::print_node(const Expr_stmt &node) { return get_indent() + print_expr_ptr(node.expression) + ";"; }

std::string Source_printer::print_node(const Block_stmt &node)
{
    std::string res = get_indent() + "{\n";
    indent_level++;
    for (const auto *s : node.statements) res += print_stmt_ptr(s) + "\n";
    indent_level--;
    res += get_indent() + "}";
    return res;
}

std::string Source_printer::print_node(const If_stmt &node)
{
    std::string res = get_indent() + "if " + print_expr_ptr(node.condition) + "\n";
    res += print_stmt_ptr(node.then_branch);
    if (node.else_branch)
        res += " else\n" + print_stmt_ptr(node.else_branch);
    return res;
}

std::string Source_printer::print_node(const While_stmt &node)
{
    std::string res = get_indent() + "while " + print_expr_ptr(node.condition) + "\n";
    res += print_stmt_ptr(node.body);
    return res;
}

std::string Source_printer::print_node(const For_stmt &node)
{
    std::string res = get_indent() + "for (";
    std::string init = node.initializer ? print_stmt_ptr(node.initializer) : ";";
    while (!init.empty() && (init.back() == '\n' || init.back() == ' ')) init.pop_back();

    res += init + " " + (node.condition ? print_expr_ptr(node.condition) : "") + "; ";
    res += (node.increment ? print_expr_ptr(node.increment) : "") + ")\n";
    res += print_stmt_ptr(node.body);
    return res;
}

std::string Source_printer::print_node(const For_in_stmt &node)
{
    std::string res = get_indent() + "for " + node.var_name + " in " + print_expr_ptr(node.iterable) + "\n";
    res += print_stmt_ptr(node.body);
    return res;
}

std::string Source_printer::print_node(const Union_stmt &node)
{
    std::string res = get_indent() + "union " + node.name + " {\n";
    indent_level++;
    for (const auto &var : node.variants) res += get_indent() + "let " + var.first + ": " + types::type_to_string(var.second) + ";\n";
    indent_level--;
    res += get_indent() + "}";
    return res;
}

std::string Source_printer::print_node(const Match_stmt &node)
{
    std::string res = get_indent() + "match " + print_expr_ptr(node.subject) + " {\n";
    indent_level++;
    for (const auto &arm : node.arms)
    {
        res += get_indent();
        if (arm.is_wildcard)
            res += "_";
        else
        {
            res += arm.union_name;
            if (!arm.bind_name.empty())
                res += "(" + arm.bind_name + ")";
        }
        res += ":\n";
        indent_level++;
        res += print_stmt_ptr(arm.body) + ",\n";
        indent_level--;
    }
    indent_level--;
    res += get_indent() + "}";
    return res;
}

}  // namespace phos::ast
