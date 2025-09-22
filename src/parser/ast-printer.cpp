#include "ast-printer.hpp"
#include "../utility/utility.hpp"
#include "ast.hpp"

#include <print>
#include <memory>

namespace phos::ast {

// ---------- private utils ----------

std::string AstPrinter::branch_sym(bool has_next) const {
    return use_unicode ? (has_next ? "├── " : "└── ")
                       : (has_next ? "+-- " : "\\-- ");
}

std::string AstPrinter::vertical_sym(bool has_next) const {
    return use_unicode ? (has_next ? "│   " : "    ")
                       : (has_next ? "|   " : "    ");
}

void AstPrinter::indent() const
{
    for (size_t i = 0; i + 1 < branch_stack.size(); ++i)
        std::print("{}", vertical_sym(branch_stack[i]));
    if (!branch_stack.empty())
        std::print("{}", branch_sym(branch_stack.back()));
}

void AstPrinter::print_str(const std::string &s) const { std::println("{}", s); }

// ---------- public ----------

AstPrinter::AstPrinter(bool unicode) : use_unicode(unicode) {}

void AstPrinter::print_statements(const std::vector<Stmt*> &statements)
{
    for (size_t i = 0; i < statements.size(); ++i)
        with_child(i + 1 < statements.size(), [&] { print_stmt_ptr(statements[i]); });
}

void AstPrinter::print_expr_ptr(const Expr* expr)
{
    if (!expr)
    {
        print_str("<nullptr>");
        return;
    }
    std::visit([this](auto const& e) { this->print_expr_node(e); }, expr->node);
}

void AstPrinter::print_stmt_ptr(const Stmt* stmt)
{
    if (!stmt)
    {
        print_str("<nullptr>");
        return;
    }
    std::visit([this](auto const& s) { this->print_stmt_node(s); }, stmt->node);
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
    print_str("Literal: " + util::value_to_string(node.value));
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
    print_str("Call: " + node.callee);
    bool has_args = !node.arguments.empty();
    with_child(has_args,
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
                                      print_str((node.parameters[i].is_const ? "const" : "") + node.parameters[i].name+ " : " + types::type_to_string(node.parameters[i].type));
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
    print_str("Function: " + node.name);
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
                                      print_str((node.parameters[i].is_const ? "const" : "") + node.parameters[i].name+ " : " + types::type_to_string(node.parameters[i].type));
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
                       {
                           with_child(i + 1 < node.methods.size(), [&] { print_stmt_node(*node.methods[i]); });
                       }
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

void AstPrinter::print_stmt_node(const Field_assignment_expr &node)
{
    indent();
    print_str("Field Assign Stmt: " + node.field_name);
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

}  // namespace phos::ast

