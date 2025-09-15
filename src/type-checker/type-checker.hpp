#pragma once

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <optional>
#include <variant>

#include "../parser/ast.hpp"
#include "../error/err.hpp"
#include "../error/result.hpp"
#include "../value/type.hpp"

namespace phos
{

class Type_checker;

// ===================================================================
// TYPE RESOLVER
// ===================================================================
class TypeResolver
{
public:
    TypeResolver(Type_checker &checker) : checker(checker) {}
    void resolve(std::vector<std::unique_ptr<ast::Stmt>> &statements);

private:
    Type_checker &checker;
    void resolve_stmt(ast::Stmt &stmt);
    void resolve_expr(ast::Expr &expr);
    void resolve_type(types::Type &type);

    // Visitor overloads for dispatch
    template <typename T>
    void visit(T &node)
    {
    }  // Default for nodes with no children that need resolving.

    void visit(ast::Function_stmt &stmt);
    void visit(ast::Model_stmt &stmt);
    void visit(ast::Block_stmt &stmt);
    void visit(ast::Var_stmt &stmt);
    void visit(ast::Print_stmt &stmt);
    void visit(ast::Expr_stmt &stmt);
    void visit(ast::If_stmt &stmt);
    void visit(ast::While_stmt &stmt);
    void visit(ast::For_stmt &stmt);
    void visit(ast::Return_stmt &stmt);
    void visit(ast::Assignment_expr &expr);
    void visit(ast::Binary_expr &expr);
    void visit(ast::Call_expr &expr);
    void visit(ast::Cast_expr &expr);
    void visit(ast::Closure_expr &expr);
    void visit(ast::Field_access_expr &expr);
    void visit(ast::Field_assignment_expr &expr);
    void visit(ast::Method_call_expr &expr);
    void visit(ast::Model_literal_expr &expr);
    void visit(ast::Unary_expr &expr);
    // NEW: Array visitors
    void visit(ast::Array_literal_expr &expr);
    void visit(ast::Array_access_expr &expr);
    void visit(ast::Array_assignment_expr &expr);
};

// ===================================================================
// TYPE CHECKER
// ===================================================================
class Type_checker
{
    friend class TypeResolver;

public:
    std::unordered_map<std::string, std::shared_ptr<types::Model_type>> model_signatures;

    void type_error(const ast::Source_location &loc, const std::string &message)
    {
        errors.push_back({message, this->phase, loc.line, loc.column});
    }

    std::vector<err::msg> check(std::vector<std::unique_ptr<ast::Stmt>> &statements);

private:
    struct FunctionData
    {
        const ast::Function_stmt *declaration;
    };
    struct ModelData
    {
        std::shared_ptr<types::Model_type> signature;
        std::unordered_map<std::string, FunctionData> methods;
    };

    using Scope = std::unordered_map<std::string, types::Type>;
    std::vector<Scope> scopes;
    std::unordered_map<std::string, FunctionData> functions;
    std::unordered_map<std::string, ModelData> model_data;

    std::optional<types::Type> current_return_type;
    std::optional<std::shared_ptr<types::Model_type>> current_model_type;
    std::vector<err::msg> errors;
    std::string phase = "type-checking";

    void begin_scope();
    void end_scope();
    void declare(const std::string &name, const types::Type &type, const ast::Source_location &loc);
    Result<types::Type> lookup(const std::string &name, const ast::Source_location &loc);
    bool is_compatible(const types::Type &a, const types::Type &b) const;
    types::Type promote_numeric_type(const types::Type &left, const types::Type &right) const;
    bool is_numeric(const types::Type &type) const;
    bool is_boolean(const types::Type &type) const;
    bool is_string(const types::Type &type) const;
    // NEW: Array type check
    bool is_array(const types::Type &type) const;

    void collect_signatures(const std::vector<std::unique_ptr<ast::Stmt>> &statements);
    void check_stmt(ast::Stmt &stmt);
    Result<types::Type> check_expr(ast::Expr &expr);

    void check_stmt_node(ast::Function_stmt &stmt);
    void check_stmt_node(ast::Model_stmt &stmt);
    void check_stmt_node(ast::Block_stmt &stmt);
    void check_stmt_node(ast::Expr_stmt &stmt);
    void check_stmt_node(ast::If_stmt &stmt);
    void check_stmt_node(ast::Print_stmt &stmt);
    void check_stmt_node(ast::Return_stmt &stmt);
    void check_stmt_node(ast::Var_stmt &stmt);
    void check_stmt_node(ast::While_stmt &stmt);
    void check_stmt_node(ast::For_stmt &stmt);

    Result<types::Type> check_expr_node(ast::Assignment_expr &expr);
    Result<types::Type> check_expr_node(ast::Binary_expr &expr);
    Result<types::Type> check_expr_node(ast::Call_expr &expr);
    Result<types::Type> check_expr_node(ast::Cast_expr &expr);
    Result<types::Type> check_expr_node(ast::Closure_expr &expr);
    Result<types::Type> check_expr_node(ast::Field_access_expr &expr);
    Result<types::Type> check_expr_node(ast::Field_assignment_expr &expr);
    Result<types::Type> check_expr_node(ast::Literal_expr &expr);
    Result<types::Type> check_expr_node(ast::Method_call_expr &expr);
    Result<types::Type> check_expr_node(ast::Model_literal_expr &expr);
    Result<types::Type> check_expr_node(ast::Unary_expr &expr);
    Result<types::Type> check_expr_node(ast::Variable_expr &expr);
    // NEW: Array expression checkers
    Result<types::Type> check_expr_node(ast::Array_literal_expr &expr);
    Result<types::Type> check_expr_node(ast::Array_access_expr &expr);
    Result<types::Type> check_expr_node(ast::Array_assignment_expr &expr);
};

// ===================================================================
// TYPE RESOLVER IMPLEMENTATION
// ===================================================================

inline void TypeResolver::resolve(std::vector<std::unique_ptr<ast::Stmt>> &statements)
{
    for (auto &stmt : statements)
        if (stmt)
            resolve_stmt(*stmt);
}
inline void TypeResolver::resolve_stmt(ast::Stmt &stmt)
{
    std::visit([this](auto &s) { visit(s); }, stmt.node);
}
inline void TypeResolver::resolve_expr(ast::Expr &expr)
{
    std::visit([this](auto &e) { visit(e); }, expr.node);
}
inline void TypeResolver::resolve_type(types::Type &type)
{
    if (auto *model_type_ptr = std::get_if<std::shared_ptr<types::Model_type>>(&type))
    {
        if ((*model_type_ptr)->name.empty())
            return;
        const std::string name = (*model_type_ptr)->name;
        if (checker.model_signatures.contains(name))
            type = checker.model_signatures.at(name);
        else
            checker.type_error({}, "Unknown model type '" + name + "'.");
    }
    else if (auto *closure_type_ptr = std::get_if<std::shared_ptr<types::Closure_type>>(&type))
    {
        for (auto &param_type : (*closure_type_ptr)->function_type.parameter_types) resolve_type(param_type);
        resolve_type((*closure_type_ptr)->function_type.return_type);
    }
    // NEW: Recursively resolve array element types
    else if (auto *array_type_ptr = std::get_if<std::shared_ptr<types::Array_type>>(&type))
    {
        if (*array_type_ptr)
            resolve_type((*array_type_ptr)->element_type);
    }
}
inline void TypeResolver::visit(ast::Function_stmt &stmt)
{
    resolve_type(stmt.return_type);
    for (auto &param : stmt.parameters) resolve_type(param.second);
    if (stmt.body)
        resolve_stmt(*stmt.body);
}
inline void TypeResolver::visit(ast::Model_stmt &stmt)
{
    for (auto &method : stmt.methods) visit(method);
}
inline void TypeResolver::visit(ast::Block_stmt &stmt)
{
    for (auto &s : stmt.statements)
        if (s)
            resolve_stmt(*s);
}
inline void TypeResolver::visit(ast::Var_stmt &stmt)
{
    resolve_type(stmt.type);
    if (stmt.initializer)
        resolve_expr(*stmt.initializer);
}
inline void TypeResolver::visit(ast::Print_stmt &stmt)
{
    if (stmt.expression)
        resolve_expr(*stmt.expression);
}
inline void TypeResolver::visit(ast::Expr_stmt &stmt)
{
    if (stmt.expression)
        resolve_expr(*stmt.expression);
}
inline void TypeResolver::visit(ast::If_stmt &stmt)
{
    if (stmt.condition)
        resolve_expr(*stmt.condition);
    if (stmt.then_branch)
        resolve_stmt(*stmt.then_branch);
    if (stmt.else_branch)
        resolve_stmt(*stmt.else_branch);
}
inline void TypeResolver::visit(ast::While_stmt &stmt)
{
    if (stmt.condition)
        resolve_expr(*stmt.condition);
    if (stmt.body)
        resolve_stmt(*stmt.body);
}
inline void TypeResolver::visit(ast::For_stmt &stmt)
{
    if (stmt.initializer)
        resolve_stmt(*stmt.initializer);
    if (stmt.condition)
        resolve_expr(*stmt.condition);
    if (stmt.increment)
        resolve_expr(*stmt.increment);
    if (stmt.body)
        resolve_stmt(*stmt.body);
}
inline void TypeResolver::visit(ast::Return_stmt &stmt)
{
    if (stmt.expression)
        resolve_expr(*stmt.expression);
}
inline void TypeResolver::visit(ast::Assignment_expr &expr)
{
    if (expr.value)
        resolve_expr(*expr.value);
}
inline void TypeResolver::visit(ast::Binary_expr &expr)
{
    if (expr.left)
        resolve_expr(*expr.left);
    if (expr.right)
        resolve_expr(*expr.right);
}
inline void TypeResolver::visit(ast::Call_expr &expr)
{
    for (auto &arg : expr.arguments)
        if (arg)
            resolve_expr(*arg);
}
inline void TypeResolver::visit(ast::Cast_expr &expr)
{
    if (expr.expression)
        resolve_expr(*expr.expression);
    resolve_type(expr.target_type);
}
inline void TypeResolver::visit(ast::Closure_expr &expr)
{
    resolve_type(expr.return_type);
    for (auto &param : expr.parameters) resolve_type(param.second);
    if (expr.body)
        resolve_stmt(*expr.body);
}
inline void TypeResolver::visit(ast::Field_access_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
}
inline void TypeResolver::visit(ast::Field_assignment_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
    if (expr.value)
        resolve_expr(*expr.value);
}
inline void TypeResolver::visit(ast::Method_call_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
    for (auto &arg : expr.arguments)
        if (arg)
            resolve_expr(*arg);
}
inline void TypeResolver::visit(ast::Model_literal_expr &expr)
{
    for (auto &field : expr.fields)
        if (field.second)
            resolve_expr(*field.second);
}
inline void TypeResolver::visit(ast::Unary_expr &expr)
{
    if (expr.right)
        resolve_expr(*expr.right);
}
// NEW: Array resolver implementations
inline void TypeResolver::visit(ast::Array_literal_expr &expr)
{
    for (auto &element : expr.elements)
        if (element)
            resolve_expr(*element);
}
inline void TypeResolver::visit(ast::Array_access_expr &expr)
{
    if (expr.array)
        resolve_expr(*expr.array);
    if (expr.index)
        resolve_expr(*expr.index);
}
inline void TypeResolver::visit(ast::Array_assignment_expr &expr)
{
    if (expr.array)
        resolve_expr(*expr.array);
    if (expr.index)
        resolve_expr(*expr.index);
    if (expr.value)
        resolve_expr(*expr.value);
}

// ===================================================================
// TYPE CHECKER IMPLEMENTATIONS
// ===================================================================

inline std::vector<err::msg> Type_checker::check(std::vector<std::unique_ptr<ast::Stmt>> &statements)
{
    begin_scope();
    collect_signatures(statements);
    TypeResolver resolver(*this);
    resolver.resolve(statements);
    for (auto &stmt : statements)
        if (stmt)
            check_stmt(*stmt);
    end_scope();
    return errors;
}

inline void Type_checker::collect_signatures(const std::vector<std::unique_ptr<ast::Stmt>> &statements)
{
    for (const auto &stmt : statements)
    {
        if (const auto *func_stmt = std::get_if<ast::Function_stmt>(&stmt->node))
        {
            if (functions.contains(func_stmt->name))
                type_error(func_stmt->loc, "Function '" + func_stmt->name + "' is already defined.");
            else
                functions[func_stmt->name] = {func_stmt};
        }
        else if (const auto *model_stmt = std::get_if<ast::Model_stmt>(&stmt->node))
        {
            if (model_signatures.contains(model_stmt->name))
            {
                type_error(model_stmt->loc, "Model '" + model_stmt->name + "' is already defined.");
            }
            else
            {
                auto model_type = std::make_shared<types::Model_type>();
                model_type->name = model_stmt->name;
                for (const auto &field : model_stmt->fields) model_type->fields[field.first] = field.second;

                ModelData data;
                data.signature = model_type;

                for (const auto &method_ast : model_stmt->methods)
                {
                    types::Function_type method_type;
                    for (const auto &param : method_ast.parameters) method_type.parameter_types.push_back(param.second);
                    method_type.return_type = method_ast.return_type;

                    data.methods[method_ast.name] = {&method_ast};
                    model_type->methods[method_ast.name] = method_type;
                }
                model_signatures[model_stmt->name] = model_type;
                model_data[model_stmt->name] = std::move(data);
            }
        }
    }
}

inline void Type_checker::begin_scope() { scopes.emplace_back(); }
inline void Type_checker::end_scope() { scopes.pop_back(); }

inline void Type_checker::declare(const std::string &name, const types::Type &type, const ast::Source_location &loc)
{
    if (scopes.empty() || scopes.back().contains(name))
        type_error(loc, "Variable '" + name + "' is already declared in this scope.");
    else
        scopes.back()[name] = type;
}

inline Result<types::Type> Type_checker::lookup(const std::string &name, const ast::Source_location &loc)
{
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it)
        if (it->contains(name))
            return it->at(name);

    if (functions.contains(name))
    {
        const auto &func_data = functions.at(name);
        auto func_type = std::make_shared<types::Function_type>();
        for (const auto &param : func_data.declaration->parameters) func_type->parameter_types.push_back(param.second);
        func_type->return_type = func_data.declaration->return_type;
        return types::Type(func_type);
    }

    type_error(loc, "Undefined variable or function '" + name + "'.");
    return types::Primitive_kind::Void;
}

inline bool Type_checker::is_compatible(const types::Type &a, const types::Type &b) const
{
    if (const auto *a_closure = std::get_if<std::shared_ptr<types::Closure_type>>(&a))
    {
        if (const auto *b_closure = std::get_if<std::shared_ptr<types::Closure_type>>(&b))
            return (*a_closure)->function_type == (*b_closure)->function_type;
    }

    // NEW: Check for array compatibility
    if (const auto *a_array = std::get_if<std::shared_ptr<types::Array_type>>(&a))
    {
        if (const auto *b_array = std::get_if<std::shared_ptr<types::Array_type>>(&b))
            return is_compatible((*a_array)->element_type, (*b_array)->element_type);
    }

    if (std::holds_alternative<std::shared_ptr<types::Model_type>>(a) && std::holds_alternative<std::shared_ptr<types::Model_type>>(b))
        return a == b;

    return a == b;
}

inline types::Type Type_checker::promote_numeric_type(const types::Type &left, const types::Type &right) const
{
    if ((is_numeric(left) && std::get<types::Primitive_kind>(left) == types::Primitive_kind::Float) ||
        (is_numeric(right) && std::get<types::Primitive_kind>(right) == types::Primitive_kind::Float))
    {
        return types::Primitive_kind::Float;
    }
    return types::Primitive_kind::Int;
}

inline bool Type_checker::is_numeric(const types::Type &type) const
{
    if (const auto *prim = std::get_if<types::Primitive_kind>(&type))
        return *prim == types::Primitive_kind::Int || *prim == types::Primitive_kind::Float;
    return false;
}
inline bool Type_checker::is_boolean(const types::Type &type) const
{
    if (const auto *prim = std::get_if<types::Primitive_kind>(&type))
        return *prim == types::Primitive_kind::Bool;
    return false;
}
inline bool Type_checker::is_string(const types::Type &type) const
{
    if (const auto *prim = std::get_if<types::Primitive_kind>(&type))
        return *prim == types::Primitive_kind::String;
    return false;
}
// NEW: Array type checker implementation
inline bool Type_checker::is_array(const types::Type &type) const
{
    return std::holds_alternative<std::shared_ptr<types::Array_type>>(type);
}

inline void Type_checker::check_stmt(ast::Stmt &stmt)
{
    std::visit([this](auto &s) { check_stmt_node(s); }, stmt.node);
}

inline Result<types::Type> Type_checker::check_expr(ast::Expr &expr)
{
    return std::visit([this](auto &e) -> Result<types::Type> { return check_expr_node(e); }, expr.node);
}

// Statement Implementations
inline void Type_checker::check_stmt_node(ast::Function_stmt &stmt)
{
    auto saved_return = current_return_type;
    current_return_type = stmt.return_type;
    begin_scope();
    if (current_model_type)
        declare("this", current_model_type.value(), stmt.loc);
    for (const auto &p : stmt.parameters) declare(p.first, p.second, stmt.loc);
    if (stmt.body)
        check_stmt(*stmt.body);
    end_scope();
    current_return_type = saved_return;
}
inline void Type_checker::check_stmt_node(ast::Model_stmt &stmt)
{
    auto saved_model = current_model_type;
    current_model_type = model_signatures.at(stmt.name);
    for (auto &method : stmt.methods) check_stmt_node(method);
    current_model_type = saved_model;
}
inline void Type_checker::check_stmt_node(ast::Block_stmt &stmt)
{
    begin_scope();
    for (auto &s : stmt.statements)
        if (s)
            check_stmt(*s);
    end_scope();
}
inline void Type_checker::check_stmt_node(ast::Expr_stmt &stmt)
{
    if (stmt.expression)
        std::ignore = check_expr(*stmt.expression);
}
inline void Type_checker::check_stmt_node(ast::If_stmt &stmt)
{
    if (stmt.condition)
        if (auto res = check_expr(*stmt.condition); res && !is_boolean(res.value()))
            type_error(ast::get_loc(stmt.condition->node), "If condition must be a boolean");
    if (stmt.then_branch)
        check_stmt(*stmt.then_branch);
    if (stmt.else_branch)
        check_stmt(*stmt.else_branch);
}
inline void Type_checker::check_stmt_node(ast::Print_stmt &stmt)
{
    if (stmt.expression)
        std::ignore = check_expr(*stmt.expression);
}
inline void Type_checker::check_stmt_node(ast::Return_stmt &stmt)
{
    if (!current_return_type)
    {
        type_error(stmt.loc, "Return statement used outside of a function");
        return;
    }
    if (stmt.expression)
    {
        if (auto val_res = check_expr(*stmt.expression); val_res && !is_compatible(current_return_type.value(), val_res.value()))
            type_error(stmt.loc, "Return value type does not match function's return type");
    }
    else if (current_return_type.value() != types::Type(types::Primitive_kind::Void))
    {
        type_error(stmt.loc, "Function with non-void return type must return a value");
    }
}
inline void Type_checker::check_stmt_node(ast::Var_stmt &stmt)
{
    types::Type init_type = types::Primitive_kind::Void;
    if (stmt.initializer)
    {
        if (auto res = check_expr(*stmt.initializer); res)
            init_type = res.value();
        else
            return;
    }
    if (stmt.type_inferred)
        stmt.type = init_type;
    else if (stmt.initializer && !is_compatible(stmt.type, init_type))
        type_error(stmt.loc, std::format("Initializer type does not match variable's declared type.\n    Expected: {}\n    Got:      {}",
                                         types::type_to_string(stmt.type), types::type_to_string(init_type)));
    declare(stmt.name, stmt.type, stmt.loc);
}
inline void Type_checker::check_stmt_node(ast::While_stmt &stmt)
{
    if (stmt.condition)
        if (auto res = check_expr(*stmt.condition); res && !is_boolean(res.value()))
            type_error(ast::get_loc(stmt.condition->node), "While condition must be a boolean");
    if (stmt.body)
        check_stmt(*stmt.body);
}
inline void Type_checker::check_stmt_node(ast::For_stmt &stmt)
{
    begin_scope();
    if (stmt.initializer)
        check_stmt(*stmt.initializer);
    if (stmt.condition)
        if (auto res = check_expr(*stmt.condition); res && !is_boolean(res.value()))
            type_error(ast::get_loc(stmt.condition->node), "For loop condition must be a boolean");
    if (stmt.increment)
        std::ignore = check_expr(*stmt.increment);
    if (stmt.body)
        check_stmt(*stmt.body);
    end_scope();
}

// Expression Implementations
inline Result<types::Type> Type_checker::check_expr_node(ast::Assignment_expr &expr)
{
    auto var_type = lookup(expr.name, expr.loc);
    if (!var_type)
        return var_type;
    auto val_type = check_expr(*expr.value);
    if (!val_type)
        return val_type;
    if (!is_compatible(var_type.value(), val_type.value()))
        type_error(expr.loc, "Assignment type mismatch");
    return expr.type = var_type.value();
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Binary_expr &expr)
{
    auto left = check_expr(*expr.left);
    auto right = check_expr(*expr.right);
    if (!left || !right)
        return expr.type = types::Primitive_kind::Void;  // MODIFIED
    types::Type left_type = left.value();
    types::Type right_type = right.value();
    switch (expr.op)
    {
        case lex::TokenType::Plus:
            if (is_string(left_type) && is_string(right_type))
                return expr.type = types::Primitive_kind::String;
        case lex::TokenType::Minus:
        case lex::TokenType::Star:
        case lex::TokenType::Slash:
            if (!is_numeric(left_type) || !is_numeric(right_type))
            {
                type_error(expr.loc, "Operands for arithmetic must be numbers");
                return expr.type = types::Primitive_kind::Void;
            }
            return expr.type = promote_numeric_type(left_type, right_type);
        case lex::TokenType::Percent:
            if (!(std::holds_alternative<types::Primitive_kind>(left_type) &&
                  std::get<types::Primitive_kind>(left_type) == types::Primitive_kind::Int) ||
                !(std::holds_alternative<types::Primitive_kind>(right_type) &&
                  std::get<types::Primitive_kind>(right_type) == types::Primitive_kind::Int))
            {
                type_error(expr.loc, "Operands for '%' must be integers");
            }
            return expr.type = types::Primitive_kind::Int;
        case lex::TokenType::Greater:
        case lex::TokenType::GreaterEqual:
        case lex::TokenType::Less:
        case lex::TokenType::LessEqual:
            if (!is_numeric(left_type) || !is_numeric(right_type))
                type_error(expr.loc, "Operands for comparison must be numbers");
            return expr.type = types::Primitive_kind::Bool;
        case lex::TokenType::Equal:
        case lex::TokenType::NotEqual:
            if (!is_compatible(left_type, right_type) && !is_compatible(right_type, left_type))
                type_error(expr.loc, "Cannot compare incompatible types");
            return expr.type = types::Primitive_kind::Bool;
        case lex::TokenType::LogicalAnd:
        case lex::TokenType::LogicalOr:
            if (!is_boolean(left_type) || !is_boolean(right_type))
                type_error(expr.loc, "Operands for logical operators must be booleans");
            return expr.type = types::Primitive_kind::Bool;
        default:
            type_error(expr.loc, "Unsupported binary operator");
            return expr.type = types::Primitive_kind::Void;
    }
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Call_expr &expr)
{
    auto callee_res = lookup(expr.callee, expr.loc);
    if (!callee_res)
        return expr.type = types::Primitive_kind::Void;  // MODIFIED

    types::Type callee_type = callee_res.value();
    types::Function_type *signature = nullptr;

    if (auto *ft = std::get_if<std::shared_ptr<types::Function_type>>(&callee_type))
    {
        signature = ft->get();
    }
    else if (auto *ct = std::get_if<std::shared_ptr<types::Closure_type>>(&callee_type))
    {
        signature = &(*ct)->function_type;
    }
    else
    {
        type_error(expr.loc, "'" + expr.callee + "' is not callable");
        return expr.type = types::Primitive_kind::Void;
    }

    if (expr.arguments.size() != signature->parameter_types.size())
    {
        type_error(expr.loc, "Incorrect number of arguments");
    }
    else
    {
        for (size_t i = 0; i < expr.arguments.size(); ++i)
            if (auto arg_t = check_expr(*expr.arguments[i]); arg_t && !is_compatible(signature->parameter_types[i], arg_t.value()))
                type_error(ast::get_loc(expr.arguments[i]->node), "Argument type mismatch");
    }
    return expr.type = signature->return_type;
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Cast_expr &expr)
{
    // MODIFIED: Ensure type is assigned to the expression node.
    // This assumes `ast::Cast_expr` has a `type` member like other expressions.
    std::ignore = check_expr(*expr.expression);
    return expr.target_type = expr.target_type;
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Closure_expr &expr)
{
    auto saved_ret = current_return_type;
    current_return_type = expr.return_type;
    begin_scope();
    for (auto const &p : expr.parameters) declare(p.first, p.second, expr.loc);
    if (expr.body)
        check_stmt(*expr.body);
    end_scope();
    current_return_type = saved_ret;
    auto ct = std::make_shared<types::Closure_type>();
    for (auto const &p : expr.parameters) ct->function_type.parameter_types.push_back(p.second);
    ct->function_type.return_type = expr.return_type;
    return expr.type = types::Type(ct);
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Field_access_expr &expr)
{
    auto obj_type = check_expr(*expr.object);
    if (!obj_type)
        return expr.type = types::Primitive_kind::Void;  // MODIFIED
    if (auto *mt = std::get_if<std::shared_ptr<types::Model_type>>(&obj_type.value()))
    {
        if ((*mt)->fields.contains(expr.field_name))
            return expr.type = (*mt)->fields.at(expr.field_name);
        if ((*mt)->methods.contains(expr.field_name))
        {
            const auto &method = (*mt)->methods.at(expr.field_name);
            auto ft = std::make_shared<types::Function_type>();
            ft->parameter_types = method.parameter_types;
            ft->return_type = method.return_type;
            return expr.type = types::Type(ft);
        }
        type_error(expr.loc, "Model '" + (*mt)->name + "' has no member '" + expr.field_name + "'");
    }
    else
        type_error(expr.loc, "Can only access fields on model instances");
    return expr.type = types::Primitive_kind::Void;
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Field_assignment_expr &expr)
{
    auto field_access_temp = ast::Field_access_expr{std::move(expr.object), expr.field_name, {}, expr.loc};
    auto field_type = check_expr_node(field_access_temp);
    expr.object = std::move(field_access_temp.object);
    if (!field_type)
        return expr.type = types::Primitive_kind::Void;  // MODIFIED
    auto val_type = check_expr(*expr.value);
    if (!val_type)
        return expr.type = types::Primitive_kind::Void;  // MODIFIED
    if (!is_compatible(field_type.value(), val_type.value()))
        type_error(expr.loc, "Assignment type mismatch for field");
    return expr.type = field_type.value();
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Literal_expr &expr) { return expr.type; }
inline Result<types::Type> Type_checker::check_expr_node(ast::Method_call_expr &expr)
{
    auto field_access = ast::Field_access_expr{std::move(expr.object), expr.method_name, {}, expr.loc};
    auto callee_type = check_expr_node(field_access);
    expr.object = std::move(field_access.object);
    if (!callee_type)
        return expr.type = types::Primitive_kind::Void;  // MODIFIED

    if (auto *ft = std::get_if<std::shared_ptr<types::Function_type>>(&callee_type.value()))
    {
        auto sig = *ft;
        if (expr.arguments.size() != sig->parameter_types.size())
            type_error(expr.loc, "Incorrect argument count for method");
        else
            for (size_t i = 0; i < expr.arguments.size(); ++i)
                if (auto arg_t = check_expr(*expr.arguments[i]); arg_t && !is_compatible(sig->parameter_types[i], arg_t.value()))
                    type_error(ast::get_loc(expr.arguments[i]->node), "Argument type mismatch");
        return expr.type = sig->return_type;
    }
    type_error(expr.loc, "'" + expr.method_name + "' is not a method");
    return expr.type = types::Primitive_kind::Void;
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Model_literal_expr &expr)
{
    if (!model_signatures.contains(expr.model_name))
    {
        type_error(expr.loc, "No model named '" + expr.model_name + "' defined");
        return expr.type = types::Primitive_kind::Void;
    }
    auto model_type = model_signatures.at(expr.model_name);
    std::unordered_set<std::string> provided_fields;
    for (auto &field_init : expr.fields)
    {
        provided_fields.insert(field_init.first);
        if (!model_type->fields.contains(field_init.first))
        {
            type_error(expr.loc, "Model '" + expr.model_name + "' has no field '" + field_init.first + "'");
        }
        else
        {
            auto field_type = model_type->fields.at(field_init.first);
            auto init_type_res = check_expr(*field_init.second);
            if (init_type_res && !is_compatible(field_type, init_type_res.value()))
                type_error(ast::get_loc(field_init.second->node), "Initializer type does not match field's type");
        }
    }
    for (const auto &[field_name, field_type] : model_type->fields)
        if (!provided_fields.contains(field_name))
            type_error(expr.loc, "Field '" + field_name + "' is missing in initializer for model '" + expr.model_name + "'");
    return expr.type = types::Type(model_type);
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Unary_expr &expr)
{
    auto right_type = check_expr(*expr.right);
    if (!right_type)
        return expr.type = types::Primitive_kind::Void;  // MODIFIED
    switch (expr.op)
    {
        case lex::TokenType::Minus:
            if (!is_numeric(right_type.value()))
                type_error(expr.loc, "Operand for '-' must be a number");
            return expr.type = right_type.value();
        case lex::TokenType::LogicalNot:
            if (!is_boolean(right_type.value()))
                type_error(expr.loc, "Operand for '!' must be a boolean");
            return expr.type = types::Primitive_kind::Bool;
        default:
            type_error(expr.loc, "Unsupported unary operator");
            return expr.type = types::Primitive_kind::Void;
    }
}
inline Result<types::Type> Type_checker::check_expr_node(ast::Variable_expr &expr)
{
    if (expr.name == "this")
    {
        if (!current_model_type)
        {
            type_error(expr.loc, "Cannot use 'this' outside of a model method");
            return expr.type = types::Primitive_kind::Void;
        }
        return expr.type = current_model_type.value();
    }
    auto type_res = lookup(expr.name, expr.loc);
    if (!type_res)
        return expr.type = types::Primitive_kind::Void;  // MODIFIED
    return expr.type = type_res.value();
}

// NEW: Array expression checker implementations
inline Result<types::Type> Type_checker::check_expr_node(ast::Array_literal_expr &expr)
{
    if (expr.elements.empty())
    {
        // Type of an empty array literal cannot be inferred without context.
        // This is often resolved during variable declaration type checking.
        // For now, we'll mark it as an error here or assign a special "any" type.
        // Let's create a void array type for now.
        auto element_type = types::Type(types::Primitive_kind::Void);
        return expr.type = types::Type(std::make_shared<types::Array_type>(element_type));
    }

    auto first_element_type_res = check_expr(*expr.elements[0]);
    if (!first_element_type_res)
        return expr.type = types::Primitive_kind::Void;
    types::Type common_type = first_element_type_res.value();

    for (size_t i = 1; i < expr.elements.size(); ++i)
    {
        auto element_type_res = check_expr(*expr.elements[i]);
        if (!element_type_res)
            return expr.type = types::Primitive_kind::Void;

        if (!is_compatible(common_type, element_type_res.value()))
        {
            type_error(ast::get_loc(expr.elements[i]->node), "Array elements must have a consistent type.");
            // In case of error, create a void array type to prevent further cascading errors.
            auto err_element_type = types::Type(types::Primitive_kind::Void);
            return expr.type = types::Type(std::make_shared<types::Array_type>(err_element_type));
        }
    }

    return expr.type = types::Type(std::make_shared<types::Array_type>(common_type));
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Array_access_expr &expr)
{
    auto array_type_res = check_expr(*expr.array);
    if (!array_type_res)
        return expr.type = types::Primitive_kind::Void;

    if (!is_array(array_type_res.value()))
    {
        type_error(expr.loc, "Subscript operator '[]' can only be used on arrays.");
        return expr.type = types::Primitive_kind::Void;
    }

    auto index_type_res = check_expr(*expr.index);
    if (index_type_res && index_type_res.value() != types::Type(types::Primitive_kind::Int))
        type_error(ast::get_loc(expr.index->node), "Array index must be an integer.");

    const auto &array_type = std::get<std::shared_ptr<types::Array_type>>(array_type_res.value());
    return expr.type = array_type->element_type;
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Array_assignment_expr &expr)
{
    auto array_type_res = check_expr(*expr.array);
    if (!array_type_res)
        return expr.type = types::Primitive_kind::Void;

    if (!is_array(array_type_res.value()))
    {
        type_error(expr.loc, "Subscript operator '[]' can only be used on arrays for assignment.");
        return expr.type = types::Primitive_kind::Void;
    }

    auto index_type_res = check_expr(*expr.index);
    if (index_type_res && index_type_res.value() != types::Type(types::Primitive_kind::Int))
        type_error(ast::get_loc(expr.index->node), "Array index must be an integer.");

    auto value_type_res = check_expr(*expr.value);
    if (!value_type_res)
        return expr.type = types::Primitive_kind::Void;

    const auto &array_type = std::get<std::shared_ptr<types::Array_type>>(array_type_res.value());
    if (!is_compatible(array_type->element_type, value_type_res.value()))
        type_error(ast::get_loc(expr.value->node), "Type of value being assigned does not match array's element type.");

    return expr.type = value_type_res.value();
}

}  // namespace phos
