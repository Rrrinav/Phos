#include "type-checker.hpp"
#include <print>

namespace phos
{

void TypeResolver::resolve(std::vector<ast::Stmt *> &statements)
{
    for (auto *stmt : statements)
        if (stmt)
            resolve_stmt(*stmt);
}

void TypeResolver::resolve_stmt(ast::Stmt &stmt)
{
    std::visit([this](auto &s) { visit(s); }, stmt.node);
}

void TypeResolver::resolve_expr(ast::Expr &expr)
{
    std::visit([this](auto &e) { visit(e); }, expr.node);
}

void TypeResolver::resolve_type(types::Type &type)
{
    if (auto *model_type_ptr = std::get_if<std::shared_ptr<types::Model_type>>(&type))
    {
        if ((*model_type_ptr)->name.empty())
            return;

        const std::string &name = (*model_type_ptr)->name;
        if (checker.model_signatures.count(name))
        {
            // Replace the placeholder type with the fully resolved signature
            type = checker.model_signatures.at(name);
        }
        else
        {
            checker.type_error({}, "Unknown model type '" + name + "'.");
        }
    }
    else if (auto *closure_type_ptr = std::get_if<std::shared_ptr<types::Closure_type>>(&type))
    {
        for (auto &param_type : (*closure_type_ptr)->function_type.parameter_types) resolve_type(param_type);
        resolve_type((*closure_type_ptr)->function_type.return_type);
    }
    else if (auto *array_type_ptr = std::get_if<std::shared_ptr<types::Array_type>>(&type))
    {
        if (*array_type_ptr)
            resolve_type((*array_type_ptr)->element_type);
    }
    else if (auto *optional_type_ptr = std::get_if<std::shared_ptr<types::Optional_type>>(&type))
    {
        if (*optional_type_ptr)
            resolve_type((*optional_type_ptr)->base_type);
    }
}

void TypeResolver::visit(ast::Function_stmt &stmt)
{
    resolve_type(stmt.return_type);
    for (auto &param : stmt.parameters) resolve_type(param.type);
    if (stmt.body)
        resolve_stmt(*stmt.body);
}

void TypeResolver::visit(ast::Model_stmt &stmt)
{
    for (auto *method : stmt.methods)
        if (method)
            visit(*method);
}

void TypeResolver::visit(ast::Block_stmt &stmt)
{
    for (auto *s : stmt.statements)
        if (s)
            resolve_stmt(*s);
}

void TypeResolver::visit(ast::Var_stmt &stmt)
{
    resolve_type(stmt.type);
    if (stmt.initializer)
        resolve_expr(*stmt.initializer);
}

void TypeResolver::visit(ast::Print_stmt &stmt)
{
    if (stmt.expression)
        resolve_expr(*stmt.expression);
}

void TypeResolver::visit(ast::Expr_stmt &stmt)
{
    if (stmt.expression)
        resolve_expr(*stmt.expression);
}

void TypeResolver::visit(ast::If_stmt &stmt)
{
    if (stmt.condition)
        resolve_expr(*stmt.condition);
    if (stmt.then_branch)
        resolve_stmt(*stmt.then_branch);
    if (stmt.else_branch)
        resolve_stmt(*stmt.else_branch);
}

void TypeResolver::visit(ast::While_stmt &stmt)
{
    if (stmt.condition)
        resolve_expr(*stmt.condition);
    if (stmt.body)
        resolve_stmt(*stmt.body);
}

void TypeResolver::visit(ast::For_stmt &stmt)
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

void TypeResolver::visit(ast::Return_stmt &stmt)
{
    if (stmt.expression)
        resolve_expr(*stmt.expression);
}

void TypeResolver::visit(ast::Assignment_expr &expr)
{
    if (expr.value)
        resolve_expr(*expr.value);
}

void TypeResolver::visit(ast::Binary_expr &expr)
{
    if (expr.left)
        resolve_expr(*expr.left);
    if (expr.right)
        resolve_expr(*expr.right);
}

void TypeResolver::visit(ast::Call_expr &expr)
{
    for (auto *arg : expr.arguments)
        if (arg)
            resolve_expr(*arg);
}

void TypeResolver::visit(ast::Cast_expr &expr)
{
    if (expr.expression)
        resolve_expr(*expr.expression);
    resolve_type(expr.target_type);
}

void TypeResolver::visit(ast::Closure_expr &expr)
{
    resolve_type(expr.return_type);
    for (auto &param : expr.parameters) resolve_type(param.type);
    if (expr.body)
        resolve_stmt(*expr.body);
}

void TypeResolver::visit(ast::Field_access_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
}

void TypeResolver::visit(ast::Field_assignment_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
    if (expr.value)
        resolve_expr(*expr.value);
}

void TypeResolver::visit(ast::Method_call_expr &expr)
{
    if (expr.object)
        resolve_expr(*expr.object);
    for (auto *arg : expr.arguments)
        if (arg)
            resolve_expr(*arg);
}

void TypeResolver::visit(ast::Model_literal_expr &expr)
{
    for (auto &field : expr.fields)
        if (field.second)
            resolve_expr(*field.second);
}

void TypeResolver::visit(ast::Unary_expr &expr)
{
    if (expr.right)
        resolve_expr(*expr.right);
}

void TypeResolver::visit(ast::Array_literal_expr &expr)
{
    for (auto *element : expr.elements)
        if (element)
            resolve_expr(*element);
}

void TypeResolver::visit(ast::Array_access_expr &expr)
{
    if (expr.array)
        resolve_expr(*expr.array);
    if (expr.index)
        resolve_expr(*expr.index);
}

void TypeResolver::visit(ast::Array_assignment_expr &expr)
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
std::vector<err::msg> Type_checker::check(std::vector<ast::Stmt*> &statements)
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

inline void Type_checker::collect_signatures(const std::vector<ast::Stmt*> &statements)
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
                    for (const auto &param : method_ast->parameters) method_type.parameter_types.push_back(param.type);
                    method_type.return_type = method_ast->return_type;
                    data.methods[method_ast->name] = {method_ast};
                    model_type->methods[method_ast->name] = method_type;
                }
                model_signatures[model_stmt->name] = model_type;
                model_data[model_stmt->name] = std::move(data);
            }
        }
    }
}

inline void Type_checker::begin_scope()
{
    scopes.emplace_back();
    m_nil_checked_vars_stack.emplace_back();
}

inline void Type_checker::end_scope()
{
    if (!scopes.empty())
        scopes.pop_back();
    if (!m_nil_checked_vars_stack.empty())
        m_nil_checked_vars_stack.pop_back();
}

inline void Type_checker::declare(const std::string &name, const types::Type &type, bool is_constant, const ast::Source_location &loc)
{
    if (!scopes.empty() && scopes.back().count(name))
        type_error(loc, "Variable '" + name + "' is already declared in this scope.");
    else
        scopes.back()[name] = {type, is_constant};
}

inline bool Type_checker::is_compatible(const types::Type &expected, const types::Type &actual) const
{
    if (is_any(expected))
        return true;

    if (is_optional(expected))
    {
        if (is_nil(actual))
            return true;
        if (is_optional(actual))
            return is_compatible(types::get_optional_type(expected)->base_type, types::get_optional_type(actual)->base_type);
        return is_compatible(types::get_optional_type(expected)->base_type, actual);
    }
    if (is_optional(actual) || is_nil(actual))
        return false;
    if (auto *expected_array = std::get_if<std::shared_ptr<types::Array_type>>(&expected))
    {
        if (is_any((*expected_array)->element_type) && is_array(actual))
            return true;
    }
    if (const auto *a_array = std::get_if<std::shared_ptr<types::Array_type>>(&expected))
    {
        if (const auto *b_array = std::get_if<std::shared_ptr<types::Array_type>>(&actual))
            return is_compatible((*a_array)->element_type, (*b_array)->element_type);
    }
    if (const auto *a_closure = std::get_if<std::shared_ptr<types::Closure_type>>(&expected))
    {
        if (const auto *b_closure = std::get_if<std::shared_ptr<types::Closure_type>>(&actual))
            return (*a_closure)->function_type == (*b_closure)->function_type;
    }
    return expected == actual;
}

inline bool Type_checker::is_argument_compatible(const std::vector<types::Type> &allowed_types, const types::Type &actual_type) const
{
    for (const auto &allowed : allowed_types)
        if (is_compatible(allowed, actual_type))
            return true;
    return false;
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

inline bool Type_checker::is_array(const types::Type &type) const
{
    return std::holds_alternative<std::shared_ptr<types::Array_type>>(type);
}

inline bool Type_checker::is_function(const types::Type &type) const
{
    return std::holds_alternative<std::shared_ptr<types::Function_type>>(type);
}

inline bool Type_checker::is_closure(const types::Type &type) const
{
    return std::holds_alternative<std::shared_ptr<types::Closure_type>>(type);
}

inline bool Type_checker::is_model(const types::Type &type) const
{
    return std::holds_alternative<std::shared_ptr<types::Model_type>>(type);
}

inline bool Type_checker::is_any(const types::Type &type) const
{
    if (const auto *prim = std::get_if<types::Primitive_kind>(&type))
        return *prim == types::Primitive_kind::Any;
    return false;
}

inline bool Type_checker::is_nil(const types::Type &type) const { return types::is_nil(type); }
inline bool Type_checker::is_optional(const types::Type &type) const { return types::is_optional(type); }
inline Result<std::pair<types::Type, bool>> Type_checker::lookup_variable(const std::string &name, const ast::Source_location &loc)
{
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it)
        if (it->contains(name))
            return it->at(name);
    return std::unexpected(err::msg("", "", 0, 0));
}

inline Result<std::pair<types::Type, bool>> Type_checker::lookup(const std::string &name, const ast::Source_location &loc)
{
    auto var = lookup_variable(name, loc);
    if (var)
        return var;
    if (functions.contains(name))
    {
        auto func_type = std::make_shared<types::Function_type>();
        const auto &func_data = functions.at(name);
        for (const auto &param : func_data.declaration->parameters) func_type->parameter_types.push_back(param.type);
        func_type->return_type = func_data.declaration->return_type;
        return std::make_pair(types::Type(func_type), true);
    }
    type_error(loc, "Undefined variable or function '" + name + "'.");
    return std::unexpected(err::msg("", "", 0, 0));
}

inline void Type_checker::check_stmt(ast::Stmt &stmt)
{
    std::visit([this](auto &s) { check_stmt_node(s); }, stmt.node);
}

inline Result<types::Type> Type_checker::check_expr(ast::Expr &expr)
{
    return std::visit([this](auto &e) -> Result<types::Type> { return check_expr_node(e); }, expr.node);
}

inline void Type_checker::check_stmt_node(ast::Function_stmt &stmt)
{
    auto saved_return = current_return_type;
    current_return_type = stmt.return_type;
    begin_scope();
    if (current_model_type)
        declare("this", current_model_type.value(), true, stmt.loc);
    for (const auto &p : stmt.parameters) declare(p.name, p.type, p.is_const, stmt.loc);
    if (stmt.body)
        check_stmt(*stmt.body);
    end_scope();
    current_return_type = saved_return;
}

inline void Type_checker::check_stmt_node(ast::Var_stmt &stmt)
{
    types::Type init_type = types::Type(types::Primitive_kind::Void);
    if (stmt.initializer)
    {
        auto res = check_expr(*stmt.initializer);
        if (res)
            init_type = res.value();
        else
            return;
    }
    if (stmt.type_inferred)
    {
        if (is_array(init_type))
        {
            const auto &array_type = std::get<std::shared_ptr<types::Array_type>>(init_type);
            if (array_type->element_type == types::Type(types::Primitive_kind::Void))
                type_error(stmt.loc, "Cannot infer type of an empty array initializer.");
        }
        stmt.type = init_type;
    }
    else if (stmt.initializer && !is_compatible(stmt.type, init_type))
    {
        type_error(stmt.loc, std::string("Initializer type does not match variable's declared type.") + "expected: " + types::type_to_string(stmt.type) + " got: " + types::type_to_string(init_type));
    }
    declare(stmt.name, stmt.type, stmt.is_const, stmt.loc);
}

inline void Type_checker::check_stmt_node(ast::Model_stmt &stmt)
{
    auto saved_model = current_model_type;
    current_model_type = model_signatures.at(stmt.name);
    for (auto &method : stmt.methods) check_stmt_node(*method);
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
    auto condition_res = check_expr(*stmt.condition);
    if (!condition_res)
        return;
    auto condition_type = condition_res.value();
    if (!is_boolean(condition_type) && !is_optional(condition_type))
        type_error(ast::get_loc(stmt.condition->node), "If condition must be a boolean or an optional type.");
    std::string narrowed_var_name;
    bool narrow_in_then = false;
    bool narrow_in_else = false;
    if (auto *bin_expr = std::get_if<ast::Binary_expr>(&stmt.condition->node))
    {
        ast::Variable_expr *var_expr = nullptr;
        if (auto *left_var = std::get_if<ast::Variable_expr>(&bin_expr->left->node))
        {
            if (is_nil(ast::get_type(bin_expr->right->node)))
                var_expr = left_var;
        }
        else if (auto *right_var = std::get_if<ast::Variable_expr>(&bin_expr->right->node))
        {
            if (is_nil(ast::get_type(bin_expr->left->node)))
                var_expr = right_var;
        }
        if (var_expr)
        {
            narrowed_var_name = var_expr->name;
            if (bin_expr->op == lex::TokenType::NotEqual)
                narrow_in_then = true;
            else if (bin_expr->op == lex::TokenType::Equal)
                narrow_in_else = true;
        }
    }
    else if (auto *var_expr = std::get_if<ast::Variable_expr>(&stmt.condition->node))
    {
        if (is_optional(var_expr->type))
        {
            narrowed_var_name = var_expr->name;
            narrow_in_then = true;
        }
    }
    if (narrow_in_then && !narrowed_var_name.empty())
        m_nil_checked_vars_stack.back().insert(narrowed_var_name);
    if (stmt.then_branch)
        check_stmt(*stmt.then_branch);
    if (narrow_in_then && !narrowed_var_name.empty())
        m_nil_checked_vars_stack.back().erase(narrowed_var_name);
    if (narrow_in_else && !narrowed_var_name.empty())
        m_nil_checked_vars_stack.back().insert(narrowed_var_name);
    if (stmt.else_branch)
        check_stmt(*stmt.else_branch);
    if (narrow_in_else && !narrowed_var_name.empty())
        m_nil_checked_vars_stack.back().erase(narrowed_var_name);
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

inline void Type_checker::check_stmt_node(ast::While_stmt &stmt)
{
    if (stmt.condition)
    {
        auto res = check_expr(*stmt.condition);
        if (res && !is_boolean(res.value()))
            type_error(ast::get_loc(stmt.condition->node), "While condition must be a boolean");
    }
    if (stmt.body)
        check_stmt(*stmt.body);
}

inline void Type_checker::check_stmt_node(ast::For_stmt &stmt)
{
    begin_scope();
    if (stmt.initializer)
        check_stmt(*stmt.initializer);
    if (stmt.condition)
    {
        auto res = check_expr(*stmt.condition);
        if (res && !is_boolean(res.value()))
            type_error(ast::get_loc(stmt.condition->node), "For loop condition must be a boolean");
    }
    if (stmt.increment)
        std::ignore = check_expr(*stmt.increment);
    if (stmt.body)
        check_stmt(*stmt.body);
    end_scope();
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Assignment_expr &expr)
{
    auto var_info_res = lookup(expr.name, expr.loc);
    if (!var_info_res)
        return std::unexpected(err::msg("Variable not found", "", 0, 0));
    bool is_constant = var_info_res.value().second;
    if (is_constant)
        type_error(expr.loc, "Cannot assign to a constant variable '" + expr.name + "'.");
    auto var_type = var_info_res.value().first;
    auto val_type_res = check_expr(*expr.value);
    if (!val_type_res)
        return val_type_res;
    if (!is_compatible(var_type, val_type_res.value()))
        type_error(expr.loc, "Assignment type mismatch");
    return expr.type = var_type;
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
        return expr.type = types::Primitive_kind::Void;
    auto actual_type = type_res.value().first;
    for (const auto &scope : m_nil_checked_vars_stack)
    {
        if (scope.count(expr.name))
        {
            if (is_optional(actual_type))
                return expr.type = types::get_optional_type(actual_type)->base_type;
        }
    }
    return expr.type = actual_type;
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Binary_expr &expr)
{
    auto left_res = check_expr(*expr.left);
    auto right_res = check_expr(*expr.right);
    if (!left_res || !right_res)
        return expr.type = types::Primitive_kind::Void;

    types::Type left_type = left_res.value();
    types::Type right_type = right_res.value();

    switch (expr.op)
    {
        // --- Arithmetic Operators ---
        case lex::TokenType::Plus:
            if (is_string(left_type) && is_string(right_type))
                return expr.type = types::Primitive_kind::String;
            // Fallthrough for numeric addition
        case lex::TokenType::Minus:
        case lex::TokenType::Star:
            if (!is_numeric(left_type) || !is_numeric(right_type))
            {
                type_error(expr.loc, "Operands must be two numbers or two strings for '+'");
                return expr.type = types::Primitive_kind::Void;
            }
            return expr.type = promote_numeric_type(left_type, right_type);

        case lex::TokenType::Slash:
            if (!is_numeric(left_type) || !is_numeric(right_type))
            {
                type_error(expr.loc, "Operands for division must be numbers.");
                return expr.type = types::Primitive_kind::Void;
            }
            // Division always results in a float.
            return expr.type = types::Primitive_kind::Float;

        case lex::TokenType::Percent:
            if (left_type != types::Type(types::Primitive_kind::Int) || right_type != types::Type(types::Primitive_kind::Int))
            {
                type_error(expr.loc, "Operands for '%' must be integers.");
                return expr.type = types::Primitive_kind::Void;
            }
            return expr.type = types::Primitive_kind::Int;

        // --- Comparison Operators ---
        case lex::TokenType::Greater:
        case lex::TokenType::GreaterEqual:
        case lex::TokenType::Less:
        case lex::TokenType::LessEqual:
            if (!is_numeric(left_type) || !is_numeric(right_type))
                type_error(expr.loc, "Comparison operators require numeric operands.");
            return expr.type = types::Primitive_kind::Bool;

        // --- Equality Operators ---
        case lex::TokenType::Equal:
        case lex::TokenType::NotEqual:
            // Allow comparison of an optional with nil
            if ((is_optional(left_type) && is_nil(right_type)) || (is_nil(left_type) && is_optional(right_type)))
                return expr.type = types::Primitive_kind::Bool;
            if (!is_compatible(left_type, right_type) && !is_compatible(right_type, left_type))
                type_error(expr.loc, "Cannot compare incompatible types.");
            return expr.type = types::Primitive_kind::Bool;

        // --- Logical Operators ---
        case lex::TokenType::LogicalAnd:
        case lex::TokenType::LogicalOr:
            if (!is_boolean(left_type) || !is_boolean(right_type))
                type_error(expr.loc, "Operands for logical operators must be booleans.");
            return expr.type = types::Primitive_kind::Bool;

        default:
            type_error(expr.loc, "Unsupported binary operator.");
            return expr.type = types::Primitive_kind::Void;
    }
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Call_expr &expr)
{
    if (m_native_signatures.count(expr.callee))
    {
        const auto &signature = m_native_signatures.at(expr.callee);
        if (expr.arguments.size() != signature.allowed_params.size())
        {
            type_error(expr.loc, "Incorrect number of arguments for '" + expr.callee + "'. Expected " +
                                     std::to_string(signature.allowed_params.size()) + ", but got " +
                                     std::to_string(expr.arguments.size()) + ".");
            return expr.type = signature.return_type;
        }
        for (size_t i = 0; i < expr.arguments.size(); ++i)
        {
            auto arg_type_res = check_expr(*expr.arguments[i]);
            if (!arg_type_res)
                return types::Primitive_kind::Void;
            const auto &allowed_for_param = signature.allowed_params[i];
            if (!is_argument_compatible(allowed_for_param, arg_type_res.value()))
                type_error(ast::get_loc(expr.arguments[i]->node), "Argument type mismatch for function '" + expr.callee + "'.");
        }
        return expr.type = signature.return_type;
    }
    if (functions.count(expr.callee))
    {
        const auto &func_data = functions.at(expr.callee);
        const auto &signature = func_data.declaration;
        if (expr.arguments.size() != signature->parameters.size())
        {
            type_error(expr.loc, "Incorrect number of arguments for function '" + expr.callee + "'.");
        }
        else
        {
            for (size_t i = 0; i < expr.arguments.size(); ++i)
            {
                auto arg_type_res = check_expr(*expr.arguments[i]);
                if (arg_type_res && !is_compatible(signature->parameters[i].type, arg_type_res.value()))
                    type_error(ast::get_loc(expr.arguments[i]->node), "Argument type mismatch.");
            }
        }
        return expr.type = signature->return_type;
    }
    auto callable_var_res = lookup_variable(expr.callee, expr.loc);
    if (callable_var_res)
    {
        if (auto *ct = std::get_if<std::shared_ptr<types::Closure_type>>(&callable_var_res.value().first))
        {
            const auto &signature = (*ct)->function_type;
            return expr.type = signature.return_type;
        }
    }
    type_error(expr.loc, "'" + expr.callee + "' is not a function or callable variable.");
    return types::Primitive_kind::Void;
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Cast_expr &expr)
{
    auto original_type_res = check_expr(*expr.expression);
    if (!original_type_res)
        return std::unexpected(original_type_res.error());
    auto original_type = original_type_res.value();
    auto &target_type = expr.target_type;
    if (is_optional(original_type) && !is_optional(target_type))
    {
        if (auto *var_expr = std::get_if<ast::Variable_expr>(&expr.expression->node))
        {
            bool is_checked = false;
            for (const auto &scope : m_nil_checked_vars_stack)
            {
                if (scope.count(var_expr->name))
                {
                    is_checked = true;
                    break;
                }
            }
            if (!is_checked)
            {
                type_error(expr.loc, "Cannot cast optional type '" + types::type_to_string(original_type) + "' to non-optional type '" +
                                         types::type_to_string(target_type) + "' without a nil check.");
            }
        }
        else
        {
            type_error(expr.loc, "Cannot cast an optional expression to a non-optional type.");
        }
    }
    if (is_nil(original_type) && !is_optional(target_type))
        type_error(expr.loc, "Cannot cast a 'nil' value to a non-optional type.");
    return target_type;
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Closure_expr &expr)
{
    auto saved_ret = current_return_type;
    current_return_type = expr.return_type;

    begin_scope();

    for (auto const &p : expr.parameters) declare(p.name, p.type, p.is_const, expr.loc);

    if (expr.body)
        check_stmt(*expr.body);

    end_scope();
    current_return_type = saved_ret;

    auto ct = std::make_shared<types::Closure_type>();

    for (auto const &p : expr.parameters) ct->function_type.parameter_types.push_back(p.type);

    ct->function_type.return_type = expr.return_type;
    return expr.type = types::Type(ct);
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Field_access_expr &expr)
{
    auto obj_type_res = check_expr(*expr.object);
    if (!obj_type_res)
        return expr.type = types::Primitive_kind::Void;
    auto obj_type = obj_type_res.value();
    if (is_optional(obj_type))
    {
        type_error(expr.loc, "Cannot access field on an optional type. Use an 'if' check to unwrap it first.");
        return expr.type = types::Primitive_kind::Void;
    }
    if (auto *mt = std::get_if<std::shared_ptr<types::Model_type>>(&obj_type))
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
        return expr.type = types::Primitive_kind::Void;
    auto val_type = check_expr(*expr.value);
    if (!val_type)
        return expr.type = types::Primitive_kind::Void;
    if (!is_compatible(field_type.value(), val_type.value()))
        type_error(expr.loc, "Assignment type mismatch for field");
    return expr.type = field_type.value();
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Literal_expr &expr)
{
    if (auto *nil_literal = std::get_if<std::nullptr_t>(&expr.value))
        return expr.type = types::Primitive_kind::Nil;
    return expr.type;
}

inline Result<types::Type> Type_checker::check_expr_node(ast::Method_call_expr &expr)
{
    auto obj_type_res = check_expr(*expr.object);
    if (!obj_type_res)
        return expr.type = types::Primitive_kind::Void;
    auto obj_type = obj_type_res.value();
    if (expr.method_name == "map")
    {
        if (expr.arguments.size() != 1)
            type_error(expr.loc, "Map must have one and only one closure argument.");
        auto closure_type_res = check_expr(*expr.arguments[0]);
        if (!closure_type_res || !is_closure(closure_type_res.value()))
        {
            type_error(ast::get_loc(expr.arguments[0]->node), "Argument to 'map' must be a closure.");
            return expr.type = types::Primitive_kind::Void;
        }

        auto closure_type = types::get_closure_type(closure_type_res.value());

        if (is_array(obj_type))
        {
            // Check if closure parameter is compatible with array element type T
            auto element_type = types::get_array_type(obj_type)->element_type;
            if (closure_type->function_type.parameter_types.size() != 1 ||
                !is_compatible(closure_type->function_type.parameter_types[0], element_type))
            {
                type_error(ast::get_loc(expr.arguments[0]->node), "Closure parameter type is not compatible with array element type.");
            }
            // map on T[] with closure |T|->U returns U[]
            return expr.type = types::Type(std::make_shared<types::Array_type>(closure_type->function_type.return_type));
        }

        if (is_optional(obj_type))
        {
            // Check if closure parameter is compatible with optional base type T
            auto base_type = types::get_optional_type(obj_type)->base_type;
            if (closure_type->function_type.parameter_types.size() != 1 ||
                !is_compatible(closure_type->function_type.parameter_types[0], base_type))
            {
                type_error(ast::get_loc(expr.arguments[0]->node), "Closure parameter type is not compatible with optional's base type.");
            }
            // map on T? with closure |T|->U returns U?
            return expr.type = types::Type(std::make_shared<types::Optional_type>(closure_type->function_type.return_type));
        }
    }
    if (m_native_methods.count(expr.method_name))
    {
        const auto &signature = m_native_methods.at(expr.method_name);
        bool is_valid_this_type = false;
        for (const auto &valid_type : signature.valid_this_types)
        {
            if (is_compatible(valid_type, obj_type))
            {
                is_valid_this_type = true;
                break;
            }
        }
        if (is_valid_this_type)
        {
            if (expr.arguments.size() != signature.allowed_params.size())
            {
                type_error(expr.loc, "Incorrect number of arguments for '" + expr.method_name + "'.");
                return expr.type = signature.return_type;
            }
            for (size_t i = 0; i < expr.arguments.size(); ++i)
            {
                auto arg_type_res = check_expr(*expr.arguments[i]);
                if (!arg_type_res)
                    return types::Primitive_kind::Void;
                auto allowed_for_param = signature.allowed_params[i];
                auto actual_arg_type = arg_type_res.value();
                if (is_any(allowed_for_param[0]) && is_array(obj_type))
                    allowed_for_param[0] = types::get_array_type(obj_type)->element_type;
                if (!is_argument_compatible(allowed_for_param, actual_arg_type))
                    type_error(ast::get_loc(expr.arguments[i]->node), "Argument type mismatch for method '" + expr.method_name + "'.");
            }
            if (is_optional(obj_type))
            {
                if (expr.method_name == "value" || expr.method_name == "value_or")
                    return expr.type = types::get_optional_type(obj_type)->base_type;
            }
            if (is_any(signature.return_type))
            {
                if (is_array(obj_type))
                    return expr.type = types::get_array_type(obj_type)->element_type;
            }
            return expr.type = signature.return_type;
        }
    }
    if (is_optional(obj_type))
    {
        type_error(expr.loc, "Cannot call method on an optional type. Use an 'if' check to unwrap it first.");
        return expr.type = types::Primitive_kind::Void;
    }
    if (auto *mt = std::get_if<std::shared_ptr<types::Model_type>>(&obj_type))
    {
        if ((*mt)->methods.count(expr.method_name))
        {
            const auto &method_signature = (*mt)->methods.at(expr.method_name);
            return expr.type = method_signature.return_type;
        }
    }
    type_error(expr.loc, "No method named '" + expr.method_name + "' found for this type.");
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
        return expr.type = types::Primitive_kind::Void;
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

inline Result<types::Type> Type_checker::check_expr_node(ast::Array_literal_expr &expr)
{
    if (expr.elements.empty())
        return expr.type = types::Type(std::make_shared<types::Array_type>(types::Type(types::Primitive_kind::Void)));
    types::Type common_type = types::Primitive_kind::Nil;
    bool has_nil = false;
    for (const auto &elem_expr : expr.elements)
    {
        auto elem_type_res = check_expr(*elem_expr);
        if (!elem_type_res)
            continue;
        auto elem_type = elem_type_res.value();
        if (is_nil(elem_type))
        {
            has_nil = true;
        }
        else if (is_nil(common_type))
        {
            common_type = elem_type;
        }
        else if (!is_compatible(common_type, elem_type) && !is_compatible(elem_type, common_type))
        {
            type_error(ast::get_loc(elem_expr->node), "Array elements must have a consistent type.");
            return expr.type = types::Type(std::make_shared<types::Array_type>(types::Primitive_kind::Void));
        }
    }
    if (has_nil && !is_optional(common_type) && !is_nil(common_type))
        common_type = types::Type(std::make_shared<types::Optional_type>(common_type));
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

}  //namespace phos
