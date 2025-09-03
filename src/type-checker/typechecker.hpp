#pragma once
#include <unordered_map>
#include <vector>
#include <string>
#include <optional>
#include "../error/err.hpp"
#include "../parser/ast.hpp"
#include "../value/type.hpp"

namespace phos
{
namespace types
{

class Type_checker
{
private:
    struct FunctionSignature
    {
        std::vector<types::Type> param_types;
        types::Type return_type;
    };

    std::vector<err::msg> errors;
    // Stack-based variable scopes for block/function scoping
    std::vector<std::unordered_map<std::string, types::Type>> variable_stack;
    std::unordered_map<std::string, FunctionSignature> functions;

public:
    Type_checker() { variable_stack.push_back({}); }
    void check(const std::vector<std::unique_ptr<phos::ast::Stmt>> &stmts)
    {
        for (const auto &stmt : stmts) check_stmt(*stmt);
    }
    // Error reporting
    const std::vector<err::msg> &get_errors() const { return errors; }
    bool has_errors() const { return !errors.empty(); }
    void add_error(const std::string &message, const phos::ast::Source_location &loc)
    {
        errors.push_back(err::msg(message.c_str(), "type-checking", loc.line, loc.column));
    }

    // Scoped variable environment
    void enter_scope() { variable_stack.push_back({}); }
    void exit_scope() { variable_stack.pop_back(); }
    void declare_variable(const std::string &name, const types::Type &type) { variable_stack.back()[name] = type; }

    std::optional<types::Type> lookup_variable(const std::string &name) const
    {
        for (auto it = variable_stack.rbegin(); it != variable_stack.rend(); ++it)
        {
            auto found = it->find(name);
            if (found != it->end())
                return found->second;
        }
        return std::nullopt;
    }
    // Function signature environment
    void declare_function(const std::string &name, const std::vector<types::Type> &param_types, const types::Type &return_type)
    {
        functions[name] = FunctionSignature{param_types, return_type};
    }

    std::optional<FunctionSignature> lookup_function(const std::string &name) const
    {
        auto it = functions.find(name);
        if (it != functions.end())
            return it->second;
        return std::nullopt;
    }

    // Deep type equality (expand as needed for arrays/structs)
    bool equals(const types::Type &t1, const types::Type &t2) const { return t1.kind_ == t2.kind_; }

    // Expression Type Checking
    types::Type check_expr(const phos::ast::Expr &expr)
    {
        using namespace phos::ast;
        switch (expr.node.index())
        {
            case 0:
                return check_expr_node(std::get<Literal_expr>(expr.node), expr);  // Literal
            case 1:
                return check_expr_node(std::get<Variable_expr>(expr.node), expr);  // Variable
            case 2:
                return check_expr_node(std::get<Binary_expr>(expr.node), expr);  // Binary
            case 3:
                return check_expr_node(std::get<Unary_expr>(expr.node), expr);  // Unary
            case 4:
                return check_expr_node(std::get<Call_expr>(expr.node), expr);  // Call
            case 5:
                return check_expr_node(std::get<Assignment_expr>(expr.node), expr);  // Assignment
            case 6:
                return check_expr_node(std::get<Cast_expr>(expr.node), expr);  // Cast
            default:
                add_error("Unknown expression type", expr.loc);
                return types::Type(types::kind::Void);
        }
    }

    types::Type check_expr_node(const phos::ast::Literal_expr &node, const phos::ast::Expr &expr)
    {
        using namespace types;
        return std::visit(
            [](const auto &val) -> Type
            {
                using T = std::decay_t<decltype(val)>;
                if constexpr (std::is_same_v<T, bool>)
                    return Type(kind::Bool);
                else if constexpr (std::is_same_v<T, int64_t>)
                    return Type(kind::Int);
                else if constexpr (std::is_same_v<T, double>)
                    return Type(kind::Float);
                else if constexpr (std::is_same_v<T, std::string>)
                    return Type(kind::String);
                else
                    return Type(kind::Void);
            },
            node.value);
    }

    types::Type check_expr_node(const phos::ast::Variable_expr &node, const phos::ast::Expr &expr)
    {
        auto type_opt = lookup_variable(node.name);
        if (!type_opt)
        {
            add_error("Undefined variable: " + node.name, expr.loc);
            return types::Type(types::kind::Void);
        }
        return *type_opt;
    }

    types::Type check_expr_node(const phos::ast::Binary_expr &node, const phos::ast::Expr &expr)
    {
        types::Type left = check_expr(*node.left);
        types::Type right = check_expr(*node.right);

        if (!equals(left, right))
        {
            // Allow int/float upgrade
            if (!((left.kind_ == types::kind::Float && right.kind_ == types::kind::Int) ||
                  (left.kind_ == types::kind::Int && right.kind_ == types::kind::Float)))
            {
                add_error("Type mismatch in binary expression. Left: " + left.to_string() + ", Right: " + right.to_string(), expr.loc);
            }
        }
        return (left.kind_ == types::kind::Float || right.kind_ == types::kind::Float) ? types::Type(types::kind::Float) : left;
    }

    types::Type check_expr_node(const phos::ast::Unary_expr &node, const phos::ast::Expr &expr) { return check_expr(*node.right); }

    types::Type check_expr_node(const phos::ast::Call_expr &node, const phos::ast::Expr &expr)
    {
        auto fsig = lookup_function(node.callee);
        if (!fsig)
        {
            add_error("Undefined function: " + node.callee, expr.loc);
            return types::Type(types::kind::Void);
        }
        // Check arg count and types
        if (node.arguments.size() != fsig->param_types.size())
        {
            add_error("Function call arity mismatch for '" + node.callee + "': expected " + std::to_string(fsig->param_types.size()) +
                          ", got " + std::to_string(node.arguments.size()),
                      expr.loc);
        }
        else
        {
            for (size_t i = 0; i < node.arguments.size(); ++i)
            {
                types::Type arg_type = check_expr(*node.arguments[i]);
                if (!equals(arg_type, fsig->param_types[i]))
                {
                    add_error("Function call argument type mismatch for '" + node.callee + "' at position " + std::to_string(i + 1) +
                                  ": expected " + fsig->param_types[i].to_string() + ", got " + arg_type.to_string(),
                              expr.loc);
                }
            }
        }
        return fsig->return_type;
    }

    types::Type check_expr_node(const phos::ast::Assignment_expr &node, const phos::ast::Expr &expr)
    {
        auto var_type_opt = lookup_variable(node.name);
        types::Type value_type = check_expr(*node.value);

        if (!var_type_opt)
        {
            add_error("Assignment to undefined variable: " + node.name, expr.loc);
            return types::Type(types::kind::Void);
        }
        if (!equals(*var_type_opt, value_type))
        {
            add_error("Type mismatch in assignment to '" + node.name + "': expected " + var_type_opt->to_string() + ", got " +
                          value_type.to_string(),
                      expr.loc);
        }
        return *var_type_opt;
    }

    types::Type check_expr_node(const phos::ast::Cast_expr &node, const phos::ast::Expr &expr)
    {
        types::Type src_type = check_expr(*node.expression);
        // Extract target type from expr
        types::Type target_type = expr.type;  // Provided by parser, else fallback
        return target_type;
    }

    // Statement Type Checking
    void check_stmt(const phos::ast::Stmt &stmt)
    {
        using namespace phos::ast;
        switch (stmt.node.index())
        {
            case 0:
                check_stmt_node(std::get<Return_stmt>(stmt.node), stmt);
                break;
            case 1:
                check_stmt_node(std::get<Function_stmt>(stmt.node), stmt);
                break;
            case 2:
                check_stmt_node(std::get<Var_stmt>(stmt.node), stmt);
                break;
            case 3:
                check_stmt_node(std::get<Print_stmt>(stmt.node), stmt);
                break;
            case 4:
                check_stmt_node(std::get<Expr_stmt>(stmt.node), stmt);
                break;
            case 5:
                check_stmt_node(std::get<Block_stmt>(stmt.node), stmt);
                break;
            case 6:
                check_stmt_node(std::get<If_stmt>(stmt.node), stmt);
                break;
            case 7:
                check_stmt_node(std::get<While_stmt>(stmt.node), stmt);
                break;
            case 8:
                check_stmt_node(std::get<For_stmt>(stmt.node), stmt);
                break;
            default:
                add_error("Unknown statement type", stmt.loc);
        }
    }

    void check_stmt_node(const phos::ast::Return_stmt &node, const phos::ast::Stmt &stmt)
    {
        if (node.expression)
            check_expr(*node.expression);
    }

    void check_stmt_node(const phos::ast::Function_stmt &node, const phos::ast::Stmt &stmt)
    {
        // Register function signature
        std::vector<types::Type> param_types;
        for (auto &param : node.parameters) param_types.push_back(param.second);
        declare_function(node.name, param_types, node.return_type);

        enter_scope();
        for (const auto &param : node.parameters) declare_variable(param.first, param.second);
        check_stmt(*node.body);
        exit_scope();
    }

    void check_stmt_node(const phos::ast::Var_stmt &node, const phos::ast::Stmt &stmt)
    {
        declare_variable(node.name, node.type);
        if (node.initializer)
        {
            types::Type init_type = check_expr(*node.initializer);
            if (!equals(node.type, init_type))
            {
                add_error("Variable '" + node.name + "' declaration type does not match initializer: expected " + node.type.to_string() +
                              ", got " + init_type.to_string(),
                          stmt.loc);
            }
        }
    }

    void check_stmt_node(const phos::ast::Print_stmt &node, const phos::ast::Stmt &stmt) { check_expr(*node.expression); }
    void check_stmt_node(const phos::ast::Expr_stmt &node, const phos::ast::Stmt &stmt) { check_expr(*node.expression); }

    void check_stmt_node(const phos::ast::Block_stmt &node, const phos::ast::Stmt &stmt)
    {
        enter_scope();
        for (const auto &st : node.statements) check_stmt(*st);
        exit_scope();
    }

    void check_stmt_node(const phos::ast::If_stmt &node, const phos::ast::Stmt &stmt)
    {
        types::Type cond_type = check_expr(*node.condition);
        if (cond_type.kind_ != types::kind::Bool)
            add_error("If condition must be boolean, got " + cond_type.to_string(), stmt.loc);
        check_stmt(*node.then_branch);
        if (node.else_branch)
            check_stmt(*node.else_branch);
    }

    void check_stmt_node(const phos::ast::While_stmt &node, const phos::ast::Stmt &stmt)
    {
        types::Type cond_type = check_expr(*node.condition);
        if (cond_type.kind_ != types::kind::Bool)
            add_error("While condition must be boolean, got " + cond_type.to_string(), stmt.loc);
        check_stmt(*node.body);
    }

    void check_stmt_node(const phos::ast::For_stmt &node, const phos::ast::Stmt &stmt)
    {
        enter_scope();
        if (node.initializer)
            check_stmt(*node.initializer);
        if (node.condition)
        {
            types::Type cond_type = check_expr(*node.condition);
            if (cond_type.kind_ != types::kind::Bool)
                add_error("For condition must be boolean, got " + cond_type.to_string(), stmt.loc);
        }
        if (node.increment)
            check_expr(*node.increment);
        check_stmt(*node.body);
        exit_scope();
    }
};

}  // namespace types
}  // namespace phos
