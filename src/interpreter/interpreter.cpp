#include "interpreter.hpp"
#include "./native_globals.hpp"
#include "../error/err.hpp"
#include "../utility/utility.hpp"
#include "../utility/try_res.hpp"

#include <format>
#include <print>
#include <variant>

// RAII guard to restore the environment automatically after scope exits
template <typename F>
struct Finally
{
    F func;
    ~Finally() { func(); }
};

template <typename F>
Finally<F> finally(F func)
{
    return {func};
}

namespace phos
{

Interpreter::Interpreter()
{
    globals = mem::make_rc<Environment>();
    environment = globals;
    auto native_functions = native::get_natives();
    for (auto &a : native_functions) this->define_native(a->name, a->arity, a->signature, a->parameters, a->code);

    auto native_mthds = native::get_native_methods();

    for (auto &a : native_mthds) this->define_native_method(a.this_type, a.name, a.arity, a.code);
}

Result<void> Interpreter::interpret(const std::vector<ast::Stmt*> &statements)
{
    for (const auto &statement : statements)
    {
        auto result = __Try(execute(*statement));
        if (result.is_return)
            return std::unexpected(err::msg("Cannot return from top-level code.", "interpreter", 0, 0));
    }
    return {};
}

void Interpreter::define_native(const std::string &name, int arity, types::Function_type signature,
                                std::vector<std::pair<std::string, types::Type>> parameters,
                                std::function<Result<Value>(const std::vector<Value> &)> code)
{
    auto func_val = mem::make_rc<Native_function_value>();
    func_val->name = name;
    func_val->arity = arity;
    func_val->signature = std::move(signature);
    func_val->parameters = std::move(parameters);
    func_val->code = std::move(code);

    std::ignore = globals->define(name, Value(func_val));
}

void Interpreter::define_native_method(std::string type, const std::string &name, int arity, Native_method code)
{
    native_methods[type][name] = code;
}

// TODO: Complete refactor

Result<Value> Interpreter::evaluate_visitor(const ast::Array_literal_expr & expr)
{
    auto array_val = mem::make_rc<Array_value>();
    for (const auto &elem_expr : expr.elements)
    {
        auto elem_result = __Try(evaluate(*elem_expr));
        array_val->elements.push_back(elem_result);
    }
    return Value(array_val);
}

Result<Value> Interpreter::evaluate_visitor(const ast::Array_access_expr & expr)
{
    auto array_res = evaluate(*expr.array);
    if (!array_res)
        return array_res;
    auto index_res = evaluate(*expr.index);
    if (!index_res)
        return index_res;

    if (!is_array(array_res.value()))
        return std::unexpected(err::msg("Can only use subscript '[]' on arrays.", "interpreter", expr.loc.line, expr.loc.column));
    if (!is_int(index_res.value()))
        return std::unexpected(err::msg("Array index must be an integer.", "interpreter", expr.loc.line, expr.loc.column));

    auto array_val = get_array(array_res.value());
    auto index = get_int(index_res.value());

    if (index < 0 || static_cast<size_t>(index) >= array_val->elements.size())
        return std::unexpected(err::msg("Array index out of bounds.", "interpreter", expr.loc.line, expr.loc.column));

    return array_val->elements.at(index);
}

Result<Value> Interpreter::evaluate_visitor(const ast::Array_assignment_expr & expr)
{
    auto array_res = __Try(evaluate(*expr.array));
    auto index_res = __Try(evaluate(*expr.index));
    auto value_res = __Try(evaluate(*expr.value));

    if (!is_array(array_res))
        return std::unexpected(
            err::msg("Can only use subscript '[]' on arrays for assignment.", "interpreter", expr.loc.line, expr.loc.column));
    if (!is_int(index_res))
        return std::unexpected(err::msg("Array index must be an integer.", "interpreter", expr.loc.line, expr.loc.column));

    auto array_val = get_array(array_res);
    auto index = get_int(index_res);

    if (index < 0 || static_cast<size_t>(index) >= array_val->elements.size())
        return std::unexpected(err::msg("Array index out of bounds.", "interpreter", expr.loc.line, expr.loc.column));

    array_val->elements[index] = value_res;
    return value_res;
}

Result<Value> Interpreter::evaluate_visitor(const ast::Binary_expr& expr)
{
    auto left_result = __Try(evaluate(*expr.left));
    auto right_result = __Try(evaluate(*expr.right));

    const auto &left = left_result;
    const auto &right = right_result;
    Result<Value> res;
    switch (expr.op)
    {
        case lex::TokenType::Plus:
            res = add(left, right); break;
        case lex::TokenType::Minus:
            res = subtract(left, right); break;
        case lex::TokenType::Star:
            res = multiply(left, right); break;
        case lex::TokenType::Slash:
            res = divide(left, right); break;
        case lex::TokenType::Percent:
            res = modulo(left, right); break;
        case lex::TokenType::Equal:
            res = Value(equals(left, right)); break;
        case lex::TokenType::NotEqual:
            res = Value(!equals(left, right)); break;
        case lex::TokenType::Less:
            res = lessThan(left, right); break;
        case lex::TokenType::LessEqual:
            res = less_than_or_equal(left, right); break;
        case lex::TokenType::Greater:
            res = greater_than(left, right); break;
        case lex::TokenType::GreaterEqual:
            res = greater_than_or_equal(left, right); break;
        case lex::TokenType::LogicalAnd:
            res = Value(is_truthy(left) && is_truthy(right)); break;
        case lex::TokenType::LogicalOr:
            res = Value(is_truthy(left) || is_truthy(right)); break;
        default:
            return std::unexpected(err::msg("Unknown binary operator " + util::operator_token_to_string(expr.op), "interpreter", expr.loc.line, expr.loc.column));
    }
    if (!res.has_value())
    {
        res.error().line = expr.loc.line;
        res.error().column = expr.loc.column;
    }
    return res;
}

Result<Value> Interpreter::evaluate_visitor(const ast::Call_expr& expr)
{
    // 1. Evaluate the callee expression to get the function/closure Value itself.
    auto callee = __Try(evaluate(*expr.callee));

    // 2. Evaluate all the argument expressions.
    std::vector<Value> arguments;
    arguments.reserve(expr.arguments.size());
    for (const auto& argument : expr.arguments)
    {
        arguments.push_back(__Try(evaluate(*argument)));
    }

    // 3. Pass the evaluated callee and arguments to the new call helper.
    return call(callee, arguments, expr.loc);
}

Result<Value> Interpreter::evaluate_visitor(const ast::Closure_expr& expr)
{
    size_t id = next_closure_id++;

    types::Closure_type closure_type_info;
    for (const auto &param : expr.parameters) closure_type_info.function_type.parameter_types.push_back(param.type);
    closure_type_info.function_type.return_type = expr.return_type;

    closures[id] = ClosureData{id, closure_type_info, mem::make_rc<ast::Closure_expr>(expr), environment};

    std::vector<std::pair<std::string, types::Type>> param_data;
    for (const auto &p : expr.parameters) param_data.push_back({p.name, p.type});

    auto func_val = mem::make_rc<Function_value>(closure_type_info.function_type, param_data, environment);
    auto closure_val = mem::make_rc<Closure_value>(func_val, std::unordered_map<std::string, Value>{});
    closure_val->id = id;

    return Value(closure_val);
}

Result<Value> Interpreter::evaluate_visitor(const ast::Cast_expr& expr)
{
    auto value_result = __Try(evaluate(*expr.expression));
    return cast_value(value_result, expr.target_type);
}

Result<Value> Interpreter::evaluate_visitor(const ast::Field_access_expr& expr)
{

    auto object_result = __Try(evaluate(*expr.object));

    if (!is_model(object_result))
        return std::unexpected(
            err::msg("Can only access fields on model instances.", "interpreter", expr.loc.line, expr.loc.column));

    auto model_val = get_model(object_result);
    if (model_val->fields.contains(expr.field_name))
        return model_val->fields.at(expr.field_name);

    return std::unexpected(err::msg("Field '" + expr.field_name + "' not found.", "interpreter", expr.loc.line, expr.loc.column));
}

Result<Value> Interpreter::evaluate_visitor(const ast::Field_assignment_expr& expr)
{
    auto object_result = __Try(evaluate(*expr.object));

    if (!is_model(object_result))
        return std::unexpected(
            err::msg("Can only assign to fields of a model instance.", "interpreter", expr.loc.line, expr.loc.column));

    auto value_res = __Try(evaluate(*expr.value));
    get_model(object_result)->fields[expr.field_name] = value_res;
    return value_res;
}

Result<Value> Interpreter::evaluate_visitor(const ast::Literal_expr& expr)
{
    return expr.value;
}

Result<Value> Interpreter::evaluate_visitor(const ast::Variable_expr& expr)
{
    return environment->get(expr.name);
}

Result<Value> Interpreter::evaluate_visitor(const ast::Unary_expr& expr)
{
    auto right_result = __Try(evaluate(*expr.right));

    switch (expr.op)
    {
        case lex::TokenType::Minus:
            return negate(right_result);
        case lex::TokenType::LogicalNot:
            return Value(!is_truthy(right_result));
        default:
            return std::unexpected(err::msg("Unknown unary operator.", "interpreter", expr.loc.line, expr.loc.column));
    }
}

Result<Value> Interpreter::evaluate_visitor(const ast::Method_call_expr& expr)
{
    auto object_result = __Try(evaluate(*expr.object));

    Value object = object_result;

    std::vector<Value> arguments;
    for (const auto &argument : expr.arguments)
    {
        auto arg_result = __Try(evaluate(*argument));
        arguments.push_back(arg_result);
    }
    if (expr.method_name == "map")
    {
        auto object_result = __Try(evaluate(*expr.object));
        Value object = object_result;

        if (expr.arguments.size() != 1)
            return std::unexpected(
                err::msg("map() expects exactly one argument.", "interpreter", expr.loc.line, expr.loc.column));

        auto closure_res = evaluate(*expr.arguments[0]);
        if (!closure_res || !is_closure(closure_res.value()))
        {
            return std::unexpected(
                err::msg("map() expects a closure as its argument.", "interpreter", expr.loc.line, expr.loc.column));
        }

        if (is_array(object))
            return this->map_array(object, closure_res.value());
        if (is_optional(ast::get_type(expr.object->node)))
            return this->map_optional(object, closure_res.value());

        return std::unexpected(err::msg(".map() is not supported for this type.", "interpreter", expr.loc.line, expr.loc.column));
    }

    // 1. Check for NATIVE methods on OPTIONAL types first.
    if (is_optional(ast::get_type(expr.object->node)))
    {
        if (native_methods.count(native::optional_type_string) &&
            native_methods.at(native::optional_type_string).count(expr.method_name))
        {
            return native_methods.at(native::optional_type_string).at(expr.method_name)(object, arguments);
        }
    }

    // 2. Check for NATIVE methods on ARRAYS.
    if (is_array(object))
    {
        if (native_methods.count(native::array_type_string) &&
            native_methods.at(native::array_type_string).count(expr.method_name))
            return native_methods.at(native::array_type_string).at(expr.method_name)(object, arguments);
    }

    // 3. Check for NATIVE methods on STRINGS.
    if (is_string(object))
    {
        if (native_methods.count(native::string_type_string) &&
            native_methods.at(native::string_type_string).count(expr.method_name))
        {
            return native_methods.at(native::string_type_string).at(expr.method_name)(object, arguments);
        }
    }

    // 4. Check for USER-DEFINED methods on MODELS.
    if (is_model(object))
    {
        auto model_val = get_model(object);
        const auto &model_name = model_val->signature.name;

        if (model_data.contains(model_name) && model_data.at(model_name).methods.contains(expr.method_name))
        {
            const auto &method_data = model_data.at(model_name).methods.at(expr.method_name);

            // Create a new environment for the method call that inherits from the method's definition environment.
            auto new_env = mem::make_rc<Environment>(method_data.definition_environment);
            new_env->define("this", object);  // Define 'this'

            // Define parameters
            for (size_t i = 0; i < method_data.declaration->parameters.size(); ++i)
                new_env->define(method_data.declaration->parameters[i].name, arguments[i]);

            auto result = __Try(execute_block(std::get<ast::Block_stmt>(method_data.declaration->body->node).statements, new_env));
            if (result.is_return)
                return result.value;

            return Value(std::monostate{});  // Return void if no explicit return
        }
    }

    // 5. If no method was found, it's a runtime error.
    return std::unexpected(err::msg("No method named '" + expr.method_name + "' found for this type.", "interpreter",
                                    expr.loc.line, expr.loc.column));
}

Result<Value> Interpreter::evaluate_visitor(const ast::Model_literal_expr& expr)
{

    if (!model_data.contains(expr.model_name))
        return std::unexpected( err::msg("Model type '" + expr.model_name + "' is not defined.", "interpreter", expr.loc.line, expr.loc.column));

    auto model_type = model_data.at(expr.model_name).signature;
    auto instance = mem::make_rc<Model_value>(*model_type, std::unordered_map<std::string, Value>{});

    for (const auto &[name, expr] : expr.fields)
    {
        auto val_res = __Try(evaluate(*expr));
        instance->fields[name] = val_res;
    }
    return Value(instance);
}

Result<Value> Interpreter::evaluate(const ast::Expr &expr)
{
    return std::visit([this](auto &&arg) -> Result<Value> {
        return evaluate_visitor(arg);
    }, expr.node);
}
// ========================================================================
// Statement Execution
// ========================================================================

Result<Return_value> Interpreter::execute_visitor(const ast::Return_stmt &stmt)
{
    Value value;
    if (stmt.expression != nullptr)
    {
        auto value_result = __Try(evaluate(*stmt.expression));
        value = value_result;
    }
    return Return_value(value);
}

Result<Return_value> Interpreter::execute_visitor(const ast::Expr_stmt &stmt)
{
    auto eval_result = __Try(evaluate(*stmt.expression));
    return Return_value{};
}

Result<Return_value> Interpreter::execute_visitor(const ast::Var_stmt &stmt)
{
    Value value(std::monostate{});
    if (stmt.initializer != nullptr)
    {
        auto init_result = __Try(evaluate(*stmt.initializer));
        value = init_result;
    }
    __TryVoid(environment->define(stmt.name, value));
    return Return_value{};

}

Result<Return_value> Interpreter::execute_visitor(const ast::Function_stmt &stmt)
{
    types::Function_type func_type;
    for (const auto &param : stmt.parameters) func_type.parameter_types.push_back(param.type);
    func_type.return_type = stmt.return_type;

    functions[stmt.name] = {func_type, mem::make_rc<ast::Function_stmt>(std::move(stmt)) , environment};

    // PERF: This is slow, maybe straight away move it?
    std::vector<std::pair<std::string, types::Type>> param_data;
    for (const auto &p : stmt.parameters) param_data.push_back({p.name, p.type});

    // Now, create the Function_value with the clean data.
    auto func_val = mem::make_rc<Function_value>(func_type, param_data, environment);
    func_val->name = stmt.name;

    __TryVoid(environment->define(stmt.name, Value(func_val)));

    return Return_value{};
}

Result<Return_value> Interpreter::execute_visitor(const ast::Model_stmt &stmt)
{
    auto model_type = mem::make_rc<types::Model_type>();
    model_type->name = stmt.name;

    for (const auto &[name, type] : stmt.fields) model_type->fields[name] = type;

    ModelData data;
    data.signature = model_type;

    for (const auto &method_ast : stmt.methods)
    {
        types::Function_type method_type;
        for (const auto &param : method_ast->parameters) method_type.parameter_types.push_back(param.type);
        method_type.return_type = method_ast->return_type;

        data.methods[method_ast->name] = {method_type, mem::make_rc<ast::Function_stmt>(*method_ast), environment};
        model_type->methods[method_ast->name] = method_type;
    }

    model_data[stmt.name] = std::move(data);
    return Return_value{};

}

Result<Return_value> Interpreter::execute_visitor(const ast::Block_stmt &stmt)
{
    return execute_block(stmt.statements, mem::make_rc<Environment>(environment));
}

Result<Value> Interpreter::evaluate_visitor(const ast::Assignment_expr& expr)
{
    auto value_result = __Try(evaluate(*expr.value));
    __TryVoid(environment->assign(expr.name, value_result));

    return value_result;
}

Result<Return_value> Interpreter::execute_visitor(const ast::Print_stmt &stmt)
{
    auto value_result = __Try(evaluate(*stmt.expression));

    auto value = value_result;
    std::string str = "";
    if (is_string(value))
        str = unescape_string(get_string(value));
    else
        str = value_to_string(value);
    if (stmt.stream == ast::Print_stream::STDOUT)
        std::println("{}", str);
    else
        std::println(stderr, "{}", str);

    return Return_value{};
}

Result<Return_value> Interpreter::execute_visitor(const ast::If_stmt &stmt)
{
    auto condition_result = __Try(evaluate(*stmt.condition));

    if (is_truthy(condition_result))
        return execute(*stmt.then_branch);
    else if (stmt.else_branch != nullptr)
        return execute(*stmt.else_branch);

    return Return_value{};
}

Result<Return_value> Interpreter::execute_visitor(const ast::While_stmt &stmt)
{
    while (true)
    {
        auto condition_result = __Try(evaluate(*stmt.condition));
        if (!is_truthy(condition_result)) break;

        auto body_result = __Try(execute(*stmt.body));
        if (body_result.is_return) return body_result;
    }
    return Return_value{};

}

Result<Return_value> Interpreter::execute_visitor(const ast::For_stmt &stmt)
{
    if (stmt.initializer != nullptr)
        auto init_result = __Try(execute(*stmt.initializer));

    while (true)
    {
        if (stmt.condition != nullptr)
        {
            auto cond_result = __Try(evaluate(*stmt.condition));
            if (!is_truthy(cond_result)) break;
        }

        auto body_result = __Try(execute(*stmt.body));
        if (body_result.is_return) return body_result;

        if (stmt.increment != nullptr)
            auto inc_result = __Try(evaluate(*stmt.increment));
    }
    return Return_value{};
}

Result<Return_value> Interpreter::execute(const ast::Stmt &stmt)
{
    return std::visit([this](auto &&arg) -> Result<Return_value> {
        return execute_visitor(arg);
    }, stmt.node);
}

std::string Interpreter::unescape_string(const std::string &s)
{
    std::string res;
    res.reserve(s.length());
    for (std::string::size_type i = 0; i < s.length(); ++i)
    {
        if (s[i] == '\\' && i + 1 < s.length())
        {
            switch (s[++i])
            {
                case 'n': res += '\n'; break;
                case 't': res += '\t'; break;
                case 'r': res += '\r'; break;
                case '\\': res += '\\'; break;
                case '"': res += '"'; break;
                default: res += '\\'; res += s[i]; break;  // Keep unrecognized sequences
            }
        }
        else
        {
            res += s[i];
        }
    }
    return res;
}

Result<Return_value> Interpreter::execute_block(const std::vector<ast::Stmt*> &statements, mem::rc_ptr<Environment> block_env)
{
    auto previous = this->environment;
    this->environment = block_env;
    auto guard = finally([this, previous]() { this->environment = previous; });

    for (const auto &statement : statements)
    {
        auto result = execute(*statement);
        if (!result || result.value().is_return)
            return result;
    }
    return Return_value{};
}

// ========================================================================
// Function & Closure Calling
// ========================================================================

Result<Value> Interpreter::call(const Value &callee, const std::vector<Value> &arguments, const ast::Source_location &loc)
{
    if (is_function(callee))
    {
        // This relies on the function's name being stored in the Function_value.
        // You'll need to add `std::string name;` to your `Function_value` struct
        // and set it when you create it in `execute_visitor(const ast::Function_stmt&)`
        // for this lookup to work.
        auto func = get_function(callee);
        if (functions.count(func->name))
            return call_function(func->name, arguments, loc);
        return std::unexpected(err::msg("Callable is not a known function.", "interpreter", loc.line, loc.column));
    }
    else if (is_closure(callee))
    {
        return call_closure(get_closure(callee)->id, arguments, loc);
    }
    else if (is_native_function(callee))
    {
        auto native_fn = get_native_function(callee);
        if (native_fn->arity != -1 && arguments.size() != static_cast<size_t>(native_fn->arity))
        {
            return std::unexpected(
            err::msg(std::format("'{}' expected {} arguments but got {}", native_fn->name, native_fn->arity, arguments.size()),
                     "interpreter",
                     loc.line,
                     loc.column));
        }
        return native_fn->code(arguments);
    }

    return std::unexpected(err::msg("Expression is not a function and cannot be called.", "interpreter", loc.line, loc.column));
}
Result<Value> Interpreter::call_function(const std::string &name, const std::vector<Value> &arguments, const ast::Source_location &loc)
{
    if (!functions.contains(name))
        return std::unexpected(err::msg("Undefined function '" + name + "'.", "interpreter", loc.line, loc.column));

    const auto &func_data = functions.at(name);
    auto new_env = mem::make_rc<Environment>(func_data.definition_environment);

    for (size_t i = 0; i < func_data.declaration->parameters.size(); ++i)
        auto _ = new_env->define(func_data.declaration->parameters[i].name, arguments[i]);

    auto result = __Try(execute_block(std::get<ast::Block_stmt>(func_data.declaration->body->node).statements, new_env));
    if (result.is_return) return result.value;

    return Value(std::monostate{});
}

Result<Value> Interpreter::call_closure(size_t id, const std::vector<Value> &arguments, const ast::Source_location &loc)
{
    if (!closures.contains(id))
        return std::unexpected(err::msg("Invalid or stale closure.", "interpreter", loc.line, loc.column));

    const auto &closure_data = closures.at(id);
    auto new_env = mem::make_rc<Environment>(closure_data.captured_environment);

    for (size_t i = 0; i < closure_data.declaration->parameters.size(); ++i)
        std::ignore = new_env->define(closure_data.declaration->parameters[i].name, arguments[i]);

    auto result = __Try(execute_block(std::get<ast::Block_stmt>(closure_data.declaration->body->node).statements, new_env));
    if (result.is_return) return result.value;

    return Value(std::monostate{});
}

Result<Value> Interpreter::add(const Value &l, const Value &r)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) + get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) + get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) + get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) + static_cast<double>(get_int(r)));
    if (is_string(l) && is_string(r))
        return Value(get_string(l) + get_string(r));
    return std::unexpected(err::msg(
        std::format("Operands must be two numbers or two strings for '+': left: {}, right: {}",
            get_value_type_string(l),
            get_value_type_string(r)),
        "interpreter", 0, 0)
    );
}

Result<Value> Interpreter::subtract(const Value &l, const Value &r)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) - get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) - get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) - get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) - static_cast<double>(get_int(r)));
    return std::unexpected(err::msg(std::format("Operands must be numbers for '-': left: {}, right: {}",
        get_value_type_string(l),
        get_value_type_string(r)),
    "interpreter", 0, 0));
}

Result<Value> Interpreter::multiply(const Value &l, const Value &r)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) * get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) * get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) * get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) * static_cast<double>(get_int(r)));
    return std::unexpected(err::msg(std::format("Operands must be numbers for '*': left: {}, right: {}",
        get_value_type_string(l),
        get_value_type_string(r)),
    "interpreter", 0, 0));
}

Result<Value> Interpreter::divide(const Value &l, const Value &r)
{
    if ((is_int(r) && get_int(r) == 0) || (is_float(r) && get_float(r) == 0.0))
        return std::unexpected(err::msg("Division by zero.", "interpreter", 0, 0));

    if (is_int(l) && is_int(r))
        return Value(static_cast<double>(get_int(l)) / static_cast<double>(get_int(r)));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) / get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) / get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) / static_cast<double>(get_int(r)));
    return std::unexpected(err::msg(std::format("Operands must be numbers for '/': left: {}, right: {}",
        get_value_type_string(l),
        get_value_type_string(r)),
    "interpreter", 0, 0));
}

Result<Value> Interpreter::modulo(const Value &l, const Value &r)
{
    if (!is_int(l) || !is_int(r))
        return std::unexpected(err::msg(std::format("Operands must be intergers for '%': left: {}, right: {}",
            get_value_type_string(l),
            get_value_type_string(r)),
        "interpreter", 0, 0));
    if (get_int(r) == 0)
        return std::unexpected(err::msg("Modulo by zero.", "interpreter", 0, 0));
    return Value(get_int(l) % get_int(r));
}

Result<Value> Interpreter::negate(const Value &v)
{
    if (is_int(v))
        return Value(-get_int(v));
    if (is_float(v))
        return Value(-get_float(v));
    return std::unexpected(err::msg(std::format("Operand must be a number for '-': value: {}", get_value_type_string(v)), "interpreter", 0, 0));
}

// ========================================================================
// Comparison Operations
// ========================================================================

Result<Value> Interpreter::lessThan(const Value &l, const Value &r)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) < get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) < get_float(r));
    return std::unexpected(err::msg(std::format("Operands must be numbers of same type for '<'. Even the comparison between f64 and i64 is not allowed: left: {}, right: {}",
        get_value_type_string(l),
        get_value_type_string(r)),
    "interpreter", 0, 0));
}

Result<Value> Interpreter::less_than_or_equal(const Value &l, const Value &r)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) <= get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) <= get_float(r));

    return std::unexpected(err::msg(std::format("Operands must be numbers of same type for '<='. Even the comparison between f64 and i64 is not allowed: left: {}, right: {}",
        get_value_type_string(l),
        get_value_type_string(r)),
    "interpreter", 0, 0));
}

Result<Value> Interpreter::greater_than(const Value &l, const Value &r)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) > get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) > get_float(r));
    return std::unexpected(err::msg(std::format("Operands must be numbers of same type for '>'. Even the comparison between f64 and i64 is not allowed: left: {}, right: {}",
        get_value_type_string(l),
        get_value_type_string(r)),
    "interpreter", 0, 0));
}

Result<Value> Interpreter::greater_than_or_equal(const Value &l, const Value &r)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) >= get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) >= get_float(r));

    return std::unexpected(err::msg(std::format("Operands must be numbers of same type for '>='. Even the comparison between f64 and i64 is not allowed: left: {}, right: {}",
        get_value_type_string(l),
        get_value_type_string(r)),
    "interpreter", 0, 0));
}

// ========================================================================
// Utility Methods
// ========================================================================

bool Interpreter::equals(const Value &l, const Value &r) { return l == r; }

bool Interpreter::is_truthy(const Value &v)
{
    if (is_bool(v))
        return get_bool(v);
    if (is_void(v) || is_nil(v))
        return false;
    if (is_int(v))
        return get_int(v) != 0;
    if (is_float(v))
        return get_float(v) != 0.0;
    if (is_string(v))
        return get_string(v).size() > 0;
    return true;  // Models, functions, closures, and non-empty strings are truthy
}

Result<Value> Interpreter::cast_value(const Value &v, const types::Type &t)
{
    if (types::is_primitive(t) && types::get_primitive_kind(t) == types::Primitive_kind::String)
        return Value(value_to_string(v));
    return v;
}

Result<Value> Interpreter::map_array(Value &array_val, const Value &closure)
{
    auto arr = get_array(array_val);
    auto result_arr = mem::make_rc<Array_value>();

    // Get the closure ID just once.
    size_t closure_id = get_closure(closure)->id;

    for (const auto &element : arr->elements)
    {
        auto mapped_res = __Try(this->call_closure(closure_id, {element}, {}));
        result_arr->elements.push_back(mapped_res);
    }
    return Value(result_arr);
}

Result<Value> Interpreter::map_optional(Value &optional_val, const Value &closure)
{
    if (is_nil(optional_val))
        return Value(nullptr);

    // Get the closure ID.
    size_t closure_id = get_closure(closure)->id;

    auto mapped_res = __Try(this->call_closure(closure_id, {optional_val}, {}));

    return mapped_res;
}
} // namespace phos
