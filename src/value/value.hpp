#pragma once

#include <cstddef>
#include <unordered_map>
#include <memory>
#include <variant>
#include <cstdint>
#include <string>
#include <sstream>
#include <functional>
#include <vector>

#include "type.hpp"
#include "../error/result.hpp"

namespace phos
{

// Forward declarations
struct Environment;
struct Function_value;
struct Closure_value;
struct Model_value;
struct Array_value;
struct Native_function_value;

using Value = std::variant<int64_t, double, bool, std::string, std::shared_ptr<Model_value>, std::shared_ptr<Function_value>,
                           std::shared_ptr<Closure_value>, std::shared_ptr<Array_value>, std::nullptr_t, std::monostate,
                           std::shared_ptr<Native_function_value>>;

struct Function_value
{
    types::Function_type signature;
    std::vector<std::pair<std::string, types::Type>> parameters;
    std::shared_ptr<Environment> definition_environment;
    bool is_native = false;

    Function_value(types::Function_type sig, std::vector<std::pair<std::string, types::Type>> params, std::shared_ptr<Environment> env)
        : signature(std::move(sig)), parameters(std::move(params)), definition_environment(std::move(env))
    {
    }
};

struct Closure_value
{
    std::shared_ptr<Function_value> function;
    std::unordered_map<std::string, Value> captured_variables;
    std::size_t id;
    Closure_value(std::shared_ptr<Function_value> func, std::unordered_map<std::string, Value> captures)
        : function(std::move(func)), captured_variables(std::move(captures))
    {
    }
};

struct Model_value
{
    types::Model_type signature;
    std::unordered_map<std::string, Value> fields;

    Model_value(types::Model_type sig, std::unordered_map<std::string, Value> mems) : signature(std::move(sig)), fields(std::move(mems)) {}
};

struct Array_value
{
    types::Type type;
    std::vector<Value> elements;
};

struct Native_function_value
{
    std::string name;
    int arity;
    types::Function_type signature;
    std::vector<std::pair<std::string, types::Type>> parameters;
    std::function<Result<Value>(const std::vector<Value>& arguments)> code;
};

// Helper functions for value manipulation
inline bool is_int(const Value &value) { return std::holds_alternative<int64_t>(value); }
inline bool is_float(const Value &value) { return std::holds_alternative<double>(value); }
inline bool is_bool(const Value &value) { return std::holds_alternative<bool>(value); }
inline bool is_string(const Value &value) { return std::holds_alternative<std::string>(value); }
inline bool is_model(const Value &value) { return std::holds_alternative<std::shared_ptr<Model_value>>(value); }
inline bool is_function(const Value &value) { return std::holds_alternative<std::shared_ptr<Function_value>>(value); }
inline bool is_closure(const Value &value) { return std::holds_alternative<std::shared_ptr<Closure_value>>(value); }
inline bool is_array(const Value &value) { return std::holds_alternative<std::shared_ptr<Array_value>>(value); }
inline bool is_null(const Value &value) { return std::holds_alternative<std::nullptr_t>(value); }
inline bool is_void(const Value &value) { return std::holds_alternative<std::monostate>(value); }
inline bool is_native_function(const Value &value) { return std::holds_alternative<std::shared_ptr<Native_function_value>>(value); }
inline bool is_callable(const Value &value) {
    return is_function(value) || is_closure(value) || is_native_function(value);
}

inline int64_t get_int(const Value &value) { return std::get<int64_t>(value); }
inline double get_float(const Value &value) { return std::get<double>(value); }
inline bool get_bool(const Value &value) { return std::get<bool>(value); }
inline std::string get_string(const Value &value) { return std::get<std::string>(value); }
inline std::shared_ptr<Model_value> get_model(const Value &value) { return std::get<std::shared_ptr<Model_value>>(value); }
inline std::shared_ptr<Function_value> get_function(const Value &value) { return std::get<std::shared_ptr<Function_value>>(value); }
inline std::shared_ptr<Closure_value> get_closure(const Value &value) { return std::get<std::shared_ptr<Closure_value>>(value); }
inline std::shared_ptr<Array_value> get_array(const Value &value) { return std::get<std::shared_ptr<Array_value>>(value); }
inline std::shared_ptr<Native_function_value> get_native_function(const Value &value) { return std::get<std::shared_ptr<Native_function_value>>(value); }

std::string value_to_string_impl(const Value &value, int indent_level);
std::string value_to_string(const Value &value) { return value_to_string_impl(value, 0); }

// The new implementation with indentation logic
std::string value_to_string_impl(const Value &value, int indent_level)
{
    // --- Primitives ---
    if (is_int(value))
        return std::to_string(get_int(value));
    if (is_float(value))
        return std::to_string(get_float(value));
    if (is_bool(value))
        return get_bool(value) ? "true" : "false";
    if (is_string(value))
        return "\"" + get_string(value) + "\"";
    if (is_null(value))
        return "null";
    if (is_void(value))
        return "void";

    // --- Array Printing ---
    if (is_array(value))
    {
        auto arr = get_array(value);
        if (arr->elements.empty())
            return "[]";

        // Heuristic: check if any element is complex or if the array is long
        bool is_complex = arr->elements.size() > 10;
        if (!is_complex)
        {
            for (const auto &el : arr->elements)
            {
                if (is_array(el) || is_model(el))
                {
                    is_complex = true;
                    break;
                }
            }
        }

        std::stringstream ss;
        if (!is_complex)  // Single-line format
        {
            ss << "[";
            bool first = true;
            for (const auto &element : arr->elements)
            {
                if (!first)
                    ss << ", ";
                ss << value_to_string_impl(element, indent_level);
                first = false;
            }
            ss << "]";
        }
        else  // Multi-line format
        {
            std::string base_indent(indent_level * 2, ' ');
            std::string inner_indent((indent_level + 1) * 2, ' ');
            ss << "[\n";
            bool first = true;
            for (const auto &element : arr->elements)
            {
                if (!first)
                    ss << ",\n";
                ss << inner_indent << value_to_string_impl(element, indent_level + 1);
                first = false;
            }
            ss << "\n" << base_indent << "]";
        }
        return ss.str();
    }

    if (is_model(value))
    {
        auto model = get_model(value);
        if (model->fields.empty())
            return model->signature.name + " { }";

        // Heuristic: check if any field is complex or if there are many fields
        bool is_complex = model->fields.size() > 3;
        if (!is_complex)
        {
            for (const auto &[name, field_val] : model->fields)
            {
                if ((is_array(field_val) && get_array(field_val)->elements.size() > 5 )|| is_model(field_val))
                {
                    is_complex = true;
                    break;
                }
            }
        }

        std::stringstream ss;
        if (!is_complex)  // Single-line format
        {
            ss << model->signature.name << " { ";
            bool first = true;
            for (const auto &[name, field_value] : model->fields)
            {
                if (!first)
                    ss << ", ";
                ss << "." << name << " = " << value_to_string_impl(field_value, indent_level);
                first = false;
            }
            ss << " }";
        }
        else  // Multi-line format
        {
            std::string base_indent(indent_level * 2, ' ');
            std::string inner_indent((indent_level + 1) * 2, ' ');
            ss << model->signature.name << " {\n";
            bool first = true;
            for (const auto &[name, field_value] : model->fields)
            {
                if (!first)
                    ss << ",\n";
                ss << inner_indent << "." << name << " = " << value_to_string_impl(field_value, indent_level + 1);
                first = false;
            }
            ss << "\n" << base_indent << "}";
        }
        return ss.str();
    }

    // --- Functions & Closures (signatures are single-line) ---
    if (is_function(value))
    {
        auto func = get_function(value);
        std::stringstream ss;
        ss << "fn(";
        bool first = true;
        for (const auto &param : func->parameters)
        {
            if (!first)
                ss << ", ";
            ss << param.first << ": " << types::type_to_string(param.second);
            first = false;
        }
        ss << ") -> " << types::type_to_string(func->signature.return_type);
        return ss.str();
    }
    if (is_closure(value))
    {
        auto func = get_closure(value)->function;
        std::stringstream ss;
        ss << "closure fn(";
        bool first = true;
        for (const auto &param : func->parameters)
        {
            if (!first)
                ss << ", ";
            ss << param.first << ": " << types::type_to_string(param.second);
            first = false;
        }
        ss << ") -> " << types::type_to_string(func->signature.return_type);
        return ss.str();
    }

    return "unknown";
}

}  // namespace phos
