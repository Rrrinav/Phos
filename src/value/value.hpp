#pragma once

#include <cstddef>
#include <unordered_map>
#include <memory>
#include <variant>
#include <cstdint>
#include <string>
#include <functional>

#include "type.hpp"

namespace phos
{

// Forward declarations
struct Environment;
struct FunctionValue;
struct ClosureValue;
struct ModelValue;

using Value = std::variant<int64_t, double, bool, std::string, std::shared_ptr<ModelValue>, std::shared_ptr<FunctionValue>,
                           std::shared_ptr<ClosureValue>, std::nullptr_t, std::monostate>;

struct FunctionValue
{
    types::Function_type signature;
    std::vector<std::pair<std::string, types::Type>> parameters;
    std::shared_ptr<Environment> definition_environment;
    bool is_native = false;

    FunctionValue(types::Function_type sig, std::vector<std::pair<std::string, types::Type>> params, std::shared_ptr<Environment> env)
        : signature(std::move(sig)), parameters(std::move(params)), definition_environment(std::move(env))
    {
    }
};

struct ClosureValue
{
    std::shared_ptr<FunctionValue> function;
    std::unordered_map<std::string, Value> captured_variables;
    std::size_t id;
    ClosureValue(std::shared_ptr<FunctionValue> func, std::unordered_map<std::string, Value> captures)
        : function(std::move(func)), captured_variables(std::move(captures))
    {
    }
};

struct ModelValue
{
    types::Model_type signature;
    std::unordered_map<std::string, Value> fields;

    ModelValue(types::Model_type sig, std::unordered_map<std::string, Value> mems) : signature(std::move(sig)), fields(std::move(mems)) {}
};

// Helper functions for value manipulation
inline bool is_int(const Value &value) { return std::holds_alternative<int64_t>(value); }
inline bool is_float(const Value &value) { return std::holds_alternative<double>(value); }
inline bool is_bool(const Value &value) { return std::holds_alternative<bool>(value); }
inline bool is_string(const Value &value) { return std::holds_alternative<std::string>(value); }
inline bool is_model(const Value &value) { return std::holds_alternative<std::shared_ptr<ModelValue>>(value); }
inline bool is_function(const Value &value) { return std::holds_alternative<std::shared_ptr<FunctionValue>>(value); }
inline bool is_closure(const Value &value) { return std::holds_alternative<std::shared_ptr<ClosureValue>>(value); }
inline bool is_null(const Value &value) { return std::holds_alternative<std::nullptr_t>(value); }
inline bool is_void(const Value &value) { return std::holds_alternative<std::monostate>(value); }
inline bool is_callable(const Value &value)
{
    return std::holds_alternative<std::shared_ptr<FunctionValue>>(value) || std::holds_alternative<std::shared_ptr<ClosureValue>>(value);
}

inline int64_t get_int(const Value &value) { return std::get<int64_t>(value); }
inline double get_float(const Value &value) { return std::get<double>(value); }
inline bool get_bool(const Value &value) { return std::get<bool>(value); }
inline std::string get_string(const Value &value) { return std::get<std::string>(value); }
inline std::shared_ptr<ModelValue> get_model(const Value &value) { return std::get<std::shared_ptr<ModelValue>>(value); }
inline std::shared_ptr<FunctionValue> get_function(const Value &value) { return std::get<std::shared_ptr<FunctionValue>>(value); }
inline std::shared_ptr<ClosureValue> get_closure(const Value &value) { return std::get<std::shared_ptr<ClosureValue>>(value); }

// Value to string conversion
std::string value_to_string(const Value &value)
{
    if (is_int(value))
        return std::to_string(get_int(value));
    if (is_float(value))
        return std::to_string(get_float(value));
    if (is_bool(value))
        return get_bool(value) ? "true" : "false";
    if (is_string(value))
        return get_string(value);
    if (is_model(value))
        return "[model instance]";
    if (is_function(value))
        return "[function]";
    if (is_closure(value))
        return "[closure]";
    if (is_null(value))
        return "null";
    if (is_void(value))
        return "void";
    return "unknown";
}

}  // namespace phos
