#pragma once

#include <cstddef>
#include <unordered_map>
#include <memory>
#include <variant>
#include <cstdint>
#include <string>
#include <functional>
#include <vector>

#include "type.hpp"
#include "../error/result.hpp"

namespace phos
{
class Environment;

using Value = std::variant<
    int64_t,
    double,
    bool,
    std::string,
    phos::mem::rc_ptr<struct Model_value>,
    phos::mem::rc_ptr<struct Function_value>,
    phos::mem::rc_ptr<struct Closure_value>,
    phos::mem::rc_ptr<struct Array_value>,
    phos::mem::rc_ptr<struct Native_function_value>,
    std::nullptr_t,  // nil
    std::monostate   // void
>;

bool operator==(const Value &lhs, const Value &rhs);

struct Function_value
{
    types::Function_type signature;
    std::vector<std::pair<std::string, types::Type>> parameters;
    mem::rc_ptr<Environment> definition_environment;
    bool is_native = false;

    Function_value(types::Function_type sig, std::vector<std::pair<std::string, types::Type>> params, mem::rc_ptr<Environment> env)
        : signature(std::move(sig)), parameters(std::move(params)), definition_environment(std::move(env))
    {
    }
};

struct Closure_value
{
    mem::rc_ptr<Function_value> function;
    std::unordered_map<std::string, Value> captured_variables;
    std::size_t id;
    Closure_value(mem::rc_ptr<Function_value> func, std::unordered_map<std::string, Value> captures)
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

using Native_callable = std::function<Result<Value>(const std::vector<Value> &arguments)>;

struct Native_function_value
{
    std::string name;
    int arity;
    types::Function_type signature;
    std::vector<std::pair<std::string, types::Type>> parameters;
    Native_callable code;
};

// Helper functions for value manipulation
inline bool is_int(const Value &value) { return std::holds_alternative<int64_t>(value); }
inline bool is_float(const Value &value) { return std::holds_alternative<double>(value); }
inline bool is_bool(const Value &value) { return std::holds_alternative<bool>(value); }
inline bool is_string(const Value &value) { return std::holds_alternative<std::string>(value); }
inline bool is_model(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Model_value>>(value); }
inline bool is_function(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Function_value>>(value); }
inline bool is_closure(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Closure_value>>(value); }
inline bool is_array(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Array_value>>(value); }
inline bool is_nil(const Value &value) { return std::holds_alternative<std::nullptr_t>(value); }
inline bool is_void(const Value &value) { return std::holds_alternative<std::monostate>(value); }
inline bool is_native_function(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Native_function_value>>(value); }

inline bool is_callable(const Value &value) { return is_function(value) || is_closure(value) || is_native_function(value); }

inline int64_t get_int(const Value &value) { return std::get<int64_t>(value); }
inline double get_float(const Value &value) { return std::get<double>(value); }
inline bool get_bool(const Value &value) { return std::get<bool>(value); }
inline std::string get_string(const Value &value) { return std::get<std::string>(value); }

inline phos::mem::rc_ptr<Model_value> get_model(const Value &value) { return std::get<phos::mem::rc_ptr<Model_value>>(value); }
inline phos::mem::rc_ptr<Function_value> get_function(const Value &value) { return std::get<phos::mem::rc_ptr<Function_value>>(value); }
inline phos::mem::rc_ptr<Closure_value> get_closure(const Value &value) { return std::get<phos::mem::rc_ptr<Closure_value>>(value); }
inline phos::mem::rc_ptr<Array_value> get_array(const Value &value) { return std::get<phos::mem::rc_ptr<Array_value>>(value); }

inline phos::mem::rc_ptr<Native_function_value> get_native_function(const Value &value)
{
    return std::get<phos::mem::rc_ptr<Native_function_value>>(value);
}

std::string value_to_string(const Value &value);
std::string get_value_type_string(const Value &value);

}  // namespace phos
