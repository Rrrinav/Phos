#pragma once

#include <cstddef>
#include <unordered_map>
#include <variant>
#include <cstdint>
#include <string>
#include <functional>
#include <vector>
#include <optional>

#include "type.hpp"
#include "../error/result.hpp"
#include "../memory/ref_counted.hpp"

// Forward declare the VM Chunk so functions can own their bytecode
namespace phos::vm
{
struct Chunk;
}

namespace phos
{

struct Call_frame;  // Forward declare for Green_thread

using Value = std::variant<
    int64_t,
    double,
    bool,
    std::string,
    phos::mem::rc_ptr<struct Model_value>,
    phos::mem::rc_ptr<struct Closure_value>,
    phos::mem::rc_ptr<struct Array_value>,
    phos::mem::rc_ptr<struct Native_function_value>,
    phos::mem::rc_ptr<struct Union_value>,
    phos::mem::rc_ptr<struct Green_thread_value>,
    std::nullptr_t,
    std::monostate>;

bool operator==(const Value &lhs, const Value &rhs);

// At runtime, all Phos functions are closures (a pure function is just a closure with 0 captures)
struct Closure_value
{
    std::string name;
    int arity = 0;
    types::Function_type signature;

    // The bytecode chunk owned by this specific function
    mem::rc_ptr<vm::Chunk> chunk;

    // Captured values from the outer lexical scopes
    std::unordered_map<std::string, Value> captured_variables;

    Closure_value(std::string n, int a, types::Function_type sig, mem::rc_ptr<vm::Chunk> ch)
        : name(std::move(n)), arity(a), signature(std::move(sig)), chunk(std::move(ch))
    {
    }
};

// --- GREEN THREAD ARCHITECTURE ---
struct Call_frame
{
    mem::rc_ptr<Closure_value> closure;
    uint8_t *ip = nullptr;
    size_t stack_offset = 0;  // The base index of this frame's local variables on the thread stack
};

struct Green_thread_value
{
    std::vector<Value> stack;
    std::vector<Call_frame> frames;
    bool is_completed = false;
    std::optional<Value> yield_value;
};
// ---------------------------------

struct Model_value
{
    types::Model_type signature;
    std::vector<Value> fields;

    Model_value(types::Model_type sig, std::vector<Value> mems) : signature(std::move(sig)), fields(std::move(mems)) {}
};

struct Array_value
{
    types::Type type;
    std::vector<Value> elements;
};

struct Union_value
{
    mem::rc_ptr<types::Union_type> signature;
    std::string tag;
    std::optional<Value> value;

    Union_value(mem::rc_ptr<types::Union_type> sig, std::string t, std::optional<Value> val)
        : signature(std::move(sig)), tag(std::move(t)), value(std::move(val))
    {
    }
};

using Native_callable = std::function<Result<Value>(const std::vector<Value> &arguments)>;

struct Native_function_value
{
    std::string name;
    int arity;
    types::Function_type signature;
    Native_callable code;
};

// --- Fast Inline Helpers ---
inline bool is_int(const Value &value) { return std::holds_alternative<int64_t>(value); }
inline bool is_float(const Value &value) { return std::holds_alternative<double>(value); }
inline bool is_bool(const Value &value) { return std::holds_alternative<bool>(value); }
inline bool is_string(const Value &value) { return std::holds_alternative<std::string>(value); }
inline bool is_model(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Model_value>>(value); }
inline bool is_closure(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Closure_value>>(value); }
inline bool is_array(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Array_value>>(value); }
inline bool is_union(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Union_value>>(value); }
inline bool is_thread(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Green_thread_value>>(value); }
inline bool is_nil(const Value &value) { return std::holds_alternative<std::nullptr_t>(value); }
inline bool is_void(const Value &value) { return std::holds_alternative<std::monostate>(value); }
inline bool is_native_function(const Value &value) { return std::holds_alternative<phos::mem::rc_ptr<Native_function_value>>(value); }

inline bool is_callable(const Value &value) { return is_closure(value) || is_native_function(value); }

inline int64_t get_int(const Value &value) { return std::get<int64_t>(value); }
inline double get_float(const Value &value) { return std::get<double>(value); }
inline bool get_bool(const Value &value) { return std::get<bool>(value); }
inline std::string get_string(const Value &value) { return std::get<std::string>(value); }

inline phos::mem::rc_ptr<Model_value> get_model(const Value &value) { return std::get<phos::mem::rc_ptr<Model_value>>(value); }
inline phos::mem::rc_ptr<Closure_value> get_closure(const Value &value) { return std::get<phos::mem::rc_ptr<Closure_value>>(value); }
inline phos::mem::rc_ptr<Array_value> get_array(const Value &value) { return std::get<phos::mem::rc_ptr<Array_value>>(value); }
inline phos::mem::rc_ptr<Union_value> get_union(const Value &value) { return std::get<phos::mem::rc_ptr<Union_value>>(value); }
inline phos::mem::rc_ptr<Green_thread_value> get_thread(const Value &value)
{
    return std::get<phos::mem::rc_ptr<Green_thread_value>>(value);
}
inline phos::mem::rc_ptr<Native_function_value> get_native_function(const Value &value)
{
    return std::get<phos::mem::rc_ptr<Native_function_value>>(value);
}

std::string value_to_string(const Value &value);
std::string get_value_type_string(const Value &value);

}  // namespace phos
