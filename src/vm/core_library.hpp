#pragma once

#include <chrono>
#include <cmath>

#include "virtual_machine.hpp"
#include "../type-checker/type-checker.hpp"

namespace phos::vm::core
{

// --- Identity / Reference Equality ---
inline bool core_is_same(Value a, Value b)
{
    if (a.index() != b.index())
        return false;

    if (is_model(a))
        return get_model(a).get() == get_model(b).get();
    if (is_array(a))
        return get_array(a).get() == get_array(b).get();
    if (is_closure(a))
        return get_closure(a).get() == get_closure(b).get();

    if (is_int(a))
        return get_int(a) == get_int(b);
    if (is_float(a))
        return get_float(a) == get_float(b);
    if (is_bool(a))
        return get_bool(a) == get_bool(b);
    if (is_string(a))
        return get_string(a) == get_string(b);
    if (is_nil(a))
        return true;

    return false;
}

inline double clock_now()
{
    using namespace std::chrono;
    return duration_cast<duration<double>>(system_clock::now().time_since_epoch()).count();
}

// --- Queries (Functions) ---
inline int64_t core_len(Value val)
{
    if (is_array(val))
        return static_cast<int64_t>(get_array(val)->elements.size());
    if (is_string(val))
        return static_cast<int64_t>(get_string(val).length());
    return 0;  // Fallback for unsupported types
}

// --- Mutations (Methods) ---
inline Value array_push(mem::rc_ptr<Array_value> arr, Value val)
{
    arr->elements.push_back(val);
    return val;
}

inline Value array_pop(mem::rc_ptr<Array_value> arr)
{
    if (arr->elements.empty())
        return nullptr;
    Value last = std::move(arr->elements.back());
    arr->elements.pop_back();
    return last;
}

inline bool string_starts_with(std::string str, std::string prefix)
{
    return str.starts_with(prefix);
}

inline bool string_ends_with(std::string str, std::string suffix) { return str.ends_with(suffix); }

inline int64_t string_index_of(std::string str, std::string search)
{
    auto pos = str.find(search);
    return pos == std::string::npos ? -1 : static_cast<int64_t>(pos);
}

inline std::string string_repeat(std::string str, int64_t count)
{
    if (count <= 0)
        return "";
    std::string result;
    result.reserve(str.length() * count);
    for (int64_t i = 0; i < count; ++i) result += str;
    return result;
}

// Safely parse a string into an integer. Returns 'nil' if it fails!
inline Value string_parse_int(std::string str)
{
    try
    {
        return Value(static_cast<int64_t>(std::stoll(str)));
    }
    catch (...)
    {
        return Value(nullptr);
    }
}

// Safely parse a string into a float. Returns 'nil' if it fails!
inline Value string_parse_float(std::string str)
{
    try
    {
        return Value(std::stod(str));
    }
    catch (...)
    {
        return Value(nullptr);
    }
}

// Let's add a join method for arrays too!
inline std::string array_join(mem::rc_ptr<Array_value> arr, std::string delimiter)
{
    std::string result;
    for (size_t i = 0; i < arr->elements.size(); ++i)
    {
        result += value_to_string(arr->elements[i]);
        if (i < arr->elements.size() - 1)
            result += delimiter;
    }
    return result;
}

inline Value string_split(std::string str, std::string delimiter)
{
    std::vector<Value> elements;

    if (delimiter.empty())
    {
        // Split into individual characters if delimiter is empty
        for (char c : str) elements.push_back(Value(std::string(1, c)));
    }
    else
    {
        size_t pos = 0;
        while ((pos = str.find(delimiter)) != std::string::npos)
        {
            elements.push_back(Value(str.substr(0, pos)));
            str.erase(0, pos + delimiter.length());
        }
        // Push the final chunk
        elements.push_back(Value(str));
    }

    // Construct the lightweight Phos Array
    auto arr_type = types::Type(mem::make_rc<types::Array_type>(types::Primitive_kind::String));
    return Value(mem::make_rc<Array_value>(arr_type, std::move(elements)));
}

inline std::string string_trim(std::string str)
{
    size_t start = 0;
    while (start < str.size() && std::isspace(static_cast<unsigned char>(str[start]))) ++start;

    size_t end = str.size();
    while (end > start && std::isspace(static_cast<unsigned char>(str[end - 1]))) --end;

    return str.substr(start, end - start);
}

inline std::string string_trim_left(std::string str)
{
    size_t start = 0;
    while (start < str.size() && std::isspace(static_cast<unsigned char>(str[start]))) ++start;

    return str.substr(start);
}

inline std::string string_trim_right(std::string str)
{
    size_t end = str.size();
    while (end > 0 && std::isspace(static_cast<unsigned char>(str[end - 1]))) --end;

    return str.substr(0, end);
}

inline double math_sqrt(double x) { return std::sqrt(x); }
inline double math_pow(double base, double exp) { return std::pow(base, exp); }
inline double math_abs(double x) { return std::abs(x); }

inline Value core_to_i64(std::string str)
{
    int64_t result = 0;
    auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), result);

    if (ec == std::errc())
        return Value(result);

    return Value(nullptr);  // Return Phos 'nil' if parsing fails!
}

inline Value core_to_f64(std::string str)
{
    double result = 0.0;
    auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), result);

    if (ec == std::errc())
        return Value(result);

    return Value(nullptr);
}

inline std::string core_to_str(Value val)
{
    return value_to_string(val);
}

inline void register_core_library(Virtual_machine &vm, Type_checker &tc)
{
    vm.bind_native<core_is_same>("is_same", {"T", "T"}, "bool", tc);
    // Clock
    vm.bind_native<clock_now>("clock", {}, "f64", tc);

    // len() strictly accepts either ONE string OR ONE array
    // (We use two bind_native calls to register two valid signatures for the same function!)
    vm.bind_native<core_len>("len", {"string"}, "i64", tc);
    vm.bind_native<core_len>("len", {"any[]"}, "i64", tc);

    // Array.push() takes an array of T, and a value of T. Returns T.
    vm.bind_native<array_push>("Array::push", {"T[]", "T"}, "T", tc);
    // Array.pop() takes an array of T, and returns an optional T?
    vm.bind_native<array_pop>("Array::pop", {"T[]"}, "T?", tc);
    vm.bind_native<array_join>("Array::join", {"T[]", "string"}, "string", tc);

    // Math
    vm.bind_native<math_sqrt>("sqrt", {"f64"}, "f64", tc);
    vm.bind_native<math_pow>("pow", {"f64", "f64"}, "f64", tc);
    vm.bind_native<math_abs>("abs", {"f64"}, "f64", tc);
    // Advanced String Bindings
    vm.bind_native<string_starts_with>("string::starts_with", {"string", "string"}, "bool", tc);
    vm.bind_native<string_ends_with>("string::ends_with", {"string", "string"}, "bool", tc);
    vm.bind_native<string_index_of>("string::index_of", {"string", "string"}, "i64", tc);
    vm.bind_native<string_repeat>("string::repeat", {"string", "i64"}, "string", tc);
    vm.bind_native<string_split>("string::split", {"string", "string"}, "string[]", tc);
    vm.bind_native<string_trim>("string::trim", {"string"}, "string", tc);
    vm.bind_native<string_trim_left>("string::trim_left", {"string"}, "string", tc);
    vm.bind_native<string_trim_right>("string::trim_right", {"string"}, "string", tc);

    // Notice the "?" in the return type! The compiler knows these can fail and return nil.
    vm.bind_native<string_parse_int>("string::parse_int", {"string"}, "i64?", tc);
    vm.bind_native<string_parse_float>("string::parse_float", {"string"}, "f64?", tc);
}

}  // namespace phos::vm::core
