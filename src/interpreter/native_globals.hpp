#pragma once

#include <array>
#include <chrono>
#include <vector>
#include <algorithm>
#include <cctype>

#include "../value/value.hpp"  // Includes Native_function_value, Result, etc.
#include "native_signatures.hpp"

namespace phos::native
{

// Using these because unrodered map can't index some types because of recursive definition or something
// This also has something to do with hashing of std::variant
inline const std::string array_type_string = "__array__";
inline const std::string string_type_string = "__string__";

struct Native_method_definition
{
    std::string this_type;
    std::string name;
    int arity;
    std::function<Result<Value>(Value &, const std::vector<Value> &)> code;
};

// This constexpr value must be updated if you add or remove a native function.
inline constexpr int NUM_NATIVE_FUNCS = 2;

// Returns a fixed-size array of all native function implementations.
inline std::array<std::shared_ptr<Native_function_value>, NUM_NATIVE_FUNCS> get_natives()
{
    auto signatures = get_native_fn_signatures();
    std::array<std::shared_ptr<Native_function_value>, NUM_NATIVE_FUNCS> functions;

    // === len ===
    {
        std::string name = "len";
        auto signature_data = signatures.at(name);

        auto func = std::make_shared<Native_function_value>();
        func->name = name;
        func->arity = signature_data.allowed_params.size();
        func->code = [](const std::vector<Value> &args) -> Result<Value>
        {
            const auto &subject = args[0];
            if (is_string(subject))
                return Value((int64_t)get_string(subject).length());
            if (is_array(subject))
                return Value((int64_t)get_array(subject)->elements.size());
            return std::unexpected(err::msg("len() expects a string or an array.", "interpreter", 0, 0));
        };

        functions[0] = func;
    }

    // === clock ===
    {
        std::string name = "clock";

        auto func = std::make_shared<Native_function_value>();
        func->name = name;
        func->arity = 0;
        func->code = [](const std::vector<Value> &args) -> Result<Value>
        {
            auto time = std::chrono::system_clock::now().time_since_epoch();
            double millis = std::chrono::duration<double, std::milli>(time).count();
            return Value(millis);
        };

        functions[1] = func;
    }

    return functions;
}

inline constexpr int NUM_NATIVE_METHODS = 13;
// Returns a fixed-size array of all native METHOD implementations.
inline std::array<Native_method_definition, NUM_NATIVE_METHODS> get_native_methods()
{
    std::array<Native_method_definition, NUM_NATIVE_METHODS> methods;
    auto any_array_type = array_type_string;
    auto string_type = string_type_string;
    int i = 0;

    // ===================================
    // ## Array Methods
    // ===================================
    methods[i++] = {any_array_type, "push", 1, [](Value &s, const auto &a) -> Result<Value>
                    {
                        get_array(s)->elements.push_back(a[0]);
                        return Value(std::monostate{});
                    }};
    methods[i++] = {any_array_type, "pop", 0, [](Value &s, const auto &a) -> Result<Value>
                    {
                        auto arr = get_array(s);
                        if (arr->elements.empty())
                            return std::unexpected(err::msg("Cannot pop from an empty array.", "interpreter", 0, 0));
                        Value b = arr->elements.back();
                        arr->elements.pop_back();
                        return b;
                    }};
    methods[i++] = {any_array_type, "insert", 2, [](Value &s, const auto &a) -> Result<Value>
                    {
                        auto arr = get_array(s);
                        if (!is_int(a[0]))
                            return std::unexpected(err::msg("Index must be an integer.", "interpreter", 0, 0));
                        int64_t idx = get_int(a[0]);
                        if (idx < 0 || (size_t)idx > arr->elements.size())
                            return std::unexpected(err::msg("Insert index out of bounds.", "interpreter", 0, 0));
                        arr->elements.insert(arr->elements.begin() + idx, a[1]);
                        return Value(std::monostate{});
                    }};
    methods[i++] = {any_array_type, "remove", 1, [](Value &s, const auto &a) -> Result<Value>
                    {
                        auto arr = get_array(s);
                        if (!is_int(a[0]))
                            return std::unexpected(err::msg("Index must be an integer.", "interpreter", 0, 0));
                        int64_t idx = get_int(a[0]);
                        if (arr->elements.empty() || idx < 0 || (size_t)idx >= arr->elements.size())
                            return std::unexpected(err::msg("Remove index out of bounds.", "interpreter", 0, 0));
                        Value r = arr->elements[idx];
                        arr->elements.erase(arr->elements.begin() + idx);
                        return r;
                    }};
    methods[i++] = {any_array_type, "clear", 0, [](Value &s, const auto &a) -> Result<Value>
                    {
                        get_array(s)->elements.clear();
                        return Value(std::monostate{});
                    }};
    methods[i++] = {any_array_type, "contains", 1, [](Value &s, const auto &a) -> Result<Value>
                    {
                        for (const auto &e : get_array(s)->elements)
                            if (e == a[0])
                                return Value(true);
                        return Value(false);
                    }};

    // ===================================
    // ## String & Array Methods
    // ===================================
    methods[i++] = {any_array_type, "is_empty", 0,
                    [](Value &s, const auto &a) -> Result<Value> { return Value(get_array(s)->elements.empty()); }};
    methods[i++] = {string_type, "is_empty", 0, [](Value &s, const auto &a) -> Result<Value> { return Value(get_string(s).empty()); }};

    // ===================================
    // ## String Methods
    // ===================================
    methods[i++] = {string_type, "to_upper", 0, [](Value &s, const auto &a) -> Result<Value>
                    {
                        std::string str = get_string(s);
                        std::transform(str.begin(), str.end(), str.begin(), ::toupper);
                        return Value(str);
                    }};
    methods[i++] = {string_type, "to_lower", 0, [](Value &s, const auto &a) -> Result<Value>
                    {
                        std::string str = get_string(s);
                        std::transform(str.begin(), str.end(), str.begin(), ::tolower);
                        return Value(str);
                    }};
    methods[i++] = {string_type, "trim", 0, [](Value &s, const auto &a) -> Result<Value>
                    {
                        std::string str = get_string(s);
                        auto start = str.find_first_not_of(" \t\n\r");
                        if (start == std::string::npos)
                            return Value("");
                        auto end = str.find_last_not_of(" \t\n\r");
                        return Value(str.substr(start, (end - start + 1)));
                    }};
    methods[i++] = {string_type, "split", 1, [](Value &s, const auto &a) -> Result<Value>
                    {
                        if (!is_string(a[0]))
                            return std::unexpected(err::msg("Delimiter must be a string.", "interpreter", 0, 0));
                        std::string str = get_string(s);
                        std::string delim = get_string(a[0]);
                        auto res = std::make_shared<Array_value>();
                        size_t start = 0;
                        size_t end = str.find(delim);
                        while (end != std::string::npos)
                        {
                            res->elements.push_back(Value(str.substr(start, end - start)));
                            start = end + delim.length();
                            end = str.find(delim, start);
                        }
                        res->elements.push_back(Value(str.substr(start, end)));
                        return Value(res);
                    }};
    methods[i++] = {string_type, "replace", 2, [](Value &s, const auto &a) -> Result<Value>
                    {
                        if (!is_string(a[0]) || !is_string(a[1]))
                            return std::unexpected(err::msg("Search and replacement must be strings.", "interpreter", 0, 0));
                        std::string str = get_string(s);
                        std::string search = get_string(a[0]);
                        std::string replace = get_string(a[1]);
                        size_t pos = 0;
                        while ((pos = str.find(search, pos)) != std::string::npos)
                        {
                            str.replace(pos, search.length(), replace);
                            pos += replace.length();
                        }
                        return Value(str);
                    }};

    return methods;
}
}  // namespace phos::native
