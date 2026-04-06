#pragma once

#include <cstdlib>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <print>
#include <unordered_map>
#include <utility>

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
    if (is_iterator(a))
        return get_iterator(a) == get_iterator(b);
    if (is_signed_integer(a))
        return get_int(a) == get_int(b);
    if (is_unsigned_integer(a))
        return get_uint(a) == get_uint(b);
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

inline bool core_is_same_mem()
{
    return true;
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

[[noreturn]] inline void optional_panic(const std::string &message);

inline std::vector<size_t> utf8_boundaries(const std::string &str)
{
    std::vector<size_t> boundaries;
    boundaries.push_back(0);

    size_t i = 0;
    while (i < str.size())
    {
        unsigned char byte = static_cast<unsigned char>(str[i]);
        size_t width = 1;

        if ((byte & 0x80u) == 0x00u)
            width = 1;
        else if ((byte & 0xE0u) == 0xC0u)
            width = 2;
        else if ((byte & 0xF0u) == 0xE0u)
            width = 3;
        else if ((byte & 0xF8u) == 0xF0u)
            width = 4;

        if (i + width > str.size())
            width = 1;

        i += width;
        boundaries.push_back(i);
    }

    return boundaries;
}

inline int64_t iterator_length(const Iterator_value &iter)
{
    return std::visit(
    [](const auto &state) -> int64_t
    {
        using T = std::decay_t<decltype(state)>;
        if constexpr (std::is_same_v<T, Iterator_value::Empty_state>)
        {
            return 0;
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Singleton_state>)
        {
            return 1;
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Interval_state>)
        {
            if (state.start == state.end)
                return state.inclusive ? 1 : 0;

            if (state.start < state.end)
                return state.inclusive ? (state.end - state.start + 1) : (state.end - state.start);

            return state.inclusive ? (state.start - state.end + 1) : (state.start - state.end);
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Array_state>)
        {
            return static_cast<int64_t>(state.array ? state.array->elements.size() : 0);
        }
        else if constexpr (std::is_same_v<T, Iterator_value::String_state>)
        {
            return state.boundaries.empty() ? 0 : static_cast<int64_t>(state.boundaries.size() - 1);
        }
        return 0;
    },
    iter.source);
}

inline Value iterator_item_at(const Iterator_value &iter, int64_t index)
{
    return std::visit(
    [&](const auto &state) -> Value
    {
        using T = std::decay_t<decltype(state)>;
        if constexpr (std::is_same_v<T, Iterator_value::Empty_state>)
        {
            return Value(nullptr);
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Singleton_state>)
        {
            return index == 0 ? state.value : Value(nullptr);
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Interval_state>)
        {
            const int64_t step = state.start <= state.end ? 1 : -1;
            Value raw = Value(static_cast<std::int64_t>(state.start + (index * step)));
            if (types::is_primitive(iter.element_type) &&
                types::is_integer_primitive(types::get_primitive_kind(iter.element_type)))
            {
                auto casted = cast_numeric_value(raw, types::get_primitive_kind(iter.element_type));
                if (casted)
                    return casted.value();
            }
            return raw;
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Array_state>)
        {
            if (!state.array || index < 0 || index >= static_cast<int64_t>(state.array->elements.size()))
                return Value(nullptr);
            return state.array->elements[static_cast<size_t>(index)];
        }
        else if constexpr (std::is_same_v<T, Iterator_value::String_state>)
        {
            if (index < 0 || index + 1 >= static_cast<int64_t>(state.boundaries.size()))
                return Value(nullptr);
            size_t start = state.boundaries[static_cast<size_t>(index)];
            size_t end = state.boundaries[static_cast<size_t>(index + 1)];
            return Value(state.source.substr(start, end - start));
        }
        return Value(nullptr);
    },
    iter.source);
}

inline Value iter_next(mem::rc_ptr<Iterator_value> iter)
{
    if (!iter)
        return Value(nullptr);

    const int64_t len = iterator_length(*iter);
    if (iter->cursor < 0)
    {
        if (len > 0)
        {
            iter->cursor = 0;
            return iterator_item_at(*iter, iter->cursor);
        }

        iter->cursor = len;
        return Value(nullptr);
    }

    if (iter->cursor < len - 1)
    {
        ++iter->cursor;
        return iterator_item_at(*iter, iter->cursor);
    }

    iter->cursor = len;
    return Value(nullptr);
}

inline Value iter_prev(mem::rc_ptr<Iterator_value> iter)
{
    if (!iter)
        return Value(nullptr);

    const int64_t len = iterator_length(*iter);
    if (iter->cursor == len && len > 0)
    {
        --iter->cursor;
        return iterator_item_at(*iter, iter->cursor);
    }

    if (iter->cursor > 0)
    {
        --iter->cursor;
        return iterator_item_at(*iter, iter->cursor);
    }

    iter->cursor = -1;
    return Value(nullptr);
}

inline Value iter_get(mem::rc_ptr<Iterator_value> iter)
{
    if (!iter)
        return Value(nullptr);

    if (iter->cursor >= 0 && iter->cursor < iterator_length(*iter))
        return iterator_item_at(*iter, iter->cursor);
    return Value(nullptr);
}

inline int64_t require_i64(Value value, const std::string &context)
{
    auto converted = try_get_i64(value);
    if (!converted)
        optional_panic(context + " must fit into the current i64-backed runtime range.");
    return *converted;
}

inline types::Primitive_kind promote_integer_kind(types::Primitive_kind left, types::Primitive_kind right)
{
    bool same_signedness =
    (types::is_signed_integer_primitive(left) && types::is_signed_integer_primitive(right)) ||
    (types::is_unsigned_integer_primitive(left) && types::is_unsigned_integer_primitive(right));

    if (!same_signedness)
        return types::Primitive_kind::I64;

    return types::primitive_bit_width(left) >= types::primitive_bit_width(right) ? left : right;
}

inline bool iter_has_next(mem::rc_ptr<Iterator_value> iter, Value steps_value)
{
    if (!iter)
        return false;

    int64_t steps = require_i64(steps_value, "Iterator step");
    const int64_t len = iterator_length(*iter);
    int64_t cursor = iter->cursor;

    if (steps < 0)
        return false;
    if (steps == 0)
        return cursor >= 0 && cursor < len;

    for (int64_t i = 0; i < steps; ++i)
    {
        if (cursor < 0)
        {
            if (len > 0)
                cursor = 0;
            else
                return false;
        }
        else if (cursor < len - 1)
            ++cursor;
        else
            return false;
    }
    return true;
}

inline bool iter_has_prev(mem::rc_ptr<Iterator_value> iter, Value steps_value)
{
    if (!iter)
        return false;

    int64_t steps = require_i64(steps_value, "Iterator step");
    const int64_t len = iterator_length(*iter);
    int64_t cursor = iter->cursor;

    if (steps < 0)
        return false;
    if (steps == 0)
        return cursor >= 0 && cursor < len;

    for (int64_t i = 0; i < steps; ++i)
    {
        if (cursor > 0)
            --cursor;
        else if (cursor == len && len > 0)
            --cursor;
        else
            return false;
    }
    return true;
}

inline Value iter_next(mem::rc_ptr<Iterator_value> iter, Value steps_value)
{
    if (!iter)
        return Value(nullptr);

    int64_t steps = require_i64(steps_value, "Iterator step");
    if (steps < 0)
        return Value(nullptr);
    if (steps == 0)
    {
        if (iter->cursor >= 0 && iter->cursor < iterator_length(*iter))
            return iterator_item_at(*iter, iter->cursor);
        return Value(nullptr);
    }

    Value result = Value(nullptr);
    for (int64_t i = 0; i < steps; ++i)
        result = iter_next(iter);
    return result;
}

inline Value iter_prev(mem::rc_ptr<Iterator_value> iter, Value steps_value)
{
    if (!iter)
        return Value(nullptr);

    int64_t steps = require_i64(steps_value, "Iterator step");
    if (steps < 0)
        return Value(nullptr);
    if (steps == 0)
    {
        if (iter->cursor >= 0 && iter->cursor < iterator_length(*iter))
            return iterator_item_at(*iter, iter->cursor);
        return Value(nullptr);
    }

    Value result = Value(nullptr);
    for (int64_t i = 0; i < steps; ++i)
        result = iter_prev(iter);
    return result;
}

inline bool iter_contains(mem::rc_ptr<Iterator_value> iter, Value target)
{
    if (!iter)
        return false;

    return std::visit(
    [&](const auto &state) -> bool
    {
        using T = std::decay_t<decltype(state)>;

        // 1. O(1) Math check for Ranges (The secret sauce for pattern matching!)
        if constexpr (std::is_same_v<T, Iterator_value::Interval_state>)
        {
            auto maybe_target = try_get_i64(target);
            if (!maybe_target)
                return false;
            int64_t val = *maybe_target;

            if (state.start <= state.end)
                if (state.inclusive)
                    return val >= state.start && val <= state.end;
                else
                    return val >= state.start && val < state.end;
            else if (state.inclusive)
                return val <= state.start && val >= state.end;
            else
                return val <= state.start && val > state.end;
        }
        // 2. O(N) Linear scan for Arrays
        else if constexpr (std::is_same_v<T, Iterator_value::Array_state>)
        {
            if (!state.array)
                return false;
            for (const auto &elem : state.array->elements)
                if (core_is_same(elem, target))
                    return true;
            return false;
        }
        // 3. Substring check for Strings
        else if constexpr (std::is_same_v<T, Iterator_value::String_state>)
        {
            if (!is_string(target))
                return false;
            return state.source.find(get_string(target)) != std::string::npos;
        }
        // 4. Exact match for Singletons
        else if constexpr (std::is_same_v<T, Iterator_value::Singleton_state>)
        {
            return core_is_same(state.value, target);
        }

        // Empty state
        return false;
    },
    iter->source);
}

inline Value make_iterator(types::Type element_type, Iterator_value::Source source)
{
    return Value(mem::make_rc<Iterator_value>(std::move(element_type), std::move(source)));
}

inline types::Type runtime_value_type(const Value &value)
{
    if (is_numeric(value))
        return numeric_type_of(value);
    if (is_float(value))
        return numeric_type_of(value);
    if (is_bool(value))
        return types::Primitive_kind::Bool;
    if (is_string(value))
        return types::Primitive_kind::String;
    if (is_array(value))
    {
        auto array = get_array(value);
        if (types::is_array(array->type))
            return types::get_array_type(array->type)->element_type;
        return types::Primitive_kind::Any;
    }
    if (is_closure(value))
        return types::Type(mem::make_rc<types::Function_type>(get_closure(value)->signature));
    if (is_model(value))
        return types::Type(mem::make_rc<types::Model_type>(get_model(value)->signature));
    if (is_iterator(value))
        return get_iterator(value)->element_type;
    return types::Primitive_kind::Any;
}

inline Value iterator_from_value(Value value)
{
    if (is_iterator(value))
        return value;
    if (is_nil(value))
        return make_iterator(types::Primitive_kind::Any, Iterator_value::Empty_state{});
    if (is_array(value))
        return make_iterator(runtime_value_type(value), Iterator_value::Array_state{get_array(value)});
    if (is_string(value))
    {
        auto str = get_string(value);
        return make_iterator(types::Primitive_kind::String, Iterator_value::String_state{str, utf8_boundaries(str)});
    }

    return make_iterator(runtime_value_type(value), Iterator_value::Singleton_state{value});
}

inline Value range_exclusive(Value start_value, Value end_value)
{
    int64_t start = require_i64(start_value, "Range start");
    int64_t end = require_i64(end_value, "Range end");
    auto element_kind = promote_integer_kind(numeric_type_of(start_value), numeric_type_of(end_value));
    return make_iterator(element_kind, Iterator_value::Interval_state{start, end, false});
}

inline Value range_inclusive(Value start_value, Value end_value)
{
    int64_t start = require_i64(start_value, "Range start");
    int64_t end = require_i64(end_value, "Range end");
    auto element_kind = promote_integer_kind(numeric_type_of(start_value), numeric_type_of(end_value));
    return make_iterator(element_kind, Iterator_value::Interval_state{start, end, true});
}

inline Value deep_clone_value(const Value &value, std::unordered_map<const void *, Value> &seen);

inline Value clone_iterator(mem::rc_ptr<Iterator_value> iter, std::unordered_map<const void *, Value> &seen)
{
    if (!iter)
        return Value(nullptr);
    if (auto it = seen.find(iter.get()); it != seen.end())
        return it->second;

    Iterator_value::Source source = std::visit(
    [&](const auto &state) -> Iterator_value::Source
    {
        using T = std::decay_t<decltype(state)>;
        if constexpr (std::is_same_v<T, Iterator_value::Empty_state>)
        {
            return state;
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Singleton_state>)
        {
            return Iterator_value::Singleton_state{deep_clone_value(state.value, seen)};
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Interval_state>)
        {
            return state;
        }
        else if constexpr (std::is_same_v<T, Iterator_value::Array_state>)
        {
            auto cloned = deep_clone_value(Value(state.array), seen);
            return Iterator_value::Array_state{get_array(cloned)};
        }
        else if constexpr (std::is_same_v<T, Iterator_value::String_state>)
        {
            return state;
        }
        return Iterator_value::Empty_state{};
    },
    iter->source);

    auto cloned_iter = mem::make_rc<Iterator_value>(iter->element_type, std::move(source));
    cloned_iter->cursor = iter->cursor;
    Value result = Value(cloned_iter);
    seen[iter.get()] = result;
    return result;
}

inline Value deep_clone_value(const Value &value, std::unordered_map<const void *, Value> &seen)
{
    if (is_nil(value) || is_bool(value) || is_numeric(value) || is_string(value) || is_closure(value))
        return value;

    if (is_array(value))
    {
        auto arr = get_array(value);
        if (auto it = seen.find(arr.get()); it != seen.end())
            return it->second;

        auto cloned = mem::make_rc<Array_value>(arr->type, std::vector<Value>{});
        Value result = Value(cloned);
        seen[arr.get()] = result;

        cloned->elements.reserve(arr->elements.size());
        for (const auto &elem : arr->elements)
            cloned->elements.push_back(deep_clone_value(elem, seen));
        return result;
    }

    if (is_model(value))
    {
        auto model = get_model(value);
        if (auto it = seen.find(model.get()); it != seen.end())
            return it->second;

        auto cloned = mem::make_rc<Model_value>(model->signature, std::vector<Value>{});
        Value result = Value(cloned);
        seen[model.get()] = result;

        cloned->fields.reserve(model->fields.size());
        for (const auto &field : model->fields)
            cloned->fields.push_back(deep_clone_value(field, seen));
        return result;
    }

    if (is_union(value))
    {
        auto uni = get_union(value);
        if (auto it = seen.find(uni.get()); it != seen.end())
            return it->second;

        auto cloned = mem::make_rc<Union_value>(uni->union_name, uni->variant_name, Value(nullptr));
        Value result = Value(cloned);
        seen[uni.get()] = result;

        cloned->payload = deep_clone_value(uni->payload, seen);
        return result;
    }

    if (is_iterator(value))
        return clone_iterator(get_iterator(value), seen);

    return value;
}

inline Value core_clone(Value value)
{
    std::unordered_map<const void *, Value> seen;
    return deep_clone_value(value, seen);
}

[[noreturn]] inline void optional_panic(const std::string &message)
{
    std::println(stderr, "Runtime error: {}", message);
    std::exit(1);
}

inline Value optional_get(Value value, std::string message)
{
    if (is_nil(value))
        optional_panic(message);
    return value;
}

inline Value optional_or(Value value, Value fallback)
{
    if (is_nil(value))
        return fallback;
    return value;
}

inline bool optional_has_value(Value value) { return !is_nil(value); }
inline bool optional_is_some(Value value) { return !is_nil(value); }
inline bool optional_is_none(Value value) { return is_nil(value); }

inline void register_core_library(Virtual_machine &vm, Type_checker &tc)
{
    constexpr std::string_view integer_types = "i8 | i16 | i32 | i64 | u8 | u16 | u32 | u64";

    vm.bind_native<core_is_same>("is_same", {"T", "T"}, "bool", tc);
    // Clock
    vm.bind_native<clock_now>("clock", {}, "f64", tc);

    vm.bind_native<iterator_from_value>("iter", {"any"}, "any", tc);
    vm.bind_native<core_clone>("clone", {"any"}, "any", tc);

    vm.bind_native_sig<range_exclusive>("__range_exclusive",
                                        {{"start", std::string(integer_types), std::nullopt},
                                         {"end", std::string(integer_types), std::nullopt}},
                                        "any",
                                        tc);
    vm.bind_native_sig<range_inclusive>("__range_inclusive",
                                        {{"start", std::string(integer_types), std::nullopt},
                                         {"end", std::string(integer_types), std::nullopt}},
                                        "any",
                                        tc);

    // len() strictly accepts either ONE string OR ONE array
    // (We use two bind_native calls to register two valid signatures for the same function!)
    vm.bind_native_sig<core_len>("len", {{"value", "string", std::nullopt}}, "i64", tc);
    vm.bind_native_sig<core_len>("len", {{"value", "any[]", std::nullopt}}, "i64", tc);

    // Array.push() takes an array of T, and a value of T. Returns T.
    vm.bind_native_sig<array_push>("Array::push", {{"", "T[]", std::nullopt}, {"value", "T", std::nullopt}}, "T", tc);
    // Array.pop() takes an array of T, and returns an optional T?
    vm.bind_native<array_pop>("Array::pop", {"T[]"}, "T?", tc);
    vm.bind_native_sig<array_join>("Array::join", {{"", "T[]", std::nullopt}, {"separator", "string", std::nullopt}}, "string", tc);

    vm.bind_native_sig<static_cast<Value (*)(mem::rc_ptr<Iterator_value>, Value)>(&iter_next)>(
    "Iter::next",
    {{"", "any", std::nullopt}, {"step", std::string(integer_types), Value(int64_t(1))}},
    "any",
    tc);
    vm.bind_native_sig<static_cast<Value (*)(mem::rc_ptr<Iterator_value>, Value)>(&iter_prev)>(
    "Iter::prev",
    {{"", "any", std::nullopt}, {"step", std::string(integer_types), Value(int64_t(1))}},
    "any",
    tc);
    vm.bind_native_sig<iter_get>("Iter::get", {{"", "any", std::nullopt}}, "any", tc);
    vm.bind_native_sig<static_cast<bool (*)(mem::rc_ptr<Iterator_value>, Value)>(&iter_has_next)>(
    "Iter::has_next",
    {{"", "any", std::nullopt}, {"step", std::string(integer_types), Value(int64_t(1))}},
    "bool",
    tc);
    vm.bind_native_sig<static_cast<bool (*)(mem::rc_ptr<Iterator_value>, Value)>(&iter_has_prev)>(
    "Iter::has_prev",
    {{"", "any", std::nullopt}, {"step", std::string(integer_types), Value(int64_t(1))}},
    "bool",
    tc);
    vm.bind_native_sig<iter_contains>("Iter::contains", {{"", "any", std::nullopt}, {"target", "any", std::nullopt}}, "bool", tc);

    vm.bind_native_sig<optional_get>("Optional::get", {{"", "T?", std::nullopt}, {"message", "string", Value(std::string("Tried to extract a value from nil."))}}, "T", tc);
    vm.bind_native_sig<optional_or>("Optional::or", {{"", "T?", std::nullopt}, {"default", "T", std::nullopt}}, "T", tc);
    vm.bind_native_sig<optional_has_value>("Optional::has_value", {{"", "T?", std::nullopt}}, "bool", tc);
    vm.bind_native_sig<optional_is_some>("Optional::is_some", {{"", "T?", std::nullopt}}, "bool", tc);
    vm.bind_native_sig<optional_is_none>("Optional::is_none", {{"", "T?", std::nullopt}}, "bool", tc);

    // Math
    vm.bind_native<math_sqrt>("sqrt", {"f64"}, "f64", tc);
    vm.bind_native_sig<math_pow>("pow", {{"base", "f64", std::nullopt}, {"exponent", "f64", std::nullopt}}, "f64", tc);
    vm.bind_native<math_abs>("abs", {"f64"}, "f64", tc);
    // Advanced String Bindings
    vm.bind_native_sig<string_starts_with>("string::starts_with", {{"", "string", std::nullopt}, {"prefix", "string", std::nullopt}}, "bool", tc);
    vm.bind_native_sig<string_ends_with>("string::ends_with", {{"", "string", std::nullopt}, {"suffix", "string", std::nullopt}}, "bool", tc);
    vm.bind_native_sig<string_index_of>("string::index_of", {{"", "string", std::nullopt}, {"search", "string", std::nullopt}}, "i64", tc);
    vm.bind_native_sig<string_repeat>("string::repeat", {{"", "string", std::nullopt}, {"count", "i64", std::nullopt}}, "string", tc);
    vm.bind_native_sig<string_split>("string::split", {{"", "string", std::nullopt}, {"separator", "string", std::nullopt}}, "string[]", tc);
    vm.bind_native<string_trim>("string::trim", {"string"}, "string", tc);
    vm.bind_native<string_trim_left>("string::trim_left", {"string"}, "string", tc);
    vm.bind_native<string_trim_right>("string::trim_right", {"string"}, "string", tc);
    // Notice the "?" in the return type! The compiler knows these can fail and return nil.
    vm.bind_native_sig<string_parse_int>("string::parse_i64", {{"", "string", std::nullopt}}, "i64?", tc);
    vm.bind_native_sig<string_parse_float>("string::parse_f64", {{"", "string", std::nullopt}}, "f64?", tc);
    vm.bind_native_sig<string_parse_int>("parse_i64", {{"text", "string", std::nullopt}}, "i64?", tc);
    vm.bind_native_sig<string_parse_float>("parse_f64", {{"text", "string", std::nullopt}}, "f64?", tc);
}

}  // namespace phos::vm::core
