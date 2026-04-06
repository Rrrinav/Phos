#include "value.hpp"
#include <charconv>
#include <cmath>
#include <format>
#include <limits>

namespace phos
{

namespace
{

template <typename T>
std::string format_integer(T value)
{
    char buf[32];
    if constexpr (std::is_signed_v<T>)
    {
        auto [ptr, ec] = std::to_chars(buf, buf + sizeof(buf), static_cast<long long>(value));
        (void)ec;
        return std::string(buf, ptr);
    }
    else
    {
        auto [ptr, ec] = std::to_chars(buf, buf + sizeof(buf), static_cast<unsigned long long>(value));
        (void)ec;
        return std::string(buf, ptr);
    }
}

template <typename T>
std::string format_float(T value)
{
    char buf[64];
    auto [ptr, ec] = std::to_chars(buf, buf + sizeof(buf), static_cast<double>(value));
    (void)ec;
    return std::string(buf, ptr);
}

template <typename Target, typename Source>
bool integral_fits(Source value)
{
    if constexpr (!std::is_integral_v<Target> || !std::is_integral_v<Source>)
    {
        return false;
    }
    else if constexpr (std::is_signed_v<Source> == std::is_signed_v<Target>)
    {
        return value >= std::numeric_limits<Target>::min() && value <= std::numeric_limits<Target>::max();
    }
    else if constexpr (std::is_signed_v<Source>)
    {
        if (value < 0)
            return false;
        using UnsignedSource = std::make_unsigned_t<Source>;
        return static_cast<UnsignedSource>(value) <= std::numeric_limits<Target>::max();
    }
    else
    {
        if (value > static_cast<std::make_unsigned_t<Target>>(std::numeric_limits<Target>::max()))
            return false;
        return true;
    }
}

template <typename Target, typename Source>
Value numeric_cast_value(Source value)
{
    return Value(static_cast<Target>(value));
}

template <typename Target, typename Source>
std::optional<Value> coerce_literal_to(Source value)
{
    if constexpr (is_integer_cpp_v<Source> && is_integer_cpp_v<Target>)
    {
        if (!integral_fits<Target>(value))
            return std::nullopt;
        return Value(static_cast<Target>(value));
    }
    else if constexpr (is_integer_cpp_v<Source> && is_float_cpp_v<Target>)
    {
        return Value(static_cast<Target>(value));
    }
    else if constexpr (is_float_cpp_v<Source> && is_float_cpp_v<Target>)
    {
        return Value(static_cast<Target>(value));
    }
    else
    {
        return std::nullopt;
    }
}

template <typename Target, typename Source>
std::optional<Value> cast_numeric_source(Source value)
{
    if constexpr (is_numeric_cpp_v<Source>)
        return numeric_cast_value<Target>(value);
    return std::nullopt;
}

template <typename Source>
std::optional<Value> cast_numeric_dispatch(Source value, types::Primitive_kind target_type)
{
    switch (target_type)
    {
        case types::Primitive_kind::I8:
            return cast_numeric_source<std::int8_t>(value);
        case types::Primitive_kind::I16:
            return cast_numeric_source<std::int16_t>(value);
        case types::Primitive_kind::I32:
            return cast_numeric_source<std::int32_t>(value);
        case types::Primitive_kind::I64:
            return cast_numeric_source<std::int64_t>(value);
        case types::Primitive_kind::U8:
            return cast_numeric_source<std::uint8_t>(value);
        case types::Primitive_kind::U16:
            return cast_numeric_source<std::uint16_t>(value);
        case types::Primitive_kind::U32:
            return cast_numeric_source<std::uint32_t>(value);
        case types::Primitive_kind::U64:
            return cast_numeric_source<std::uint64_t>(value);
        case types::Primitive_kind::F16:
            return cast_numeric_source<std::float16_t>(value);
        case types::Primitive_kind::F32:
            return cast_numeric_source<float>(value);
        case types::Primitive_kind::F64:
            return cast_numeric_source<double>(value);
        default:
            return std::nullopt;
    }
}

template <typename Target, typename Source>
std::optional<Value> coerce_literal_source(Source value)
{
    return coerce_literal_to<Target>(value);
}

template <typename Source>
std::optional<Value> coerce_literal_dispatch(Source value, types::Primitive_kind target_type)
{
    switch (target_type)
    {
        case types::Primitive_kind::I8:
            return coerce_literal_source<std::int8_t>(value);
        case types::Primitive_kind::I16:
            return coerce_literal_source<std::int16_t>(value);
        case types::Primitive_kind::I32:
            return coerce_literal_source<std::int32_t>(value);
        case types::Primitive_kind::I64:
            return coerce_literal_source<std::int64_t>(value);
        case types::Primitive_kind::U8:
            return coerce_literal_source<std::uint8_t>(value);
        case types::Primitive_kind::U16:
            return coerce_literal_source<std::uint16_t>(value);
        case types::Primitive_kind::U32:
            return coerce_literal_source<std::uint32_t>(value);
        case types::Primitive_kind::U64:
            return coerce_literal_source<std::uint64_t>(value);
        case types::Primitive_kind::F16:
            return coerce_literal_source<std::float16_t>(value);
        case types::Primitive_kind::F32:
            return coerce_literal_source<float>(value);
        case types::Primitive_kind::F64:
            return coerce_literal_source<double>(value);
        default:
            return std::nullopt;
    }
}

}  // namespace

// --- Type Checkers ---
bool is_nil(const Value &val) { return std::holds_alternative<std::nullptr_t>(val) || std::holds_alternative<std::monostate>(val); }
bool is_bool(const Value &val) { return std::holds_alternative<bool>(val); }
bool is_integer(const Value &val)
{
    return std::holds_alternative<std::int8_t>(val) || std::holds_alternative<std::int16_t>(val) || std::holds_alternative<std::int32_t>(val) ||
           std::holds_alternative<std::int64_t>(val) || std::holds_alternative<std::uint8_t>(val) || std::holds_alternative<std::uint16_t>(val) ||
           std::holds_alternative<std::uint32_t>(val) || std::holds_alternative<std::uint64_t>(val);
}
bool is_signed_integer(const Value &val)
{
    return std::holds_alternative<std::int8_t>(val) || std::holds_alternative<std::int16_t>(val) || std::holds_alternative<std::int32_t>(val) ||
           std::holds_alternative<std::int64_t>(val);
}
bool is_unsigned_integer(const Value &val)
{
    return std::holds_alternative<std::uint8_t>(val) || std::holds_alternative<std::uint16_t>(val) || std::holds_alternative<std::uint32_t>(val) ||
           std::holds_alternative<std::uint64_t>(val);
}
bool is_float(const Value &val)
{
    return std::holds_alternative<std::float16_t>(val) || std::holds_alternative<float>(val) || std::holds_alternative<double>(val);
}
bool is_numeric(const Value &val) { return is_integer(val) || is_float(val); }
bool is_string(const Value &val) { return std::holds_alternative<std::string>(val); }
bool is_array(const Value &val) { return std::holds_alternative<mem::rc_ptr<Array_value>>(val); }
bool is_model(const Value &val) { return std::holds_alternative<mem::rc_ptr<Model_value>>(val); }
bool is_union(const Value &val) { return std::holds_alternative<mem::rc_ptr<Union_value>>(val); }
bool is_closure(const Value &val) { return std::holds_alternative<mem::rc_ptr<Closure_value>>(val); }
bool is_iterator(const Value &val) { return std::holds_alternative<mem::rc_ptr<Iterator_value>>(val); }

// --- Getters ---
bool get_bool(const Value &val) { return std::get<bool>(val); }
std::int64_t get_int(const Value &val)
{
    return std::visit(
    [](const auto &value) -> std::int64_t
    {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, std::int8_t> || std::is_same_v<T, std::int16_t> || std::is_same_v<T, std::int32_t> ||
                      std::is_same_v<T, std::int64_t>)
        {
            return static_cast<std::int64_t>(value);
        }
        else
        {
            throw std::bad_variant_access();
        }
    },
    val);
}
std::uint64_t get_uint(const Value &val)
{
    return std::visit(
    [](const auto &value) -> std::uint64_t
    {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, std::uint8_t> || std::is_same_v<T, std::uint16_t> || std::is_same_v<T, std::uint32_t> ||
                      std::is_same_v<T, std::uint64_t>)
        {
            return static_cast<std::uint64_t>(value);
        }
        else
        {
            throw std::bad_variant_access();
        }
    },
    val);
}
double get_float(const Value &val)
{
    return std::visit(
    [](const auto &value) -> double
    {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, std::float16_t> || std::is_same_v<T, float> || std::is_same_v<T, double>)
            return static_cast<double>(value);
        throw std::bad_variant_access();
    },
    val);
}
std::string get_string(const Value &val) { return std::get<std::string>(val); }
mem::rc_ptr<Array_value> get_array(const Value &val) { return std::get<mem::rc_ptr<Array_value>>(val); }
mem::rc_ptr<Model_value> get_model(const Value &val) { return std::get<mem::rc_ptr<Model_value>>(val); }
mem::rc_ptr<Closure_value> get_closure(const Value &val) { return std::get<mem::rc_ptr<Closure_value>>(val); }
mem::rc_ptr<Union_value> get_union(const Value &val) { return std::get<mem::rc_ptr<Union_value>>(val); }
mem::rc_ptr<Iterator_value> get_iterator(const Value &val) { return std::get<mem::rc_ptr<Iterator_value>>(val); }

types::Primitive_kind numeric_type_of(const Value &val)
{
    return std::visit(
    [](const auto &value) -> types::Primitive_kind
    {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, std::int8_t>)
            return types::Primitive_kind::I8;
        else if constexpr (std::is_same_v<T, std::int16_t>)
            return types::Primitive_kind::I16;
        else if constexpr (std::is_same_v<T, std::int32_t>)
            return types::Primitive_kind::I32;
        else if constexpr (std::is_same_v<T, std::int64_t>)
            return types::Primitive_kind::I64;
        else if constexpr (std::is_same_v<T, std::uint8_t>)
            return types::Primitive_kind::U8;
        else if constexpr (std::is_same_v<T, std::uint16_t>)
            return types::Primitive_kind::U16;
        else if constexpr (std::is_same_v<T, std::uint32_t>)
            return types::Primitive_kind::U32;
        else if constexpr (std::is_same_v<T, std::uint64_t>)
            return types::Primitive_kind::U64;
        else if constexpr (std::is_same_v<T, std::float16_t>)
            return types::Primitive_kind::F16;
        else if constexpr (std::is_same_v<T, float>)
            return types::Primitive_kind::F32;
        else if constexpr (std::is_same_v<T, double>)
            return types::Primitive_kind::F64;
        else
            return types::Primitive_kind::Any;
    },
    val);
}

std::optional<Value> cast_numeric_value(const Value &val, types::Primitive_kind target_type)
{
    if (!is_numeric(val) || !types::is_numeric_primitive(target_type))
        return std::nullopt;

    return std::visit(
    [&](const auto &value) -> std::optional<Value>
    {
        return cast_numeric_dispatch(value, target_type);
    },
    val);
}

std::optional<Value> coerce_numeric_literal(const Value &val, types::Primitive_kind target_type)
{
    if (!is_numeric(val) || !types::is_numeric_primitive(target_type))
        return std::nullopt;

    return std::visit(
    [&](const auto &value) -> std::optional<Value>
    {
        return coerce_literal_dispatch(value, target_type);
    },
    val);
}
// --- Utility ---
std::string value_to_string(const Value &val)
{
    if (is_nil(val))
        return "nil";
    if (is_bool(val))
        return get_bool(val) ? "true" : "false";

    if (is_numeric(val))
        return std::visit(
        [](const auto &value) -> std::string
        {
            using T = std::decay_t<decltype(value)>;
            if constexpr (is_signed_integer_cpp_v<T> || is_unsigned_integer_cpp_v<T>)
                return format_integer(value);
            else if constexpr (is_float_cpp_v<T>)
                return format_float(value);
            else
                return "";
        },
        val);
    if (is_string(val))
        return get_string(val);  // No quotes internally

    if (is_closure(val))
    {
        auto closure = get_closure(val);
        return closure->native_func ? std::format("<native fn {}>", closure->name) : std::format("<fn {}>", closure->name);
    }
    if (is_array(val))
        return std::format("<array len {}>", get_array(val)->elements.size());
    if (is_model(val))
        return std::format("<model {}>", get_model(val)->signature.name);
    if (is_iterator(val))
        return std::format("<iter {}>", types::type_to_string(get_iterator(val)->element_type));

    if (std::holds_alternative<mem::rc_ptr<Union_value>>(val))
    {
        auto u = std::get<mem::rc_ptr<Union_value>>(val);
        return std::format("<{}::{}>", u->union_name, u->variant_name);
    }
    if (std::holds_alternative<mem::rc_ptr<Green_thread_value>>(val))
        return "<green thread>";

    return "<unknown>";
}

std::string value_to_str_debug(const Value &val)
{
    if (is_nil(val))
        return "nil";
    if (is_bool(val))
        return get_bool(val) ? "true" : "false";

    if (is_numeric(val))
        return value_to_string(val);

    if (is_string(val))
        return std::format("{}", get_string(val));

    if (is_closure(val))
        return value_to_string(val);

    if (is_array(val))
    {
        auto arr = get_array(val);
        std::string res = "[";
        for (size_t i = 0; i < arr->elements.size(); ++i)
        {
            res += value_to_str_debug(arr->elements[i]);
            if (i != arr->elements.size() - 1)
                res += ", ";
        }
        res += "]";
        return res;
    }

    if (is_model(val))
    {
        auto model = get_model(val);
        std::string res = model->signature.name + " { ";
        for (size_t i = 0; i < model->fields.size(); ++i)
        {
            res += value_to_str_debug(model->fields[i]);
            if (i != model->fields.size() - 1)
                res += ", ";
        }
        res += " }";
        return res;
    }

    if (std::holds_alternative<mem::rc_ptr<Union_value>>(val))
    {
        auto u = std::get<mem::rc_ptr<Union_value>>(val);
        return std::format("{}::{}({})", u->union_name, u->variant_name, value_to_str_debug(u->payload));
    }

    return value_to_string(val);  // Fallback to internal
}

// --- Equality Operators ---
bool operator==(const Value &a, const Value &b)
{
    // If they aren't even the same variant type, they are definitely not equal
    if (a.index() != b.index())
        return false;

    if (is_nil(a))
        return true;
    if (is_bool(a))
        return get_bool(a) == get_bool(b);
    if (is_signed_integer(a))
        return get_int(a) == get_int(b);
    if (is_unsigned_integer(a))
        return get_uint(a) == get_uint(b);
    if (is_float(a))
        return get_float(a) == get_float(b);
    if (is_string(a))
        return get_string(a) == get_string(b);

    // For Reference Counted objects, we do fast pointer equality
    // (If you want deep-equality later for Arrays, you would loop through elements here)
    if (is_closure(a))
        return get_closure(a) == get_closure(b);
    if (is_array(a))
        return get_array(a) == get_array(b);
    if (is_model(a))
        return get_model(a) == get_model(b);

    if (std::holds_alternative<mem::rc_ptr<Union_value>>(a))
        return std::get<mem::rc_ptr<Union_value>>(a) == std::get<mem::rc_ptr<Union_value>>(b);
    if (std::holds_alternative<mem::rc_ptr<Iterator_value>>(a))
        return std::get<mem::rc_ptr<Iterator_value>>(a) == std::get<mem::rc_ptr<Iterator_value>>(b);
    if (std::holds_alternative<mem::rc_ptr<Green_thread_value>>(a))
        return std::get<mem::rc_ptr<Green_thread_value>>(a) == std::get<mem::rc_ptr<Green_thread_value>>(b);

    return false;
}

bool operator!=(const Value &a, const Value &b) { return !(a == b); }

}  // namespace phos
