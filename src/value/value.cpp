#include "value.hpp"
#include <format>

namespace phos
{

// --- Type Checkers ---
bool is_nil(const Value &val) { return std::holds_alternative<std::nullptr_t>(val) || std::holds_alternative<std::monostate>(val); }
bool is_bool(const Value &val) { return std::holds_alternative<bool>(val); }
bool is_int(const Value &val) { return std::holds_alternative<long>(val); }
bool is_float(const Value &val) { return std::holds_alternative<double>(val); }
bool is_string(const Value &val) { return std::holds_alternative<std::string>(val); }
bool is_array(const Value &val) { return std::holds_alternative<mem::rc_ptr<Array_value>>(val); }
bool is_model(const Value &val) { return std::holds_alternative<mem::rc_ptr<Model_value>>(val); }
bool is_union(const Value &val) { return std::holds_alternative<mem::rc_ptr<Union_value>>(val); }
bool is_closure(const Value &val) { return std::holds_alternative<mem::rc_ptr<Closure_value>>(val); }
bool is_iterator(const Value &val) { return std::holds_alternative<mem::rc_ptr<Iterator_value>>(val); }

// --- Getters ---
bool get_bool(const Value &val) { return std::get<bool>(val); }
long get_int(const Value &val) { return std::get<long>(val); }
double get_float(const Value &val) { return std::get<double>(val); }
std::string get_string(const Value &val) { return std::get<std::string>(val); }
mem::rc_ptr<Array_value> get_array(const Value &val) { return std::get<mem::rc_ptr<Array_value>>(val); }
mem::rc_ptr<Model_value> get_model(const Value &val) { return std::get<mem::rc_ptr<Model_value>>(val); }
mem::rc_ptr<Closure_value> get_closure(const Value &val) { return std::get<mem::rc_ptr<Closure_value>>(val); }
mem::rc_ptr<Union_value> get_union(const Value &val) { return std::get<mem::rc_ptr<Union_value>>(val); }
mem::rc_ptr<Iterator_value> get_iterator(const Value &val) { return std::get<mem::rc_ptr<Iterator_value>>(val); }
// --- Utility ---
std::string value_to_string(const Value &val)
{
    if (is_nil(val))
        return "nil";
    if (is_bool(val))
        return get_bool(val) ? "true" : "false";

    if (is_int(val))
    {
        char buf[32];
        auto [ptr, ec] = std::to_chars(buf, buf + sizeof(buf), get_int(val));
        return std::string(buf, ptr);
    }
    if (is_float(val))
    {
        char buf[64];
        auto [ptr, ec] = std::to_chars(buf, buf + sizeof(buf), get_float(val));
        return std::string(buf, ptr);
    }
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

    if (is_int(val) || is_float(val))
        return value_to_string(val);

    if (is_string(val))
        return std::format("\"{}\"", get_string(val));

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
    if (is_int(a))
        return get_int(a) == get_int(b);
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
