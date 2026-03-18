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
bool is_closure(const Value &val) { return std::holds_alternative<mem::rc_ptr<Closure_value>>(val); }

// --- Getters ---
bool get_bool(const Value &val) { return std::get<bool>(val); }
long get_int(const Value &val) { return std::get<long>(val); }
double get_float(const Value &val) { return std::get<double>(val); }
std::string get_string(const Value &val) { return std::get<std::string>(val); }
mem::rc_ptr<Array_value> get_array(const Value &val) { return std::get<mem::rc_ptr<Array_value>>(val); }
mem::rc_ptr<Model_value> get_model(const Value &val) { return std::get<mem::rc_ptr<Model_value>>(val); }
mem::rc_ptr<Closure_value> get_closure(const Value &val) { return std::get<mem::rc_ptr<Closure_value>>(val); }

// --- Utility ---
std::string value_to_string(const Value &val)
{
    if (is_nil(val))
        return "nil";
    if (is_bool(val))
        return get_bool(val) ? "true" : "false";
    if (is_int(val))
        return std::to_string(get_int(val));
    if (is_float(val))
        return std::to_string(get_float(val));
    if (is_string(val))
        return get_string(val);

    if (is_closure(val))
    {
        auto closure = get_closure(val);
        // Differeniate string output so you know if it's Native or Bytecode!
        if (closure->native_func)
            return std::format("<native fn {}>", closure->name);
        return std::format("<fn {}>", closure->name);
    }

    if (is_array(val))
    {
        auto arr = get_array(val);
        std::string res = "[ ";
        for (size_t i = 0; i < arr->elements.size(); ++i)
        {
            res += value_to_string(arr->elements[i]);
            if (i != arr->elements.size() - 1)
                res += ", ";
        }
        res += " ]";
        return res;
    }

    if (is_model(val))
    {
        auto model = get_model(val);
        return std::format("<model {}>", model->signature.name);
    }

    if (std::holds_alternative<mem::rc_ptr<Union_value>>(val))
    {
        auto u = std::get<mem::rc_ptr<Union_value>>(val);
        return std::format("<{}::{}({})>", u->union_name, u->variant_name, value_to_string(u->payload));
    }

    if (std::holds_alternative<mem::rc_ptr<Green_thread_value>>(val))
        return "<green thread>";

    return "<unknown>";
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
    if (std::holds_alternative<mem::rc_ptr<Green_thread_value>>(a))
        return std::get<mem::rc_ptr<Green_thread_value>>(a) == std::get<mem::rc_ptr<Green_thread_value>>(b);

    return false;
}

bool operator!=(const Value &a, const Value &b) { return !(a == b); }

}  // namespace phos
