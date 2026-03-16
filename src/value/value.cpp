#include "value.hpp"

#include <sstream>
#include "type.hpp"

namespace phos
{

bool operator==(const Value &lhs, const Value &rhs)
{
    if (lhs.index() != rhs.index())
        return false;

    return std::visit(
    [&](auto &&l_val) -> bool
    {
        auto &&r_val = std::get<std::decay_t<decltype(l_val)>>(rhs);
        using T = std::decay_t<decltype(l_val)>;

        if constexpr (std::is_same_v<T, std::nullptr_t> || std::is_same_v<T, std::monostate>)
        {
            return true;
        }
        else if constexpr (std::is_same_v<T, mem::rc_ptr<Model_value>> || std::is_same_v<T, mem::rc_ptr<Closure_value>> ||
                           std::is_same_v<T, mem::rc_ptr<Array_value>> || std::is_same_v<T, mem::rc_ptr<Native_function_value>> ||
                           std::is_same_v<T, mem::rc_ptr<Green_thread_value>>)
        {
            // Pointer identity check for heap objects is much faster for VM execution
            if (!l_val || !r_val)
                return l_val == r_val;
            return l_val == r_val;
        }
        else
        {
            return l_val == r_val;
        }
    },
    lhs);
}

std::string value_to_string_impl(const Value &value, int indent_level)
{
    if (is_int(value))
        return std::to_string(get_int(value));
    if (is_float(value))
        return std::to_string(get_float(value));
    if (is_bool(value))
        return get_bool(value) ? "true" : "false";
    if (is_string(value))
        return "\"" + get_string(value) + "\"";
    if (is_nil(value))
        return "nil";
    if (is_void(value))
        return "void";

    if (is_array(value))
    {
        auto arr = get_array(value);
        if (arr->elements.empty())
            return "[]";
        std::stringstream ss;
        ss << "[";
        bool first = true;
        for (const auto &element : arr->elements)
        {
            if (!first)
                ss << ", ";
            ss << value_to_string_impl(element, indent_level);
            first = false;
        }
        return ss.str() + "]";
    }

    if (is_model(value))
    {
        auto model = get_model(value);
        if (model->fields.empty())
            return model->signature.name + " { }";
        std::stringstream ss;
        ss << model->signature.name << " { ";
        for (size_t i = 0; i < model->fields.size(); ++i)
        {
            if (i > 0)
                ss << ", ";
            // The name is safely stored in the compile-time signature!
            ss << model->signature.fields[i].first << ": " << value_to_string_impl(model->fields[i], indent_level);
        }
        return ss.str() + " }";
    }

    if (is_closure(value))
        return "fn " + get_closure(value)->name + "()";

    if (is_union(value))
        return "union<" + get_union(value)->tag + ">";

    if (is_thread(value))
    {
        auto t = get_thread(value);
        return t->is_completed ? "<thread: dead>" : "<thread: suspended>";
    }

    if (is_native_function(value))
        return "native_fn " + get_native_function(value)->name + "()";

    return "unknown";
}

std::string value_to_string(const Value &value) { return value_to_string_impl(value, 0); }

std::string get_value_type_string(const Value &value)
{
    return std::visit(
    [](const auto &v) -> std::string
    {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, int64_t>)
            return "i64";
        else if constexpr (std::is_same_v<T, double>)
            return "f64";
        else if constexpr (std::is_same_v<T, bool>)
            return "bool";
        else if constexpr (std::is_same_v<T, std::string>)
            return "string";
        else if constexpr (std::is_same_v<T, mem::rc_ptr<Model_value>>)
            return types::type_to_string(types::Type(phos::mem::make_rc<types::Model_type>(v->signature)));
        else if constexpr (std::is_same_v<T, mem::rc_ptr<Closure_value>>)
            return types::type_to_string(types::Type(mem::make_rc<types::Function_type>(v->signature)));
        else if constexpr (std::is_same_v<T, mem::rc_ptr<Array_value>>)
            return types::type_to_string(v->type);
        else if constexpr (std::is_same_v<T, mem::rc_ptr<Native_function_value>>)
            return "native_fn<" + v->name + ">";
        else if constexpr (std::is_same_v<T, mem::rc_ptr<Union_value>>)
            return v->tag;
        else if constexpr (std::is_same_v<T, mem::rc_ptr<Green_thread_value>>)
            return "thread";
        else if constexpr (std::is_same_v<T, std::nullptr_t>)
            return "nil";
        else if constexpr (std::is_same_v<T, std::monostate>)
            return "void";
        else
            return "unknown";
    },
    value);
}

}  // namespace phos
