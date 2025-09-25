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
            return true;  // nil == nil, void == void
        }
        else if constexpr (std::is_same_v<T, mem::rc_ptr<Model_value>>||
                           std::is_same_v<T, mem::rc_ptr<Function_value>> ||
                           std::is_same_v<T, mem::rc_ptr<Closure_value>> ||
                           std::is_same_v<T, mem::rc_ptr<Array_value>> ||
                           std::is_same_v<T, mem::rc_ptr<Native_function_value>>)
        {
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
    // --- Primitives ---
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

    // --- Array Printing ---
    if (is_array(value))
    {
        auto arr = get_array(value);
        if (arr->elements.empty())
            return "[]";

        // Heuristic: check if any element is complex or if the array is long
        bool is_complex = arr->elements.size() > 10;
        if (!is_complex)
        {
            for (const auto &el : arr->elements)
            {
                if (is_array(el) || is_model(el))
                {
                    is_complex = true;
                    break;
                }
            }
        }

        std::stringstream ss;
        if (!is_complex)  // Single-line format
        {
            ss << "[";
            bool first = true;
            for (const auto &element : arr->elements)
            {
                if (!first)
                    ss << ", ";
                ss << value_to_string_impl(element, indent_level);
                first = false;
            }
            ss << "]";
        }
        else  // Multi-line format
        {
            std::string base_indent(indent_level * 2, ' ');
            std::string inner_indent((indent_level + 1) * 2, ' ');
            ss << "[\n";
            bool first = true;
            for (const auto &element : arr->elements)
            {
                if (!first)
                    ss << ",\n";
                ss << inner_indent << value_to_string_impl(element, indent_level + 1);
                first = false;
            }
            ss << "\n" << base_indent << "]";
        }
        return ss.str();
    }

    if (is_model(value))
    {
        auto model = get_model(value);
        if (model->fields.empty())
            return model->signature.name + " { }";

        // Heuristic: check if any field is complex or if there are many fields
        bool is_complex = model->fields.size() > 3;
        if (!is_complex)
        {
            for (const auto &[name, field_val] : model->fields)
            {
                if ((is_array(field_val) && get_array(field_val)->elements.size() > 5) || is_model(field_val))
                {
                    is_complex = true;
                    break;
                }
            }
        }

        std::stringstream ss;
        if (!is_complex)  // Single-line format
        {
            ss << model->signature.name << " { ";
            bool first = true;
            for (const auto &[name, field_value] : model->fields)
            {
                if (!first)
                    ss << ", ";
                ss << "." << name << " = " << value_to_string_impl(field_value, indent_level);
                first = false;
            }
            ss << " }";
        }
        else  // Multi-line format
        {
            std::string base_indent(indent_level * 2, ' ');
            std::string inner_indent((indent_level + 1) * 2, ' ');
            ss << model->signature.name << " {\n";
            bool first = true;
            for (const auto &[name, field_value] : model->fields)
            {
                if (!first)
                    ss << ",\n";
                ss << inner_indent << "." << name << " = " << value_to_string_impl(field_value, indent_level + 1);
                first = false;
            }
            ss << "\n" << base_indent << "}";
        }
        return ss.str();
    }

    // --- Functions & Closures (signatures are single-line) ---
    if (is_function(value))
    {
        auto func = get_function(value);
        std::stringstream ss;
        ss << "fn(";
        bool first = true;
        for (const auto &param : func->parameters)
        {
            if (!first)
                ss << ", ";
            ss << param.first << ": " << types::type_to_string(param.second);
            first = false;
        }
        ss << ") -> " << types::type_to_string(func->signature.return_type);
        return ss.str();
    }
    if (is_closure(value))
    {
        auto func = get_closure(value)->function;
        std::stringstream ss;
        ss << "closure fn(";
        bool first = true;
        for (const auto &param : func->parameters)
        {
            if (!first)
                ss << ", ";
            ss << param.first << ": " << types::type_to_string(param.second);
            first = false;
        }
        ss << ") -> " << types::type_to_string(func->signature.return_type);
        return ss.str();
    }

    return "unknown";
}

std::string value_to_string(const Value &value) { return value_to_string_impl(value, 0); }

std::string get_value_type_string(const Value &value)
{
    return std::visit(
        [](const auto &v) -> std::string {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, int64_t>) return "i64";
            else if constexpr (std::is_same_v<T, double>) return "f64";
            else if constexpr (std::is_same_v<T, bool>) return "bool";
            else if constexpr (std::is_same_v<T, std::string>) return "string";
            else if constexpr (std::is_same_v<T, mem::rc_ptr<Model_value>>)
                return types::type_to_string(types::Type(phos::mem::make_rc<types::Model_type>(v->signature)));
            else if constexpr (std::is_same_v<T, mem::rc_ptr<Function_value>>)
                return types::type_to_string(types::Type(mem::make_rc<types::Function_type>(v->signature)));
            else if constexpr (std::is_same_v<T, mem::rc_ptr<Closure_value>>)
                return types::type_to_string(types::Type(mem::make_rc<types::Closure_type>(v->function->signature)));
            else if constexpr (std::is_same_v<T, mem::rc_ptr<Array_value>>)
                return types::type_to_string(v->type);
            else if constexpr (std::is_same_v<T, mem::rc_ptr<Native_function_value>>)
                return "native_fn<" + v->name + ">";
            else if constexpr (std::is_same_v<T, std::nullptr_t>) return "nil";
            else if constexpr (std::is_same_v<T, std::monostate>) return "void";
            else return "unknown";
        },
        value);
}

}  // namespace phos
