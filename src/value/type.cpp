#include "type.hpp"

namespace phos::types
{

bool operator==(const Type& lhs, const Type& rhs)
{
    if (lhs.index() != rhs.index())
        return false;

    return std::visit(
    [&](auto &&l_val) -> bool
    {
        auto &&r_val = std::get<std::decay_t<decltype(l_val)>>(rhs);
        using T = std::decay_t<decltype(l_val)>;

        if constexpr (std::is_same_v<T, Primitive_kind>)
        {
            return l_val == r_val;
        }
        else
        {
            if (!l_val || !r_val)
                return l_val == r_val;
            return *l_val == *r_val;
        }
    },
    lhs);
}

std::string type_to_string(const Type &type)
{
    if (is_primitive(type))
    {
        switch (get_primitive_kind(type))
        {
            case Primitive_kind::Int:    return "i64";
            case Primitive_kind::Float:  return "f64";
            case Primitive_kind::Bool:   return "bool";
            case Primitive_kind::String: return "string";
            case Primitive_kind::Void:   return "void";
            case Primitive_kind::Nil:    return "nil";
            case Primitive_kind::Any:    return "any";
        }
    }
    else if (is_function(type))
    {
        auto func_type = get_function_type(type);
        std::string result = "fn(";
        for (size_t i = 0; i < func_type->parameter_types.size(); i++)
        {
            if (i > 0)
                result += ", ";
            result += type_to_string(func_type->parameter_types[i]);
        }
        result += ") -> " + type_to_string(func_type->return_type);
        return result;
    }
    else if (is_closure(type))
    {
        phos::mem::rc_ptr<Closure_type> closure_type = get_closure_type(type);
        std::string result = "|";
        for (int i = 0; i < closure_type->function_type.parameter_types.size(); i++)
        {
            if (i > 0)
                result += ", ";
            result += type_to_string(closure_type->function_type.parameter_types[i]);
        }
        result += "| -> " + type_to_string(closure_type->function_type.return_type);
        return result;
    }
    else if (is_model(type))
    {
        auto model_type = get_model_type(type);

        std::string result = "model " + model_type->name + " { ";
        for (const auto &[name, member_type] : model_type->fields) result += name + ": " + type_to_string(member_type) + ", ";

        if (!model_type->fields.empty())
            result = result.substr(0, result.size() - 2);

        result += " }";

        return result;
    }
    else if (is_array(type))
    {
        return type_to_string(get_array_type(type)->element_type) + "[]";
    }
    else if (is_optional(type))
    {
        return type_to_string(get_optional_type(type)->base_type) + "?";
    }
    else if (is_union(type))
    {
        auto union_type = get_union_type(type);
        std::string res = "union " + union_type->name + " {";
        for (const auto &[name, mem_type] : union_type->variants) res += name + ": " + type_to_string(mem_type) + ", ";

        if (!union_type->variants.empty())
            res = res.substr(0, res.size() - 2);
        res += " }";
        return res;
    }

    return "unknown";
}

}  // namespace phos::types
