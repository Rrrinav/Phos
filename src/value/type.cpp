#include "type.hpp"
#include <sstream>

namespace phos::types
{

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
        std::shared_ptr<Closure_type> closure_type = get_closure_type(type);
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

    return "unknown";
}

}  // namespace phos::types
