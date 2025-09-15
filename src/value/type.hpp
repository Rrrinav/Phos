#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <variant>

namespace phos::types
{

enum class Primitive_kind : uint8_t
{
    Int,
    Float,
    Bool,
    String,
    Void,
    Any
};

// Forward declarations
struct Function_type;
struct Closure_type;
struct Model_type;
struct Array_type;

// Modern type representation using variant
using Type = std::variant<Primitive_kind, std::shared_ptr<Function_type>, std::shared_ptr<Closure_type>, std::shared_ptr<Model_type>,
                          std::shared_ptr<Array_type>>;

struct Function_type
{
    std::vector<Type> parameter_types;
    Type return_type;

    bool operator==(const Function_type &other) const
    {
        if (parameter_types.size() != other.parameter_types.size())
            return false;
        for (size_t i = 0; i < parameter_types.size(); i++)
            if (parameter_types[i] != other.parameter_types[i])
                return false;
        return return_type == other.return_type;
    }
};

struct Closure_type
{
    Function_type function_type;
    std::unordered_map<std::string, Type> captured_variables;

    bool operator==(const Closure_type &other) const
    {
        return function_type == other.function_type && captured_variables == other.captured_variables;
    }
};

struct Model_type
{
    std::string name;
    std::unordered_map<std::string, Type> fields;
    std::unordered_map<std::string, Function_type> methods;

    bool operator==(const Model_type &other) const { return name == other.name && fields == other.fields && methods == other.methods; }
};

struct Array_type
{
    types::Type type;
};

// Helper functions for type manipulation
inline bool is_primitive(const Type &type)                                { return std::holds_alternative<Primitive_kind>(type); }

inline Primitive_kind get_primitive_kind(const Type &type)                { return std::get<Primitive_kind>(type); }

inline bool is_function(const Type &type)                                 { return std::holds_alternative<std::shared_ptr<Function_type>>(type); }

inline std::shared_ptr<Function_type> get_function_type(const Type &type) { return std::get<std::shared_ptr<Function_type>>(type); }

inline bool is_closure(const Type &type)                                  { return std::holds_alternative<std::shared_ptr<Closure_type>>(type); }

inline std::shared_ptr<Closure_type> get_closure_type(const Type &type)   { return std::get<std::shared_ptr<Closure_type>>(type); }

inline bool is_model(const Type &type)                                    { return std::holds_alternative<std::shared_ptr<Model_type>>(type); }

inline std::shared_ptr<Model_type> get_model_type(const Type &type)       { return std::get<std::shared_ptr<Model_type>>(type); }

inline bool is_array(const Type &type)                                    { return std::holds_alternative<std::shared_ptr<Array_type>>(type); }

inline std::shared_ptr<Array_type> get_array_type(const Type &type)       { return std::get<std::shared_ptr<Array_type>>(type); }

// Type conversion to string
std::string type_to_string(const Type &type)
{
    if (is_primitive(type))
    {
        switch (get_primitive_kind(type))
        {
            case Primitive_kind::Int:
                return "i64";
            case Primitive_kind::Float:
                return "f64";
            case Primitive_kind::Bool:
                return "bool";
            case Primitive_kind::String:
                return "string";
            case Primitive_kind::Void:
                return "void";
            case Primitive_kind::Any:
                return "any";
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
        std::string result = "cl|";
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
        return types::type_to_string(get_array_type(type)->type) + "[]";
    }

    return "unknown";
}

// Helper function to create primitive types
inline Type make_int_type() { return Primitive_kind::Int; }
inline Type make_float_type() { return Primitive_kind::Float; }
inline Type make_bool_type() { return Primitive_kind::Bool; }
inline Type make_string_type() { return Primitive_kind::String; }
inline Type make_void_type() { return Primitive_kind::Void; }

}  // namespace phos::types
