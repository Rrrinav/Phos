#pragma once

#include <cstdint>
#include <vector>
#include <string>
#include<memory>

namespace phos::types {

enum class kind : uint8_t { Int, Float, Bool, String, Void, Function };

enum class primitives : uint8_t { Int, Float, Bool, String, Void, Function };

struct Type
{
    kind kind_;
    std::vector<Type> parameter_types;
    std::unique_ptr<Type> return_type;

    Type(enum kind k) : kind_(k) {}
    Type() : kind_(kind::Void) {}
    Type(enum kind k, std::vector<Type> params, Type ret)
        : kind_(k), parameter_types(std::move(params)), return_type(std::make_unique<Type>(std::move(ret)))
    {
    }
    Type(const Type &other) : kind_(other.kind_), parameter_types(other.parameter_types)
    {
        if (other.return_type)
            return_type = std::make_unique<Type>(*other.return_type);
    }
    Type &operator=(const Type &other)
    {
        if (this != &other)
        {
            kind_ = other.kind_;
            parameter_types = other.parameter_types;
            if (other.return_type)
                return_type = std::make_unique<Type>(*other.return_type);
            else
                return_type.reset();
        }
        return *this;
    }
    bool operator==(const Type &other) const
    {
        if (kind_ != other.kind_)
            return false;
        if (kind_ == kind::Function)
        {
            if (parameter_types.size() != other.parameter_types.size())
                return false;
            for (size_t i = 0; i < parameter_types.size(); i++)
                if (parameter_types[i] != other.parameter_types[i])
                    return false;
            if (return_type && other.return_type)
                return *return_type == *other.return_type;
            return !return_type && !other.return_type;
        }
        return true;
    }
    std::string to_string() const
    {
        switch (kind_)
        {
            case kind::Int:
                return "int";
            case kind::Float:
                return "float";
            case kind::Bool:
                return "bool";
            case kind::String:
                return "string";
            case kind::Void:
                return "void";
            case kind::Function:
            {
                std::string result = "fn(";
                for (size_t i = 0; i < parameter_types.size(); i++)
                {
                    if (i > 0)
                        result += ", ";
                    result += parameter_types[i].to_string();
                }
                result += ") -> " + (return_type ? return_type->to_string() : "void");
                return result;
            }
        }
        return "unknown";
    }
};

} // namespace phos::types
