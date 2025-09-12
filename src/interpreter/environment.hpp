#pragma once

#include <unordered_map>
#include <memory>
#include <string>
#include "../value/value.hpp"

namespace phos
{

struct Environment : public std::enable_shared_from_this<Environment>
{
    std::unordered_map<std::string, Value> variables;
    std::shared_ptr<Environment> parent;

    Environment(std::shared_ptr<Environment> p = nullptr) : parent(std::move(p)) {}

    bool define(const std::string &name, Value value)
    {
        if (variables.find(name) != variables.end())
            return false;
        variables[name] = std::move(value);
        return true;
    }

    bool assign(const std::string &name, Value value)
    {
        if (variables.find(name) != variables.end())
        {
            variables[name] = std::move(value);
            return true;
        }
        if (parent)
            return parent->assign(name, std::move(value));
        return false;
    }

    Value *lookup(const std::string &name)
    {
        auto it = variables.find(name);
        if (it != variables.end())
            return &it->second;
        if (parent)
            return parent->lookup(name);
        return nullptr;
    }

    std::shared_ptr<Environment> create_child() { return std::make_shared<Environment>(shared_from_this()); }

    // Capture current environment state for closures
    std::unordered_map<std::string, Value> capture_environment() const
    {
        std::unordered_map<std::string, Value> captured;
        for (const auto &[name, value] : variables) captured[name] = value;
        return captured;
    }
};

}  // namespace phos
