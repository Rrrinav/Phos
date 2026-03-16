#pragma once

#include <unordered_map>
#include <expected>

#include "../value/value.hpp"
#include "../error/err.hpp"

namespace phos {

class Environment
{
private:
    std::unordered_map<std::string, Value> values;
    mem::rc_ptr<Environment> enclosing;

public:
    Environment() : enclosing(nullptr) {}
    Environment(mem::rc_ptr<Environment> enclosing) : enclosing(std::move(enclosing)) {}

    Result<void> define(const std::string &name, const Value &value)
    {
        values[name] = value;
        return {};
    }

    Result<Value> get(const std::string &name) const
    {
        if (values.contains(name))
            return values.at(name);
        if (enclosing != nullptr)
            return enclosing->get(name);
        return std::unexpected(err::msg("Undefined variable '" + name + "'", "interpreter", 0, 0));
    }

    Result<void> assign(const std::string &name, const Value &value)
    {
        if (values.contains(name))
        {
            values[name] = value;
            return {};
        }
        if (enclosing != nullptr)
            return enclosing->assign(name, value);
        return std::unexpected(err::msg("Undefined variable '" + name + "'", "interpreter", 0, 0));
    }
};

} //namespace phos 
