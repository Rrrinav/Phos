#pragma once

#include <unordered_map>
#include <string>

#include "../value/value.hpp"

namespace phos {

struct Environment
{
    std::unordered_map<std::string, Value> vars;
    Environment *parent;

    Environment(Environment *parent_ = nullptr) : parent(parent_) {}

    Value *lookup(const std::string &name)
    {
        auto it = vars.find(name);
        if (it != vars.end())
            return &it->second;
        if (parent)
            return parent->lookup(name);
        return nullptr;
    }

    void define(const std::string &name, Value val) { vars[name] = std::move(val); }

    bool assign(const std::string &name, Value val)
    {
        auto it = vars.find(name);
        if (it != vars.end())
        {
            it->second = std::move(val);
            return true;
        }
        if (parent)
            return parent->assign(name, val);
        return false;
    }
};

} // namespace phos
