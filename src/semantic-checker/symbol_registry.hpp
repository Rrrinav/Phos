#pragma once

#include "symbol.hpp"

#include <string>
#include <vector>

namespace phos {

class Symbol_registry
{
public:
    // The master lists. Everything in the program lives here.
    std::vector<Symbol> symbols_;
    std::vector<Module_data> modules_;

    // Module Management
    Module_id create_module(std::string os_path, std::string logical_name)
    {
        Module_id id{modules_.size()};
        modules_.push_back(Module_data{.id = id, .os_path = std::move(os_path), .logical_name = std::move(logical_name), .exports = {}});
        return id;
    }

    Module_data &get_module(Module_id id)
    {
        return modules_[static_cast<size_t>(id)];
    }

    Symbol_id create_symbol(Symbol sym)
    {
        Symbol_id id{symbols_.size()};
        sym.id = id;
        symbols_.push_back(std::move(sym));
        return id;
    }

    // C++23 Deducing 'this' (Explicit Object Parameter)
    decltype(auto) get_symbol(this auto &&self, Symbol_id id)
    {
        return self.symbols_[static_cast<size_t>(id)];
    }

    decltype(auto) get_module(this auto &&self, Module_id id)
    {
        return self.modules_[static_cast<size_t>(id)];
    }

    // Registers a symbol as publically accessible from a specific module
    void export_symbol(Module_id mod_id, const std::string &name, Symbol_id sym_id)
    {
        modules_[static_cast<size_t>(mod_id)].exports[name] = sym_id;
    }

    std::optional<Symbol_id> lookup_symbol_by_path(const std::string &full_name)
    {
        auto pos = full_name.find("::");
        if (pos == std::string::npos) {
            return std::nullopt;
        }

        std::string mod_name = full_name.substr(0, pos);
        std::string sym_name = full_name.substr(pos + 2);

        // 1. Find the module by logical name
        for (const auto &mod : modules_) {
            if (mod.logical_name == mod_name) {
                // 2. Use your existing lookup_export logic
                return lookup_export(mod.id, sym_name);
            }
        }
        return std::nullopt;
    }

    // Check if a module exports a specific symbol
    std::optional<Symbol_id> lookup_export(Module_id mod_id, const std::string &name)
    {
        auto &mod = get_module(mod_id);
        if (auto it = mod.exports.find(name); it != mod.exports.end()) {
            return it->second;
        }
        return std::nullopt;
    }
};

} // namespace phos
