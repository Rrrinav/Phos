#pragma once

#include "../value/type.hpp"
#include "symbol.hpp" // Brings in Symbol_id

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace phos {

struct Scope_symbol
{
    types::Type_id type;
    Symbol_id id; // --- NEW: The semantic anchor ---
    bool is_mut;
    int depth;
};

class Scope_tracker
{
public:
    void begin_scope()
    {
        current_depth_++;
        scope_markers_.push_back(locals_.size());
    }

    void end_scope()
    {
        if (scope_markers_.empty()) {
            return;
        }

        size_t marker = scope_markers_.back();
        scope_markers_.pop_back();

        // Pop all variables declared in this scope (O(1) shadowing unwinding)
        while (locals_.size() > marker) {
            const std::string &name = locals_.back();
            symbols_[name].pop_back();

            if (symbols_[name].empty()) {
                symbols_.erase(name);
            }
            locals_.pop_back();
        }
        current_depth_--;
    }

    bool declare(const std::string &name, types::Type_id type, bool is_mut, Symbol_id id)
    {
        auto it = symbols_.find(name);

        // Prevent re-declaration in the exact same scope level
        if (it != symbols_.end() && !it->second.empty() && it->second.back().depth == current_depth_) {
            return false;
        }

        symbols_[name].push_back({type, id, is_mut, current_depth_});
        locals_.push_back(name);
        return true;
    }

    std::optional<Scope_symbol> lookup(const std::string &name) const
    {
        auto it = symbols_.find(name);
        if (it != symbols_.end() && !it->second.empty()) {
            return it->second.back();
        }
        return std::nullopt;
    }

    int current_depth() const
    {
        return current_depth_;
    }

private:
    std::unordered_map<std::string, std::vector<Scope_symbol>> symbols_;
    std::vector<std::string> locals_;
    std::vector<size_t> scope_markers_;
    int current_depth_ = 0;
};

} // namespace phos
