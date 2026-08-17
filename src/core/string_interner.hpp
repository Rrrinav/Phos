#pragma once

#include "core/core_types.hpp"

#include <deque>
#include <string>
#include <string_view>
#include <unordered_map>

namespace phos {

// Maps strings to stable integer ids. Storage is a deque: push_back never
// invalidates references to existing elements, so string_views into the
// stored strings (including short-string-optimized buffers) stay valid for
// the interner's lifetime.
class String_interner
{
public:
    String_id intern(std::string_view text)
    {
        if (auto it = map_.find(text); it != map_.end()) {
            return it->second;
        }
        String_id id{strings_.size()};
        strings_.push_back(std::string(text));
        map_.emplace(std::string_view(strings_.back()), id);
        return id;
    }

    std::string_view resolve(String_id id) const
    {
        return strings_[id.value];
    }

private:
    std::unordered_map<std::string_view, String_id> map_;
    std::deque<std::string> strings_;
};

} // namespace phos