#include "symbol.hpp"

namespace phos {

Symbol_id Symbol_registry::create_symbol(Symbol sym)
{
    Symbol_id id{symbols.size()};
    sym.id = id;
    global_index[sym.name] = id;
    symbols.push_back(std::move(sym));
    return id;
}

Symbol &Symbol_registry::get_symbol(Symbol_id id)
{
    return symbols[id.value];
}

const Symbol &Symbol_registry::get_symbol(Symbol_id id) const
{
    return symbols[id.value];
}

std::optional<Symbol_id> Symbol_registry::lookup_global(String_id name) const
{
    if (auto it = global_index.find(name); it != global_index.end()) {
        return it->second;
    }
    return std::nullopt;
}

std::optional<Symbol_id> Symbol_registry::lookup_global(const std::string_view name) const
{
    return lookup_global(strings.intern(name));
}

} // namespace phos
