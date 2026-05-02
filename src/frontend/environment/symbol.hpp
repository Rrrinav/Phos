#pragma once

#include "core/core_types.hpp"
#include "core/value/type.hpp"
#include "core/value/value.hpp"

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace phos {

enum class Symbol_kind { Local_var, Global_var, Native_func, Native_const, Phos_func, Model_def, Enum_def, Union_def };

struct Symbol
{
    Symbol_id id;
    std::string name; // The canonical name (e.g., "std::math::sin")
    Symbol_kind kind;
    types::Type_id type;

    Module_id owner_module;
    bool is_public;

    std::optional<Value> const_value;
    std::optional<uint16_t> stack_offset;
    std::optional<size_t> ffi_index;

    ast::Stmt_id declaration = ast::Stmt_id::null();
};

class Symbol_registry
{
public:
    std::vector<Symbol> symbols;
    std::unordered_map<std::string, Symbol_id> global_index;

    Symbol_id create_symbol(Symbol sym);
    Symbol &get_symbol(Symbol_id id);
    const Symbol &get_symbol(Symbol_id id) const;
    std::optional<Symbol_id> lookup_global(const std::string &canonical_name) const;
};

} // namespace phos

namespace std {

template <>
struct formatter<phos::Symbol_kind, char>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const phos::Symbol_kind &kind, FormatContext &ctx) const
    {
        string_view text = "unknown";
        switch (kind) {
        case phos::Symbol_kind::Local_var:    text = "Local_var";    break;
        case phos::Symbol_kind::Global_var:   text = "Global_var";   break;
        case phos::Symbol_kind::Native_func:  text = "Native_func";  break;
        case phos::Symbol_kind::Native_const: text = "Native_const"; break;
        case phos::Symbol_kind::Phos_func:    text = "Phos_func";    break;
        case phos::Symbol_kind::Model_def:    text = "Model_def";    break;
        case phos::Symbol_kind::Enum_def:     text = "Enum_def";     break;
        case phos::Symbol_kind::Union_def:    text = "Union_def";    break;
        }
        return format_to(ctx.out(), "{}", text);
    }
};

} // namespace std
