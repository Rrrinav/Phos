#pragma once

#include "../value/type.hpp"
#include "../value/value.hpp"

#include <functional>
#include <optional>
#include <string>
#include <unordered_map>

namespace phos {

// Zero-cost strong ID wrapper
template <typename Tag>
struct Strong_id
{
    size_t value;

    auto operator<=>(const Strong_id &) const = default;
    explicit constexpr operator size_t() const
    {
        return value;
    }
};

struct Symbol_tag
{};
struct Module_tag
{};

// The actual strongly typed IDs
using Symbol_id = Strong_id<Symbol_tag>;
using Module_id = Strong_id<Module_tag>;

enum class Symbol_kind { Local_var, Global_var, Native_func, Native_const, Phos_func, Model_def, Enum_def, Union_def };

struct Symbol
{
    Symbol_id id;
    std::string name;
    Symbol_kind kind;
    types::Type_id type;

    Module_id owner_module;
    bool is_public;

    std::optional<Value> const_value;
    std::optional<uint16_t> stack_offset;
    std::optional<size_t> ffi_index;
};

struct Module_data
{
    Module_id id;
    std::string os_path;
    std::string logical_name;

    std::unordered_map<std::string, Symbol_id> exports;
};

} // namespace phos

// Inject into std namespace so these IDs can be used as keys in unordered_maps
template <typename Tag>
struct std::hash<phos::Strong_id<Tag>>
{
    size_t operator()(const phos::Strong_id<Tag> &id) const noexcept
    {
        return std::hash<size_t>{}(id.value);
    }
};

#include "symbol.hpp"

#include <format>
#include <string>

namespace std {

// -------------------------------------
// Strong IDs
// -------------------------------------
template <typename Tag>
struct formatter<phos::Strong_id<Tag>, char>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(
        const phos::Strong_id<Tag> &id,
        FormatContext &ctx
    ) const
    {
        return std::format_to(
            ctx.out(),
            "{}",
            id.value
        );
    }
};

// -------------------------------------
// Symbol_kind
// -------------------------------------
template <>
struct formatter<phos::Symbol_kind, char>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(
        const phos::Symbol_kind &kind,
        FormatContext &ctx
    ) const
    {
        std::string_view text = "unknown";

        switch (kind) {
        case phos::Symbol_kind::Local_var:
            text = "Local_var";
            break;
        case phos::Symbol_kind::Global_var:
            text = "Global_var";
            break;
        case phos::Symbol_kind::Native_func:
            text = "Native_func";
            break;
        case phos::Symbol_kind::Native_const:
            text = "Native_const";
            break;
        case phos::Symbol_kind::Phos_func:
            text = "Phos_func";
            break;
        case phos::Symbol_kind::Model_def:
            text = "Model_def";
            break;
        case phos::Symbol_kind::Enum_def:
            text = "Enum_def";
            break;
        case phos::Symbol_kind::Union_def:
            text = "Union_def";
            break;
        }

        return std::format_to(
            ctx.out(),
            "{}",
            text
        );
    }
};

// -------------------------------------
// Symbol
// -------------------------------------
template <>
struct formatter<phos::Symbol, char>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(
        const phos::Symbol &s,
        FormatContext &ctx
    ) const
    {
        return std::format_to(
            ctx.out(),
            "{{id: {}, name: \"{}\", kind: {}, type: {}, owner_module: {}, public: {}}}",
            s.id,
            s.name,
            s.kind,
            s.type,
            s.owner_module,
            s.is_public
        );
    }
};

// -------------------------------------
// Module_data
// -------------------------------------
template <>
struct formatter<phos::Module_data, char>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(
        const phos::Module_data &m,
        FormatContext &ctx
    ) const
    {
        return std::format_to(
            ctx.out(),
            "{{id: {}, os_path: \"{}\", logical_name: \"{}\", exports: {} symbols}}",
            m.id,
            m.os_path,
            m.logical_name,
            m.exports.size()
        );
    }
};

} // namespace std
