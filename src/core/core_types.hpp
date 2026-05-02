#pragma once

#include <cstddef>
#include <format>
#include <functional>

namespace phos {

namespace detail {

template <typename Tag>
struct Strong_id
{
    size_t value;

    auto operator<=>(const Strong_id &) const = default;
    explicit constexpr operator size_t() const
    {
        return value;
    }

    bool is_null() const
    {
        return value == static_cast<size_t>(-1);
    }
    static Strong_id null()
    {
        return Strong_id{static_cast<size_t>(-1)};
    }

    operator bool() const {
        return is_null();
    }
};

} // namespace detail

struct Symbol_tag
{};
struct Module_tag
{};

using Symbol_id = detail::Strong_id<Symbol_tag>;
using Module_id = detail::Strong_id<Module_tag>;

namespace ast {
struct Expr_tag
{};
struct Stmt_tag
{};
using Expr_id = detail::Strong_id<Expr_tag>;
using Stmt_id = detail::Strong_id<Stmt_tag>;
} // namespace ast

} // namespace phos

namespace std {

template <typename Tag>
struct hash<phos::detail::Strong_id<Tag>>
{
    size_t operator()(const phos::detail::Strong_id<Tag> &id) const noexcept
    {
        return hash<size_t>{}(id.value);
    }
};

template <typename Tag>
struct formatter<phos::detail::Strong_id<Tag>, char>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const phos::detail::Strong_id<Tag> &id, FormatContext &ctx) const
    {
        if (id.is_null()) {
            return format_to(ctx.out(), "null");
        }
        return format_to(ctx.out(), "{}", id.value);
    }
};

} // namespace std
