#pragma once

#include "../error/result.hpp"
#include "../parser/ast.hpp"
#include "../value/value.hpp"

namespace phos::util {

inline std::optional<types::Primitive_kind> promote_numeric_kind(types::Primitive_kind left, types::Primitive_kind right)
{
    if (types::is_float_primitive(left) || types::is_float_primitive(right)) {
        if (types::is_float_primitive(left) && types::is_float_primitive(right))
            return types::primitive_bit_width(left) >= types::primitive_bit_width(right) ? left : right;
        return types::is_float_primitive(left) ? left : right;
    }

    bool same_signedness = (types::is_signed_integer_primitive(left) && types::is_signed_integer_primitive(right))
        || (types::is_unsigned_integer_primitive(left) && types::is_unsigned_integer_primitive(right));

    if (!same_signedness)
        return std::nullopt;

    return types::primitive_bit_width(left) >= types::primitive_bit_width(right) ? left : right;
}

template <typename Op>
inline Result<Value>
apply_promoted_numeric_op(const Value &l, const Value &r, ast::Source_location loc, Op &&op, const std::string &message)
{
    if (!is_numeric(l) || !is_numeric(r))
        return std::unexpected(err::msg(message, "vm", loc.l, loc.c));

    auto promoted = promote_numeric_kind(numeric_type_of(l), numeric_type_of(r));
    if (!promoted)
        return std::unexpected(err::msg("Mixed signed and unsigned numeric operands require an explicit cast.", "vm", loc.l, loc.c));

    auto left_cast = cast_numeric_value(l, *promoted);
    auto right_cast = cast_numeric_value(r, *promoted);
    if (!left_cast || !right_cast)
        return std::unexpected(err::msg(message, "vm", loc.l, loc.c));

    return std::visit(
        [&](const auto &lhs, const auto &rhs) -> Result<Value> {
            using L = std::decay_t<decltype(lhs)>;
            using R = std::decay_t<decltype(rhs)>;
            if constexpr (is_numeric_cpp_v<L> && std::is_same_v<L, R>)
                return Value(op(lhs, rhs));
            return std::unexpected(err::msg(message, "vm", loc.l, loc.c));
        },
        *left_cast,
        *right_cast);
}

template <typename Op>
inline Result<Value> apply_integer_op(const Value &l, const Value &r, ast::Source_location loc, Op &&op, const std::string &message)
{
    if (!is_integer(l) || !is_integer(r))
        return std::unexpected(err::msg(message, "vm", loc.l, loc.c));

    auto promoted = promote_numeric_kind(numeric_type_of(l), numeric_type_of(r));
    if (!promoted)
        return std::unexpected(err::msg("Mixed signed and unsigned integer operands require an explicit cast.", "vm", loc.l, loc.c));

    auto left_cast = cast_numeric_value(l, *promoted);
    auto right_cast = cast_numeric_value(r, *promoted);
    if (!left_cast || !right_cast)
        return std::unexpected(err::msg(message, "vm", loc.l, loc.c));

    return std::visit(
        [&](const auto &lhs, const auto &rhs) -> Result<Value> {
            using L = std::decay_t<decltype(lhs)>;
            using R = std::decay_t<decltype(rhs)>;
            if constexpr (is_integer_cpp_v<L> && std::is_same_v<L, R>)
                return Value(op(lhs, rhs));
            return std::unexpected(err::msg(message, "vm", loc.l, loc.c));
        },
        *left_cast,
        *right_cast);
}

// --- Arithmetic ---
inline Result<Value> add_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_string(l) && is_string(r))
        return Value(get_string(l) + get_string(r));
    return apply_promoted_numeric_op(
        l,
        r,
        loc,
        [](auto lhs, auto rhs) { return lhs + rhs; },
        "Operands must be two numbers or two strings for '+'.");
}

inline Result<Value> subtract_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_promoted_numeric_op(l, r, loc, [](auto lhs, auto rhs) { return lhs - rhs; }, "Operands must be numbers for '-'.");
}

inline Result<Value> multiply_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_promoted_numeric_op(l, r, loc, [](auto lhs, auto rhs) { return lhs * rhs; }, "Operands must be numbers for '*'.");
}

inline Result<Value> divide_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (!is_numeric(l) || !is_numeric(r))
        return std::unexpected(err::msg("Operands must be numbers for '/'.", "vm", loc.l, loc.c));

    if (is_float(l) || is_float(r)) {
        auto promoted = promote_numeric_kind(numeric_type_of(l), numeric_type_of(r));
        auto left_cast = cast_numeric_value(l, *promoted);
        auto right_cast = cast_numeric_value(r, *promoted);
        return std::visit(
            [&](const auto &lhs, const auto &rhs) -> Result<Value> {
                using T = std::decay_t<decltype(lhs)>;
                if constexpr (is_float_cpp_v<T> && std::is_same_v<T, std::decay_t<decltype(rhs)>>) {
                    if (rhs == static_cast<T>(0))
                        return std::unexpected(err::msg("Division by zero.", "vm", loc.l, loc.c));
                    return Value(static_cast<T>(lhs / rhs));
                }
                return std::unexpected(err::msg("Operands must be numbers for '/'.", "vm", loc.l, loc.c));
            },
            *left_cast,
            *right_cast);
    }

    auto left_cast = cast_numeric_value(l, types::Primitive_kind::F64);
    auto right_cast = cast_numeric_value(r, types::Primitive_kind::F64);
    double lhs = get_float(*left_cast);
    double rhs = get_float(*right_cast);
    if (rhs == 0.0)
        return std::unexpected(err::msg("Division by zero.", "vm", loc.l, loc.c));
    return Value(lhs / rhs);
}

inline Result<Value> modulo_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (!is_integer(l) || !is_integer(r))
        return std::unexpected(err::msg("Operands must be integers for '%'.", "vm", loc.l, loc.c));

    auto promoted = promote_numeric_kind(numeric_type_of(l), numeric_type_of(r));
    if (!promoted)
        return std::unexpected(err::msg("Mixed signed and unsigned integer modulo requires an explicit cast.", "vm", loc.l, loc.c));

    auto left_cast = cast_numeric_value(l, *promoted);
    auto right_cast = cast_numeric_value(r, *promoted);
    if (!left_cast || !right_cast)
        return std::unexpected(err::msg("Operands must be integers for '%'.", "vm", loc.l, loc.c));

    return std::visit(
        [&](const auto &lhs, const auto &rhs) -> Result<Value> {
            using T = std::decay_t<decltype(lhs)>;
            if constexpr (is_integer_cpp_v<T> && std::is_same_v<T, std::decay_t<decltype(rhs)>>) {
                if (rhs == static_cast<T>(0))
                    return std::unexpected(err::msg("Modulo by zero.", "vm", loc.l, loc.c));
                return Value(static_cast<T>(lhs % rhs));
            }
            return std::unexpected(err::msg("Operands must be integers for '%'.", "vm", loc.l, loc.c));
        },
        *left_cast,
        *right_cast);
}

// --- Relational Operators (Strict Type Matching) ---
inline Result<Value> less_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_promoted_numeric_op(
        l,
        r,
        loc,
        [](auto lhs, auto rhs) { return lhs < rhs; },
        "Operands must be compatible numbers for '<'.");
}

inline Result<Value> less_equal_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_promoted_numeric_op(
        l,
        r,
        loc,
        [](auto lhs, auto rhs) { return lhs <= rhs; },
        "Operands must be compatible numbers for '<='.");
}

inline Result<Value> greater_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_promoted_numeric_op(
        l,
        r,
        loc,
        [](auto lhs, auto rhs) { return lhs > rhs; },
        "Operands must be compatible numbers for '>'.");
}

inline Result<Value> greater_equal_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_promoted_numeric_op(
        l,
        r,
        loc,
        [](auto lhs, auto rhs) { return lhs >= rhs; },
        "Operands must be compatible numbers for '>='.");
}

inline Result<Value> equal_op(const Value &l, const Value &r, ast::Source_location loc)
{
    (void)loc;
    return Value(l == r);
}

inline Result<Value> not_equal_op(const Value &l, const Value &r, ast::Source_location loc)
{
    (void)loc;
    return Value(!(l == r));
}

// --- Bitwise Ops ---
inline Result<Value> bitwise_and_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_integer_op(l, r, loc, [](auto lhs, auto rhs) { return lhs & rhs; }, "Operands must be integers for '&'.");
}

inline Result<Value> bitwise_or_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_integer_op(l, r, loc, [](auto lhs, auto rhs) { return lhs | rhs; }, "Operands must be integers for '|'.");
}

inline Result<Value> bitwise_xor_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_integer_op(l, r, loc, [](auto lhs, auto rhs) { return lhs ^ rhs; }, "Operands must be integers for '^'.");
}

inline Result<Value> bitwise_lshift_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_integer_op(l, r, loc, [](auto lhs, auto rhs) { return lhs << rhs; }, "Operands must be integers for '<<'.");
}

inline Result<Value> bitwise_rshift_op(const Value &l, const Value &r, ast::Source_location loc)
{
    return apply_integer_op(l, r, loc, [](auto lhs, auto rhs) { return lhs >> rhs; }, "Operands must be integers for '>>'.");
}

} // namespace phos::util
