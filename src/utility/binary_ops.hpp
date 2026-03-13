#pragma once

#include "../value/value.hpp"
#include "../error/result.hpp"
#include "../parser/ast.hpp"

namespace phos::util
{

// --- Arithmetic ---
inline Result<Value> add_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) + get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) + get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) + get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) + static_cast<double>(get_int(r)));
    if (is_string(l) && is_string(r))
        return Value(get_string(l) + get_string(r));
    return std::unexpected(err::msg("Operands must be two numbers or two strings for '+'.", "vm", loc.l, loc.c));
}

inline Result<Value> subtract_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) - get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) - get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) - get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) - static_cast<double>(get_int(r)));
    return std::unexpected(err::msg("Operands must be numbers for '-'.", "vm", loc.l, loc.c));
}

inline Result<Value> multiply_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) * get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) * get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) * get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) * static_cast<double>(get_int(r)));
    return std::unexpected(err::msg("Operands must be numbers for '*'.", "vm", loc.l, loc.c));
}

inline Result<Value> divide_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
    {
        if (get_int(r) == 0)
            return std::unexpected(err::msg("Division by zero.", "vm", loc.l, loc.c));
        return Value(static_cast<double>(get_int(l)) / static_cast<double>(get_int(r)));
    }
    if (is_float(l) && is_float(r))
        return Value(get_float(l) / get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) / get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) / static_cast<double>(get_int(r)));
    return std::unexpected(err::msg("Operands must be numbers for '/'.", "vm", loc.l, loc.c));
}

inline Result<Value> modulo_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
    {
        int64_t right = get_int(r);
        if (right == 0)
            return std::unexpected(err::msg("Modulo by zero.", "vm", loc.l, loc.c));
        return Value(get_int(l) % right);
    }
    return std::unexpected(err::msg("Operands must be integers for '%'.", "vm", loc.l, loc.c));
}

// --- Relational Operators (Strict Type Matching) ---
inline Result<Value> less_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) < get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) < get_float(r));
    return std::unexpected(err::msg("Operands must be numbers of the exact same type for '<'.", "vm", loc.l, loc.c));
}

inline Result<Value> less_equal_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) <= get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) <= get_float(r));
    return std::unexpected(err::msg("Operands must be numbers of the exact same type for '<='.", "vm", loc.l, loc.c));
}

inline Result<Value> greater_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) > get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) > get_float(r));
    return std::unexpected(err::msg("Operands must be numbers of the exact same type for '>'.", "vm", loc.l, loc.c));
}

inline Result<Value> greater_equal_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) >= get_int(r));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) >= get_float(r));
    return std::unexpected(err::msg("Operands must be numbers of the exact same type for '>='.", "vm", loc.l, loc.c));
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
    if (is_int(l) && is_int(r))
        return Value(get_int(l) & get_int(r));
    return std::unexpected(err::msg("Operands must be integers for '&'.", "vm", loc.l, loc.c));
}

inline Result<Value> bitwise_or_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) | get_int(r));
    return std::unexpected(err::msg("Operands must be integers for '|'.", "vm", loc.l, loc.c));
}

inline Result<Value> bitwise_xor_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) ^ get_int(r));
    return std::unexpected(err::msg("Operands must be integers for '^'.", "vm", loc.l, loc.c));
}

inline Result<Value> bitwise_lshift_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) << get_int(r));
    return std::unexpected(err::msg("Operands must be integers for '<<'.", "vm", loc.l, loc.c));
}

inline Result<Value> bitwise_rshift_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if (is_int(l) && is_int(r))
        return Value(get_int(l) >> get_int(r));
    return std::unexpected(err::msg("Operands must be integers for '>>'.", "vm", loc.l, loc.c));
}

}  // namespace phos::util
