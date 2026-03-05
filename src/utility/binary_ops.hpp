#pragma once

#include "../value/value.hpp"
#include "../parser/ast.hpp"
#include <expected>

namespace phos::util
{

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
    return std::unexpected(err::msg("Operands must be two numbers or two strings for '+'.", "interpreter", loc.l, loc.c));
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
    return std::unexpected(err::msg("Operands must be numbers for '-'.", "interpreter", loc.l, loc.c));
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
    return std::unexpected(err::msg("Operands must be numbers for '*'.", "interpreter", loc.l, loc.c));
}

inline Result<Value> divide_op(const Value &l, const Value &r, ast::Source_location loc)
{
    if ((is_int(r) && get_int(r) == 0) || (is_float(r) && get_float(r) == 0.0))
        return std::unexpected(err::msg("Division by zero.", "interpreter", loc.l, loc.c));

    if (is_int(l) && is_int(r))
        return Value(static_cast<double>(get_int(l)) / static_cast<double>(get_int(r)));
    if (is_float(l) && is_float(r))
        return Value(get_float(l) / get_float(r));
    if (is_int(l) && is_float(r))
        return Value(static_cast<double>(get_int(l)) / get_float(r));
    if (is_float(l) && is_int(r))
        return Value(get_float(l) / static_cast<double>(get_int(r)));
    return std::unexpected(err::msg("Operands must be numbers for '/'.", "interpreter", loc.l, loc.c));
}

}  // namespace phos::util
