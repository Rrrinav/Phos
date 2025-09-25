#pragma once

#include "../value/value.hpp"
#include <cstddef>

namespace phos::util
{

// --- Arithmetic Functors ---
struct Add_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
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
        return std::unexpected(err::msg("Operands must be two numbers or two strings for '+'.", "interpreter", line, column));
    }
};

struct Subtract_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) - get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) - get_float(r));
        if (is_int(l) && is_float(r))
            return Value(static_cast<double>(get_int(l)) - get_float(r));
        if (is_float(l) && is_int(r))
            return Value(get_float(l) - static_cast<double>(get_int(r)));
        return std::unexpected(err::msg("Operands must be numbers for '-'.", "interpreter", line, column));
    }
};

struct Multiply_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) * get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) * get_float(r));
        if (is_int(l) && is_float(r))
            return Value(static_cast<double>(get_int(l)) * get_float(r));
        if (is_float(l) && is_int(r))
            return Value(get_float(l) * static_cast<double>(get_int(r)));
        return std::unexpected(err::msg("Operands must be numbers for '*'.", "interpreter", line, column));
    }
};

struct Divide_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if ((is_int(r) && get_int(r) == 0) || (is_float(r) && get_float(r) == 0.0))
            return std::unexpected(err::msg("Division by zero.", "interpreter", line, column));

        if (is_int(l) && is_int(r))
            return Value(static_cast<double>(get_int(l)) / static_cast<double>(get_int(r)));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) / get_float(r));
        if (is_int(l) && is_float(r))
            return Value(static_cast<double>(get_int(l)) / get_float(r));
        if (is_float(l) && is_int(r))
            return Value(get_float(l) / static_cast<double>(get_int(r)));
        return std::unexpected(err::msg("Operands must be numbers for '/'.", "interpreter", line, column));
    }
};

struct Modulo_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (!is_int(l) || !is_int(r))
            return std::unexpected(err::msg("Operands must be integers for '%'.", "interpreter", line, column));
        if (get_int(r) == 0)
            return std::unexpected(err::msg("Division by zero.", "interpreter", line, column));
        return Value(get_int(l) % get_int(r));
    }
};

// --- Comparison Functors ---

struct Less_than_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) < get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) < get_float(r));
        return std::unexpected(err::msg("Operands must be numbers of the same type for '<'.", "interpreter", line, column));
    }
};

struct Less_than_or_equal_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) <= get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) <= get_float(r));
        return std::unexpected(err::msg("Operands must be numbers of the same type for '<='.", "interpreter", line, column));
    }
};

struct Greate_than_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) > get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) > get_float(r));
        return std::unexpected(err::msg("Operands must be numbers of the same type for '>'.", "interpreter", line, column));
    }
};

struct Greater_than_or_equal_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (is_int(l) && is_int(r))
            return Value(get_int(l) >= get_int(r));
        if (is_float(l) && is_float(r))
            return Value(get_float(l) >= get_float(r));
        return std::unexpected(err::msg("Operands must be numbers of the same type for '>='.", "interpreter", line, column));
    }
};

// --- Equality Functors ---

struct Equals_op
{
    Result<Value> operator()(const Value &l, const Value &r,std::size_t line, std::size_t column)
    {
        return Value(l == r);  // Reuses the global operator== for Value
    }
};

struct Not_equals_op
{
    Result<Value> operator()(const Value &l, const Value &r,std::size_t line, std::size_t column)
    {
        return Value(!(l == r));  // Reuses the global operator== for Value
    }
};

// --- Logical Functors ---

struct Logical_and_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (!is_bool(l) || !is_bool(r))
            return std::unexpected(err::msg("Operands for '&&' must be booleans.", "interpreter", line, column));
        return Value(get_bool(l) && get_bool(r));
    }
};

struct Logical_or_op
{
    Result<Value> operator()(const Value &l, const Value &r, std::size_t line, std::size_t column) const
    {
        if (!is_bool(l) || !is_bool(r))
            return std::unexpected(err::msg("Operands for '||' must be booleans.", "interpreter", line, column));
        return Value(get_bool(l) || get_bool(r));
    }
};

}  // namespace phos
