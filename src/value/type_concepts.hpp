#pragma once

#include "../lexer/token.hpp"
#include "../value/value.hpp"
#include <concepts>

namespace phos
{

template<lex::TokenType T>
concept is_bianry_op = (
    T == lex::TokenType::Plus         ||
    T == lex::TokenType::Minus        ||
    T == lex::TokenType::Star         ||
    T == lex::TokenType::Slash        ||
    T == lex::TokenType::Percent      ||
    T == lex::TokenType::Less         ||
    T == lex::TokenType::LessEqual    ||
    T == lex::TokenType::Greater      ||
    T == lex::TokenType::GreaterEqual ||
    T == lex::TokenType::Equal        ||
    T == lex::TokenType::NotEqual     ||
    T == lex::TokenType::LogicalAnd   ||
    T == lex::TokenType::LogicalOr
);

// --- Operator Concepts (as you designed) ---
template <lex::TokenType T>
concept is_arithmetic_op = (
    T == lex::TokenType::Plus  ||
    T == lex::TokenType::Minus ||
    T == lex::TokenType::Star  ||
    T == lex::TokenType::Slash ||
    T == lex::TokenType::Percent
);

template <lex::TokenType T>
concept is_comparison_op = (
    T == lex::TokenType::Less         ||
    T == lex::TokenType::LessEqual    ||
    T == lex::TokenType::Greater      ||
    T == lex::TokenType::GreaterEqual
);

template <lex::TokenType T>
concept is_equality_op = (T == lex::TokenType::Equal || T == lex::TokenType::NotEqual);

template <lex::TokenType T>
concept is_logical_op = (T == lex::TokenType::LogicalAnd || T == lex::TokenType::LogicalOr);

// --- Value Type Concepts (your brilliant new idea!) ---
template <typename L, typename R>
concept are_numeric = (is_int(L{}) && is_int(R{})) || (is_numeric(L{}) && is_numeric(R{}));

template <typename L, typename R>
concept are_addable = are_numeric<L, R> || (is_string(L{}) && is_string(R{}));

template <typename L, typename R>
concept are_comparable = are_numeric<L, R>;

template <typename L, typename R>
concept are_equatable = true;  // For now, any two types can be compared for equality

template <typename L, typename R>
concept are_logical = is_bool(L{}) && is_bool(R{});

}  // namespace phos
