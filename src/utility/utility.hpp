#pragma once

#include "../lexer/token.hpp"
#include "../error/result.hpp"

#include <expected>
#include <concepts>
#include <string>
#include <type_traits>
#include <variant>

namespace util {

inline std::string operator_token_to_string(phos::lex::TokenType type)
{
    static const std::unordered_map<phos::lex::TokenType, std::string> token_names = {
        {phos::lex::TokenType::Plus, "+"},       {phos::lex::TokenType::Minus, "-"},      {phos::lex::TokenType::Star, "*"},          {phos::lex::TokenType::Slash, "/"},
        {phos::lex::TokenType::Percent, "%"},    {phos::lex::TokenType::Equal, "=="},     {phos::lex::TokenType::NotEqual, "!="},     {phos::lex::TokenType::Less, "<"},
        {phos::lex::TokenType::Greater, ">"},    {phos::lex::TokenType::LessEqual, "<="}, {phos::lex::TokenType::GreaterEqual, ">="}, {phos::lex::TokenType::LogicalAnd, "&&"},
        {phos::lex::TokenType::LogicalOr, "||"}, {phos::lex::TokenType::LogicalNot, "!"}};
    auto it = token_names.find(type);
    return it != token_names.end() ? it->second : "unknown";
}

inline std::string value_to_string(const phos::Value &value)
{
    return std::visit(
        [](const auto &v) -> std::string {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, int64_t>)
                return std::to_string(v);
            else if constexpr (std::is_same_v<T, double>)
                return std::to_string(v);
            else if constexpr (std::is_same_v<T, bool>)
                return v ? "true" : "false";
            else if constexpr (std::is_same_v<T, std::string>)
                return "\"" + v + "\"";
            else if constexpr (std::is_same_v<T, std::monostate>)
                return "void";
            else
                return "unknown";
        },
    value);
}

#define invalid_oper(op, operand) "Invalid operands (" operand ") for operator '" op "'"

template <typename T>
concept AllowedType = std::is_same_v<T, std::string> || std::is_same_v<T, int64_t> || std::is_same_v<T, bool> || std::is_same_v<T, double>;

template <AllowedType T>
struct BinaryOperation
{
    T left;
    T right;
    phos::lex::TokenType op;
    size_t line;
    size_t column;

    BinaryOperation(T l, T r, phos::lex::TokenType o, size_t ln, size_t col) : left(std::move(l)), right(std::move(r)), op(o), line(ln), column(col) {}

    phos::Result<phos::Value> execute() const
    {
        using ThisT = std::decay_t<decltype(left)>;

        switch (op)
        {
            case phos::lex::TokenType::Plus:
                return left + right;

            case phos::lex::TokenType::Minus:
                if constexpr (std::is_same_v<ThisT, std::string>)
                    return std::unexpected(phos::err::msg(invalid_oper("*", "string"), "runtime", line, column));
                else if constexpr (std::is_same_v<ThisT, bool>)
                    return std::unexpected(phos::err::msg(invalid_oper("*", "bool"), "runtime", line, column));
                else return left - right;

            case phos::lex::TokenType::Star:
                if constexpr (std::is_same_v<ThisT, std::string>)
                    return std::unexpected(phos::err::msg(invalid_oper("*", "string"), "runtime", line, column));
                else if constexpr (std::is_same_v<ThisT, bool>)
                    return std::unexpected(phos::err::msg(invalid_oper("*", "bool"), "runtime", line, column));
                else
                    return left * right;

            case phos::lex::TokenType::Slash:
                if constexpr (std::is_same_v<ThisT, std::string>)
                    return std::unexpected(phos::err::msg(invalid_oper("/", "string"), "runtime", line, column));
                else if constexpr (std::is_same_v<ThisT, bool>)
                    return std::unexpected(phos::err::msg(invalid_oper("/", "bool"), "runtime", line, column));
                else if (right == 0 || right == 0.0)
                    return std::unexpected(phos::err::msg("Division by zero", "runtime", line, column));
                else
                    return left / right;

            case phos::lex::TokenType::Percent:
                if constexpr (std::is_same_v<ThisT, std::string>)
                    return std::unexpected(phos::err::msg(invalid_oper("%", "string"), "runtime", line, column));
                else if constexpr (std::is_same_v<ThisT, bool>)
                    return std::unexpected(phos::err::msg(invalid_oper("%", "bool"), "runtime", line, column));
                else if constexpr (std::is_same_v<ThisT, double>)
                    return std::unexpected(phos::err::msg(invalid_oper("%", "double"), "runtime", line, column));
                else if (right == 0)
                    return std::unexpected(phos::err::msg("Modulo by zero", "runtime", line, column));
                else
                    return left % right;

            case phos::lex::TokenType::Equal:
                return left == right;

            case phos::lex::TokenType::NotEqual:
                return left != right;

            case phos::lex::TokenType::Less:
                if constexpr (std::is_same_v<ThisT, bool>)
                    return std::unexpected(phos::err::msg(invalid_oper("<", "bool"), "runtime", line, column));
                else
                    return left < right;

            case phos::lex::TokenType::LessEqual:
                if constexpr (std::is_same_v<ThisT, bool>)
                    return std::unexpected(phos::err::msg(invalid_oper("<=", "bool"), "runtime", line, column));
                else
                    return left <= right;

            case phos::lex::TokenType::Greater:
                if constexpr (std::is_same_v<ThisT, bool>)
                    return std::unexpected(phos::err::msg(invalid_oper(">", "bool"), "runtime", line, column));
                else
                    return left > right;

            case phos::lex::TokenType::GreaterEqual:
                if constexpr (std::is_same_v<ThisT, bool>)
                    return std::unexpected(phos::err::msg(invalid_oper(">=", "bool"), "runtime", line, column));
                else
                    return left >= right;

            case phos::lex::TokenType::LogicalAnd:
                if constexpr (std::is_same_v<ThisT, std::string>)
                    return std::unexpected(phos::err::msg(invalid_oper("&&", "string"), "runtime", line, column));
                else
                    return left && right;

            case phos::lex::TokenType::LogicalOr:
                if constexpr (std::is_same_v<ThisT, std::string>)
                    return std::unexpected(phos::err::msg(invalid_oper("||", "string"), "runtime", line, column));
                else
                    return left || right;
            default:
                return std::unexpected(phos::err::msg("Unknown binary operator", "runtime", line, column));
        }
    }
};

} // namespace util
