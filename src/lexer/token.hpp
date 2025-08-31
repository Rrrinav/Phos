#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>

#include "../value/value.hpp"

namespace phos {
namespace lex {

enum class TokenType : uint8_t
{
    Integer, Float, String, Bool, Identifier, Let, If, Else, While, For,
    Fn, Return, Print, Plus, Minus, Star, Slash, Percent, Equal, NotEqual,
    Less, Greater, LessEqual, GreaterEqual, Assign, LogicalAnd, LogicalOr,
    LogicalNot, Arrow, LeftParen, RightParen, LeftBrace, RightBrace, Semicolon,
    Comma, Colon, As, Newline, Eof, Invalid
};

struct Token
{
    TokenType type;
    std::string lexeme;
    Value literal;
    size_t line;
    size_t column;
    Token(TokenType t, std::string lex, Value lit, size_t l, size_t c)
        : type(t), lexeme(std::move(lex)), literal(std::move(lit)), line(l), column(c) {}
};

static const std::unordered_map<std::string_view, TokenType> token_keywords = {
    {"let",    TokenType::Let},
    {"if",     TokenType::If},             {"else",      TokenType::Else},
    {"while",  TokenType::While},          {"for",       TokenType::For},
    {"fn",     TokenType::Fn},             {"return",    TokenType::Return},
    {"print",  TokenType::Print},          {"true",      TokenType::Bool},
    {"false",  TokenType::Bool},           {"int",       TokenType::Identifier},
    {"float",  TokenType::Identifier},     {"bool",      TokenType::Identifier},
    {"string", TokenType::Identifier},     {"as",        TokenType::As}
};

} // namespace lex
} // namespace phos
