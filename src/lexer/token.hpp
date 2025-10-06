#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>

#include "../value/value.hpp"

namespace phos {
namespace lex {

enum class TokenType : uint8_t
{
    Integer64, Float64, String, Bool, Array, Identifier, Let, Const, Nil, If, Else, While, For,
    Fn, Return, Print, PrintErr, Plus, Minus, Star, Slash, Percent, Equal, NotEqual, Static,
    Less, Greater, LessEqual, GreaterEqual, Assign, LogicalAnd, LogicalOr, ColonColon,
    LogicalNot, Arrow, LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket, Semicolon,
    Model, Union, Pipe, Dot, Comma, Colon, Question, As, This, Newline, Eof, Invalid
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
    {"let",    TokenType::Let},            {"print_err", TokenType::PrintErr},
    {"if",     TokenType::If},             {"else",      TokenType::Else},
    {"while",  TokenType::While},          {"for",       TokenType::For},
    {"fn",     TokenType::Fn},             {"return",    TokenType::Return},
    {"print",  TokenType::Print},          {"true",      TokenType::Bool},
    {"false",  TokenType::Bool},           {"i64",       TokenType::Identifier},
    {"f64",    TokenType::Identifier},     {"bool",      TokenType::Identifier},
    {"string", TokenType::Identifier},     {"as",        TokenType::As},
    {"arr",    TokenType::Array},          {"model",     TokenType::Model},
    {"this",   TokenType::This},           {"const",     TokenType::Const},
    {"nil",    TokenType::Nil},            {"union",     TokenType::Union},
    {"static", TokenType::Static}
};

} // namespace lex
} // namespace phos
