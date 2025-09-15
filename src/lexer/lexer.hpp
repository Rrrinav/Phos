#pragma once

#include "./token.hpp"

#include <string>
#include <vector>
#include <optional>
#include <format>

namespace phos {
namespace lex {

class Lexer
{
private:
    std::string_view source;
    size_t current = 0;
    size_t line = 1, column = 1;
    const std::unordered_map<std::string_view, TokenType> keywords = token_keywords;

public:
    explicit Lexer(std::string_view src) : source(src) {}

    std::vector<Token> tokenize()
    {
        std::vector<Token> tokens;
        while (!is_at_end())
            if (auto token = scan_token(); (token.has_value() && token.value().type != TokenType::Newline))
                tokens.push_back(std::move(*token));
        tokens.emplace_back(TokenType::Eof, "", 0, line, column);
        return tokens;
    }

private:
    bool is_at_end() const { return current >= source.length(); }
    char advance()
    {
        column++;
        return source[current++];
    }
    char peek() const { return is_at_end() ? '\0' : source[current]; }
    char peek_next() const { return current + 1 >= source.length() ? '\0' : source[current + 1]; }
    bool match(char expected)
    {
        if (is_at_end() || source[current] != expected)
            return false;
        current++;
        column++;
        return true;
    }

    std::optional<Token> scan_token()
    {
        size_t start_col = column;
        char c = advance();
        switch (c)
        {
            case ' ':
            case '\r':
            case '\t':
                return std::nullopt;
            case '\n':
                line++;
                column = 1;
                return Token(TokenType::Newline, "\n", 0, line - 1, start_col);
            case '.':
                return Token(TokenType::Dot, ".", 0, line, start_col);
            case '[':
                return Token(TokenType::LeftBracket, "[", 0, line, start_col);
            case ']':
                return Token(TokenType::RightBracket, "]", 0, line, start_col);
            case '(':
                return Token(TokenType::LeftParen, "(", 0, line, start_col);
            case ')':
                return Token(TokenType::RightParen, ")", 0, line, start_col);
            case '{':
                return Token(TokenType::LeftBrace, "{", 0, line, start_col);
            case '}':
                return Token(TokenType::RightBrace, "}", 0, line, start_col);
            case ';':
                return Token(TokenType::Semicolon, ";", 0, line, start_col);
            case ',':
                return Token(TokenType::Comma, ",", 0, line, start_col);
            case ':':
                return Token(TokenType::Colon, ":", 0, line, start_col);
            case '+':
                return Token(TokenType::Plus, "+", 0, line, start_col);
            case '-':
                if (match('>'))
                    return Token(TokenType::Arrow, "->", 0, line, start_col);
                return Token(TokenType::Minus, "-", 0, line, start_col);
            case '*':
                return Token(TokenType::Star, "*", 0, line, start_col);
            case '%':
                return Token(TokenType::Percent, "%", 0, line, start_col);
            case '!':
                if (match('='))
                    return Token(TokenType::NotEqual, "!=", 0, line, start_col);
                return Token(TokenType::LogicalNot, "!", 0, line, start_col);
            case '=':
                if (match('='))
                    return Token(TokenType::Equal, "==", 0, line, start_col);
                return Token(TokenType::Assign, "=", 0, line, start_col);
            case '<':
                if (match('='))
                    return Token(TokenType::LessEqual, "<=", 0, line, start_col);
                return Token(TokenType::Less, "<", 0, line, start_col);
            case '>':
                if (match('='))
                    return Token(TokenType::GreaterEqual, ">=", 0, line, start_col);
                return Token(TokenType::Greater, ">", 0, line, start_col);
            case '&':
                if (match('&'))
                    return Token(TokenType::LogicalAnd, "&&", 0, line, start_col);
                break;
            case '|':
                if (match('|'))
                    return Token(TokenType::LogicalOr, "||", 0, line, start_col);
                else return Token(TokenType::Pipe, "|", 0, line, start_col);
                break;
            case '"':
                return scan_string();
            case '/':
                if (match('/'))
                {
                    // single-line comment: skip until newline or end
                    while (peek() != '\n' && !is_at_end()) advance();
                    return std::nullopt;
                }
                else if (match('*'))
                {
                    // block comment: skip until */
                    while (!(peek() == '*' && peek_next() == '/') && !is_at_end())
                    {
                        if (peek() == '\n')
                        {
                            line++;
                            column = 1;
                        }
                        advance();
                    }
                    if (!is_at_end())
                    {
                        advance();  // consume '*'
                        advance();  // consume '/'
                    }
                    return std::nullopt;
                }
                else
                {
                    return Token(TokenType::Slash, "/", 0, line, start_col);
                }

            default:
                if (std::isdigit(c))
                    return scan_number();
                if (std::isalpha(c) || c == '_')
                    return scan_identifier();
                break;
        }
        return Token(TokenType::Invalid, std::string(1, c), 0, line, start_col);
    }

    Token scan_string()
    {
        size_t start_col = column - 1;
        std::string value;
        while (peek() != '"' && !is_at_end())
        {
            if (peek() == '\n')
            {
                line++;
                column = 0;
            }
            value += advance();
        }
        if (is_at_end())
            return Token(TokenType::Invalid, "Unterminated string", value, line, start_col);
        advance();
        return Token(TokenType::String, std::format("\"{}\"", value), std::move(value), line, start_col);
    }

    Token scan_number()
    {
        size_t start = current - 1, start_col = column - 1;
        while (std::isdigit(peek())) advance();
        bool is_float = false;
        if (peek() == '.' && std::isdigit(peek_next()))
        {
            is_float = true;
            advance();
            while (std::isdigit(peek())) advance();
        }
        std::string lexeme(source.substr(start, current - start));
        if (is_float)
            return Token(TokenType::Float64, lexeme, std::stod(lexeme), line, start_col);
        else
            return Token(TokenType::Integer64, lexeme, std::stoll(lexeme), line, start_col);
    }

    Token scan_identifier()
    {
        size_t start = current - 1, start_col = column - 1;

        while (std::isalnum(peek()) || peek() == '_') advance();

        std::string lexeme(source.substr(start, current - start));
        auto it = keywords.find(lexeme);
        TokenType type = (it != keywords.end()) ? it->second : TokenType::Identifier;
        Value value = lexeme;

        if (type == TokenType::Bool)
            value = (lexeme == "true");

        return Token(type, lexeme, std::move(value), line, start_col);
    }
};

} // namespace lex
} // namespace phos
