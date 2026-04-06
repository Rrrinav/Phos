#pragma once

#include "./token.hpp"

#include <string>
#include <vector>
#include <optional>
#include <format>
#include <cctype>

namespace phos::lex
{

class Lexer
{
public:
    explicit Lexer(std::string_view src) : source(src) {}

    std::vector<Token> tokenize()
    {
        std::vector<Token> tokens;
        while (!is_at_end())
        {
            if (auto tok = scan_token(); tok.has_value())
                tokens.push_back(std::move(*tok));
        }
        tokens.emplace_back(TokenType::Eof, "", std::monostate{}, line, column);
        return tokens;
    }

private:
    std::string_view source;
    size_t current = 0;
    size_t line    = 1;
    size_t column  = 1;

    const std::unordered_map<std::string_view, TokenType> keywords = token_keywords;

    //  primitives

    bool is_at_end() const { return current >= source.size(); }

    char peek()      const { return is_at_end() ? '\0' : source[current]; }
    char peek_next() const { return current + 1 >= source.size() ? '\0' : source[current + 1]; }

    char advance()
    {
        char c = source[current++];
        column++;
        return c;
    }

    bool match(char expected)
    {
        if (is_at_end() || source[current] != expected)
            return false;
        current++;
        column++;
        return true;
    }

    //  main dispatch

    std::optional<Token> scan_token()
    {
        size_t start_col = column;
        char c = advance();

        switch (c)
        {
        //  whitespace
        case ' ':
        case '\r':
        case '\t':
            return std::nullopt;

        case '\n':
            line++;
            column = 1;
            return std::nullopt; // newlines are invisible (parser skips them already)

        //  unambiguous single-char
        case '(': return make(TokenType::LeftParen,    "(", start_col);
        case ')': return make(TokenType::RightParen,   ")", start_col);
        case '{': return make(TokenType::LeftBrace,    "{", start_col);
        case '}': return make(TokenType::RightBrace,   "}", start_col);
        case '[': return make(TokenType::LeftBracket,  "[", start_col);
        case ']': return make(TokenType::RightBracket, "]", start_col);
        case ';': return make(TokenType::Semicolon,    ";", start_col);
        case ',': return make(TokenType::Comma,        ",", start_col);
        case '+': return make(TokenType::Plus,         "+", start_col);
        case '*': return make(TokenType::Star,         "*", start_col);
        case '%': return make(TokenType::Percent,      "%", start_col);
        case '^': return make(TokenType::BitXor,       "^", start_col);
        case '~': return make(TokenType::BitNot,       "~", start_col);
        case '?': return make(TokenType::Question,     "?", start_col);

        //  dot / range
        // .    →  Dot
        // ..   →  DotDot        (exclusive range)
        // ..=  →  DotDotEq      (inclusive range)
        case '.':
            if (match('.'))
            {
                if (match('='))
                    return make(TokenType::DotDotEq, "..=", start_col);
                return make(TokenType::DotDot, "..", start_col);
            }
            return make(TokenType::Dot, ".", start_col);

        //  colon / scope
        case ':':
            if (match(':'))
                return make(TokenType::ColonColon, "::", start_col);
            return make(TokenType::Colon, ":", start_col);

        //  arrow / minus
        case '-':
            if (match('>'))
                return make(TokenType::Arrow, "->", start_col);
            return make(TokenType::Minus, "-", start_col);

        //  fat arrow / assign / equal
        // =    →  Assign
        // ==   →  Equal
        // =>   →  FatArrow      (match arm)
        case '=':
            if (match('='))
                return make(TokenType::Equal,    "==", start_col);
            if (match('>'))
                return make(TokenType::FatArrow, "=>", start_col);
            return make(TokenType::Assign, "=", start_col);

        //  not / not-equal
        case '!':
            if (match('='))
                return make(TokenType::NotEqual,    "!=", start_col);
            return make(TokenType::LogicalNot, "!",  start_col);

        //  relational + bitwise shifts
        case '<':
            if (match('=')) return make(TokenType::LessEqual,  "<=", start_col);
            if (match('<')) return make(TokenType::BitLShift,  "<<", start_col);
            return make(TokenType::Less, "<", start_col);

        case '>':
            if (match('=')) return make(TokenType::GreaterEqual, ">=", start_col);
            if (match('>')) return make(TokenType::BitRshift,    ">>", start_col);
            return make(TokenType::Greater, ">", start_col);

        //  logical / bitwise and
        case '&':
            if (match('&'))
                return make(TokenType::LogicalAnd, "&&", start_col);
            return make(TokenType::BitAnd, "&", start_col);

        //  logical / bitwise or / pipe
        case '|':
            if (match('|'))
                return make(TokenType::LogicalOr, "||", start_col);
            return make(TokenType::Pipe, "|", start_col);

        //  strings and f-strings
        case '"':
            return scan_string(start_col);

        //  comments and division
        case '/':
            if (match('/'))
            {
                // line comment — consume until newline
                while (!is_at_end() && peek() != '\n')
                    advance();
                return std::nullopt;
            }
            if (match('*'))
            {
                // block comment — consume until */
                while (!is_at_end())
                {
                    if (peek() == '\n') { line++; column = 1; }
                    if (peek() == '*' && peek_next() == '/')
                    {
                        advance(); advance(); // consume '*' '/'
                        break;
                    }
                    advance();
                }
                return std::nullopt;
            }
            return make(TokenType::Slash, "/", start_col);

        default:
            // f-string:  f"..."
            if (c == 'f' && peek() == '"')
            {
                advance(); // consume opening "
                return scan_fstring(start_col);
            }

            if (std::isdigit(c))
                return scan_number(start_col);

            if (std::isalpha(c) || c == '_')
                return scan_identifier(start_col);

            return Token(TokenType::Invalid, std::string(1, c), std::monostate{}, line, start_col);
        }
    }

    //  string scanner

    Token scan_string(size_t start_col)
    {
        std::string value;
        while (!is_at_end() && peek() != '"')
        {
            char c = advance();
            if (c == '\n') { line++; column = 1; }
            else if (c == '\\')
            {
                // basic escape sequences
                char esc = advance();
                switch (esc)
                {
                case 'n':  value += '\n'; break;
                case 't':  value += '\t'; break;
                case 'r':  value += '\r'; break;
                case '\\': value += '\\'; break;
                case '"':  value += '"';  break;
                case '0':  value += '\0'; break;
                default:   value += '\\'; value += esc; break;
                }
            }
            else
            {
                value += c;
            }
        }

        if (is_at_end())
            return Token(TokenType::Invalid, "Unterminated string",
                         std::string("Unterminated string"), line, start_col);

        advance(); // closing "
        return Token(TokenType::String,
                     std::format("\"{}\"", value), std::move(value), line, start_col);
    }

    //  f-string scanner
    // Called after the opening " has been consumed (start_col points at the 'f').
    // Stores the raw template (with braces intact) as the literal value.
    // The parser will split on {} and build a concat tree from it.
    Token scan_fstring(size_t start_col)
    {
        std::string raw;
        int depth = 0; // track nested braces inside interpolations

        while (!is_at_end())
        {
            char c = advance();

            if (c == '"' && depth == 0)
            {
                // closing " outside an interpolation — done
                return Token(TokenType::Fstring, std::format("f\"{}\"", raw), std::move(raw), line, start_col);
            }

            if (c == '\n') { line++; column = 1; }

            // escape sequences inside f-string literal portions
            if (c == '\\' && depth == 0)
            {
                char esc = advance();
                switch (esc)
                {
                case 'n':  raw += '\n'; break;
                case 't':  raw += '\t'; break;
                case 'r':  raw += '\r'; break;
                case '\\': raw += '\\'; break;
                case '"':  raw += '"';  break;
                case '{':  raw += '{';  break; // escaped brace → literal
                case '}':  raw += '}';  break;
                default:   raw += '\\'; raw += esc; break;
                }
                continue;
            }

            // track interpolation depth so e.g. f"{obj.method({1,2})}" works
            if (c == '{') depth++;
            if (c == '}' && depth > 0) depth--;

            raw += c;
        }

        return Token(TokenType::Invalid, "Unterminated f-string",
                     std::string("Unterminated f-string"), line, start_col);
    }

    //  number scanner

    Token scan_number(size_t start_col)
    {
        size_t start = current - 1;

        // hex literal: 0x...
        if (source[start] == '0' && (peek() == 'x' || peek() == 'X'))
        {
            advance(); // consume 'x'
            while (std::isxdigit(peek())) advance();
            std::string lexeme(source.substr(start, current - start));
            int64_t val = std::stoll(lexeme, nullptr, 16);
            return Token(TokenType::Integer64, lexeme, val, line, start_col);
        }

        // binary literal: 0b...
        if (source[start] == '0' && (peek() == 'b' || peek() == 'B'))
        {
            advance(); // consume 'b'
            while (peek() == '0' || peek() == '1') advance();
            std::string lexeme(source.substr(start, current - start));
            int64_t val = std::stoll(lexeme.substr(2), nullptr, 2);
            return Token(TokenType::Integer64, lexeme, val, line, start_col);
        }

        while (std::isdigit(peek())) advance();

        // float: digits '.' digits  (but NOT '..' which is a range token)
        if (peek() == '.' && peek_next() != '.' && std::isdigit(peek_next()))
        {
            advance(); // consume '.'
            while (std::isdigit(peek())) advance();
            // optional exponent: e/E [+-] digits
            if (peek() == 'e' || peek() == 'E')
            {
                advance();
                if (peek() == '+' || peek() == '-') advance();
                while (std::isdigit(peek())) advance();
            }
            std::string lexeme(source.substr(start, current - start));
            return Token(TokenType::Float64, lexeme, std::stod(lexeme), line, start_col);
        }

        std::string lexeme(source.substr(start, current - start));
        return Token(TokenType::Integer64, lexeme, std::stoll(lexeme), line, start_col);
    }

    //  identifier / keyword scanner

    Token scan_identifier(size_t start_col)
    {
        size_t start = current - 1;
        while (std::isalnum(peek()) || peek() == '_') advance();

        std::string_view lexeme = source.substr(start, current - start);

        auto it = keywords.find(lexeme);
        TokenType type = (it != keywords.end()) ? it->second : TokenType::Identifier;

        Value literal = std::string(lexeme); // default
        if (type == TokenType::Bool)
            literal = (lexeme == "true");

        return Token(type, std::string(lexeme), std::move(literal), line, start_col);
    }

    //  factory

    Token make(TokenType type, std::string_view lexeme, size_t start_col) const
    {
        return Token(type, std::string(lexeme), std::monostate{}, line, start_col);
    }
};

} // namespace phos::lex
