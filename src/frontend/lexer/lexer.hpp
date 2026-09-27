#pragma once

#include "core/arena.hpp"
#include "core/error/err.hpp"
#include "token.hpp"

#include <cctype>
#include <flat_map>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace phos::lex {

class Lexer
{
public:
    struct Tokenize_result
    {
        std::vector<Token> tokens;
        phos::err::Engine diagnostics{"lexing"};
    };

    explicit Lexer(std::string_view src, phos::mem::Arena &arena, std::string source_name = "<input>")
        : source(src), arena_(arena), source_name_(std::move(source_name))
    {}

    Tokenize_result tokenize()
    {
        Tokenize_result result;
        while (!is_at_end()) {
            if (auto tok = scan_token(); tok.has_value()) {
                if ((*tok).type == TokenType::Invalid) {
                    result.diagnostics.error(tok->line, tok->column, source_name_, "Invalid token: {}", tok->lexeme);
                    continue;
                }
                result.tokens.push_back(std::move(*tok));
            }
        }
        result.tokens.emplace_back(TokenType::Eof, "", Value(), line, column);
        return result;
    }

private:
    std::string_view source;
    size_t current = 0;
    size_t line = 1;
    size_t column = 1;

    phos::mem::Arena &arena_;
    std::string source_name_;
    const std::flat_map<std::string_view, TokenType> keywords = token_keywords;

    //  primitives
    bool is_at_end() const
    {
        return current >= source.size();
    }

    char peek() const
    {
        return is_at_end() ? '\0' : source[current];
    }
    char peek_next() const
    {
        return current + 1 >= source.size() ? '\0' : source[current + 1];
    }

    char advance()
    {
        char c = source[current++];
        column++;
        return c;
    }

    bool match(char expected)
    {
        if (is_at_end() || source[current] != expected) {
            return false;
        }
        current++;
        column++;
        return true;
    }

    //  main dispatch

    std::optional<Token> scan_token()
    {
        size_t start_col = column;
        char c = advance();

        switch (c) {
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
        case '(':
            return make(TokenType::LeftParen, "(", start_col);
        case ')':
            return make(TokenType::RightParen, ")", start_col);
        case '{':
            return make(TokenType::LeftBrace, "{", start_col);
        case '}':
            return make(TokenType::RightBrace, "}", start_col);
        case '[':
            return make(TokenType::LeftBracket, "[", start_col);
        case ']':
            return make(TokenType::RightBracket, "]", start_col);
        case ';':
            return make(TokenType::Semicolon, ";", start_col);
        case ',':
            return make(TokenType::Comma, ",", start_col);
        case '+':
            return make(TokenType::Plus, "+", start_col);
        case '*':
            return make(TokenType::Star, "*", start_col);
        case '%':
            return make(TokenType::Percent, "%", start_col);
        case '^':
            return make(TokenType::BitXor, "^", start_col);
        case '~':
            return make(TokenType::BitNot, "~", start_col);
        case '?':
            return make(TokenType::Question, "?", start_col);

        //  dot / range
        // .    →  Dot
        // ..   →  DotDot        (exclusive range)
        // ..=  →  DotDotEq      (inclusive range)
        case '.':
            if (match('.')) {
                if (match('=')) {
                    return make(TokenType::DotDotEq, "..=", start_col);
                }
                return make(TokenType::DotDot, "..", start_col);
            }
            return make(TokenType::Dot, ".", start_col);

        //  colon / scope
        case ':':
            if (match(':')) {
                return make(TokenType::ColonColon, "::", start_col);
            }
            return make(TokenType::Colon, ":", start_col);

        //  arrow / minus
        case '-':
            if (match('>')) {
                return make(TokenType::Arrow, "->", start_col);
            }
            return make(TokenType::Minus, "-", start_col);

        //  fat arrow / assign / equal
        // =    →  Assign
        // ==   →  Equal
        // =>   →  FatArrow      (match arm)
        case '=':
            if (match('=')) {
                return make(TokenType::Equal, "==", start_col);
            }
            if (match('>')) {
                return make(TokenType::FatArrow, "=>", start_col);
            }
            return make(TokenType::Assign, "=", start_col);

        //  not / not-equal
        case '!':
            if (match('=')) {
                return make(TokenType::NotEqual, "!=", start_col);
            }
            return make(TokenType::LogicalNot, "!", start_col);

        //  relational + bitwise shifts
        case '<':
            if (match('=')) {
                return make(TokenType::LessEqual, "<=", start_col);
            }
            if (match('<')) {
                return make(TokenType::BitLShift, "<<", start_col);
            }
            return make(TokenType::Less, "<", start_col);

        case '>':
            if (match('=')) {
                return make(TokenType::GreaterEqual, ">=", start_col);
            }
            if (match('>')) {
                return make(TokenType::BitRshift, ">>", start_col);
            }
            return make(TokenType::Greater, ">", start_col);

        //  logical / bitwise and
        case '&':
            if (match('&')) {
                return make(TokenType::LogicalAnd, "&&", start_col);
            }
            return make(TokenType::BitAnd, "&", start_col);

        //  logical / bitwise or / pipe
        case '|':
            if (match('|')) {
                return make(TokenType::LogicalOr, "||", start_col);
            }
            return make(TokenType::Pipe, "|", start_col);

        //  strings and f-strings
        case '"':
            return scan_string(start_col);

        case '@':
            return make(TokenType::At, "@", start_col);
        //  comments and division
        case '/':
            if (match('/')) {
                // line comment — consume until newline
                while (!is_at_end() && peek() != '\n') {
                    advance();
                }
                return std::nullopt;
            }
            if (match('*')) {
                // block comment — consume until */
                bool closed = false;
                while (!is_at_end()) {
                    if (peek() == '*' && peek_next() == '/') {
                        advance();
                        advance(); // consume '*' '/'
                        closed = true;
                        break;
                    }
                    char cc = advance();
                    if (cc == '\n') {
                        line++;
                        column = 1;
                    }
                }
                if (!closed) {
                    return Token(TokenType::Invalid, std::string("Unterminated block comment"), Value(), line, start_col);
                }
                return std::nullopt;
            }
            return make(TokenType::Slash, "/", start_col);

        default:
            if (std::isdigit(c)) {
                return scan_number(start_col);
            }

            if (std::isalpha(c) || c == '_') {
                return scan_identifier(start_col);
            }

            return Token(TokenType::Invalid, std::string(1, c), Value(), line, start_col);
        }
    }

    //  string scanner
    Token scan_string(size_t start_col)
    {
        std::string value;
        while (!is_at_end() && peek() != '"') {
            char c = advance();
            if (c == '\n') {
                line++;
                column = 1;
            } else if (c == '\\') {
                // basic escape sequences
                char esc = advance();
                switch (esc) {
                case 'n':
                    value += '\n';
                    break;
                case 't':
                    value += '\t';
                    break;
                case 'r':
                    value += '\r';
                    break;
                case '\\':
                    value += '\\';
                    break;
                case '"':
                    value += '"';
                    break;
                case '0':
                    value += '\0';
                    break;
                default:
                    value += '\\';
                    value += esc;
                    break;
                }
            } else {
                value += c;
            }
        }

        if (is_at_end()) {
            return Token(TokenType::Invalid, std::string("Unterminated string"), Value(), line, start_col);
        }

        advance(); // closing "
        Value str_val = Value::make_string(arena_, value);
        return Token(TokenType::String, std::format("\"{}\"", value), str_val, line, start_col);
    }

    Token scan_number(size_t start_col)
    {
        size_t start = current - 1;

        struct Numeric_suffix
        {
            std::string_view suffix;
            types::Primitive_kind kind;
            TokenType token;
        };
        // One table drives suffix matching in consume_numeric_suffix and the
        // suffix->TokenType mapping in finish_numeric_token, so the two can
        // never disagree about which suffixes exist.
        static constexpr Numeric_suffix kNumericSuffixes[] = {
            {"i8", types::Primitive_kind::I8, TokenType::TInt8},
            {"i16", types::Primitive_kind::I16, TokenType::TInt16},
            {"i32", types::Primitive_kind::I32, TokenType::TInt32},
            {"i64", types::Primitive_kind::I64, TokenType::TInt64},
            {"u8", types::Primitive_kind::U8, TokenType::TUInt8},
            {"u16", types::Primitive_kind::U16, TokenType::TUInt16},
            {"u32", types::Primitive_kind::U32, TokenType::TUInt32},
            {"u64", types::Primitive_kind::U64, TokenType::TUInt64},
            {"f16", types::Primitive_kind::F16, TokenType::TFloat16},
            {"f32", types::Primitive_kind::F32, TokenType::TFloat32},
            {"f64", types::Primitive_kind::F64, TokenType::TFloat64},
        };

        auto consume_numeric_suffix = [&]() -> std::optional<types::Primitive_kind> {
            std::string_view rest = source.substr(current);
            for (const auto &entry : kNumericSuffixes) {
                if (rest.starts_with(entry.suffix)) {
                    current += entry.suffix.size();
                    return entry.kind;
                }
            }
            return std::nullopt;
        };

        auto suffix_token_type = [](types::Primitive_kind kind) -> std::optional<TokenType> {
            for (const auto &entry : kNumericSuffixes) {
                if (entry.kind == kind) {
                    return entry.token;
                }
            }
            return std::nullopt;
        };

        auto finish_numeric_token = [&](Value default_value, TokenType default_token_type) -> Token {
            auto suffix_kind = consume_numeric_suffix();
            std::string lexeme(source.substr(start, current - start));

            if (!suffix_kind) {
                return Token(default_token_type, lexeme, default_value, line, start_col);
            }

            auto coerced = coerce_numeric_literal(default_value, *suffix_kind);
            if (!coerced) {
                return Token(TokenType::Invalid, lexeme, Value(), line, start_col);
            }

            auto final_type = suffix_token_type(*suffix_kind);
            if (!final_type) {
                return Token(TokenType::Invalid, lexeme, Value(), line, start_col);
            }

            return Token(*final_type, lexeme, coerced.value(), line, start_col);
        };

        auto strip_underscores = [](std::string_view text) -> std::string {
            std::string cleaned;
            cleaned.reserve(text.size());
            for (char c : text) {
                if (c != '_') {
                    cleaned += c;
                }
            }
            return cleaned;
        };

        auto consume_digits = [&](auto is_valid_digit, bool saw_digit = false) -> bool {
            while (true) {
                char c = peek();
                if (is_valid_digit(c)) {
                    saw_digit = true;
                    advance();
                    continue;
                }

                if (c == '_' && saw_digit && is_valid_digit(peek_next())) {
                    advance();
                    continue;
                }

                break;
            }
            return saw_digit;
        };

        // parses the full unsigned 64-bit magnitude for a given base, without
        // truncating to 32 bits — truncation must happen only after the
        // suffix (if any) is known, inside finish_numeric_token/coerce_numeric_literal.
        auto parse_full_width = [&](std::string_view digits, int base) -> std::optional<std::uint64_t> {
            try {
                size_t pos = 0;
                std::uint64_t val = std::stoull(std::string(digits), &pos, base);
                if (pos != digits.size()) {
                    return std::nullopt;
                }
                return val;
            } catch (const std::exception &) {
                return std::nullopt;
            }
        };

        // One body for 0x/0b/0o literals: consume the radix letter, the
        // digits, then hand off to finish_numeric_token for suffix/coercion.
        // Returns nullopt when the source is not this radix (caller tries
        // the next one); otherwise returns the finished token, which may be
        // Invalid when the digits or range are bad.
        auto scan_radix = [&](char letter, auto is_digit, int base, std::string_view low_name, std::string_view cap_name)
            -> std::optional<Token> {
            char upper = static_cast<char>(std::toupper(static_cast<unsigned char>(letter)));
            if (source[start] != '0' || (peek() != letter && peek() != upper)) {
                return std::nullopt;
            }
            advance(); // consume the radix letter
            if (!consume_digits(is_digit)) {
                return Token(TokenType::Invalid, std::format("Invalid {} literal", low_name), Value(), line, start_col);
            }
            std::string cleaned = strip_underscores(source.substr(start, current - start));
            auto raw = parse_full_width(std::string_view(cleaned).substr(2), base);
            if (!raw) {
                return Token(TokenType::Invalid, std::format("{} literal out of range", cap_name), Value(), line, start_col);
            }
            return finish_numeric_token(Value(*raw), TokenType::Integer32);
        };

        // hex literal: 0x...
        if (auto tok
            = scan_radix('x', [](char c) { return std::isxdigit(static_cast<unsigned char>(c)) != 0; }, 16, "hex", "Hex")) {
            return *tok;
        }

        // binary literal: 0b...
        if (auto tok = scan_radix('b', [](char c) { return c == '0' || c == '1'; }, 2, "binary", "Binary")) {
            return *tok;
        }

        // octal literal: 0o...
        if (auto tok = scan_radix('o', [](char c) { return c >= '0' && c <= '7'; }, 8, "octal", "Octal")) {
            return *tok;
        }

        consume_digits([](char c) { return std::isdigit(static_cast<unsigned char>(c)); }, true);
        bool is_float = false;

        // float: digits '.' digits  (but NOT '..' which is a range token)
        if (peek() == '.' && peek_next() != '.' && std::isdigit(peek_next())) {
            is_float = true;
            advance(); // consume '.'
            consume_digits([](char c) { return std::isdigit(static_cast<unsigned char>(c)); });
        }

        if (peek() == 'e' || peek() == 'E') {
            size_t exp_pos = current;
            size_t exp_col = column;
            advance();
            if (peek() == '+' || peek() == '-') {
                advance();
            }
            if (!consume_digits([](char c) { return std::isdigit(static_cast<unsigned char>(c)); })) {
                current = exp_pos;
                column = exp_col;
            } else {
                is_float = true;
            }
        }

        std::string cleaned = strip_underscores(source.substr(start, current - start));
        if (is_float) {
            double dval;
            try {
                dval = std::stod(cleaned);
            } catch (const std::exception &) {
                return Token(TokenType::Invalid, std::string("Float literal out of range"), Value(), line, start_col);
            }
            return finish_numeric_token(Value(dval), TokenType::Float64);
        }

        auto raw = parse_full_width(cleaned, 10);
        if (!raw) {
            return Token(TokenType::Invalid, std::string("Integer literal out of range"), Value(), line, start_col);
        }
        return finish_numeric_token(Value(*raw), TokenType::Integer32);
    }

    //  identifier / keyword scanner

    Token scan_identifier(size_t start_col)
    {
        size_t start = current - 1;
        while (std::isalnum(peek()) || peek() == '_') {
            advance();
        }

        std::string_view lexeme = source.substr(start, current - start);

        auto it = keywords.find(lexeme);
        TokenType type = (it != keywords.end()) ? it->second : TokenType::Identifier;

        Value literal = Value();
        if (type == TokenType::Bool) {
            literal = Value(lexeme == "true");
        }

        return Token(type, std::string(lexeme), literal, line, start_col);
    }

    //  factory

    Token make(TokenType type, std::string_view lexeme, size_t start_col) const
    {
        return Token(type, std::string(lexeme), Value(), line, start_col);
    }
};

} // namespace phos::lex
