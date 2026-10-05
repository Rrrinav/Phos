#include "parser.hpp"

#include "core/error/err.hpp"
#include "core/error/result.hpp"
#include "core/value/type.hpp"
#include "frontend/lexer/token.hpp"
#include "frontend/parser/ast.hpp"
#include "frontend/parser/parser_internal.hpp"

#include <cassert>
#include <cstddef>
#include <expected>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace phos {

Result<types::Type_id> Parser::parse_type()
{
    using namespace types;

    Type_id current;

    // grouped
    if (match({lex::TokenType::LeftParen})) {
        ASSIGN_OR_RETURN(current, parse_type());
        TRY_IGNORE(consume(lex::TokenType::RightParen, "Expected ')' after type"));
    } else if (match({lex::TokenType::Fn})) {
        // function type
        std::vector<Type_id> params;

        TRY_IGNORE(consume(lex::TokenType::LeftParen, "Expect '(' after 'fn' for function type"));

        if (!check(lex::TokenType::RightParen)) {
            do {
                if (check(lex::TokenType::RightParen)) {
                    break;
                }
                DECL_OR_RETURN(param_type, parse_type());
                params.push_back(param_type);
            } while (match({lex::TokenType::Comma}));
        }

        TRY_IGNORE(consume(lex::TokenType::RightParen, "Expect ')' after function type parameters"));
        TRY_IGNORE(consume(lex::TokenType::Arrow, "Expect '->' after function type parameters"));
        std::vector<Type_id> returns;
        do {
            DECL_OR_RETURN(ret, parse_type());
            returns.push_back(ret);
        } while (match({lex::TokenType::Comma}));

        current = ctx_.tt.function(params, returns);
    }
    // Primitives
    else if (match({lex::TokenType::TInt64})) {
        current = ctx_.tt.get_i64();
    } else if (match({lex::TokenType::TInt32})) {
        current = ctx_.tt.get_i32();
    } else if (match({lex::TokenType::TInt16})) {
        current = ctx_.tt.get_i16();
    } else if (match({lex::TokenType::TInt8})) {
        current = ctx_.tt.get_i8();
    } else if (match({lex::TokenType::TUInt64})) {
        current = ctx_.tt.get_u64();
    } else if (match({lex::TokenType::TUInt32})) {
        current = ctx_.tt.get_u32();
    } else if (match({lex::TokenType::TUInt16})) {
        current = ctx_.tt.get_u16();
    } else if (match({lex::TokenType::TUInt8})) {
        current = ctx_.tt.get_u8();
    } else if (match({lex::TokenType::TFloat64})) {
        current = ctx_.tt.get_f64();
    } else if (match({lex::TokenType::TFloat32})) {
        current = ctx_.tt.get_f32();
    } else if (match({lex::TokenType::TFloat16})) {
        current = ctx_.tt.get_f16();
    } else if (match({lex::TokenType::TBool})) {
        current = ctx_.tt.get_bool();
    } else if (match({lex::TokenType::TString})) {
        current = ctx_.tt.get_string();
    } else if (match({lex::TokenType::TVoid})) {
        current = ctx_.tt.get_void();
    } else if (match({lex::TokenType::TAny})) {
        current = ctx_.tt.get_any();
    } else if (match({lex::TokenType::Model})) {

        std::vector<std::pair<std::string, Type_id>> fields;

        TRY_IGNORE(consume(lex::TokenType::LeftBrace, "Expect '{' after model"));

        while (!check(lex::TokenType::RightBrace) && !is_at_end()) {
            DECL_OR_RETURN(name, consume(lex::TokenType::Identifier, "Expect field name"));
            TRY_IGNORE(consume(lex::TokenType::Colon, "Expect ':'"));

            DECL_OR_RETURN(ty, parse_type());
            fields.push_back({name.lexeme, ty});

            if (check(lex::TokenType::Semicolon) || check(lex::TokenType::Comma)) {
                advance();
            }
        }

        TRY_IGNORE(consume(lex::TokenType::RightBrace, "Expect '}'"));
        current = ctx_.tt.model("", fields); // anonymous
    } else if (match({lex::TokenType::Identifier})) {
        std::string name = previous().lexeme;
        if (name == "any") {
            return std::unexpected(create_error(peek(), "Expected a type, 'any' is not one."));
        }

        while (match({lex::TokenType::ColonColon})) {
            DECL_OR_RETURN(member, consume(lex::TokenType::Identifier, "Expect type name after '::'"));
            name += "::" + member.lexeme;
        }
        // The parser doesn't attempt to resolve if this is a Model, Union, or Enum!
        // It simply creates an Unresolved_type placeholder. The Type Checker maps it later.
        current = ctx_.tt.unresolved(name);
    } else {
        return std::unexpected(create_error(peek(), "Expected a type"));
    }

    // suffixes (Arrays and Optionals)
    while (true) {
        if (match({lex::TokenType::LeftBracket})) {
            TRY_IGNORE(consume(lex::TokenType::RightBracket, "Expected ']'"));
            current = ctx_.tt.array(current);
        } else if (match({lex::TokenType::Question})) {
            current = ctx_.tt.optional(current);
        } else {
            break;
        }
    }

    return current;
}

} // namespace phos
