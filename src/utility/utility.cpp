#include <unordered_map>
#include "utility.hpp"

namespace phos::util
{

std::string operator_token_to_string(phos::lex::TokenType type)
{
    static const std::unordered_map<phos::lex::TokenType, std::string> token_names = {
        {phos::lex::TokenType::Plus, "+"},
        {phos::lex::TokenType::Minus, "-"},
        {phos::lex::TokenType::Star, "*"},
        {phos::lex::TokenType::Slash, "/"},
        {phos::lex::TokenType::Percent, "%"},
        {phos::lex::TokenType::Equal, "=="},
        {phos::lex::TokenType::NotEqual, "!="},
        {phos::lex::TokenType::Less, "<"},
        {phos::lex::TokenType::Greater, ">"},
        {phos::lex::TokenType::LessEqual, "<="},
        {phos::lex::TokenType::GreaterEqual, ">="},
        {phos::lex::TokenType::LogicalAnd, "&&"},
        {phos::lex::TokenType::LogicalOr, "||"},
        {phos::lex::TokenType::LogicalNot, "!"}
    };
    auto it = token_names.find(type);
    return it != token_names.end() ? it->second : "unknown_operator";
}
} // namespace phos::util


