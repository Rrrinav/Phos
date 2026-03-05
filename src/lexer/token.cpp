#include "token.hpp"
#include <string>

std::string phos::lex::token_to_string(phos::lex::TokenType t)
{
    switch(t) {
        case TokenType::Integer64:    return "Integer64";
        case TokenType::Float64:      return "Float64";
        case TokenType::String:       return "String";
        case TokenType::Bool:         return "Bool";
        case TokenType::Array:        return "Array";
        case TokenType::Identifier:   return "Identifier";
        case TokenType::Let:          return "Let";
        case TokenType::Const:        return "Const";
        case TokenType::Nil:          return "Nil";
        case TokenType::If:           return "If";
        case TokenType::Else:         return "Else";
        case TokenType::While:        return "While";
        case TokenType::For:          return "For";
        case TokenType::Fn:           return "Fn";
        case TokenType::Return:       return "Return";
        case TokenType::Print:        return "Print";
        case TokenType::PrintErr:     return "PrintErr";
        case TokenType::Plus:         return "Plus";
        case TokenType::Minus:        return "Minus";
        case TokenType::Star:         return "Star";
        case TokenType::Slash:        return "Slash";
        case TokenType::Percent:      return "Percent";
        case TokenType::Equal:        return "Equal";
        case TokenType::NotEqual:     return "NotEqual";
        case TokenType::Static:       return "Static";
        case TokenType::Less:         return "Less";
        case TokenType::Greater:      return "Greater";
        case TokenType::LessEqual:    return "LessEqual";
        case TokenType::GreaterEqual: return "GreaterEqual";
        case TokenType::Assign:       return "Assign";
        case TokenType::LogicalAnd:   return "LogicalAnd";
        case TokenType::LogicalOr:    return "LogicalOr";
        case TokenType::ColonColon:   return "ColonColon";
        case TokenType::LogicalNot:   return "LogicalNot";
        case TokenType::Arrow:        return "Arrow";
        case TokenType::LeftParen:    return "LeftParen";
        case TokenType::RightParen:   return "RightParen";
        case TokenType::LeftBrace:    return "LeftBrace";
        case TokenType::RightBrace:   return "RightBrace";
        case TokenType::LeftBracket:  return "LeftBracket";
        case TokenType::RightBracket: return "RightBracket";
        case TokenType::Semicolon:    return "Semicolon";
        case TokenType::BitAnd:       return "BitAnd";
        case TokenType::BitXor:       return "BitXor";
        case TokenType::BitNot:       return "BitNot";
        case TokenType::BitLShift:    return "BitLShift";
        case TokenType::BitRshift:    return "BitRshift";
        case TokenType::PlusEqual:    return "PlusEqual";
        case TokenType::MinusEqual:   return "MinusEqual";
        case TokenType::StarEqual:    return "StarEqual";
        case TokenType::SlashEqual:   return "SlashEqual";
        case TokenType::Model:        return "Model";
        case TokenType::Union:        return "Union";
        case TokenType::Pipe:         return "Pipe";
        case TokenType::Dot:          return "Dot";
        case TokenType::Comma:        return "Comma";
        case TokenType::Colon:        return "Colon";
        case TokenType::Question:     return "Question";
        case TokenType::As:           return "As";
        case TokenType::This:         return "This";
        case TokenType::Newline:      return "Newline";
        case TokenType::Eof:          return "Eof";
        case TokenType::Invalid:      return "Invalid";
        case TokenType::Integer32:    return "Integer32";
        case TokenType::Integer16:    return "Integer16";
        case TokenType::Integer8:     return "Integer8";
        case TokenType::UInt64:       return "UInt64";
        case TokenType::UInt32:       return "UInt32";
        case TokenType::UInt16:       return "UInt16";
        case TokenType::UInt8:        return "UInt8";
        default: return "Unknown token-Su57, you damn?";
    }
}
