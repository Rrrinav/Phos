#pragma once

#include <string>

namespace lex
{
  enum class Token_type
  {
    // Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    // One or two character tokens.
    BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,

    // Literals.
    IDENTIFIER, STRING, NUMBER,

    // Keywords.
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    END_OF_FILE
  };

  std::string token_type_to_string(Token_type type)
  {
    switch (type)
    {
      case Token_type::LEFT_PAREN:    return "left_paren";
      case Token_type::RIGHT_PAREN:   return "right_paren";
      case Token_type::LEFT_BRACE:    return "left_brace";
      case Token_type::RIGHT_BRACE:   return "right_brace";
      case Token_type::COMMA:         return "comma";
      case Token_type::DOT:           return "dot";
      case Token_type::MINUS:         return "minus";
      case Token_type::PLUS:          return "plus";
      case Token_type::SEMICOLON:     return "semicolon";
      case Token_type::SLASH:         return "slash";
      case Token_type::STAR:          return "star";

      case Token_type::BANG:          return "bang";
      case Token_type::BANG_EQUAL:    return "bang_equal";
      case Token_type::EQUAL:         return "equal";
      case Token_type::EQUAL_EQUAL:   return "equal_equal";
      case Token_type::GREATER:       return "greater";
      case Token_type::GREATER_EQUAL: return "greater_equal";
      case Token_type::LESS:          return "less";
      case Token_type::LESS_EQUAL:    return "less_equal";

      case Token_type::IDENTIFIER:    return "identifier";
      case Token_type::STRING:        return "string";
      case Token_type::NUMBER:        return "number";

      case Token_type::AND:           return "and";
      case Token_type::CLASS:         return "class";
      case Token_type::ELSE:          return "else";
      case Token_type::FALSE:         return "false";
      case Token_type::FUN:           return "fun";
      case Token_type::FOR:           return "for";
      case Token_type::IF:            return "if";
      case Token_type::NIL:           return "nil";
      case Token_type::OR:            return "or";
      case Token_type::PRINT:         return "print";
      case Token_type::RETURN:        return "return";
      case Token_type::SUPER:         return "super";
      case Token_type::THIS:          return "this";
      case Token_type::TRUE:          return "true";
      case Token_type::VAR:           return "var";
      case Token_type::WHILE:         return "while";
      case Token_type::END_OF_FILE:   return "eof";
    }
    return "unknown";
  }
}

