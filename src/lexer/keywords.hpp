#include <unordered_map>
#include <string>

#include "./token_type.hpp"

namespace lex 
{
  std::unordered_map<std::string, Token_type> make_keywords() {
    std::unordered_map<std::string, Token_type> m;
    m["and"]    = Token_type::AND;
    m["class"]  = Token_type::CLASS;
    m["else"]   = Token_type::ELSE;
    m["false"]  = Token_type::FALSE;
    m["for"]    = Token_type::FOR;
    m["fun"]    = Token_type::FUN;
    m["if"]     = Token_type::IF;
    m["nil"]    = Token_type::NIL;
    m["or"]     = Token_type::OR;
    m["print"]  = Token_type::PRINT;
    m["return"] = Token_type::RETURN;
    m["super"]  = Token_type::SUPER;
    m["this"]   = Token_type::THIS;
    m["true"]   = Token_type::TRUE;
    m["var"]    = Token_type::VAR;
    m["while"]  = Token_type::WHILE;
    return m;
  }

  static const std::unordered_map<std::string, Token_type> keywords = make_keywords();
} // namespace lex
