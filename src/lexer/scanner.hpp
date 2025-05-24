#include <cctype>
#include <unordered_map>
#include <variant>
#include <vector>
#include <string>

#include "./token.hpp"
#include "./token_type.hpp"
#include "./keywords.hpp"
#include "../misc/error_reporting.hpp"

class Scanner
{
  std::string source;
  std::vector<lex::Token> tokens;
  std::unordered_map<std::string, lex::Token_type> keywords;

  std::size_t start   = 0;
  std::size_t current = 0;
  std::size_t line    = 1;

public:
  Scanner(std::string source) : source(source) { keywords = lex::make_keywords(); }

  std::vector<lex::Token> scan_tokens()
  {
    while (!is_at_end())
    {
      start = current;
      scan_token();
    }
    tokens.push_back(lex::Token{lex::Token_type::END_OF_FILE, "", std::monostate(), line});
    return tokens;
  }

private:
  bool is_at_end() { return current >= source.length(); }

  void scan_token()
  {
    char c = advance();
    switch(c)
    {
      case '(': add_token(lex::Token_type::LEFT_PAREN);  break;
      case ')': add_token(lex::Token_type::RIGHT_PAREN); break;
      case '{': add_token(lex::Token_type::LEFT_BRACE);  break;
      case '}': add_token(lex::Token_type::RIGHT_BRACE); break;
      case ',': add_token(lex::Token_type::COMMA);       break;
      case '.': add_token(lex::Token_type::DOT);         break;
      case '-': add_token(lex::Token_type::MINUS);       break;
      case '+': add_token(lex::Token_type::PLUS);        break;
      case ';': add_token(lex::Token_type::SEMICOLON);   break;
      case '*': add_token(lex::Token_type::STAR);        break;
      case '!':
        add_token(match('=') ? lex::Token_type::BANG_EQUAL : lex::Token_type::BANG);
        break;
      case '=':
        add_token(match('=') ? lex::Token_type::EQUAL_EQUAL : lex::Token_type::EQUAL);
        break;
      case '<':
        add_token(match('=') ? lex::Token_type::LESS_EQUAL : lex::Token_type::LESS);
        break;
      case '>':
        add_token(match('=') ? lex::Token_type::GREATER_EQUAL : lex::Token_type::GREATER);
        break;
      case '/':
        scan_backslash();
        break;
      case ' ': case '\r': case '\t': break; // ignore whitespace
      case '\n': line++; break;
      case '"': scan_string(); break;
      default:
        if (std::isdigit(c)) scan_number();
        else if (std::isalpha(c)) scan_identifier();
        else if (c == '_') scan_identifier();
        else if (is_at_end()) break;
        else report(line, "Unexpected character");
    }
  }

  char advance()
  {
    current++;
    return source[current - 1];
  }

  void add_token(lex::Token_type type, std::variant<std::monostate, double, std::string> literal)
  {
    std::string text = source.substr(start, current - start);
    tokens.push_back({type, text, literal, line});
  }

  void add_token(lex::Token_type type) { add_token(type, std::monostate()); }

  bool match(char x)
  {
    if (is_at_end()) return false;
    if (source[current] != x) return false;

    current++;
    return true;
  }

  char peek() {
    if (is_at_end()) return '\0';
    return source[current];
  }

  char peek(int x) {
    if (is_at_end()) return '\0';

    if (current + x < source.size())
      return source[current + x];
    else
      return '\0';
  }

  void scan_backslash()
  {
    if (match('/'))
      while (peek() != '\n' && !is_at_end()) advance();
    else
      add_token(lex::Token_type::SLASH);
  }

  void scan_string()
  {
    while (peek() != '"' && !is_at_end())
    {
      if (peek() == '\n')
        line++;
      advance();
    }

    if (is_at_end())
    {
      report(line, "Unterminated string");
      return;
    }

    advance();
    std::string value = source.substr(start + 1, current - start - 1);
    add_token(lex::Token_type::STRING, value);
  }

  void scan_number()
  {
    while (std::isdigit(peek())) advance();

    if (peek() == '.' && std::isdigit(peek(1))) {
      advance();
      while (std::isdigit(peek())) advance();
    }
    add_token(lex::Token_type::NUMBER, std::stod(source.substr(start, current - start)));
  }

  void scan_identifier()
  {
    while (std::isalnum(peek()) || peek() == '_') advance();

    std::string text = source.substr(start, current - start);
    auto it = keywords.find(text);
    lex::Token_type type = (it != keywords.end()) ? it->second : lex::Token_type::IDENTIFIER;

    add_token(type);
  }
};
