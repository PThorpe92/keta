#include "token.hpp"
#include <string>
#include <vector>

class Lexer {

public:
  std::string input;
  u_long position;
  int line;
  char current_char;
  std::vector<TokenSpan> tokens;

public:
  // ___________________________CONSTRUCTORS___________________________________
  Lexer(std::string input)
      : input(input), position(0), line(0), current_char(input[0]), tokens({}) {
  }
  Lexer(std::string input, int position, int line, char current_char,
        std::vector<TokenSpan> tokens)
      : input(input), position(position), line(line),
        current_char(current_char), tokens(tokens) {}
  Lexer(const Lexer &) = default;
  Lexer(Lexer &&) = default;
  Lexer &operator=(const Lexer &) = default;
  ~Lexer() = default;

  // ____________________________METHODS_______________________________________
public:
  char next();
  char peek() const;
  void skip_whitespace();
  void skip_comment();
  void parse_tokens();
  Token build_identifier();
  Token build_number();
  Token build_string();
  Token build_operator();
  void print_tokens() const;
};
