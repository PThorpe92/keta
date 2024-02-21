#include "lexer.hpp"

bool is_letter(char c) { return std::isalpha(static_cast<int>(c)); }

bool is_digit(char c) { return std::isdigit(static_cast<int>(c)); }

bool is_space(char c) { return std::isspace(static_cast<int>(c)); }

bool is_twodigit_op(char one, char next_char) {
  if ((one == '-' && next_char == '>') || (one == '=' && next_char == '=') ||
      (one == '!' && next_char == '=') || (one == '>' && next_char == '=') ||
      (one == '<' && next_char == '=') || (one == ':' && next_char == ':') ||
      (one == '&' && next_char == '&') || (one == '|' && next_char == '|') ||
      (one == '+' && next_char == '=') || (one == '-' && next_char == '=') ||
      (one == '/' && next_char == '/') || (one == '\"' && next_char == '\"') ||
      (one == '[' && next_char == ']')) {
    return true;
  } else {
    return false;
  }
}
bool is_type(std::string keyword) {
  if (keyword == "int" || keyword == "bool" || keyword == "string" ||
      keyword == "struct" || keyword == "float" || keyword == "enum")
    return true;
  else
    return false;
}

Token Lexer::build_identifier() {
  std::string keyword;
  if (is_letter(this->current_char) || this->current_char == '_')
    keyword.push_back(this->current_char);
  while (this->next()) {
    if (is_letter(this->current_char) || is_digit(this->current_char)) {
      keyword.push_back(this->current_char);
    } else {
      break;
    }
    if (this->current_char == '[' && this->peek() == ']' && is_type(keyword)) {
      keyword += "[]";
    }
  }
  return Token::make_identifier(keyword, this->line);
}

void Lexer::skip_comment() {
  while (this->next() != '\n') {
    continue;
  }
  this->line++;
  this->next();
  this->parse_tokens();
}

void Lexer::skip_whitespace() {
  if (this->current_char == '\n') {
    this->line++;
  }
  if (is_space(this->current_char)) {
    this->next();
  }
}

Token Lexer::build_number() {
  std::string num_literal;
  num_literal.push_back(this->current_char);
  while (this->next()) {
    if (is_digit(this->current_char) || this->current_char == '.') {
      num_literal.push_back(this->current_char);
    } else {
      this->tokens.push_back(TokenSpan(
          Token::make_token(TokenType::EOFToken, Identifier(), this->line),
          Span(this->position, this->position)));
      break;
    }
  }
  return Token::make_number_literal(num_literal, this->line);
}

Token Lexer::build_string() {
  std::string str_literal;
  while (this->next() != '\"') {
    str_literal.push_back(this->current_char);
    if (this->current_char == '\n') {
      this->line++;
    }
  }
  return Token::make_string_literal(str_literal, this->line);
}

char Lexer::next() {
  if (this->position >= this->input.size() - 1) {
    return '\0';
  } else {
    this->position++;
    this->current_char = this->input[this->position];
    return this->current_char;
  }
}

char Lexer::peek() const {
  if (this->position >= this->input.size() - 1) {
    return '\0';
  } else {
    return this->input[this->position + 1];
  }
}
void Lexer::print_tokens() const {
  TokenPrinter printer;
  for (auto &tokenspan : this->tokens) {
    printer.print_token_data(tokenspan.token);
  }
}

void Lexer::parse_tokens() {
  if (is_space(this->current_char)) {
    this->skip_whitespace();
  }
  if (is_digit(this->current_char)) {
    auto pos = this->position;
    Token token = this->build_number();
    Span span = Span(pos, this->position);
    this->tokens.push_back(TokenSpan(token, span));
    if (this->current_char != '\0') {
      this->parse_tokens();
    } else {
      this->tokens.push_back(TokenSpan(
          Token::make_token(TokenType::EOFToken, Identifier(), this->line),
          Span(this->position, this->position)));
      return;
    }
  } else if (is_letter(this->current_char)) {
    auto pos = this->position;
    Token token = this->build_identifier();
    Span span = Span(pos, this->position);
    this->tokens.push_back(TokenSpan(token, span));
    if (this->current_char != '\0') {
      this->parse_tokens();
    } else {
      this->tokens.push_back(TokenSpan(
          Token::make_token(TokenType::EOFToken, Identifier(), this->line),
          Span(this->position, this->position)));
      return;
    }
  } else {
    auto pos = this->position;
    Token token = this->build_operator();
    Span span = Span(pos, this->position);
    this->tokens.push_back(TokenSpan(token, span));
    if (this->next() != '\0') {
      this->parse_tokens();
    } else {
      this->tokens.push_back(TokenSpan(
          Token::make_token(TokenType::EOFToken, Identifier(), this->line),
          Span(this->position, this->position)));
      return;
    }
  }
}

Token Lexer::build_operator() {
  auto peek = this->peek();
  std::string op(1, this->current_char);
  auto one = this->current_char;
  char next_char = this->peek();
  if (is_twodigit_op(one, next_char)) {
    op += next_char;
    this->next();
  }
  if (op == "->")
    return Token::make_operator(OperatorType::Arrow, this->line);
  else if (op == "==")
    return Token::make_operator(OperatorType::EqEq, this->line);
  else if (op == "!=")
    return Token::make_operator(OperatorType::NotEq, this->line);
  else if (op == ">=")
    return Token::make_operator(OperatorType::GreaterEq, this->line);
  else if (op == "<=")
    return Token::make_operator(OperatorType::LessEq, this->line);
  else if (op == "<<")
    return Token::make_operator(OperatorType::LeftShift, this->line);
  else if (op == ">>")
    return Token::make_operator(OperatorType::RightShift, this->line);
  else if (op == "::")
    return Token::make_operator(OperatorType::ColCol, this->line);
  else if (op == "&&")
    return Token::make_operator(OperatorType::LogicalAnd, this->line);
  else if (op == "||")
    return Token::make_operator(OperatorType::LogicalOr, this->line);
  else if (op == "+=")
    return Token::make_operator(OperatorType::PlusEq, this->line);
  else if (op == "-=")
    return Token::make_operator(OperatorType::MinusEq, this->line);
  else if (op == "/=")
    return Token::make_operator(OperatorType::DivEq, this->line);
  else if (op == "*=")
    return Token::make_operator(OperatorType::TimesEq, this->line);
  else if (op == "//")
    this->skip_comment();
  else if (op == "\'\'")
    return Token::make_operator(OperatorType::EmptyChar, this->line);
  else if (op == "\"\"")
    return Token::make_operator(OperatorType::EmptyString, this->line);
  else if (op == "[]")
    return Token::make_operator(OperatorType::EmptyBrackets, this->line);
  else {
    switch (this->current_char) {
    case '(':
      return Token::make_operator(OperatorType::LParen, this->line);
      break;
    case ')':
      return Token::make_operator(OperatorType::RParen, this->line);
      break;
    case '[':
      return Token::make_operator(OperatorType::LBracket, this->line);
      break;
    case ']':
      return Token::make_operator(OperatorType::RBracket, this->line);
      break;
    case '{':
      return Token::make_operator(OperatorType::LBrace, this->line);
      break;
    case '}':
      return Token::make_operator(OperatorType::RBrace, this->line);
      break;
    case ';':
      return Token::make_operator(OperatorType::Semicolon, this->line);
      break;
    case ':':
      return Token::make_operator(OperatorType::Colon, this->line);
      break;
    case ',':
      return Token::make_operator(OperatorType::Comma, this->line);
      break;
    case '.': {
      return Token::make_operator(OperatorType::Period, this->line);
    }
    case '+':
      return Token::make_operator(OperatorType::Plus, this->line);
      break;
    case '-':
      return Token::make_operator(OperatorType::Minus, this->line);
      break;
    case '*':
      return Token::make_operator(OperatorType::Asterisk, this->line);
      break;
    case '%':
      return Token::make_operator(OperatorType::Modulo, this->line);
      break;
    case '=':
      return Token::make_operator(OperatorType::Eq, this->line);
      break;
    case '!':
      return Token::make_operator(OperatorType::Bang, this->line);
      break;
    case '<':
      return Token::make_operator(OperatorType::LCaret, this->line);
      break;
    case '>':
      return Token::make_operator(OperatorType::RCaret, this->line);
    case '&':
      return Token::make_operator(OperatorType::Amp, this->line);
    case '|':
      return Token::make_operator(OperatorType::Pipe, this->line);
      break;
    case '^':
      return Token::make_operator(OperatorType::Caret, this->line);
      break;
    case '#':
      return Token::make_operator(OperatorType::Hashtag, this->line);
      break;
    case '?':
      return Token::make_operator(OperatorType::QuestionMark, this->line);
      break;
    case '_':
      if (is_letter(peek)) {
        return this->build_identifier();
        break;
      } else {
        return Token::make_operator(OperatorType::Underscore, this->line);
      }
    case '\\':
      return Token::make_operator(OperatorType::Backslash, this->line);
      break;
    case '\"': {
      return this->build_string();
      break;
    }
    case '\'':
      return Token::make_operator(OperatorType::SingleQuote, this->line);
    };
  };
  return this->build_identifier();
};
