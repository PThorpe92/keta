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
  return Token::make_identifier(keyword);
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
      break;
    }
  }
  return Token::make_number_literal(num_literal);
}

Token Lexer::build_string() {
  std::string str_literal;
  while (this->next() != '\"') {
    str_literal.push_back(this->current_char);
    if (this->current_char == '\n') {
      this->line++;
    }
  }
  return Token::make_string_literal(str_literal);
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
    return Token::make_operator(OperatorType::Arrow);
  else if (op == "==")
    return Token::make_operator(OperatorType::EqEq);
  else if (op == "!=")
    return Token::make_operator(OperatorType::NotEq);
  else if (op == ">=")
    return Token::make_operator(OperatorType::GreaterEq);
  else if (op == "<=")
    return Token::make_operator(OperatorType::LessEq);
  else if (op == "<<")
    return Token::make_operator(OperatorType::LeftShift);
  else if (op == ">>")
    return Token::make_operator(OperatorType::RightShift);
  else if (op == "::")
    return Token::make_operator(OperatorType::ColCol);
  else if (op == "&&")
    return Token::make_operator(OperatorType::LogicalAnd);
  else if (op == "||")
    return Token::make_operator(OperatorType::LogicalOr);
  else if (op == "+=")
    return Token::make_operator(OperatorType::PlusEq);
  else if (op == "-=")
    return Token::make_operator(OperatorType::MinusEq);
  else if (op == "/=")
    return Token::make_operator(OperatorType::DivEq);
  else if (op == "*=")
    return Token::make_operator(OperatorType::TimesEq);
  else if (op == "//")
    this->skip_comment();
  else if (op == "\'\'")
    return Token::make_operator(OperatorType::EmptyChar);
  else if (op == "\"\"")
    return Token::make_operator(OperatorType::EmptyString);
  else if (op == "[]")
    return Token::make_operator(OperatorType::EmptyBrackets);
  else {
    switch (this->current_char) {
    case '(':
      return Token::make_operator(OperatorType::LParen);
      break;
    case ')':
      return Token::make_operator(OperatorType::RParen);
      break;
    case '[':
      return Token::make_operator(OperatorType::LBracket);
      break;
    case ']':
      return Token::make_operator(OperatorType::RBracket);
      break;
    case '{':
      return Token::make_operator(OperatorType::LBrace);
      break;
    case '}':
      return Token::make_operator(OperatorType::RBrace);
      break;
    case ';':
      return Token::make_operator(OperatorType::Semicolon);
      break;
    case ':':
      return Token::make_operator(OperatorType::Colon);
      break;
    case ',':
      return Token::make_operator(OperatorType::Comma);
      break;
    case '.': {
      return Token::make_operator(OperatorType::Period);
    }
    case '+':
      return Token::make_operator(OperatorType::Plus);
      break;
    case '-':
      return Token::make_operator(OperatorType::Minus);
      break;
    case '*':
      return Token::make_operator(OperatorType::Asterisk);
      break;
    case '%':
      return Token::make_operator(OperatorType::Modulo);
      break;
    case '=':
      return Token::make_operator(OperatorType::Eq);
      break;
    case '!':
      return Token::make_operator(OperatorType::Bang);
      break;
    case '<':
      return Token::make_operator(OperatorType::LCaret);
      break;
    case '>':
      return Token::make_operator(OperatorType::RCaret);
    case '&':
      return Token::make_operator(OperatorType::Amp);
    case '|':
      return Token::make_operator(OperatorType::Pipe);
      break;
    case '^':
      return Token::make_operator(OperatorType::Caret);
      break;
    case '#':
      return Token::make_operator(OperatorType::Hashtag);
      break;
    case '?':
      return Token::make_operator(OperatorType::QuestionMark);
      break;
    case '_':
      if (is_letter(peek)) {
        return this->build_identifier();
        break;
      } else {
        return Token::make_operator(OperatorType::Underscore);
      }
    case '\\':
      return Token::make_operator(OperatorType::Backslash);
      break;
    case '\"': {
      return this->build_string();
      break;
    }
    case '\'':
      return Token::make_operator(OperatorType::SingleQuote);
    };
  };
  return this->build_identifier();
};
