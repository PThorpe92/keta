#include "token.hpp"

Keyword Keyword::keyword_from_string(std::string value) {
  if (value.compare("let") == 0) {
    return Keyword{KeywordType::Let};
  }
  if (value.compare("fn") == 0) {
    return Keyword{KeywordType::Fn};
  }
  if (value.compare("match") == 0) {
    return Keyword{KeywordType::Match};
  }
  if (value.compare("if") == 0) {
    return Keyword{KeywordType::If};
  }
  if (value.compare("else") == 0) {
    return Keyword{KeywordType::Else};
  }
  if (value.compare("then") == 0) {
    return Keyword{KeywordType::Then};
  }
  if (value.compare("int") == 0) {
    return Keyword{KeywordType::Int};
  }
  if (value.compare("bool") == 0) {
    return Keyword{KeywordType::Bool};
  }
  if (value.compare("string") == 0) {
    return Keyword{KeywordType::String};
  }
  if (value.compare("enum") == 0) {
    return Keyword{KeywordType::Enum};
  }
  if (value.compare("return") == 0) {
    return Keyword{KeywordType::Return};
  }
  if (value.compare("struct") == 0) {
    return Keyword{KeywordType::Struct};
  }
  if (value.compare("pub") == 0) {
    return Keyword{KeywordType::Pub};
  }
  if (value.compare("const") == 0) {
    return Keyword{KeywordType::Const};
  }
  if (value.compare("static") == 0) {
    return Keyword{KeywordType::Static};
  }
  if (value.compare("impl") == 0) {
    return Keyword{KeywordType::Impl};
  }
  if (value.compare("type") == 0) {
    return Keyword{KeywordType::Type};
  }
  if (value.compare("float") == 0) {
    return Keyword{KeywordType::Float};
  }
  if (value.compare("rec") == 0) {
    return Keyword{KeywordType::Rec};
  }
  if (value.compare("func") == 0) {
    return Keyword{KeywordType::Func};
  }
  if (value.compare("true") == 0) {
    return Keyword{KeywordType::True};
  }
  if (value.compare("false") == 0) {
    return Keyword{KeywordType::False};
  }
  if (value.compare("with") == 0) {
    return Keyword{KeywordType::With};
  }
  if (value.compare("int[]") == 0) {
    return Keyword{KeywordType::IntArr};
  }
  if (value.compare("bool[]") == 0) {
    return Keyword{KeywordType::BoolArr};
  }
  if (value.compare("string[]") == 0) {
    return Keyword{KeywordType::StringArr};
  }
  if (value.compare("enum[]") == 0) {
    return Keyword{KeywordType::EnumArr};
  }
  if (value.compare("float[]") == 0) {
    return Keyword{KeywordType::FloatArr};
  }
  if (value.compare("char[]") == 0) {
    return Keyword{KeywordType::CharArr};
  }
  if (value.compare("struct[]") == 0) {
    return Keyword{KeywordType::StructArr};
  }
  if (value.compare("iter") == 0) {
    return Keyword{KeywordType::Iter};
  }
  if (value.compare("option") == 0) {
    return Keyword{KeywordType::Option};
  }
  if (value.compare("some") == 0) {
    return Keyword{KeywordType::Some};
  }
  if (value.compare("open") == 0) {
    return Keyword{KeywordType::Open};
  }
  if (value.compare("char") == 0) {
    return Keyword{KeywordType::Char};
  }
  return Keyword{KeywordType::None};
};

std::string Token::keyword_to_string(KeywordType type) {
  auto keyword = "";
  switch (type) {
  case KeywordType::Fn:
    keyword = "fn";
    break;
  case KeywordType::Const:
    keyword = "const";
    break;
  case KeywordType::Let:
    keyword = "let";
    break;
  case KeywordType::Pub:
    keyword = "pub";
    break;
  case KeywordType::Static:
    keyword = "static";
    break;
  case KeywordType::Impl:
    keyword = "impl";
    break;
  case KeywordType::Type:
    keyword = "type";
    break;
  case KeywordType::Int:
    keyword = "int";
    break;
  case KeywordType::Bool:
    keyword = "bool";
    break;
  case KeywordType::String:
    keyword = "string";
    break;
  case KeywordType::Enum:
    keyword = "enum";
    break;
  case KeywordType::Iter:
    keyword = "iter";
    break;
  case KeywordType::Return:
    keyword = "return";
    break;
  case KeywordType::Float:
    keyword = "float";
    break;
  case KeywordType::Rec:
    keyword = "rec";
    break;
  case KeywordType::Func:
    keyword = "func";
    break;
  case KeywordType::True:
    keyword = "true";
    break;
  case KeywordType::False:
    keyword = "false";
    break;
  case KeywordType::With:
    keyword = "with";
    break;
  case KeywordType::Open:
    keyword = "open";
    break;
  case KeywordType::Match:
    keyword = "match";
    break;
  case KeywordType::Option:
    keyword = "option";
    break;
  case KeywordType::Some:
    keyword = "some";
    break;
  case KeywordType::Struct:
    keyword = "struct";
    break;
  case KeywordType::If:
    keyword = "if";
    break;
  case KeywordType::Else:
    keyword = "else";
    break;
  case KeywordType::Then:
    keyword = "then";
    break;
  case KeywordType::IntArr:
    keyword = "int[]";
    break;
  case KeywordType::BoolArr:
    keyword = "bool[]";
    break;
  case KeywordType::StringArr:
    keyword = "string[]";
    break;
  case KeywordType::EnumArr:
    keyword = "enum[]";
    break;
  case KeywordType::FloatArr:
    keyword = "float[]";
    break;
  case KeywordType::CharArr:
    keyword = "char[]";
    break;
  case KeywordType::StructArr:
    keyword = "struct[]";
    break;
  case KeywordType::Char:
    keyword = "char";
    break;
  default:
    keyword = "none";
  }
  return std::string(keyword);
}

std::string Token::operator_to_string(OperatorType type) {
  auto op_type = "";
  switch (type) {
  case OperatorType::LParen:
    op_type = "(";
    break;
  case OperatorType::RParen:
    op_type = ")";
    break;
  case OperatorType::LBrace:
    op_type = "{";
    break;
  case OperatorType::RBrace:
    op_type = "}";
    break;
  case OperatorType::Bang:
    op_type = "!";
    break;
  case OperatorType::Eq:
    op_type = "=";
    break;
  case OperatorType::Amp:
    op_type = "&";
    break;
  case OperatorType::DoubleQuote:
    op_type = "\"";
    break;
  case OperatorType::SingleQuote:
    op_type = "'";
    break;
  case OperatorType::LCaret:
    op_type = "<";
    break;
  case OperatorType::RCaret:
    op_type = ">";
    break;
  case OperatorType::Arrow:
    op_type = "->";
    break;
  case OperatorType::Asterisk:
    op_type = "*";
    break;
  case OperatorType::Caret:
    op_type = "^";
    break;
  case OperatorType::Comma:
    op_type = ",";
    break;
  case OperatorType::Period:
    op_type = ".";
    break;
  case OperatorType::QuestionMark:
    op_type = "?";
    break;
  case OperatorType::Underscore:
    op_type = "_";
    break;
  case OperatorType::Colon:
    op_type = ":";
    break;
  case OperatorType::Semicolon:
    op_type = ";";
    break;
  case OperatorType::Hashtag:
    op_type = "#";
    break;
  case OperatorType::Backtick:
    op_type = "`";
    break;
  case OperatorType::Pipe:
    op_type = "|";
    break;
  case OperatorType::Plus:
    op_type = "+";
    break;
  case OperatorType::Minus:
    op_type = "-";
    break;
  case OperatorType::Backslash:
    op_type = "\\";
    break;
  case OperatorType::FwdSlash:
    op_type = "/";
    break;
  case OperatorType::RBracket:
    op_type = "]";
    break;
  case OperatorType::LBracket:
    op_type = "[";
    break;
  case OperatorType::Modulo:
    op_type = "%";
    break;
  case OperatorType::LogicalAnd:
    op_type = "&&";
    break;
  case OperatorType::LogicalOr:
    op_type = "||";
    break;
  case OperatorType::EqEq:
    op_type = "==";
    break;
  case OperatorType::PlusEq:
    op_type = "+=";
    break;
  case OperatorType::MinusEq:
    op_type = "-=";
    break;
  case OperatorType::DivEq:
    op_type = "/=";
    break;
  case OperatorType::TimesEq:
    op_type = "*=";
    break;
  case OperatorType::NotEq:
    op_type = "!=";
    break;
  case OperatorType::GreaterEq:
    op_type = ">=";
    break;
  case OperatorType::LessEq:
    op_type = "<=";
    break;
  case OperatorType::LeftShift:
    op_type = "<<";
    break;
  case OperatorType::RightShift:
    op_type = ">>";
    break;
  case OperatorType::ColCol:
    op_type = "::";
    break;
  case OperatorType::EmptyString:
    op_type = "EMPTY_STR";
    break;
  case OperatorType::EmptyChar:
    op_type = "EMPTY_CHAR";
    break;
  case OperatorType::EmptyBrackets:
    op_type = "[]";
    break;
  case OperatorType::FatArrow:
    op_type = "=>";
    break;
  case OperatorType::PipeArrow:
    op_type = "|>";
    break;
  }
  return std::string(op_type);
}
// Identifiers and Keywords
Token Token::make_identifier(std::string value, uint32_t line) {
  if (Keyword::keyword_from_string(value).value != KeywordType::None) {
    return Token{TokenType::Keyword, Keyword::keyword_from_string(value), line};
  } else {
    return Token{TokenType::Identifier, Identifier{value}, line};
  }
}

bool Token::is_operator_type(OperatorType op) const {
  auto operator_data = std::get<Operator>(this->data);
  return operator_data.value == op;
}

bool Token::is_literal() const { return this->kind == TokenType::Literal; }
bool Token::is_string_literal() const {
  return this->kind == TokenType::Literal &&
         std::get<Literal>(this->data).type == LiteralType::String;
}
bool Token::is_number_literal() const {
  return this->kind == TokenType::Literal &&
         std::get<Literal>(this->data).type == LiteralType::Number;
}
bool Token::is_char_literal() const {
  return this->kind == TokenType::Literal &&
         std::get<Literal>(this->data).type == LiteralType::Char;
}
bool Token::is_float_literal() const {
  return this->kind == TokenType::Literal &&
         std::get<Literal>(this->data).type == LiteralType::Float;
}
bool Token::is_identifier() const {
  return this->kind == TokenType::Identifier;
}
bool Token::is_operator() const { return this->kind == TokenType::Operator; }
bool Token::is_keyword() const { return this->kind == TokenType::Keyword; }

bool Token::is_type() const {
  if (this->kind != TokenType::Keyword) {
    return false;
  }

  auto keyword = std::get<Keyword>(this->data).value;
  return keyword == KeywordType::Int || keyword == KeywordType::Bool ||
         keyword == KeywordType::String || keyword == KeywordType::Enum ||
         keyword == KeywordType::Float || keyword == KeywordType::Char;
}

bool Token::are_equal(const Token &lhs, const Token &rhs) {
  if (lhs.kind != rhs.kind) {
    return false;
  }
  if (lhs.kind == TokenType::Identifier) {
    return std::get<Identifier>(lhs.data).value ==
           std::get<Identifier>(rhs.data).value;
  }
  if (lhs.kind == TokenType::Keyword) {
    return std::get<Keyword>(lhs.data).value ==
           std::get<Keyword>(rhs.data).value;
  }
  if (lhs.kind == TokenType::Literal) {
    return std::get<Literal>(lhs.data).value ==
           std::get<Literal>(rhs.data).value;
  }
  if (lhs.kind == TokenType::Operator) {
    return std::get<Operator>(lhs.data).value ==
           std::get<Operator>(rhs.data).value;
  }
  return false;
}
Token Token::make_token(TokenType kind, TokenData data, uint32_t line) {
  return Token{kind, data, line};
}

Token Token::make_operator(OperatorType value, uint32_t line) {
  return Token{TokenType::Operator, Operator{value}, line};
}

Token Token::make_string_literal(const std::string &value, uint32_t line) {
  return Token{TokenType::Literal, Literal{value, LiteralType::String}, line};
}

Token Token::make_char_literal(const std::string &value, uint32_t line) {
  return Token{TokenType::Literal, Literal{value, LiteralType::Char}, line};
}

Token Token::make_number_literal(const std::string &value, uint32_t line) {
  if (value.find('.') != std::string::npos) {
    return Token{TokenType::Literal, Literal{value, LiteralType::Float}, line};
  }
  return Token{TokenType::Literal, Literal{value, LiteralType::Number}, line};
}

std::string Token::token_type_string(TokenType kind) {
  if (kind == TokenType::Identifier) {
    return std::string("Identifier");
  }
  if (kind == TokenType::Keyword) {
    return std::string("Keyword");
  }
  if (kind == TokenType::Literal) {
    return std::string("Literal");
  }
  if (kind == TokenType::Operator) {
    return std::string("Operator");
  }
  return "none";
}
