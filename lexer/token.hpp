#pragma once
#include <cstdint>
#include <string>
#include <variant>

enum class TokenType {
  None,
  Identifier,
  Operator,
  Keyword,
  Literal,
  EOFToken,
};

enum class OperatorType {
  LParen,
  RParen,
  LBrace,
  RBrace,
  Bang,
  Eq,
  Amp,
  DoubleQuote,
  SingleQuote,
  LCaret,
  RCaret,
  Asterisk,
  Caret,
  Comma,
  Period,
  QuestionMark,
  Underscore,
  Colon,
  Semicolon,
  Hashtag,
  Backtick,
  Pipe,
  Plus,
  Minus,
  Backslash,
  FwdSlash,
  RBracket,
  LBracket,
  Modulo,
  LogicalAnd,
  LogicalOr,
  EqEq,
  PlusEq,
  MinusEq,
  DivEq,
  TimesEq,
  NotEq,
  GreaterEq,
  LessEq,
  LeftShift,
  RightShift,
  Arrow,
  FatArrow,
  PipeArrow,
  ColCol,
  EmptyString,
  EmptyChar,
  EmptyBrackets,
};
enum class KeywordType {
  None,
  Let,
  Open,
  Fn,
  Match,
  If,
  Else,
  Then,
  Int,
  Bool,
  String,
  Enum,
  Return,
  Struct,
  Option,
  Some,
  Pub,
  Const,
  Static,
  Impl,
  Type,
  Float,
  Iter,
  Rec,
  Func,
  True,
  False,
  With,
  IntArr,
  BoolArr,
  EnumArr,
  StructArr,
  FloatArr,
  StringArr,
  CharArr,
  Char,
};
enum class LiteralType { String, Number, Char, Float };

struct Literal {
  std::string value;
  LiteralType type;
  Literal(std::string value, LiteralType type) : value(value), type(type) {}
  ~Literal() = default;
  void operator=(const std::string &value) { this->value = value; }
  void operator=(const Literal &other) {
    value = other.value;
    type = other.type;
  }
};

struct Keyword {
  KeywordType value;
  static Keyword keyword_from_string(std::string value);
  bool operator==(const KeywordType &other) const { return value == other; }
  bool operator!=(const Keyword &other) const { return value != other.value; }
  ~Keyword() = default;
};

struct Identifier {
  std::string value;
  Identifier identifier_from_string(std::string value);
};

struct Operator {
  OperatorType value;
  Operator() = default;
  Operator(OperatorType value) : value(value) {}
  ~Operator() = default;
  static Operator operator_from_string(std::string value);

  bool operator==(const Operator &other) const { return value == other.value; }
  bool operator!=(const Operator &other) const { return value != other.value; }
};

using TokenData = std::variant<Identifier, Keyword, Operator, Literal>;

class Token {
public:
  TokenType kind;
  TokenData data;
  uint32_t line;

  template <typename T>
  Token(TokenType kind, T data, uint32_t line)
      : kind(kind), data(data), line(line) {}
  Token() : kind(TokenType::None), data(Identifier()) {}
  Token(const Token &other) : kind(other.kind), data(other.data) {}
  Token &operator=(const Token &other) {
    kind = other.kind;
    data = other.data;
    line = other.line;
    return *this;
  }
  ~Token() = default;

  // overload operator to compare tokens
  bool operator==(const Token &other) const { return are_equal(*this, other); }
  static bool are_equal(const Token &lhs, const Token &rhs);
  static Token make_identifier(std::string value, uint32_t line);
  static Token make_operator(OperatorType value, uint32_t line);
  static Token make_string_literal(const std::string &value, uint32_t line);
  static Token make_char_literal(const std::string &value, uint32_t line);
  static Token make_number_literal(const std::string &value, uint32_t line);
  static Token make_token(TokenType kind, TokenData value, uint32_t line);
  bool is_literal() const;
  bool is_string_literal() const;
  bool is_char_literal() const;
  bool is_number_literal() const;
  bool is_float_literal() const;
  bool is_identifier() const;
  bool is_operator() const;
  bool is_keyword() const;
  bool is_type() const;
  bool is_operator_type(OperatorType type) const;
  static std::string token_type_string(TokenType kind);
  static std::string keyword_to_string(KeywordType type);
  static std::string operator_to_string(OperatorType type);
  static std::string number_literal_to_string(Literal num);
  static std::string string_literal_to_string(Literal str);
  static std::string char_literal_to_string(Literal chr);
  static std::string float_literal_to_string(Literal flt);
};

struct TokenPrinter {
  void operator()(const Identifier &id) const {
    std::printf("Identifier: %s\n", id.value.data());
  }

  void operator()(const Keyword &kw) const {
    std::string keyword = Token::keyword_to_string(kw.value);
    std::printf("Keyword: %s\n", keyword.data());
  }

  void operator()(const Literal &num) const {
    switch (num.type) {
    case LiteralType::String:
      std::printf("String Literal: %s\n", num.value.data());
      break;
    case LiteralType::Char:
      std::printf("Char Literal: %s\n", num.value.data());
    case LiteralType::Float:
      std::printf("Float Literal: %s\n", num.value.data());
      break;
    case LiteralType::Number:
      std::printf("Number Literal: %s\n", num.value.data());
      break;
    }
  }
  void operator()(const Operator &op) const {
    auto op_type = Token::operator_to_string(op.value);
    std::printf("Operator: %s\n", op_type.data());
  }
  void print_token_data(const Token &token) const {
    std::visit(*this, token.data);
  }
};

struct Span {
  u_long beg;
  u_long end;

  Span() : beg(0), end(0) {}
  Span(u_long beg, u_long end) : beg(beg), end(end) {}
  static Span from(u_long beg, std::string token) {
    return Span(beg, token.length() + beg);
  }
  ~Span() = default;
};
struct TokenSpan {
public:
  Token token;
  struct Span span;

public:
  TokenSpan() : token({}), span({}) {}
  TokenSpan(struct Token token, struct Span span) : token(token), span(span) {}
};
