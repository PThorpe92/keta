#pragma once
#include <cstdint>
#include <string>
#include <variant>

enum class TokenType {
  None,
  Identifier,
  Operator,
  StringLiteral,
  Keyword,
  CharLiteral,
  NumberLiteral,
  FloatLiteral,
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
  For,
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

struct StringLiteral {
  std::string value;
  StringLiteral(std::string value) : value(value) {}
  ~StringLiteral() = default;

  static StringLiteral string_literal_from_string(std::string value);
  bool operator==(const StringLiteral &other) const {
    return value == other.value;
  }
  bool operator!=(const StringLiteral &other) const {
    return value != other.value;
  }
};

struct CharLiteral {
  char value;
  CharLiteral(char value) : value(value) {}
  ~CharLiteral() = default;

  static CharLiteral char_literal_from_string(std::string value);
  bool operator==(const CharLiteral &other) const {
    return value == other.value;
  }
  bool operator!=(const CharLiteral &other) const {
    return value != other.value;
  }
};

struct NumberLiteral {
  std::string value;

  NumberLiteral(std::string value) : value(value) {}
  ~NumberLiteral() = default;

  NumberLiteral number_literal_from_string(std::string value);
  bool operator==(const NumberLiteral &other) const {
    return value == other.value;
  }
  bool operator!=(const NumberLiteral &other) const {
    return value != other.value;
  }
  bool operator<(const NumberLiteral &other) const {
    return value < other.value;
  }
  bool operator>(const NumberLiteral &other) const {
    return value > other.value;
  }
  bool operator<=(const NumberLiteral &other) const {
    return value <= other.value;
  }
  bool operator>=(const NumberLiteral &other) const {
    return value >= other.value;
  }
};

struct FloatLiteral {
  std::string value;
  FloatLiteral(std::string value) : value(value) {}
  ~FloatLiteral() = default;

  static FloatLiteral float_literal_from_string(std::string value);
};

using TokenData =
    std::variant<Identifier, Keyword, StringLiteral, NumberLiteral,
                 FloatLiteral, Operator, CharLiteral>;

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
  static Token make_string_literal(std::string value, uint32_t line);
  static Token make_char_literal(char value, uint32_t line);
  static Token make_number_literal(std::string value, uint32_t line);
  static Token make_token(TokenType kind, TokenData value, uint32_t line);
  static std::string token_type_string(TokenType kind);

  static std::string keyword_to_string(KeywordType type);
  static std::string operator_to_string(OperatorType type);
  static std::string number_literal_to_string(NumberLiteral num);
  static std::string string_literal_to_string(StringLiteral str);
  static std::string char_literal_to_string(CharLiteral chr);
  static std::string float_literal_to_string(FloatLiteral flt);
};

struct TokenPrinter {
  void operator()(const Identifier &id) const {
    std::printf("Identifier: %s\n", id.value.data());
  }

  void operator()(const Keyword &kw) const {
    std::string keyword = Token::keyword_to_string(kw.value);
    std::printf("Keyword: %s\n", keyword.data());
  }

  void operator()(const NumberLiteral &num) const {
    std::printf("Number Literal: %s\n", num.value.data());
  }
  void operator()(const StringLiteral &str) const {
    std::printf("StringLit: %s\n", str.value.data());
  }
  void operator()(const FloatLiteral &flt) const {
    std::printf("FloatLit: %s\n", flt.value.data());
  }
  void operator()(const CharLiteral &chr) const {
    std::printf("CharLit: %s\n", &chr.value);
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
