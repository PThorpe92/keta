#include "../lexer/lexer.hpp"
#include "ast_node.hpp"
#include <vector>

class Parser {

public:
  Lexer *lexer;
  TokenSpan &current_token;
  TokenSpan &peek_token;
  std::vector<TokenSpan> tokens;
  uint32_t index = 0;
  std::vector<AstNode *> ast = {};
  Parser(Lexer *lexer)
      : tokens(lexer->tokens), current_token(lexer->tokens[0]),
        peek_token(lexer->tokens[1]) {
    this->lexer = lexer;
    this->tokens = this->lexer->tokens;
    this->current_token = this->tokens[0];
    this->peek_token = this->tokens[1];
    this->index = 0;
    this->ast = {};
  };
  ~Parser();
  void next();
  Token *peek();
  void assert_token(TokenType type);
  void assert_operator(OperatorType type);
  void assert_keyword(KeywordType type);
  void assert_identifier(Identifier *ident);
  void assert_number();
  // bootleg typechecking for now ;)
  void assert_type(KeywordType type);
  void parse();
  void print_ast();
  void print_tokens();
};
