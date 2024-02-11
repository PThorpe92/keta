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
  std::unique_ptr<AstNode> root;
  AstNode *current_node;
  Parser(Lexer *lexer)
      : tokens(lexer->tokens), current_token(lexer->tokens[0]),
        peek_token(lexer->tokens[1]) {
    this->lexer = lexer;
    this->tokens = this->lexer->tokens;
    if (this->tokens.size() > 0) {
      this->current_token = this->tokens[0];
      this->peek_token = this->tokens[1];
      this->index = 0;
      this->root = AstNode::make_node(AstNode::NodeType::Program);
      this->current_node = this->root.get();
    }
  };
  ~Parser();
  Token *peek();
  void advance();
  void assert_token(TokenType type);
  void assert_operator(OperatorType type);
  void assert_keyword(KeywordType type);
  void assert_identifier(Identifier *id);
  void assert_is_number();
  void assert_is_operator();
  void assert_is_type();
  void assert_is_identifier();
  void assert_number(NumberLiteral *num);
  // bootleg typechecking for now ;)
  void assert_type(KeywordType type);
  void parse();
  std::unique_ptr<AstNode> parse_let(), parse_function(), parse_open_module(),
      parse_var_type(), parse_variable(), parse_function_parameters(),
      parse_return_type(), parse_function_body(), parse_function_call(),
      parse_function_argumerts(), parse_expression(), parse_number_literal(),
      parse_string_literal(), parse_char_literal(), parse_operator(),
      parse_identifier(), parse_variable_assignment(), parse_return_statement();
  void parse_ast();
  void print_ast() const;
  void print_tokens() const;

private:
  void print_ast_recursive(const AstNode *node) const;
};
