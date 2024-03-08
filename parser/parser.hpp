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
  AstNode *root;
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
      this->current_node = this->root;
    }
  };
  ~Parser();
  Token *peek();
  void advance();
  void assert_token(TokenType type);
  void assert_operator(OperatorType type);
  void assert_keyword(KeywordType type);
  void assert_identifier(Identifier *id);
  void assert_is_number_literal();
  void assert_is_string_literal();
  void assert_is_float_literal();
  void assert_is_operator();
  void assert_is_type();
  void assert_is_identifier();
  void assert_number(Literal *num);
  void operator=(Parser &parser);
  void operator=(Parser *parser);
  void operator=(Parser parser);
  // bootleg typechecking for now ;)
  void assert_type(KeywordType type);
  void parse();
  std::unique_ptr<AstNode> parse_let(), parse_function(), parse_open_module(),
      parse_var_type(), parse_variable(), parse_function_parameters(),
      parse_return_type(), parse_function_body(), parse_function_call(),
      parse_number_literal(), parse_string_literal(), parse_char_literal(),
      parse_operator(), parse_identifier(), parse_variable_assignment(),
      parse_return_statement(), parse_expression(),
      parse_expression_statement(), parse_if_statement(),
      parse_function_declaration(), parse_struct_declaration(),
      parse_enum_declaration(), parse_enum_case(), parse_enum_body(),
      parse_struct_body(), parse_struct_member(),
      parse_struct_member_declaration();
  void parse_ast();
  void print_ast() const;
  void print_tokens() const;

private:
  void print_ast_recursive(const AstNode *node) const;
};
