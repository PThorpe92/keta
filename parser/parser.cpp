#include "parser.hpp"
#include <iostream>
#include <memory>

void Parser::advance() {
  if (this->index >= this->tokens.size() + 2) {
    return;
  }
  this->index++;
  this->current_token = this->tokens[this->index];
  this->peek_token = this->tokens[this->index + 1];
}

Token *Parser::peek() {
  if (this->index >= this->tokens.size() + 2) {
    return nullptr;
  }
  return &this->peek_token.token;
}

void Parser::assert_token(TokenType kind) {
  TokenPrinter *printer;
  if (this->current_token.token.kind != kind) {
    std::cerr << "Expected: " << Token::token_type_string(kind)
              << "on line: " << this->current_token.token.line << " but got "
              << '\n';
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
  this->advance();
}
void Parser::assert_operator(OperatorType kind) {
  if (this->current_token.token.kind != TokenType::Operator) {
    std::cerr << "Expected: " << Token::operator_to_string(kind)
              << " on line: " << this->current_token.token.line << " but got "
              << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
  this->advance();
}
void Parser::assert_keyword(KeywordType kind) {
  if (this->current_token.token.kind != TokenType::Keyword) {
    std::cerr << "Expected: " << Token::keyword_to_string(kind)
              << " on line: " << this->current_token.token.line << " but got "
              << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
  this->advance();
}
void Parser::assert_is_identifier() {
  if (this->current_token.token.kind != TokenType::Identifier) {
    std::cerr << "Unknown or unexpected: "
              << " on line: " << this->current_token.token.line << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
}

void Parser::assert_is_number_literal() {
  if (this->current_token.token.kind != TokenType::Literal) {
    auto number = std::get_if<Literal>(&this->current_token.token.data);
    if (number != nullptr) {
      if (number->type != LiteralType::Number) {
        std::cerr << "Expected a number literal on line: "
                  << this->current_token.token.line << " but got " << '\n';
        TokenPrinter *printer;
        printer->print_token_data(this->current_token.token);
        std::exit(1);
      }
    } else {
      std::cerr << "Expected a number literal on line: "
                << this->current_token.token.line << " but got " << '\n';
      TokenPrinter *printer;
      printer->print_token_data(this->current_token.token);
      std::exit(1);
    }
  }
}

void Parser::assert_number(Literal *num) {
  Literal *number = std::get_if<Literal>(&this->current_token.token.data);
  if (number == nullptr) {
    std::cerr << "Expected number literal: " << num->value
              << " on line: " << this->current_token.token.line << " but got "
              << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  } else if (number->value != num->value) {
    std::cerr << "Expected number literal: " << num->value
              << " on line: " << this->current_token.token.line << " but got "
              << number->value << '\n';
    std::exit(1);
  }
}

void Parser::assert_type(KeywordType type) {
  if (this->current_token.token.kind != TokenType::Keyword) {
    std::cerr << "Expected type: " << Token::keyword_to_string(type)
              << " on line: " << this->current_token.token.line << " but got "
              << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
  this->advance();
}
void Parser::assert_is_type() {
  if (this->current_token.token.kind != TokenType::Keyword) {
    std::cerr << "Expected type: "
              << " on line: " << this->current_token.token.line << " but got "
              << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
}

void Parser::assert_identifier(Identifier *id) {
  if (this->current_token.token.kind != TokenType::Identifier) {
    std::cerr << "Expected identifier: " << id->value
              << " on line: " << this->current_token.token.line << " but got "
              << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
}

void Parser::print_ast_recursive(const AstNode *node) const {
  if (node == nullptr) {
    return;
  }
  node->print_node();

  if (node->left != nullptr) {
    print_ast_recursive(node->left.get());
  }

  if (node->right != nullptr) {
    print_ast_recursive(node->right.get());
  }
}

// let x: int = 5
std::unique_ptr<AstNode> Parser::parse_let() {
  // assert_keyword will advance the token
  assert_keyword(KeywordType::Let);
  auto let_node = AstNode::make_node(AstNode::NodeType::LetExpression);
  this->advance();
  this->assert_is_identifier();
  let_node->set_left(parse_var_type());
  this->advance();
  this->assert_operator(OperatorType::Eq);
  let_node->set_right(parse_expression());
  return std::unique_ptr<AstNode>(let_node);
}
// x: int
std::unique_ptr<AstNode> Parser::parse_var_type() {
  this->assert_is_identifier();
  auto variable = static_cast<VarType *>(
      AstNode::make_node(AstNode::NodeType::VariableTypeDef));
  if (variable != nullptr) {
    variable->set_identifier(
        std::get_if<Identifier>(&this->current_token.token.data));
    // assert colon, will advance token
    this->assert_operator(OperatorType::Colon);
    this->assert_type(
        std::get_if<Keyword>(&this->current_token.token.data)->value);
    Keyword *type = std::get_if<Keyword>(&this->current_token.token.data);
    std::get_if<Keyword>(&this->current_token.token.data);
    return std::unique_ptr<AstNode>(variable);
  } else {
    std::cerr << "Failed to parse var type\n";
    std::exit(1);
  }
}
// ()
std::unique_ptr<FunctionArguments> Parser::parse_function_arguments() {
  this->assert_operator(OperatorType::LParen);
  this->advance();
  auto node = std::make_unique<FunctionArguments>();
  while (!this->current_token.token.is_operator_type(OperatorType::RParen)) {
    if (this->current_token.token.kind == TokenType::Identifier) {
      // if its an identifier, we can assume its a variable, add it to the list
      auto variable = static_cast<Variable *>(
          AstNode::make_node(AstNode::NodeType::Variable));
      variable->set_identifier(
          std::get_if<Identifier>(&this->current_token.token.data));
      node->add_argument(variable);
      this->advance();
    } else if (this->current_token.token.is_literal()) {
      // if its a literal, we can assume its a constant, add it to the list
      auto val = std::get_if<Literal>(&this->current_token.token.data);
      if (val != nullptr) {
        ParsedLiteral *literal = new ParsedLiteral(*val);
        literal->set_value(this->current_token.token);
        node->set_left(std::move(literal));
        this->advance();
      }
    } else if (this->current_token.token.is_operator_type(
                   OperatorType::Comma)) {
      continue;
    }
  }
  this->assert_operator(OperatorType::RParen);
  this->advance();
  return node;
}
std::unique_ptr<AstNode> Parser::parse_function() {
  this->assert_keyword(KeywordType::Fn);
  this->advance();
  auto function = std::make_unique<Function>();
  this->assert_is_identifier();
  function->set_identifier(
      std::get_if<Identifier>(&this->current_token.token.data));
  this->advance();
  function->set_left(parse_function_arguments());
  this->assert_operator(OperatorType::Arrow);
  this->advance();
  function->set_right(parse_expression());
  return function;
}

std::unique_ptr<AstNode> Parser::parse_return_statement() {
  this->assert_keyword(KeywordType::Return);
  this->advance();
  auto return_node = std::make_unique<ReturnStatement>();
  return_node->set_left(parse_expression());
  return return_node;
}

std::unique_ptr<AstNode> Parser::parse_expression() {
  if (this->current_token.token.kind == TokenType::Literal) {
    auto val = std::get_if<Literal>(&this->current_token.token.data);
    if (val != nullptr) {
      auto literal = std::make_unique<ParsedLiteral>(*val);
      this->advance();
      return literal;
    }
  } else if (this->current_token.token.kind == TokenType::Identifier) {
    auto identifier = std::get_if<Identifier>(&this->current_token.token.data);
    if (identifier != nullptr) {
      auto variable = std::make_unique<Variable>();
      variable->set_identifier(identifier);
      this->advance();
      return variable;
    }
  }
  return nullptr;
}

std::unique_ptr<AstNode> Parser::parse_function_body() {
  this->assert_operator(OperatorType::LBrace);
  this->advance();
  auto node = std::make_unique<FunctionBody>();
  while (!this->current_token.token.is_operator_type(OperatorType::RBrace)) {
    node->add_statement(parse_expression());
    auto return_node = std::get_if<Keyword>(&this->current_token.token.data);
    if (return_node != nullptr) {
      if (return_node->value == KeywordType::Return) {
        node->set_right(parse_return_statement());
      }
    }
  }

  this->assert_operator(OperatorType::RBrace);
  this->advance();
  return node;
}

void Parser::print_ast() const { print_ast_recursive(this->root); }

void Parser::parse() {
  if (this->current_token.token.kind == TokenType::EOFToken) {
    return;
  }
  switch (this->current_token.token.kind) {
  case TokenType::Keyword:
    const Keyword *keywordPtr = std::get_if<Keyword>(&current_token.token.data);
    if (keywordPtr != nullptr) {
      const Keyword &keyword = *keywordPtr;
      switch (keyword.value) {
      case KeywordType::Open:
        this->current_node->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::OpenModule)));
        this->advance();
        this->parse();
        break;
      case KeywordType::Let:
        this->current_node->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::LetExpression)));
        this->advance();
        this->parse();
        break;
      case KeywordType::Fn:
        this->current_node->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::Function)));
        this->advance();
        this->parse();
        break;
      case KeywordType::Return:
        this->current_node->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::ReturnStatement)));
        this->advance();
        this->parse();
        break;
      case KeywordType::Enum:
        this->current_node->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::EnumDefinition)));
        this->advance();
        this->parse();
        break;
      case KeywordType::Struct:
        this->root->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::EnumDefinition)));
        this->advance();
        this->parse();
        break;
      case KeywordType::If:
        this->root->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::EnumDefinition)));
        this->advance();
        this->parse();
        break;
      case KeywordType::Else:
        this->root->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::ElseBlock)));
        this->advance();
        this->parse();
        break;
      case KeywordType::Match:
        this->root->set_left(std::unique_ptr<AstNode>(
            AstNode::make_node(AstNode::NodeType::MatchExpression)));
        this->advance();
        this->parse();
        break;
      }
    }
  }
}
