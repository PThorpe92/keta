#include "parser.hpp"
#include <iostream>

void Parser::next() {
  // account for EOF token
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
  this->next();
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
  this->next();
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
  this->next();
}
void Parser::assert_identifier(Identifier *ident) {
  if (this->current_token.token.kind != TokenType::Identifier) {
    std::cerr << "Unknown or unexpected: " << ident->value
              << " on line: " << this->current_token.token.line << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
  this->next();
}

void Parser::assert_number() {
  if (this->current_token.token.kind != TokenType::NumberLiteral) {
    std::cerr << "Expected a number literal on line: "
              << this->current_token.token.line << " but got " << '\n';
    TokenPrinter *printer;
    printer->print_token_data(this->current_token.token);
    std::exit(1);
  }
  this->next();
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
  this->next();
}

void Parser::parse() {
  while (this->current_token.token.kind != TokenType::EOFToken) {
    this->next();
  }
}
