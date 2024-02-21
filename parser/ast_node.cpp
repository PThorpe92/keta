#include "ast_node.hpp"
#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>

AstNode *AstNode::make_node(NodeType nodetype) {
  switch (nodetype) {
  case NodeType::Program:
    return new Program();
  case NodeType::OpenModule:
    return new OpenModule();
  case NodeType::Function:
    return new Function();
  case NodeType::FunctionParameters:
    return new FunctionParameters();
  case NodeType::FunctionReturnType:
    return new FunctionReturnType();
  case NodeType::FunctionBody:
    return new FunctionBody();
  case NodeType::FunctionCall:
    return new FunctionCall();
  case NodeType::ReturnStatement:
    return new ReturnStatement();
  case NodeType::StructDefinition:
    return new StructDefinition();
  case NodeType::StructDefBody:
    return new StructDefBody();
  case NodeType::StructFieldAssignment:
    return new StructFieldAssignment();
  case NodeType::StructFieldAccess:
    return new StructFieldAccess();
  case NodeType::BinaryExpression:
    return new BinaryExpression();
  case NodeType::EnumDefinition:
    return new EnumDefinition();
  case NodeType::EnumMemberList:
    return new EnumMemberList();
  case NodeType::UnaryExpression:
    return new UnaryExpression();
  case NodeType::LetExpression:
    return new LetExpression();
  case NodeType::VariableAssignment:
    return new VariableAssignment();
  case NodeType::MethodCall:
    return new MethodCall();
  case NodeType::VarType:
    return new VarType();
  case NodeType::Variable:
    return new Variable();
  case NodeType::Statement:
    return new Statement();
  case NodeType::Expression:
    return new Expression();
  }
  std::cout << "Unknown node type \n";
  std::exit(1);
}

VarType new_type(Keyword *type_name) {
  KeywordType type = type_name->value;
  bool is_primitive = false;
  bool is_reference = false;
  bool is_array = false;
  switch (type) {
  case KeywordType::Int:
    is_primitive = true;
    is_reference = false;
    is_array = false;
    break;
  case KeywordType::String:
    is_primitive = true;
    is_reference = true;
    is_array = false;
    break;
  case KeywordType::Bool:
    is_primitive = true;
    is_reference = false;
    is_array = false;
    break;
  case KeywordType::Char:
    is_primitive = true;
    is_reference = false;
    is_array = false;
    break;
  case KeywordType::Float:
    is_primitive = true;
    is_reference = false;
    is_array = false;
    break;
  case KeywordType::Enum:
    is_primitive = true;
    is_reference = false;
    is_array = false;
    break;
  case KeywordType::Struct:
    is_primitive = true;
    is_reference = false;
    is_array = false;
    break;
  case KeywordType::Option:
    is_primitive = true;
    is_reference = false;
    is_array = false;
  case KeywordType::Some:
    is_primitive = true;
    is_reference = false;
    is_array = false;
  case KeywordType::IntArr:
    is_primitive = true;
    is_reference = false;
    is_array = true;
  case KeywordType::StringArr:
    is_primitive = true;
    is_reference = true;
    is_array = true;
  case KeywordType::BoolArr:
    is_primitive = true;
    is_reference = false;
    is_array = true;
  case KeywordType::EnumArr:
    is_primitive = true;
    is_reference = false;
    is_array = true;
  case KeywordType::StructArr:
    is_primitive = true;
    is_reference = false;
    is_array = true;
  case KeywordType::FloatArr:
    is_primitive = true;
    is_reference = false;
    is_array = true;
  case KeywordType::CharArr:
    is_primitive = true;
    is_reference = false;
    is_array = true;
  }
  return VarType(is_primitive, is_reference, is_array);
}

void ParsedLiteral::set_value(Token &val) {
  auto value = std::get<Literal>(val.data);
  this->value = value;
}

void FunctionParameters::add_parameter(AstNode *node) {
  this->parameters.push_back(std::unique_ptr<AstNode>(node));
}

void AstNode::print_node() const {
  std::cout << "Printing node \n" << this->identifier->value << '\n';
}

void AstNode::set_left(std::unique_ptr<AstNode> node) {
  node->set_parent(this);
  this->left = std::move(node);
}

AstNode *AstNode::get_left() { return this->left.get(); }

AstNode *AstNode::get_right() { return this->right.get(); }

AstNode *AstNode::get_parent() { return this->parent; }

void AstNode::operator=(AstNode *node) { this->left = std::move(node->left); }

void AstNode::operator=(AstNode &node) { this->left = std::move(node.left); }

void AstNode::operator=(std::unique_ptr<AstNode> node) {
  this->left = std::move(node);
}

void AstNode::set_identifier(Identifier *ident) { this->identifier = ident; }

void AstNode::set_parent(AstNode *node) { this->parent = node; }

void AstNode::set_right(std::unique_ptr<AstNode> node) {
  node->set_parent(this);
  this->left = std::move(node);
}

void Function::set_name(Identifier *name) { this->function_name = name->value; }

void Function::set_parameters(FunctionParameters *parameters) {
  this->parameters = parameters;
}
void Function::set_return_type(AstNode *return_type) {
  this->return_type =
      std::unique_ptr<VarType>(dynamic_cast<VarType *>(return_type));
}
void Function::set_body(AstNode *body) {
  this->body =
      std::unique_ptr<FunctionBody>(dynamic_cast<FunctionBody *>(body));
}
void FunctionCall::set_function_name(Function *function_name) {
  this->function_name = function_name;
}
void FunctionCall::set_arguments(FunctionArguments *arguments) {
  this->arguments = arguments;
}

void Variable::set_type(Keyword *type) { VarType newtype = new_type(type); }
void VariableAssignment::set_variable(Variable *variable) {
  this->variable = variable;
}

void VariableAssignment::set_expression(Expression *expression) {
  this->expression = expression;
}
void ReturnStatement::set_expression(AstNode *expression) {
  this->expression = expression;
}
void StructDefinition::set_name(Identifier *name) {
  this->struct_name = name->value;
}

void StructDefBody::add_field(AstNode *field) {
  this->fields.push_back(std::unique_ptr<AstNode>(field));
}
