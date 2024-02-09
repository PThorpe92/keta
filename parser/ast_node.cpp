#include "ast_node.hpp"
#include <memory>

std::unique_ptr<AstNode> AstNode::make_node(NodeType nodetype) {
  switch (nodetype) {
  case NodeType::Program:
    return std::make_unique<Program>();
  case NodeType::OpenModule:
    return std::make_unique<OpenModule>();
  case NodeType::Function:
    return std::make_unique<Function>();
  case NodeType::FunctionParameters:
    return std::make_unique<FunctionParameters>();
  case NodeType::FunctionReturnType:
    return std::make_unique<FunctionReturnType>();
  case NodeType::FunctionBody:
    return std::make_unique<FunctionBody>();
  case NodeType::FunctionCall:
    return std::make_unique<FunctionCall>();
  case NodeType::ReturnStatement:
    return std::make_unique<ReturnStatement>();
  case NodeType::StructDefinition:
    return std::make_unique<StructDefinition>();
  case NodeType::StructDefBody:
    return std::make_unique<StructDefBody>();
  case NodeType::StructFieldAssignment:
    return std::make_unique<StructFieldAssignment>();
  case NodeType::StructFieldAccess:
    return std::make_unique<StructFieldAccess>();
  case NodeType::BinaryExpression:
    return std::make_unique<BinaryExpression>();
  case NodeType::EnumDefinition:
    return std::make_unique<EnumDefinition>();
  case NodeType::EnumMemberList:
    return std::make_unique<EnumMemberList>();
  case NodeType::UnaryExpression:
    return std::make_unique<UnaryExpression>();
  case NodeType::LetExpression:
    return std::make_unique<LetExpression>();
  case NodeType::VariableAssignment:
    return std::make_unique<VariableAssignment>();
  case NodeType::MethodCall:
    return std::make_unique<MethodCall>();
  case NodeType::VarType:
    return std::make_unique<VarType>();
  case NodeType::Variable:
    return std::make_unique<Variable>();
  case NodeType::Statement:
    return std::make_unique<Statement>();
  case NodeType::Expression:
    return std::make_unique<Expression>();
  }
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
