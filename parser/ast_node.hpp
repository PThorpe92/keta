#include "../lexer/token.hpp"
#include <memory>
#include <string>
#include <vector>

class AstNode {
public:
  enum class NodeType {
    Program,
    OpenModule,
    EnumMember,
    Function,
    FunctionParameters,
    FunctionArguments,
    FunctionReturnType,
    FunctionBody,
    FunctionCall,
    ImplForBlock,
    ReturnStatement,
    StructDefinition,
    StructDefBody,
    StructField,
    StructFieldAssignment,
    StructFieldAccess,
    StructConstructor,
    BinaryExpression,
    EnumDefinition,
    EnumMemberList,
    UnaryExpression,
    LetExpression,
    VariableTypeDef,
    VariableAssignment,
    MethodCall,
    TypeDeclaration,
    VarType,
    Variable,
    Statement,
    Expression,
    PrefixOperator,
    PostfixOperator,
  };

protected:
  NodeType type;

public:
  AstNode *parent = nullptr;
  std::unique_ptr<AstNode> left = nullptr;
  std::unique_ptr<AstNode> right = nullptr;
  AstNode() = default;
  explicit AstNode(NodeType type) : type(type) {}
  virtual ~AstNode() = default;

  NodeType get_type() const;
  virtual void set_left(AstNode *node);
  virtual void set_right(AstNode *node);
  virtual std::unique_ptr<AstNode> make_node(NodeType type);
  virtual void set_parent(AstNode *node);
};

class Program : public AstNode {
public:
  std::string filename;
  Program() : AstNode(NodeType::Program) {}
};

class OpenModule : public AstNode {
public:
  std::string module_name;
  OpenModule() : AstNode(NodeType::OpenModule){};
};

class VarType : public AstNode {
public:
  Keyword type_name;
  bool is_primitive;
  bool is_reference;
  bool is_array;
  VarType() : AstNode(NodeType::VarType){};
  VarType(bool is_primitive, bool is_reference, bool is_array)
      : AstNode(NodeType::VarType), is_primitive(is_primitive),
        is_reference(is_reference), is_array(is_array) {}
};

class Variable : public AstNode {
public:
  std::string variable_name;
  VarType *var_type;
  Variable() : AstNode(NodeType::Variable){};
};

class FunctionParameters : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> parameters;
  FunctionParameters() : AstNode(NodeType::FunctionParameters){};
};

class Function : public AstNode {
public:
  std::string function_name;
  FunctionParameters *parameters;
  std::unique_ptr<VarType> return_type;
  std::unique_ptr<AstNode> body;
  Function() : AstNode(NodeType::Function){};
  void set_name(Identifier *name);
  void set_parameters(FunctionParameters *parameters);
  void set_return_type(AstNode *return_type);
};

class FunctionReturnType : public AstNode {
public:
  VarType *return_type;
  FunctionReturnType() : AstNode(NodeType::FunctionReturnType){};
};

class FunctionBody : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> statements;
  FunctionBody() : AstNode(NodeType::FunctionBody){};
};

class FunctionArguments : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> arguments;
  FunctionArguments() : AstNode(NodeType::FunctionArguments){};
};

class FunctionCall : public AstNode {
public:
  Function *function_name;
  std::unique_ptr<FunctionArguments> arguments;
  FunctionCall() : AstNode(NodeType::FunctionCall){};
};

class ReturnStatement : public AstNode {
public:
  std::unique_ptr<AstNode> expression;
  ReturnStatement() : AstNode(NodeType::ReturnStatement){};
};

class StructField : public AstNode {
public:
  Variable *field;
  VarType *type;
  bool is_mutable;
  StructField() : AstNode(NodeType::StructField){};
  StructField(Variable *field, VarType *type, bool is_mutable)
      : field(field), type(type), is_mutable(is_mutable) {}
};

class StructDefinition : public AstNode {
public:
  std::string struct_name;
  std::vector<std::unique_ptr<StructField>> body;
  StructDefinition() : AstNode(NodeType::StructDefinition){};
  void add_field(StructField *field);
  void set_name(Identifier *name);
};

class StructFieldAssignment : public AstNode {
public:
  std::string field_name;
  std::unique_ptr<AstNode> value;
  StructFieldAssignment() : AstNode(NodeType::StructFieldAssignment){};
};

class StructFieldAccess : public AstNode {
public:
  std::string field_name;
  StructFieldAccess() : AstNode(NodeType::StructFieldAccess){};
};

class BinaryExpression : public AstNode {
public:
  Operator op;
  std::unique_ptr<AstNode> lhs;
  std::unique_ptr<AstNode> rhs;
  BinaryExpression() : AstNode(NodeType::BinaryExpression){};
  BinaryExpression(Operator *op) : AstNode(NodeType::BinaryExpression){};
  void set_operator(Operator *op);
  void set_lhs(AstNode *lhs);
  void set_rhs(AstNode *rhs);
};

class UnaryExpression : public AstNode {
public:
  Operator op;
  std::unique_ptr<AstNode> expression;
  UnaryExpression() : AstNode(NodeType::UnaryExpression){};
  UnaryExpression(Operator *op) : AstNode(NodeType::UnaryExpression){};
  void set_operator(Operator *op);
  void set_expression(AstNode *expression);
};

class LetExpression : public AstNode {
public:
  Variable *variable;
  std::unique_ptr<VarType> type;
  std::unique_ptr<AstNode> expression;
  LetExpression() : AstNode(NodeType::LetExpression){};
};

class EnumDefinition : public AstNode {
public:
  std::string enum_name;
  std::vector<std::unique_ptr<AstNode>> members;
  EnumDefinition() : AstNode(NodeType::EnumDefinition){};
};
class EnumMember : public AstNode {
public:
  std::string member_name;
  EnumMember() : AstNode(NodeType::EnumMember){};
};

class EnumMemberList : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> members;
  EnumMemberList() : AstNode(NodeType::EnumMemberList){};
};

class Statement : public AstNode {
public:
  std::unique_ptr<AstNode> statement;
  Statement() : AstNode(NodeType::Statement){};
};

class Expression : public AstNode {
public:
  std::unique_ptr<AstNode> expression;
  Expression() : AstNode(NodeType::Expression){};
};

class VariableAssignment : public AstNode {
public:
  Variable *variable;
  std::unique_ptr<AstNode> expression;
  VariableAssignment() : AstNode(NodeType::VariableAssignment){};
};

class StructDefBody : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> fields;
};

class Method : public AstNode {
public:
  std::string method_name;
  StructDefinition *associated_struct;
  std::unique_ptr<AstNode> parameters;
  std::unique_ptr<AstNode> return_type;
  std::unique_ptr<AstNode> body;
};

class MethodCall : public AstNode {
public:
  Method *method;
  std::unique_ptr<AstNode> arguments;
};
