#include "../lexer/token.hpp"
#include <memory>
#include <optional>
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
    Literal,
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
    MatchExpression,
    LetExpression,
    VariableTypeDef,
    VariableAssignment,
    MethodCall,
    TypeDeclaration,
    VarType,
    Variable,
    ElseBlock,
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
  Identifier *identifier = nullptr;
  AstNode() = default;
  explicit AstNode(NodeType type) : type(type) {}
  virtual ~AstNode() = default;

  NodeType get_type() const;
  void operator=(AstNode *node);
  void operator=(AstNode &node);
  void operator=(std::unique_ptr<AstNode> node);
  void operator=(std::unique_ptr<AstNode> &node);
  void set_left(AstNode *node);
  void set_right(AstNode *node);
  virtual void set_left(std::unique_ptr<AstNode> node);
  virtual void set_right(std::unique_ptr<AstNode> node);
  virtual void set_identifier(Identifier *ident);
  static AstNode *make_node(NodeType type);
  virtual AstNode *get_parent();
  virtual void set_parent(AstNode *node);
  AstNode *get_left();
  AstNode *get_right();
  virtual void print_node() const;
};

class Program : public AstNode {
public:
  Program() : AstNode(NodeType::Program) {}
};

class OpenModule : public AstNode {
public:
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
  VarType *var_type;
  Variable() : AstNode(NodeType::Variable){};
  void set_name(Identifier *name);
  void set_type(Keyword *type);
};

class FunctionParameters : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> parameters;
  void add_parameter(AstNode *parameter);
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
  void set_body(AstNode *body);
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
  void add_statement(std::unique_ptr<AstNode> statement);
  std::vector<std::unique_ptr<AstNode>> get_statements();
};

class FunctionArguments : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> arguments;
  FunctionArguments() : AstNode(NodeType::FunctionArguments){};
  void add_argument(AstNode *argument);
};

class FunctionCall : public AstNode {
public:
  Function *function_name;
  FunctionArguments *arguments;
  FunctionCall() : AstNode(NodeType::FunctionCall){};
  FunctionCall(Function *function_name, FunctionArguments *arguments)
      : function_name(function_name), arguments(arguments) {}
  void set_function_name(Function *function_name);
  void set_arguments(FunctionArguments *arguments);
};

class ReturnStatement : public AstNode {
public:
  AstNode *expression;
  ReturnStatement() : AstNode(NodeType::ReturnStatement){};
  ~ReturnStatement() = default;
  void operator=(AstNode *expression);
  void set_expression(AstNode *expression);
};

class StructField : public AstNode {
public:
  Variable *field;
  VarType *type;
  bool is_mutable;
  StructField() : AstNode(NodeType::StructField){};
  StructField(Variable *field, VarType *type, bool is_mutable)
      : field(field), type(type), is_mutable(is_mutable) {}
  void set_field(Variable *field);
  void set_type(VarType *type);
  void set_mutable(bool is_mutable);
};
class ElseBlock : public AstNode {
public:
  std::unique_ptr<AstNode> body;
  ElseBlock() : AstNode(NodeType::ElseBlock){};
  void set_body(AstNode *body);
};

class StructDefinition : public AstNode {
public:
  std::string struct_name;
  std::vector<StructField *> body;
  StructDefinition() : AstNode(NodeType::StructDefinition){};
  void add_field(StructField *field);
  void set_name(Identifier *name);
};
class MatchExpression : public AstNode {
public:
  std::unique_ptr<AstNode> expression;
  std::vector<std::unique_ptr<AstNode>> match_cases;
  void set_expression(AstNode *expression);
  void add_match_case(AstNode *match_case);
};

class StructFieldAssignment : public AstNode {
public:
  StructFieldAssignment() : AstNode(NodeType::StructFieldAssignment){};
};

class StructFieldAccess : public AstNode {
public:
  StructFieldAccess() : AstNode(NodeType::StructFieldAccess){};
};

class ParsedLiteral : public AstNode {
public:
  Literal value;
  ParsedLiteral(Literal &value) : AstNode(NodeType::Literal), value(value){};
  void set_value(Token &value);
};

class BinaryExpression : public AstNode {
public:
  Operator op;
  BinaryExpression() : AstNode(NodeType::BinaryExpression){};
  BinaryExpression(Operator *op) : AstNode(NodeType::BinaryExpression){};
  void set_operator(Operator *op);
};

class UnaryExpression : public AstNode {
public:
  Operator op;
  UnaryExpression() : AstNode(NodeType::UnaryExpression){};
  UnaryExpression(Operator *op) : AstNode(NodeType::UnaryExpression){};
  void set_operator(Operator *op);
};

class LetExpression : public AstNode {
public:
  LetExpression() : AstNode(NodeType::LetExpression){};
};

class EnumMember : public AstNode {
public:
  std::string member_name;
  std::optional<Variable *> associated_value;
  EnumMember() : AstNode(NodeType::EnumMember){};
};

class EnumDefinition : public AstNode {
public:
  std::string enum_name;
  std::vector<AstNode *> members;
  EnumDefinition() : AstNode(NodeType::EnumDefinition){};
  void add_member(AstNode *member);
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
  std::unique_ptr<AstNode> get_statement();
  void operator=(AstNode *statement);
  void set_statement(AstNode *statement);
};

class Expression : public AstNode {
public:
  std::unique_ptr<AstNode> expression;
  Expression() : AstNode(NodeType::Expression){};
  std::unique_ptr<AstNode> get_expression();
  void operator=(AstNode *statement);
  void set_expression(AstNode *expression);
};

class VariableAssignment : public AstNode {
public:
  Variable *variable;
  Expression expression;
  VariableAssignment() : AstNode(NodeType::VariableAssignment){};
  void set_variable(Variable *variable);
  void set_expression(Expression *expression);
};

class StructDefBody : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> fields;
  void add_field(AstNode *field);
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
