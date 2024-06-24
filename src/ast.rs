use crate::parser::ParsedSpan;
use crate::token::{self, DataType};
use std::boxed::Box;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct Program {
    pub namespace: String,
    pub body: Vec<AstNode>,
}

impl Program {
    pub fn find_function(&self, name: &str) -> Option<&FunctionDef> {
        for node in &self.body {
            if let NodeType::Definition(Definition::Function(func)) = &node.node_type {
                if func.name.0 == name {
                    return Some(func);
                }
            }
        }
        None
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for node in &self.body {
            writeln!(f, "{}", node)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct EvaluationContext {
    pub variables: HashMap<Identifier, Option<ScopedVar>>,
    pub functions: HashMap<Identifier, usize>,
}

#[derive(Debug, PartialEq, Hash, Clone)]
pub struct ScopedVar {
    pub scope: usize,
    pub value: DataType,
}

#[derive(Debug)]
pub struct AstBuilder {
    pub program: Program,
    pub context: EvaluationContext,
}

impl AstBuilder {
    pub fn new(namespace: String) -> Self {
        Self {
            program: Program {
                namespace,
                body: Vec::new(),
            },
            context: EvaluationContext::new(),
        }
    }
    pub fn push(&mut self, node: AstNode) {
        self.program.body.push(node);
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    pub fn from(op: &token::Operator) -> Self {
        match op {
            token::Operator::BitwiseAnd
            | token::Operator::BitwiseOr
            | token::Operator::BitwiseXor => Self::Prefix,
            token::Operator::Add | token::Operator::Sub => Self::Sum,
            token::Operator::Mul | token::Operator::Div => Self::Product,
            token::Operator::Equal | token::Operator::NotEqual => Self::Equals,
            token::Operator::LessThan | token::Operator::GreaterThan => Self::LessGreater,
            token::Operator::Call => Self::Call,
            _ => Self::Lowest,
        }
    }
}

impl EvaluationContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub span: ParsedSpan,
    pub body: Vec<AstNode>,
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for node in &self.body {
            write!(f, "{}", node)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct AstNode {
    pub span: ParsedSpan,
    pub node_type: NodeType,
}
impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.node_type)
    }
}

impl std::fmt::Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            NodeType::Expression(expr) => write!(f, "{}", expr),
            NodeType::Statement(stmt) => write!(f, "{}", stmt),
            NodeType::Definition(def) => write!(f, "{}", def),
        }
    }
}

#[derive(Debug)]
pub enum NodeType {
    Expression(Expression),
    Statement(Statement),
    Definition(Definition),
}

#[derive(Debug)]
pub enum Expression {
    Literal(DataType),
    Identifier(Identifier),
    BinaryOp(Box<BinaryOp>),
    UnaryOp(Box<UnaryOp>),
    FnCall(Box<FnCall>),
    Return(Box<Return>),
    Variable(Box<Variable>),
    FnArgs(Vec<FnArg>),
    FnParams(Vec<FnParam>),
    DataType(DataType),
    IfStatement(Box<IfStatement>),
}
impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Literal(lit) => writeln!(f, "Literal: {}", lit),
            Expression::Identifier(ident) => writeln!(f, "Identifier: {}", ident.0),
            Expression::Variable(var) => writeln!(f, "Variable: {}", var.name.0),
            Expression::BinaryOp(op) => writeln!(f, "BinaryOp: {}", op),
            Expression::UnaryOp(op) => writeln!(f, "UnaryOp: {}", op),
            Expression::FnCall(call) => writeln!(f, "FnCall: {}", call),
            Expression::Return(ret) => writeln!(f, "Return: {}", ret),
            Expression::FnArgs(args) => writeln!(f, "FnArgs: {:?}", args),
            Expression::FnParams(params) => writeln!(f, "FnParams: {:?}", params),
            Expression::DataType(data_type) => writeln!(f, "DataType: {}", data_type),
            Expression::IfStatement(stmt) => writeln!(f, "IfStatement: {}", stmt),
        }
    }
}

#[derive(Debug)]
pub struct BinaryOp {
    pub left: Expression,
    pub operator: token::Operator,
    pub right: Expression,
}
impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct UnaryOp {
    pub operator: String,
    pub expression: Expression,
}
impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}{}", self.operator, self.expression)
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub identifier: Identifier,
    pub operator: token::Operator,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct IfStatement {
    pub span: ParsedSpan,
    pub condition: Expression,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

impl std::fmt::Display for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}", self.condition)?;
        writeln!(f, "{}", self.consequence)?;
        if let Some(alt) = &self.alternative {
            writeln!(f, "{}", alt)?
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ForLoop {
    pub variable: Variable,
    pub iterable: Expression,
    pub body: Block,
}

impl std::fmt::Display for ForLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}", self.variable)?;
        writeln!(f, "{}", self.iterable)?;
        writeln!(f, "{}", self.body)
    }
}

#[derive(Debug)]
pub struct Variable {
    pub name: Identifier,
    pub value: Expression,
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Name: {}", self.name)?;
        writeln!(f, "Value: {}", self.value)
    }
}

#[derive(Debug)]
pub struct FnCall {
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}
impl Display for FnCall {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "Name: {}\nArgs:", self.name)?;
        for arg in &self.arguments {
            writeln!(f, "{}", arg)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct FnArg(pub Expression);
impl Display for FnArg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct Return {
    pub expression: Expression,
}

impl std::fmt::Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}", self.expression)
    }
}
#[derive(Debug)]
pub struct ImportStmt {
    pub path: Identifier,
}

#[derive(Debug)]
pub enum Definition {
    Variable(VariableDef),
    Function(FunctionDef),
    Struct(StructDef),
}

impl Display for Definition {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Definition::Variable(var) => writeln!(f, "Variable: {}", var),
            Definition::Function(func) => writeln!(f, "Funciton: {}", func),
            Definition::Struct(struct_def) => writeln!(f, "Struct: {}", struct_def),
        }
    }
}

#[derive(Debug)]
pub struct VariableDef {
    pub name: Identifier,
    pub value: Expression,
    pub is_const: bool,
}
impl Display for VariableDef {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}", self.name)?;
        writeln!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct StructDef {
    pub name: Identifier,
    pub fields: Vec<StructField>,
}
impl Display for StructDef {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}", self.name)?;
        for field in &self.fields {
            writeln!(f, "{}", field)?;
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum Statement {
    ImportStmt(Box<ImportStmt>),
    Assignment(Assignment),
    If(Box<IfStatement>),
    ForLoop(Box<ForLoop>),
    Return(Box<Return>),
    Expression(Expression),
    Block(Block),
    FunctionDef(Box<FunctionDef>),
}
impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::ImportStmt(stmt) => writeln!(f, "ImportStmt: {}", stmt.path.0),
            Statement::Assignment(assign) => writeln!(f, "Assignment: {}", assign.identifier.0),
            Statement::If(stmt) => writeln!(f, "If: {}", stmt),
            Statement::ForLoop(stmt) => writeln!(f, "ForLoop: {}", stmt),
            Statement::Return(stmt) => writeln!(f, "Return: {}", stmt),
            Statement::Expression(expr) => writeln!(f, "Expression: {}", expr),
            Statement::Block(block) => writeln!(f, "Block: {}", block),
            Statement::FunctionDef(func) => writeln!(f, "FunctionDef: {}", func),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: Identifier,
    pub return_type: DataType,
    pub parameters: Vec<FnParam>,
    pub body: Block,
}
impl Display for FunctionDef {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "Name: {}", self.name)?;
        writeln!(f, "Return type: {}", self.return_type)?;
        for param in &self.parameters {
            writeln!(f, "Param: {}", param)?;
        }
        writeln!(f, "Body: {}", self.body)
    }
}

#[derive(Debug)]
pub struct StructField {
    pub name: Identifier,
    pub data_type: DataType,
}
impl Display for StructField {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}", self.name)?;
        writeln!(f, "{}", self.data_type)
    }
}
#[derive(Debug)]
pub struct FnParam {
    pub name: Identifier,
    pub data_type: DataType,
}
impl Display for FnParam {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}", self.name)?;
        writeln!(f, "{}", self.data_type)
    }
}
