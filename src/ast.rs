use crate::parser::ParsedSpan;
use crate::token::{self, DataType};
use std::boxed::Box;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub namespace: String,
    pub body: Vec<AstNode>,
}

pub struct EvaluationContext(pub HashMap<String, Option<DataType>>);

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
    pub fn from(op: token::Operator) -> Self {
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
        Self(HashMap::new())
    }
}
#[derive(Debug)]
pub struct Block {
    pub span: ParsedSpan,
    pub body: Vec<AstNode>,
}

#[derive(Debug)]
pub struct AstNode {
    pub span: ParsedSpan,
    pub node_type: NodeType,
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
    Identifier(String),
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

#[derive(Debug)]
pub struct BinaryOp {
    pub left: Expression,
    pub operator: token::Operator,
    pub right: Expression,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub operator: String,
    pub expression: Expression,
}
#[derive(Debug)]
pub struct Assignment {
    pub identifier: String,
    pub operator: token::Operator,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct IfStatement {
    pub span: ParsedSpan,
    pub condition: BinaryOp,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

#[derive(Debug)]
pub struct ForLoop {
    pub variable: Variable,
    pub range: (i64, i64),
    pub body: Block,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub struct FnCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}
#[derive(Debug)]
pub enum FnArg {
    DataType(crate::token::DataType),
    Variable(Identifier),
}

#[derive(Debug)]
pub struct Return {
    pub expression: Expression,
}

#[derive(Debug)]
pub struct ImportStmt {
    pub path: String,
}

#[derive(Debug)]
pub enum Definition {
    Variable(VariableDef),
    Variant(VariantDef),
    Function(FunctionDef),
    Struct(StructDef),
    Union(UnionDef),
}

#[derive(Debug)]
pub struct UnionDef {
    pub name: Identifier,
    pub variants: Vec<StructField>,
}

#[derive(Debug)]
pub struct VariableDef {
    pub name: Identifier,
    pub value: Expression,
    pub is_const: bool,
}

#[derive(Debug)]
pub struct VariantDef {
    pub name: Identifier,
    pub variants: Vec<Identifier>,
}

#[derive(Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Statement {
    ImportStmt(Box<ImportStmt>),
    Assignment(Assignment),
    If(Box<IfStatement>),
    ForLoop(Box<ForLoop>),
    Return(Box<Return>),
    Expression(Expression),
    Block(Block),
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: Identifier,
    pub return_type: DataType,
    pub parameters: Vec<FnParam>,
    pub body: Block,
}

#[derive(Debug)]
pub struct StructField {
    pub name: String,
    pub data_type: DataType,
}
#[derive(Debug)]
pub struct FnParam {
    pub name: Identifier,
    pub data_type: DataType,
}
