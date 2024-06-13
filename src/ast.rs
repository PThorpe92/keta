use crate::parser::ParsedSpan;
use crate::token::{self, Literal};
use std::boxed::Box;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub namespace: String,
    pub body: Vec<AstNode>,
}

pub struct EvaluationContext(HashMap<String, Literal>);

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
    Literal(Literal),
    Identifier(String),
    BinaryOp(Box<BinaryOp>),
    UnaryOp(Box<UnaryOp>),
    FnCall(Box<FnCall>),
    Return(Box<Return>),
    Variable(Box<Variable>),
    FnArgs(Vec<FnArg>),
    FnParams(Vec<FnParam>),
    DataType(DataType),
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
    pub condition: Expression,
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
    Literal(crate::token::Literal),
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

#[derive(Debug)]
pub enum DataType {
    Integer,
    Boolean,
    Float,
    String,
    Struct,
    Enum,
    Reference(Box<DataType>),
    UserDefined(Identifier),
    Result(Box<DataType>),
    Option(Box<DataType>),
}

impl DataType {
    pub fn from_token_type(token_type: &token::TokenType) -> Self {
        match token_type {
            token::TokenType::Keyword(kw) => match kw {
                token::Keyword::Int => Self::Integer,
                token::Keyword::Bool => Self::Boolean,
                token::Keyword:: => Self::Float,
                token::Keyword::String => Self::String,
                _ => Self::UserDefined(Identifier(kw.to_string())),
            },
            token::TokenType::Identifier(ident) => Self::UserDefined(Identifier(ident.to_string())),
            _ => Self::Integer,
        }
    }
    pub fn from_str(ident: &str) -> Self {
        match ident {
            "int" => Self::Integer,
            "bool" => Self::Boolean,
            "float" => Self::Float,
            "string" => Self::String,
            _ => Self::UserDefined(Identifier(ident.to_string())),
        }
    }
}
