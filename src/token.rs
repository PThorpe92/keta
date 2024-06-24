use std::{
    error::Error,
    fmt::{Display, Formatter},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
}
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Literal(DataType),
    Keyword(Keyword),
    Identifier(String),
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Underscore,
    Asterisk,
    Semicolon,
    Colon,
    ColonColon,
    Plus,
    Minus,
    Not,
    Equals,
    Lt,
    Gt,
    EqEq,
    NotEq,
    Mod,
    Caret,
    LogicalAnd,
    LogicalOr,
    LeftShift,
    RightShift,
    Arrow,
    GtEquals,
    MinusEquals,
    MulEquals,
    StarStar,
    DivEquals,
    PlusEquals,
    Ternary,
    LtEquals,
    Ampersand,
    Period,
    Pipe,
    Comma,
    EOF,
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Div,
    Mod,
    Mul,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Pow,
    LeftShift,
    RightShift,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEq,
    GreaterThan,
    GreaterThanOrEq,
    Call,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "+",
                Self::Sub => "-",
                Self::Div => "/",
                Self::Mod => "%",
                Self::Mul => "*",
                Self::BitwiseAnd => "&",
                Self::BitwiseOr => "|",
                Self::BitwiseXor => "^",
                Self::Pow => "**",
                Self::LeftShift => "<<",
                Self::RightShift => ">>",
                Self::Equal => "==",
                Self::NotEqual => "!=",
                Self::LessThan => "<",
                Self::LessThanOrEq => "<=",
                Self::GreaterThanOrEq => ">=",
                Self::GreaterThan => ">",
                Self::Call => "(",
            }
        )
    }
}

impl TryFrom<TokenType> for Operator {
    type Error = String;
    fn try_from(value: TokenType) -> Result<Self, String> {
        match value {
            TokenType::Ampersand => Ok(Self::BitwiseAnd),
            TokenType::Plus => Ok(Self::Add),
            TokenType::Minus => Ok(Self::Sub),
            TokenType::Asterisk => Ok(Self::Mul),
            TokenType::DivEquals => Ok(Self::Div),
            TokenType::Mod => Ok(Self::Mod),
            TokenType::Pipe => Ok(Self::BitwiseOr),
            TokenType::Caret => Ok(Self::BitwiseXor),
            TokenType::StarStar => Ok(Self::Pow),
            TokenType::LeftShift => Ok(Self::LeftShift),
            TokenType::RightShift => Ok(Self::RightShift),
            TokenType::LParen => Ok(Self::Call),
            _ => Err("Invalid operator".to_string()),
        }
    }
}

#[derive(Debug, Hash, Clone, PartialEq)]
pub enum DataType {
    String(Option<String>),
    Integer(Option<i64>),
    Float(Option<String>),
    Boolean(bool),
    Char(char),
    Array(Vec<Box<DataType>>),
    Reference(Box<DataType>),
    Void,
    Error(String),
    Result(Box<(DataType, DataType)>),
    Option(Box<DataType>),
    Inferred,
}

impl DataType {
    pub fn from_token_type(token_type: &TokenType) -> Result<Self, Box<dyn Error>> {
        match token_type {
            TokenType::Literal(lit) => Ok(lit.clone()),
            _ => Err("Invalid DataType".into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Const,
    Let,
    Mut,
    Float,
    Fn,
    If,
    Match,
    Else,
    Struct,
    Void,
    String,
    Result,
    Ok,
    Error,
    Bool,
    Option,
    Int,
    Some,
    None,
    For,
    Import,
    Return,
    In,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::String(str) => write!(f, "String: {}", str),
            DataType::Integer(int) => write!(f, "Integer: {}", int),
            DataType::Float(float) => write!(f, "Float: {}", float),
            DataType::Option(opt) => write!(f, "Option<{}>", opt),
            DataType::Result(res) => write!(f, "Result<{:?}>", res),
            DataType::Void => write!(f, "Void"),
            DataType::Error(err) => write!(f, "Error: {}", err),
            DataType::Boolean(b) => write!(f, "Boolean: {}", b),
            DataType::Char(ch) => write!(f, "Char: {}", ch),
            DataType::Array(lit) => write!(f, "Array<{:?}>", lit[0]),
            DataType::Reference(ident) => write!(f, "Reference<{:?}>", ident),
            DataType::None => write!(f, "None"),
        }
    }
}
impl TokenType {
    pub fn is_type(&self) -> bool {
        match self {
            Self::Keyword(kw) => matches!(
                kw,
                Keyword::Struct | Keyword::String | Keyword::Bool | Keyword::Int | Keyword::Void
            ),
            Self::Identifier(_) => true, // possibly a user defined type
            _ => false,
        }
    }
}

impl Span {
    pub fn new(st_ed: (usize, usize), line: usize) -> Self {
        Self {
            start: st_ed.0,
            end: st_ed.1,
            line,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_type)?;
        write!(f, " at line: {}", self.span.line)
    }
}

impl Token {
    pub fn new(token_type: TokenType, span: (usize, usize), line: usize) -> Self {
        Self {
            token_type,
            span: Span::new(span, line),
        }
    }
}
impl From<TokenType> for String {
    fn from(value: TokenType) -> String {
        match value {
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",
            TokenType::LBracket => "[",
            TokenType::RBracket => "]",
            TokenType::Underscore => "_",
            TokenType::Asterisk => "*",
            TokenType::Semicolon => ";",
            TokenType::Colon => ":",
            TokenType::ColonColon => "::",
            TokenType::Ampersand => "&",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Not => "!",
            TokenType::Equals => "=",
            TokenType::Lt => "<",
            TokenType::Gt => ">",
            TokenType::Pipe => "|",
            TokenType::EqEq => "==",
            TokenType::NotEq => "!=",
            TokenType::Mod => "%",
            TokenType::Caret => "^",
            TokenType::LogicalAnd => "&&",
            TokenType::LogicalOr => "||",
            TokenType::Arrow => "->",
            TokenType::GtEquals => ">=",
            TokenType::MinusEquals => "-=",
            TokenType::StarStar => "**",
            TokenType::MulEquals => "*=",
            TokenType::DivEquals => "/=",
            TokenType::PlusEquals => "+=",
            TokenType::LeftShift => "<<",
            TokenType::RightShift => ">>",
            TokenType::Ternary => "?",
            TokenType::LtEquals => "<=",
            TokenType::Period => ".",
            TokenType::Comma => ",",
            TokenType::EOF => "EOF",
            TokenType::Literal(literal) => {
                return match literal {
                    DataType::String(str) => format!("String: {}", str),
                    DataType::Integer(int) => format!("Integer: {}", int),
                    DataType::Float(float) => format!("Float: {}", float),
                    DataType::Option(opt) => format!("Option<{:?}>", opt),
                    DataType::Result(res) => format!("Result<{:?}>", res),
                    DataType::Void => "Void".to_string(),
                    DataType::Error(err) => format!("Error: {}", err),
                    DataType::Boolean(b) => format!("Boolean: {}", b),
                    DataType::Char(ch) => format!("Char: {}", ch),
                    DataType::Array(lit) => format!("Array<{:?}>", lit[0]),
                    DataType::Reference(ident) => format!("Reference<{:?}>", ident),
                    DataType::None => "None".to_string(),
                };
            }
            TokenType::Identifier(ident) => {
                return format!("Identifier: {}", ident);
            }
            TokenType::Keyword(kw) => kw.into(),
            TokenType::Whitespace => "whitespace",
        }
        .to_string()
    }
}

impl TokenType {
    pub fn from_char(c: char) -> Option<TokenType> {
        match c {
            '(' => Some(Self::LParen),
            ')' => Some(Self::RParen),
            '{' => Some(Self::LBrace),
            '}' => Some(Self::RBrace),
            '[' => Some(Self::LBracket),
            ']' => Some(Self::RBracket),
            '_' => Some(Self::Underscore),
            '*' => Some(Self::Asterisk),
            '+' => Some(Self::Plus),
            ':' => Some(Self::Colon),
            '<' => Some(Self::Lt),
            '>' => Some(Self::Gt),
            ';' => Some(Self::Semicolon),
            '-' => Some(Self::Minus),
            '!' => Some(Self::Not),
            '=' => Some(Self::Equals),
            '%' => Some(Self::Mod),
            '^' => Some(Self::Caret),
            '&' => Some(Self::Ampersand),
            '.' => Some(Self::Period),
            ',' => Some(Self::Comma),
            '|' => Some(Self::Pipe),
            _ => None,
        }
    }
}

impl From<Keyword> for &str {
    fn from(value: Keyword) -> &'static str {
        match value {
            Keyword::Const => "const",
            Keyword::Let => "let",
            Keyword::Mut => "mut",
            Keyword::Float => "float",
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::Match => "match",
            Keyword::Else => "else",
            Keyword::Struct => "struct",
            Keyword::Void => "void",
            Keyword::String => "str",
            Keyword::Result => "result",
            Keyword::Ok => "ok",
            Keyword::Error => "error",
            Keyword::Bool => "bool",
            Keyword::Int => "int",
            Keyword::Option => "option",
            Keyword::Some => "Some",
            Keyword::None => "None",
            Keyword::For => "for",
            Keyword::Import => "import",
            Keyword::Return => "return",
            Keyword::In => "in",
        }
    }
}

impl From<&str> for TokenType {
    fn from(value: &str) -> Self {
        match value {
            "true" => Self::Literal(DataType::Boolean(true)),
            "false" => Self::Literal(DataType::Boolean(false)),
            "let" => Self::Keyword(Keyword::Let),
            "mut" => Self::Keyword(Keyword::Mut),
            "fn" => Self::Keyword(Keyword::Fn),
            "if" => Self::Keyword(Keyword::If),
            "match" => Self::Keyword(Keyword::Match),
            "else" => Self::Keyword(Keyword::Else),
            "struct" => Self::Keyword(Keyword::Struct),
            "str" => Self::Keyword(Keyword::String),
            "bool" => Self::Keyword(Keyword::Bool),
            "int" => Self::Keyword(Keyword::Int),
            "opt" => Self::Keyword(Keyword::Option),
            "void" => Self::Keyword(Keyword::Void),
            "float" => Self::Keyword(Keyword::Float),
            "for" => Self::Keyword(Keyword::For),
            "import" => Self::Keyword(Keyword::Import),
            "return" => Self::Keyword(Keyword::Return),
            "ok" => Self::Keyword(Keyword::Ok),
            "err" => Self::Keyword(Keyword::Error),
            "in" => Self::Keyword(Keyword::In),
            _ => Self::Identifier(value.to_string()),
        }
    }
}
