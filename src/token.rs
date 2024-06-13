use std::{
    error::Error,
    fmt::{Display, Formatter},
};

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}
#[derive(Debug, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
}
#[derive(Debug, PartialEq)]
pub enum TokenType {
    Literal(Literal),
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
    GreaterThan,
    Call,
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

#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Char(char),
    Array(Vec<Box<Literal>>),
    Reference(Box<Literal>),
}

impl Literal {
    pub fn from_token_type(token_type: TokenType) -> Result<Self, Box<dyn Error>> {
        match token_type {
            TokenType::Literal(lit) => Ok(lit),
            _ => Err("Invalid literal".into()),
        }
    }
}

#[derive(Debug, PartialEq)]
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
    With,
    Result,
    Ok,
    Error,
    Variant,
    Union,
    Bool,
    Option,
    Int,
    Some,
    None,
    Public,
    Static,
    For,
    Yield,
    Import,
    Return,
}
impl TokenType {
    pub fn is_type(&self) -> bool {
        match self {
            Self::Keyword(kw) => match kw {
                Keyword::Union
                | Keyword::Struct
                | Keyword::String
                | Keyword::Bool
                | Keyword::Int
                | Keyword::Void => true,
                _ => false,
            },
            Self::Identifier(ident) => true, // possibly a user defined type
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
impl Into<&str> for TokenType {
    fn into(self) -> &'static str {
        match self {
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LBrace => "{",
            Self::RBrace => "}",
            Self::LBracket => "[",
            Self::RBracket => "]",
            Self::Underscore => "_",
            Self::Asterisk => "*",
            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::ColonColon => "::",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Not => "!",
            Self::Equals => "=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Pipe => "|",
            Self::EqEq => "==",
            Self::NotEq => "!=",
            Self::Mod => "%",
            Self::Caret => "^",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
            Self::Arrow => "->",
            Self::GtEquals => ">=",
            Self::MinusEquals => "-=",
            Self::StarStar => "**",
            Self::MulEquals => "*=",
            Self::DivEquals => "/=",
            Self::PlusEquals => "+=",
            Self::LeftShift => "<<",
            Self::RightShift => ">>",
            Self::Ternary => "?",
            Self::LtEquals => "<=",
            Self::Ampersand => "&",
            Self::Period => ".",
            Self::Comma => ",",
            Self::EOF => "EOF",
            Self::Whitespace => "Whitespace",
            Self::Literal(lit) => match lit {
                Literal::String(str) => format!("String: {}", str).as_str(),
                Literal::Integer(int) => format!("Integer: {}", int).as_str(),
                Literal::Float(float) => format!("Float: {}", float).as_str(),
                Literal::Boolean(b) => format!("Boolean: {}", b).as_str(),
                Literal::Char(ch) => format!("Char: {}", ch).as_str(),
                Literal::Array(lit) => format!("Array<{:?}>", lit[0]).as_str(),
                Literal::Reference(ident) => format!("Reference<{:?}>", ident).as_str(),
            },
            Self::Identifier(ident) => &ident,
            Self::Keyword(kw) => match kw {
                Keyword::Let => "let",
                Keyword::Mut => "mut",
                Keyword::Fn => "fn",
                Keyword::If => "if",
                Keyword::Match => "match",
                Keyword::Else => "else",
                Keyword::Struct => "struct",
                Keyword::Void => "void",
                Keyword::String => "String",
                Keyword::Bool => "bool",
                Keyword::Option => "opt",
                Keyword::Int => "int",
                Keyword::Some => "Some",
                Keyword::None => "None",
                Keyword::Public => "public",
                Keyword::Static => "static",
                Keyword::For => "for",
                Keyword::Yield => "yield",
                Keyword::Import => "import",
                Keyword::Return => "return",
                Keyword::Match => "match",
                Keyword::With => "with",
                Keyword::Result => "result",
                Keyword::Ok => "Ok",
                Keyword::Error => "Error",
                Keyword::Float => "float",
                Keyword::Variant => "variant",
                Keyword::Union => "union",
                Keyword::Const => "const",
            },
        }
    }
}
impl TokenType {
    pub fn from_char(c: char) -> Option<Self> {
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

impl From<&str> for TokenType {
    fn from(value: &str) -> Self {
        match value {
            "true" => Self::Literal(Literal::Boolean(true)),
            "false" => Self::Literal(Literal::Boolean(false)),
            "let" => Self::Keyword(Keyword::Let),
            "mut" => Self::Keyword(Keyword::Mut),
            "fn" => Self::Keyword(Keyword::Fn),
            "if" => Self::Keyword(Keyword::If),
            "match" => Self::Keyword(Keyword::Match),
            "else" => Self::Keyword(Keyword::Else),
            "struct" => Self::Keyword(Keyword::Struct),
            "String" => Self::Keyword(Keyword::String),
            "bool" => Self::Keyword(Keyword::Bool),
            "int" => Self::Keyword(Keyword::Int),
            "opt" => Self::Keyword(Keyword::Option),
            "Some" => Self::Keyword(Keyword::Some),
            "None" => Self::Keyword(Keyword::None),
            "void" => Self::Keyword(Keyword::Void),
            "public" => Self::Keyword(Keyword::Public),
            "static" => Self::Keyword(Keyword::Static),
            "float" => Self::Keyword(Keyword::Float),
            "for" => Self::Keyword(Keyword::For),
            "yield" => Self::Keyword(Keyword::Yield),
            "import" => Self::Keyword(Keyword::Import),
            "return" => Self::Keyword(Keyword::Return),
            _ => Self::Identifier(value.to_string()),
        }
    }
}
