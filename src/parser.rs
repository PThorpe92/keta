use crate::{
    ast::{self, EvaluationContext, Identifier, VariableDef},
    lexer,
    token::{self, DataType, Token, TokenType},
};

pub struct Parser<'a> {
    tokens: std::iter::Peekable<TokenStream>,
    pub identifiers: ast::EvaluationContext,
    current_token: Token,
    current_scope: usize,
    is_global: bool,
    namespace: &'a str,
}

pub struct TokenStream {
    position: usize,
    tokens: Vec<Token>,
}
impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            position: 0,
            tokens,
        }
    }
}

impl Iterator for TokenStream {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.position).cloned();
        self.position += 1;
        token
    }
}

#[derive(Debug)]
pub struct ParsedSpan {
    pub line_start: usize,
    pub line_end: usize,
}

impl ParsedSpan {
    pub fn from_tokens(start: &usize, end: &usize) -> Self {
        Self {
            line_start: *start,
            line_end: *end,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a lexer::Lexer) -> Self {
        let tokens = lexer.tokens.clone();
        let mut peekable = TokenStream::new(tokens).peekable();
        let first = peekable
            .next()
            .unwrap_or(Token::new(TokenType::Whitespace, (0, 0), 1));
        Self {
            tokens: peekable,
            identifiers: ast::EvaluationContext::new(),
            current_token: first,
            current_scope: 0,
            namespace: &lexer.filename,
            is_global: true,
        }
    }
    pub fn advance(&mut self) {
        self.current_token = self
            .tokens
            .next()
            .unwrap_or(Token::new(TokenType::EOF, (0, 0), 0));
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn parse(&mut self) -> ast::AstBuilder {
        let mut builder = ast::AstBuilder::new(self.namespace.to_owned());
        builder.program = self.parse_program(&mut builder.context);
        builder
    }

    fn assert_token(&mut self, expected: TokenType) {
        if std::mem::discriminant(&self.current_token.token_type)
            != std::mem::discriminant(&expected)
        {
            panic!(
                "Expected token {:?}, got {:?}",
                expected, self.current_token.token_type
            );
        }
        self.advance();
    }

    fn parse_program(&mut self, ctx: &mut ast::EvaluationContext) -> ast::Program {
        let mut program = ast::Program {
            namespace: self.namespace.to_owned(),
            body: Vec::new(),
        };
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::EOF => break,
                _ => {
                    program.body.push(self.parse_statement(ctx));
                }
            }
        }
    }

    fn parse_statement(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        match self.current_token.token_type {
            TokenType::Keyword(kw) => match kw {
                token::Keyword::Let => self.parse_let(ctx),
                token::Keyword::Import => self.parse_import(ctx),
                token::Keyword::Fn => self.parse_function(ctx),
                token::Keyword::Struct => self.parse_struct_def(ctx),
                token::Keyword::Const => self.parse_const(ctx),
                token::Keyword::If => self.parse_if(ctx),
                token::Keyword::For => self.parse_for(ctx),
                token::Keyword::Return => self.parse_return(ctx),
                _ => self.parse_expression(ctx),
            },
            TokenType::Identifier(_) => self.parse_assignment(ctx),
        }
    }

    fn parse_if(&mut self, ctx: &mut EvaluationContext) -> ast::AstNode {
        self.advance();
        let condition = self.parse_expression(ctx);
        let body = self.parse_block(ctx);
        let mut else_block = None;
        if let TokenType::Keyword(token::Keyword::Else) = self.current_token.token_type {
            self.advance();
            else_block = Some(self.parse_block(ctx));
        }
        ast::AstNode {
            span: ParsedSpan {
                line_start: self.current_token.span.line,
                line_end: self.current_token.span.line,
            },
            node_type: ast::NodeType::Statement(ast::Statement::IfStmt(Box::new(ast::IfStmt {
                condition,
                body,
                else_block,
            }))),
        }
    }

    fn parse_assignment(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        let name = self.parse_identifier(ctx);
        if ctx.variables.get(&name).is_none() {
            panic!("Variable {} not found", name.0);
        }
        self.assert_token(TokenType::Equals);
        let value = self.parse_expression(ctx);
        ast::AstNode {
            span: ParsedSpan {
                line_start: self.current_token.span.line,
                line_end: self.current_token.span.line,
            },
            node_type: ast::NodeType::Statement(ast::Statement::Assignment(ast::Assignment {
                identifier: name,
                operator: token::Operator::Equal,
                expression: value,
            })),
        }
    }

    fn parse_let(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        self.advance();
        let name = self.parse_identifier(ctx);
        let data_type = self.parse_data_type();
        let value = if let TokenType::Equals = self.current_token.token_type {
            self.advance();
            Some(self.parse_expression(ctx))
        } else {
            None
        };
        ctx.variables.insert(
            name.clone(),
            Some(ast::ScopedVar {
                scope: self.current_scope,
                value: data_type.clone(),
            }),
        );
        ast::AstNode {
            span: ParsedSpan {
                line_start: self.current_token.span.line,
                line_end: self.current_token.span.line,
            },
            node_type: ast::NodeType::Definition(ast::Definition::Variable(VariableDef {
                name,
                value: value.unwrap(),
                is_const: false,
            })),
        }
    }
    fn parse_const(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        self.advance();
        let name = self.parse_identifier(ctx);
        let data_type = self.parse_data_type();
        self.assert_token(TokenType::Equals);
        let value = self.parse_expression(ctx);
        ctx.variables.insert(
            name.clone(),
            Some(ast::ScopedVar {
                scope: self.current_scope,
                value: data_type.clone(),
            }),
        );
        ast::AstNode {
            span: ParsedSpan {
                line_start: self.current_token.span.line,
                line_end: self.current_token.span.line,
            },
            node_type: ast::NodeType::Definition(ast::Definition::Variable(VariableDef {
                name,
                value,
                is_const: true,
            })),
        }
    }

    fn parse_struct_def(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        self.advance();
        let name = self.parse_identifier(ctx);
        let fields = self.parse_struct_fields(ctx);
        ast::AstNode {
            span: ParsedSpan {
                line_start: self.current_token.span.line,
                line_end: self.current_token.span.line,
            },
            node_type: ast::NodeType::Definition(ast::Definition::Struct(ast::StructDef {
                name,
                fields,
            })),
        }
    }

    fn parse_struct_fields(&mut self, ctx: &mut ast::EvaluationContext) -> Vec<ast::StructField> {
        let mut fields = Vec::new();
        self.assert_token(TokenType::LBrace);
        while TokenType::RBrace != self.current_token.token_type {
            let name = self.parse_identifier(ctx);
            let data_type = self.parse_data_type();
            fields.push(ast::StructField { name, data_type });
        }
        self.assert_token(TokenType::RBrace);
        fields
    }

    fn parse_import(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        self.advance();
        let path = self.parse_identifier(ctx);
        ast::AstNode {
            span: ParsedSpan {
                line_start: self.current_token.span.line,
                line_end: self.current_token.span.line,
            },
            node_type: ast::NodeType::Statement(ast::Statement::ImportStmt(Box::new(
                ast::ImportStmt { path },
            ))),
        }
    }

    fn parse_function(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        let start_line = self.current_token.span.line;
        self.advance();
        let name = self.parse_identifier(ctx);
        let params = self.parse_parameters(ctx);
        let return_type = self.parse_return_type();
        let body = self.parse_block(ctx);
        ast::AstNode {
            span: ParsedSpan {
                line_start: self.current_token.span.line,
                line_end: self.current_token.span.line,
            },
            node_type: ast::NodeType::Statement(ast::Statement::FunctionDef(Box::new(
                ast::FunctionDef {
                    name,
                    parameters: params,
                    return_type,
                    body,
                },
            ))),
        }
    }

    fn parse_return_type(&mut self) -> DataType {
        if let TokenType::Arrow = self.current_token.token_type {
            self.advance();
            self.parse_data_type()
        } else {
            DataType::Void
        }
    }

    fn parse_block(&mut self, ctx: &mut EvaluationContext) -> ast::Block {
        let start_line = self.current_token.span.line;
        self.assert_token(TokenType::LBrace);
        self.is_global = false;
        let mut body = Vec::new();
        while let TokenType::RBrace = self.current_token.token_type {
            body.push(self.parse_statement(ctx));
        }
        self.assert_token(TokenType::RBrace);
        ast::Block {
            span: ParsedSpan::from_tokens(&start_line, &self.current_token.span.line),
            body,
        }
    }

    fn parse_data_type(&mut self) -> DataType {
        if let TokenType::Keyword(kw) = self.current_token.token_type {
            match kw {
                token::Keyword::Int => DataType::Integer(0),
                token::Keyword::Bool => DataType::Boolean(false),
                token::Keyword::String => DataType::String(String::new()),
                token::Keyword::Float => DataType::Float("0.0".to_string()),
                token::Keyword::Void => DataType::Void,
                token::Keyword::Option => {
                    self.advance();
                    self.assert_token(TokenType::Lt);
                    let inner = self.parse_data_type();
                    self.assert_token(TokenType::Gt);
                    DataType::Option(Box::new(inner))
                }
                token::Keyword::Result => {
                    self.advance();
                    self.assert_token(TokenType::Lt);
                    let inner = self.parse_data_type();
                    self.assert_token(TokenType::Comma);
                    let err = self.parse_data_type();
                    self.assert_token(TokenType::Gt);
                    DataType::Result(Box::new((inner, err)))
                }
                _ => unreachable!("Expected data type"),
            }
        } else {
            unreachable!("Expected data type");
        }
    }

    fn parse_identifier(&mut self, ctx: &mut ast::EvaluationContext) -> ast::Identifier {
        if let TokenType::Identifier(ident) = self.current_token.token_type.clone() {
            let identifier = ast::Identifier(ident);
            if ctx.variables.get(&identifier).is_none() {
                ctx.variables.insert(identifier.clone(), None);
            }
            self.advance();
            identifier
        } else {
            unreachable!("Expected identifier");
        }
    }

    fn parse_parameters(&mut self, ctx: &mut ast::EvaluationContext) -> Vec<ast::FnParam> {
        let mut params = Vec::new();
        self.assert_token(TokenType::LParen);
        while TokenType::RParen != self.current_token.token_type {
            match self.current_token.token_type {
                TokenType::Comma => {
                    self.advance();
                    continue;
                }
                TokenType::Identifier(_) => {
                    let name = self.parse_identifier(ctx);
                    let data_type = self.parse_data_type();
                    params.push(ast::FnParam { name, data_type });
                    ctx.variables.insert(
                        name,
                        Some(ast::ScopedVar {
                            scope: self.current_scope,
                            value: data_type.clone(),
                        }),
                    );
                }
                _ => panic!("Expected identifier"),
            }
        }
        self.assert_token(TokenType::RParen);
        params
    }

    fn parse_expression(&mut self, ctx: &mut ast::EvaluationContext) -> ast::Expression {
        match self.current_token.token_type {
            TokenType::Identifier(_) => {
                let ident = self.parse_identifier(ctx);
                if let TokenType::LParen = self.current_token.token_type {
                    self.parse_function_call(ctx, ident)
                } else {
                    ast::Expression::Identifier(ident)
                }
            }
            TokenType::LParen => {
                self.advance();
                let expr = self.parse_expression(ctx);
                self.assert_token(TokenType::RParen);
                expr
            }
            TokenType::Minus => {
                self.advance();
                let expr = self.parse_expression(ctx);
                ast::Expression::UnaryOp(Box::new(ast::UnaryOp {
                    operator: "-".to_string(),
                    expression: expr,
                }))
            }
            TokenType::Not => {
                self.advance();
                let expr = self.parse_expression(ctx);
                ast::Expression::UnaryOp(Box::new(ast::UnaryOp {
                    operator: "!".to_string(),
                    expression: expr,
                }))
            }
            TokenType::Literal(lit) => {
                self.advance();
                if token::Operator::try_from(self.current_token.token_type).is_ok() {
                    let operator =
                        token::Operator::try_from(self.current_token.token_type.clone()).unwrap();
                    self.advance();
                    let right = self.parse_expression(ctx);
                    return ast::Expression::BinaryOp(Box::new(ast::BinaryOp {
                        left: ast::Expression::Literal(lit),
                        operator,
                        right,
                    }));
                }
                ast::Expression::Literal(lit)
            }
            _ => {
                let left = self.parse_expression(ctx);
                let operator =
                    token::Operator::try_from(self.current_token.token_type.clone()).unwrap();
                self.advance();
                let right = self.parse_expression(ctx);
                ast::Expression::BinaryOp(Box::new(ast::BinaryOp {
                    left,
                    operator,
                    right,
                }))
            }
        }
    }
    fn parse_function_call(
        &mut self,
        ctx: &mut ast::EvaluationContext,
        ident: ast::Identifier,
    ) -> ast::Expression {
        let args = self.parse_arguments(ctx);
        ast::Expression::FnCall(Box::new(ast::FnCall {
            name: ident,
            arguments: args,
        }))
    }

    fn parse_arguments(&mut self, ctx: &mut ast::EvaluationContext) -> Vec<ast::Expression> {
        let mut args = Vec::new();
        self.assert_token(TokenType::LParen);
        while TokenType::RParen != self.current_token.token_type {
            match self.current_token.token_type {
                TokenType::Comma => {
                    self.advance();
                    continue;
                }
                _ => {
                    let arg = self.parse_expression(ctx);
                    args.push(arg);
                }
            }
        }
        self.assert_token(TokenType::RParen);
        args
    }
}
