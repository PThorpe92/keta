use crate::{
    ast::{self, EvaluationContext, Precedence, Variable, VariableDef},
    lexer,
    token::{self, DataType, Token, TokenType},
};
use nu_ansi_term::{AnsiString, Color, Style};
use tracing::debug;

pub struct Parser<'a> {
    tokens: std::iter::Peekable<TokenStream>,
    current_token: Token,
    pub current_scope: usize,
    pub is_global: bool,
    namespace: &'a str,
    input: &'a str,
    pub program: ast::Program,
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

#[derive(Debug, Clone)]
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
            current_token: first,
            current_scope: 0,
            namespace: &lexer.filename,
            is_global: true,
            input: &lexer.input,
            program: ast::Program {
                namespace: lexer.filename.to_owned(),
                body: Vec::new(),
            },
        }
    }

    fn increment_scope(&mut self) {
        self.is_global = false;
        self.current_scope += 1;
    }

    fn decrement_scope(&mut self) {
        self.current_scope -= 1;
        self.is_global = self.current_scope == 0;
    }

    pub fn advance(&mut self) {
        self.current_token = self
            .tokens
            .next()
            .unwrap_or(Token::new(TokenType::EOF, (0, 0), 0));
    }

    fn peek(&mut self) -> Option<&TokenType> {
        self.tokens.peek().map(|t| &t.token_type)
    }

    fn get_input_line(&self, line: usize) -> String {
        let error_line = AnsiString::from(self.input.lines().nth(line - 1).unwrap_or(""));
        let style = Style::new()
            .fg(Color::Green)
            .bold()
            .paint(error_line.to_string());
        style.to_string()
    }

    pub fn parse(&mut self) {
        let mut builder = ast::AstBuilder::new(self.namespace.to_owned());
        self.program = self.parse_program(&mut builder.context);
    }

    pub fn analyze(&self, ctx: &mut EvaluationContext) {
        for node in &self.program.body {
            self.analyze_node(&node, ctx);
        }
    }

    fn assert_token(&mut self, expected: TokenType) {
        if std::mem::discriminant(&self.current_token.token_type)
            != std::mem::discriminant(&expected)
        {
            let style = Style::new().bold().fg(Color::Red);
            let err = format!(
                "Expected token {:?}, got {:?}",
                expected, self.current_token.token_type
            );
            panic!("{}", style.paint(err).to_string());
        }
        self.advance();
    }

    fn parse_program(&mut self, ctx: &mut ast::EvaluationContext) -> ast::Program {
        let mut program = ast::Program {
            namespace: self.namespace.to_owned(),
            body: Vec::new(),
        };
        while let Some(token) = self.peek() {
            match token {
                TokenType::EOF => break,
                _ => {
                    program.body.push(self.parse_statement(ctx));
                }
            }
        }
        program
    }

    fn parse_statement(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        debug!("parsing statement: {:?}", self.current_token);
        match self.current_token.token_type {
            TokenType::Keyword(kw) => match kw {
                token::Keyword::Let => self.parse_let(ctx),
                token::Keyword::Import => self.parse_import(ctx),
                token::Keyword::Fn => self.parse_function_def(ctx),
                token::Keyword::Struct => self.parse_struct_def(ctx),
                token::Keyword::Const => self.parse_const(ctx),
                token::Keyword::If => self.parse_if(ctx),
                token::Keyword::For => self.parse_for(ctx),
                _ => ast::AstNode {
                    span: ParsedSpan {
                        line_start: self.current_token.span.line,
                        line_end: self.current_token.span.line,
                    },
                    node_type: ast::NodeType::Expression(
                        self.parse_expression(ctx, Precedence::Lowest),
                    ),
                },
            },
            TokenType::Identifier(_) => {
                if self.peek() == Some(&TokenType::LParen) {
                    let ident = self.parse_identifier(ctx);
                    let exp = self.parse_function_call(ctx, ident);
                    ast::AstNode {
                        span: ParsedSpan {
                            line_start: self.current_token.span.line,
                            line_end: self.current_token.span.line,
                        },
                        node_type: ast::NodeType::Expression(exp),
                    }
                } else {
                    self.parse_assignment(ctx)
                }
            }
            _ => unreachable!(
                "Error on line {}:\n  {}\n Expected statement, got {:?}",
                self.current_token.span.line,
                self.get_input_line(self.current_token.span.line),
                self.current_token.token_type
            ),
        }
    }

    fn parse_if(&mut self, ctx: &mut EvaluationContext) -> ast::AstNode {
        debug!("parsing if: {:?}", self.current_token);
        let start_line = self.current_token.span.line;
        self.advance();
        let condition = self.parse_expression(ctx, Precedence::Lowest);
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
            node_type: ast::NodeType::Statement(ast::Statement::If(Box::new(ast::IfStatement {
                span: ParsedSpan {
                    line_start: start_line,
                    line_end: self.current_token.span.line,
                },
                condition,
                consequence: body,
                alternative: else_block,
            }))),
        }
    }

    fn parse_for(&mut self, ctx: &mut EvaluationContext) -> ast::AstNode {
        debug!("parsing for: {:?}", self.current_token);
        self.advance();
        let identifier = self.parse_identifier(ctx);
        self.assert_token(TokenType::Keyword(token::Keyword::In));
        let iterable = self.parse_expression(ctx, Precedence::Lowest);
        let mut value = None;
        match iterable {
            ast::Expression::Identifier(ref ident) => {
                if let Some(var) = ctx.variables.get(ident) {
                    value = var.as_ref().map(|v| v.value.clone())
                }
            }
            ast::Expression::Literal(ref lit) => {
                match lit {
                    DataType::String(_) => {
                        value = Some(DataType::Char('_'));
                        ctx.variables.insert(
                            identifier.clone(),
                            Some(ast::ScopedVar {
                                scope: self.current_scope + 1,
                                value: DataType::Char('_'),
                            }),
                        )
                    }
                    DataType::Array(arr) => {
                        value = Some(*arr[0].clone());
                        ctx.variables.insert(
                            identifier.clone(),
                            Some(ast::ScopedVar {
                                scope: self.current_scope + 1,
                                value: *arr[0].clone(),
                            }),
                        )
                    }
                    DataType::Reference(ref var) => match **var {
                        DataType::Array(ref arr) => {
                            value = Some(*var.clone());
                            ctx.variables.insert(
                                identifier.clone(),
                                Some(ast::ScopedVar {
                                    scope: self.current_scope + 1,
                                    value: *arr[0].clone(),
                                }),
                            )
                        }
                        _ => {
                            value = Some(*var.clone());
                            ctx.variables.insert(
                                identifier.clone(),
                                Some(ast::ScopedVar {
                                    scope: self.current_scope + 1,
                                    value: *var.clone(),
                                }),
                            )
                        }
                    },
                    _ => {
                        panic!("Expected iterable, got {:?}", lit);
                    }
                };
            }
            _ => {}
        };
        let body = self.parse_block(ctx);
        ast::AstNode {
            span: ParsedSpan {
                line_start: self.current_token.span.line,
                line_end: self.current_token.span.line,
            },
            node_type: ast::NodeType::Statement(ast::Statement::ForLoop(Box::new(ast::ForLoop {
                variable: Variable {
                    name: identifier,
                    value: ast::Expression::Literal(value.unwrap()),
                },
                iterable,
                body,
            }))),
        }
    }

    fn parse_assignment(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        debug!("parsing assignment: {:?}", self.current_token);
        let name = self.parse_identifier(ctx);
        if !ctx.variables.contains_key(&name) {
            panic!("Variable {} not found", name.0);
        }
        self.assert_token(TokenType::Equals);
        let value = self.parse_expression(ctx, Precedence::Lowest);
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
        self.assert_token(TokenType::Keyword(token::Keyword::Let));
        let name = self.parse_identifier(ctx);
        let data_type = if self.current_token.token_type == token::TokenType::Colon {
            self.advance();
            self.parse_data_type()
        } else {
            DataType::Inferred
        };
        debug!("parsing let: {:?}", self.current_token);
        let value = if let TokenType::Equals = self.current_token.token_type {
            self.advance();
            Some(self.parse_expression(ctx, Precedence::Lowest))
        } else {
            None
        };
        match value {
            Some(ref val) => {
                match val {
                    Expression::Literal(ref lit) => {
                        ctx.variables.insert(
                            name.clone(),
                            Some(ast::ScopedVar {
                                scope: self.current_scope,
                                value: lit.clone(),
                            }),
                        );
                    }
                    Expression::Variable(ref var) => {
                        ctx.variables.insert(name.clone(), (Some(ast::ScopedVarvar.value.clone())))
                    }
                }
            }
        }
        ctx.variables.insert(
            name.clone(),
            Some(ast::ScopedVar {
                scope: self.current_scope,
                value: 
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
        debug!("parsing const: {:?}", self.current_token);
        self.advance();
        let name = self.parse_identifier(ctx);
        let data_type = self.parse_data_type();
        self.assert_token(TokenType::Equals);
        let value = self.parse_expression(ctx, Precedence::Lowest);
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
        debug!("parsing struct: {:?}", self.current_token);
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
        debug!("parsing struct fields: {:?}", self.current_token);
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
        debug!(
            "parsing import stmt: current token: {:?} current_scope: {}",
            self.current_token.token_type, self.current_scope
        );
        self.advance();
        let path = self.parse_identifier(ctx);
        //FIXME: for now treat path like functions imported from other modules
        ctx.functions.insert(path.clone(), self.current_scope);
        self.assert_token(TokenType::Semicolon);
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

    fn parse_function_def(&mut self, ctx: &mut ast::EvaluationContext) -> ast::AstNode {
        self.increment_scope();
        let start_line = self.current_token.span.line;
        self.advance();
        let name = self.parse_identifier(ctx);
        debug!("parsing function def: {}", name);
        ctx.functions.insert(name.clone(), self.current_scope);
        let params = self.parse_parameters(ctx);
        let return_type = self.parse_return_type();
        let body = self.parse_block(ctx);
        ast::AstNode {
            span: ParsedSpan {
                line_start: start_line,
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
        debug!("parsing return type: {:?}", self.current_token);
        self.assert_token(TokenType::Arrow);
        self.parse_data_type()
    }

    fn parse_block(&mut self, ctx: &mut EvaluationContext) -> ast::Block {
        debug!(
            "parsing block: current token: {:?} current_scope: {}",
            self.current_token.token_type, self.current_scope
        );
        let start_line = self.current_token.span.line;
        self.assert_token(TokenType::LBrace);
        self.increment_scope();
        self.is_global = false;
        let mut body = Vec::new();
        while TokenType::RBrace != self.current_token.token_type {
            body.push(self.parse_statement(ctx));
        }
        self.assert_token(TokenType::RBrace);
        self.decrement_scope();
        ast::Block {
            span: ParsedSpan::from_tokens(&start_line, &self.current_token.span.line),
            body,
        }
    }

    fn parse_data_type(&mut self) -> DataType {
        debug!(
            "parsing data type: current token: {:?} current_scope: {}",
            self.current_token.token_type, self.current_scope
        );
        if let TokenType::Keyword(kw) = self.current_token.token_type {
            let res = match kw {
                token::Keyword::Int => DataType::Integer(0),
                token::Keyword::Bool => DataType::Boolean(false),
                token::Keyword::String => DataType::String(None),
                token::Keyword::Float => DataType::Float(None),
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
            };
            self.advance();
            res
        } else {
            unreachable!(
                "Expected data type, found {:?}",
                self.current_token.token_type
            );
        }
    }

    fn parse_identifier(&mut self, ctx: &mut ast::EvaluationContext) -> ast::Identifier {
        if let TokenType::Identifier(ident) = self.current_token.token_type.clone() {
            let identifier = ast::Identifier(ident);
            if !ctx.variables.contains_key(&identifier) {
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
        debug!(
            "parsing parameters: current token: {:?} current_scope: {}",
            self.current_token, self.current_scope
        );
        while TokenType::RParen != self.current_token.token_type {
            match self.current_token.token_type {
                TokenType::Comma => {
                    self.advance();
                    continue;
                }
                TokenType::Identifier(_) => {
                    let name = self.parse_identifier(ctx);
                    self.assert_token(TokenType::Colon);
                    let data_type = self.parse_data_type();
                    params.push(ast::FnParam {
                        name: name.clone(),
                        data_type: data_type.clone(),
                    });
                    ctx.variables.insert(
                        name,
                        Some(ast::ScopedVar {
                            scope: self.current_scope,
                            value: data_type,
                        }),
                    );
                }
                _ => panic!(
                    "Expected identifier, got {:?}",
                    self.current_token.token_type
                ),
            }
        }
        self.assert_token(TokenType::RParen);
        params
            .iter()
            .for_each(|param| debug!("function param: {:?}", param));
        params
    }

    fn parse_primary_expression(&mut self, ctx: &mut ast::EvaluationContext) -> ast::Expression {
        debug!(
            "parsing primary expression: current token: {:?} current_scope: {}",
            self.current_token.token_type, self.current_scope
        );
        match self.current_token.token_type.clone() {
            TokenType::Literal(lit) => {
                self.advance();
                ast::Expression::Literal(lit)
            }
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
                let expr = self.parse_expression(ctx, Precedence::Lowest);
                self.assert_token(TokenType::RParen);
                expr
            }
            _ => panic!("Unexpected token: {:?}", self.current_token.token_type),
        }
    }

    fn parse_expression(
        &mut self,
        ctx: &mut ast::EvaluationContext,
        precedence: Precedence,
    ) -> ast::Expression {
        debug!(
            "parsing expression: current token: {:?} current_scope: {}",
            self.current_token.token_type, self.current_scope
        );
        let mut left = self.parse_primary_expression(ctx);
        while precedence < self.current_precedence() {
            let operator: Result<token::Operator, _> =
                self.current_token.token_type.clone().try_into();
            let op = match operator {
                Ok(op) => op,
                _ => return left,
            };
            self.advance();
            let right = self.parse_expression(ctx, self.operator_precedence(&op));
            left = ast::Expression::BinaryOp(Box::new(ast::BinaryOp {
                left,
                operator: op,
                right,
            }));
        }

        left
    }

    fn current_precedence(&self) -> Precedence {
        let op: Result<token::Operator, _> = self.current_token.token_type.clone().try_into();
        match op {
            Ok(ref op) => Precedence::from(op),
            _ => Precedence::Lowest,
        }
    }

    fn operator_precedence(&self, op: &token::Operator) -> Precedence {
        Precedence::from(op.to_owned())
    }

    #[rustfmt::skip]
    fn parse_function_call(&mut self, ctx: &mut ast::EvaluationContext, ident: ast::Identifier) -> ast::Expression {
        let args = self.parse_arguments(ctx);
        if ctx.functions.contains_key(&ident) {
             ast::Expression::FnCall(Box::new(ast::FnCall {
            name: ident,
            arguments: args,
            }))
        } else {
            debug!("{:?}", ctx.functions.keys().collect::<Vec<&ast::Identifier>>());
            panic!("Undeclared function: {}", ident);
        }
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
                    let arg = self.parse_expression(ctx, ast::Precedence::Lowest);
                    args.push(arg);
                }
            }
        }
        self.assert_token(TokenType::RParen);
        args
    }
}
