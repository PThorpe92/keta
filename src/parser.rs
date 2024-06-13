use crate::{
    ast, lexer,
    token::{self, Keyword, Token, TokenType},
};
use std::convert::TryFrom;
use std::str::FromStr;

pub struct Parser {
    lexer: lexer::Lexer,
    current_token: Token,
    current_position: usize,
    pub ast: ast::Program,
    identifiers: ast::EvaluationContext,
}

#[derive(Debug)]
pub struct ParsedSpan {
    pub line_start: usize,
    pub line_end: usize,
}

impl ParsedSpan {
    pub fn from_tokens(start: &Token, end: &Token) -> Self {
        Self {
            line_start: start.span.line,
            line_end: end.span.line,
        }
    }
}

impl Parser {
    pub fn new(path: &str) -> Self {
        let mut lexer = lexer::Lexer::new(path);
        lexer.lex();
        let current_token = lexer.tokens[0];
        Self {
            lexer,
            current_token,
            current_position: 0,
            ast: ast::Program {
                namespace: lexer.get_filename(),
                body: Vec::new(),
            },
            identifiers: ast::EvaluationContext::new(),
        }
    }

    fn next_token(&mut self) {
        self.current_position += 1;
        self.current_token = self.lexer.tokens[self.current_position];
    }

    fn peek_token(&self) -> Token {
        self.lexer.tokens[self.current_position + 1]
    }

    fn assert_identifier(&self) {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            return;
        }
        let error_msg = format!(
            "expected identifier on line: {} \n got: {:?}",
            self.current_token.span.line, self.current_token.token_type
        );
        panic!("{}", error_msg);
    }

    fn assert_literal(&self) {
        if let TokenType::Literal(_) = self.current_token.token_type {
            return;
        }
        let error_msg = format!(
            "expected literal on line: {} \n got: {:?}",
            self.current_token.span.line, self.current_token.token_type
        );
        panic!("{}", error_msg);
    }

    fn assert_keyword(&self, keyword: Keyword) {
        if let TokenType::Keyword(k) = self.current_token.token_type {
            if k == keyword {
                return;
            }
        }
        let error_msg = format!(
            "expected keyword: {:?} on line: {} \n got: {:?}",
            keyword, self.current_token.span.line, self.current_token.token_type
        );
        panic!("{}", error_msg);
    }

    fn assert_symbol(&self, symbol: TokenType) {
        if symbol == self.current_token.token_type {
            return;
        } else {
            let error_msg = format!(
                "expected symbol: {:?} on line: {} \n got: {:?}",
                symbol, self.current_token.span.line, self.current_token.token_type
            );
            panic!("{}", error_msg);
        }
    }

    fn parse_program(&mut self) {
        while self.current_token.token_type != TokenType::EOF {
            self.ast.body.push(self.parse_statement());
        }
    }

    fn parse_statement(&mut self) -> ast::AstNode {
        let node_type = match self.current_token.token_type {
            TokenType::Keyword(keyword) => match keyword {
                Keyword::Import => self.parse_import(),
                Keyword::Const => self.parse_let(),
                Keyword::Fn => self.parse_fn_def(),
                Keyword::Struct => self.parse_struct_def(),
                Keyword::Variant => self.parse_variant_def(),
                Keyword::Union => self.parse_union_def(),
                _ => panic!(
                    "unexpected keyword: {:?} on line: {}",
                    keyword, self.current_token.span.line
                ),
            },
            TokenType::Identifier(ident) => self.parse_let(),
            TokenType::Fn => self.parse_fn(),
            _ => ast::NodeType::Expression(self.parse_expression()),
        };
    }

    fn parse_literal(&mut self) -> ast::Expression {
        let start = self.current_token;
        self.assert_literal();
        let value = self.current_token.token_type;
        ast::Expression::Literal(token::Literal::from_token_type(value).expect("expected literal"))
    }

    fn parse_expression(&mut self) -> ast::Expression {
        match self.current_token.token_type {
            TokenType::Literal(_) => self.parse_literal(),
            TokenType::Identifier(_) => self.parse_identifier(),
            TokenType::Keyword(keyword) => match keyword {
                Keyword::If => self.parse_if(),
                Keyword::While => self.parse_while(),
                Keyword::Return => self.parse_return(),
                Keyword::Let => self.parse_let(),
                _ => panic!(
                    "unexpected keyword: {:?} on line: {}",
                    keyword, self.current_token.span.line
                ),
            },
            _ => panic!(
                "unexpected token: {:?} on line: {}",
                self.current_token.token_type, self.current_token.span.line
            ),
        }
    }

    fn parse_identifier(&mut self) -> ast::Expression {
        self.assert_identifier();
        let ident: &str = self.current_token.token_type.into();
        self.next_token();
        ast::Expression::Identifier(ident.to_string())
    }

    fn parse_if(&mut self) -> ast::Expression {
        let start = self.current_token;
        self.assert_keyword(Keyword::If);
        self.next_token();
        self.assert_symbol(TokenType::LParen);
        self.next_token();
        let condition = self.parse_expression();
        self.assert_symbol(TokenType::RParen);
        self.next_token();
        self.assert_symbol(TokenType::LBrace);
        self.next_token();
        let body = self.parse_block();
        ast::Expression(ast::IfStatement {
            span: ParsedSpan::from_tokens(&start, &self.current_token),
            condition,
            body,
        })
    }

    fn parse_struct_def(&mut self) -> ast::AstNode {
        let span = self.current_token;
        self.assert_keyword(Keyword::Struct);
        self.next_token();
        self.assert_identifier();
        let name: &str = self.current_token.token_type.into();
        self.next_token();
        self.assert_symbol(TokenType::LBrace);
        self.next_token();
        let mut fields = Vec::new();
        while self.current_token.token_type != TokenType::RBrace {
            self.assert_identifier();
            let field_name: &str = self.current_token.token_type.into();
            self.next_token();
            self.assert_symbol(TokenType::Colon);
            self.next_token();
            let data_type = ast::DataType::from_token_type(&self.current_token.token_type);
            fields.push(ast::StructField {
                name: field_name.to_string(),
                data_type,
            });
            self.next_token();
            if self.current_token.token_type == TokenType::Comma {
                self.next_token();
            }
        }
        ast::AstNode {
            span: ParsedSpan::from_tokens(&span, &self.current_token),
            node_type: ast::NodeType::Definition(ast::Definition::Struct(ast::StructDef {
                name: name.to_string(),
                fields,
            })),
        }
    }

    fn parse_import(&mut self) -> ast::AstNode {
        let span = self.current_token;
        self.next_token();
        self.assert_identifier();
        let node = ast::Expression::Identifier(
            String::from_str(self.current_token.token_type.into()).expect("expected string"),
        );
        self.next_token();
        self.assert_symbol(TokenType::Semicolon);
        ast::AstNode {
            span: ParsedSpan::from_tokens(&span, &self.current_token),
            node_type: ast::NodeType::Expression(node),
        }
    }

    fn parse_let_stmt(&mut self) -> ast::AstNode {
        let start = self.current_token;
        self.assert_keyword(Keyword::Let);
        self.next_token();
        let ident: &str = self.current_token.token_type.into();
        let node = ast::Expression::Identifier(String::from(ident));
        self.next_token();
        self.assert_symbol(TokenType::Equals);
        self.next_token();
        let expression = self.parse_expression();
        ast::AstNode {
            span: ParsedSpan::from_tokens(&start, &self.current_token),
            node_type: ast::NodeType::Definition(ast::Definition::Variable(ast::VariableDef {
                name: ast::Identifier(ident.to_string()),
                value: expression,
                is_const: false,
            })),
        }
    }

    fn parse_assignment(&mut self) -> ast::AstNode {
        let start = self.current_token;
        self.assert_identifier();
        let ident: &str = self.current_token.token_type.into();
        self.next_token();
        let operator = token::Operator::try_from(self.current_token.token_type).unwrap();
        self.next_token();
        let expression = self.parse_expression();
        ast::AstNode {
            span: ParsedSpan::from_tokens(&start, &self.current_token),
            node_type: ast::NodeType::Statement(ast::Statement::Assignment(ast::Assignment {
                identifier: ident.to_string(),
                operator,
                expression,
            })),
        }
    }

    fn parse_const_stmt(&mut self) -> ast::AstNode {
        let start = self.current_token;
        self.assert_keyword(Keyword::Const);
        self.next_token();
        let ident: &str = self.current_token.token_type.into();
        let node = ast::Expression::Identifier(String::from(ident));
        self.next_token();
        self.assert_symbol(TokenType::Equals);
        self.next_token();
        let expression = self.parse_expression();
        ast::AstNode {
            span: ParsedSpan::from_tokens(&start, &self.current_token),
            node_type: ast::NodeType::Definition(ast::Definition::Variable(ast::VariableDef {
                name: ast::Identifier(ident.to_string()),
                value: expression,
                is_const: true,
            })),
        }
    }

    fn parse_fn_args(&mut self) -> ast::AstNode {
        let start = self.current_token;
        let mut args = Vec::new();
        while self.current_token.token_type != TokenType::RParen {
            self.next_token();
            // could be literal or ident
            let mut node: ast::FnArg;
            if let TokenType::Literal(lit) = self.current_token.token_type {
                node = ast::FnArg::Literal(lit);
            } else if let TokenType::Identifier(ident) = self.current_token.token_type {
                node = ast::FnArg::Variable(ast::Identifier(ident));
            }
            args.push(node);
            self.next_token();
        }
        ast::AstNode {
            span: ParsedSpan::from_tokens(&start, &self.current_token),
            node_type: ast::NodeType::Expression(ast::Expression::FnArgs(args)),
        }
    }

    fn parse_fn_params(&mut self) -> Vec<ast::FnParam> {
        let mut params = Vec::new();
        while self.current_token.token_type != TokenType::RParen {
            self.next_token();
            self.assert_identifier();
            let ident: &str = self.current_token.token_type.into();
            self.next_token();
            self.assert_symbol(TokenType::Colon);
            let data_type = ast::DataType::from_token_type(&self.current_token.token_type);
            self.next_token();
            params.push(ast::FnParam {
                name: ast::Identifier(ident.to_string()),
                data_type,
            });
            if self.current_token.token_type == TokenType::Comma {
                self.next_token();
            }
        }
        params
    }

    fn parse_fn_return_type(&mut self) -> ast::DataType {
        self.assert_symbol(TokenType::Lt);
        self.next_token();
        if !self.current_token.token_type.is_type() {
            panic!("expected type on line: {}", self.current_token.span.line);
        }
        let return_type = self.current_token.token_type;
        self.next_token();
        self.assert_symbol(TokenType::Gt);
        ast::DataType::from_token_type(&return_type)
    }

    fn parse_fn_def(&mut self) -> ast::AstNode {
        // fn<return_type> name(params) { body }
        let start = self.current_token;
        self.assert_keyword(Keyword::Fn);
        self.next_token();
        self.assert_identifier();
        let name: &str = self.current_token.token_type.into();
        self.next_token();
        let return_type = self.parse_fn_return_type();
        self.next_token();
        self.assert_symbol(TokenType::LParen);
        let params = self.parse_fn_params();
        self.assert_symbol(TokenType::RParen);
        self.next_token();
        self.assert_symbol(TokenType::LBrace);
        self.next_token();
        let body = self.parse_block();
        ast::AstNode {
            span: ParsedSpan::from_tokens(&start, &self.current_token),
            node_type: ast::NodeType::Definition(ast::Definition::Function(ast::FunctionDef {
                name: ast::Identifier(name.to_string()),
                return_type,
                parameters: params,
                body,
            })),
        }
    }

    fn parse_block(&mut self) -> ast::Block {
        let start = self.current_token;
        let mut statements = Vec::new();
        while self.current_token.token_type != TokenType::RBrace {
            self.next_token();
            statements.push(self.parse_statement());
        }
        ast::Block {
            span: ParsedSpan::from_tokens(&start, &self.current_token),
            body: statements,
        }
    }
}
