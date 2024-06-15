use crate::token::{DataType, Token, TokenType};

pub struct Lexer {
    filename: String,
    input: String,
    position: usize,
    line: usize,
    current: char,
    pub tokens: Vec<Token>,
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || ['_', '-'].contains(&c)
}

impl Lexer {
    pub fn new(path: &str) -> Self {
        let filename = std::path::Path::new(path)
            .file_name()
            .expect("invalid filepath");
        let input = std::fs::read_to_string(path).expect("file not found");
        let first = input.chars().nth(0).expect("empty file");
        Self {
            filename: filename.to_string_lossy().to_string(),
            input,
            position: 0,
            current: first,
            tokens: Vec::new(),
            line: 1,
        }
    }
    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.position + 1)
    }

    pub fn get_filename(&self) -> String {
        self.filename.clone()
    }

    fn advance(&mut self) {
        if let Some(c) = self.peek_char() {
            self.current = c;
            self.position += 1;
            if c == '\n' {
                self.line += 1;
            }
        } else {
            self.current = '\0';
        }
    }

    fn parse_token(&mut self) -> Option<Token> {
        match self.current {
            '\0' => Some(self.single_token(TokenType::EOF)),
            ch if ch.is_whitespace() => Some(self.single_token(TokenType::Whitespace)),
            '\"' => self.extract_string(),
            '-' => match self.peek_char() {
                Some(c) if c.is_ascii_digit() => self.extract_digit(),
                Some(_) => Some(self.single_token(TokenType::Minus)),
                None => Some(self.single_token(TokenType::EOF)),
            },
            '.' => match self.peek_char() {
                Some(c) if c.is_ascii_digit() => self.extract_float(),
                Some(_) => Some(self.single_token(TokenType::Period)),
                None => Some(self.single_token(TokenType::EOF)),
            },
            ch if TokenType::from_char(ch).is_some() => self.extract_symbol(),
            ch if ch.is_ascii_digit() => self.extract_digit(),
            _ => self.extract_ident(),
        }
    }

    fn extract_symbol(&mut self) -> Option<Token> {
        let start = self.position;
        let mut symbol = String::from(self.current);
        if let Some(peek) = self.peek_char() {
            symbol.push(peek);
            match symbol.as_str() {
                "==" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::EqEq,
                        (start, self.position),
                        self.line,
                    ))
                }
                "!=" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::NotEq,
                        (start, self.position),
                        self.line,
                    ))
                }
                "<=" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::LtEquals,
                        (start, self.position),
                        self.line,
                    ))
                }
                ">=" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::GtEquals,
                        (start, self.position),
                        self.line,
                    ))
                }
                "::" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::ColonColon,
                        (start, self.position),
                        self.line,
                    ))
                }
                "->" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::Arrow,
                        (start, self.position),
                        self.line,
                    ))
                }
                "&&" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::LogicalAnd,
                        (start, self.position),
                        self.line,
                    ))
                }
                "||" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::LogicalOr,
                        (start, self.position),
                        self.line,
                    ))
                }
                "??" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::Ternary,
                        (start, self.position),
                        self.line,
                    ))
                }
                "*=" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::MulEquals,
                        (start, self.position),
                        self.line,
                    ))
                }
                "+=" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::PlusEquals,
                        (start, self.position),
                        self.line,
                    ))
                }
                "-=" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::MinusEquals,
                        (start, self.position),
                        self.line,
                    ))
                }
                "/=" => {
                    self.advance();
                    Some(Token::new(
                        TokenType::DivEquals,
                        (start, self.position),
                        self.line,
                    ))
                }
                _ => Some(self.single_token(TokenType::from_char(self.current)?)),
            }
        } else {
            Some(self.single_token(TokenType::from_char(self.current)?))
        }
    }

    fn extract_string(&mut self) -> Option<Token> {
        let start = self.position;
        let mut literal = String::new();
        self.advance();
        while self.current != '\"' {
            literal.push(self.current);
            self.advance();
        }
        self.advance();
        Some(Token::new(
            TokenType::Literal(DataType::String(literal)),
            (start, self.position),
            self.line,
        ))
    }

    fn single_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, (self.position, self.position + 1), self.line)
    }

    fn extract_ident(&mut self) -> Option<Token> {
        let start = self.position;
        let mut ident = String::from(self.current);
        while is_ident_char(self.peek_char()?) {
            self.advance();
            ident.push(self.current);
        }
        Some(Token::new(
            TokenType::from(ident.as_str()),
            (start, self.position),
            self.line,
        ))
    }

    fn extract_digit(&mut self) -> Option<Token> {
        let start = self.position;
        let mut number = String::new();
        if self.current == '-' {
            number.push(self.current);
            self.advance();
        }
        while self.current.is_ascii_digit() {
            number.push(self.current);
            self.advance();
        }
        Some(Token::new(
            TokenType::Literal(DataType::Integer(number.parse::<i64>().unwrap())),
            (start, self.position),
            self.line,
        ))
    }

    fn extract_float(&mut self) -> Option<Token> {
        let start = self.position;
        let mut number = String::from('.');
        self.advance();
        while self.current.is_ascii_digit() {
            number.push(self.current);
            self.advance();
        }
        Some(Token::new(
            TokenType::Literal(DataType::Float(number.parse::<f64>().unwrap())),
            (start, self.position),
            self.line,
        ))
    }

    pub fn lex(&mut self) {
        while let Some(token) = self.parse_token() {
            if token.token_type == TokenType::EOF {
                break;
            } else if token.token_type != TokenType::Whitespace {
                self.tokens.push(token);
            }
            self.advance();
        }
    }
}
