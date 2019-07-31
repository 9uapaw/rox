use crate::error::error;
use crate::error::CustomError;
use crate::error::InterpreterError;
use crate::error::ParserError;
use crate::error::Result;
use crate::error::ScannerError;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut hash = HashMap::new();
        hash.insert("and", TokenType::AND);
        hash.insert("class", TokenType::CLASS);
        hash.insert("else", TokenType::ELSE);
        hash.insert("false", TokenType::FALSE);
        hash.insert("true", TokenType::TRUE);
        hash.insert("or", TokenType::OR);
        hash.insert("return", TokenType::RETURN);
        hash.insert("for", TokenType::FOR);
        hash.insert("fun", TokenType::FUN);
        hash.insert("if", TokenType::IF);
        hash.insert("nil", TokenType::NIL);
        hash.insert("super", TokenType::SUPER);
        hash.insert("this", TokenType::THIS);
        hash.insert("var", TokenType::VAR);
        hash.insert("while", TokenType::WHILE);
        hash.insert("print", TokenType::PRINT);
        hash
    };
}

pub struct Scanner {
    source: Source,
    tokens: Vec<Token>,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        let tokens = Vec::<Token>::new();
        let source = Source::new(source);
        Scanner { source, tokens }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        //        self.source
        //            .source
        //            .chars()
        //            .for_each(|c| println!("CHARS: {}", c));
        loop {
            self.source.start = self.source.current;
            let result = self.scan_single_token();
            match &result {
                Err(error) => {
                    error.throw();
                    continue;
                }
                Ok(_) => (),
            }

            if self.source.is_eof() {
                break;
            }
        }

        self.tokens.push(Token {
            token_type: TokenType::EOF,
            lexem: String::from(""),
            literal: None,
            line: self.source.line,
        });
        return &self.tokens;
    }

    fn scan_single_token(&mut self) -> Result<()> {
        let c: char = self.source.advance();
        //        println!("CHAR: {} - CURRENT: {}", c, self.source.current);

        let token = match c {
            '(' => Some(TokenType::LEFT_PAREN),
            ')' => Some(TokenType::RIGHT_PAREN),
            '{' => Some(TokenType::LEFT_BRACE),
            '}' => Some(TokenType::RIGHT_BRACE),
            ',' => Some(TokenType::COMMA),
            '.' => Some(TokenType::DOT),
            '-' => Some(TokenType::MINUS),
            '+' => Some(TokenType::PLUS),
            ';' => Some(TokenType::SEMICOLON),
            '*' => Some(TokenType::STAR),
            '?' => Some(TokenType::QUESTION),
            ':' => Some(TokenType::COLON),
            '!' => {
                (if self.source.advance_match('=') {
                    Some(TokenType::BANG_EQUAL)
                } else {
                    Some(TokenType::BANG)
                })
            }
            '=' => {
                (if self.source.advance_match('=') {
                    Some(TokenType::EQUAL_EQUAL)
                } else {
                    Some(TokenType::EQUAL)
                })
            }
            '<' => {
                (if self.source.advance_match('=') {
                    Some(TokenType::LESS_EQUAL)
                } else {
                    Some(TokenType::LESS)
                })
            }
            '>' => {
                (if self.source.advance_match('=') {
                    Some(TokenType::GREATER_EQUAL)
                } else {
                    Some(TokenType::GREATER)
                })
            }
            '/' => {
                if self.source.advance_match('/') {
                    loop {
                        if self.source.is_closed('\n') {
                            break;
                        }
                        self.source.advance();
                    }
                    None
                } else {
                    Some(TokenType::SLASH)
                }
            }
            ' ' => None,
            '\r' => None,
            '\t' => None,
            '\n' => {
                self.source.line += 1;
                None
            }
            '"' => match self.create_string_literal() {
                Ok(token_type) => Some(token_type),
                Err(error) => return Err(error),
            },
            '0'...'9' => Some(self.create_number_literal()?),
            'a'...'z' | 'A'...'Z' | '_' => Some(self.create_identifier()),
            _ => {
                return Err(InterpreterError::new(
                    self.source.line,
                    &format!("Unexpected character: {}", self.source.query_previous()),
                    CustomError::ScannerError(ScannerError::UnexpectedChar),
                ));
            }
        };
        if let Some(token) = token {
            self.push_token(token);
        }

        Ok(())
    }

    fn push_token(&mut self, token_type: TokenType) {
        self.push_token_literal(token_type, None)
    }

    fn push_token_literal(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let mut lexem = self.source.extract_from_start();

        self.tokens.push(Token {
            token_type,
            lexem,
            literal,
            line: self.source.line,
        });
    }

    fn create_string_literal(&mut self) -> Result<TokenType> {
        let string_start = self.source.current;
        while !self.source.is_closed('"') {
            if self.source.is_newline() {
                self.source.line += 1;
            }
            self.source.advance();
        }

        if self.source.is_eof() {
            return Err(InterpreterError::new(
                self.source.line,
                "Unclosed string literal",
                CustomError::ScannerError(ScannerError::UnexpectedChar),
            ));
        }
        self.source.advance();

        let string_end = self.source.current - 1;
        let literal = self.source.extract_as_string(string_start, string_end);

        Ok(TokenType::STRING(Literal::String(literal)))
    }

    fn create_number_literal(&mut self) -> Result<TokenType> {
        while self.source.query_current().is_numeric() {
            self.source.advance();
        }

        if self.source.query_current() == '.' && self.source.query_next().is_numeric() {
            self.source.advance();
            while self.source.query_current().is_numeric() {
                self.source.advance();
            }
        }

        let number_string = self.source.extract_as_string(self.source.start, self.source.current);

        let number = match number_string.parse::<f64>() {
            Ok(number) => number,
            Err(error) => {
                return Err(InterpreterError::new(
                    self.source.line,
                    &format!("Unable to parse number: {}", number_string),
                    CustomError::ScannerError(ScannerError::InvalidNumber),
                ));
            }
        };

        Ok(TokenType::NUMBER(Literal::Number(number)))
    }

    fn create_identifier(&mut self) -> TokenType {
        let identifier_start = self.source.current;
        while self.source.query_current().is_alphanumeric() {
            self.source.advance();
        }
        let identifier_end = self.source.current;
        let text = self
            .source
            .extract_as_string(identifier_start - 1, identifier_end);
        let token = match KEYWORDS.get::<str>(&text) {
            Some(token) => token.clone(),
            None => TokenType::IDENTIFIER,
        };

        token
    }
}

pub struct Source {
    pub source: String,
    pub start: usize,
    pub current: usize,
    pub line: usize,
}

impl Source {
    pub fn new(source: String) -> Self {
        Source {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn query_current(&self) -> char {
        if self.is_eof() {
            return '\0';
        }
        self.query(self.current)
    }

    pub fn query_previous(&self) -> char {
        self.query(self.current - 1)
    }

    pub fn query_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.query(self.current + 1)
    }

    fn query(&self, n: usize) -> char {
        self.source.chars().nth(n).unwrap()
    }

    pub fn advance(&mut self) -> char {
        let current = self.query_current();
        self.current += 1;
        current
    }

    pub fn advance_match(&mut self, expected: char) -> bool {
        if self.is_eof() {
            return false;
        }

        if self.query_current() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    pub fn extract_from_start(&self) -> String {
        self.extract_as_string(self.start, self.current)
    }

    pub fn extract_as_string(&self, start: usize, end: usize) -> String {
        String::from(&self.source.as_str()[start..end])
    }

    pub fn is_eof(&self) -> bool {
        self.current as usize >= self.source.len()
    }

    pub fn is_closed(&self, enclosing: char) -> bool {
        self.query_current() == enclosing || self.is_eof()
    }

    pub fn is_newline(&self) -> bool {
        self.query_current() == '\n'
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexem: String,
    literal: Option<Literal>,
    pub line: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", &self.lexem)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Boolean(bool),
    Number(f64),
    Null,
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Literal::String(s) => write!(f, "{}", s),
            Literal::Null => write!(f, "NULL"),
            Literal::Number(n) => write!(f, "{}", n),
            Literal::Boolean(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    QUESTION,
    COLON,

    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    IDENTIFIER,
    STRING(Literal),
    NUMBER(Literal),

    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}
