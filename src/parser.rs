use crate::code::{Expr, Stmt};
use crate::error::CustomError;
use crate::error::InterpreterError;
use crate::error::ParserError;
use crate::error::Result;
use crate::error::ScannerError;
use crate::scanner::Literal;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::variable::Var;

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_eof() {
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt> {
        println!("{:?}", self.tokens);
        if self.advance_match(&[TokenType::VAR]) {
            return self.declare_var();
        }

        self.statement()
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.advance_match(&[TokenType::PRINT]) {
            return self.print_statement();
        }
        if self.advance_match(&[TokenType::LEFT_BRACE]) {
            return Ok(Stmt::new_block(self.block()?));
        }

        self.expr_statement()
    }

    fn block(&mut self) -> Result<Vec<Box<Stmt>>> {
        let mut statements = vec![];

        while !self.check(&TokenType::RIGHT_BRACE) && !self.is_eof() {
            statements.push(Box::new(self.declaration()?));
        }

        self.consume(
            TokenType::RIGHT_BRACE,
            "Missing } after block initialization",
        );
        Ok(statements)
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let value = self.comma()?;
        self.consume(TokenType::SEMICOLON, "Missing semicolon")?;
        Ok(Stmt::Print(Box::new(value)))
    }

    fn expr_statement(&mut self) -> Result<Stmt> {
        let value = self.comma()?;
        self.consume(TokenType::SEMICOLON, "Missing semicolon")?;
        Ok(Stmt::Expression(Box::new(value)))
    }

    fn comma(&mut self) -> Result<Expr> {
        self.create_binary_expr(Parser::expression, &[TokenType::COMMA])
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        self.create_assignment()
    }

    fn ternary(&mut self) -> Result<Expr> {
        self.create_ternary_expr()
    }

    fn equality(&mut self) -> Result<Expr> {
        self.create_binary_expr(
            Parser::comparison,
            &[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL],
        )
    }

    fn comparison(&mut self) -> Result<Expr> {
        self.create_binary_expr(
            Parser::addition,
            &[
                TokenType::GREATER,
                TokenType::GREATER_EQUAL,
                TokenType::LESS,
                TokenType::LESS_EQUAL,
            ],
        )
    }

    fn addition(&mut self) -> Result<Expr> {
        self.create_binary_expr(Parser::multiplication, &[TokenType::MINUS, TokenType::PLUS])
    }

    fn multiplication(&mut self) -> Result<Expr> {
        self.create_binary_expr(Parser::unary, &[TokenType::SLASH, TokenType::STAR])
    }

    fn advance_match(&mut self, tokens: &[TokenType]) -> bool {
        for token in tokens {
            if self.check(token) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_eof() {
            return false;
        }

        self.peek().token_type == *token_type
    }

    fn advance(&mut self) -> &Token {
        if !self.is_eof() {
            self.current += 1;
        }

        self.previous()
    }

    fn match_literals(&mut self) -> Option<Expr> {
        match &self.peek().token_type {
            TokenType::STRING(s) => {
                self.advance();
                Some(Expr::Literal { literal: s.clone() })
            }
            TokenType::NUMBER(n) => {
                self.advance();
                Some(Expr::Literal { literal: n.clone() })
            }
            _ => None,
        }
    }

    fn is_eof(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn peek(&self) -> &'a Token {
        self.tokens.get(self.current).unwrap()
    }

    fn previous(&self) -> &'a Token {
        self.tokens.get(self.current - 1).unwrap()
    }
}

impl<'a> Parser<'a> {
    fn create_assignment(&mut self) -> Result<Expr> {
        let expr = self.ternary()?;

        if self.advance_match(&[TokenType::EQUAL]) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expr::Variable(t) => {
                    return Ok(Expr::Assign {
                        name: t.clone(),
                        value: Box::new(value),
                    });
                }
                _ => {
                    return Err(InterpreterError::new(
                        1,
                        "Invalid assignment target",
                        CustomError::ParserError(ParserError::InvalidStatement),
                    ));
                }
            }
        }

        return Ok(expr);
    }

    fn create_binary_expr<F: Fn(&mut Parser<'a>) -> Result<Expr>>(
        &mut self,
        operation: F,
        token_types: &[TokenType],
    ) -> Result<Expr> {
        let mut expr = operation(self)?;

        while self.advance_match(token_types) {
            let operator = self.previous().clone();
            let right = operation(self)?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn create_ternary_expr(&mut self) -> Result<Expr> {
        let mut condition = self.equality()?;

        if self.advance_match(&[TokenType::QUESTION]) {
            let mut left = Box::new(self.expression()?);
            if self.advance_match(&[TokenType::COLON]) {
                let mut right = Box::new(self.expression()?);
                return Ok(Expr::Ternary {
                    condition: Box::new(condition),
                    left,
                    right,
                });
            } else {
                return Err(InterpreterError::new(
                    1,
                    "Expected right side of ternary conditional",
                    CustomError::ParserError(ParserError::UnterminatedToken),
                ));
            }
        } else {
            return Ok(condition);
        }
    }
}

impl<'a> Parser<'a> {
    fn unary(&mut self) -> Result<Expr> {
        if self.advance_match(&[TokenType::BANG, TokenType::MINUS]) {
            let operator = self.previous().clone();
            let right = self.unary();
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right?),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr> {
        if self.advance_match(&[TokenType::FALSE]) {
            return Ok(Expr::Literal {
                literal: Literal::Boolean(false),
            });
        }
        if self.advance_match(&[TokenType::TRUE]) {
            return Ok(Expr::Literal {
                literal: Literal::Boolean(true),
            });
        }
        if self.advance_match(&[TokenType::NIL]) {
            return Ok(Expr::Literal {
                literal: Literal::Null,
            });
        }
        match &self.peek().token_type {
            TokenType::IDENTIFIER(_) => {
                let result = Ok(Expr::Variable(self.peek().clone()));
                self.advance();
                return result;
            }
            _ => {
                ();
            }
        };

        match self.match_literals() {
            Some(expr) => return Ok(expr),
            None => (),
        };

        if self.advance_match(&[TokenType::LEFT_PAREN]) {
            let expr = self.expression()?;
            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after expression")?;
            return Ok(Expr::Grouping {
                expression: Box::new(expr),
            });
        }

        Err(InterpreterError::new(
            1,
            "Expect expression",
            CustomError::ParserError(ParserError::InvalidStatement),
        ))
    }

    fn consume(&mut self, token_type: TokenType, expect: &'a str) -> Result<()> {
        if self.check(&token_type) {
            self.advance();
            return Ok(());
        }

        Err(InterpreterError::new(
            1,
            expect,
            CustomError::ParserError(ParserError::UnterminatedToken),
        ))
    }
}

impl<'a> Parser<'a> {
    fn declare_var(&mut self) -> Result<Stmt> {
        let name = self.advance().clone();
        match name.token_type {
            TokenType::IDENTIFIER(_) => (),
            _ => {
                return Err(InterpreterError::new(
                    1,
                    "Invalid variable name",
                    CustomError::ScannerError(ScannerError::SyntaxError),
                ));
            }
        };
        let initializer = if self.advance_match(&[TokenType::EQUAL]) {
            self.comma()?
        } else {
            Expr::Literal {
                literal: Literal::Null,
            }
        };
        self.consume(TokenType::SEMICOLON, "Missing semicolon")?;
        Ok(Stmt::Var(Var::new(name, Box::new(initializer))))
    }
}

impl<'a> Parser<'a> {
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_eof() {
            match &self.previous().token_type {
                TokenType::SEMICOLON => return (),
                _ => (),
            }

            match &self.peek().token_type {
                TokenType::RETURN => return (),
                _ => (),
            }

            self.advance();
        }
    }
}
