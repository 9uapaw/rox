use crate::ast::{Expr, Stmt};
use crate::config::FN_ARG_LIMIT;
use crate::error::CustomError;
use crate::error::InterpreterError;
use crate::error::ParserError;
use crate::error::Result;
use crate::error::ScannerError;
use crate::obj::class::{Get, Set};
use crate::obj::function::Call;
use crate::scanner::Literal;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::variable::Var;
use std::cell::RefCell;
use std::rc::Rc;

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
        if self.advance_match(&[TokenType::CLASS]) {
            return self.class();
        }
        if self.advance_match(&[TokenType::FUN]) {
            return self.function("function");
        }
        if self.advance_match(&[TokenType::VAR]) {
            return self.declare_var();
        }

        self.statement()
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.advance_match(&[TokenType::IF]) {
            return self.if_statement();
        }
        if self.advance_match(&[TokenType::PRINT]) {
            return self.print_statement();
        }
        if self.advance_match(&[TokenType::RETURN]) {
            return self.return_statement();
        }
        if self.advance_match(&[TokenType::FOR]) {
            return self.for_statement();
        }
        if self.advance_match(&[TokenType::WHILE]) {
            return self.while_statement();
        }
        if self.advance_match(&[TokenType::LEFT_BRACE]) {
            return self.block();
        }

        self.expr_statement()
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(
            TokenType::LEFT_PAREN,
            "Condition must be enclosed in parentheses",
        )?;
        let cond = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN, "Unclosed parentheses")?;

        let then_branch = self.statement()?;
        let mut else_branch: Option<Box<Stmt>> = None;

        if self.advance_match(&[TokenType::ELSE]) {
            else_branch = Some(Box::new(self.statement()?));
        }

        return Ok(Stmt::If {
            condition: Box::new(cond),
            then_branch: Box::new(then_branch),
            else_branch,
        });
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LEFT_PAREN, "For must be followed by parentheses")?;
        let init = self.declaration()?;
        match init {
            Stmt::Var(_) | Stmt::Expression(_) => (),
            _ => {
                return Err(InterpreterError::new(
                    1,
                    "For initialization part can only be an expression or a declaration",
                    CustomError::ScannerError(ScannerError::SyntaxError),
                ));
            }
        };
        let cond = self.comma()?;
        self.consume(TokenType::SEMICOLON, "Missing delimiter: ;")?;
        let post = self.comma()?;
        self.consume(TokenType::RIGHT_PAREN, "Unclosed parentheses")?;
        let body = self.statement()?;

        Ok(Stmt::For {
            init: Box::new(init),
            cond: Box::new(cond),
            post: Box::new(post),
            body: Box::new(body),
        })
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.previous().clone();

        let mut value = None;

        if !self.check(&TokenType::SEMICOLON) {
            value = Some(Box::new(self.expression()?));
        }

        self.consume(TokenType::SEMICOLON, "Expect ';' after return statement");

        Ok(Stmt::Return { keyword, value })
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(
            TokenType::LEFT_PAREN,
            "Condition must be enclosed in parentheses",
        )?;
        let cond = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN, "Unclosed parentheses")?;
        let body = self.statement()?;

        return Ok(Stmt::While {
            condition: Box::new(cond),
            body: Box::new(body),
        });
    }

    fn block(&mut self) -> Result<Stmt> {
        let mut statements = vec![];

        while !self.check(&TokenType::RIGHT_BRACE) && !self.is_eof() {
            statements.push(Box::new(self.declaration()?));
        }

        self.consume(
            TokenType::RIGHT_BRACE,
            "Missing } after block initialization",
        )?;
        Ok(Stmt::new_block(statements))
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

    fn or(&mut self) -> Result<Expr> {
        self.create_logical_expr(Parser::and, &[TokenType::OR])
    }

    fn and(&mut self) -> Result<Expr> {
        self.create_logical_expr(Parser::ternary, &[TokenType::AND])
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
        let expr = self.or()?;

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
                Expr::Get(get) => {
                    return Ok(Expr::Set(Set {
                        object: get.object.clone(),
                        name: get.name,
                        value: Box::new(value),
                    }));
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

    fn create_logical_expr<F: Fn(&mut Parser<'a>) -> Result<Expr>>(
        &mut self,
        operation: F,
        token_types: &[TokenType],
    ) -> Result<Expr> {
        let mut expr = operation(self)?;

        while self.advance_match(token_types) {
            let operator = self.previous().clone();
            let right = operation(self)?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
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

        self.call()
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.advance_match(&[TokenType::LEFT_PAREN]) {
                expr = self.finish_call(expr)?;
            } else if self.advance_match(&[TokenType::DOT]) {
                let name = self.consume(
                    TokenType::IDENTIFIER,
                    "Expect identifier after attribute access '.'.",
                )?;
                expr = Expr::Get(Get {
                    object: Rc::new(RefCell::new(expr)),
                    name,
                })
            } else {
                break;
            }
        }

        return Ok(expr);
    }

    fn finish_call(&mut self, expr: Expr) -> Result<Expr> {
        let mut args = vec![];

        if !self.check(&TokenType::RIGHT_PAREN) {
            loop {
                args.push(Box::new(self.expression()?));
                if !self.advance_match(&[TokenType::COMMA]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RIGHT_PAREN, "Missing ) in function call")?;
        if args.len() >= FN_ARG_LIMIT {
            return Err(InterpreterError::new(
                paren.line,
                &format!(
                    "Maximum arguments allowed in a function call is: {}",
                    FN_ARG_LIMIT
                ),
                CustomError::ScannerError(ScannerError::SyntaxError),
            ));
        }

        return Ok(Expr::Call(Call::new(Box::new(expr), paren, args)));
    }

    fn primary(&mut self) -> Result<Expr> {
        if self.advance_match(&[TokenType::SUPER]) {
            let keyword = self.previous().clone();
            self.consume(TokenType::DOT, "Expected '.' after super");
            let method = self.consume(
                TokenType::IDENTIFIER,
                "Expected method identifier after super",
            )?;
            return Ok(Expr::Super { keyword, method });
        }
        if self.advance_match(&[TokenType::THIS]) {
            return Ok(Expr::This(self.previous().clone()));
        }
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
            TokenType::IDENTIFIER => {
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
            self.peek().line,
            &format!("Expected expression at: {}", self.peek()),
            CustomError::ParserError(ParserError::InvalidStatement),
        ))
    }

    fn consume(&mut self, token_type: TokenType, expect: &'a str) -> Result<Token> {
        if self.check(&token_type) {
            return Ok(self.advance().clone());
        }

        Err(InterpreterError::new(
            self.peek().line,
            expect,
            CustomError::ParserError(ParserError::UnterminatedToken),
        ))
    }
}

impl<'a> Parser<'a> {
    fn class(&mut self) -> Result<Stmt> {
        let name = self.consume(TokenType::IDENTIFIER, "Expect class name")?;
        let mut super_class = None;

        if self.advance_match(&[TokenType::LESS]) {
            super_class = Some(Box::new(Expr::Variable(
                self.consume(TokenType::IDENTIFIER, "Expect super class name")?,
            )));
        }

        self.consume(TokenType::LEFT_BRACE, "Expect '{' before class body")?;

        let mut methods = Vec::new();

        while !self.check(&TokenType::RIGHT_BRACE) || self.is_eof() {
            methods.push(Rc::new(RefCell::new(self.function("method")?)));
        }

        self.consume(TokenType::RIGHT_BRACE, "Expect '}' after class body")?;

        Ok(Stmt::Class {
            name,
            methods,
            super_class,
        })
    }

    fn function(&mut self, kind: &str) -> Result<Stmt> {
        let name = self.consume(TokenType::IDENTIFIER, "Expect identifier name")?;
        self.consume(TokenType::LEFT_PAREN, "Expect {} name")?;

        let mut params = vec![];

        if !self.check(&TokenType::RIGHT_PAREN) {
            loop {
                if params.len() >= 8 {
                    return Err(InterpreterError::new(
                        self.peek().line,
                        "Can not have more than 8 parameters",
                        CustomError::ParserError(ParserError::InvalidStatement),
                    ));
                }

                params.push(self.consume(TokenType::IDENTIFIER, "Missing parameter name")?);

                if !self.advance_match(&[TokenType::COMMA]) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RIGHT_PAREN,
            "Unclosed parentheses after parameters.",
        )?;

        self.consume(TokenType::LEFT_BRACE, "Expected closing brace")?;

        let body = self.block()?;

        Ok(Stmt::Function {
            name,
            params,
            body: Rc::new(RefCell::new(body)),
        })
    }

    fn declare_var(&mut self) -> Result<Stmt> {
        let name = self.advance().clone();
        match name.token_type {
            TokenType::IDENTIFIER => (),
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
