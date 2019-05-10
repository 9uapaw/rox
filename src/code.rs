use crate::error::Result;
use crate::interpreter::interpret_unary;
use crate::interpreter::{interpret_binary, interpret_literal, interpret_ternary};
use crate::scanner::Literal;
use crate::scanner::Token;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Debug, Clone, PartialEq)]
pub enum RoxObject {
    Literal(Literal),
}

pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        literal: Literal,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Ternary {
        condition: Box<Expr>,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expr::Grouping { expression } => write!(f, "({})", expression),
            Expr::Literal { literal } => write!(f, "({})", literal),
            Expr::Unary { operator, right } => write!(f, "({}{})", operator, right),
            Expr::Ternary {
                condition,
                left,
                right,
            } => write!(f, "({} ? {} : {})", condition, left, right),
        }
    }
}

impl Expr {
    pub fn interpret(&self) -> Result<RoxObject> {
        match &self {
            Expr::Literal { literal } => interpret_literal(literal),
            Expr::Grouping { expression } => expression.interpret(),
            Expr::Unary { operator, right } => interpret_unary(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => interpret_binary(left, operator, right),
            Expr::Ternary {
                condition,
                left,
                right,
            } => interpret_ternary(condition, left, right),
        }
    }
}
