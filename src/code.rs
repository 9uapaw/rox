use crate::scanner::Literal;
use crate::scanner::Token;
use std::fmt::Display;
use std::fmt;
use std::fmt::Formatter;

pub enum Expr {
    Binary{left: Box<Expr>, operator: Token, right: Box<Expr>},
    Grouping{expression: Box<Expr>},
    Literal{literal: Literal},
    Unary{operator: Token, right: Box<Expr>}
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expr::Binary{left, operator, right} => write!(f, "({} {} {})", left, operator, right),
            Expr::Grouping {expression} => write!(f, "({})", expression),
            Expr::Literal {literal} => write!(f, "({})", literal),
            Expr::Unary {operator, right} => write!(f, "({}{})", operator, right)
        }
    }
}



