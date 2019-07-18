use crate::error::{CustomError, InterpreterError, Result};
use crate::interpreter::interpret_unary;
use crate::interpreter::{interpret_binary, interpret_literal, interpret_ternary};
use crate::scanner::Literal;
use crate::scanner::Token;
use crate::variable::{Environment, Var, Env, RcObj};
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::cell::RefCell;
use std::rc::Rc;
use std::borrow::{Borrow, BorrowMut};

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
    Variable(Token),
    Assign {
        name: Token,
        value: Box<Expr>,
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
            Expr::Variable(name) => write!(f, "(var {})", name),
            Expr::Assign { name, value } => write!(f, "{} = {}", name, value),
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
            _ => Err(InterpreterError::new(
                0,
                "Unexpected error",
                CustomError::UnknownError,
            )),
        }
    }
}
impl<'a> Expr {
    pub fn env_interpret(&self, environment: Env) -> Result<RcObj> {
        match &self {
            Expr::Variable(token) => (*environment).borrow_mut().get(token),
            Expr::Assign { name, value } => {
                let res = value.interpret()?;
                (*environment).borrow_mut().assign(&name.lexem, res)?;
                (*environment).borrow_mut().get(name)
            }
            _ => Err(InterpreterError::new(
                0,
                "Unexpected error",
                CustomError::UnknownError,
            )),
        }
    }
}
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var(Var),
    Block(Block),
}

impl<'a> Stmt {
    pub fn new_block(stmts: Vec<Box<Stmt>>) -> Self {
        Stmt::Block(Block::new(stmts))
    }

    pub fn evaluate(&self, environment: Env) -> Result<()> {
        match &self {
            Stmt::Print(expr) => println!("{:?}", self.interpret(expr, environment, true)?),
            Stmt::Expression(expr) => {
                self.interpret(expr, environment, false)?;
            }
            Stmt::Var(var) => (*environment).borrow_mut().define(&var.name.lexem, var.initializer.interpret()?),
            Stmt::Block(ref block) => {
                block.execute(environment)?
            }
        };

        Ok(())
    }

    fn interpret(&self, expr: &Box<Expr>, env: Env, print: bool) -> Result<()> {
        match **expr {
            Expr::Assign { .. } | Expr::Variable(_) => {
                if print {
                    println!("{:?}", expr.env_interpret(env)?.as_ref().borrow());
                } else {
                    expr.env_interpret(env)?;
                }
            }
            _ => {
                if print {
                    println!("{:?}", expr.interpret()?);
                } else {
                    expr.interpret()?;
                }
            }
        }

        Ok(())
    }
}

pub struct Block {
    statements: Vec<Box<Stmt>>
}

impl<'a> Block {

    pub fn new(stmts: Vec<Box<Stmt>>) -> Block {
        Block{statements: stmts}
    }

    pub fn execute(&self, parent_env: Env) -> Result<()> {
        let env = Environment::new_with_parent(parent_env.clone());
        let env_ref = Rc::new(RefCell::new(env));

        for stmt in &self.statements {
            stmt.evaluate(env_ref.clone())?;
        }

        Ok(())
    }
}
