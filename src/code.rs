use crate::error::{CustomError, InterpreterError, Result};
use crate::interpreter::{
    interpret_binary, interpret_literal, interpret_logical, interpret_ternary,
};
use crate::interpreter::{interpret_unary, is_rox_obj_truthy };
use crate::scanner::Literal;
use crate::scanner::Token;
use crate::variable::{Env, Environment, RcObj, Var};
use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum RoxObject {
    Literal(Literal),
}

impl Display for RoxObject {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
      match self {
          RoxObject::Literal(lit) => write!(f, "{}", lit)
      }
    }
}

pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Logical {
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
            Expr::Logical {
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
    pub fn evaluate(&self, env: Env) -> Result<RcObj> {
        match &self {
            Expr::Literal { literal } => interpret_literal(literal),
            Expr::Grouping { expression } => expression.evaluate(env),
            Expr::Unary { operator, right } => Ok(Rc::new(RefCell::new(interpret_unary(
                operator, right, env,
            )?))),
            Expr::Binary {
                left,
                operator,
                right,
            } => Ok(Rc::new(RefCell::new(interpret_binary(
                left, operator, right, env,
            )?))),
            Expr::Logical {
                left,
                operator,
                right,
            } => interpret_logical(left, operator, right, env),
            Expr::Ternary {
                condition,
                left,
                right,
            } => interpret_ternary(condition, left, right, env),
            Expr::Variable(token) => (*env).borrow_mut().get(token),
            Expr::Assign { name, value } => {
                let res = value.evaluate(env.clone())?;
                (*env).borrow_mut().assign(&name.lexem, res)?;
                (*env).borrow_mut().get(name)
            }
            _ => Err(InterpreterError::new(
                0,
                &format!("Unexpected error on: {}", &self),
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
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    For {
        init: Box<Stmt>,
        cond: Box<Expr>,
        post: Box<Expr>,
        body: Box<Stmt>
    }
}

impl<'a> Stmt {
    pub fn new_block(stmts: Vec<Box<Stmt>>) -> Self {
        Stmt::Block(Block::new(stmts))
    }

    pub fn evaluate(&self, env: Env) -> Result<()> {
        match &self {
            Stmt::Print(expr) => println!("{:?}", self.interpret_expr(expr, env, true)?),
            Stmt::Expression(expr) => {
                self.interpret_expr(expr, env, false)?;
            }
            Stmt::Var(var) => (*env)
                .borrow_mut()
                .wrap(&var.name.lexem, var.initializer.evaluate(env.clone())?),
            Stmt::Block(ref block) => block.execute(env)?,
            Stmt::If {
                condition: cond,
                then_branch,
                else_branch,
            } => {
                if is_rox_obj_truthy(cond.evaluate(env.clone())?) {
                    then_branch.evaluate(env.clone())?
                } else {
                    match else_branch {
                        Some(ref stmt) => stmt.evaluate(env)?,
                        None => (),
                    }
                }
            }
            Stmt::While {
                condition,
                ref body,
            } => {
                while is_rox_obj_truthy(condition.evaluate(env.clone())?) {
                    body.evaluate(env.clone())?;
                }
            }
            Stmt::For {init, cond, post, body} => {
                init.evaluate(env.clone())?;
                loop {
                    if !is_rox_obj_truthy(cond.evaluate(env.clone())?) {
                        break
                    } else {
                        body.evaluate(env.clone())?;
                    }
                    post.evaluate(env.clone())?;
                }
            }
        };

        Ok(())
    }

    fn interpret_expr(&self, expr: &Box<Expr>, env: Env, print: bool) -> Result<()> {
        if print {
            println!("{}", expr.evaluate(env)?.as_ref().borrow());
        } else {
            expr.evaluate(env)?;
        }

        Ok(())
    }
}

pub struct Block {
    statements: Vec<Box<Stmt>>,
}

impl<'a> Block {
    pub fn new(stmts: Vec<Box<Stmt>>) -> Block {
        Block { statements: stmts }
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
