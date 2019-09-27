use crate::ast::Expr;
use crate::ast::{Block, Stmt};
use crate::error::ParserError;
use crate::error::{CompileError, CustomError, InterpreterError, Result};
use crate::interpreter::Interpreter;
use crate::obj::function::FncType;
use crate::scanner::{Literal, Token};
use crate::variable::Var;
use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Resolver<'a> {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: &'a mut Interpreter,
    current_function: Option<FncType>,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Resolver<'a> {
        Resolver {
            scopes: Vec::new(),
            interpreter,
            current_function: None,
        }
    }

    pub fn resolve_block(&mut self, block: &mut Block) -> Result<()> {
        self.begin_scope();
        self.resolve_boxed(&mut (block.statements))?;

        self.end_scope();

        Ok(())
    }

    pub fn resolve_block_without_scope(&mut self, block: &mut Block) -> Result<()> {
        self.resolve_boxed(&mut (block.statements))?;

        Ok(())
    }

    pub fn resolve_var(&mut self, var: &mut Var) -> Result<()> {
        self.declare(&var.name)?;

        match var.initializer.borrow() {
            Expr::Literal { literal } => match literal {
                Literal::Null => (),
                _ => self.accept_expr(&var.initializer)?,
            },
            _ => self.accept_expr(&var.initializer)?,
        };

        self.define(&var.name);
        Ok(())
    }

    pub fn resolve_fn(
        &mut self,
        name: &Token,
        params: &Vec<Token>,
        body: Rc<RefCell<Stmt>>,
    ) -> Result<()> {
        self.declare(name)?;
        self.define(name);

        let enclosing_function = self.current_function.take();
        self.current_function = Some(FncType::Function);

        self.begin_scope();

        for param in params {
            self.declare(param)?;
            self.define(param);
        }

        self.accept_stmt((*body.as_ref().borrow_mut()).borrow_mut())?;

        self.end_scope();
        self.current_function = enclosing_function;

        Ok(())
    }

    pub fn resolve_boxed(&mut self, statements: &mut Vec<Box<Stmt>>) -> Result<()> {
        for statement in statements {
            self.accept_stmt(statement)?;
        }

        Ok(())
    }

    pub fn resolve(&mut self, statements: &mut Vec<Stmt>) -> Result<()> {
        for statement in statements {
            self.accept_stmt(statement)?;
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result<()> {
        if let Some(scope) = self.scopes.iter_mut().last() {
            if scope.contains_key(&name.lexem) {
                return Err(InterpreterError::new(
                    name.line,
                    &format!("Variable '{}' already declared in this scope", name.lexem),
                    CustomError::CompileError(CompileError::InvalidDeclaration),
                ));
            }
            scope.insert(name.lexem.clone(), false);
        }

        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.iter_mut().last() {
            scope.insert(name.lexem.clone(), true);
        } else {
            return;
        }
    }

    fn accept_stmt(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Block(ref mut block) => self.resolve_block(block),
            Stmt::Var(ref mut var) => self.resolve_var(var),
            Stmt::Expression(ref mut expr) => self.accept_expr(expr),
            Stmt::If {
                else_branch,
                then_branch,
                condition,
            } => {
                self.accept_expr(condition)?;
                self.accept_stmt(then_branch.as_mut())?;
                if let Some(else_stmt) = else_branch {
                    self.accept_stmt(else_stmt.as_mut())?;
                }
                Ok(())
            }
            Stmt::Return { value, keyword } => {
                if self.current_function.is_none() {
                    return Err(InterpreterError::new(
                        keyword.line,
                        "Error returning from global scope",
                        CustomError::ParserError(ParserError::InvalidStatement),
                    ));
                }
                if let Some(ret_exp) = value {
                    self.accept_expr(ret_exp)?;
                }

                Ok(())
            }
            Stmt::Print(print) => self.accept_expr(print),
            Stmt::While { condition, body } => {
                self.accept_expr(condition)?;
                self.accept_stmt(body.as_mut())
            }
            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                self.accept_stmt(init.as_mut())?;
                self.accept_stmt(body.as_mut())?;
                self.accept_expr(cond)?;
                self.accept_expr(post)
            }
            Stmt::Function { name, params, body } => self.resolve_fn(name, params, body.clone()),
        }
    }

    fn accept_expr(&mut self, expr: &Box<Expr>) -> Result<()> {
        match expr.borrow() {
            Expr::Variable(name) => {
                if !self.scopes.is_empty()
                    && match self.scopes.last().unwrap().get(&name.lexem) {
                        Some(is_defined) => *is_defined,
                        None => true,
                    } == false
                {
                    for (k, val) in self.scopes.last().unwrap().iter() {
                        println!("{} {} {}", k, val, name);
                    }
                    return Err(InterpreterError::new(
                        name.line,
                        "\
                         Cannot read local variable in its own initializer",
                        CustomError::CompileError(CompileError::InvalidDeclaration),
                    ));
                }

                self.resolve_local(expr.get_hash(), name)?;
                Ok(())
            }
            Expr::Assign { name, value } => {
                self.accept_expr(value)?;
                self.resolve_local(expr.get_hash(), name)
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                self.accept_expr(left)?;
                self.accept_expr(right)
            }
            Expr::Call(call) => {
                self.accept_expr(&call.callee)?;

                for arg in call.args.iter() {
                    self.accept_expr(arg)?;
                }

                Ok(())
            }
            Expr::Grouping { expression } => self.accept_expr(expression),
            Expr::Literal { .. } => Ok(()),
            Expr::Ternary {
                condition,
                left,
                right,
            } => {
                self.accept_expr(condition)?;
                self.accept_expr(left)?;
                self.accept_expr(right)
            }
            Expr::Unary { right, .. } => self.accept_expr(right),
            Expr::Logical { left, right, .. } => {
                self.accept_expr(left)?;
                self.accept_expr(right)
            }
        }
    }

    fn resolve_local(&mut self, hash: u64, name: &Token) -> Result<()> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexem) {
                self.interpreter.resolve(hash, i);
                return Ok(());
            }
        }

        Ok(())
    }
}
