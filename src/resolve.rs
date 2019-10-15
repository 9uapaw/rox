use crate::ast::Expr;
use crate::ast::{Block, Stmt};
use crate::error::ParserError;
use crate::error::{CompileError, CustomError, InterpreterError, Result};
use crate::interpreter::Interpreter;
use crate::obj::class::ClassType;
use crate::obj::function::FncType;
use crate::scanner::{Literal, Token};
use crate::variable::{Env, Var};
use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Resolver<'a> {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: &'a mut Interpreter,
    current_function: Option<FncType>,
    current_class: Option<ClassType>,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Resolver<'a> {
        Resolver {
            scopes: Vec::new(),
            interpreter,
            current_function: None,
            current_class: None,
        }
    }

    pub fn resolve_block(&mut self, block: &mut Block) -> Result<()> {
        self.begin_scope();
        self.resolve_boxed(&mut (block.statements))?;

        self.end_scope();

        Ok(())
    }

    pub fn resolve_var(&mut self, var: &mut Var) -> Result<()> {
        self.declare(&var.name)?;

        if &var.name.lexem == "this" && self.current_class.is_none() {
            return Err(InterpreterError::new(
                var.name.line,
                "'this' is only defined inside class instance methods.",
                CustomError::CompileError(CompileError::InvalidDeclaration),
            ));
        }

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
        fun_type: FncType,
    ) -> Result<()> {
        self.declare(name)?;
        self.define(name);

        let enclosing_function = self.current_function.take();
        self.current_function = Some(fun_type);

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

    fn resolve_class(
        &mut self,
        name: &Token,
        methods: &Vec<Rc<RefCell<Stmt>>>,
        super_class: &Option<Box<Expr>>,
    ) -> Result<()> {
        let current_class = self.current_class.take();
        self.current_class = Some(ClassType::CLASS);
        self.declare(name)?;
        self.define(name);

        if let Some(super_class) = super_class {
            self.current_class = Some(ClassType::SUBCLASS);
            if let Expr::Variable(ref super_name) = *super_class.as_ref().borrow() {
                if super_name.lexem == name.lexem {
                    return Err(InterpreterError::new(
                        name.line,
                        "Class can not inherit from itself",
                        CustomError::CompileError(CompileError::TypeError),
                    ));
                }
            }
            self.accept_expr(super_class.as_ref().borrow())?;

            self.begin_scope();
            self.scopes.last_mut().expect("").insert(String::from("super"), true);
        }

        self.begin_scope();
        self.scopes
            .last_mut()
            .expect("")
            .insert(String::from("this"), true);

        for method in methods {
            if let Stmt::Function {
                ref name,
                ref params,
                ref body,
            } = *method.as_ref().borrow()
            {
                self.resolve_fn(&name, &params, body.clone(), FncType::Method)?;
            }
        }

        self.end_scope();
        if super_class.is_some() {
            self.end_scope();
        }

        self.current_class = current_class;
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
            Stmt::Expression(expr) => self.accept_expr((*expr).borrow()),
            Stmt::If {
                else_branch,
                then_branch,
                condition,
            } => {
                self.accept_expr((*condition).borrow())?;
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
                } else if let Some(ref fnc_type) = self.current_function {
                    if *fnc_type == FncType::Constructor {
                        return Err(InterpreterError::new(
                            keyword.line,
                            "Error returning value from initializer",
                            CustomError::CompileError(CompileError::InvalidDeclaration),
                        ));
                    }
                }

                if let Some(ret_exp) = value {
                    self.accept_expr(ret_exp)?;
                }

                Ok(())
            }
            Stmt::Print(print) => self.accept_expr((*print).borrow()),
            Stmt::While { condition, body } => {
                self.accept_expr((*condition).borrow())?;
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
                self.accept_expr((*cond).borrow())?;
                self.accept_expr((*post).borrow())
            }
            Stmt::Function { name, params, body } => {
                self.resolve_fn(name, params, body.clone(), FncType::Function)
            }
            Stmt::Class {
                name,
                methods,
                super_class,
            } => self.resolve_class(name, methods, super_class),
        }
    }

    fn accept_expr(&mut self, expr: &Expr) -> Result<()> {
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
                self.accept_expr(value.borrow())?;
                self.resolve_local(expr.get_hash(), name)
            }
            Expr::This(keyword) => {
                if self.current_class.is_none() {
                    return Err(InterpreterError::new(
                        keyword.line,
                        "'this' is only defined inside class instance methods.",
                        CustomError::CompileError(CompileError::InvalidDeclaration),
                    ));
                }
                self.resolve_local(expr.get_hash(), keyword)
            }
            Expr::Super {keyword, method} => {
                match self.current_class {
                    Some(ClassType::CLASS) => {
                        return Err(InterpreterError::new(
                            keyword.line,
                            "Can not use 'super' without parent class",
                            CustomError::CompileError(CompileError::InvalidDeclaration),
                        ));
                    },
                    None => {
                        return Err(InterpreterError::new(
                            keyword.line,
                            "Can not use 'super' outside of a class",
                            CustomError::CompileError(CompileError::InvalidDeclaration),
                        ));
                    },
                    _ => ()
                }
                self.resolve_local(expr.get_hash(), keyword)
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                self.accept_expr(left.borrow())?;
                self.accept_expr(right.borrow())
            }
            Expr::Call(call) => {
                self.accept_expr(call.callee.borrow())?;

                for arg in call.args.iter() {
                    self.accept_expr(arg.borrow())?;
                }

                Ok(())
            }
            Expr::Get(get) => self.accept_expr(&(*get.object.as_ref().borrow()).borrow()),
            Expr::Set(set) => {
                self.accept_expr(&(*set.object.as_ref().borrow()).borrow())?;
                self.accept_expr(set.value.borrow())
            }
            Expr::Grouping { expression } => self.accept_expr(expression.borrow()),
            Expr::Literal { .. } => Ok(()),
            Expr::Ternary {
                condition,
                left,
                right,
            } => {
                self.accept_expr(condition.borrow())?;
                self.accept_expr(left.borrow())?;
                self.accept_expr(right.borrow())
            }
            Expr::Unary { right, .. } => self.accept_expr(right.borrow()),
            Expr::Logical { left, right, .. } => {
                self.accept_expr(left.borrow())?;
                self.accept_expr(right.borrow())
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
