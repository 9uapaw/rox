use crate::ast::RoxObject;
use crate::ast::{Expr, Stmt};
use crate::error::RuntimeError;
use crate::error::{CustomError, InterpreterError, Result};
use crate::obj::class::{ClassInstanceInternal, ClassInstanceObj, ClassObj};
use crate::obj::Interpretable;
use crate::scanner::Literal;
use crate::scanner::Token;
use crate::variable::{Env, Environment, RcObj};
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::SystemTime;

#[derive(Debug, Clone, PartialEq)]
pub enum FncType {
    Function,
    Method,
    Constructor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    Builtin(String),
    FnObj(FnObj),
    ClassObj(ClassObj),
}

impl Callable {
    pub fn call(&self, args: Vec<RcObj>, env: Env, line: usize) -> Result<RcObj> {
        if let Some(ref mut builtin) = self.get_builtin() {
            return builtin.call(args, env);
        }
        match self {
            Callable::FnObj(fun) => {
                if args.len() != fun.arity() {
                    return Err(InterpreterError::new(
                        line,
                        &format!(
                            "Function has {} arguments, {} provided",
                            args.len(),
                            fun.arity()
                        ),
                        CustomError::RuntimeError(RuntimeError::WrongArguments),
                    ));
                }

                return fun.call(args, env).or_else(|mut return_val| {
                    if let CustomError::Return(ref mut val) = return_val.error {
                        if fun.is_initializer {
                            let mut this_token = fun.name.clone();
                            this_token.lexem = String::from("this");
                            return fun.closure.borrow().get_at(&this_token, 0);
                        }
                        return Ok(val.take().unwrap());
                    } else {
                        return Err(return_val);
                    }
                });
            }
            Callable::ClassObj(class) => {
                if args.len() != class.arity() {
                    return Err(InterpreterError::new(
                        line,
                        &format!(
                            "Class constructor has {} arguments, {} provided",
                            args.len(),
                            class.arity()
                        ),
                        CustomError::RuntimeError(RuntimeError::WrongArguments),
                    ));
                }

                return class.call(args, env);
            }
            _ => {}
        };

        return Err(InterpreterError::new(
            line,
            "Unexpected error",
            CustomError::RuntimeError(RuntimeError::TypeError),
        ));
    }
    pub fn get_builtin(&self) -> Option<impl CallableObj> {
        match self {
            Callable::Builtin(builtin) => match builtin.as_str() {
                "clock" => Some(Clock {}),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clock {}

impl CallableObj for Clock {
    fn call(&self, args: Vec<RcObj>, env: Env) -> Result<RcObj> {
        return Ok(Rc::new(RefCell::new(RoxObject::Literal(Literal::Number(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .expect("Check time setup, serious error occured, the time has shifted")
                .as_secs() as f64,
        )))));
    }

    fn arity(&self) -> usize {
        return 0;
    }
}

pub trait CallableObj {
    fn call(&self, args: Vec<RcObj>, env: Env) -> Result<RcObj>;
    fn arity(&self) -> usize;
}

#[derive(PartialEq, Clone, Debug)]
pub struct FnObj {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Rc<RefCell<Stmt>>,
    pub closure: Env,
    is_initializer: bool,
}

impl FnObj {
    pub fn new(
        name: Token,
        params: Vec<Token>,
        body: Rc<RefCell<Stmt>>,
        closure: Env,
        is_initializer: bool,
    ) -> FnObj {
        FnObj {
            name,
            params,
            body,
            closure,
            is_initializer,
        }
    }
}

impl CallableObj for FnObj {
    fn call(&self, args: Vec<RcObj>, env: Env) -> Result<RcObj> {
        let mut local_env =
            Environment::new_with_parent(self.closure.clone(), env.borrow().locals_ref.clone());

        for (i, param) in self.params.iter().enumerate() {
            local_env.wrap(&param.lexem, args.get(i).unwrap().clone());
        }

        self.body
            .as_ref()
            .borrow()
            .evaluate(Rc::new(RefCell::new(local_env)))?;

        if self.is_initializer {
            let mut this_token = self.name.clone();
            this_token.lexem = String::from("this");
            return self.closure.borrow().get_at(&this_token, 0);
        }

        Ok(Rc::new(RefCell::new(RoxObject::Literal(Literal::Null))))
    }

    fn arity(&self) -> usize {
        self.params.len()
    }
}

impl FnObj {
    pub fn bind(&self, class: &ClassInstanceObj) -> FnObj {
        let mut local_env = Environment::new_with_parent(
            self.closure.clone(),
            self.closure.borrow().locals_ref.clone(),
        );
        local_env.wrap(
            "this",
            Rc::new(RefCell::new(RoxObject::Class(class.clone()))),
        );

        Self::new(
            self.name.clone(),
            self.params.clone(),
            self.body.clone(),
            Rc::new(RefCell::new(local_env)),
            self.is_initializer,
        )
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub struct Call {
    pub callee: Box<Expr>,
    paren: Token,
    pub args: Vec<Box<Expr>>,
}

impl Call {
    pub fn new(callee: Box<Expr>, paren: Token, args: Vec<Box<Expr>>) -> Self {
        Call {
            callee,
            paren,
            args,
        }
    }
}

impl Interpretable for Call {
    fn interpret(&self, env: Env) -> Result<RcObj> {
        let callee = self.callee.evaluate(env.clone())?;

        let mut evaluated_args = vec![];

        for arg in &self.args {
            evaluated_args.push(arg.evaluate(env.clone())?);
        }

        let fn_call_res = match *callee.as_ref().borrow() {
            RoxObject::Callable(ref callable) => {
                callable.call(evaluated_args, env, self.paren.line)
            }
            _ => {
                return Err(InterpreterError::new(
                    self.paren.line,
                    "Object is not callable",
                    CustomError::RuntimeError(RuntimeError::TypeError),
                ));
            }
        };

        fn_call_res
    }
}
