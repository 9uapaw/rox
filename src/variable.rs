use crate::code::{Expr, RoxObject};
use crate::error::CustomError;
use crate::error::RuntimeError;
use crate::error::{InterpreterError, Result};
use crate::scanner::Token;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type Env = Rc<RefCell<Environment>>;
pub type RcObj = Rc<RefCell<RoxObject>>;

pub struct Environment {
    values: HashMap<String, RcObj>,
    enclosing: Option<Env>,
}

impl<'a, 'b> Environment {
    pub fn new() -> Environment {
        let values = HashMap::new();
        Environment {
            values,
            enclosing: None,
        }
    }

    pub fn new_with_parent(parent: Env) -> Environment {
        let values = HashMap::new();
        Environment {
            values,
            enclosing: Some(parent),
        }
    }

    pub fn get(&'a self, name: &Token) -> Result<RcObj> {
        let rox_obj = match self.values.get(&name.lexem) {
            Some(obj) => Ok(obj.clone()),
            None => {
                if let Some(ref parent_env) = self.enclosing {
                    parent_env.as_ref().borrow().get(name)
                } else {
                    Err(InterpreterError::new(
                        name.line,
                        &format!("Undefined variable {}", name.lexem),
                        CustomError::RuntimeError(RuntimeError::NotFound),
                    ))
                }
            }
        };

        rox_obj
    }

    pub fn define(&mut self, name: &str, value: RoxObject) {
        self.values
            .insert(String::from(name), Rc::new(RefCell::new(value)));
    }

    pub fn wrap(&mut self, name: &str, obj: RcObj) {
        self.values.insert(String::from(name), obj);
    }

    pub fn assign(&mut self, name: &str, value: RcObj) -> Result<()> {
        if self.values.contains_key(name) {
            self.values.insert(String::from(name), value);
            return Ok(());
        }

        if let Some(ref mut parent_env) = self.enclosing {
            return parent_env.borrow_mut().assign(name, value);
        } else {
            return Err(InterpreterError::new(
                1,
                &format!("Undefined variable {}", name),
                CustomError::RuntimeError(RuntimeError::NotFound),
            ));
        }
    }
}

pub struct Var {
    pub name: Token,
    pub initializer: Box<Expr>,
}

impl Var {
    pub fn new(name: Token, initializer: Box<Expr>) -> Var {
        Var { name, initializer }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "(var {})", self.name)
    }
}
