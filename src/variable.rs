use crate::ast::{Expr, RoxObject};
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
pub type Locals = Rc<RefCell<HashMap<u64, usize>>>;

#[derive(PartialEq, Clone, Debug)]
pub struct Environment {
    values: HashMap<String, RcObj>,
    enclosing: Option<Env>,
    pub locals_ref: Locals,
}

impl<'a, 'b> Environment {
    pub fn new(locals_ref: Locals) -> Environment {
        let values = HashMap::new();
        Environment {
            values,
            enclosing: None,
            locals_ref,
        }
    }

    pub fn new_with_parent(parent: Env, locals_ref: Locals) -> Environment {
        let values = HashMap::new();
        Environment {
            values,
            enclosing: Some(parent),
            locals_ref,
        }
    }

    pub fn get_at(&self, name: &Token, distance: usize) -> Result<RcObj> {
        let mut env = None;
        if distance == 0 {
            return Ok(self
                .values
                .get(&name.lexem)
                .expect(&format!(
                    "Unexpected error at environment get_at on finding {} in {:?} on line {}",
                    name.lexem, &self.values.keys(), name.line
                ))
                .clone());
        }
        env = self.find_ancestor(
            self.enclosing
                .as_ref()
                .expect("Unexpected resolution error")
                .clone(),
            1,
            distance,
        );
        return Ok(env
            .expect("Unexpected resolution error")
            .as_ref()
            .borrow()
            .values
            .get(&name.lexem)
            .expect(&format!(
                "Unexpected error at environment get_at on finding {} in {:?} on line {}",
                name.lexem, &self.values.keys(), name.line
            ))
            .clone());
    }

    pub fn lookup_variable(&self, name: &Token, hash: u64) -> Result<RcObj> {
        let mut env = None;
        if let Some(distance) = self.locals_ref.borrow_mut().get(&hash) {
            if *distance == 0 {
                return Ok(self
                    .values
                    .get(&name.lexem)
                    .expect(&format!(
                        "Variable lookup key is not secured on line {} with values: {:?}",
                        &name.line,
                        self.values.keys()
                    ))
                    .clone());
            }
            env = self.find_ancestor(
                self.enclosing
                    .as_ref()
                    .expect("Unexpected resolution error")
                    .clone(),
                1,
                *distance,
            );
        } else {
            if let Some(parent) = self.enclosing.clone() {
                env = Some(self.find_global_env(parent.clone()))
            } else {
                if let Some(value) = self.values.get(&name.lexem) {
                    return Ok(value.clone());
                } else {
                    return Err(InterpreterError::new(
                        name.line,
                        &format!("Undeclared variable '{}'", &name.lexem),
                        CustomError::RuntimeError(RuntimeError::NotFound),
                    ));
                }
            }
        }
        //        println!("{:?}", env.as_ref().expect("").as_ref().borrow().values.keys());

        return Ok(env
            .expect("Unexpected resolution error")
            .as_ref()
            .borrow()
            .values
            .get(&name.lexem)
            .expect(&format!(
                "Variable '{}' not found in environment on line {}",
                &name.lexem, &name.line
            ))
            .clone());
    }

    pub fn assign_variable(&mut self, name: &Token, value: RcObj, hash: u64) -> Result<()> {
        if let Some(distance) = self.locals_ref.borrow_mut().get(&hash) {
            let mut env = self.find_ancestor(
                self.enclosing
                    .as_ref()
                    .expect("Unexpected resolution")
                    .clone(),
                1,
                *distance,
            );
            env.expect("Broken Environment resolution")
                .borrow_mut()
                .values
                .insert(String::from(name.lexem.as_str()), value);
        } else {
            if let Some(parent) = self.enclosing.clone() {
                let mut env = self.find_global_env(parent.clone());
                env.borrow_mut()
                    .values
                    .insert(String::from(name.lexem.as_str()), value);
            } else {
                self.values.insert(String::from(name.lexem.as_str()), value);
            }
        }

        Ok(())
    }

    pub fn wrap(&mut self, name: &str, obj: RcObj) {
        self.values.insert(String::from(name), obj);
    }

    fn find_global_env(&self, env: Env) -> Env {
        if let Some(parent) = &env.as_ref().borrow().enclosing {
            self.find_global_env(parent.clone())
        } else {
            env.clone()
        }
    }

    fn find_ancestor(&self, env: Env, current: usize, distance: usize) -> Option<Env> {
        //        println!("current: {}, distance: {}, env: {:?}", current, distance, env.as_ref().borrow().values.keys());
        if current != distance {
            let current_env = env
                .as_ref()
                .borrow()
                .enclosing
                .as_ref()
                .expect("Unexpected resolution error")
                .clone();
            self.find_ancestor(current_env, current + 1, distance)
        } else {
            Some(env)
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
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
