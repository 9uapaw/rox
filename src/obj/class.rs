use crate::ast::{Expr, RoxObject};
use crate::error::Result;
use crate::error::{CustomError, InterpreterError, RuntimeError};
use crate::obj::function::{Callable, CallableObj};
use crate::obj::Interpretable;
use crate::scanner::Token;
use crate::variable::{Environment, RcObj};
use core::fmt;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub enum ClassType {
    CLASS,
    SUBCLASS
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassObjInternal {
    pub name: Token,
    pub methods: HashMap<String, RcObj>,
    pub super_class: Option<RcObj>,
}

impl ClassObjInternal {
    fn find_method(&self, name: &str) -> Option<RcObj> {
        let mut found_method = None;
        if self.methods.contains_key(name) {
            if let Some(method) = self.methods.get(name) {
                found_method = Some(method.clone());
            }
        } else {
            if let Some(ref super_cls) = self.super_class {
                match *super_cls.as_ref().borrow() {
                    RoxObject::Callable(ref call) => match call {
                        Callable::ClassObj(cls_obj) => {
                            found_method = cls_obj.data.as_ref().borrow().find_method(name);
                        }
                        _ => (),
                    },
                    _ => (),
                }
            }
        }

        found_method
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassObj {
    data: Rc<RefCell<ClassObjInternal>>,
}

impl ClassObj {
    pub fn new(name: Token, methods: HashMap<String, RcObj>, super_class: Option<RcObj>) -> Self {
        ClassObj {
            data: Rc::new(RefCell::new(ClassObjInternal {
                name,
                methods,
                super_class,
            })),
        }
    }
}

impl ClassObj {
    pub fn find_method(&self, name: &Token) -> Result<RcObj> {
        let mut method = self.data.as_ref().borrow().find_method(&name.lexem);
        if method.is_none() {
            return Err(InterpreterError::new(
                name.line,
                &format!("Method named '{}' not found on super class", name.lexem),
                CustomError::RuntimeError(RuntimeError::NotFound),
            ));
        } else {
            return Ok(method.take().expect(""));
        }
    }
}

impl CallableObj for ClassObj {
    fn call(
        &self,
        args: Vec<Rc<RefCell<RoxObject>>>,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<RoxObject>>> {
        let instance = ClassInstanceObj::new(&self);
        if let Some(method) = self.data.as_ref().borrow().find_method("init") {
            match *method.as_ref().borrow() {
                RoxObject::Callable(ref callable) => match callable {
                    Callable::FnObj(ref fn_obj) => {
                        let m = fn_obj.bind(&instance);
                        m.call(args, env)?;
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        let obj = Rc::new(RefCell::new(RoxObject::Class(instance)));
        Ok(obj)
    }

    fn arity(&self) -> usize {
        if let Some(method) = self.data.as_ref().borrow().find_method("init") {
            match *method.as_ref().borrow() {
                RoxObject::Callable(ref callable) => match callable {
                    Callable::FnObj(ref fn_obj) => {
                        return fn_obj.arity();
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInstanceInternal {
    pub class_internal: Rc<RefCell<ClassObjInternal>>,
    pub fields: HashMap<String, RcObj>,
}

impl ClassInstanceInternal {
    pub fn new(class: &ClassObj) -> Self {
        let fields = HashMap::new();
        ClassInstanceInternal {
            class_internal: class.data.clone(),
            fields,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInstanceObj {
    data: Rc<RefCell<ClassInstanceInternal>>,
}

impl ClassInstanceObj {
    pub fn new(class: &ClassObj) -> Self {
        let data = Rc::new(RefCell::new(ClassInstanceInternal::new(class)));

        ClassInstanceObj { data }
    }

    pub fn get(&self, token: &Token) -> Result<RcObj> {
        if let Some(field) = self.data.as_ref().borrow().fields.get(&token.lexem) {
            return Ok(field.clone());
        } else if let Some(method) = self
            .data
            .as_ref()
            .borrow()
            .class_internal
            .as_ref()
            .borrow()
            .find_method(&token.lexem)
        {
            match *method.as_ref().borrow() {
                RoxObject::Callable(ref callable) => match callable {
                    Callable::FnObj(ref fn_obj) => {
                        return Ok(Rc::new(RefCell::new(RoxObject::Callable(Callable::FnObj(
                            fn_obj.bind(&self),
                        )))));
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        Err(InterpreterError::new(
            token.line,
            &format!(
                "Property {} is undefined on class instance {}",
                token.lexem,
                self.data
                    .as_ref()
                    .borrow()
                    .class_internal
                    .as_ref()
                    .borrow()
                    .name
                    .lexem
            ),
            CustomError::RuntimeError(RuntimeError::NotFound),
        ))
    }

    pub fn set(&mut self, token: &Token, value: RcObj) {
        self.data
            .borrow_mut()
            .fields
            .insert(token.lexem.clone(), value);
    }
}

impl Display for ClassInstanceObj {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "<{}: {:x}>",
            self.data
                .as_ref()
                .borrow()
                .class_internal
                .as_ref()
                .borrow()
                .name
                .lexem,
            self as *const ClassInstanceObj as u64
        )
    }
}

#[derive(Debug, Eq, Clone, PartialEq)]
pub struct Get {
    pub object: Rc<RefCell<Expr>>,
    pub name: Token,
}

impl Interpretable for Get {
    fn interpret(&self, env: Rc<RefCell<Environment>>) -> Result<Rc<RefCell<RoxObject>>> {
        let obj = self.object.as_ref().borrow().evaluate(env)?;

        let res = match *obj.as_ref().borrow() {
            RoxObject::Class(ref instance) => instance.get(&self.name),
            _ => Err(InterpreterError::new(
                self.name.line,
                &format!("Only class instances have properties."),
                CustomError::RuntimeError(RuntimeError::TypeError),
            )),
        };

        res
    }
}

impl Hash for Get {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.object.as_ref().borrow().hash(state)
    }
}

#[derive(Debug, Eq, Clone, PartialEq)]
pub struct Set {
    pub object: Rc<RefCell<Expr>>,
    pub name: Token,
    pub value: Box<Expr>,
}

impl Interpretable for Set {
    fn interpret(&self, env: Rc<RefCell<Environment>>) -> Result<Rc<RefCell<RoxObject>>> {
        let obj = self.object.as_ref().borrow().evaluate(env.clone())?;
        let evaluation = self.value.evaluate(env)?;

        let res = match *obj.as_ref().borrow_mut() {
            RoxObject::Class(ref mut instance) => {
                instance.set(&self.name, evaluation.clone());
                Ok(evaluation)
            }
            _ => Err(InterpreterError::new(
                self.name.line,
                &format!("Only class instances have properties."),
                CustomError::RuntimeError(RuntimeError::TypeError),
            )),
        };

        res
    }
}

impl Hash for Set {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.object.as_ref().borrow().hash(state)
    }
}
