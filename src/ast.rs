use crate::error::{CustomError, InterpreterError, Result, RuntimeError};
use crate::interpreter::{
    interpret_binary, interpret_literal, interpret_logical, interpret_ternary,
};
use crate::interpreter::{interpret_unary, is_rox_obj_truthy};
use crate::obj::class::{ClassInstanceObj, ClassObj, Get, Set};
use crate::obj::function::{Call, Callable, FnObj};
use crate::obj::Interpretable;
use crate::scanner::Literal;
use crate::scanner::Token;
use crate::variable::{Env, Environment, RcObj, Var};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum RoxObject {
    Literal(Literal),
    Callable(Callable),
    Class(ClassInstanceObj),
}

impl Display for RoxObject {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            RoxObject::Literal(lit) => write!(f, "{}", lit),
            RoxObject::Callable(fn_obj) => write!(f, "{:?}", fn_obj),
            RoxObject::Class(class_obj) => write!(f, "{}", class_obj),
        }
    }
}

#[derive(PartialEq, Clone, Hash, Debug, Eq)]
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
    Call(Call),
    Get(Get),
    Set(Set),
    This(Token),
    Super {
        keyword: Token,
        method: Token,
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
            Expr::Call(call) => write!(f, "{:?}", call),
            Expr::Get(get) => write!(f, "{:?}", get),
            Expr::Set(set) => write!(f, "{:?}", set),
            Expr::Ternary {
                condition,
                left,
                right,
            } => write!(f, "({} ? {} : {})", condition, left, right),
            Expr::Variable(name) => write!(f, "(var {})", name),
            Expr::Assign { name, value } => write!(f, "{} = {}", name, value),
            Expr::This(keyword) => write!(f, "(instance {})", keyword),
            Expr::Super { keyword, method } => write!(f, "({}.{})", keyword, method),
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
            Expr::Call(call) => call.interpret(env),
            Expr::Get(get) => get.interpret(env),
            Expr::Set(set) => set.interpret(env),
            Expr::This(keyword) => env
                .as_ref()
                .borrow()
                .lookup_variable(keyword, self.get_hash()),
            Expr::Super { keyword, method } => {
                let mut this_token = keyword.clone();
                this_token.lexem = String::from("this");
                let locals = env.as_ref().borrow().locals_ref.clone();
                let distance = locals
                    .as_ref()
                    .borrow()
                    .get(&self.get_hash())
                    .expect("Unexpected error at super resolution")
                    .clone();
                let super_class = env.as_ref().borrow().get_at(keyword, distance)?;
                let instance = env.as_ref().borrow().get_at(&this_token, distance - 1)?;
                let mut found_method = None;

                if let RoxObject::Callable(ref call) = *super_class.as_ref().borrow() {
                    match call {
                        Callable::ClassObj(ref super_cls) => match *instance.as_ref().borrow() {
                            RoxObject::Class(ref obj) => {
                                found_method = Some(super_cls.find_method(method)?);
                                if let Some(found_method) = found_method {
                                    match *found_method.as_ref().borrow() {
                                        RoxObject::Callable(ref method_call) => match method_call {
                                            Callable::FnObj(ref method_obj) => {
                                                return Ok(Rc::new(RefCell::new(
                                                    RoxObject::Callable(Callable::FnObj(
                                                        method_obj.bind(obj),
                                                    )),
                                                )));
                                            }
                                            _ => (),
                                        },
                                        _ => (),
                                    }
                                }
                            }
                            _ => (),
                        },
                        _ => (),
                    }
                }

                Err(InterpreterError::new(
                    keyword.line,
                    "Error resolving super class",
                    CustomError::UnknownError,
                ))
            }
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
            Expr::Variable(token) => (*env).borrow().lookup_variable(token, self.get_hash()),
            Expr::Assign { name, value } => {
                let res = value.evaluate(env.clone())?;
                println!("Assign value: {:?} - hash: {:?}", res, self.get_hash());
                (*env)
                    .borrow_mut()
                    .assign_variable(name, res, self.get_hash())?;
                (*env).borrow().lookup_variable(name, self.get_hash())
            }
            _ => Err(InterpreterError::new(
                0,
                &format!("Undefined expression handling on {}", &self),
                CustomError::UnknownError,
            )),
        }
    }
}

impl Expr {
    pub fn get_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

#[derive(PartialEq, Clone, Debug)]
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
        body: Box<Stmt>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Rc<RefCell<Stmt>>,
    },
    Return {
        keyword: Token,
        value: Option<Box<Expr>>,
    },
    Class {
        name: Token,
        methods: Vec<Rc<RefCell<Stmt>>>,
        super_class: Option<Box<Expr>>,
    },
}

impl<'a> Stmt {
    pub fn new_block(stmts: Vec<Box<Stmt>>) -> Self {
        Stmt::Block(Block::new(stmts))
    }

    pub fn evaluate(&self, env: Env) -> Result<()> {
        match self {
            Stmt::Print(expr) => println!("{}", expr.evaluate(env)?.as_ref().borrow()),
            Stmt::Expression(expr) => {
                expr.evaluate(env)?;
            }
            Stmt::Var(var) => {
                let evaluated_init = var.initializer.evaluate(env.clone())?;
                (*env).borrow_mut().wrap(&var.name.lexem, evaluated_init);
            }
            Stmt::Block(ref block) => block.execute(env)?,
            Stmt::If {
                condition: cond,
                ref then_branch,
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
            Stmt::Return { keyword, value } => {
                let ret_val = if let Some(return_val) = value {
                    Some(return_val.evaluate(env)?)
                } else {
                    Some(Rc::new(RefCell::new(RoxObject::Literal(Literal::Null))))
                };

                return Err(InterpreterError::new(
                    keyword.line,
                    "Return from a function",
                    CustomError::Return(ret_val),
                ));
            }
            Stmt::Function { name, params, body } => {
                (*env).borrow_mut().wrap(
                    &name.lexem,
                    Rc::new(RefCell::new(RoxObject::Callable(Callable::FnObj(
                        FnObj::new(
                            name.clone(),
                            params.clone(),
                            body.clone(),
                            env.clone(),
                            false,
                        ),
                    )))),
                );
            }
            Stmt::While {
                condition,
                ref body,
            } => {
                while is_rox_obj_truthy(condition.evaluate(env.clone())?) {
                    body.evaluate(env.clone())?;
                }
            }
            Stmt::For {
                ref init,
                cond,
                post,
                ref body,
            } => {
                init.evaluate(env.clone())?;
                loop {
                    if !is_rox_obj_truthy(cond.evaluate(env.clone())?) {
                        break;
                    } else {
                        body.evaluate(env.clone())?;
                    }
                    post.evaluate(env.clone())?;
                }
            }
            Stmt::Class {
                name,
                methods,
                super_class,
            } => {
                let mut evaluated_parent = None;
                let mut new_env = Rc::new(RefCell::new(Environment::new_with_parent(
                    env.clone(),
                    env.as_ref().borrow().locals_ref.clone(),
                )));
                if let Some(sup_class) = super_class {
                    evaluated_parent = Some(sup_class.evaluate(env.clone())?);
                }
                if let Some(ref sup_class) = evaluated_parent {
                    new_env.borrow_mut().wrap("super", sup_class.clone());
                }
                let mut method_map = HashMap::new();

                for method in methods {
                    if let Stmt::Function {
                        ref name,
                        ref params,
                        ref body,
                    } = *method.as_ref().borrow()
                    {
                        method_map.insert(
                            name.lexem.clone(),
                            Rc::new(RefCell::new(RoxObject::Callable(Callable::FnObj(
                                FnObj::new(
                                    name.clone(),
                                    params.clone(),
                                    body.clone(),
                                    new_env.clone(),
                                    &name.lexem == "init",
                                ),
                            )))),
                        );
                    }
                }

                if let Some(sup_class) = &evaluated_parent {
                    if let RoxObject::Callable(ref call) = *sup_class.as_ref().borrow() {
                        if let Callable::ClassObj(_) = call {
                            ()
                        } else {
                            return Err(InterpreterError::new(
                                name.line,
                                "Superclass must be a class object",
                                CustomError::RuntimeError(RuntimeError::TypeError),
                            ));
                        }
                    } else {
                        return Err(InterpreterError::new(
                            name.line,
                            "Superclass must be a class object",
                            CustomError::RuntimeError(RuntimeError::TypeError),
                        ));
                    }
                }
                let class_obj = Rc::new(RefCell::new(RoxObject::Callable(Callable::ClassObj(
                    ClassObj::new(name.clone(), method_map, evaluated_parent),
                ))));
                env.as_ref().borrow_mut().wrap(&name.lexem, class_obj);
            }
        };

        Ok(())
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Block {
    pub statements: Vec<Box<Stmt>>,
}

impl<'a> Block {
    pub fn new(stmts: Vec<Box<Stmt>>) -> Block {
        Block { statements: stmts }
    }

    pub fn execute(&self, parent_env: Env) -> Result<()> {
        let env = Environment::new_with_parent(
            parent_env.clone(),
            parent_env.as_ref().borrow().locals_ref.clone(),
        );
        let env_ref = Rc::new(RefCell::new(env));

        for stmt in self.statements.iter() {
            stmt.evaluate(env_ref.clone())?;
        }

        Ok(())
    }
}
