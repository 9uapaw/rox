use crate::code::RoxObject;
use crate::code::{Expr, Stmt};
use crate::error::CustomError;
use crate::error::InterpreterError;
use crate::error::Result;
use crate::error::RuntimeError;
use crate::error::ScannerError;
use crate::scanner::Literal;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::variable::{Env, Environment, RcObj};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter {
    environment: Env,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn run_interpretation(&mut self, mut statements: Vec<Stmt>) {
        for stmt in statements.iter_mut() {
            match stmt.evaluate(self.environment.clone()) {
                Ok(_) => (),
                Err(error) => println!("{}", error),
            }
        }
    }
}

pub fn interpret_literal(literal: &Literal) -> Result<RoxObject> {
    Ok(RoxObject::Literal(literal.clone()))
}

pub fn interpret_unary(operator: &Token, right: &Expr) -> Result<RoxObject> {
    let right_object = right.interpret()?;

    match operator.token_type {
        TokenType::MINUS => {
            let num = match right_object {
                RoxObject::Literal(literal) => match literal {
                    Literal::Number(n) => n,
                    _ => {
                        return Err(InterpreterError::new(
                            1,
                            "Unable to use unary operator on anything but numbers",
                            CustomError::RuntimeError(RuntimeError::TypeError),
                        ));
                    }
                },
            };
            return Ok(RoxObject::Literal(Literal::Number(-num)));
        }
        TokenType::BANG => {
            return Ok(RoxObject::Literal(Literal::Boolean(is_truthy(
                &right_object,
            ))));
        }
        _ => {
            return Err(InterpreterError::new(
                1,
                &format!(
                    "Unexpected character {:?} {:?}",
                    &operator.token_type, &right_object
                ),
                CustomError::ScannerError(ScannerError::UnexpectedChar),
            ));
        }
    }
}

pub fn interpret_binary(left: &Expr, operator: &Token, right: &Expr) -> Result<RoxObject> {
    let left_object = left.interpret()?;
    let right_object = right.interpret()?;

    match &operator.token_type {
        TokenType::MINUS => operate_on_number(&left_object, &right_object, "-"),
        TokenType::STAR => operate_on_number(&left_object, &right_object, "*"),
        TokenType::SLASH => operate_on_number(&left_object, &right_object, "/"),
        TokenType::PLUS => add_objects(&left_object, &right_object),
        TokenType::GREATER => operate_on_number(&left_object, &right_object, ">"),
        TokenType::GREATER_EQUAL => operate_on_number(&left_object, &right_object, ">="),
        TokenType::LESS => operate_on_number(&left_object, &right_object, "<"),
        TokenType::LESS_EQUAL => operate_on_number(&left_object, &right_object, "<="),
        TokenType::BANG_EQUAL => check_object_equality(&left_object, &right_object, true),
        TokenType::EQUAL_EQUAL => check_object_equality(&left_object, &right_object, false),
        TokenType::COMMA => Ok(right_object),
        _ => Err(InterpreterError::new(
            1,
            &format!("Invalid binary operator: {:?}", &operator.token_type),
            CustomError::ScannerError(ScannerError::UnexpectedChar),
        )),
    }
}

pub fn interpret_logical(left: &Expr, operator: &Token, right: &Expr) -> Result<RoxObject> {
    let left_object = left.interpret()?;

    match &operator.token_type {
        TokenType::OR => {
            if is_truthy(&left_object) {
                return Ok(left_object);
            }
        }
        TokenType::AND => {
            if !is_truthy(&left_object) {
                return Ok(left_object);
            }
        }
        _ => {
            return Err(InterpreterError::new(
                1,
                &format!("Invalid logical operator: {:?}", &operator.token_type),
                CustomError::ScannerError(ScannerError::UnexpectedChar),
            ));
        }
    };

    return right.interpret();
}

pub fn interpret_ternary(condition: &Expr, left: &Expr, right: &Expr) -> Result<RoxObject> {
    let condition = match condition.interpret()? {
        RoxObject::Literal(c) => match c {
            Literal::Boolean(b) => b,
            _ => {
                return Err(InterpreterError::new(
                    1,
                    &format!(
                        "{} is not a boolean. Only booleans are valid conditions for ternary operators",
                        &c
                    ),
                    CustomError::ScannerError(ScannerError::SyntaxError),
                ));
            }
        },
    };
    if condition {
        left.interpret()
    } else {
        right.interpret()
    }
}

fn check_object_equality(
    left: &RoxObject,
    right: &RoxObject,
    inequality: bool,
) -> Result<RoxObject> {
    let mut result = false;
    if left == right {
        result = true;
    }
    if inequality {
        result = !result;
    }

    Ok(RoxObject::Literal(Literal::Boolean(result)))
}

fn add_objects(left: &RoxObject, right: &RoxObject) -> Result<RoxObject> {
    match left {
        RoxObject::Literal(literal) => match literal {
            Literal::Number(n) => match right {
                RoxObject::Literal(right_literal) => match right_literal {
                    Literal::Number(right_n) => {
                        Ok(RoxObject::Literal(Literal::Number(n + right_n)))
                    }
                    _ => Err(InterpreterError::new(
                        1,
                        &format!(
                            "Operation: + is not usable on {} and {}",
                            literal, right_literal
                        ),
                        CustomError::RuntimeError(RuntimeError::TypeError),
                    )),
                },
            },
            Literal::String(s) => match right {
                RoxObject::Literal(right_literal) => match right_literal {
                    Literal::String(right_s) => {
                        Ok(RoxObject::Literal(Literal::String(s.clone() + &right_s)))
                    }
                    Literal::Number(right_n) => Ok(RoxObject::Literal(Literal::String(format!(
                        "{}{}",
                        s, right_n
                    )))),
                    _ => Err(InterpreterError::new(
                        1,
                        &format!(
                            "Operation: + is not usable on {} and {}",
                            literal, right_literal
                        ),
                        CustomError::RuntimeError(RuntimeError::TypeError),
                    )),
                },
            },
            _ => Err(InterpreterError::new(
                1,
                &format!("Operation: + is not allowed for {}", literal),
                CustomError::RuntimeError(RuntimeError::TypeError),
            )),
        },
    }
}

fn operate_on_number(left: &RoxObject, right: &RoxObject, operation: &str) -> Result<RoxObject> {
    let left_value = match left {
        RoxObject::Literal(literal) => match literal {
            Literal::Number(n) => *n,
            _ => {
                return Err(InterpreterError::new(
                    1,
                    &format!("Operation: {} is only allowed for numbers", operation),
                    CustomError::RuntimeError(RuntimeError::TypeError),
                ));
            }
        },
    };

    let right_value = match right {
        RoxObject::Literal(literal) => match literal {
            Literal::Number(n) => *n,
            _ => {
                return Err(InterpreterError::new(
                    1,
                    &format!("Operation: {} is only allowed for numbers", operation),
                    CustomError::RuntimeError(RuntimeError::TypeError),
                ));
            }
        },
    };

    match operation {
        "-" => Ok(RoxObject::Literal(Literal::Number(
            left_value - right_value,
        ))),
        "*" => Ok(RoxObject::Literal(Literal::Number(
            left_value * right_value,
        ))),
        "/" => Ok(RoxObject::Literal(Literal::Number(
            left_value / right_value,
        ))),
        "+" => Ok(RoxObject::Literal(Literal::Number(
            left_value + right_value,
        ))),
        ">" => Ok(RoxObject::Literal(Literal::Boolean(
            left_value > right_value,
        ))),
        "<" => Ok(RoxObject::Literal(Literal::Boolean(
            left_value < right_value,
        ))),
        ">=" => Ok(RoxObject::Literal(Literal::Boolean(
            left_value >= right_value,
        ))),
        "<=" => Ok(RoxObject::Literal(Literal::Boolean(
            left_value <= right_value,
        ))),
        _ => Err(InterpreterError::new(
            1,
            &format!("Operation: {} is not allowed for numbers", operation),
            CustomError::RuntimeError(RuntimeError::TypeError),
        )),
    }
}

pub fn is_truthy(obj: &RoxObject) -> bool {
    match obj {
        RoxObject::Literal(literal) => match literal {
            Literal::Boolean(b) => *b,
            Literal::Null => false,
            _ => true,
        },
    }
}

pub fn is_rox_obj_truthy(obj: RcObj) -> bool {
    match *obj.as_ref().borrow() {
        RoxObject::Literal(ref literal) => match literal {
            Literal::Boolean(b) => *b,
            Literal::Null => false,
            _ => true,
        },
        _ => true,
    }
}
