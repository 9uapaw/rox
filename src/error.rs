use std::error;
use std::result;
use std::fmt;
use std::fmt::Formatter;

pub type Result<T> = result::Result<T, InterpreterError>;

#[derive(Debug)]
pub struct InterpreterError {
    line: usize,
    message: String,
    error: CustomError,
}

impl InterpreterError {
    pub fn new(line: usize, message: &str, error: CustomError) -> InterpreterError {
        InterpreterError {
            line,
            message: String::from(message),
            error,
        }
    }

    pub fn throw(&self) {
        error(self.line, &self.message);
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Error on line: {}. Cause: {}", self.line, self.message)
    }
}

impl error::Error for InterpreterError {
}

#[derive(Debug)]
pub enum CustomError {
    ParserError(ParserError),
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedChar,
    InvalidNumber,
}

pub fn error(line: usize, message: &str) {
    report(line, "", message);
}

fn report(line: usize, location: &str, message: &str) {
    println!("[line {}] Error {}: {}", line, location, message);
}
