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
        error(self.line, &self.message, &self.error);
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
    ScannerError(ScannerError)
}

#[derive(Debug)]
pub enum ParserError {
    InvalidStatement,
    UnterminatedToken
}

#[derive(Debug)]
pub enum ScannerError {
    UnexpectedChar,
    InvalidNumber,
}

pub fn error(line: usize, message: &str, error_type: &CustomError) {
    report(line, "", message, error_type);
}

fn report(line: usize, location: &str, message: &str, error_type: &CustomError) {
    println!("[line {}] {:?} {}: {}", line, error_type, location, message);
}
