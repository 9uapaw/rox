use std::error;
use std::result;
use std::fmt;
use std::fmt::Formatter;
use std::iter::repeat;
use crate::scanner::Token;
use crate::variable::RcObj;

pub type Result<T> = result::Result<T, InterpreterError>;

#[derive(Debug)]
pub struct InterpreterError {
    line: usize,
    message: String,
    pub error: CustomError,
    snippet: String
}

impl InterpreterError {
    pub fn new(line: usize, message: &str, error: CustomError) -> InterpreterError {
        InterpreterError {
            line,
            message: String::from(message),
            error,
            snippet: String::from("")
        }
    }

    pub fn new_with_location(line: usize, message: &str, error: CustomError, location: &[Token]) -> InterpreterError {
        InterpreterError {
            line,
            message: String::from(message),
            error,
            snippet: location.iter().map(|s| {s.lexem.clone()}).collect::<Vec<String>>().join(" ")
        }
    }

    pub fn throw(&self) {
        let underline: String = repeat("^").take(self.snippet.len()).collect();
        println!("{}  |   {}", self.line, self.snippet);
        println!("       {} error: {}", underline, self.message);
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?} on line: {}. Cause: {}", self.error, self.line, self.message)
    }
}

impl error::Error for InterpreterError {
}

#[derive(Debug)]
pub enum CustomError {
    ParserError(ParserError),
    ScannerError(ScannerError),
    RuntimeError(RuntimeError),
    Return(Option<RcObj>),
    UnknownError
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
    SyntaxError
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeError,
    NotFound,
    WrongArguments
}

pub fn error(line: usize, message: &str, error_type: &CustomError) {
    report(line, "", message, error_type);
}

fn report(line: usize, location: &str, message: &str, error_type: &CustomError) {
    println!("[line {}] {:?} {}: {}", line, error_type, location, message);
}
