#[macro_use]
extern crate lazy_static;

pub mod scanner;
pub mod runner;
pub mod error;
pub mod ast;
pub mod parser;
pub mod interpreter;
pub mod variable;
pub mod config;
pub mod obj;
pub mod resolve;