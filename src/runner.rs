use crate::error::error;
use crate::error::Result;
use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::scanner::Token;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::Path;
use crate::resolve::Resolver;

pub struct Runner {
    error_occured: bool,
    interpreter: Interpreter
}

impl Runner {
    pub fn new() -> Runner {
        Runner {
            error_occured: false,
            interpreter: Interpreter::new()
        }
    }

    pub fn run_file(&mut self, path: &str) {
        let content =
            String::from_utf8_lossy(&fs::read(Path::new(path)).expect(&format!("Path not found: {}", path)))
                .parse()
                .expect("Unable to parse source file");

        match self.run(content) {
            Ok(_) => println!("Successful run"),
            Err(error) => eprintln!("{}", format!("Error occured: {}", error)),
        }
    }

    pub fn run_prompt(&mut self) {
        let stdin = io::stdin();

        loop {
            let mut buffer = String::new();
            print!("> ");
            io::stdout().flush().unwrap();
            stdin
                .read_line(&mut buffer)
                .expect("Unexpected error on reading input");
            self.run(buffer);
            self.error_occured = false;
        }
    }

    fn run(&mut self, source: String) -> Result<()> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);
        let mut statements = parser.parse()?;
        let mut resolver = Resolver::new(&mut self.interpreter);

        resolver.resolve(&mut statements)?;

        self.interpreter.run_interpretation(statements)
    }
}
