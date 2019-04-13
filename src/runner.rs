use std::fs;
use std::io;
use std::io::prelude::*;
use crate::scanner::Scanner;
use crate::scanner::Token;
use crate::error::error;

pub struct Runner {
    error_occured: bool
}

impl Runner {

    pub fn new() -> Runner {
        Runner{error_occured: false}
    }

    pub fn run_file(&mut self, path: &str) {
        let content =
            String::from_utf8_lossy(&fs::read(path).expect(&format!("Path not found: {}", path)))
                .parse()
                .expect("Unable to parse source file");

        match self.run(content) {
            Ok(_) => println!("Successful run"),
            Err(error) => panic!(format!("Error occured: {}", error))
        }
    }

    pub fn run_prompt(&mut self) {
        let stdin = io::stdin();

        loop {
            let mut buffer = String::new();
            print!("> ");
            io::stdout().flush().unwrap();
            stdin.read_line(&mut buffer).expect("Unexpected error on reading input");
            println!("BUFFER: {}", buffer);
            self.run(buffer);
            self.error_occured = false;
        }
    }

    fn run(&mut self, source: String) -> Result<(), String>{
        if self.error_occured {
            error(1, "ERROR");
            return Err(String::from("ERROR"));
        }

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        for token in tokens {
            println!("{:?}", token);
        }
        Ok(())
    }
}