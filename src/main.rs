use std::env;
use std::fs;
use std::process::exit;
use std::io::prelude::*;
use std::io;

extern crate interpreter;

use interpreter::runner::Runner;

const EXIT: i32 = 64;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut runner = Runner::new();

    if args.len() > 2 {
        println!("Using jlox [script]");
        exit(EXIT);
    } else if args.len() == 2 {
        runner.run_file(args.get(0).unwrap());
    } else {
        runner.run_prompt();
    }

}
