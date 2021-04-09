use std::fmt::Debug;
use std::io::BufRead;

use std::{io, process};

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
use stmt::Stmt;

mod enviroment;
mod expr;
mod interpreter;
mod object;
mod parser;
mod peek2;
mod scanner;
mod stmt;
mod token;

#[derive(Debug, Default)]
pub struct Rilox;

impl Rilox {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn run_file(&mut self, path: &str) -> Result<(), io::Error> {
        let source = std::fs::read_to_string(path)?;

        if let Ok(stmts) = self.prepare_run(&source) {
            let mut interpreter = Interpreter::new();
            if interpreter.interpret(&stmts).is_none() {
                process::exit(66)
            }
            Ok(())
        } else {
            process::exit(65)
        }
    }

    pub fn run_prompt(&mut self) -> Result<(), io::Error> {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();

        let mut interpreter = Interpreter::new();

        while handle.read_line(&mut buffer)? != 0 {
            // Ignore errors in REPL
            if let Ok(stmts) = self.prepare_run(buffer.trim()) {
                interpreter.interpret(&stmts);
            }
            buffer.clear();
        }

        Ok(())
    }

    fn prepare_run(&mut self, source: &str) -> Result<Vec<Stmt>, ()> {
        // Scanning
        let scanner = Scanner::new(source);
        let (tokens, had_scan_error) = scanner.scan_tokens();

        // FIXME: REMOVE THIS
        if had_scan_error {
            return Err(());
        }

        // Parsing
        let mut parser = Parser::new(tokens);
        parser.parse().ok_or(())
    }
}

pub fn report_error(etype: &str, line: usize, loc: &str, message: &str) {
    eprintln!("[line {}] {} Error{}: {}", line, etype, loc, message);
}
