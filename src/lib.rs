use std::fmt::Debug;
use std::io::BufRead;

use std::{io, process};

use ast_printer::AstPrinter;
use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

pub mod ast_printer;
pub mod expr;
pub mod interpreter;
pub mod object;
pub mod parser;
pub mod peek2;
pub mod scanner;
pub mod stmt;
pub mod token;

#[derive(Debug, Default)]
pub struct Rilox;

impl Rilox {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn run_file(&mut self, path: &str) -> Result<(), io::Error> {
        let source = std::fs::read_to_string(path)?;

        if self.run(&source).is_err() {
            process::exit(65)
        }

        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<(), io::Error> {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();

        while handle.read_line(&mut buffer)? != 0 {
            // Ignore errors in REPL
            let _ = self.run(buffer.trim());
            buffer.clear();
        }

        Ok(())
    }

    fn run(&mut self, source: &str) -> Result<(), ()> {
        // Scanning
        let scanner = Scanner::new(source);
        let (tokens, had_scan_error) = scanner.scan_tokens();

        // for token in &tokens {
        //     eprintln!("{:?}", token);
        // }

        if had_scan_error {
            return Err(());
        }

        // Parsing
        let mut parser = Parser::new(tokens);
        if let Some(expression) = parser.parse() {
            eprintln!("{}", AstPrinter.print(&expression));

            // Interpreting
            let mut interpreter = Interpreter::new();
            if let Some(result) = interpreter.interpret(&expression) {
                println!("{}", result);
                return Ok(());
            }
        }

        Err(())
    }
}

pub fn report_error(line: usize, loc: &str, message: &str) {
    eprintln!("[line {}] Error{}: {}", line, loc, message);
}
