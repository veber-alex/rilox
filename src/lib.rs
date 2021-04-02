use std::any::Any;
use std::fmt::{Debug, Display};
use std::io::BufRead;
use std::{io, process};

use ast_printer::AstPrinter;
use parser::Parser;
use scanner::Scanner;

pub mod ast_printer;
pub mod expr;
pub mod parser;
pub mod peek2;
pub mod scanner;
pub mod token;

pub trait LoxType: Debug + Display + Any {}
impl<T: Debug + Display + Any> LoxType for T {}
pub type LoxObject = Box<dyn LoxType>;

#[derive(Debug, Default)]
pub struct Rilox {
    had_error: bool,
}

impl Rilox {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn run_file(&mut self, path: &str) -> Result<(), io::Error> {
        let source = std::fs::read_to_string(path)?;
        self.run(&source);

        if self.had_error {
            process::exit(65)
        }

        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<(), io::Error> {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();

        while handle.read_line(&mut buffer)? != 0 {
            self.run(buffer.trim());
            self.had_error = false;
            buffer.clear();
        }

        Ok(())
    }

    fn run(&mut self, source: &str) {
        // Scanning
        let scanner = Scanner::new(source);
        let (tokens, had_scan_error) = scanner.scan_tokens();

        // for token in &tokens {
        //     eprintln!("{:?}", token);
        // }

        if had_scan_error {
            return;
        }

        // Parsing
        let mut parser = Parser::new(tokens);
        if let Some(expression) = parser.parse() {
            eprintln!("{}", AstPrinter.print(&expression));
        }
    }
}

pub fn report_error(line: usize, loc: &str, message: &str) {
    eprintln!("[line {}] Error{}: {}", line, loc, message);
}
