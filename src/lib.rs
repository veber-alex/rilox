#![warn(rust_2018_idioms)]

use context::Context;
use interpreter::Interpreter;
use model::builtins::install_builtins;
use parser::Parser;
use resolver::Resolver;
use scanner::Scanner;
use std::fmt::Debug;
// use std::io::BufRead;
use std::{io, process};
use stmt::Stmt;

mod arena;
mod context;
mod environment;
mod expr;
mod interpreter;
mod model;
mod parser;
pub mod peek2;
mod resolver;
mod scanner;
mod stmt;
mod token;

#[derive(Debug, Default)]
pub struct Rilox {
    ctx: Context,
}

impl Rilox {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn run_file(&mut self, path: &str) -> Result<(), io::Error> {
        let source = std::fs::read_to_string(path)?;

        if let Ok((mut interpreter, stmts)) = self.prepare_run(&source) {
            if interpreter.interpret(&stmts).is_none() {
                process::exit(66)
            }
            Ok(())
        } else {
            process::exit(65)
        }
    }

    pub fn run_prompt(&mut self) -> Result<(), io::Error> {
        todo!()
        // let mut buffer = String::new();
        // let stdin = io::stdin();
        // let mut handle = stdin.lock();

        // while handle.read_line(&mut buffer)? != 0 {
        //     // Ignore errors in REPL
        //     if let Ok((mut interpreter, stmts)) = self.prepare_run(buffer.trim()) {
        //         interpreter.interpret(&stmts);
        //     }
        //     buffer.clear();
        // }

        // Ok(())
    }

    fn prepare_run(&mut self, source: &str) -> Result<(Interpreter<'_>, Vec<Stmt>), ()> {
        // Scanning
        let scanner = Scanner::new(source);
        let (tokens, had_scan_error) = scanner.scan_tokens();

        // FIXME: REMOVE THIS
        if had_scan_error {
            return Err(());
        }

        // Parsing
        let mut parser = Parser::new(tokens, &mut self.ctx);
        let stmts = parser.parse().ok_or(())?;

        // Resolving
        // FIXME: This is ugly
        let mut interpreter = Interpreter::new(&self.ctx);
        let mut resolver = Resolver::new(&self.ctx);
        install_builtins(&mut interpreter, &mut resolver);
        resolver.resolve(&*stmts);
        if resolver.had_error {
            process::exit(64)
        }

        Ok((interpreter, stmts))
    }
}

pub fn report_error(etype: &str, line: usize, loc: &str, message: &str) {
    eprintln!("[line {}] {} Error{}: {}", line, etype, loc, message);
}
