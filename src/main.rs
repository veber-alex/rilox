use scanner::Scanner;
use std::io::{self, BufRead};
use std::{env, process};

mod scanner;
mod token;

fn main() -> Result<(), io::Error> {
    let mut args = env::args().skip(1);
    if args.len() > 1 {
        eprintln!("Usage: rilox [script]");
        process::exit(64)
    }

    let mut rilox = Rilox::new();

    if let Some(path) = args.next() {
        rilox.run_file(&path)?;
    } else {
        rilox.run_prompt()?;
    }

    Ok(())
}

#[derive(Debug, Default)]
pub struct Rilox {
    had_error: bool,
}

impl Rilox {
    fn new() -> Self {
        Default::default()
    }

    fn run_file(&mut self, path: &str) -> Result<(), io::Error> {
        let source = std::fs::read_to_string(path)?;
        self.run(&source);

        if self.had_error {
            process::exit(65)
        }

        Ok(())
    }

    fn run_prompt(&mut self) -> Result<(), io::Error> {
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
        let scanner = Scanner::new(source, self);
        let tokens = scanner.scan_tokens();

        for token in tokens {
            dbg!(token);
        }
    }

    fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: usize, loc: &str, message: &str) {
        eprintln!("[line {}] Error {}: {}", line, loc, message);
        self.had_error = true;
    }
}
