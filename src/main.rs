use rilox::Rilox;
use std::{env, io, process};

fn main() -> Result<(), io::Error> {
    let mut args = env::args().skip(1);
    if args.len() > 1 {
        eprintln!("Usage: rilox [script]");
        process::exit(64)
    }

    let mut rilox = Rilox::new();

    if let Some(path) = args.next() {
        rilox.run_file(&path)
    } else {
        rilox.run_prompt()
    }
}
