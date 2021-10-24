use std::ffi::OsStr;
use std::path::Path;
use std::process::{self, Command, Stdio};
use std::{env, fs, io};

type Result<T, E = io::Error> = std::result::Result<T, E>;

const ROOT: &str = "benches/";

fn main() -> Result<()> {
    let mut args = env::args().skip(1);
    if args.len() > 2 {
        eprintln!("Usage: cargo bench --bench rilox [-- {{path_to_test/test_name.rilox}}]");
        process::exit(1)
    }

    let path;
    let arg = args.next();
    let tests_path = match arg.as_deref() {
        Some("--bench") | None => Path::new(ROOT),
        Some(p) => {
            path = p;
            Path::new(&path)
        }
    };

    let mut sum_time = 0;

    if tests_path.is_dir() {
        visit_dirs(tests_path, &mut sum_time)?;
    } else {
        visit_file(tests_path, &mut sum_time)?;
    }

    println!("{:<40}: {}ms", "Total time", sum_time);

    Ok(())
}

fn visit_dirs(dir: &Path, sum: &mut u32) -> Result<()> {
    let mut entries = fs::read_dir(dir)?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_unstable_by_key(|e| e.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            visit_dirs(&path, sum)?;
        } else {
            visit_file(&path, sum)?;
        };
    }

    Ok(())
}

fn visit_file(file: &Path, sum: &mut u32) -> Result<()> {
    if file.extension() == Some(OsStr::new("lox")) {
        let output = Command::new(env!("CARGO_BIN_EXE_rilox"))
            .arg(&file)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()?;
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        if let Some(error) = stderr.trim().lines().next() {
            return Err(io::Error::new(io::ErrorKind::Other, error));
        }

        let time_ms = stdout
            .trim()
            .lines()
            .last()
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Benchmark returned no output"))?
            .parse::<u32>()
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "Benchmark returned not a number"))?;

        *sum += time_ms;

        println!("{:<40}: {}ms", file.display(), time_ms);
    }

    Ok(())
}
