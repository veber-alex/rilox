use std::path::Path;
use std::process::{self, Command, Stdio};
use std::{env, fs, io};

macro_rules! red {
    ($e:expr) => {
        format_args!("\u{001b}[31m{}\u{001b}[0m", $e)
    };
}

macro_rules! green {
    ($e:expr) => {
        format_args!("\u{001b}[32m{}\u{001b}[0m", $e)
    };
}

macro_rules! yellow {
    ($e:expr) => {
        format_args!("\u{001b}[33m{}\u{001b}[0m", $e)
    };
}

#[derive(Debug)]
enum TestError {
    Fail,
    Skip,
    IoError(io::Error),
}

impl From<io::Error> for TestError {
    fn from(e: io::Error) -> Self {
        TestError::IoError(e)
    }
}

struct TestsResult {
    passed: u32,
    failed: u32,
    skipped: u32,
}

impl TestsResult {
    fn new(passed: u32, failed: u32, skipped: u32) -> Self {
        Self {
            passed,
            failed,
            skipped,
        }
    }
}

type Result<T, E = TestError> = std::result::Result<T, E>;

const ROOT: &str = "tests/";

fn main() -> Result<()> {
    let mut args = env::args().skip(1);
    if args.len() > 1 {
        eprintln!("Usage: cargo test --test rilox [-- {{path_to_test/test_name.rilox}}]");
        process::exit(1)
    }

    let path;
    let tests_path = if let Some(p) = args.next() {
        path = p;
        Path::new(&path)
    } else {
        Path::new(ROOT)
    };

    let mut result = TestsResult::new(0, 0, 0);
    if tests_path.is_dir() {
        visit_dirs(tests_path, &mut result)?;
    } else {
        visit_file(tests_path, &mut result)?;
    }

    let TestsResult {
        passed,
        failed,
        skipped,
    } = result;

    println!();
    let exit_code = if failed > 0 {
        print!("test result: {}. ", red!("failed"));
        1
    } else {
        print!("test result: {}. ", green!("ok"));
        0
    };
    println!("{} passed; {} failed; {} skipped;", passed, failed, skipped);

    process::exit(exit_code);
}

fn visit_dirs(dir: &Path, result: &mut TestsResult) -> Result<()> {
    let mut entries = fs::read_dir(dir)?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_unstable_by_key(|e| e.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            visit_dirs(&path, result)?;
        } else {
            visit_file(&path, result)?;
        };
    }

    Ok(())
}

fn visit_file(file: &Path, result: &mut TestsResult) -> Result<()> {
    let to_skip = file.file_stem().unwrap().to_string_lossy().starts_with('_');
    match run_test(file) {
        Err(TestError::Fail) => result.failed += 1,
        Err(TestError::Skip) if to_skip => {
            result.passed += 1;
            println!("test {} ... {}", file.display(), green!("ok(skipped)"));
        }
        Err(TestError::Skip) => {
            result.skipped += 1;
            println!("test {} ... {}", file.display(), yellow!("skipped"));
        }
        Err(err @ TestError::IoError(_)) => return Err(err),
        Ok(_) => {
            result.passed += 1;
            println!("test {} ... {}", file.display(), green!("ok"));
        }
    }

    Ok(())
}

fn run_test(file: &Path) -> Result<()> {
    let output = Command::new(env!("CARGO_BIN_EXE_rilox"))
        .arg(&file)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let mut stdout_iter = stdout.trim().lines();
    let mut stderr_iter = stderr.trim().lines();

    let test_data = fs::read_to_string(&file)?;
    let mut test_ran = false;
    for (n, line) in test_data.lines().enumerate() {
        let n = n + 1;

        // expect
        if let Some((_, expected)) = line.split_once("// expect: ") {
            test_ran = true;
            handle_expect_case(file, n, expected, stdout_iter.next())?;
        }

        // expect error
        if let Some((_, expected)) = line.split_once("// expect error: ") {
            test_ran = true;
            handle_expect_case(file, n, expected, stderr_iter.next())?;
        }
    }

    if !test_ran {
        Err(TestError::Skip)
    } else {
        Ok(())
    }
}

fn handle_expect_case(
    test_path: &Path,
    line: usize,
    expected: &str,
    got: Option<&str>,
) -> Result<()> {
    match got {
        None | Some(_) if got != Some(expected) => {
            eprintln!("test {} ... {}", test_path.display(), red!("failed"));
            eprintln!("    at line {}", line);
            eprintln!("    expected: `{}`", expected);
            if let Some(got) = got {
                eprintln!("    got:      `{}`", got);
            } else {
                eprintln!("    got:       no output");
            }
            Err(TestError::Fail)
        }
        _ => Ok(()),
    }
}
