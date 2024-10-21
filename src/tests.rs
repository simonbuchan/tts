use std::fs;
use std::io;
use std::path::Path;

use super::*;

// fixme: test per input
// will need to move output to test-files/output/{name}/{tree|errors}
#[test]
fn baseline() -> io::Result<()> {
    let test_dir = Path::new("test-files");
    let input_dir = test_dir.join("input");
    let expected_dir = test_dir.join("expected");
    let output_dir = test_dir.join("output");
    fs::create_dir_all(&expected_dir)?;
    fs::remove_dir_all(&output_dir)?;
    fs::create_dir_all(&output_dir)?;

    for entry in input_dir.read_dir()? {
        let input_path = entry?.path();
        let name = input_path
            .file_stem()
            .expect("missing file name?")
            .to_str()
            .expect("not unicode file name?");
        println!("testing {name}...");
        let source = fs::read_to_string(&input_path)?;
        let source = Source::new(name.to_string(), source);
        let result = match std::panic::catch_unwind(|| compile(&source)) {
            Err(panic) => {
                // panic!() may have a payload of either &str or String
                let output_path = output_dir.join(format!("{name}.panic"));
                if let Some(str) = panic.downcast_ref::<&str>() {
                    fs::write(output_path, str)?;
                } else if let Some(str) = panic.downcast_ref::<String>() {
                    fs::write(output_path, str)?;
                } else {
                    std::panic::panic_any(panic);
                }
                continue;
            }
            Ok(result) => result,
        };

        fs::write(
            output_dir.join(format!("{name}.tree")),
            format!(
                "{}",
                syntax::TreeWriter::new(&result.syntax, result.module.0)
            ),
        )?;

        if !result.reports.is_empty() {
            let output_path = output_dir.join(format!("{name}.errors"));
            let mut output = String::new();
            let mut f = diag::ReportFormatter::new(&mut output, &source);
            for report in &result.reports {
                f.write(report).unwrap();
            }
            fs::write(output_path, output)?;
        }
    }

    let actual_names = read_file_names(&output_dir)?;
    let expected_names = read_file_names(&expected_dir)?;

    let (equal_names, added_names): (Vec<_>, _) = actual_names
        .iter()
        .partition(|name| expected_names.contains(name));
    let removed_names = expected_names
        .iter()
        .filter(|name| !actual_names.contains(name))
        .collect::<Vec<_>>();

    let mut result = Ok(());

    for name in equal_names {
        let actual = fs::read_to_string(output_dir.join(name))?;
        let expected = fs::read_to_string(expected_dir.join(name))?;
        if actual != expected {
            println!("changes in output {}:", name.to_string_lossy());
            for (line, diff) in diff::lines(&expected, &actual).iter().enumerate() {
                match diff {
                    diff::Result::Left(l) => {
                        println!("{line:>2}: -{l}");
                    }
                    diff::Result::Both(l, _) => {
                        println!("{line:>2}:  {l}");
                    }
                    diff::Result::Right(r) => {
                        println!("{line:>2}: +{r}");
                    }
                }
            }
            result = result.and(Err("changed output"));
        }
    }

    for name in added_names {
        println!("added output: {}", name.to_string_lossy());
        result = result.and(Err("added output"));
    }

    for name in removed_names {
        println!("removed output: {}", name.to_string_lossy());
        result = result.and(Err("removed output"));
    }

    result.map_err(io::Error::other)
}

fn read_file_names(path: &Path) -> io::Result<Vec<std::ffi::OsString>> {
    path.read_dir()?
        .map(|entry| entry.map(|entry| entry.file_name()))
        .collect()
}
