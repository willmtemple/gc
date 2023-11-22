use std::{env::args, fs::read_to_string, path::PathBuf, sync::Arc};

use interpreter::{
    value2::{self},
    InterpreterHost,
};

const MANIFEST_DIR: &str = env!("CARGO_MANIFEST_DIR");

pub struct CliHost {}

impl InterpreterHost for CliHost {
    fn write_std(&mut self, target: interpreter::WriteTarget, s: &str) {
        match target {
            interpreter::WriteTarget::Out => {
                print!("{}", s);
            }
            interpreter::WriteTarget::Err => {
                eprint!("{}", s);
            }
        }
    }

    fn load_boot_module(&mut self) -> Option<Arc<std::path::Path>> {
        Some(Arc::from(
            PathBuf::new()
                .join(MANIFEST_DIR)
                .join("lib")
                .join("init.l")
                .as_path(),
        ))
    }

    fn resolve_module(&mut self, module_name: &str) -> Option<Arc<std::path::Path>> {
        let path = PathBuf::new()
            .join(MANIFEST_DIR)
            .join(".modules")
            .join(module_name)
            .with_extension("l");

        if path.exists() {
            Some(Arc::from(path.as_path()))
        } else {
            None
        }
    }

    fn read_file(&mut self, path: &std::path::Path) -> Option<Arc<str>> {
        read_to_string(path).ok().map(Arc::from)
    }
}

fn main() {
    let path = args().nth(1).expect("no path given");

    let mut interpreter = interpreter::Interpreter::new(Box::new(CliHost {}));

    let r = interpreter.read_and_eval_module(path).unwrap_or_else(|e| {
        eprintln!("{:?}", e);
        std::process::exit(1);
    });

    if !r.is::<()>() {
        println!(
            "{}",
            r.to_string(&mut interpreter)
                .expect_result()
                .unwrap()
                .downcast::<value2::String>()
                .unwrap()
                .value
        );
    }
}
