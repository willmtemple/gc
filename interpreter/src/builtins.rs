use std::{
    ops::Deref,
    path::{Path, PathBuf},
};

use crate::{
    ast::{Operator, OperatorPosition},
    value2::{self, Map, Nil, Sigil, Slice, Symbol, Value},
    Interpreter, InterpreterResult, WriteTarget,
};

pub fn get_builtins() -> Map {
    let mut m = Map::new();

    m = m.insert(
        Symbol::new("println").to_object(),
        (println as fn(&mut crate::Interpreter, Slice) -> InterpreterResult).to_object(),
    );

    m = m.insert(
        Symbol::new("bind_operator").to_object(),
        (bind_operator as fn(&mut Interpreter, Slice) -> InterpreterResult).to_object(),
    );

    m = m.insert(
        Symbol::new("eq").to_object(),
        (eq as fn(&mut Interpreter, Slice) -> InterpreterResult).to_object(),
    );

    m = m.insert(
        Symbol::new("load_module").to_object(),
        (load_module as fn(&mut Interpreter, Slice) -> InterpreterResult).to_object(),
    );

    m = m.insert(
        Symbol::new("add").to_object(),
        (add as fn(i64, i64) -> i64).to_object(),
    );

    m = m.insert(
        Symbol::new("sub").to_object(),
        (sub as fn(i64, i64) -> i64).to_object(),
    );

    m = m.insert(
        Symbol::new("neg").to_object(),
        (neg as fn(i64) -> i64).to_object(),
    );

    m = m.insert(
        Symbol::new("mul").to_object(),
        (mul as fn(i64, i64) -> i64).to_object(),
    );

    m = m.insert(
        Symbol::new("div").to_object(),
        (div as fn(i64, i64) -> i64).to_object(),
    );

    m = m.insert(
        Symbol::new("mod").to_object(),
        (modulo as fn(i64, i64) -> i64).to_object(),
    );

    m = m.insert(
        Symbol::new("pow").to_object(),
        (pow as fn(i64, i64) -> i64).to_object(),
    );

    m
}

pub fn println(interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
    {
        let mut init = false;
        for argument in &args {
            if init {
                interpreter.host.write_std(WriteTarget::Out, " ");
            } else {
                init = true;
            }

            let s = argument.to_string(interpreter)?;

            let s = s.to_string(interpreter)?;

            interpreter.host.write_std(
                WriteTarget::Out,
                &s.downcast::<value2::String>().unwrap().value,
            );
        }

        interpreter.host.write_std(WriteTarget::Out, "\n");

        InterpreterResult::Value(Nil.to_object())
    }
}

fn bind_operator(interpreter: &mut Interpreter, arguments: Slice) -> InterpreterResult {
    let Some(f) = arguments.get(0) else {
        panic!("First argument to bind_operator should be an object.")
    };

    let Some(op) = arguments.get(1).and_then(|v| v.downcast::<Sigil>()) else {
        panic!("Second argument to bind_operator should be a sigil.")
    };

    let Some(precedence) = arguments.get(2).and_then(|v| v.downcast::<i64>()) else {
        panic!("Third argument to bind_operator should be a number.")
    };

    let Some(position) = arguments
        .get(3)
        .and_then(|v| v.downcast::<value2::String>())
        .map(|v| v.value.deref())
    else {
        panic!("Fourth argument to bind_operator should be a string.")
    };

    let position = match position {
        ":prefix" => OperatorPosition::Prefix,
        ":infix" => OperatorPosition::Infix,
        ":postfix" => OperatorPosition::Postfix,
        _ => panic!("Invalid operator position: {}", position),
    };

    interpreter.operators = interpreter.operators.insert(
        (*op, position),
        Operator {
            position,
            precedence: *precedence,
            value: f.clone(),
        },
    );

    InterpreterResult::Value(Nil.to_object())
}

pub fn eq(_: &mut Interpreter, args: Slice) -> InterpreterResult {
    let a = args.get(0).cloned().unwrap_or(Nil.to_object());
    let b = args.get(1).cloned().unwrap_or(Nil.to_object());

    InterpreterResult::Value((a == b).to_object())
}

pub fn load_module(interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
    let Some(path) = args
        .get(0)
        .and_then(|v| v.downcast::<value2::String>())
        .map(|v| v.value.deref())
    else {
        panic!("First argument to load_module should be a string.")
    };

    // We have a few cases. This works like Node.js module resolution.
    // If the path doesn't start with `.` or `/`, we try to resolve it as a module using the host.
    // If the path is absolute, we just load it from the host.
    // If the path is definitely relative (i.e. starts with `.`), we load it relative to the current module's dirname.

    let is_path = path.starts_with('.') || path.starts_with('/');

    let final_path = if is_path {
        let path = Path::new(path);

        if path.is_absolute() {
            PathBuf::from(path)
        } else {
            let module = interpreter.get_current_module();

            module
                .parent()
                .expect("fatal error: module has no parent")
                .join(path)
        }
    } else {
        let Some(module) = interpreter.host.resolve_module(path) else {
            panic!("failed to resolve module '{}'", path);
        };

        PathBuf::from(module.deref())
    };

    InterpreterResult::from(interpreter.read_and_eval_module(final_path))
}

fn add(l: i64, r: i64) -> i64 {
    l + r
}

fn sub(l: i64, r: i64) -> i64 {
    l - r
}

fn neg(l: i64) -> i64 {
    -l
}

fn mul(l: i64, r: i64) -> i64 {
    l * r
}

fn div(l: i64, r: i64) -> i64 {
    l / r
}

fn modulo(l: i64, r: i64) -> i64 {
    l % r
}

fn pow(l: i64, r: i64) -> i64 {
    l.pow(r as u32)
}
