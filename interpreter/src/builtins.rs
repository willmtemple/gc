use std::{
    ops::Deref,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    ast::{Operator, OperatorPosition},
    value::{self, Map, NativeFn, Sigil, Slice, Symbol, Type, Value},
    Interpreter, InterpreterResult, WriteTarget,
};

macro_rules! map_line {
    (@$v:ident; $s:ident : $e:expr) => {
        $v = $v.insert(
            $crate::value::Symbol::new(stringify!($s)).to_object(),
            $crate::value::Value::to_object($e),
        );
    };
}

macro_rules! map_lines {
    (@$v:ident; $s:ident : $e:expr $(, $($rest:tt)*)?) => {
        map_line!(@$v; $s : $e);

        map_lines!(@$v; $($($rest)*)?)
    };
    (@$v:ident; $s:ident as $t:ty $(, $($rest:tt)*)?) => {
        map_line!(@$v; $s: $s as $t);

        map_lines!(@$v; $($($rest)*)?)
    };
    (@$v:ident; $s:ident $(, $($rest:tt)*)?) => {
        map_line!(@$v; $s: $s);

        map_lines!(@$v; $($($rest)*)?)
    };
    (@$v:ident;) => {}
}

macro_rules! map {
    ($($tokens:tt)*) => {
        {
            let mut value = $crate::value::Map::new();

            map_lines!(@value; $($tokens)*);

            value
        }
    }
}

pub fn get_builtins() -> Map {
    let types = get_type_map();

    let mut m = map! {
        types,

        println as NativeFn,
        bind_operator as NativeFn,
        eq as NativeFn,
        load_module as NativeFn,
        type_of as NativeFn,

        add as fn(i64, i64) -> i64,
        sub as fn(i64, i64) -> i64,
        neg as fn(i64) -> i64,
        mul as fn(i64, i64) -> i64,
        div as fn(i64, i64) -> i64,
        mod: modulo as fn(i64, i64) -> i64,
        pow as fn(i64, i64) -> i64,
    };

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

            interpreter
                .host
                .write_std(WriteTarget::Out, s.downcast::<Arc<str>>().unwrap());
        }

        interpreter.host.write_std(WriteTarget::Out, "\n");

        InterpreterResult::Value(().to_object())
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
        .and_then(|v| v.downcast::<Arc<str>>())
        .map(|v| v.deref())
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

    InterpreterResult::Value(().to_object())
}

pub fn eq(_: &mut Interpreter, args: Slice) -> InterpreterResult {
    let a = args.get(0).cloned().unwrap_or(().to_object());
    let b = args.get(1).cloned().unwrap_or(().to_object());

    InterpreterResult::Value((a == b).to_object())
}

pub fn load_module(interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
    let Some(path) = args
        .get(0)
        .and_then(|v| v.downcast::<Arc<str>>())
        .map(|v| v.deref())
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

fn type_of(interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
    let Some(value) = args.get(0) else {
        panic!("First argument to type_of should be a value.")
    };

    let s = value.get_type().to_object();

    InterpreterResult::Value(s)
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

macro_rules! type_map {
    ($($name:ident => $ty:ty),* $(,)?) => {
        let mut m = super::Map::new();

        $(
            m = m.insert(
                Symbol::new(stringify!($name)).to_object(),
                Type::of::<$ty>().to_object(),
            );
        )*

        m
    };
}

fn get_type_map() -> value::Map {
    type_map! {
        block => value::Block,
        function => value::Function,

        map => value::Map,
        vec => value::Vec,
        set => value::Set,

        i64 => i64,
        f64 => f64,
        bool => bool,
        nil => (),

        string => value::String,
        symbol => value::Symbol,
        sigil => value::Sigil,
        type => value::Type,
    }
}
