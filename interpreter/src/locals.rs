use std::cell::RefCell;

use hamt::{vec::HamtVecSlice, HamtMap};

use crate::{value::Value, value2::Nil, Interpreter};

use crate::value2::Value as Value2;

thread_local! {
    pub static LOCALS: RefCell<Locals> = RefCell::new(Locals::new());
}

pub struct Locals {
    cursor: usize,
    data: HamtMap<usize, Value>,
}

impl Locals {
    fn new() -> Self {
        Self {
            cursor: 0,
            data: HamtMap::new(),
        }
    }

    fn get(&self, index: usize) -> Option<&Value> {
        self.data.get(&index)
    }

    fn set(&mut self, index: usize, value: Value) {
        self.data = self.data.insert(index, value);
    }
}

pub fn def_local(_: &mut Interpreter, args: HamtVecSlice<Value>) -> Value {
    let value = args
        .get(0)
        .expect("expected exactly one argument in call to def_local");

    LOCALS.with(|locals| {
        let mut locals = locals.borrow_mut();

        let index = locals.cursor;

        locals.cursor += 1;

        locals.data = locals.data.insert(index, value.clone());

        (index as i64).to_value1()
    })
}

pub fn get_local(_: &mut Interpreter, args: HamtVecSlice<Value>) -> Value {
    let index = args
        .get(0)
        .expect("expected exactly one argument in call to get_local")
        .downcast::<i64>();

    LOCALS.with(|locals| {
        let locals = locals.borrow();

        let index = match index {
            Some(n) => *n as usize,
            _ => panic!("expected numeric index in call to get_local"),
        };

        locals
            .get(index)
            .cloned()
            .unwrap_or_else(|| panic!("no local with index {}", index))
    })
}

pub fn set_local(_: &mut Interpreter, args: HamtVecSlice<Value>) -> Value {
    let index = args
        .get(0)
        .expect("expected exactly one argument in call to set_local")
        .downcast::<i64>();

    let value = args
        .get(1)
        .expect("expected exactly two arguments in call to set_local");

    LOCALS.with(|locals| {
        let mut locals = locals.borrow_mut();

        let index = match index {
            Some(n) => *n as usize,
            _ => panic!("expected numeric index in call to set_local"),
        };

        locals.set(index, value.clone());

        Nil.to_value1()
    })
}
