use std::cell::RefCell;
use std::sync::Arc;

use hamt::HamtMap;

use crate::Interpreter;

use crate::value2::{Object, Slice, Value};

thread_local! {
    pub static LOCALS: RefCell<Locals> = RefCell::new(Locals::new());
}

pub struct Locals {
    cursor: usize,
    data: HamtMap<usize, Arc<Object>>,
}

impl Locals {
    fn new() -> Self {
        Self {
            cursor: 0,
            data: HamtMap::new(),
        }
    }

    fn get(&self, index: usize) -> Option<&Arc<Object>> {
        self.data.get(&index)
    }

    fn set(&mut self, index: usize, value: Arc<Object>) {
        self.data = self.data.insert(index, value);
    }
}

pub fn def_local(_: &mut Interpreter, args: Slice) -> Arc<Object> {
    let value = args
        .get(0)
        .expect("expected exactly one argument in call to def_local");

    LOCALS.with(|locals| {
        let mut locals = locals.borrow_mut();

        let index = locals.cursor;

        locals.cursor += 1;

        locals.data = locals.data.insert(index, value.clone());

        (index as i64).to_object()
    })
}

pub fn get_local(_: &mut Interpreter, args: Slice) -> Arc<Object> {
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

pub fn set_local(_: &mut Interpreter, args: Slice) -> Arc<Object> {
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

        ().to_object()
    })
}
