use crate::{Interpreter, InterpreterResult};

use super::Value;

pub struct Nil;

impl Value for Nil {
    const NAME: &'static str = "nil";

    fn egal(&self, _: &Self) -> bool {
        true
    }

    fn to_string(&self, _: &mut Interpreter) -> InterpreterResult {
        InterpreterResult::Value(super::String::from("nil").to_object())
    }
}
