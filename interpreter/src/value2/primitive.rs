use crate::{Interpreter, InterpreterResult};

use super::Value;

impl Value for () {
    const NAME: &'static str = "nil";

    fn egal(&self, _: &Self) -> bool {
        true
    }

    fn to_string(&self, _: &mut Interpreter) -> InterpreterResult {
        InterpreterResult::Value(super::String::from("nil").to_object())
    }
}

impl Value for bool {
    const NAME: &'static str = "boolean";

    fn egal(&self, other: &Self) -> bool {
        self == other
    }

    fn d_hash(&self) -> crate::InterpreterResult<u64> {
        crate::InterpreterResult::Value(*self as u64)
    }

    fn to_string(&self, _interpreter: &mut crate::Interpreter) -> crate::InterpreterResult {
        crate::InterpreterResult::Value(
            super::String::from(if *self { "true" } else { "false" }).to_object(),
        )
    }
}
