use crate::{value2, Interpreter, InterpreterError, InterpreterResult};

use super::Value;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Block {
    pub body: value2::Vec,
}

impl Value for Block {
    const NAME: &'static str = "block";

    fn egal(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }

    fn to_string(&self, interpreter: &mut Interpreter) -> crate::InterpreterResult {
        let mut parts = vec![];

        for v in self.body.iter() {
            let vs = v.to_string(interpreter)?;

            if !vs.is::<super::String>() {
                return InterpreterResult::Error(InterpreterError::UnexpectedType {
                    expected: super::String::NAME,
                    actual: vs.type_name(),
                });
            }

            parts.push(vs.downcast::<super::String>().unwrap().value.clone());
        }

        InterpreterResult::Value(
            crate::value2::String::from(format!("{{{}}}", parts.join("; "))).to_object(),
        )
    }
}
