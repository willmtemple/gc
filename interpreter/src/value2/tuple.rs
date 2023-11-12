use std::sync::Arc;

use crate::{InterpreterError, InterpreterResult};

use super::{Object, Value};

pub struct Tuple {
    data: super::Vec,
}

impl Tuple {
    pub fn new(data: super::Vec) -> Self {
        Self { data }
    }

    pub fn data(&self) -> &super::Vec {
        &self.data
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Value for Tuple {
    const NAME: &'static str = "tuple";

    fn egal(&self, other: &Self) -> bool {
        self.data == other.data
    }

    fn get(&self, _: &mut crate::Interpreter, index: Arc<Object>) -> crate::InterpreterResult {
        let index = match index.downcast::<i64>() {
            Some(n @ 0..) => *n as usize,
            Some(n) => {
                return InterpreterResult::Error(InterpreterError::InvalidIndex(n.to_object()));
            }
            None => {
                return InterpreterResult::Error(InterpreterError::UnexpectedType {
                    expected: i64::NAME,
                    actual: index.type_name(),
                })
            }
        };

        if index >= self.data.len() {
            return InterpreterResult::Error(InterpreterError::InvalidIndex(
                (index as i64).to_object(),
            ));
        }

        InterpreterResult::Value(self.data[index].clone())
    }

    fn to_string(&self, interpreter: &mut crate::Interpreter) -> InterpreterResult {
        let mut parts = Vec::new();

        for v in self.data.iter() {
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
            crate::value2::String::from(format!("({})", parts.join(", "))).to_object(),
        )
    }
}
