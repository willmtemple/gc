use std::sync::Arc;

use crate::{InterpreterError, InterpreterResult};

use super::{Object, Type, Value};

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
                    expected: Type::of::<i64>().name(),
                    actual: index.get_type().name(),
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
                    expected: Type::of::<super::String>().name(),
                    actual: vs.get_type().name(),
                });
            }

            parts.push(vs.downcast::<super::String>().unwrap().clone());
        }

        InterpreterResult::Value(
            crate::value::String::from(format!("({})", parts.join(", "))).to_object(),
        )
    }
}
