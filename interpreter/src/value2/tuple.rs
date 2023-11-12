use std::sync::Arc;

use hamt::HamtVec;

use crate::{InterpreterError, InterpreterResult};

use super::{Object, Value};

use crate::Value as Value1;

pub struct Tuple {
    data: HamtVec<Arc<Object>>,
}

impl Tuple {
    pub fn new(data: HamtVec<Arc<Object>>) -> Self {
        Self { data }
    }

    pub fn data(&self) -> &HamtVec<Arc<Object>> {
        &self.data
    }

    pub fn len(&self) -> usize {
        self.data.len()
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
                return InterpreterResult::Error(InterpreterError::InvalidIndex(n.to_value1()));
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
                (index as i64).to_value1(),
            ));
        }

        InterpreterResult::Value(self.data[index].clone().to_value1())
    }

    fn to_string(&self, interpreter: &mut crate::Interpreter) -> InterpreterResult {
        let mut parts = Vec::new();

        for v in self.data.iter() {
            let vs = v.to_string(interpreter)?;

            let Value1::Object(vo) = vs else {
                panic!("Expected to_string to return a string.");
            };

            if !vo.is::<super::String>() {
                return InterpreterResult::Error(InterpreterError::UnexpectedType {
                    expected: super::String::NAME,
                    actual: vo.type_name(),
                });
            }

            parts.push(vo.downcast::<super::String>().unwrap().value.clone());
        }

        InterpreterResult::Value(
            crate::value2::String::from(format!("({})", parts.join(", "))).to_value1(),
        )
    }
}
