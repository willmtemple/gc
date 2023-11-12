use std::{fmt::Debug, hash::Hash, ops::Deref, sync::Arc};

use hamt::{vec::HamtVecSlice, HamtVec};

use crate::{
    value2::{Legacy, Object, Value as Value2},
    Interpreter, InterpreterError, InterpreterResult,
};

#[derive(Clone, Eq, Hash, PartialEq, Debug)]
pub enum Value {
    Object(Arc<Object>),
}

impl Value {
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: HamtVecSlice<Value>,
    ) -> InterpreterResult<Value> {
        match self {
            Value::Object(o) => o.call(
                interpreter,
                args.iter()
                    .map(|v| v.to_value2())
                    .collect::<HamtVec<_>>()
                    .as_slice(),
            ),
            _ => InterpreterResult::Error(InterpreterError::UncallableValue(self.clone())),
        }
    }

    pub fn get(&self, interpreter: &mut Interpreter, key: &Value) -> Value {
        match self {
            Value::Object(o) => match key {
                Value::Object(k) => o.get(interpreter, k.clone()).expect_result().unwrap(),
                k => o
                    .get(interpreter, Legacy::new(k.clone()).to_object())
                    .expect_result()
                    .unwrap_or_else(|e| panic!("fatal error: {:?}", e)),
            },
        }
    }

    pub fn print(&self, interpreter: &mut Interpreter, indent: usize) -> String {
        match self {
            Value::Object(o) => match o.to_string(interpreter).expect_result().unwrap() {
                Value::Object(o) if o.type_id() == crate::value2::String::TYPE_ID => {
                    String::from(o.downcast::<crate::value2::String>().unwrap().value.deref())
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn type_name(&self) -> Arc<str> {
        match self {
            Value::Object(o) => o.type_name().into(),
        }
    }

    pub fn to_value2(&self) -> Arc<Object> {
        match self {
            Value::Object(o) => o.clone(),
            _ => Object::new(Legacy::new(self.clone())).into(),
        }
    }

    pub fn downcast<T: Value2 + 'static>(&self) -> Option<&T> {
        match self {
            Value::Object(o) => o.downcast(),
            _ => None,
        }
    }
}
