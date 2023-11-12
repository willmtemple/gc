use std::sync::Arc;

use hamt::vec::HamtVecSlice;

use crate::{Interpreter, InterpreterError, InterpreterResult};

use super::{Nil, Object, Value};

pub type CallImpl = fn(&mut Interpreter, HamtVecSlice<Arc<Object>>) -> InterpreterResult;
pub type ToStringImpl = fn(&mut Interpreter, &NativeObject) -> InterpreterResult;
pub type GetImpl = fn(&mut Interpreter, &NativeObject, Arc<Object>) -> InterpreterResult;

#[derive(Default)]
pub struct NativeObject {
    call: Option<CallImpl>,
    to_string: Option<ToStringImpl>,
    get: Option<GetImpl>,
}

impl NativeObject {
    pub fn new() -> NativeObject {
        NativeObject {
            ..Default::default()
        }
    }

    pub fn with_call(self, call: CallImpl) -> Self {
        Self {
            call: Some(call),
            ..self
        }
    }

    pub fn with_to_string(self, to_string: ToStringImpl) -> Self {
        Self {
            to_string: Some(to_string),
            ..self
        }
    }

    pub fn with_get(self, get: GetImpl) -> Self {
        Self {
            get: Some(get),
            ..self
        }
    }
}

impl Value for NativeObject {
    const NAME: &'static str = "object";

    fn egal(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _args: HamtVecSlice<Arc<Object>>,
    ) -> InterpreterResult {
        if let Some(call) = self.call {
            call(_interpreter, _args)
        } else {
            InterpreterResult::Error(InterpreterError::UncallableValue(crate::Value::Object(
                Nil.to_object(),
            )))
        }
    }

    fn to_string(&self, interpreter: &mut Interpreter) -> InterpreterResult {
        if let Some(to_string) = self.to_string {
            to_string(interpreter, self)
        } else {
            InterpreterResult::Value(super::String::from("<native object>").to_value1())
            // InterpreterResult::Error(InterpreterError::ProtocolNotImplemented(
            //     "As<string>",
            //     Self::NAME,
            // ))
        }
    }

    fn get(&self, _interpreter: &mut Interpreter, _key: Arc<Object>) -> InterpreterResult {
        if let Some(get) = self.get {
            get(_interpreter, self, _key)
        } else {
            InterpreterResult::Error(InterpreterError::ProtocolNotImplemented("Get", Self::NAME))
        }
    }
}
