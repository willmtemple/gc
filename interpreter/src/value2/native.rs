use std::sync::Arc;

use crate::{Interpreter, InterpreterError, InterpreterResult};

use super::{Nil, Object, Slice, Value};

pub type CallImpl = fn(&mut Interpreter, Slice) -> InterpreterResult;
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

    fn call(&self, _interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
        if let Some(call) = self.call {
            call(_interpreter, args)
        } else {
            InterpreterResult::Error(InterpreterError::UncallableValue(Nil.to_object()))
        }
    }

    fn to_string(&self, interpreter: &mut Interpreter) -> InterpreterResult {
        if let Some(to_string) = self.to_string {
            to_string(interpreter, self)
        } else {
            InterpreterResult::Value(super::String::from("<native object>").to_object())
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

impl Value for fn(&mut Interpreter, Slice) -> InterpreterResult {
    const NAME: &'static str = "function";

    fn egal(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }

    fn call(&self, interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
        self(interpreter, args)
    }

    fn to_string(&self, _: &mut Interpreter) -> InterpreterResult {
        InterpreterResult::Value(super::String::from("<native function>").to_object())
    }
}

macro_rules! impl_value_for_fn {
    ($($t:ident)*) => {
        impl<$($t : Value + Clone,)* R: Value> Value for fn($($t),*) -> R {
            const NAME: &'static str = "function";

            fn egal(&self, other: &Self) -> bool {
                core::ptr::eq(self, other)
            }

            fn call(&self, _: &mut Interpreter, args: Slice) -> InterpreterResult {
                let mut idx = 0;
                InterpreterResult::Value(
                    self(
                        $(
                            #[allow(unused_assignments)]
                            {
                                let r = args
                                    .get(idx)
                                    .and_then(|v| v.downcast::<$t>().map(InterpreterResult::Value))
                                    .unwrap_or(InterpreterResult::Error(
                                        InterpreterError::ArgumentError {
                                            expected: <$t as Value>::NAME,
                                            position: idx,
                                        },
                                    ))?;
                                idx += 1;
                                r.clone()
                            }
                        ),*
                    )
                    .to_object(),
                )
            }
        }
    };
}

impl_value_for_fn!(T1);

impl<T1: Value + Clone, T2: Value + Clone, R: Value> Value for fn(T1, T2) -> R {
    const NAME: &'static str = "function";

    fn egal(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }

    fn call(&self, _: &mut Interpreter, args: Slice) -> InterpreterResult {
        let mut idx = 0;
        InterpreterResult::Value(
            self(
                #[allow(unused_assignments)]
                {
                    let r = args
                        .get(idx)
                        .and_then(|v| v.downcast::<T1>().map(InterpreterResult::Value))
                        .unwrap_or(InterpreterResult::Error(InterpreterError::ArgumentError {
                            expected: T1::NAME,
                            position: idx,
                        }))?;
                    idx += 1;
                    r.clone()
                },
                #[allow(unused_assignments)]
                {
                    let r = args
                        .get(idx)
                        .and_then(|v| v.downcast::<T2>().map(InterpreterResult::Value))
                        .unwrap_or(InterpreterResult::Error(InterpreterError::ArgumentError {
                            expected: T1::NAME,
                            position: idx,
                        }))?;
                    idx += 1;

                    r.clone()
                },
            )
            .to_object(),
        )
    }
}
