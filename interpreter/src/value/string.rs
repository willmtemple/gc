use std::{ops::Deref, sync::Arc};

use super::Value;

pub type String = Arc<str>;

impl Value for Arc<str> {
    fn egal(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }

    fn to_string(&self, _interpreter: &mut crate::Interpreter) -> crate::InterpreterResult {
        crate::InterpreterResult::Value(self.clone().to_object())
    }
}
