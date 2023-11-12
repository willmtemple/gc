use std::sync::Arc;

use super::Value;

pub struct String {
    pub value: Arc<str>,
}

impl<T: Into<Arc<str>>> From<T> for String {
    fn from(value: T) -> Self {
        Self {
            value: value.into(),
        }
    }
}

impl Value for String {
    const NAME: &'static str = "string";

    fn egal(&self, other: &Self) -> bool {
        self.value == other.value
    }

    fn to_string(&self, _interpreter: &mut crate::Interpreter) -> crate::InterpreterResult {
        crate::InterpreterResult::Value(
            Self {
                value: self.value.clone(),
            }
            .to_object(),
        )
    }
}
