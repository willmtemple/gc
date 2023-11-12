use super::Value;

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
            super::String::from(if *self { "true" } else { "false" }).to_value1(),
        )
    }
}
