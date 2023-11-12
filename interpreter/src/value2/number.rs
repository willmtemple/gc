use super::Value;

impl Value for i64 {
    const NAME: &'static str = "number";

    fn egal(&self, other: &Self) -> bool {
        *self == *other
    }

    fn to_string(&self, _: &mut crate::Interpreter) -> crate::InterpreterResult {
        crate::InterpreterResult::Value(
            super::String::from(<i64 as ToString>::to_string(self)).to_object(),
        )
    }
}
