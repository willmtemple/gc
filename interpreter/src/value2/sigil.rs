use crate::Interpreter;

use super::Value;

#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct Sigil {
    pub(crate) identity: usize,
}

impl Sigil {
    pub fn text<'a>(&self, interpreter: &'a Interpreter) -> &'a str {
        interpreter.get_sigil_text(self)
    }
}

impl Value for Sigil {
    const NAME: &'static str = "sigil";

    fn egal(&self, other: &Self) -> bool {
        self.identity == other.identity
    }

    fn to_string(&self, interpreter: &mut Interpreter) -> crate::InterpreterResult {
        crate::InterpreterResult::Value(
            super::String::from(interpreter.get_sigil_text(self)).to_value1(),
        )
    }
}
