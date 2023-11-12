use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    sync::Arc,
};

use super::Value;

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub struct Symbol {
    pub name: Arc<str>,
}

impl Symbol {
    pub fn new(name: impl Into<Arc<str>>) -> Self {
        Self { name: name.into() }
    }
}

impl Value for Symbol {
    const NAME: &'static str = "symbol";

    fn egal(&self, other: &Self) -> bool {
        self.name == other.name
    }

    fn d_hash(&self) -> crate::InterpreterResult<u64> {
        let mut hasher = DefaultHasher::new();

        self.name.hash(&mut hasher);
        Self::TYPE_ID.hash(&mut hasher);

        crate::InterpreterResult::Value(hasher.finish())
    }

    fn to_string(&self, _: &mut crate::Interpreter) -> crate::InterpreterResult {
        crate::InterpreterResult::Value(super::String::from(format!(":{}", self.name)).to_object())
    }
}
