use std::{
    any::TypeId,
    hash::{Hash, Hasher},
};

use crate::InterpreterResult;

use super::Value;

#[derive(Debug)]
pub struct Type {
    name: &'static str,
    id: TypeId,
}

impl Copy for Type {}

impl Clone for Type {
    fn clone(&self) -> Self {
        *self
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Type {
    pub const fn name(&self) -> &'static str {
        self.name
    }

    pub const fn id(&self) -> TypeId {
        self.id
    }
}

impl Value for Type {
    fn egal(&self, other: &Self) -> bool {
        self.id == other.id
    }

    fn d_hash(&self) -> crate::InterpreterResult<u64> {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();

        self.id.hash(&mut hasher);

        InterpreterResult::Value(hasher.finish())
    }

    fn to_string(&self, interpreter: &mut crate::Interpreter) -> InterpreterResult {
        InterpreterResult::Value(super::String::from(self.name).to_object())
    }
}

impl Type {
    pub const fn of<T: Value>() -> Self {
        Self {
            name: core::any::type_name::<T>(),
            id: TypeId::of::<T>(),
        }
    }
}
