use std::{hash::Hash, sync::Arc};

use hamt::{config::CloningConfig, HamtVec};

use crate::{ast::Expr, scope::LexicalEnvironment, Interpreter, InterpreterResult};

use super::{Object, Value};

pub struct LexicalBlock {
    pub body: HamtVec<Arc<Object>, CloningConfig>,
}

impl Value for LexicalBlock {
    fn egal(&self, other: &Self) -> bool {
        self.body == other.body
    }
}

pub type Body = HamtVec<Expr, CloningConfig>;

#[derive(Debug, Clone)]
pub struct Block {
    pub body: Body,
    environ: LexicalEnvironment,
}

impl Eq for Block {}

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body
    }
}

impl Hash for Block {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.body.hash(state)
    }
}

impl Block {
    pub fn new(body: Body, environ: LexicalEnvironment) -> Self {
        Self { body, environ }
    }

    pub fn environ(&self) -> &LexicalEnvironment {
        &self.environ
    }
}

impl Value for Block {
    fn egal(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }

    fn to_string(&self, interpreter: &mut Interpreter) -> crate::InterpreterResult {
        let mut parts = vec![];

        for v in self.body.iter() {
            parts.push(format!("{:?}", v));
        }

        InterpreterResult::Value(
            crate::value::String::from(format!("{{{}}}", parts.join("; "))).to_object(),
        )
    }
}
