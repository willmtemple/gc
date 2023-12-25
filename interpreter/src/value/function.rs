use std::sync::Arc;

use hamt::HamtVec;

use crate::{ast::Parameter, Interpreter, InterpreterResult};

use super::{block::Block, Slice, Value};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Function {
    pub name: Option<Arc<str>>,
    pub params: HamtVec<Parameter>,
    pub body: Block,
}

impl Value for Function {
    fn egal(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }

    fn call(&self, interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
        let env = self.body.environ().clone();

        for (idx, param) in self.params.iter().enumerate() {
            match param {
                Parameter::Symbol(s) => {
                    let argument = args.get(idx).cloned().unwrap_or(().to_object());

                    let Some(binding) = env.get_binding(s) else {
                        panic!("failed to get function binding: {}", s.name())
                    };

                    binding.set(argument);
                }
                Parameter::Rest(r) => {
                    let rest = args.slice(idx..).to_object();

                    let Some(binding) = env.get_binding(r) else {
                        panic!("failed to get function rest binding: {}", r.name());
                    };

                    binding.set(rest);
                }
            }
        }

        interpreter.eval_block(&self.body)
    }

    fn to_string(&self, _interpreter: &mut Interpreter) -> InterpreterResult {
        let mut strs = vec![];

        for param in self.params.iter() {
            strs.push(match param {
                Parameter::Symbol(s) => ToString::to_string(&s.name()),
                Parameter::Rest(r) => format!("...{}", r.name()),
            });
        }

        InterpreterResult::Value(
            crate::value::String::from(format!(
                "fn {}({}) {{ ... }}",
                self.name.as_deref().unwrap_or(""),
                strs.join(", "),
            ))
            .to_object(),
        )
    }
}
