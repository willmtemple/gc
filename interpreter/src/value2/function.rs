use std::sync::Arc;

use hamt::HamtVec;

use crate::{ast::Param, scope::Scope, Interpreter, InterpreterResult};

use super::{block::Block, Nil, Slice, Value};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Function {
    pub name: Option<Arc<str>>,
    pub params: HamtVec<Param>,
    pub body: Block,
    pub scope: Scope,
}

impl Value for Function {
    const NAME: &'static str = "function";

    fn egal(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }

    fn call(&self, interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
        let mut call_scope = self.scope.clone();

        for (idx, param) in self.params.iter().enumerate() {
            match param {
                Param::Symbol(s) => {
                    let argument = args.get(idx).cloned().unwrap_or(Nil.to_object());

                    call_scope
                        .define(s)
                        .value
                        .set(argument)
                        .expect("fatal error: new binding is already set")
                }
                Param::Rest(r) => {
                    let rest = args.slice(idx..);

                    call_scope
                        .define(r)
                        .value
                        .set({
                            let mut vec = super::Vec::default();

                            for item in rest.iter() {
                                vec = vec.push(item.clone());
                            }

                            vec.to_object()
                        })
                        .expect("fatal error: new binding is already set")
                }
            }
        }

        interpreter.eval_block(&mut call_scope, &self.body)
    }

    fn to_string(&self, _interpreter: &mut Interpreter) -> InterpreterResult {
        let mut strs = vec![];

        for param in self.params.iter() {
            strs.push(match param {
                Param::Symbol(s) => s.name.clone(),
                Param::Rest(r) => format!("...{}", r.name).into(),
            });
        }

        InterpreterResult::Value(
            crate::value2::String::from(format!(
                "fn {}({}) {{ ... }}",
                self.name.as_deref().unwrap_or(""),
                strs.join(", "),
            ))
            .to_object(),
        )
    }
}
