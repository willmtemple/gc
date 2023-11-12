use std::sync::Arc;

use hamt::{vec::HamtVecSlice, HamtVec};

use crate::{ast::Param, scope::Scope, Interpreter, InterpreterResult, Value as Value1};

use super::{block::Block, to_value1_args, Nil, Object, Value};

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

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: HamtVecSlice<Arc<Object>>,
    ) -> InterpreterResult {
        let mut call_scope = self.scope.clone();

        let args = to_value1_args(args);

        for (idx, param) in self.params.iter().enumerate() {
            match param {
                Param::Symbol(s) => {
                    let argument = args
                        .get(idx)
                        .cloned()
                        .unwrap_or(Value1::Object(Nil.to_object()));

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
                                vec = vec.push(item.clone().to_value2());
                            }

                            vec.to_value1()
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
            .to_value1(),
        )
    }
}
