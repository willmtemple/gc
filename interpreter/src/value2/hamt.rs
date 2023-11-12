use std::sync::Arc;

use hamt::{config::CloningConfig, vec::HamtVecSlice, HamtMap, HamtSet, HamtVec};

use crate::{InterpreterError, InterpreterResult};

use super::{Nil, Object, Value};

pub type Map = HamtMap<Arc<Object>, Arc<Object>, CloningConfig>;

impl Value for Map {
    const NAME: &'static str = "map";

    fn egal(&self, other: &Self) -> bool {
        // Compare all keys and values... This is pretty expensive.
        let mut other = other.clone();

        for (k, v) in self.iter() {
            if let Some(v2) = other.get(k) {
                if !v.eq(v2) {
                    return false;
                }

                other = other.remove(k);
            } else {
                return false;
            }
        }

        let next: Option<(&Arc<Object>, &Arc<Object>)> = other.iter().next();

        next.is_none()
    }

    fn call(&self, _: &mut crate::Interpreter, args: Slice) -> crate::InterpreterResult {
        // Maps act as functions of keys
        if args.len() != 1 {
            return InterpreterResult::Error(InterpreterError::InvalidArity(1, args.len()));
        }

        let key = args.get(0).unwrap();

        InterpreterResult::Value(self.get(key).cloned().unwrap_or(Nil.to_object()))
    }

    fn to_string(&self, interpreter: &mut crate::Interpreter) -> crate::InterpreterResult {
        let mut parts = vec![];

        for (k, v) in self.iter() {
            let ks = k.to_string(interpreter)?;

            if !ks.is::<super::String>() {
                return InterpreterResult::Error(InterpreterError::UnexpectedType {
                    expected: super::String::NAME,
                    actual: ks.type_name(),
                });
            }

            let vs = v.to_string(interpreter)?;

            if !vs.is::<super::String>() {
                return InterpreterResult::Error(InterpreterError::UnexpectedType {
                    expected: super::String::NAME,
                    actual: vs.type_name(),
                });
            }

            parts.push(format!(
                "{}: {}",
                ks.downcast::<super::String>().unwrap().value,
                vs.downcast::<super::String>().unwrap().value
            ));
        }

        InterpreterResult::Value(
            crate::value2::String::from(format!("#{{{}}}", parts.join(", "))).to_object(),
        )
    }

    fn get(&self, _interpreter: &mut crate::Interpreter, key: Arc<Object>) -> InterpreterResult {
        InterpreterResult::Value(Map::get(self, &key).cloned().unwrap_or(Nil.to_object()))
    }
}

pub type Set = HamtSet<Arc<Object>, CloningConfig>;

impl Value for Set {
    const NAME: &'static str = "set";

    fn egal(&self, other: &Self) -> bool {
        // Compare all keys and values... This is pretty expensive.
        let mut other = other.clone();

        for k in self.iter() {
            if other.contains(k) {
                other = other.remove(k);
            } else {
                return false;
            }
        }

        let next: Option<&Arc<Object>> = other.iter().next();

        next.is_none()
    }

    fn call(&self, _: &mut crate::Interpreter, args: Slice) -> crate::InterpreterResult {
        // Sets act as functions of keys
        if args.len() != 1 {
            return InterpreterResult::Error(InterpreterError::InvalidArity(1, args.len()));
        }

        let key = args.get(0).unwrap();

        InterpreterResult::Value(if self.contains(key) {
            key.clone()
        } else {
            Nil.to_object()
        })
    }

    fn to_string(&self, interpreter: &mut crate::Interpreter) -> crate::InterpreterResult {
        let mut parts = vec![];

        for k in self.iter() {
            let ks = k.to_string(interpreter)?;

            if !ks.is::<super::String>() {
                return InterpreterResult::Error(InterpreterError::UnexpectedType {
                    expected: super::String::NAME,
                    actual: ks.type_name(),
                });
            }

            parts.push(ks.downcast::<super::String>().unwrap().value.clone());
        }

        InterpreterResult::Value(
            crate::value2::String::from(format!("#({})", parts.join(", "))).to_object(),
        )
    }

    fn get(&self, _: &mut crate::Interpreter, _key: Arc<Object>) -> InterpreterResult {
        InterpreterResult::Value(self.contains(&_key).to_object())
    }
}

pub type Slice = HamtVecSlice<Arc<Object>, CloningConfig>;
pub type Vec = HamtVec<Arc<Object>, CloningConfig>;

impl Value for Vec {
    const NAME: &'static str = "vec";

    fn egal(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for (a, b) in self.iter().zip(other.iter()) {
            if a != b {
                return false;
            }
        }

        true
    }

    fn to_string(&self, interpreter: &mut crate::Interpreter) -> InterpreterResult {
        let mut parts = vec![];

        for v in self.iter() {
            let vs = v.to_string(interpreter)?;

            if !vs.is::<super::String>() {
                return InterpreterResult::Error(InterpreterError::UnexpectedType {
                    expected: super::String::NAME,
                    actual: vs.type_name(),
                });
            }

            parts.push(vs.downcast::<super::String>().unwrap().value.clone());
        }

        InterpreterResult::Value(
            crate::value2::String::from(format!("[{}]", parts.join(", "))).to_object(),
        )
    }

    fn call(&self, interpreter: &mut crate::Interpreter, args: Slice) -> InterpreterResult {
        // Vectors act as functions of indices
        if args.len() != 1 {
            return InterpreterResult::Error(InterpreterError::InvalidArity(1, args.len()));
        }

        let index = args.get(0).cloned().unwrap();

        self.get(interpreter, index)
    }

    fn get(&self, _: &mut crate::Interpreter, index: Arc<Object>) -> InterpreterResult {
        let index = match index.downcast::<i64>() {
            Some(n @ 0..) => *n as usize,
            Some(n) => {
                return InterpreterResult::Error(InterpreterError::InvalidIndex(n.to_object()));
            }
            None => {
                return InterpreterResult::Error(InterpreterError::UnexpectedType {
                    expected: i64::NAME,
                    actual: index.type_name(),
                })
            }
        };

        if index >= self.len() {
            return InterpreterResult::Error(InterpreterError::InvalidIndex(
                (index as i64).to_object(),
            ));
        }

        InterpreterResult::Value(self[index].clone())
    }
}
