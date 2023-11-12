use std::sync::Arc;

use hamt::HamtVec;

use crate::{value2, Interpreter};

use super::{Object, Value};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Block {
    pub body: HamtVec<Arc<Object>>,
}

impl Block {
    pub fn print(&self, interpreter: &mut Interpreter, indent: usize) -> String {
        let spaces = " ".repeat(indent);

        use itertools::Itertools;

        let mut lines = HamtVec::new();
        for (value_idx, value) in self.body.iter().enumerate() {
            let Some(segment) = value.downcast::<value2::Vec>() else {
                panic!("Blocks cannot be parsed without doubly-nested vectors.");
            };

            let mut sv = HamtVec::new();

            for token in segment.iter() {
                sv = sv.push(
                    token
                        .to_string(interpreter)
                        .expect_result()
                        .unwrap()
                        .downcast::<value2::String>()
                        .unwrap()
                        .value
                        .clone(),
                );
            }

            lines = lines.push(format!(
                "{}{}{}",
                spaces,
                sv.iter().join(" "),
                if value_idx == self.body.len() - 1 {
                    ""
                } else {
                    ";"
                }
            ));
        }

        format!("{{\n{}\n}}", lines.iter().join("\n"))
    }
}

impl Value for Block {
    const NAME: &'static str = "block";

    fn egal(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }
}
