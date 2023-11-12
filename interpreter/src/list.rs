use std::sync::Arc;

#[derive(Debug, PartialEq)]
pub enum List<T> {
    Nil,
    Node { first: Arc<T>, rest: Arc<List<T>> },
}

impl<T> Clone for List<T> {
    fn clone(&self) -> Self {
        match self {
            List::Nil => List::Nil,
            List::Node { first, rest } => List::Node {
                first: first.clone(),
                rest: rest.clone(),
            },
        }
    }
}

impl<T> List<T> {
    pub fn push_start(&self, v: T) -> Self {
        List::Node {
            first: Arc::new(v),
            rest: Arc::new(self.clone()),
        }
    }

    pub fn first(&self) -> Option<&T> {
        match self {
            List::Nil => None,
            List::Node { first, .. } => Some(first),
        }
    }

    pub fn rest(&self) -> Option<&List<T>> {
        match self {
            List::Nil => None,
            List::Node { rest, .. } => Some(rest),
        }
    }
}
