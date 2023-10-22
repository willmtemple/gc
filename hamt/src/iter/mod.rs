use core::{hash::Hash, ops::Deref};

use crate::node::NodePtr;

use super::{
    config::{HamtConfig, Kvp},
    node::util::MAX_LEVEL,
    node::NodeHeader,
};

pub struct HamtIterator<'a, K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    // We don't edit this. Only hold on to it so that no underlying memory is freed until this iterator is dropped.
    _root: Option<Config::NodeStore>,
    size: usize,
    stack: [NodeCursor<'a, K, V, Config>; MAX_LEVEL],
}

struct NodeCursor<'a, K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    node: Option<&'a NodeHeader<K, V, Config>>,
    index: usize,
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Copy for NodeCursor<'_, K, V, Config> {}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Clone for NodeCursor<'_, K, V, Config> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Default for NodeCursor<'_, K, V, Config> {
    fn default() -> Self {
        Self {
            node: None,
            index: 0,
        }
    }
}

impl<'a, K: Eq + Hash, V, Config: HamtConfig<K, V>> HamtIterator<'a, K, V, Config> {
    pub fn new(root: Option<Config::NodeStore>) -> Self {
        if let Some(rn) = root.as_ref() {
            let mut r = Self {
                _root: root.clone(),
                size: 1,
                stack: [Default::default(); MAX_LEVEL],
            };

            r.stack[0] = NodeCursor {
                index: 0,
                node: Some(unsafe {
                    core::mem::transmute::<&NodeHeader<K, V, Config>, &'a NodeHeader<K, V, Config>>(
                        rn.deref(),
                    )
                }),
            };

            r.find_bottom();

            r
        } else {
            Self {
                _root: None,
                size: 0,
                stack: [Default::default(); MAX_LEVEL],
            }
        }
    }

    fn is_empty(&self) -> bool {
        self.size == 0
    }

    fn push(&mut self, node: &NodeHeader<K, V, Config>) {
        assert!(self.size < MAX_LEVEL);

        let cursor = NodeCursor {
            node: Some(unsafe {
                core::mem::transmute::<&NodeHeader<K, V, Config>, &'a NodeHeader<K, V, Config>>(
                    node,
                )
            }),
            index: 0,
        };

        self.stack[{
            let s = self.size;
            self.size += 1;
            s
        }] = cursor;
    }

    fn cur(&self) -> NodeCursor<'a, K, V, Config> {
        self.stack[self.size - 1]
    }

    fn cur_mut(&mut self) -> &mut NodeCursor<'a, K, V, Config> {
        &mut self.stack[self.size - 1]
    }

    fn find_bottom(&mut self) -> &'a Config::Kvp {
        let mut cursor = self.cur();

        while let Some(node) = &cursor.node {
            match node.upgrade() {
                NodePtr::Collision(l) => {
                    return &l.values[cursor.index];
                }
                NodePtr::Inner(node) => {
                    self.push(&node.children[cursor.index]);
                    cursor = self.cur();
                }
            }
        }

        unreachable!();
    }

    fn shift_cursor(&mut self) {
        while self.size > 0 {
            let cursor = self.cur_mut();
            let node = cursor.node.as_ref().expect("unreachable None node in shr");

            match node.upgrade() {
                NodePtr::Collision(l) => {
                    if cursor.index < l.values.len() - 1 {
                        cursor.index += 1;
                        break;
                    } else {
                        self.size -= 1;
                    }
                }
                NodePtr::Inner(i) => {
                    if cursor.index < i.children.len() - 1 {
                        cursor.index += 1;
                        break;
                    } else {
                        self.size -= 1;
                    }
                }
            }
        }
    }
}

impl<'a, K: Eq + Hash, V, Config: HamtConfig<K, V>> Iterator for HamtIterator<'a, K, V, Config> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            return None;
        }

        let kvp = self.find_bottom();
        let (k, v) = kvp.key_value();

        // eprintln!("Ret : {:p}, {:p}", k, v);

        self.shift_cursor();

        Some(unsafe {
            (
                core::mem::transmute::<&K, &'a K>(k),
                core::mem::transmute::<&V, &'a V>(v),
            )
        })
    }
}
