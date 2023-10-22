use core::{
    marker::PhantomData,
    ops::{Deref, Index},
};

use crate::hamt::config::Kvp;

use super::{
    config::{DefaultGlobal, HamtConfig},
    iter::HamtIterator,
    node::{util::HashCode, Collision},
};

#[derive(Default)]
pub struct HamtVec<V, Config: HamtConfig<(), V> = DefaultGlobal> {
    size: usize,
    root: Option<Config::NodeStore>,
}

impl<V, Config: HamtConfig<(), V>> HamtVec<V, Config> {
    pub fn new() -> Self {
        Self {
            size: 0,
            root: None,
        }
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn get(&self, index: usize) -> Option<&V> {
        if index >= self.size {
            None
        } else {
            self.root
                .as_ref()
                .and_then(|root| root.get(&(), index as HashCode))
                .map(|kvp| kvp.value())
        }
    }

    pub fn push(&self, v: V) -> Self {
        Self {
            size: self.size + 1,
            root: match self.root.as_ref() {
                Some(root) => Some(root.insert((), v, self.size as HashCode)),
                None => Some(Collision::<(), V, Config>::create_with_pair(
                    (),
                    v,
                    self.size as HashCode,
                )),
            },
        }
    }

    pub fn pop(&self) -> Option<(impl Deref<Target = V>, Self)> {
        if self.size == 0 {
            None
        } else {
            struct ValueGuard<T, V> {
                kvp: T,
                _ph: PhantomData<V>,
            }

            impl<T: Kvp<(), V>, V> ValueGuard<T, V> {
                pub fn new(kvp: T) -> Self {
                    Self {
                        kvp,
                        _ph: PhantomData,
                    }
                }
            }

            impl<T: Kvp<(), V>, V> Deref for ValueGuard<T, V> {
                type Target = V;

                fn deref(&self) -> &Self::Target {
                    self.kvp.value()
                }
            }

            let root = self
                .root
                .as_ref()
                .expect("no root node when size is nonzero");

            let kvp = root
                .get(&(), self.size as HashCode - 1)
                .expect("failed to get known existing value");

            let root = root
                .remove(&(), self.size as HashCode - 1)
                .expect("failed to remove value");

            Some((
                ValueGuard::new(kvp.clone()),
                Self {
                    size: self.size - 1,
                    root: Some(root),
                },
            ))
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &V> {
        HamtIterator::<(), V, Config>::new(self.root.clone()).map(|v| v.1)
    }

    #[cfg(all(test, feature = "std"))]
    pub fn print(&self)
    where
        V: core::fmt::Debug,
    {
        use super::node::NodePtr;
        use core::hash::Hash;

        if self.root.is_none() {
            println!("Empty HAMT.");
            return;
        }

        print_node::<(), V, Config>(self.root.as_ref().unwrap(), 0);

        fn print_node<
            K: Eq + Hash + core::fmt::Debug,
            V: core::fmt::Debug,
            Alloc: HamtConfig<K, V>,
        >(
            node: &Alloc::NodeStore,
            level: usize,
        ) {
            let spaces = " ".repeat(level * 2);

            match node.deref().upgrade() {
                NodePtr::Collision(collision) => {
                    println!(
                        "{}- Collision (hash: {:#066b}):",
                        spaces, collision._header.hash
                    );
                    for kvp in collision.values.iter() {
                        println!("{}  - {:?}: {:?}", spaces, kvp.key(), kvp.value());
                    }
                }
                NodePtr::Inner(node) => {
                    println!(
                        "{}- Node ({}): {:#066b}",
                        spaces,
                        node._header.level(),
                        node.bitmap
                    );
                    for child in node.children.iter() {
                        print_node::<K, V, Alloc>(child, level + 1);
                    }
                }
            }
        }
    }
}

impl<V, Config: HamtConfig<(), V>> Index<usize> for HamtVec<V, Config> {
    type Output = V;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap_or_else(|| {
            panic!(
                "index out of bounds: the len is {} but the index is {}",
                self.size, index
            )
        })
    }
}
