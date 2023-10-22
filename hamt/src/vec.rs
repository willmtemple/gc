use core::{
    marker::PhantomData,
    ops::{Deref, Index, RangeBounds},
};

use crate::{
    config::{DefaultConfig, HamtConfig, Kvp},
    iter::HamtIterator,
    node::{util::HashCode, LeafNode},
};

#[derive(Default)]
pub struct HamtVec<V, Config: HamtConfig<(), V> = DefaultConfig> {
    size: usize,
    root: Option<Config::NodeStore>,
}

impl<V, Config: HamtConfig<(), V>> Clone for HamtVec<V, Config> {
    fn clone(&self) -> Self {
        Self {
            size: self.size,
            root: self.root.clone(),
        }
    }
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
                None => Some(LeafNode::<(), V, Config>::create_with_pair(
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

    pub fn slice<R: RangeBounds<usize>>(&self, index: R) -> HamtVecSlice<V, Config> {
        let start = match index.start_bound() {
            core::ops::Bound::Included(&start) => start,
            core::ops::Bound::Excluded(&start) => start + 1,
            core::ops::Bound::Unbounded => 0,
        };

        let end = match index.end_bound() {
            core::ops::Bound::Included(&end) => end + 1,
            core::ops::Bound::Excluded(&end) => end,
            core::ops::Bound::Unbounded => self.size,
        };

        assert!(start <= end, "start of range cannot be greater than end");

        assert!(end <= self.size, "end of range cannot be greater than size");

        HamtVecSlice {
            offset: start,
            len: end - start,
            vec: self.clone(),
        }
    }

    #[cfg(feature = "std")]
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
                NodePtr::Leaf(leaf) => {
                    println!("{}- Leaf (hash: {:#066b}):", spaces, leaf._header.hash);
                    println!("{}  Key   : {:?}", spaces, leaf.entry.key());
                    println!("{}  Value : {:?}", spaces, leaf.entry.value());
                }
                NodePtr::Collision(_) => unreachable!(),
                NodePtr::Interior(node) => {
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

pub struct HamtVecSlice<V, Config: HamtConfig<(), V>> {
    offset: usize,
    len: usize,
    vec: HamtVec<V, Config>,
}

impl<V, Config: HamtConfig<(), V>> Clone for HamtVecSlice<V, Config> {
    fn clone(&self) -> Self {
        Self {
            offset: self.offset,
            len: self.len,
            vec: self.vec.clone(),
        }
    }
}

impl<V, Config: HamtConfig<(), V>> HamtVecSlice<V, Config> {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn get(&self, index: usize) -> Option<&V> {
        if index >= self.len {
            None
        } else {
            self.vec.get(self.offset + index)
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &V> {
        let mut iter = HamtIterator::<(), V, Config>::new(self.vec.root.clone());

        if !self.is_empty() {
            iter.set_cursor(self.offset as HashCode);
        }

        iter.take(self.len).map(|v| v.1)
    }
}

// impl Index

impl<V, Config: HamtConfig<(), V>> Index<usize> for HamtVecSlice<V, Config> {
    type Output = V;

    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.len {
            panic!(
                "index out of bounds: the len is {} but the index is {}",
                self.len, index
            )
        } else {
            self.vec
                .get(self.offset + index)
                .expect("unexpected error: HamtVecSlice index not found")
        }
    }
}
