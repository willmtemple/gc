use core::{
    fmt::{self, Debug, Formatter},
    iter::{Map, Take},
    ops::{Deref, Index, RangeBounds},
};

use crate::{
    config::{DefaultConfig, HamtConfig, Kvp},
    iter::HamtIterator,
    node::{util::HashCode, LeafNode},
};

pub struct HamtVec<V, Config: HamtConfig<(), V> = DefaultConfig> {
    config: Config,
    slice: HamtVecSlice<V, Config>,
}

impl<V, Config: HamtConfig<(), V> + Default> Default for HamtVec<V, Config> {
    fn default() -> Self {
        Self::new_with_config(Default::default())
    }
}

impl<V, Config: HamtConfig<(), V>> Clone for HamtVec<V, Config> {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            slice: self.slice.clone(),
        }
    }
}

impl<V, Config: HamtConfig<(), V> + Default> HamtVec<V, Config> {
    pub fn new() -> Self {
        Self::new_with_config(Config::default())
    }
}

impl<V, Config: HamtConfig<(), V>> HamtVec<V, Config> {
    pub fn new_with_config(config: Config) -> Self {
        Self {
            config,
            slice: HamtVecSlice {
                offset: 0,
                len: 0,
                root: None,
            },
        }
    }

    pub fn push(&self, v: V) -> Self {
        Self {
            config: self.config.clone(),
            slice: HamtVecSlice {
                offset: self.slice.offset,
                len: self.slice.len + 1,
                root: match self.slice.root.as_ref() {
                    Some(root) => {
                        Some(root.insert(&self.config, (), v, self.slice.len as HashCode))
                    }
                    None => Some(LeafNode::<(), V, Config>::create_with_pair(
                        &self.config,
                        (),
                        v,
                        self.slice.len as HashCode,
                    )),
                },
            },
        }
    }

    // TODO/wtemple - difficult to represent `pop` with persistence
    // pub fn pop(&self) -> Option<(impl Deref<Target = V>, Self)> {
    //     if self.size == 0 {
    //         None
    //     } else {
    //         struct ValueGuard<T, V> {
    //             kvp: T,
    //             _ph: PhantomData<V>,
    //         }

    //         impl<T: Kvp<(), V>, V> ValueGuard<T, V> {
    //             pub fn new(kvp: T) -> Self {
    //                 Self {
    //                     kvp,
    //                     _ph: PhantomData,
    //                 }
    //             }
    //         }

    //         impl<T: Kvp<(), V>, V> Deref for ValueGuard<T, V> {
    //             type Target = V;

    //             fn deref(&self) -> &Self::Target {
    //                 self.kvp.value()
    //             }
    //         }

    //         let root = self
    //             .root
    //             .as_ref()
    //             .expect("no root node when size is nonzero");

    //         let kvp = root
    //             .get(&(), self.size as HashCode - 1)
    //             .expect("failed to get known existing value");

    //         let root = root
    //             .remove(&(), self.size as HashCode - 1)
    //             .expect("failed to remove value");

    //         Some((
    //             ValueGuard::new(kvp.clone()),
    //             Self {
    //                 size: self.size - 1,
    //                 root: Some(root),
    //             },
    //         ))
    //     }
    // }

    pub fn as_slice(&self) -> HamtVecSlice<V, Config> {
        self.slice.clone()
    }

    // TODO/wtemple
    // pub fn truncate(&self, len: usize) -> Self;

    // TODO/wtemple
    // pub fn swap_remove(&self, index: usize) -> Self;

    // TODO/wtemple - expensive
    // pub fn insert(&self, index: usize, element: V) -> Self;

    // TODO/wtemple
    // pub fn retain(&self, f: impl FnMut(&V) -> bool) -> Self;
}

impl<V, Config: HamtConfig<(), V>> Deref for HamtVec<V, Config> {
    type Target = HamtVecSlice<V, Config>;

    fn deref(&self) -> &Self::Target {
        &self.slice
    }
}

impl<'a, V, Config: HamtConfig<(), V>> IntoIterator for &'a HamtVec<V, Config> {
    type Item = &'a V;
    // TODO/wtemple - Can't figure this out.
    // type IntoIter = Map<Take<HamtIterator<'a, (), V, Config>>, fn((&'a (), &'a V)) -> &'a V>;

    type IntoIter = Box<dyn Iterator<Item = Self::Item> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.slice.into_iter())
    }
}

impl<V, Config: HamtConfig<(), V>> Debug for HamtVec<V, Config> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "HamtVec {{ ")?;
        write!(f, "{}, ", self.len())?;
        if let Some(root) = self.root.as_ref() {
            write!(f, "@{:p}", root)?;
        } else {
            write!(f, "Empty")?;
        }
        write!(f, " }}")
    }
}

pub struct HamtVecSlice<V, Config: HamtConfig<(), V> = DefaultConfig> {
    offset: usize,
    len: usize,
    root: Option<Config::NodeStore>,
}

impl<V, Config: HamtConfig<(), V>> Clone for HamtVecSlice<V, Config> {
    fn clone(&self) -> Self {
        Self {
            offset: self.offset,
            len: self.len,
            root: self.root.clone(),
        }
    }
}

impl<V, Config: HamtConfig<(), V>> Debug for HamtVecSlice<V, Config> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "HamtVecSlice {{ ")?;

        if let Some(root) = self.root.as_ref() {
            write!(f, "@{:p}", root)?;
        } else {
            write!(f, "Empty")?;
        }

        write!(f, " }}")?;

        write!(f, "[{}..{}]", self.offset, self.offset + self.len)
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
            self.root
                .as_ref()
                .and_then(|root| root.get(&(), (self.offset + index) as HashCode))
                .map(|kvp| kvp.value())
        }
    }

    pub fn try_as_vec(&self) -> Option<HamtVec<V, Config>>
    where
        Config: Default,
    {
        if self.offset == 0 {
            Some(HamtVec {
                config: Default::default(),
                slice: self.clone(),
            })
        } else {
            None
        }
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
            core::ops::Bound::Unbounded => self.len,
        };

        assert!(start <= end, "start of range cannot be greater than end");

        assert!(end <= self.len, "end of range cannot be greater than len");

        Self {
            offset: self.offset + start,
            len: end - start,
            root: self.root.clone(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &V> {
        let mut iter = HamtIterator::<(), V, Config>::new(self.root.clone());

        if !self.is_empty() && self.offset != 0 {
            iter.set_cursor(self.offset as HashCode);
        }

        iter.take(self.len).map(|v| v.1)
    }
}

impl<'a, V, Config: HamtConfig<(), V>> IntoIterator for &'a HamtVecSlice<V, Config> {
    type Item = &'a V;

    type IntoIter = Map<Take<HamtIterator<'a, (), V, Config>>, fn((&'a (), &'a V)) -> &'a V>;

    fn into_iter(self) -> Self::IntoIter {
        let mut iter = HamtIterator::<'a, (), V, Config>::new(self.root.clone());

        if !self.is_empty() && self.offset != 0 {
            iter.set_cursor(self.offset as HashCode);
        }

        let f: fn((&'a (), &'a V)) -> &'a V = |(_, v)| v;

        iter.take(self.len).map(f)
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
            self.get(index)
                .expect("unexpected error: HamtVecSlice index not found")
        }
    }
}

impl<V: Eq, Config: HamtConfig<(), V>> Eq for HamtVec<V, Config> {}

impl<V: Eq, Config: HamtConfig<(), V>> PartialEq for HamtVec<V, Config> {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

use core::hash::Hash;

impl<V: Hash, Config: HamtConfig<(), V>> Hash for HamtVec<V, Config> {
    fn hash<H: core::hash::Hasher>(&self, _state: &mut H) {
        for v in self.iter() {
            v.hash(_state);
        }
    }
}

impl<V: Eq, Config: HamtConfig<(), V>> Eq for HamtVecSlice<V, Config> {}

impl<V: Eq, Config: HamtConfig<(), V>> PartialEq for HamtVecSlice<V, Config> {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl<V: Hash, Config: HamtConfig<(), V>> Hash for HamtVecSlice<V, Config> {
    fn hash<H: core::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

impl<V: Hash, Config: HamtConfig<(), V> + Default> FromIterator<V> for HamtVec<V, Config> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let mut vec = Self::new_with_config(Config::default());

        for v in iter {
            vec = vec.push(v);
        }

        vec
    }
}
