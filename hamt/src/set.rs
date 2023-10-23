use core::{
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use crate::{
    config::{DefaultConfig, HamtConfig, Kvp},
    iter::HamtIterator,
    node::{util::HashCode, LeafNode},
};

#[derive(Default)]
pub struct HamtSet<
    K: Eq + Hash,
    #[cfg(feature = "std")] H: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] H: Hasher + Default,
    Config: HamtConfig<K, ()> = DefaultConfig,
> {
    _ph: PhantomData<H>,
    // TODO/wtemple - size cannot be tracked as there is no way to know if an insert is a duplicate currently
    // or if a remove actually removed anything
    // size: usize,
    root: Option<Config::NodeStore>,
}

#[cfg(feature = "std")]
impl<K: Eq + Hash> HamtSet<K> {
    pub fn new() -> Self {
        Self {
            _ph: PhantomData,
            root: None,
        }
    }
}

impl<K: Eq + Hash, H: Hasher + Default, Config: HamtConfig<K, ()>> HamtSet<K, H, Config> {
    // TODO/wtemple - see abovea in definition of size: usize
    // pub fn len(&self) -> usize {
    //     self.size
    // }

    // pub fn is_empty(&self) -> bool {
    //     self.size == 0
    // }

    pub fn contains(&self, k: &K) -> bool {
        let hash = {
            let mut hasher = H::default();
            k.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        self.root
            .as_ref()
            .and_then(|root| root.get(k, hash))
            .is_some()
    }

    pub fn get(&self, k: impl AsRef<K>) -> Option<&K> {
        let k = k.as_ref();
        let hash = {
            let mut hasher = H::default();
            k.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        self.root
            .as_ref()
            .and_then(|root| root.get(k, hash))
            .map(|v| v.key())
    }

    pub fn insert(&self, k: K) -> Self {
        let hash = {
            let mut hasher = H::default();
            k.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                _ph: PhantomData,
                root: Some(root.insert(k, (), hash)),
            },
            None => Self {
                _ph: PhantomData,
                root: Some(LeafNode::<K, (), Config>::create_with_pair(k, (), hash)),
            },
        }
    }

    pub fn remove(&self, k: &K) -> Self {
        let hash = {
            let mut hasher = H::default();
            k.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                _ph: PhantomData,
                root: root.remove(k, hash),
            },
            None => Self {
                _ph: PhantomData,
                root: None,
            },
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &K> {
        HamtIterator::<K, (), Config>::new(self.root.clone()).map(|v| v.0)
    }

    // TODO/wtemple
    // pub fn retain(&self, f: impl Fn(&K) -> bool) -> Self;

    // TODO/wtemple
    // - Do we need to constrain to Self or can we allow any HamtSet<K, V, Config>?
    // pub fn difference(&self, other: &Self) -> Self;

    // TODO/wtemple
    // pub fn symmetric_difference(&self, other: &Self) -> Self;

    // TODO/wtemple
    // pub fn intersection(&self, other: &Self) -> Self;

    // TODO/wtemple
    // pub fn union(&self, other: &Self) -> Self;
}
