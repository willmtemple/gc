use core::{
    borrow::Borrow,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Index,
};

use crate::{
    config::{DefaultConfig, HamtConfig, Kvp},
    iter::HamtIterator,
    node::{util::HashCode, LeafNode},
};

pub struct HamtMap<
    K: Eq + Hash,
    V,
    #[cfg(feature = "std")] H: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] H: Hasher + Default,
    Config: HamtConfig<K, V> = DefaultConfig,
> {
    _ph: PhantomData<(K, V, Config, H)>,
    root: Option<Config::NodeStore>,
}

impl<K: Eq + Hash, V, H: Hasher + Default, Config: HamtConfig<K, V>> Default
    for HamtMap<K, V, H, Config>
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash, V, H: Hasher + Default, Config: HamtConfig<K, V>> Clone
    for HamtMap<K, V, H, Config>
{
    fn clone(&self) -> Self {
        Self {
            _ph: PhantomData,
            root: self.root.clone(),
        }
    }
}

impl<K, Q, V, H: Hasher + Default, Config: HamtConfig<K, V>> Index<&Q> for HamtMap<K, V, H, Config>
where
    K: Eq + Hash,
    Q: Borrow<K> + ?Sized,
{
    type Output = V;

    fn index(&self, key: &Q) -> &Self::Output {
        self.get(key.borrow()).expect("no entry found for key")
    }
}

impl<K: Eq + Hash, V, H: Hasher + Default, Config: HamtConfig<K, V>> HamtMap<K, V, H, Config> {
    pub fn new() -> Self {
        Self {
            _ph: PhantomData,
            root: None,
        }
    }

    /// Get a value from the map for a given key, if it exists.
    pub fn get(&self, key: &K) -> Option<&V> {
        let hash = {
            let mut hasher = H::default();
            key.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        self.root
            .as_ref()
            .and_then(|root| root.get(key, hash))
            .map(|kvp| kvp.value())
    }

    /// Persistent insert.
    ///
    /// Returns a new HAMT with the inserted key-value pair.
    pub fn insert(&self, key: K, value: V) -> Self {
        let hash = {
            let mut hasher = H::default();
            key.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                _ph: PhantomData,
                root: Some(root.insert(key, value, hash)),
            },
            None => Self {
                _ph: PhantomData,
                root: Some(LeafNode::<K, V, Config>::create_with_pair(key, value, hash)),
            },
        }
    }

    /// Persistent remove.
    ///
    /// Returns a new HAMT with the given key removed.
    pub fn remove(&self, key: &K) -> Self {
        let hash = {
            let mut hasher = H::default();
            key.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                _ph: PhantomData,
                root: root.remove(key, hash),
            },
            None => Self {
                _ph: PhantomData,
                root: None,
            },
        }
    }

    /// Iterate over the key-value pairs in the map.
    ///
    /// The order of iteration is not guaranteed (it is determined by the hashing function and order of insertion for
    /// collisions).
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        HamtIterator::<K, V, Config>::new(self.root.clone())
    }
}
