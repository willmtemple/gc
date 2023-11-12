use core::{
    borrow::Borrow,
    fmt::{self, Debug, Formatter},
    hash::{BuildHasher, Hash},
    ops::Index,
};

use crate::{
    config::{DefaultConfig, HamtConfig, Kvp},
    iter::HamtIterator,
    node::{util::HashCode, LeafNode},
};

pub struct HamtMap<K: Eq + Hash, V, Config: HamtConfig<K, V> = DefaultConfig> {
    config: Config,
    root: Option<Config::NodeStore>,
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V> + Default> HamtMap<K, V, Config> {
    pub fn new() -> Self {
        Self::new_with_config(Default::default())
    }
}

impl<K: Eq + Hash, V> Default for HamtMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Debug for HamtMap<K, V, Config> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "HamtMap {{ ")?;
        if let Some(root) = self.root.as_ref() {
            write!(f, "@{:p}", root)?;
        } else {
            write!(f, "<empty>")?;
        }
        write!(f, " }}")
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Clone for HamtMap<K, V, Config> {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            root: self.root.clone(),
        }
    }
}

impl<K, Q, V, Config: HamtConfig<K, V>> Index<&Q> for HamtMap<K, V, Config>
where
    K: Eq + Hash,
    Q: Borrow<K> + ?Sized,
{
    type Output = V;

    fn index(&self, key: &Q) -> &Self::Output {
        self.get(key.borrow()).expect("no entry found for key")
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> HamtMap<K, V, Config> {
    pub const fn new_with_config(config: Config) -> Self {
        Self { config, root: None }
    }

    /// Get a value from the map for a given key, if it exists.
    pub fn get(&self, key: &K) -> Option<&V> {
        let hash = self.config.build_hasher().hash_one(key) as HashCode;

        self.root
            .as_ref()
            .and_then(|root| root.get(key, hash))
            .map(|kvp| kvp.value())
    }

    /// Persistent insert.
    ///
    /// Returns a new HAMT with the inserted key-value pair.
    pub fn insert(&self, key: K, value: V) -> Self {
        let hash = self.config.build_hasher().hash_one(&key) as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                config: self.config.clone(),
                root: Some(root.insert(&self.config, key, value, hash)),
            },
            None => Self {
                config: self.config.clone(),
                root: Some(LeafNode::<K, V, Config>::create_with_pair(
                    &self.config,
                    key,
                    value,
                    hash,
                )),
            },
        }
    }

    /// Persistent remove.
    ///
    /// Returns a new HAMT with the given key removed.
    pub fn remove(&self, key: &K) -> Self {
        let hash = self.config.build_hasher().hash_one(key) as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                config: self.config.clone(),
                root: root.remove(&self.config, key, hash),
            },
            None => Self {
                config: self.config.clone(),
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

impl<K: Eq + Hash, V: Eq, Config: HamtConfig<K, V>> Eq for HamtMap<K, V, Config> {}

impl<K: Eq + Hash, V: Eq, Config: HamtConfig<K, V>> PartialEq for HamtMap<K, V, Config> {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl<K: Eq + Hash, V: Hash, Config: HamtConfig<K, V>> Hash for HamtMap<K, V, Config> {
    fn hash<H: core::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

impl<K: Eq + Hash, V: Hash, Config: HamtConfig<K, V> + Default> FromIterator<(K, V)>
    for HamtMap<K, V, Config>
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut map = Self::new_with_config(Config::default());

        for (k, v) in iter {
            map = map.insert(k, v);
        }

        map
    }
}
