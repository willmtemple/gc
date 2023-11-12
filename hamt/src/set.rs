use core::{
    fmt::{self, Formatter},
    hash::{BuildHasher, Hash},
};

use crate::{
    config::{DefaultConfig, HamtConfig, Kvp},
    iter::HamtIterator,
    node::{util::HashCode, LeafNode},
};

#[derive(Default)]
pub struct HamtSet<K: Eq + Hash, Config: HamtConfig<K, ()> = DefaultConfig> {
    // TODO/wtemple - size cannot be tracked as there is no way to know if an insert is a duplicate currently
    // or if a remove actually removed anything
    // size: usize,
    config: Config,
    root: Option<Config::NodeStore>,
}

impl<K: Eq + Hash, Config: HamtConfig<K, ()>> Clone for HamtSet<K, Config> {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            root: self.root.clone(),
        }
    }
}

use core::fmt::Debug;

impl<K: Eq + Hash, Config: HamtConfig<K, ()>> Debug for HamtSet<K, Config> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "HamtSet {{ ")?;
        if let Some(root) = self.root.as_ref() {
            write!(f, "@{:p}", root)?;
        } else {
            write!(f, "<empty>")?;
        }
        write!(f, " }}")
    }
}

impl<K: Eq + Hash> HamtSet<K> {
    pub fn new() -> Self {
        Self::new_with_config(DefaultConfig::default())
    }
}

impl<K: Eq + Hash, Config: HamtConfig<K, ()>> HamtSet<K, Config> {
    // TODO/wtemple - see above in definition of size: usize
    // pub fn len(&self) -> usize {
    //     self.size
    // }

    // pub fn is_empty(&self) -> bool {
    //     self.size == 0
    // }

    pub fn new_with_config(config: Config) -> Self {
        Self { config, root: None }
    }

    pub fn contains(&self, k: &K) -> bool {
        let hash = self.config.build_hasher().hash_one(k) as HashCode;

        self.root
            .as_ref()
            .and_then(|root| root.get(k, hash))
            .is_some()
    }

    pub fn get(&self, k: impl AsRef<K>) -> Option<&K> {
        let k = k.as_ref();
        let hash = self.config.build_hasher().hash_one(k) as HashCode;

        self.root
            .as_ref()
            .and_then(|root| root.get(k, hash))
            .map(|v| v.key())
    }

    pub fn insert(&self, k: K) -> Self {
        let hash = self.config.build_hasher().hash_one(&k) as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                config: self.config.clone(),
                root: Some(root.insert(&self.config, k, (), hash)),
            },
            None => Self {
                config: self.config.clone(),
                root: Some(LeafNode::<K, (), Config>::create_with_pair(
                    &self.config,
                    k,
                    (),
                    hash,
                )),
            },
        }
    }

    pub fn remove(&self, k: &K) -> Self {
        let hash = self.config.build_hasher().hash_one(k) as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                config: self.config.clone(),
                root: root.remove(&self.config, k, hash),
            },
            None => Self {
                config: self.config.clone(),
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

    // TODO/wtemple
    // pub fn is_disjoint(&self, other: &Self) -> bool;

    // TODO/wtemple
    // pub fn is_subset(&self, other: &Self) -> bool;

    // TODO/wtemple
    // pub fn is_superset(&self, other: &Self) -> bool;
}

impl<K: Eq + Hash, Config: HamtConfig<K, ()>> Eq for HamtSet<K, Config> {}

impl<K: Eq + Hash, Config: HamtConfig<K, ()>> PartialEq for HamtSet<K, Config> {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl<K: Eq + Hash, Config: HamtConfig<K, ()>> Hash for HamtSet<K, Config> {
    fn hash<H: core::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

impl<K: Eq + Hash, Config: HamtConfig<K, ()> + Default> FromIterator<K> for HamtSet<K, Config> {
    fn from_iter<T: IntoIterator<Item = K>>(iter: T) -> Self {
        let mut map = Self::new_with_config(Config::default());

        for k in iter {
            map = map.insert(k)
        }

        map
    }
}
