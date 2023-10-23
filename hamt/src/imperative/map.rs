use core::hash::Hash;

use crate::{
    config::{DefaultConfig, HamtConfig},
    HamtMap,
};

#[derive(Clone)]
pub struct ImperativeHamtMap<K: Eq + Hash, V, Config: HamtConfig<K, V> = DefaultConfig> {
    hamt: HamtMap<K, V, Config>,
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> ImperativeHamtMap<K, V, Config> {
    pub fn new_with_config(config: Config) -> Self {
        Self {
            hamt: HamtMap::<K, V, Config>::new_with_config(config),
        }
    }

    pub fn as_persistent(&self) -> HamtMap<K, V, Config> {
        self.hamt.clone()
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.hamt.get(k)
    }

    pub fn insert(&mut self, k: K, v: V) -> &mut Self {
        self.hamt = self.hamt.insert(k, v);
        self
    }

    pub fn remove(&mut self, k: &K) -> &mut Self {
        self.hamt = self.hamt.remove(k);
        self
    }
}

impl<K: Eq + Hash, V> Default for ImperativeHamtMap<K, V> {
    fn default() -> Self {
        Self::new_with_config(DefaultConfig::default())
    }
}
