use core::hash::{Hash, Hasher};

use crate::hamt::{
    config::{DefaultGlobal, HamtConfig},
    HamtMap,
};

#[derive(Clone)]
pub struct ImperativeHamtMap<
    K: Eq + Hash,
    V,
    #[cfg(feature = "std")] HamtHasher: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] HamtHasher: Hasher + Default,
    Config: HamtConfig<K, V> = DefaultGlobal,
> {
    hamt: Box<HamtMap<K, V, HamtHasher, Config>>,
}

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default, Config: HamtConfig<K, V>>
    ImperativeHamtMap<K, V, HamtHasher, Config>
{
    pub fn new() -> Self {
        Self {
            hamt: Box::new(HamtMap::<K, V, HamtHasher, Config>::new()),
        }
    }

    pub fn as_persistent(&self) -> HamtMap<K, V, HamtHasher, Config> {
        (*self.hamt).clone()
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.hamt.get(k)
    }

    pub fn insert(&mut self, k: K, v: V) -> &mut Self {
        *self.hamt = self.hamt.insert(k, v);
        self
    }

    pub fn remove(&mut self, k: &K) -> &mut Self {
        *self.hamt = self.hamt.remove(k);
        self
    }
}

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default, Config: HamtConfig<K, V>> Default
    for ImperativeHamtMap<K, V, HamtHasher, Config>
{
    fn default() -> Self {
        Self::new()
    }
}
