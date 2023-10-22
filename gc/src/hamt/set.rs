use core::{
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use super::{
    config::{DefaultGlobal, HamtConfig},
    node::{util::HashCode, Collision},
};

#[derive(Default)]
pub struct HamtSet<
    K: Eq + Hash,
    #[cfg(feature = "std")] HamtHasher: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] HamtHasher: Hasher + Default,
    Config: HamtConfig<K, ()> = DefaultGlobal,
> {
    _ph: PhantomData<HamtHasher>,
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

impl<K: Eq + Hash, HamtHasher: Hasher + Default, Config: HamtConfig<K, ()>>
    HamtSet<K, HamtHasher, Config>
{
    pub fn has(&self, k: &K) -> bool {
        let hash = {
            let mut hasher = HamtHasher::default();
            k.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        self.root
            .as_ref()
            .and_then(|root| root.get(k, hash))
            .is_some()
    }

    pub fn insert(&self, k: K) -> Self {
        let hash = {
            let mut hasher = HamtHasher::default();
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
                root: Some(Collision::<K, (), Config>::create_with_pair(k, (), hash)),
            },
        }
    }

    pub fn remove(&self, k: &K) -> Self {
        let hash = {
            let mut hasher = HamtHasher::default();
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
}
