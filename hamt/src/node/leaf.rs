use core::hash::Hash;

use crate::{
    config::Kvp,
    node::{util::MAX_LEVEL, InteriorNode},
    HamtConfig,
};

use super::{util::HashCode, CollisionNode, HamtNode, NodeHeader, NodeType};

use core::ptr::write;

#[repr(C)]
pub struct LeafNode<K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    pub(crate) _header: NodeHeader<K, V, Config>,
    pub(crate) entry: Config::Kvp,
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> LeafNode<K, V, Config> {
    pub(crate) fn hash(&self) -> HashCode {
        self._header.hash
    }

    pub fn get(&self, key: &K) -> Option<&Config::Kvp> {
        if self.entry.key() == key {
            Some(&self.entry)
        } else {
            None
        }
    }

    pub fn remove(&self, cfg: &Config, key: &K) -> Option<Config::NodeStore> {
        if self.entry.key() == key {
            None
        } else {
            Some(cfg.upgrade_ref(self))
        }
    }

    pub fn insert(&self, cfg: &Config, key: K, value: V, hash: HashCode) -> Config::NodeStore {
        if hash == self.hash() {
            if &key == self.entry.key() {
                // Hard collision, just return a new leaf node.
                Self::create_with_pair(cfg, key, value, hash)
            } else {
                // Soft collision, create a new collision node.
                cfg.allocate::<CollisionNode<K, V, Config>>(2, |collision| unsafe {
                    write(
                        &mut collision._header,
                        NodeHeader::new::<CollisionNode<K, V, Config>>(0, 2, hash),
                    );
                    write(&mut collision.values[0], self.entry.clone());
                    write(&mut collision.values[1], cfg.new_kvp(key, value));
                })
            }
        } else {
            InteriorNode::<K, V, Config>::reparent(
                cfg,
                cfg.upgrade_ref(self),
                LeafNode::<K, V, Config>::create_with_pair(cfg, key, value, hash),
            )
        }
    }

    pub fn create_with_pair(cfg: &Config, key: K, value: V, hash: HashCode) -> Config::NodeStore {
        cfg.allocate::<Self>((), |collision| unsafe {
            write(
                &mut collision._header,
                NodeHeader::new::<Self>(MAX_LEVEL, 1, hash),
            );

            debug_assert!(
                (&collision.entry as *const _ as *const () as usize)
                    % core::mem::align_of::<Config::Kvp>()
                    == 0
            );

            write(&mut collision.entry, cfg.new_kvp(key, value));
        })
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> HamtNode<K, V, Config> for LeafNode<K, V, Config> {
    const TAG: NodeType = NodeType::Leaf;

    fn header(&self) -> &NodeHeader<K, V, Config> {
        &self._header
    }
}
