use core::hash::Hash;

use crate::{
    config::{HamtConfig, Kvp},
    node::{util::MAX_LEVEL, InteriorNode, LeafNode},
};

use super::{util::HashCode, HamtNode, NodeHeader, NodeType};

use core::ptr::write;

#[repr(C)]
pub struct CollisionNode<K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    pub(crate) _header: NodeHeader<K, V, Config>,
    pub(crate) values: [Config::Kvp],
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> CollisionNode<K, V, Config> {
    pub(crate) fn hash(&self) -> HashCode {
        self._header.hash
    }

    fn find(&self, key: &K) -> Option<(usize, &Config::Kvp)> {
        self.values
            .iter()
            .enumerate()
            .find(|(_, kvp)| kvp.key() == key)
    }

    pub fn get(&self, key: &K, hash: HashCode) -> Option<&Config::Kvp> {
        if hash != self.hash() {
            None
        } else {
            self.find(key).map(|v| v.1)
        }
    }

    pub fn insert(&self, key: K, value: V, hash: HashCode) -> Config::NodeStore {
        if hash == self.hash() {
            self.add_pair(key, value)
        } else {
            InteriorNode::<K, V, Config>::reparent(
                unsafe { Config::upgrade_ref(self) },
                LeafNode::<K, V, Config>::create_with_pair(key, value, hash),
            )
        }
    }

    pub fn remove(&self, key: &K, hash: HashCode) -> Option<Config::NodeStore> {
        if hash != self.hash() {
            // The key is not in the map.
            return Some(unsafe { Config::upgrade_ref(self) });
        }

        let cur_value = self
            .values
            .iter()
            .enumerate()
            .find(|(_, kvp)| kvp.key() == key);

        if let Some((idx, _)) = cur_value {
            if self.values.len() == 1 {
                // The key is the only key in the collision node. We can prune the collision node.
                None
            } else {
                // The key is in the collision node, but there are other keys. We can remove the key from the collision
                // node.

                Some(self.remove_idx(idx))
            }
        } else {
            // The key is not in the collision node.
            Some(unsafe { Config::upgrade_ref(self) })
        }
    }

    fn add_pair(&self, key: K, value: V) -> Config::NodeStore {
        let cur_value = self.find(&key);

        if let Some((idx, _)) = cur_value {
            // Key is present in the collision, so replace it.
            Config::allocate::<Self>(self.values.len(), move |collision| unsafe {
                write(&mut collision._header, self._header.clone());
                for i in 0..self.values.len() {
                    if i != idx {
                        write(&mut collision.values[i], self.values[i].clone());
                    }
                }

                write(&mut collision.values[idx], Config::Kvp::new(key, value));
            })
        } else {
            // Key is not present in the collision, so add it.
            let next_len = self.values.len() + 1;

            assert!(next_len <= NodeHeader::<K, V, Config>::SIZE_MASK);

            Config::allocate::<Self>(next_len, |leaf| unsafe {
                write(
                    &mut leaf._header,
                    NodeHeader::new::<Self>(MAX_LEVEL, next_len, self.hash()),
                );
                for i in 0..self.values.len() {
                    write(&mut leaf.values[i], self.values[i].clone());
                }
                write(
                    &mut leaf.values[self.values.len()],
                    Config::Kvp::new(key, value),
                );
            })
        }
    }

    fn remove_idx(&self, idx: usize) -> Config::NodeStore {
        debug_assert!(self.values.len() > 1);

        Config::allocate::<Self>(self.values.len() - 1, |collision| unsafe {
            write(
                &mut collision._header,
                NodeHeader::new::<Self>(MAX_LEVEL, self.values.len() - 1, self.hash()),
            );
            for i in 0..idx {
                write(&mut collision.values[i], self.values[i].clone());
            }
            for i in idx + 1..self.values.len() {
                write(&mut collision.values[i - 1], self.values[i].clone());
            }
        })
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> HamtNode<K, V, Config>
    for CollisionNode<K, V, Config>
{
    const TAG: NodeType = NodeType::Collision;

    fn header(&self) -> &NodeHeader<K, V, Config> {
        let res = &self._header;
        debug_assert_eq!(self as *const _ as *const (), res as *const _ as *const ());
        res
    }
}
