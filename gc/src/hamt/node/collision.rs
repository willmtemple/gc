use core::{cmp::Ordering, hash::Hash, marker::PhantomData};

use crate::hamt::{
    config::{HamtConfig, Kvp},
    node::{
        util::{hash_bits_for_level, BITMAP_INDEX_BITS, MAX_LEVEL},
        InnerNode,
    },
};

use super::{util::HashCode, HamtNode, NodeHeader, NodeType};

use core::ptr::write;

#[repr(C)]
pub struct Collision<K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    pub(crate) _header: NodeHeader<K, V, Config>,
    _ph: PhantomData<Config>,
    // path: Path,
    pub(crate) values: [Config::WrappedKvp],
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Collision<K, V, Config> {
    fn hash(&self) -> HashCode {
        self._header.hash
    }

    pub fn get(&self, key: &K, hash: HashCode) -> Option<&Config::WrappedKvp> {
        if hash != self.hash() {
            None
        } else {
            self.values.iter().find(|kvp| kvp.key() == key)
        }
    }

    pub fn insert(
        &self,
        key: K,
        value: V,
        hash: HashCode,
    ) -> Config::Pointer<NodeHeader<K, V, Config>> {
        // debug_assert!(path.level == self.path.level);
        // eprintln!(
        //     "Inserting hash {:#066b} into collision node with hash {:#066b}",
        //     hash,
        //     self.hash()
        // );
        // debug_assert_eq!(
        //     hash_bits_for_level(hash, path.level),
        //     hash_bits_for_level(self.hash, path.level)
        // );

        let leading_same_bits = (hash ^ self.hash()).leading_zeros();
        let collision = leading_same_bits == 64;

        if collision {
            self.add_pair(key, value)
        } else {
            // Not a collision, but the hashes are the same up to some level. We will return an inner node that
            // has one or both of the patterns as children.

            // NEW LOGIC
            // We find the level at which the hashes are the same, then create an InnerNode at that level.

            let next_level = if leading_same_bits < 4 {
                0
            } else {
                ((leading_same_bits - 4) / (BITMAP_INDEX_BITS as u32)) as usize + 1
            };
            let new_bits_for_next_level = hash_bits_for_level(hash, next_level);
            let self_bits_for_next_level = hash_bits_for_level(self.hash(), next_level);
            let bitmap = (1 << new_bits_for_next_level) | (1 << self_bits_for_next_level);

            // eprintln!(
            //     "Creating inner node at level {} ({} leading same bits)",
            //     next_level, leading_same_bits
            // );

            debug_assert!(next_level < MAX_LEVEL);

            let new_inner = unsafe {
                Config::allocate::<InnerNode<K, V, Config>>(2, move |inner| {
                    write(
                        &mut inner._header,
                        NodeHeader::new::<InnerNode<K, V, Config>>(next_level, 2, hash),
                    );
                    inner.bitmap = bitmap;

                    let new_node = Self::create_with_pair(key, value, hash);
                    let this_collision = Config::ptr_from_ref_reinterpret(self);

                    match new_bits_for_next_level.cmp(&self_bits_for_next_level) {
                        Ordering::Less => {
                            write(&mut inner.children[0], new_node);
                            write(&mut inner.children[1], this_collision);
                        }
                        Ordering::Greater => {
                            write(&mut inner.children[0], this_collision);
                            write(&mut inner.children[1], new_node);
                        }
                        Ordering::Equal => unreachable!(),
                    }
                })
            };

            unsafe { Config::downgrade_ptr(new_inner) }
        }
    }

    pub fn remove(
        &self,
        key: &K,
        hash: HashCode,
    ) -> Option<Config::Pointer<NodeHeader<K, V, Config>>> {
        // debug_assert_eq!(
        //     hash_bits_for_level(path.hash, path.level),
        //     hash_bits_for_level(self.hash, path.level)
        // );

        if hash != self.hash() {
            // The key is not in the map.
            return Some(unsafe { Config::ptr_from_ref_reinterpret(self) });
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
                let new_collision = unsafe {
                    Config::allocate::<Self>(self.values.len() - 1, |collision| {
                        write(
                            &mut collision._header,
                            NodeHeader::new::<Self>(MAX_LEVEL, self.values.len() - 1, hash),
                        );
                        for i in 0..self.values.len() {
                            if i != idx {
                                write(&mut collision.values[i], self.values[i].clone());
                            }
                        }
                    })
                };

                Some(unsafe { Config::downgrade_ptr(new_collision) })
            }
        } else {
            // The key is not in the collision node.
            Some(unsafe { Config::ptr_from_ref_reinterpret(self) })
        }
    }

    pub fn create_with_pair(
        key: K,
        value: V,
        hash: HashCode,
    ) -> Config::Pointer<NodeHeader<K, V, Config>> {
        unsafe {
            Config::downgrade_ptr({
                Config::allocate::<Collision<K, V, Config>>(1, |collision| {
                    write(
                        &mut collision._header,
                        NodeHeader::new::<Self>(MAX_LEVEL, 1, hash),
                    );

                    debug_assert!(
                        (&collision.values[..] as *const _ as *const () as usize)
                            % core::mem::align_of::<Config::WrappedKvp>()
                            == 0
                    );

                    let kvp = Config::wrap_kvp(key, value);

                    debug_assert!(
                        (&collision.values[0] as *const _ as *const () as usize)
                            % core::mem::align_of::<Config::WrappedKvp>()
                            == 0
                    );

                    write(&mut collision.values[0], kvp);
                })
            })
        }
    }

    fn add_pair(&self, key: K, value: V) -> Config::Pointer<NodeHeader<K, V, Config>> {
        let cur_value = self
            .values
            .iter()
            .enumerate()
            .find(|(_, kvp)| kvp.key() == &key);

        if let Some((idx, _)) = cur_value {
            unsafe {
                Config::downgrade_ptr(Config::allocate::<Self>(
                    self.values.len(),
                    move |collision| {
                        write(&mut collision._header, self._header.clone());
                        for i in 0..self.values.len() {
                            if i != idx {
                                write(&mut collision.values[i], self.values[i].clone());
                            }
                        }

                        write(&mut collision.values[idx], Config::wrap_kvp(key, value));
                    },
                ))
            }
        } else {
            let next_len = self.values.len() + 1;

            assert!(next_len <= NodeHeader::<K, V, Config>::SIZE_MASK);

            unsafe {
                Config::downgrade_ptr(Config::allocate::<Self>(self.values.len() + 1, |leaf| {
                    write(
                        &mut leaf._header,
                        NodeHeader::new::<Self>(MAX_LEVEL, self.values.len() + 1, self.hash()),
                    );
                    for i in 0..self.values.len() {
                        write(&mut leaf.values[i], self.values[i].clone());
                    }
                    write(
                        &mut leaf.values[self.values.len()],
                        Config::wrap_kvp(key, value),
                    );
                }))
            }
        }
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> HamtNode<K, V, Config> for Collision<K, V, Config> {
    const TAG: NodeType = NodeType::Collision;

    fn header(&self) -> &NodeHeader<K, V, Config> {
        let res = &self._header;
        debug_assert_eq!(self as *const _ as *const (), res as *const _ as *const ());
        res
    }
}
