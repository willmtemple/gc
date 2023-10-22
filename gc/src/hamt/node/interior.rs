use core::{cmp::Ordering, hash::Hash};

use crate::hamt::{
    config::HamtConfig,
    node::{
        util::{hash_bits_for_level, BITMAP_INDEX_BITS, MAX_LEVEL},
        Collision,
    },
};

use super::{
    util::{Bitmap, HashCode},
    HamtNode, NodeHeader, NodeType,
};

use core::ptr::write;

#[repr(C)]
pub struct InnerNode<K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    pub(crate) _header: NodeHeader<K, V, Config>,
    pub(crate) bitmap: Bitmap,
    pub(crate) children: [Config::NodeStore],
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> InnerNode<K, V, Config> {
    fn level(&self) -> usize {
        self._header.level()
    }

    fn hash(&self) -> HashCode {
        self._header.hash
    }

    pub fn get(&self, k: &K, hash: HashCode) -> Option<&Config::Kvp> {
        debug_assert!(self.level() <= MAX_LEVEL);
        // eprintln!(
        //     "Seeking hash {:#066b} in inner node at level {}",
        //     hash,
        //     self.level()
        // );
        let index = hash_bits_for_level(hash, self.level());

        let occupied = ((self.bitmap >> index) & 1) == 1;

        if occupied {
            let masked_bitmap = ((1 << index) - 1) & self.bitmap;
            let pop = masked_bitmap.count_ones() as usize;
            self.children[pop].get(k, hash)
        } else {
            None
        }
    }

    pub fn insert(&self, key: K, value: V, hash: HashCode) -> Config::NodeStore {
        debug_assert!(self.level() <= MAX_LEVEL);

        let leading_same_bits = (self.hash() ^ hash).leading_zeros();

        // eprintln!(
        //     "Inserting hash {:#066b} into inner node with bitmap {:#066b} and {} leading same bits",
        //     hash, self.bitmap, leading_same_bits
        // );

        let regression_level = if leading_same_bits < 4 {
            0
        } else {
            ((leading_same_bits - 4) / (BITMAP_INDEX_BITS as u32)) as usize + 1
        };

        let regresses = leading_same_bits != 64 && regression_level < self.level();

        if regresses {
            // eprintln!(
            //     "Node at level {} insertion regresses to level {} with {} leading same bits",
            //     self.level(),
            //     regression_level,
            //     leading_same_bits
            // );

            let new_bits_for_next_level = hash_bits_for_level(hash, regression_level);
            let self_bits_for_next_level = hash_bits_for_level(self.hash(), regression_level);
            let bitmap = (1 << new_bits_for_next_level) | (1 << self_bits_for_next_level);

            return Config::allocate::<Self>(2, |inner| unsafe {
                write(
                    &mut inner._header,
                    NodeHeader::new::<Self>(regression_level, 2, hash),
                );

                inner.bitmap = bitmap;

                inner.bitmap = bitmap;

                let new_node = Collision::<K, V, Config>::create_with_pair(key, value, hash);
                let this_node = Config::upgrade_ref(self);

                match new_bits_for_next_level.cmp(&self_bits_for_next_level) {
                    Ordering::Less => {
                        write(&mut inner.children[0], new_node);
                        write(&mut inner.children[1], this_node);
                    }
                    Ordering::Greater => {
                        write(&mut inner.children[0], this_node);
                        write(&mut inner.children[1], new_node);
                    }
                    Ordering::Equal => unreachable!(),
                }
            });
        }

        let index = hash_bits_for_level(hash, self.level());

        // eprintln!("Index: {}", index);

        let occupied = ((self.bitmap >> index) & 1) == 1;
        let masked_bitmap = ((1 << index) - 1) & self.bitmap;
        let pop = masked_bitmap.count_ones() as usize;

        if occupied {
            self.insert_occupied(key, value, hash, pop)
        } else {
            self.insert_vacant(key, value, hash, index, pop)
        }
    }

    fn insert_vacant(
        &self,
        key: K,
        value: V,
        hash: HashCode,
        index: u64,
        pop: usize,
    ) -> Config::NodeStore {
        // Allocate a new InnerNode with a new bitmap with the `index`th bit set,
        // and with a new single-value leaf node allocated with (k, v) at index `pop`.

        let new_leaf = Collision::<K, V, Config>::create_with_pair(key, value, hash);

        // eprintln!("Bitmap was : {:#066b}", self.bitmap);
        let new_bitmap = self.bitmap | (1 << index);
        // eprintln!("Bitmap next: {:#066b}, from index {}", new_bitmap, index);

        Config::allocate::<Self>(self.children.len() + 1, move |inner| unsafe {
            write(
                &mut inner._header,
                NodeHeader::new::<Self>(self.level(), self.children.len() + 1, hash),
            );
            inner.bitmap = new_bitmap;

            for i in 0..self.children.len() + 1 {
                match i.cmp(&pop) {
                    Ordering::Less => {
                        write(&mut inner.children[i], self.children[i].clone());
                    }
                    Ordering::Greater => {
                        write(&mut inner.children[i], self.children[i - 1].clone());
                    }
                    Ordering::Equal => {}
                }
            }

            // We do this at the end to avoid requiring new_leaf to be copy.
            write(&mut inner.children[pop], new_leaf);
        })
    }

    fn insert_occupied(&self, key: K, value: V, hash: HashCode, pop: usize) -> Config::NodeStore {
        // Allocate a new InnerNode with the `pop`th child replaced by the result
        // of inserting k, v into the child.
        let new_child = self.children[pop].insert(key, value, hash);

        Config::allocate::<Self>(self.children.len(), |inner| unsafe {
            write(&mut inner._header, self._header.clone());
            inner.bitmap = self.bitmap;
            for (i, child) in self.children.iter().enumerate() {
                if i.cmp(&pop) != Ordering::Equal {
                    write(&mut inner.children[i], child.clone());
                }
            }

            // We do this at the end to avoid requiring new_child to be copy.
            write(&mut inner.children[pop], new_child);
        })
    }

    pub fn remove(&self, key: &K, hash: HashCode) -> Option<Config::NodeStore> {
        debug_assert!(self.level() <= MAX_LEVEL);

        let index = hash_bits_for_level(hash, self.level());

        let occupied = (self.bitmap >> index & 1) == 1;

        if !occupied {
            // The key is not in the map.
            return Some(unsafe { Config::upgrade_ref(self) });
        }

        let masked_bitmap = ((1 << index) - 1) & self.bitmap;
        let pop = masked_bitmap.count_ones() as usize;

        let next = self.children[pop].remove(key, hash);

        let cur_ptr = &*self.children[pop];

        if let Some(next) = next {
            if core::ptr::eq(&*next, cur_ptr) {
                // The child was not removed.
                return Some(unsafe { Config::upgrade_ref(self) });
            }

            // The child was modified. We have a new pointer that we want to replace our current pointer with, but the
            // bitmap is unnafected.
            Some(Config::allocate::<Self>(
                self.children.len(),
                |inner| unsafe {
                    write(&mut inner._header, self._header.clone());
                    inner.bitmap = self.bitmap;

                    debug_assert_eq!(self.children.len(), inner.children.len());

                    for i in 0..inner.children.len() {
                        if i.cmp(&pop) != Ordering::Equal {
                            write(&mut inner.children[i], self.children[i].clone());
                        }
                    }

                    // We do this at the end to avoid requiring next to be copy.
                    write(&mut inner.children[pop], next);
                },
            ))
        } else {
            // The child was removed.

            // If we ended up with zero children after the removal, this node is pruned and we can return None.
            let new_len = self.children.len() - 1;
            if new_len == 0 {
                None
            } else if new_len == 1 {
                // If we ended up with one child after the removal, we can just return it.
                Some(self.children[if pop == 0 { 1 } else { 0 }].clone())
            } else {
                let new_bitmap = self.bitmap & !(1 << index);

                Some(Config::allocate::<Self>(new_len, |inner| unsafe {
                    write(
                        &mut inner._header,
                        NodeHeader::new::<Self>(self.level(), new_len, self._header.hash),
                    );
                    inner.bitmap = new_bitmap;
                    for i in 0..new_len {
                        match i.cmp(&pop) {
                            Ordering::Less => {
                                write(&mut inner.children[i], self.children[i].clone());
                            }
                            Ordering::Greater | Ordering::Equal => {
                                write(&mut inner.children[i], self.children[i + 1].clone());
                            }
                        }
                    }
                }))
            }
        }
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> HamtNode<K, V, Config> for InnerNode<K, V, Config> {
    const TAG: NodeType = NodeType::Inner;

    fn header(&self) -> &NodeHeader<K, V, Config> {
        &self._header
    }
}
