mod collision;
pub use collision::CollisionNode;

mod interior;
pub use interior::InteriorNode;

mod leaf;
pub use leaf::LeafNode;

use core::hash::Hash;
use core::marker::PhantomData;

use crate::config::HamtConfig;

use util::HashCode;

pub mod util {
    #[cfg(target_pointer_width = "64")]
    pub type HashCode = u64;

    #[cfg(target_pointer_width = "64")]
    pub type Bitmap = u64;

    // Number of bits required to index the bits of bitmap (i.e. 6 on 64-bit systems, 5 on 32-bit systems)
    pub const BITMAP_INDEX_BITS: usize = (core::mem::size_of::<Bitmap>() * 8).ilog2() as usize;
    pub const BITMAP_MASK: Bitmap = (1 << BITMAP_INDEX_BITS) - 1;
    pub const MAX_LEVEL: usize = (core::mem::size_of::<Bitmap>() * 8 / BITMAP_INDEX_BITS) + 1;

    pub const LEVEL_MUL_TABLE: [usize; MAX_LEVEL] = [
        0,
        BITMAP_INDEX_BITS,
        2 * BITMAP_INDEX_BITS,
        3 * BITMAP_INDEX_BITS,
        4 * BITMAP_INDEX_BITS,
        5 * BITMAP_INDEX_BITS,
        6 * BITMAP_INDEX_BITS,
        7 * BITMAP_INDEX_BITS,
        8 * BITMAP_INDEX_BITS,
        9 * BITMAP_INDEX_BITS,
        10 * BITMAP_INDEX_BITS,
    ];

    pub fn hash_bits_for_level(hash: HashCode, level: usize) -> Bitmap {
        debug_assert!(level < MAX_LEVEL);

        let inverse_level = MAX_LEVEL - level - 1;

        (hash >> LEVEL_MUL_TABLE[inverse_level]) & BITMAP_MASK
    }
}

pub trait HamtNode<K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    const TAG: NodeType;

    fn header(&self) -> &NodeHeader<K, V, Config>;
}

#[repr(u8)]
#[non_exhaustive]
#[derive(Eq, PartialEq)]
pub enum NodeType {
    Leaf = 1,
    Interior = 2,
    Collision = 3,

    _Header = 0,
}

impl NodeType {
    fn from_u8(n: u8) -> Self {
        debug_assert_eq!(0b11 & n, n);

        match n {
            0 => Self::_Header,
            1 => Self::Leaf,
            2 => Self::Interior,
            3 => Self::Collision,
            _ => panic!("Unknown node type 0b{:02b}", n),
        }
    }
}

#[cfg(target_pointer_width = "64")]
pub struct NodeHeader<K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    _ph: PhantomData<(K, V, Config)>,
    packed: usize,
    pub(crate) hash: HashCode,
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Clone for NodeHeader<K, V, Config> {
    fn clone(&self) -> Self {
        Self {
            _ph: PhantomData,
            packed: self.packed,
            hash: self.hash,
        }
    }
}

#[cfg(target_pointer_width = "64")]
impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> NodeHeader<K, V, Config> {
    const TAG_MASK: usize = 0b11 << 62;
    const LEVEL_MASK: usize = 0b1111 << 58;
    const SIZE_MASK: usize = !(Self::TAG_MASK | Self::LEVEL_MASK);

    pub(crate) unsafe fn new<T: HamtNode<K, V, Config> + ?Sized>(
        level: usize,
        size: usize,
        hash: HashCode,
    ) -> Self {
        debug_assert!(size <= Self::SIZE_MASK);
        debug_assert!(level <= 0b1111);

        Self {
            _ph: PhantomData,
            packed: ((T::TAG as usize) << 62) | ((level & 0b1111) << 58) | (size & !Self::TAG_MASK),
            hash,
        }
    }

    fn tag(&self) -> NodeType {
        NodeType::from_u8(((self.packed & Self::TAG_MASK) >> 62) as u8)
    }

    pub(crate) fn level(&self) -> usize {
        (self.packed & Self::LEVEL_MASK) >> 58
    }

    fn size(&self) -> usize {
        self.packed & Self::SIZE_MASK
    }

    pub fn get(&self, k: &K, hash: HashCode) -> Option<&Config::Kvp> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.get(k),
            NodePtr::Collision(node) => node.get(k, hash),
            NodePtr::Interior(node) => node.get(k, hash),
        }
    }

    pub fn insert(&self, cfg: &Config, k: K, v: V, hash: HashCode) -> Config::NodeStore {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.insert(cfg, k, v, hash),
            NodePtr::Collision(node) => node.insert(cfg, k, v, hash),
            NodePtr::Interior(node) => node.insert(cfg, k, v, hash),
        }
    }

    pub fn remove(&self, cfg: &Config, k: &K, hash: HashCode) -> Option<Config::NodeStore> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.remove(cfg, k),
            NodePtr::Collision(node) => node.remove(cfg, k, hash),
            NodePtr::Interior(node) => node.remove(cfg, k, hash),
        }
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Drop for NodeHeader<K, V, Config> {
    fn drop(&mut self) {
        match self.upgrade_mut() {
            NodePtrMut::Leaf(l) => {
                unsafe { core::ptr::drop_in_place(&mut l.entry) };
            }
            NodePtrMut::Collision(l) => {
                for kvp in l.values.iter_mut() {
                    unsafe { core::ptr::drop_in_place(kvp) };
                }
            }
            NodePtrMut::Interior(i) => {
                for child in i.children.iter_mut() {
                    unsafe { core::ptr::drop_in_place(child) };
                }
            }
        }
    }
}

pub enum NodePtr<'a, K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    Leaf(&'a LeafNode<K, V, Config>),
    Collision(&'a CollisionNode<K, V, Config>),
    Interior(&'a InteriorNode<K, V, Config>),
}

pub enum NodePtrMut<'a, K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    Leaf(&'a mut LeafNode<K, V, Config>),
    Collision(&'a mut CollisionNode<K, V, Config>),
    Interior(&'a mut InteriorNode<K, V, Config>),
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> NodeHeader<K, V, Config> {
    pub fn upgrade(&self) -> NodePtr<K, V, Config> {
        match self.tag() {
            NodeType::Leaf => NodePtr::Leaf(unsafe {
                &*core::ptr::from_raw_parts(self as *const _ as *const (), ())
            }),
            NodeType::Collision => NodePtr::Collision(unsafe {
                &*core::ptr::from_raw_parts(self as *const _ as *const (), self.size())
            }),
            NodeType::Interior => NodePtr::Interior(unsafe {
                &*(core::ptr::from_raw_parts(self as *const _ as *const (), self.size()))
            }),
            NodeType::_Header => unreachable!(),
        }
    }

    fn upgrade_mut(&mut self) -> NodePtrMut<K, V, Config> {
        match self.tag() {
            NodeType::Leaf => NodePtrMut::Leaf(unsafe {
                &mut *core::ptr::from_raw_parts_mut(self as *mut _ as *mut (), ())
            }),
            NodeType::Collision => NodePtrMut::Collision(unsafe {
                &mut *core::ptr::from_raw_parts_mut(self as *mut _ as *mut (), self.size())
            }),
            NodeType::Interior => NodePtrMut::Interior(unsafe {
                &mut *(core::ptr::from_raw_parts_mut(self as *mut _ as *mut (), self.size()))
            }),
            NodeType::_Header => unreachable!(),
        }
    }
}
