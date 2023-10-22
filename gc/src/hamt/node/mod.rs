mod collision;
pub use collision::Collision;

mod interior;
pub use interior::InnerNode;

use core::hash::Hash;
use core::marker::PhantomData;
use core::ptr::Pointee;

use self::util::HashCode;

use super::config::HamtConfig;

use super::util::UsizeMetadata;

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
    Inner = 2,
    Collision = 3,

    _Header = 0,
}

impl NodeType {
    fn from_u8(n: u8) -> Self {
        debug_assert_eq!(0b111 & n, n);

        match n {
            0 => Self::_Header,
            2 => Self::Inner,
            3 => Self::Collision,
            _ => panic!("Unknown node type 0b{:03b}", n),
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
    const TAG_MASK: usize = 0b111 << 61;
    const LEVEL_MASK: usize = 0b1111 << 57;
    const SIZE_MASK: usize = !(Self::TAG_MASK | Self::LEVEL_MASK);

    pub(crate) unsafe fn new<T: HamtNode<K, V, Config> + ?Sized>(
        level: usize,
        metadata: usize,
        hash: HashCode,
    ) -> Self {
        debug_assert!(metadata <= Self::SIZE_MASK);
        debug_assert!(level <= 0b1111);

        Self {
            _ph: PhantomData,
            packed: ((T::TAG as usize) << 61)
                | ((level & 0b1111) << 57)
                | (metadata & !Self::TAG_MASK),
            hash,
        }
    }

    fn tag(&self) -> NodeType {
        NodeType::from_u8(((self.packed & Self::TAG_MASK) >> 61) as u8)
    }

    pub(crate) fn level(&self) -> usize {
        (self.packed & Self::LEVEL_MASK) >> 57
    }

    fn metadata(&self) -> usize {
        self.packed & Self::SIZE_MASK
    }

    pub fn get(&self, k: &K, hash: HashCode) -> Option<&Config::Kvp> {
        match self.upgrade() {
            NodePtr::Collision(node) => node.get(k, hash),
            NodePtr::Inner(node) => node.get(k, hash),
        }
    }

    pub fn insert(&self, k: K, v: V, hash: HashCode) -> Config::NodeStore {
        match self.upgrade() {
            NodePtr::Collision(node) => node.insert(k, v, hash),
            NodePtr::Inner(node) => node.insert(k, v, hash),
        }
    }

    pub fn remove(&self, k: &K, hash: HashCode) -> Option<Config::NodeStore> {
        match self.upgrade() {
            NodePtr::Collision(node) => node.remove(k, hash),
            NodePtr::Inner(node) => node.remove(k, hash),
        }
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> HamtNode<K, V, Config>
    for NodeHeader<K, V, Config>
{
    const TAG: NodeType = NodeType::_Header;

    fn header(&self) -> &NodeHeader<K, V, Config> {
        self
    }
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> Drop for NodeHeader<K, V, Config> {
    fn drop(&mut self) {
        match self.upgrade_mut() {
            NodePtrMut::Collision(l) => {
                for kvp in l.values.iter_mut() {
                    core::mem::drop(unsafe { core::ptr::read(kvp) });
                }
            }
            NodePtrMut::Inner(i) => {
                for child in i.children.iter_mut() {
                    core::mem::drop(unsafe { core::ptr::read(child) });
                }
            }
        }
    }
}

pub enum NodePtr<'a, K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    Collision(&'a Collision<K, V, Config>),
    Inner(&'a InnerNode<K, V, Config>),
}

pub enum NodePtrMut<'a, K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    Collision(&'a mut Collision<K, V, Config>),
    Inner(&'a mut InnerNode<K, V, Config>),
}

impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> NodeHeader<K, V, Config> {
    pub fn upgrade(&self) -> NodePtr<K, V, Config>
    where
        <Collision<K, V, Config> as Pointee>::Metadata: UsizeMetadata,
        <InnerNode<K, V, Config> as Pointee>::Metadata: UsizeMetadata,
    {
        match self.tag() {
            NodeType::Collision => NodePtr::Collision(unsafe {
                &*core::ptr::from_raw_parts(
                    self as *const _ as *const (),
                    <Collision<K, V, Config> as Pointee>::Metadata::from_usize(self.metadata()),
                )
            }),
            NodeType::Inner => NodePtr::Inner(unsafe {
                &*(core::ptr::from_raw_parts(
                    self as *const _ as *const (),
                    <InnerNode<K, V, Config> as Pointee>::Metadata::from_usize(self.metadata()),
                ))
            }),
            NodeType::_Header => unreachable!(),
        }
    }

    fn upgrade_mut(&mut self) -> NodePtrMut<K, V, Config>
    where
        <Collision<K, V, Config> as Pointee>::Metadata: UsizeMetadata,
        <InnerNode<K, V, Config> as Pointee>::Metadata: UsizeMetadata,
    {
        match self.tag() {
            NodeType::Collision => NodePtrMut::Collision(unsafe {
                &mut *core::ptr::from_raw_parts_mut(
                    self as *mut _ as *mut (),
                    <Collision<K, V, Config> as Pointee>::Metadata::from_usize(self.metadata()),
                )
            }),
            NodeType::Inner => NodePtrMut::Inner(unsafe {
                &mut *(core::ptr::from_raw_parts_mut(
                    self as *mut _ as *mut (),
                    <InnerNode<K, V, Config> as Pointee>::Metadata::from_usize(self.metadata()),
                ))
            }),
            NodeType::_Header => unreachable!(),
        }
    }
}
