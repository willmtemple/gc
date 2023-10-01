// Persistent Hash Array Mapped Trie

use core::{
    alloc::Layout,
    cmp::Ordering,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Deref,
    ptr::{null, Pointee},
};

use alloc::{alloc::Global, sync::Arc};

use crate::obj::SizedMetadata;

#[cfg(target_pointer_width = "64")]
type HashCode = u64;

#[cfg(target_pointer_width = "32")]
type HashCode = u32;

pub trait HamtAllocator: Copy {
    type Ptr<T: HamtNode<Self> + ?Sized>: Clone + Deref<Target = T>;

    type WrappedKvp<K: Eq + Hash, V>: Clone + Deref<Target = (K, V)>;

    /// Wraps a key-value pair.
    fn wrap_kvp<K: Eq + Hash, V>(k: K, v: V) -> Self::WrappedKvp<K, V>;

    /// Allocate a region of memory for a node of type `T` with the given `metadata`.
    ///
    /// # Safety
    /// This function must implement manual memory allocation and initialization.
    ///
    /// Returning a pointer to memory that has not been initialized to zero will cause
    /// undefinded behavior.
    ///
    /// Failing to honor the `metadata` parameter will cause undefined behavior.
    unsafe fn allocate<T: HamtNode<Self> + ?Sized>(
        metadata: <T as Pointee>::Metadata,
        f: impl FnOnce(&mut T),
    ) -> Self::Ptr<T>;

    /// Downgrade a pointer to a HAMT node into a pointer to a Node header.
    ///
    /// # Safety
    ///
    /// This amounts to transmutation.
    unsafe fn downgrade_ptr<T: HamtNode<Self> + ?Sized>(
        ptr: Self::Ptr<T>,
    ) -> Self::Ptr<NodeHeader<Self>>;

    /// Upgrade a ref to a HAMT node into a pointer to a Node header.
    ///
    /// # Safety
    ///
    /// This amounts to transmutation. This function yields undefined behavior if the underlying ref
    /// was not taken from a pointer that was obtained by calling `deref` on the result of `downgrade_ptr`.
    unsafe fn upgrade_ref<T: HamtNode<Self> + ?Sized>(ptr: &T) -> Self::Ptr<NodeHeader<Self>>;
}

pub trait HamtNode<Alloc: HamtAllocator> {
    const TAG: NodeType;

    fn header(&self) -> &NodeHeader<Alloc>;
}

#[derive(Clone, Copy)]
pub struct DefaultGlobal;

impl HamtAllocator for DefaultGlobal {
    type Ptr<T: HamtNode<Self> + ?Sized> = Arc<T>;

    type WrappedKvp<K: Eq + Hash, V> = Arc<(K, V)>;

    fn wrap_kvp<K: Eq + Hash, V>(k: K, v: V) -> Self::WrappedKvp<K, V> {
        Arc::new((k, v))
    }

    unsafe fn allocate<T: HamtNode<Self> + ?Sized>(
        metadata: <T as Pointee>::Metadata,
        init: impl FnOnce(&mut T),
    ) -> Self::Ptr<T> {
        let layout =
            unsafe { Layout::for_value_raw(core::ptr::from_raw_parts::<T>(null(), metadata)) };

        // This is _HORRIBLE_ because as far as I can tell there is no way to allocate
        // an arc uninit with a layout. I have to create a slice that has at least as much memory as needed.

        debug_assert!(core::mem::align_of::<usize>() == layout.align());

        // Round size up to the nearest usize alignment bytes to account for alignment.
        let size = layout.size();
        let pad_bytes = size % core::mem::size_of::<usize>();
        let size = size
            + if pad_bytes != 0 {
                core::mem::size_of::<usize>() - pad_bytes
            } else {
                0
            };

        debug_assert!(size >= layout.size());
        debug_assert!((size - layout.size()) < core::mem::align_of::<usize>());

        let len = size / core::mem::size_of::<usize>();

        let data = {
            let arc = Arc::<[usize]>::new_zeroed_slice_in(len, Global);
            let ptr = core::ptr::from_raw_parts_mut::<T>(Arc::into_raw(arc) as *mut (), metadata);

            // Check the ptr alignment matches the layout alignment
            debug_assert!((ptr as *const () as usize) % layout.align() == 0);

            ptr
        };

        init(unsafe { &mut *data });

        unsafe { Arc::from_raw(data) }
    }

    unsafe fn downgrade_ptr<T: HamtNode<Self> + ?Sized>(
        ptr: Self::Ptr<T>,
    ) -> Self::Ptr<NodeHeader<Self>> {
        unsafe {
            let raw = ptr.header() as *const _ as *const NodeHeader<Self>;
            Arc::increment_strong_count(raw);
            Arc::from_raw(raw)
        }
    }

    unsafe fn upgrade_ref<T: HamtNode<Self> + ?Sized>(ptr: &T) -> Self::Ptr<NodeHeader<Self>> {
        unsafe {
            let raw = ptr.header() as *const _ as *const NodeHeader<Self>;
            Arc::increment_strong_count(raw);
            Arc::from_raw(raw)
        }
    }
}

#[derive(Default, Clone)]
pub struct Hamt<
    K: Eq + Hash,
    V,
    #[cfg(feature = "std")] HamtHasher: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] HamtHasher: Hasher + Default,
    Alloc: HamtAllocator = DefaultGlobal,
> {
    _ph: PhantomData<(K, V, Alloc, HamtHasher)>,
    root: Option<Alloc::Ptr<NodeHeader<Alloc>>>,
}

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default> Hamt<K, V, HamtHasher> {
    pub fn new() -> Self {
        Self::with_allocator(DefaultGlobal)
    }
}

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default, Alloc: HamtAllocator>
    Hamt<K, V, HamtHasher, Alloc>
{
    fn with_allocator(_: Alloc) -> Self {
        Self {
            _ph: PhantomData,
            root: None,
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        let hash = {
            let mut hasher = HamtHasher::default();
            key.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        let mut level = 0;
        let mut node_cursor = self.root.as_ref();

        while let Some(node) = node_cursor {
            match node.deref().upgrade::<K, V>() {
                NodePtr::Leaf(leaf) => return leaf.get(key, hash),
                NodePtr::Inner(node) => {
                    node_cursor = node.get_child(hash, level);
                    level += 1;
                }
            }
        }

        None
    }

    /// Persistent insert.
    ///
    /// Returns a new HAMT with the inserted key-value pair.
    pub fn insert(&self, key: K, value: V) -> Self {
        let hash = {
            let mut hasher = HamtHasher::default();
            key.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                _ph: PhantomData,
                root: Some(root.insert(key, value, Path { hash, level: 0 })),
            },
            None => Self {
                _ph: PhantomData,
                root: Some(LeafNode::<K, V, Alloc>::create_with_pair(
                    key,
                    value,
                    Path { hash, level: 0 },
                )),
            },
        }
    }

    #[cfg(feature = "std")]
    pub fn print(&self)
    where
        K: core::fmt::Debug,
        V: core::fmt::Debug,
    {
        if self.root.is_none() {
            println!("Empty HAMT.");
            return;
        }

        print_node::<K, V, Alloc>(self.root.as_ref().unwrap(), 0);

        fn print_node<
            K: Eq + Hash + core::fmt::Debug,
            V: core::fmt::Debug,
            Alloc: HamtAllocator,
        >(
            node: &Alloc::Ptr<NodeHeader<Alloc>>,
            level: usize,
        ) {
            let spaces = " ".repeat(level * 2);

            match node.deref().upgrade::<K, V>() {
                NodePtr::Leaf(leaf) => {
                    println!("{}- Leaf (hash: {:#066b}):", spaces, leaf.hash);
                    for kvp in leaf.values.iter() {
                        let (k, v) = kvp.deref();
                        println!("{}  - {:?}: {:?}", spaces, k, v);
                    }
                }
                NodePtr::Inner(node) => {
                    println!("{}- Node: {:#066b}", spaces, node.bitmap);
                    for child in node.children.iter() {
                        print_node::<K, V, Alloc>(child, level + 1);
                    }
                }
            }
        }
    }
}

#[repr(u8)]
#[derive(Eq, PartialEq)]
pub enum NodeType {
    Leaf = 0,
    Inner = 1,

    _Header = 0xFF,
}

impl NodeType {
    const fn from_u8(n: u8) -> Self {
        match n {
            0 => Self::Leaf,
            1 => Self::Inner,
            _ => unreachable!(),
        }
    }
}

#[cfg(target_pointer_width = "32")]
#[derive(Clone, Copy)]
struct NodeHeader {
    metadata: usize,
    tag: NodeType,
}

#[cfg(target_pointer_width = "32")]
impl NodeHeader {
    const fn new(tag: NodeType, size: usize) -> Self {
        Self { tag, size }
    }

    fn tag(&self) -> NodeType {
        self.tag
    }

    fn size(&self) -> usize {
        self.metadata
    }
}

#[cfg(target_pointer_width = "64")]
#[derive(Clone, Copy)]
pub struct NodeHeader<Alloc: HamtAllocator> {
    _ph: PhantomData<Alloc>,
    tag_size: usize,
}

#[cfg(target_pointer_width = "64")]
impl<Alloc: HamtAllocator> NodeHeader<Alloc> {
    const TAG_MASK: usize = 0b111 << 61;

    unsafe fn new<T: HamtNode<Alloc> + ?Sized>(metadata: usize) -> Self {
        debug_assert!(metadata <= !Self::TAG_MASK);

        Self {
            _ph: PhantomData,
            tag_size: ((T::TAG as usize) << 61) | (metadata & !Self::TAG_MASK),
        }
    }

    fn tag(&self) -> NodeType {
        NodeType::from_u8(((self.tag_size & Self::TAG_MASK) >> 61) as u8)
    }

    fn metadata(&self) -> usize {
        self.tag_size & !Self::TAG_MASK
    }

    fn insert<K: Eq + Hash, V>(&self, k: K, v: V, path: Path) -> Alloc::Ptr<NodeHeader<Alloc>> {
        match self.upgrade::<K, V>() {
            NodePtr::Leaf(node) => node.insert(k, v, path),
            NodePtr::Inner(node) => node.insert(k, v, path),
        }
    }
}

impl<Alloc: HamtAllocator> HamtNode<Alloc> for NodeHeader<Alloc> {
    const TAG: NodeType = NodeType::_Header;

    fn header(&self) -> &NodeHeader<Alloc> {
        self
    }
}

enum NodePtr<'a, K: Eq + Hash, V, Alloc: HamtAllocator> {
    Leaf(&'a LeafNode<K, V, Alloc>),
    Inner(&'a InnerNode<Alloc>),
}

impl<Alloc: HamtAllocator> NodeHeader<Alloc> {
    fn upgrade<K: Eq + Hash, V>(&self) -> NodePtr<K, V, Alloc>
    where
        <LeafNode<K, V, Alloc> as Pointee>::Metadata: SizedMetadata,
        <InnerNode<Alloc> as Pointee>::Metadata: SizedMetadata,
    {
        match self.tag() {
            NodeType::Leaf => NodePtr::Leaf(unsafe {
                &*core::ptr::from_raw_parts(
                    self as *const _ as *const (),
                    <LeafNode<K, V, Alloc> as Pointee>::Metadata::from_usize(self.metadata()),
                )
            }),
            NodeType::Inner => NodePtr::Inner(unsafe {
                &*(core::ptr::from_raw_parts(
                    self as *const _ as *const (),
                    <InnerNode<Alloc> as Pointee>::Metadata::from_usize(self.metadata()),
                ))
            }),
            NodeType::_Header => unreachable!(),
        }
    }

    // unsafe fn reinterpret_mut<T: HamtNode<Alloc> + ?Sized>(&mut self) -> &mut T
    // where
    //     <T as Pointee>::Metadata: SizedMetadata,
    // {
    //     debug_assert!(self.tag() == T::TAG);

    //     &mut *(core::ptr::from_raw_parts_mut(
    //         self as *mut _ as *mut (),
    //         <T as Pointee>::Metadata::from_usize(self.metadata()),
    //     ))
    // }
}

#[cfg(target_pointer_width = "64")]
type Bitmap = u64;
#[cfg(target_pointer_width = "32")]
type Bitmap = u32;

// Number of bits required to index the bits of bitmap (i.e. 6 on 64-bit systems, 5 on 32-bit systems)
const BITMAP_INDEX_BITS: usize = (core::mem::size_of::<Bitmap>() * 8).ilog2() as usize;
const BITMAP_MASK: Bitmap = (1 << BITMAP_INDEX_BITS) - 1;
const MAX_LEVEL: usize = core::mem::size_of::<Bitmap>() * 8 / BITMAP_INDEX_BITS;

const LEVEL_MUL_TABLE: [usize; 11] = [
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

const fn hash_bits_for_level(hash: HashCode, level: usize) -> Bitmap {
    debug_assert!(level > 0);
    debug_assert!(level <= MAX_LEVEL);
    const SIZE_BITS: usize = core::mem::size_of::<HashCode>() * 8;
    let shift = SIZE_BITS - LEVEL_MUL_TABLE[level];

    (hash >> (shift)) & BITMAP_MASK
}

const fn hash_masked_for_level(hash: HashCode, level: usize) -> HashCode {
    const SIZE_BITS: usize = core::mem::size_of::<HashCode>() * 8;
    let n = level * BITMAP_INDEX_BITS;

    let high_n_bits_mask = if level == 0 {
        0
    } else {
        !((1 << (SIZE_BITS - n)) - 1)
    };

    hash & high_n_bits_mask
}

#[derive(Copy, Clone)]
struct Path {
    hash: HashCode,
    level: usize,
}

impl Path {
    fn increment(self) -> Self {
        Self {
            hash: self.hash,
            level: self.level + 1,
        }
    }
}

#[repr(C)]
struct InnerNode<Alloc: HamtAllocator> {
    _header: NodeHeader<Alloc>,
    bitmap: Bitmap,
    children: [Alloc::Ptr<NodeHeader<Alloc>>],
}

impl<Alloc: HamtAllocator> InnerNode<Alloc> {
    fn get_child(&self, hash: HashCode, level: usize) -> Option<&Alloc::Ptr<NodeHeader<Alloc>>> {
        debug_assert!(level <= MAX_LEVEL);

        let index = hash_bits_for_level(hash, level + 1);

        let masked_bitmap = ((1 << index) - 1) & self.bitmap;

        let pop = masked_bitmap.count_ones() as usize;

        if pop >= self.children.len() {
            None
        } else {
            Some(&self.children[pop as usize])
        }
    }

    fn insert<K: Eq + Hash, V>(
        &self,
        key: K,
        value: V,
        path: Path,
    ) -> Alloc::Ptr<NodeHeader<Alloc>> {
        debug_assert!(path.level <= MAX_LEVEL);

        let index = hash_bits_for_level(path.hash, path.level + 1);

        let occupied = (self.bitmap >> index & 1) == 1;
        let masked_bitmap = ((1 << index) - 1) & self.bitmap;
        let pop = masked_bitmap.count_ones() as usize;

        let next_path = path.increment();

        if occupied {
            // Allocate a new InnerNode with the `pop`th child replaced by the result
            // of inserting k, v into the child.
            let new_child = self.children[pop].insert(key, value, next_path);

            let new_inner = unsafe {
                Alloc::allocate::<Self>(self.children.len(), |inner| {
                    inner._header = self._header;
                    inner.bitmap = self.bitmap;
                    for (i, child) in self.children.iter().enumerate() {
                        if i.cmp(&pop) != Ordering::Equal {
                            core::mem::forget(core::mem::replace(
                                &mut inner.children[i],
                                child.clone(),
                            ));
                        }
                    }

                    // We do this at the end to avoid requiring new_child to be copy.
                    core::mem::forget(core::mem::replace(&mut inner.children[pop], new_child));
                })
            };

            unsafe { Alloc::downgrade_ptr(new_inner) }
        } else {
            // Allocate a new InnerNode with a new bitmap with the `index`th bit set,
            // and with a new single-value leaf node allocated with (k, v) at index `pop`.

            let new_leaf = LeafNode::<K, V, Alloc>::create_with_pair(key, value, next_path);

            let new_bitmap = self.bitmap | (1 << index);

            let new_inner = unsafe {
                Alloc::allocate::<Self>(self.children.len() + 1, move |inner| {
                    inner._header = NodeHeader::new::<Self>(self.children.len() + 1);
                    inner.bitmap = new_bitmap;
                    for i in 0..self.children.len() + 1 {
                        match i.cmp(&pop) {
                            Ordering::Less => {
                                core::mem::forget(core::mem::replace(
                                    &mut inner.children[i],
                                    self.children[i].clone(),
                                ));
                            }
                            Ordering::Greater => {
                                core::mem::forget(core::mem::replace(
                                    &mut inner.children[i],
                                    self.children[i - 1].clone(),
                                ));
                            }
                            Ordering::Equal => {}
                        }
                    }

                    // We do this at the end to avoid requiring new_leaf to be copy.
                    core::mem::forget(core::mem::replace(&mut inner.children[pop], new_leaf));
                })
            };

            unsafe { Alloc::downgrade_ptr(new_inner) }
        }
    }
}

impl<Alloc: HamtAllocator> HamtNode<Alloc> for InnerNode<Alloc> {
    const TAG: NodeType = NodeType::Inner;

    fn header(&self) -> &NodeHeader<Alloc> {
        &self._header
    }
}

#[repr(C)]
struct LeafNode<K: Eq + Hash, V, Alloc: HamtAllocator> {
    _header: NodeHeader<Alloc>,
    _ph: PhantomData<Alloc>,
    hash: HashCode,
    // path: Path,
    values: [Alloc::WrappedKvp<K, V>],
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator> LeafNode<K, V, Alloc> {
    fn get(&self, key: &K, hash: HashCode) -> Option<&V> {
        if hash != self.hash {
            None
        } else {
            self.values
                .iter()
                .find(|kvp| kvp.0 == *key)
                .map(|kvp| &kvp.1)
        }
    }

    fn insert(&self, key: K, value: V, path: Path) -> Alloc::Ptr<NodeHeader<Alloc>> {
        debug_assert!(path.level <= MAX_LEVEL);
        // debug_assert!(path.level == self.path.level);
        debug_assert_eq!(
            hash_masked_for_level(path.hash, path.level),
            hash_masked_for_level(self.hash, path.level)
        );

        if path.hash == self.hash {
            // collision
            unsafe { self.add_pair(key, value) }
        } else {
            // Not a collision, but the hashes are the same up to this level. We will return an inner node that
            // has one or both of the patterns as children.

            let path_bits_for_next_level = hash_masked_for_level(path.hash, path.level + 1);
            let self_bits_for_next_level = hash_masked_for_level(self.hash, path.level + 1);

            let path_next_level_bitmap = 1 << hash_bits_for_level(path.hash, path.level + 1);

            let same_next_path = path_bits_for_next_level == self_bits_for_next_level;

            let (size, bitmap) = if same_next_path {
                (1, path_next_level_bitmap)
            } else {
                (
                    2,
                    path_next_level_bitmap | (1 << hash_bits_for_level(self.hash, path.level + 1)),
                )
            };

            let new_inner = unsafe {
                Alloc::allocate::<InnerNode<Alloc>>(size, move |inner| {
                    inner._header = NodeHeader::new::<InnerNode<Alloc>>(size);
                    inner.bitmap = bitmap;

                    match size {
                        1 => {
                            core::mem::forget(core::mem::replace(
                                &mut inner.children[0],
                                // simply retry the insert at the next level
                                self.insert(key, value, path.increment()),
                            ));
                        }
                        2 => {
                            let new_leaf = Self::create_with_pair(key, value, path.increment());
                            let self_leaf = Alloc::upgrade_ref(self);

                            match path_bits_for_next_level.cmp(&self_bits_for_next_level) {
                                Ordering::Less => {
                                    core::mem::forget(core::mem::replace(
                                        &mut inner.children[0],
                                        new_leaf,
                                    ));
                                    core::mem::forget(core::mem::replace(
                                        &mut inner.children[1],
                                        self_leaf,
                                    ));
                                }
                                Ordering::Greater => {
                                    core::mem::forget(core::mem::replace(
                                        &mut inner.children[0],
                                        self_leaf,
                                    ));
                                    core::mem::forget(core::mem::replace(
                                        &mut inner.children[1],
                                        new_leaf,
                                    ));
                                }
                                Ordering::Equal => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    }
                })
            };

            unsafe { Alloc::downgrade_ptr(new_inner) }
        }
    }

    fn create_with_pair(key: K, value: V, path: Path) -> Alloc::Ptr<NodeHeader<Alloc>> {
        unsafe {
            Alloc::downgrade_ptr({
                Alloc::allocate::<LeafNode<K, V, Alloc>>(1, |leaf| {
                    leaf._header = NodeHeader::new::<LeafNode<K, V, Alloc>>(1);

                    leaf._ph = PhantomData;

                    leaf.hash = path.hash;

                    debug_assert!(
                        (&leaf.values[..] as *const _ as *const () as usize)
                            % core::mem::align_of::<Alloc::WrappedKvp<K, V>>()
                            == 0
                    );

                    let kvp = Alloc::wrap_kvp(key, value);

                    debug_assert!(
                        (&leaf.values[0] as *const _ as *const () as usize)
                            % core::mem::align_of::<Alloc::WrappedKvp<K, V>>()
                            == 0
                    );

                    core::mem::forget(core::mem::replace(&mut leaf.values[0], kvp));
                })
            })
        }
    }

    unsafe fn add_pair(&self, key: K, value: V) -> Alloc::Ptr<NodeHeader<Alloc>> {
        let cur_value = self.values.iter().enumerate().find(|(_, kvp)| kvp.0 == key);

        if let Some((idx, _)) = cur_value {
            unsafe {
                Alloc::downgrade_ptr(Alloc::allocate::<Self>(self.values.len(), move |leaf| {
                    leaf._header = self._header;
                    leaf._ph = PhantomData;
                    leaf.hash = self.hash;
                    for i in 0..self.values.len() {
                        if i != idx {
                            core::mem::forget(core::mem::replace(
                                &mut leaf.values[i],
                                self.values[i].clone(),
                            ));
                        }
                    }

                    core::mem::forget(core::mem::replace(
                        &mut leaf.values[idx],
                        Alloc::wrap_kvp(key, value),
                    ));
                }))
            }
        } else {
            unsafe {
                Alloc::downgrade_ptr(Alloc::allocate::<Self>(self.values.len() + 1, |leaf| {
                    leaf._header = NodeHeader::new::<Self>(self.values.len() + 1);
                    leaf._ph = PhantomData;
                    leaf.hash = self.hash;
                    for i in 0..self.values.len() {
                        core::mem::forget(core::mem::replace(
                            &mut leaf.values[i],
                            self.values[i].clone(),
                        ));
                    }
                    core::mem::forget(core::mem::replace(
                        &mut leaf.values[self.values.len()],
                        Alloc::wrap_kvp(key, value),
                    ));
                }))
            }
        }
    }
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator> HamtNode<Alloc> for LeafNode<K, V, Alloc> {
    const TAG: NodeType = NodeType::Leaf;

    fn header(&self) -> &NodeHeader<Alloc> {
        &self._header
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use crate::hamt::{HamtAllocator, LeafNode};

    use super::{DefaultGlobal, InnerNode};

    #[test]
    fn print_trees() {
        let hamt_mt = super::Hamt::<u64, u64>::new();

        assert!(hamt_mt.get(&0).is_none());

        let hamt_0 = hamt_mt.insert(0, 1);

        assert_eq!(hamt_mt.get(&0), None);
        assert_eq!(hamt_0.get(&0), Some(&1));

        let hamt_1 = hamt_0.insert(1, 2);

        assert_eq!(hamt_1.get(&0), Some(&1));
        assert_eq!(hamt_1.get(&1), Some(&2));
        assert_eq!(hamt_1.get(&2), None);
        assert_eq!(hamt_0.get(&1), None);
        assert_eq!(hamt_0.get(&0), Some(&1));

        let mut hamt = hamt_1.clone();

        let mut hamts = vec![hamt_0, hamt_1];

        for i in 2..100 {
            hamt = hamt.insert(i, i + 1);
            hamts.push(hamt.clone());

            assert_eq!(hamt.get(&i), Some(&(i + 1)));
        }

        for (hamt_idx, hamt) in hamts.iter().enumerate() {
            for i in 0..=hamt_idx {
                assert_eq!(hamt.get(&(i as u64)), Some(&(i as u64 + 1)));
            }
        }

        hamts[hamts.len() - 1].print();
    }

    #[test]
    fn check_align() {
        // Check that the offset of _header within the node type is 0 i.e. that a pointer
        // to the node type is also a pointer to the header.

        let data = unsafe { DefaultGlobal::allocate::<LeafNode<(), (), DefaultGlobal>>(0, |_| {}) };

        let node_ptr = data.as_ref() as *const _ as *const ();
        let node_header_ptr = &data._header as *const _ as *const ();

        assert_eq!(node_ptr as usize, node_header_ptr as usize);

        let data = unsafe { DefaultGlobal::allocate::<InnerNode<DefaultGlobal>>(0, |_| {}) };

        let node_ptr = data.as_ref() as *const _ as *const ();
        let node_header_ptr = &data._header as *const _ as *const ();

        assert_eq!(node_ptr as usize, node_header_ptr as usize);
    }
}
