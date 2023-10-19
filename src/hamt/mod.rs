// Persistent Hash Array Mapped Trie

use core::{
    alloc::Layout,
    cmp::Ordering,
    hash::{Hash, Hasher},
    // iter::{Empty, FlatMap},
    marker::PhantomData,
    ops::Deref,
    ptr::{null, write, Pointee},
};

use alloc::{alloc::Global, boxed::Box, sync::Arc};

use crate::obj::UsizeMetadata;

#[cfg(target_pointer_width = "64")]
type HashCode = u64;

/// # Safety
///
/// This trait is somewhat intricate and must be implemented with great care to avoid memory errors, undefined behavior,
/// etc.
pub unsafe trait HamtAllocator<K: Eq + Hash, V>: Copy {
    /// The type of a pointer to a HAMT node.
    type Pointer<T: HamtNode<K, V, Self> + ?Sized>: Clone + Deref<Target = T>;

    // The type of a key-value pair stored in the HAMT.
    type WrappedKvp: Clone + Deref<Target = (K, V)>;

    /// Wraps a key-value pair.
    fn wrap_kvp(k: K, v: V) -> Self::WrappedKvp;

    /// Allocate a region of memory for a node of type `T` with the given `metadata`.
    ///
    /// # Safety
    /// This function must implement manual memory allocation and initialization.
    ///
    /// Returning a pointer to memory that has not been initialized to zero will cause
    /// undefinded behavior.
    ///
    /// Failing to honor the `metadata` parameter will cause undefined behavior.
    fn allocate<T: HamtNode<K, V, Self> + ?Sized>(
        metadata: <T as Pointee>::Metadata,
        f: impl FnOnce(&mut T),
    ) -> Self::Pointer<T>;

    /// Downgrade a pointer to a HAMT node into a pointer to a Node header.
    ///
    /// # Safety
    ///
    /// This amounts to transmutation.
    unsafe fn downgrade_ptr<T: HamtNode<K, V, Self> + ?Sized>(
        ptr: Self::Pointer<T>,
    ) -> Self::Pointer<NodeHeader<K, V, Self>>;

    /// Reinterpret a ref to a HAMT node into a pointer to a Node header and convert it to a Ptr.
    ///
    /// # Safety
    ///
    /// This amounts to transmutation. This function yields undefined behavior if the underlying ref
    /// was not taken from a pointer that was obtained by calling `deref` on the result of `downgrade_ptr`.
    unsafe fn ptr_from_ref_reinterpret<T: HamtNode<K, V, Self> + ?Sized>(
        ptr: &T,
    ) -> Self::Pointer<NodeHeader<K, V, Self>>;
}

pub trait HamtNode<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    const TAG: NodeType;

    fn header(&self) -> &NodeHeader<K, V, Alloc>;
}

#[derive(Clone, Copy)]
pub struct DefaultGlobal;

unsafe impl<K: Eq + Hash, V> HamtAllocator<K, V> for DefaultGlobal {
    type Pointer<T: HamtNode<K, V, Self> + ?Sized> = Arc<T>;

    type WrappedKvp = Arc<(K, V)>;

    fn wrap_kvp(k: K, v: V) -> Self::WrappedKvp {
        Arc::new((k, v))
    }

    fn allocate<T: HamtNode<K, V, Self> + ?Sized>(
        metadata: <T as Pointee>::Metadata,
        init: impl FnOnce(&mut T),
    ) -> Self::Pointer<T> {
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
            let thin_ptr = Arc::into_raw(arc);
            let ptr = core::ptr::from_raw_parts_mut::<T>(thin_ptr as *mut (), metadata);

            // Check the ptr alignment matches the layout alignment
            debug_assert!((ptr as *const () as usize) % layout.align() == 0);

            ptr
        };

        init(unsafe { &mut *data });

        unsafe { Arc::from_raw(data) }
    }

    unsafe fn downgrade_ptr<T: HamtNode<K, V, Self> + ?Sized>(
        ptr: Self::Pointer<T>,
    ) -> Self::Pointer<NodeHeader<K, V, Self>> {
        unsafe {
            let raw = ptr.header() as *const _ as *const NodeHeader<K, V, Self>;
            Arc::increment_strong_count(raw);
            Arc::from_raw(raw)
        }
    }

    unsafe fn ptr_from_ref_reinterpret<T: HamtNode<K, V, Self> + ?Sized>(
        ptr: &T,
    ) -> Self::Pointer<NodeHeader<K, V, Self>> {
        unsafe {
            let raw = ptr.header() as *const _ as *const NodeHeader<K, V, Self>;
            Arc::increment_strong_count(raw);
            Arc::from_raw(raw)
        }
    }
}

pub struct Hamt<
    K: Eq + Hash,
    V,
    #[cfg(feature = "std")] HamtHasher: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] HamtHasher: Hasher + Default,
    Alloc: HamtAllocator<K, V> = DefaultGlobal,
> {
    _ph: PhantomData<(K, V, Alloc, HamtHasher)>,
    root: Option<Alloc::Pointer<NodeHeader<K, V, Alloc>>>,
}

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default, Alloc: HamtAllocator<K, V>> Default
    for Hamt<K, V, HamtHasher, Alloc>
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default, Alloc: HamtAllocator<K, V>> Clone
    for Hamt<K, V, HamtHasher, Alloc>
{
    fn clone(&self) -> Self {
        Self {
            _ph: PhantomData,
            root: self.root.clone(),
        }
    }
}

// enum HamtIterator<
//     'it,
//     K: 'it,
//     V: 'it,
//     LeafIter: Iterator<Item = (&'it K, &'it V)>,
//     InnerIter: Iterator<Item = (&'it K, &'it V)>,
// > {
//     Leaf(LeafIter),
//     Inner(InnerIter),
//     Empty,
// }

// impl<'it, K: 'it, V: 'it, LeafIter, InnerIter> Iterator
//     for HamtIterator<'it, K, V, LeafIter, InnerIter>
// where
//     LeafIter: Iterator<Item = (&'it K, &'it V)>,
//     InnerIter: Iterator<Item = (&'it K, &'it V)>,
// {
//     type Item = (&'it K, &'it V);

//     fn next(&mut self) -> Option<Self::Item> {
//         match self {
//             HamtIterator::Leaf(l) => l.next(),
//             HamtIterator::Inner(i) => i.next(),
//             HamtIterator::Empty => None,
//         }
//     }
// }

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default, Alloc: HamtAllocator<K, V>>
    Hamt<K, V, HamtHasher, Alloc>
{
    pub fn new() -> Self {
        Self {
            _ph: PhantomData,
            root: None,
        }
    }

    /// Get a value from the map for a given key, if it exists.
    pub fn get(&self, key: &K) -> Option<&V> {
        let hash = {
            let mut hasher = HamtHasher::default();
            key.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        let mut level = 0;
        let mut node_cursor = self.root.as_ref();

        while let Some(node) = node_cursor {
            match node.deref().upgrade() {
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

    /// Persistent remove.
    ///
    /// Returns a new HAMT with the given key removed.
    pub fn remove(&self, key: &K) -> Self {
        let hash = {
            let mut hasher = HamtHasher::default();
            key.hash(&mut hasher);
            hasher.finish()
        } as HashCode;

        match self.root.as_ref() {
            Some(root) => Self {
                _ph: PhantomData,
                root: root.remove(key, Path { hash, level: 0 }),
            },
            None => Self {
                _ph: PhantomData,
                root: None,
            },
        }
    }

    // Iterate over the key-value pairs in the map.
    //
    // The order of iteration is not guaranteed (it is determined by the hashing function and order of insertion for
    // collisions).
    // pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
    //     match self.root.as_ref() {
    //         Some(root) => match root.deref().upgrade() {
    //             NodePtr::Leaf(leaf) => HamtIterator::Leaf(leaf.iter()),
    //             NodePtr::Inner(node) => HamtIterator::Inner(node.iter()),
    //         },
    //         None => HamtIterator::Empty,
    //     }
    // }

    #[cfg(all(test, feature = "std"))]
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
            Alloc: HamtAllocator<K, V>,
        >(
            node: &Alloc::Pointer<NodeHeader<K, V, Alloc>>,
            level: usize,
        ) {
            let spaces = " ".repeat(level * 2);

            match node.deref().upgrade() {
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

#[derive(Clone)]
pub struct ImperativeHamt<
    K: Eq + Hash,
    V,
    #[cfg(feature = "std")] HamtHasher: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] HamtHasher: Hasher + Default,
    Alloc: HamtAllocator<K, V> = DefaultGlobal,
> {
    hamt: Box<Hamt<K, V, HamtHasher, Alloc>>,
}

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default, Alloc: HamtAllocator<K, V>>
    ImperativeHamt<K, V, HamtHasher, Alloc>
{
    pub fn new() -> Self {
        Self {
            hamt: Box::new(Hamt::<K, V, HamtHasher, Alloc>::new()),
        }
    }

    pub fn as_persistent(&self) -> Hamt<K, V, HamtHasher, Alloc> {
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

impl<K: Eq + Hash, V, HamtHasher: Hasher + Default, Alloc: HamtAllocator<K, V>> Default
    for ImperativeHamt<K, V, HamtHasher, Alloc>
{
    fn default() -> Self {
        Self::new()
    }
}

#[repr(u8)]
#[non_exhaustive]
#[derive(Eq, PartialEq)]
pub enum NodeType {
    Leaf = 1,
    Inner = 2,

    _Header = 0,
}

impl NodeType {
    fn from_u8(n: u8) -> Self {
        debug_assert_eq!(0b111 & n, n);

        match n {
            0 => Self::_Header,
            1 => Self::Leaf,
            2 => Self::Inner,
            _ => unreachable!(),
        }
    }
}

#[cfg(target_pointer_width = "64")]
// #[derive(Clone, Copy)]
pub struct NodeHeader<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    _ph: PhantomData<(K, V, Alloc)>,
    tag_size: usize,
}

// impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> Copy for NodeHeader<K, V, Alloc> {}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> Clone for NodeHeader<K, V, Alloc> {
    fn clone(&self) -> Self {
        Self {
            _ph: PhantomData,
            tag_size: self.tag_size,
        }
    }
}

#[cfg(target_pointer_width = "64")]
impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> NodeHeader<K, V, Alloc> {
    const TAG_MASK: usize = 0b111 << 61;
    const LEVEL_MASK: usize = 0b1111 << 57;
    const SIZE_MASK: usize = !(Self::TAG_MASK | Self::LEVEL_MASK);

    unsafe fn new<T: HamtNode<K, V, Alloc> + ?Sized>(metadata: usize) -> Self {
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

    fn insert(&self, k: K, v: V, path: Path) -> Alloc::Pointer<Self> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.insert(k, v, path),
            NodePtr::Inner(node) => node.insert(k, v, path),
        }
    }

    fn remove(&self, k: &K, path: Path) -> Option<Alloc::Pointer<Self>> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.remove(k, path),
            NodePtr::Inner(node) => node.remove(k, path),
        }
    }
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> HamtNode<K, V, Alloc>
    for NodeHeader<K, V, Alloc>
{
    const TAG: NodeType = NodeType::_Header;

    fn header(&self) -> &NodeHeader<K, V, Alloc> {
        self
    }
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> Drop for NodeHeader<K, V, Alloc> {
    fn drop(&mut self) {
        match self.upgrade_mut() {
            NodePtrMut::Leaf(l) => {
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

enum NodePtr<'a, K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    Leaf(&'a LeafNode<K, V, Alloc>),
    Inner(&'a InnerNode<K, V, Alloc>),
}

enum NodePtrMut<'a, K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    Leaf(&'a mut LeafNode<K, V, Alloc>),
    Inner(&'a mut InnerNode<K, V, Alloc>),
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> NodeHeader<K, V, Alloc> {
    fn upgrade(&self) -> NodePtr<K, V, Alloc>
    where
        <LeafNode<K, V, Alloc> as Pointee>::Metadata: UsizeMetadata,
        <InnerNode<K, V, Alloc> as Pointee>::Metadata: UsizeMetadata,
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
                    <InnerNode<K, V, Alloc> as Pointee>::Metadata::from_usize(self.metadata()),
                ))
            }),
            NodeType::_Header => unreachable!(),
        }
    }

    fn upgrade_mut(&mut self) -> NodePtrMut<K, V, Alloc>
    where
        <LeafNode<K, V, Alloc> as Pointee>::Metadata: UsizeMetadata,
        <InnerNode<K, V, Alloc> as Pointee>::Metadata: UsizeMetadata,
    {
        match self.tag() {
            NodeType::Leaf => NodePtrMut::Leaf(unsafe {
                &mut *core::ptr::from_raw_parts_mut(
                    self as *mut _ as *mut (),
                    <LeafNode<K, V, Alloc> as Pointee>::Metadata::from_usize(self.metadata()),
                )
            }),
            NodeType::Inner => NodePtrMut::Inner(unsafe {
                &mut *(core::ptr::from_raw_parts_mut(
                    self as *mut _ as *mut (),
                    <InnerNode<K, V, Alloc> as Pointee>::Metadata::from_usize(self.metadata()),
                ))
            }),
            NodeType::_Header => unreachable!(),
        }
    }
}

#[cfg(target_pointer_width = "64")]
type Bitmap = u64;

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
struct InnerNode<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    _header: NodeHeader<K, V, Alloc>,
    bitmap: Bitmap,
    children: [Alloc::Pointer<NodeHeader<K, V, Alloc>>],
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> InnerNode<K, V, Alloc> {
    fn get_child(
        &self,
        hash: HashCode,
        level: usize,
    ) -> Option<&Alloc::Pointer<NodeHeader<K, V, Alloc>>> {
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

    fn insert(&self, key: K, value: V, path: Path) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        debug_assert!(path.level <= MAX_LEVEL);

        let index = hash_bits_for_level(path.hash, path.level + 1);

        let occupied = (self.bitmap >> index & 1) == 1;
        let masked_bitmap = ((1 << index) - 1) & self.bitmap;
        let pop = masked_bitmap.count_ones() as usize;

        if occupied {
            self.insert_occupied(key, value, path, pop)
        } else {
            self.insert_vacant(key, value, path, index, pop)
        }
    }

    fn insert_vacant(
        &self,
        key: K,
        value: V,
        path: Path,
        index: u64,
        pop: usize,
    ) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        // Allocate a new InnerNode with a new bitmap with the `index`th bit set,
        // and with a new single-value leaf node allocated with (k, v) at index `pop`.

        let new_leaf = LeafNode::<K, V, Alloc>::create_with_pair(key, value, path.increment());

        let new_bitmap = self.bitmap | (1 << index);

        let new_inner = unsafe {
            Alloc::allocate::<Self>(self.children.len() + 1, move |inner| {
                write(
                    &mut inner._header,
                    NodeHeader::new::<Self>(self.children.len() + 1),
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
        };

        unsafe { Alloc::downgrade_ptr(new_inner) }
    }

    fn insert_occupied(
        &self,
        key: K,
        value: V,
        path: Path,
        pop: usize,
    ) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        // Allocate a new InnerNode with the `pop`th child replaced by the result
        // of inserting k, v into the child.
        let new_child = self.children[pop].insert(key, value, path.increment());

        let new_inner = unsafe {
            Alloc::allocate::<Self>(self.children.len(), |inner| {
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
        };

        unsafe { Alloc::downgrade_ptr(new_inner) }
    }

    fn remove(&self, key: &K, path: Path) -> Option<Alloc::Pointer<NodeHeader<K, V, Alloc>>> {
        debug_assert!(path.level <= MAX_LEVEL);

        let index = hash_bits_for_level(path.hash, path.level + 1);

        let occupied = (self.bitmap >> index & 1) == 1;

        if !occupied {
            // The key is not in the map.
            return Some(unsafe { Alloc::ptr_from_ref_reinterpret(self) });
        }

        let masked_bitmap = ((1 << index) - 1) & self.bitmap;
        let pop = masked_bitmap.count_ones() as usize;

        let next_path = path.increment();

        let next = self.children[pop].remove(key, next_path);

        let cur_ptr = &*self.children[pop];

        if let Some(next) = next {
            if core::ptr::eq(&*next, cur_ptr) {
                // The child was not removed.
                return Some(unsafe { Alloc::ptr_from_ref_reinterpret(self) });
            }

            // The child was modified. We have a new pointer that we want to replace our current pointer with, but the
            // bitmap is unnafected.
            let new_inner = unsafe {
                Alloc::allocate::<Self>(self.children.len(), |inner| {
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
                })
            };

            Some(unsafe { Alloc::downgrade_ptr(new_inner) })
        } else {
            // The child was removed.

            let remaining_child_idx_if_pruning = if pop == 0 { 1 } else { 0 };
            let can_prune_to_leaf = self.children.len() == 2
                && self.children[remaining_child_idx_if_pruning]
                    .deref()
                    .header()
                    .tag()
                    == NodeType::Leaf;

            // If we ended up with zero children after the removal, this node is pruned and we can return None.
            let new_len = self.children.len() - 1;
            if new_len == 0 {
                None
            } else if can_prune_to_leaf {
                // If we ended up with one child after the removal and it's a leaf node, we prune this node to return
                // the leaf itself.
                if let NodePtr::Leaf(leaf) = self.children[remaining_child_idx_if_pruning]
                    .deref()
                    .upgrade()
                {
                    Some(unsafe { Alloc::ptr_from_ref_reinterpret(leaf) })
                } else {
                    unreachable!()
                }
            } else {
                let new_bitmap = self.bitmap & !(1 << index);

                let new_inner = unsafe {
                    Alloc::allocate::<Self>(new_len, |inner| {
                        write(&mut inner._header, NodeHeader::new::<Self>(new_len));
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
                    })
                };

                Some(unsafe { Alloc::downgrade_ptr(new_inner) })
            }
        }
    }

    // fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
    //     self.children
    //         .iter()
    //         .flat_map(|child| match child.deref().upgrade() {
    //             NodePtr::Leaf(leaf) => HamtIterator::Leaf(leaf.iter()),
    //             NodePtr::Inner(node) => HamtIterator::Inner(node.iter()),
    //         })
    // }
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> HamtNode<K, V, Alloc> for InnerNode<K, V, Alloc> {
    const TAG: NodeType = NodeType::Inner;

    fn header(&self) -> &NodeHeader<K, V, Alloc> {
        &self._header
    }
}

#[repr(C)]
struct LeafNode<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    _header: NodeHeader<K, V, Alloc>,
    _ph: PhantomData<Alloc>,
    hash: HashCode,
    // path: Path,
    values: [Alloc::WrappedKvp],
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> LeafNode<K, V, Alloc> {
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

    fn insert(&self, key: K, value: V, path: Path) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
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
                Alloc::allocate::<InnerNode<K, V, Alloc>>(size, move |inner| {
                    write(
                        &mut inner._header,
                        NodeHeader::new::<InnerNode<K, V, Alloc>>(size),
                    );
                    inner.bitmap = bitmap;

                    match size {
                        1 => {
                            write(
                                &mut inner.children[0],
                                // simply retry the insert at the next level
                                self.insert(key, value, path.increment()),
                            );
                        }
                        2 => {
                            let new_leaf = Self::create_with_pair(key, value, path.increment());
                            let self_leaf = Alloc::ptr_from_ref_reinterpret(self);

                            match path_bits_for_next_level.cmp(&self_bits_for_next_level) {
                                Ordering::Less => {
                                    write(&mut inner.children[0], new_leaf);
                                    write(&mut inner.children[1], self_leaf);
                                }
                                Ordering::Greater => {
                                    write(&mut inner.children[0], self_leaf);
                                    write(&mut inner.children[1], new_leaf);
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

    fn remove(&self, k: &K, path: Path) -> Option<Alloc::Pointer<NodeHeader<K, V, Alloc>>> {
        debug_assert!(path.level <= MAX_LEVEL);
        // debug_assert!(path.level == self.path.level);
        debug_assert_eq!(
            hash_masked_for_level(path.hash, path.level),
            hash_masked_for_level(self.hash, path.level)
        );

        if path.hash != self.hash {
            // The key is not in the map.
            return Some(unsafe { Alloc::ptr_from_ref_reinterpret(self) });
        }

        let cur_value = self.values.iter().enumerate().find(|(_, kvp)| kvp.0 == *k);

        if let Some((idx, _)) = cur_value {
            if self.values.len() == 1 {
                // The key is the only key in the leaf node. We can prune the leaf node.
                None
            } else {
                // The key is in the leaf node, but there are other keys. We can remove the key from the leaf node.
                let new_leaf = unsafe {
                    Alloc::allocate::<Self>(self.values.len() - 1, |leaf| {
                        write(
                            &mut leaf._header,
                            NodeHeader::new::<Self>(self.values.len() - 1),
                        );
                        leaf._ph = PhantomData;
                        leaf.hash = self.hash;
                        for i in 0..self.values.len() {
                            if i != idx {
                                write(&mut leaf.values[i], self.values[i].clone());
                            }
                        }
                    })
                };

                Some(unsafe { Alloc::downgrade_ptr(new_leaf) })
            }
        } else {
            // The key is not in the leaf node.
            Some(unsafe { Alloc::ptr_from_ref_reinterpret(self) })
        }
    }

    // fn iter(&self) {
    //     self.values.iter().map(|kvp| (&kvp.0, &kvp.1))
    // }

    fn create_with_pair(key: K, value: V, path: Path) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        unsafe {
            Alloc::downgrade_ptr({
                Alloc::allocate::<LeafNode<K, V, Alloc>>(1, |leaf| {
                    write(
                        &mut leaf._header,
                        NodeHeader::new::<LeafNode<K, V, Alloc>>(1),
                    );

                    leaf._ph = PhantomData;

                    leaf.hash = path.hash;

                    debug_assert!(
                        (&leaf.values[..] as *const _ as *const () as usize)
                            % core::mem::align_of::<Alloc::WrappedKvp>()
                            == 0
                    );

                    let kvp = Alloc::wrap_kvp(key, value);

                    debug_assert!(
                        (&leaf.values[0] as *const _ as *const () as usize)
                            % core::mem::align_of::<Alloc::WrappedKvp>()
                            == 0
                    );

                    write(&mut leaf.values[0], kvp);
                })
            })
        }
    }

    unsafe fn add_pair(&self, key: K, value: V) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        let cur_value = self.values.iter().enumerate().find(|(_, kvp)| kvp.0 == key);

        if let Some((idx, _)) = cur_value {
            unsafe {
                Alloc::downgrade_ptr(Alloc::allocate::<Self>(self.values.len(), move |leaf| {
                    write(&mut leaf._header, self._header.clone());
                    leaf._ph = PhantomData;
                    leaf.hash = self.hash;
                    for i in 0..self.values.len() {
                        if i != idx {
                            write(&mut leaf.values[i], self.values[i].clone());
                        }
                    }

                    write(&mut leaf.values[idx], Alloc::wrap_kvp(key, value));
                }))
            }
        } else {
            let next_len = self.values.len() + 1;

            assert!(next_len <= NodeHeader::<K, V, Alloc>::SIZE_MASK);

            unsafe {
                Alloc::downgrade_ptr(Alloc::allocate::<Self>(self.values.len() + 1, |leaf| {
                    write(
                        &mut leaf._header,
                        NodeHeader::new::<Self>(self.values.len() + 1),
                    );
                    leaf._ph = PhantomData;
                    leaf.hash = self.hash;
                    for i in 0..self.values.len() {
                        write(&mut leaf.values[i], self.values[i].clone());
                    }
                    write(
                        &mut leaf.values[self.values.len()],
                        Alloc::wrap_kvp(key, value),
                    );
                }))
            }
        }
    }
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> HamtNode<K, V, Alloc> for LeafNode<K, V, Alloc> {
    const TAG: NodeType = NodeType::Leaf;

    fn header(&self) -> &NodeHeader<K, V, Alloc> {
        let res = &self._header;
        debug_assert_eq!(self as *const _ as *const (), res as *const _ as *const ());
        res
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

        let hamt_mt2 = hamt_0.remove(&0);

        assert_eq!(hamt_mt2.get(&0), None);

        let hamt_1 = hamt_0.insert(1, 2);

        assert_eq!(hamt_1.get(&0), Some(&1));
        assert_eq!(hamt_1.get(&1), Some(&2));
        assert_eq!(hamt_1.get(&2), None);
        assert_eq!(hamt_0.get(&1), None);
        assert_eq!(hamt_0.get(&0), Some(&1));

        let mut hamt = hamt_1.clone();

        let mut hamts = vec![hamt_0, hamt_1];

        const HAMTS: u64 = 1000;

        for i in 2..HAMTS {
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

        let mut last_hamt = hamts[hamts.len() - 1].clone();

        for i in 0..(HAMTS / 2) {
            last_hamt = last_hamt.remove(&(i * 2));

            assert_eq!(last_hamt.get(&(i * 2)), None);
        }

        for idx in 0..HAMTS {
            if idx % 2 == 0 {
                assert_eq!(last_hamt.get(&idx), None);
            } else {
                assert_eq!(last_hamt.get(&idx), Some(&(idx + 1)));
            }
        }
    }

    #[test]
    fn check_align() {
        // Check that the offset of _header within the node type is 0 i.e. that a pointer
        // to the node type is also a pointer to the header.

        let data = DefaultGlobal::allocate::<LeafNode<(), (), DefaultGlobal>>(0, |_| {});

        let node_ptr = data.as_ref() as *const _ as *const ();
        let node_header_ptr = &data._header as *const _ as *const ();

        assert_eq!(node_ptr as usize, node_header_ptr as usize);

        let data = DefaultGlobal::allocate::<InnerNode<(), (), DefaultGlobal>>(0, |_| {});

        let node_ptr = data.as_ref() as *const _ as *const ();
        let node_header_ptr = &data._header as *const _ as *const ();

        assert_eq!(node_ptr as usize, node_header_ptr as usize);
    }
}
