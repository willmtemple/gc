use core::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    // iter::{Empty, FlatMap},
    marker::PhantomData,
    ops::Deref,
    ptr::{write, NonNull, Pointee},
};

use crate::{
    inhibit_collections,
    mark::Mark,
    obj::GcObject,
    ptr::{Gc, GcRoot},
};

#[cfg(target_pointer_width = "64")]
type HashCode = u64;

pub trait HamtNode<K: Eq + Hash + Mark, V: Mark> {
    const TAG: NodeType;

    fn header(&self) -> &NodeHeader<K, V>;
}

#[allow(dead_code)]
struct GcAllocator<K: Eq + Hash + Mark, V: Mark> {
    _ph: PhantomData<(K, V)>,
}

impl<K: Eq + Hash + Mark, V: Mark> GcAllocator<K, V> {
    pub(crate) fn wrap_kvp(k: K, v: V) -> Gc<(K, V)> {
        unsafe { GcRoot::from((k, v)).as_unrooted() }
    }

    pub(crate) fn allocate<T: HamtNode<K, V> + Mark + ?Sized>(
        metadata: <GcObject<T> as Pointee>::Metadata,
        init: impl FnOnce(&mut T),
    ) -> Gc<T>
    where
        <GcObject<T> as Pointee>::Metadata: As<usize>,
    {
        let ptr = unsafe { GcRoot::<T>::new_uninitialized_with_metadata(metadata).unwrap() };

        let raw_ptr = unsafe {
            &mut *core::ptr::from_raw_parts_mut::<GcObject<T>>(
                ptr.0.ptr.as_ptr() as *mut _ as *mut (),
                metadata,
            )
        };

        init(&mut raw_ptr.data);

        unsafe { ptr.as_unrooted() }
    }

    pub(crate) unsafe fn downgrade_ptr<T: HamtNode<K, V> + Mark + ?Sized>(
        ptr: Gc<T>,
    ) -> Gc<NodeHeader<K, V>>
    where
        <GcObject<T> as Pointee>::Metadata: As<usize>,
    {
        unsafe { GcRoot::<NodeHeader<K, V>>::from_raw(ptr.ptr.cast()) }.as_unrooted()
    }

    pub(crate) unsafe fn ptr_from_ref_reinterpret<T: HamtNode<K, V> + ?Sized>(
        ptr: &T,
    ) -> Gc<NodeHeader<K, V>>
    where
        <GcObject<T> as Pointee>::Metadata: As<usize>,
        <T as Pointee>::Metadata: As<usize>,
    {
        let obj_ptr = core::ptr::from_raw_parts::<GcObject<T>>(
            core::ptr::null(),
            <<GcObject<T> as Pointee>::Metadata as As<usize>>::from_usize(
                core::ptr::metadata(ptr).to_usize(),
            ),
        );

        let data_ptr = &(*obj_ptr).data;

        assert_eq!(
            obj_ptr as *const () as usize,
            ptr as *const _ as *const () as usize
        );

        let offset = (data_ptr as *const _ as *const () as usize) - (obj_ptr as *const () as usize);

        let header_ptr =
            (ptr as *const _ as *const () as usize - offset) as *mut GcObject<NodeHeader<K, V>>;

        unsafe { GcRoot::<NodeHeader<K, V>>::from_raw(NonNull::new(header_ptr).unwrap()) }
            .as_unrooted()
    }
}

pub struct Hamt<
    K: Eq + Hash + Mark,
    V: Mark,
    #[cfg(feature = "std")] HamtHasher: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] HamtHasher: Hasher + Default,
> {
    _ph: PhantomData<HamtHasher>,
    root: Option<Gc<NodeHeader<K, V>>>,
}

impl<K: Eq + Hash + Mark, V: Mark, HamtHasher: Hasher + Default> Default
    for Hamt<K, V, HamtHasher>
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Mark, V: Mark, HamtHasher: Hasher + Default> Clone for Hamt<K, V, HamtHasher> {
    fn clone(&self) -> Self {
        Self {
            _ph: PhantomData,
            root: self.root,
        }
    }
}

impl<K: Eq + Hash + Mark, V: Mark, HamtHasher: Hasher + Default> Hamt<K, V, HamtHasher> {
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
        inhibit_collections(|| {
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
                    root: Some(LeafNode::<K, V>::create_with_pair(
                        key,
                        value,
                        Path { hash, level: 0 },
                    )),
                },
            }
        })
    }

    /// Persistent remove.
    ///
    /// Returns a new HAMT with the given key removed.
    pub fn remove(&self, key: &K) -> Self {
        inhibit_collections(|| {
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
        })
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

        print_node::<K, V>(self.root.as_ref().unwrap(), 0);

        fn print_node<K: Eq + Hash + Mark + core::fmt::Debug, V: Mark + core::fmt::Debug>(
            node: &Gc<NodeHeader<K, V>>,
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
                        print_node::<K, V>(child, level + 1);
                    }
                }
            }
        }
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
pub struct NodeHeader<K: Eq + Hash + Mark, V: Mark> {
    _ph: PhantomData<(K, V)>,
    tag_size: usize,
}

impl<K: Eq + Hash + Mark, V: Mark> Clone for NodeHeader<K, V> {
    fn clone(&self) -> Self {
        Self {
            _ph: PhantomData,
            tag_size: self.tag_size,
        }
    }
}

impl<K: Eq + Hash + Mark, V: Mark> Mark for NodeHeader<K, V> {
    fn mark(&mut self) {
        match self.upgrade_mut() {
            NodePtrMut::Leaf(l) => {
                for kvp in l.values.iter_mut() {
                    kvp.mark();
                }
            }
            NodePtrMut::Inner(i) => {
                for child in i.children.iter_mut() {
                    child.mark();
                }
            }
        }
    }
}

#[cfg(target_pointer_width = "64")]
impl<K: Eq + Hash + Mark, V: Mark> NodeHeader<K, V> {
    const TAG_MASK: usize = 0b111 << 61;
    const LEVEL_MASK: usize = 0b1111 << 57;
    const SIZE_MASK: usize = !(Self::TAG_MASK | Self::LEVEL_MASK);

    unsafe fn new<T: HamtNode<K, V> + ?Sized>(metadata: usize) -> Self {
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

    fn insert(&self, k: K, v: V, path: Path) -> Gc<Self> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.insert(k, v, path),
            NodePtr::Inner(node) => node.insert(k, v, path),
        }
    }

    fn remove(&self, k: &K, path: Path) -> Option<Gc<Self>> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.remove(k, path),
            NodePtr::Inner(node) => node.remove(k, path),
        }
    }
}

impl<K: Eq + Hash + Mark, V: Mark> HamtNode<K, V> for NodeHeader<K, V> {
    const TAG: NodeType = NodeType::_Header;

    fn header(&self) -> &NodeHeader<K, V> {
        self
    }
}

// impl<K: Eq + Hash + Mark, V: Mark> Drop for NodeHeader<K, V> {
//     fn drop(&mut self) {
//         match self.upgrade_mut() {
//             NodePtrMut::Leaf(l) => {
//                 for kvp in l.values.iter_mut() {
//                     core::mem::drop(unsafe { core::ptr::read(kvp) });
//                 }
//             }
//             NodePtrMut::Inner(i) => {
//                 for child in i.children.iter_mut() {
//                     core::mem::drop(unsafe { core::ptr::read(child) });
//                 }
//             }
//         }
//     }
// }

enum NodePtr<'a, K: Eq + Hash + Mark, V: Mark> {
    Leaf(&'a LeafNode<K, V>),
    Inner(&'a InnerNode<K, V>),
}

enum NodePtrMut<'a, K: Eq + Hash + Mark, V: Mark> {
    Leaf(&'a mut LeafNode<K, V>),
    Inner(&'a mut InnerNode<K, V>),
}

impl<K: Eq + Hash + Mark, V: Mark> NodeHeader<K, V> {
    fn upgrade(&self) -> NodePtr<K, V>
    where
        <LeafNode<K, V> as Pointee>::Metadata: As<usize>,
        <InnerNode<K, V> as Pointee>::Metadata: As<usize>,
    {
        match self.tag() {
            NodeType::Leaf => NodePtr::Leaf(unsafe {
                &*core::ptr::from_raw_parts(
                    self as *const _ as *const (),
                    <LeafNode<K, V> as Pointee>::Metadata::from_usize(self.metadata()),
                )
            }),
            NodeType::Inner => NodePtr::Inner(unsafe {
                &*(core::ptr::from_raw_parts(
                    self as *const _ as *const (),
                    <InnerNode<K, V> as Pointee>::Metadata::from_usize(self.metadata()),
                ))
            }),
            NodeType::_Header => unreachable!(),
        }
    }

    fn upgrade_mut(&mut self) -> NodePtrMut<K, V>
    where
        <LeafNode<K, V> as Pointee>::Metadata: As<usize>,
        <InnerNode<K, V> as Pointee>::Metadata: As<usize>,
    {
        match self.tag() {
            NodeType::Leaf => NodePtrMut::Leaf(unsafe {
                &mut *core::ptr::from_raw_parts_mut(
                    self as *mut _ as *mut (),
                    <LeafNode<K, V> as Pointee>::Metadata::from_usize(self.metadata()),
                )
            }),
            NodeType::Inner => NodePtrMut::Inner(unsafe {
                &mut *(core::ptr::from_raw_parts_mut(
                    self as *mut _ as *mut (),
                    <InnerNode<K, V> as Pointee>::Metadata::from_usize(self.metadata()),
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
struct InnerNode<K: Eq + Hash + Mark, V: Mark> {
    _header: NodeHeader<K, V>,
    bitmap: Bitmap,
    children: [Gc<NodeHeader<K, V>>],
}

impl<K: Eq + Hash + Mark, V: Mark> InnerNode<K, V> {
    fn get_child(&self, hash: HashCode, level: usize) -> Option<&Gc<NodeHeader<K, V>>> {
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

    fn insert(&self, key: K, value: V, path: Path) -> Gc<NodeHeader<K, V>> {
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
    ) -> Gc<NodeHeader<K, V>> {
        // Allocate a new InnerNode with a new bitmap with the `index`th bit set,
        // and with a new single-value leaf node allocated with (k, v) at index `pop`.

        let new_leaf = LeafNode::<K, V>::create_with_pair(key, value, path.increment());

        let new_bitmap = self.bitmap | (1 << index);

        let new_inner = unsafe {
            GcAllocator::allocate::<Self>(self.children.len() + 1, move |inner| {
                write(
                    &mut inner._header,
                    NodeHeader::new::<Self>(self.children.len() + 1),
                );
                inner.bitmap = new_bitmap;

                for i in 0..self.children.len() + 1 {
                    match i.cmp(&pop) {
                        Ordering::Less => {
                            write(&mut inner.children[i], self.children[i]);
                        }
                        Ordering::Greater => {
                            write(&mut inner.children[i], self.children[i - 1]);
                        }
                        Ordering::Equal => {}
                    }
                }

                // We do this at the end to avoid requiring new_leaf to be copy.
                write(&mut inner.children[pop], new_leaf);
            })
        };

        unsafe { GcAllocator::downgrade_ptr(new_inner) }
    }

    fn insert_occupied(&self, key: K, value: V, path: Path, pop: usize) -> Gc<NodeHeader<K, V>> {
        // Allocate a new InnerNode with the `pop`th child replaced by the result
        // of inserting k, v into the child.
        let new_child = self.children[pop].insert(key, value, path.increment());

        let new_inner = unsafe {
            GcAllocator::allocate::<Self>(self.children.len(), |inner| {
                write(&mut inner._header, self._header.clone());
                inner.bitmap = self.bitmap;
                for (i, child) in self.children.iter().enumerate() {
                    if i.cmp(&pop) != Ordering::Equal {
                        write(&mut inner.children[i], *child);
                    }
                }

                // We do this at the end to avoid requiring new_child to be copy.
                write(&mut inner.children[pop], new_child);
            })
        };

        unsafe { GcAllocator::downgrade_ptr(new_inner) }
    }

    fn remove(&self, key: &K, path: Path) -> Option<Gc<NodeHeader<K, V>>> {
        debug_assert!(path.level <= MAX_LEVEL);

        let index = hash_bits_for_level(path.hash, path.level + 1);

        let occupied = (self.bitmap >> index & 1) == 1;

        if !occupied {
            // The key is not in the map.
            return Some(unsafe { GcAllocator::ptr_from_ref_reinterpret(self) });
        }

        let masked_bitmap = ((1 << index) - 1) & self.bitmap;
        let pop = masked_bitmap.count_ones() as usize;

        let next_path = path.increment();

        let next = self.children[pop].remove(key, next_path);

        let cur_ptr = &*self.children[pop];

        if let Some(next) = next {
            if core::ptr::eq(&*next, cur_ptr) {
                // The child was not removed.
                return Some(unsafe { GcAllocator::ptr_from_ref_reinterpret(self) });
            }

            // The child was modified. We have a new pointer that we want to replace our current pointer with, but the
            // bitmap is unnafected.
            let new_inner = unsafe {
                GcAllocator::allocate::<Self>(self.children.len(), |inner| {
                    write(&mut inner._header, self._header.clone());
                    inner.bitmap = self.bitmap;

                    debug_assert_eq!(self.children.len(), inner.children.len());

                    for i in 0..inner.children.len() {
                        if i.cmp(&pop) != Ordering::Equal {
                            write(&mut inner.children[i], self.children[i]);
                        }
                    }

                    // We do this at the end to avoid requiring next to be copy.
                    write(&mut inner.children[pop], next);
                })
            };

            Some(unsafe { GcAllocator::downgrade_ptr(new_inner) })
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
                    Some(unsafe { GcAllocator::ptr_from_ref_reinterpret(leaf) })
                } else {
                    unreachable!()
                }
            } else {
                let new_bitmap = self.bitmap & !(1 << index);

                let new_inner = unsafe {
                    GcAllocator::allocate::<Self>(new_len, |inner| {
                        write(&mut inner._header, NodeHeader::new::<Self>(new_len));
                        inner.bitmap = new_bitmap;
                        for i in 0..new_len {
                            match i.cmp(&pop) {
                                Ordering::Less => {
                                    write(&mut inner.children[i], self.children[i]);
                                }
                                Ordering::Greater | Ordering::Equal => {
                                    write(&mut inner.children[i], self.children[i + 1]);
                                }
                            }
                        }
                    })
                };

                Some(unsafe { GcAllocator::downgrade_ptr(new_inner) })
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

impl<K: Eq + Hash + Mark, V: Mark> HamtNode<K, V> for InnerNode<K, V> {
    const TAG: NodeType = NodeType::Inner;

    fn header(&self) -> &NodeHeader<K, V> {
        &self._header
    }
}

#[repr(C)]
struct LeafNode<K: Eq + Hash + Mark, V: Mark> {
    _header: NodeHeader<K, V>,
    hash: HashCode,
    // path: Path,
    values: [Gc<(K, V)>],
}

impl<K: Eq + Hash + Mark, V: Mark> LeafNode<K, V> {
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

    fn insert(&self, key: K, value: V, path: Path) -> Gc<NodeHeader<K, V>> {
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
                GcAllocator::allocate::<InnerNode<K, V>>(size, move |inner| {
                    write(&mut inner._header, NodeHeader::new::<InnerNode<K, V>>(size));
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
                            let self_leaf = GcAllocator::ptr_from_ref_reinterpret(self);

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

            unsafe { GcAllocator::downgrade_ptr(new_inner) }
        }
    }

    fn remove(&self, k: &K, path: Path) -> Option<Gc<NodeHeader<K, V>>> {
        debug_assert!(path.level <= MAX_LEVEL);
        // debug_assert!(path.level == self.path.level);
        debug_assert_eq!(
            hash_masked_for_level(path.hash, path.level),
            hash_masked_for_level(self.hash, path.level)
        );

        if path.hash != self.hash {
            // The key is not in the map.
            return Some(unsafe { GcAllocator::ptr_from_ref_reinterpret(self) });
        }

        let cur_value = self.values.iter().enumerate().find(|(_, kvp)| kvp.0 == *k);

        if let Some((idx, _)) = cur_value {
            if self.values.len() == 1 {
                // The key is the only key in the leaf node. We can prune the leaf node.
                None
            } else {
                // The key is in the leaf node, but there are other keys. We can remove the key from the leaf node.
                let new_leaf = unsafe {
                    GcAllocator::allocate::<Self>(self.values.len() - 1, |leaf| {
                        write(
                            &mut leaf._header,
                            NodeHeader::new::<Self>(self.values.len() - 1),
                        );
                        leaf.hash = self.hash;
                        for i in 0..self.values.len() {
                            if i != idx {
                                write(&mut leaf.values[i], self.values[i]);
                            }
                        }
                    })
                };

                Some(unsafe { GcAllocator::downgrade_ptr(new_leaf) })
            }
        } else {
            // The key is not in the leaf node.
            Some(unsafe { GcAllocator::ptr_from_ref_reinterpret(self) })
        }
    }

    // fn iter(&self) {
    //     self.values.iter().map(|kvp| (&kvp.0, &kvp.1))
    // }

    fn create_with_pair(key: K, value: V, path: Path) -> Gc<NodeHeader<K, V>> {
        unsafe {
            GcAllocator::downgrade_ptr({
                GcAllocator::allocate::<LeafNode<K, V>>(1, |leaf| {
                    write(&mut leaf._header, NodeHeader::new::<LeafNode<K, V>>(1));

                    leaf.hash = path.hash;

                    debug_assert!(
                        (&leaf.values[..] as *const _ as *const () as usize)
                            % core::mem::align_of::<Gc<(K, V)>>()
                            == 0
                    );

                    let kvp = GcAllocator::wrap_kvp(key, value);

                    debug_assert!(
                        (&leaf.values[0] as *const _ as *const () as usize)
                            % core::mem::align_of::<Gc<(K, V)>>()
                            == 0
                    );

                    write(&mut leaf.values[0], kvp);
                })
            })
        }
    }

    unsafe fn add_pair(&self, key: K, value: V) -> Gc<NodeHeader<K, V>> {
        let cur_value = self.values.iter().enumerate().find(|(_, kvp)| kvp.0 == key);

        if let Some((idx, _)) = cur_value {
            unsafe {
                GcAllocator::downgrade_ptr(GcAllocator::allocate::<Self>(
                    self.values.len(),
                    move |leaf| {
                        write(&mut leaf._header, self._header.clone());
                        leaf.hash = self.hash;
                        for i in 0..self.values.len() {
                            if i != idx {
                                write(&mut leaf.values[i], self.values[i]);
                            }
                        }

                        write(&mut leaf.values[idx], GcAllocator::wrap_kvp(key, value));
                    },
                ))
            }
        } else {
            let next_len = self.values.len() + 1;

            assert!(next_len <= NodeHeader::<K, V>::SIZE_MASK);

            unsafe {
                GcAllocator::downgrade_ptr(GcAllocator::allocate::<Self>(
                    self.values.len() + 1,
                    |leaf| {
                        write(
                            &mut leaf._header,
                            NodeHeader::new::<Self>(self.values.len() + 1),
                        );
                        leaf.hash = self.hash;
                        for i in 0..self.values.len() {
                            write(&mut leaf.values[i], self.values[i]);
                        }
                        write(
                            &mut leaf.values[self.values.len()],
                            GcAllocator::wrap_kvp(key, value),
                        );
                    },
                ))
            }
        }
    }
}

impl<K: Eq + Hash + Mark, V: Mark> HamtNode<K, V> for LeafNode<K, V> {
    const TAG: NodeType = NodeType::Leaf;

    fn header(&self) -> &NodeHeader<K, V> {
        let res = &self._header;
        debug_assert_eq!(self as *const _ as *const (), res as *const _ as *const ());
        res
    }
}

impl<K: Eq + Hash + Mark, V: Mark> Mark for InnerNode<K, V> {
    fn mark(&mut self) {
        unreachable!()
    }
}

impl<K: Eq + Hash + Mark, V: Mark> Mark for LeafNode<K, V> {
    fn mark(&mut self) {
        unreachable!()
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use crate::hamt::{
        gc::GcAllocator,
        gc::{InnerNode, LeafNode, NodeHeader},
    };

    #[test]
    fn gc_print_trees() {
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
    fn gc_check_align() {
        // Check that the offset of _header within the node type is 0 i.e. that a pointer
        // to the node type is also a pointer to the header.

        let data = GcAllocator::allocate::<LeafNode<(), ()>>(0, |leaf| {
            unsafe { core::ptr::write(&mut leaf._header, NodeHeader::new::<LeafNode<(), ()>>(0)) };
        });

        let node_ptr = &(*data) as *const _ as *const ();
        let node_header_ptr = &data._header as *const _ as *const ();

        assert_eq!(node_ptr as usize, node_header_ptr as usize);

        let data = GcAllocator::allocate::<InnerNode<(), ()>>(0, |inner| {
            unsafe {
                core::ptr::write(&mut inner._header, NodeHeader::new::<InnerNode<(), ()>>(0))
            };
        });

        let node_ptr = &(*data) as *const _ as *const ();
        let node_header_ptr = &data._header as *const _ as *const ();

        assert_eq!(node_ptr as usize, node_header_ptr as usize);
    }
}
