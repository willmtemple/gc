// Persistent Hash Array Mapped Trie

// pub mod gc;

use core::{
    alloc::Layout,
    borrow::Borrow,
    cmp::Ordering,
    hash::{Hash, Hasher},
    // iter::{Empty, FlatMap},
    marker::PhantomData,
    ops::{Deref, Index},
    ptr::{null, write, Pointee},
};

use alloc::{alloc::Global, boxed::Box, rc::Rc, sync::Arc};

use crate::obj::UsizeMetadata;

#[cfg(target_pointer_width = "64")]
type HashCode = u64;

pub trait Kvp<K, V>: Clone {
    fn key(&self) -> &K;

    fn value(&self) -> &V;

    fn key_value(&self) -> (&K, &V) {
        (self.key(), self.value())
    }
}

impl<K, V> Kvp<K, V> for Arc<(K, V)> {
    fn key(&self) -> &K {
        &self.0
    }

    fn value(&self) -> &V {
        &self.1
    }
}

impl<K, V> Kvp<K, V> for Rc<(K, V)> {
    fn key(&self) -> &K {
        &self.0
    }

    fn value(&self) -> &V {
        &self.1
    }
}

#[repr(C)]
#[derive(Clone)]
pub struct Pair<K, V> {
    key: K,
    value: V,
}

impl<K: Clone, V: Clone> Kvp<K, V> for Pair<K, V> {
    fn key(&self) -> &K {
        &self.key
    }

    fn value(&self) -> &V {
        &self.value
    }
}

/// # Safety
///
/// This trait is somewhat intricate and must be implemented with great care to avoid memory errors, undefined behavior,
/// etc.
pub unsafe trait HamtAllocator<K: Eq + Hash, V>: Copy {
    /// The type of a pointer to a HAMT node.
    type Pointer<T: HamtNode<K, V, Self> + ?Sized>: Clone + Deref<Target = T>;

    // The type of a key-value pair stored in the HAMT.
    type WrappedKvp: Kvp<K, V>;

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

#[derive(Clone, Copy)]
pub struct RcGlobal;

unsafe impl<K: Eq + Hash, V> HamtAllocator<K, V> for RcGlobal {
    type Pointer<T: HamtNode<K, V, Self> + ?Sized> = Rc<T>;

    type WrappedKvp = Rc<(K, V)>;

    fn wrap_kvp(k: K, v: V) -> Self::WrappedKvp {
        Rc::new((k, v))
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
            let arc = Rc::<[usize]>::new_zeroed_slice_in(len, Global);
            let thin_ptr = Rc::into_raw(arc);
            let ptr = core::ptr::from_raw_parts_mut::<T>(thin_ptr as *mut (), metadata);

            // Check the ptr alignment matches the layout alignment
            debug_assert!((ptr as *const () as usize) % layout.align() == 0);

            ptr
        };

        init(unsafe { &mut *data });

        unsafe { Rc::from_raw(data) }
    }

    unsafe fn downgrade_ptr<T: HamtNode<K, V, Self> + ?Sized>(
        ptr: Self::Pointer<T>,
    ) -> Self::Pointer<NodeHeader<K, V, Self>> {
        unsafe {
            let raw = ptr.header() as *const _ as *const NodeHeader<K, V, Self>;
            Rc::increment_strong_count(raw);
            Rc::from_raw(raw)
        }
    }

    unsafe fn ptr_from_ref_reinterpret<T: HamtNode<K, V, Self> + ?Sized>(
        ptr: &T,
    ) -> Self::Pointer<NodeHeader<K, V, Self>> {
        unsafe {
            let raw = ptr.header() as *const _ as *const NodeHeader<K, V, Self>;
            Rc::increment_strong_count(raw);
            Rc::from_raw(raw)
        }
    }
}

#[derive(Copy, Clone)]
pub struct CloneKvpArcGlobal;

unsafe impl<K: Eq + Hash + Clone, V: Clone> HamtAllocator<K, V> for CloneKvpArcGlobal {
    type Pointer<T: HamtNode<K, V, Self> + ?Sized> = Arc<T>;

    type WrappedKvp = Pair<K, V>;

    fn wrap_kvp(k: K, v: V) -> Self::WrappedKvp {
        Pair { key: k, value: v }
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

impl<K, Q, V, HamtHasher: Hasher + Default, Alloc: HamtAllocator<K, V>> Index<&Q>
    for Hamt<K, V, HamtHasher, Alloc>
where
    K: Eq + Hash,
    Q: Borrow<K> + ?Sized,
{
    type Output = V;

    fn index(&self, key: &Q) -> &Self::Output {
        self.get(key.borrow()).expect("no entry found for key")
    }
}

struct HamtIterator<'a, K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    // We don't edit this. Only hold on to it so that no underlying memory is freed until this iterator is dropped.
    _root: Option<Alloc::Pointer<NodeHeader<K, V, Alloc>>>,
    size: usize,
    stack: [NodeCursor<'a, K, V, Alloc>; MAX_LEVEL],
}

struct NodeCursor<'a, K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    node: Option<&'a NodeHeader<K, V, Alloc>>,
    index: usize,
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> Copy for NodeCursor<'_, K, V, Alloc> {}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> Clone for NodeCursor<'_, K, V, Alloc> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> Default for NodeCursor<'_, K, V, Alloc> {
    fn default() -> Self {
        Self {
            node: None,
            index: 0,
        }
    }
}

impl<'a, K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> HamtIterator<'a, K, V, Alloc> {
    fn new(root: Option<Alloc::Pointer<NodeHeader<K, V, Alloc>>>) -> Self {
        if let Some(rn) = root.as_ref() {
            let mut r = Self {
                _root: root.clone(),
                size: 1,
                stack: [Default::default(); MAX_LEVEL],
            };

            r.stack[0] = NodeCursor {
                index: 0,
                node: Some(unsafe {
                    core::mem::transmute::<&NodeHeader<K, V, Alloc>, &'a NodeHeader<K, V, Alloc>>(
                        rn.deref(),
                    )
                }),
            };

            r.find_leaf();

            r
        } else {
            Self {
                _root: None,
                size: 0,
                stack: [Default::default(); MAX_LEVEL],
            }
        }
    }

    fn is_empty(&self) -> bool {
        self.size == 0
    }

    fn push(&mut self, node: &NodeHeader<K, V, Alloc>) {
        assert!(self.size < MAX_LEVEL);

        let cursor = NodeCursor {
            node: Some(unsafe {
                core::mem::transmute::<&NodeHeader<K, V, Alloc>, &'a NodeHeader<K, V, Alloc>>(node)
            }),
            index: 0,
        };

        self.stack[{
            let s = self.size;
            self.size += 1;
            s
        }] = cursor;
    }

    fn cur(&self) -> NodeCursor<'a, K, V, Alloc> {
        self.stack[self.size - 1]
    }

    fn cur_mut(&mut self) -> &mut NodeCursor<'a, K, V, Alloc> {
        &mut self.stack[self.size - 1]
    }

    fn find_leaf(&mut self) -> &'a Alloc::WrappedKvp {
        let mut cursor = self.cur();

        while let Some(node) = &cursor.node {
            match node.upgrade() {
                NodePtr::Leaf(l) => {
                    return &l.values[cursor.index];
                }
                NodePtr::Inner(node) => {
                    self.push(&node.children[cursor.index]);
                    cursor = self.cur();
                }
            }
        }

        unreachable!();
    }

    fn shift_cursor(&mut self) {
        while self.size > 0 {
            let cursor = self.cur_mut();
            let node = cursor.node.as_ref().expect("unreachable None node in shr");

            match node.upgrade() {
                NodePtr::Leaf(l) => {
                    if cursor.index < l.values.len() - 1 {
                        cursor.index += 1;
                        break;
                    } else {
                        self.size -= 1;
                    }
                }
                NodePtr::Inner(i) => {
                    if cursor.index < i.children.len() - 1 {
                        cursor.index += 1;
                        break;
                    } else {
                        self.size -= 1;
                    }
                }
            }
        }
    }
}

impl<'a, K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> Iterator for HamtIterator<'a, K, V, Alloc> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            return None;
        }

        let leaf = self.find_leaf();
        let (k, v) = leaf.key_value();

        // eprintln!("Ret : {:p}, {:p}", k, v);

        self.shift_cursor();

        Some(unsafe {
            (
                core::mem::transmute::<&K, &'a K>(k),
                core::mem::transmute::<&V, &'a V>(v),
            )
        })
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

        self.root
            .as_ref()
            .and_then(|root| root.get(key, hash))
            .map(|kvp| kvp.value())
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
                root: Some(root.insert(key, value, hash)),
            },
            None => Self {
                _ph: PhantomData,
                root: Some(LeafNode::<K, V, Alloc>::create_with_pair(key, value, hash)),
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
                root: root.remove(key, hash),
            },
            None => Self {
                _ph: PhantomData,
                root: None,
            },
        }
    }

    /// Iterate over the key-value pairs in the map.
    ///
    /// The order of iteration is not guaranteed (it is determined by the hashing function and order of insertion for
    /// collisions).
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        HamtIterator::<K, V, Alloc>::new(self.root.clone())
    }

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
                    println!("{}- Leaf (hash: {:#066b}):", spaces, leaf._header.hash);
                    for kvp in leaf.values.iter() {
                        println!("{}  - {:?}: {:?}", spaces, kvp.key(), kvp.value());
                    }
                }
                NodePtr::Inner(node) => {
                    println!(
                        "{}- Node ({}): {:#066b}",
                        spaces,
                        node._header.level(),
                        node.bitmap
                    );
                    println!("{}  Hash: {:#066b}", spaces, node._header.hash);
                    for child in node.children.iter() {
                        print_node::<K, V, Alloc>(child, level + 1);
                    }
                }
            }
        }
    }
}

#[derive(Default)]
pub struct HamtVec<V, Alloc: HamtAllocator<(), V> = DefaultGlobal> {
    size: usize,
    root: Option<Alloc::Pointer<NodeHeader<(), V, Alloc>>>,
}

impl<V, Alloc: HamtAllocator<(), V>> HamtVec<V, Alloc> {
    pub fn new() -> Self {
        Self {
            size: 0,
            root: None,
        }
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn get(&self, index: usize) -> Option<&V> {
        if index >= self.size {
            None
        } else {
            self.root
                .as_ref()
                .and_then(|root| root.get(&(), index as HashCode))
                .map(|kvp| kvp.value())
        }
    }

    pub fn push(&self, v: V) -> Self {
        Self {
            size: self.size + 1,
            root: match self.root.as_ref() {
                Some(root) => Some(root.insert((), v, self.size as HashCode)),
                None => Some(LeafNode::<(), V, Alloc>::create_with_pair(
                    (),
                    v,
                    self.size as HashCode,
                )),
            },
        }
    }

    pub fn pop(&self) -> Option<(impl Deref<Target = V>, Self)> {
        if self.size == 0 {
            None
        } else {
            struct ValueGuard<T, V> {
                kvp: T,
                _ph: PhantomData<V>,
            }

            impl<T: Kvp<(), V>, V> ValueGuard<T, V> {
                pub fn new(kvp: T) -> Self {
                    Self {
                        kvp,
                        _ph: PhantomData,
                    }
                }
            }

            impl<T: Kvp<(), V>, V> Deref for ValueGuard<T, V> {
                type Target = V;

                fn deref(&self) -> &Self::Target {
                    self.kvp.value()
                }
            }

            let root = self
                .root
                .as_ref()
                .expect("no root node when size is nonzero");

            let kvp = root
                .get(&(), self.size as HashCode - 1)
                .expect("failed to get known existing value");

            let root = root
                .remove(&(), self.size as HashCode - 1)
                .expect("failed to remove value");

            Some((
                ValueGuard::new(kvp.clone()),
                Self {
                    size: self.size - 1,
                    root: Some(root),
                },
            ))
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &V> {
        HamtIterator::<(), V, Alloc>::new(self.root.clone()).map(|v| v.1)
    }

    #[cfg(all(test, feature = "std"))]
    pub fn print(&self)
    where
        V: core::fmt::Debug,
    {
        if self.root.is_none() {
            println!("Empty HAMT.");
            return;
        }

        print_node::<(), V, Alloc>(self.root.as_ref().unwrap(), 0);

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
                    println!("{}- Leaf (hash: {:#066b}):", spaces, leaf._header.hash);
                    for kvp in leaf.values.iter() {
                        println!("{}  - {:?}: {:?}", spaces, kvp.key(), kvp.value());
                    }
                }
                NodePtr::Inner(node) => {
                    println!(
                        "{}- Node ({}): {:#066b}",
                        spaces,
                        node._header.level(),
                        node.bitmap
                    );
                    for child in node.children.iter() {
                        print_node::<K, V, Alloc>(child, level + 1);
                    }
                }
            }
        }
    }
}

#[derive(Default)]
pub struct HamtSet<
    K: Eq + Hash,
    #[cfg(feature = "std")] HamtHasher: Hasher + Default = std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))] HamtHasher: Hasher + Default,
    Alloc: HamtAllocator<K, ()> = DefaultGlobal,
> {
    _ph: PhantomData<HamtHasher>,
    root: Option<Alloc::Pointer<NodeHeader<K, (), Alloc>>>,
}

impl<K: Eq + Hash> HamtSet<K> {
    pub fn new() -> Self {
        Self {
            _ph: PhantomData,
            root: None,
        }
    }
}

impl<K: Eq + Hash, HamtHasher: Hasher + Default, Alloc: HamtAllocator<K, ()>>
    HamtSet<K, HamtHasher, Alloc>
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
                root: Some(LeafNode::<K, (), Alloc>::create_with_pair(k, (), hash)),
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

impl<V, Alloc: HamtAllocator<(), V>> Index<usize> for HamtVec<V, Alloc> {
    type Output = V;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap_or_else(|| {
            panic!(
                "index out of bounds: the len is {} but the index is {}",
                self.size, index
            )
        })
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
    packed: usize,
    hash: HashCode,
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> Clone for NodeHeader<K, V, Alloc> {
    fn clone(&self) -> Self {
        Self {
            _ph: PhantomData,
            packed: self.packed,
            hash: self.hash,
        }
    }
}

#[cfg(target_pointer_width = "64")]
impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> NodeHeader<K, V, Alloc> {
    const TAG_MASK: usize = 0b111 << 61;
    const LEVEL_MASK: usize = 0b1111 << 57;
    const SIZE_MASK: usize = !(Self::TAG_MASK | Self::LEVEL_MASK);

    unsafe fn new<T: HamtNode<K, V, Alloc> + ?Sized>(
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

    fn level(&self) -> usize {
        (self.packed & Self::LEVEL_MASK) >> 57
    }

    fn metadata(&self) -> usize {
        self.packed & Self::SIZE_MASK
    }

    fn get(&self, k: &K, hash: HashCode) -> Option<&Alloc::WrappedKvp> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.get(k, hash),
            NodePtr::Inner(node) => node.get(k, hash),
        }
    }

    fn insert(&self, k: K, v: V, hash: HashCode) -> Alloc::Pointer<Self> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.insert(k, v, hash),
            NodePtr::Inner(node) => node.insert(k, v, hash),
        }
    }

    fn remove(&self, k: &K, hash: HashCode) -> Option<Alloc::Pointer<Self>> {
        match self.upgrade() {
            NodePtr::Leaf(node) => node.remove(k, hash),
            NodePtr::Inner(node) => node.remove(k, hash),
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
const MAX_LEVEL: usize = (core::mem::size_of::<Bitmap>() * 8 / BITMAP_INDEX_BITS) + 1;

const LEVEL_MUL_TABLE: [usize; MAX_LEVEL] = [
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

fn hash_bits_for_level(hash: HashCode, level: usize) -> Bitmap {
    debug_assert!(level < MAX_LEVEL);

    let inverse_level = MAX_LEVEL - level - 1;

    (hash >> LEVEL_MUL_TABLE[inverse_level]) & BITMAP_MASK
}

// fn hash_masked_for_level(hash: HashCode, level: usize) -> HashCode {
//     const SIZE_BITS: usize = core::mem::size_of::<HashCode>() * 8;
//     let n = level * BITMAP_INDEX_BITS;

//     let high_n_bits_mask = if level == 0 {
//         0
//     } else if n >= SIZE_BITS {
//         hash
//     } else {
//         eprintln!("n: {}, SIZE_BITS: {:#066b}", n, SIZE_BITS);
//         !((1u64 << (SIZE_BITS - n)).wrapping_sub(1))
//     };

//     hash & high_n_bits_mask
// }

#[repr(C)]
struct InnerNode<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> {
    _header: NodeHeader<K, V, Alloc>,
    bitmap: Bitmap,
    children: [Alloc::Pointer<NodeHeader<K, V, Alloc>>],
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> InnerNode<K, V, Alloc> {
    fn level(&self) -> usize {
        self._header.level()
    }

    fn hash(&self) -> HashCode {
        self._header.hash
    }

    fn get(&self, k: &K, hash: HashCode) -> Option<&Alloc::WrappedKvp> {
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

    // fn get_child(
    //     &self,
    //     hash: HashCode,
    //     level: usize,
    // ) -> Option<&Alloc::Pointer<NodeHeader<K, V, Alloc>>> {
    //     debug_assert!(level <= MAX_LEVEL);

    //     let index = hash_bits_for_level(hash, level);

    //     let masked_bitmap = ((1 << index) - 1) & self.bitmap;

    //     let pop = masked_bitmap.count_ones() as usize;

    //     if pop >= self.children.len() {
    //         None
    //     } else {
    //         Some(&self.children[pop as usize])
    //     }
    // }

    fn insert(&self, key: K, value: V, hash: HashCode) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
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

            let new_inner = unsafe {
                Alloc::allocate::<Self>(2, |inner| {
                    write(
                        &mut inner._header,
                        NodeHeader::new::<Self>(regression_level, 2, hash),
                    );

                    inner.bitmap = bitmap;

                    inner.bitmap = bitmap;

                    let new_leaf = LeafNode::<K, V, Alloc>::create_with_pair(key, value, hash);
                    let self_leaf = Alloc::ptr_from_ref_reinterpret(self);

                    match new_bits_for_next_level.cmp(&self_bits_for_next_level) {
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
                })
            };

            return unsafe { Alloc::downgrade_ptr(new_inner) };
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
    ) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        // Allocate a new InnerNode with a new bitmap with the `index`th bit set,
        // and with a new single-value leaf node allocated with (k, v) at index `pop`.

        let new_leaf = LeafNode::<K, V, Alloc>::create_with_pair(key, value, hash);

        // eprintln!("Bitmap was : {:#066b}", self.bitmap);
        let new_bitmap = self.bitmap | (1 << index);
        // eprintln!("Bitmap next: {:#066b}, from index {}", new_bitmap, index);

        let new_inner = unsafe {
            Alloc::allocate::<Self>(self.children.len() + 1, move |inner| {
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
        };

        unsafe { Alloc::downgrade_ptr(new_inner) }
    }

    fn insert_occupied(
        &self,
        key: K,
        value: V,
        hash: HashCode,
        pop: usize,
    ) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        // Allocate a new InnerNode with the `pop`th child replaced by the result
        // of inserting k, v into the child.
        let new_child = self.children[pop].insert(key, value, hash);

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

    fn remove(&self, key: &K, hash: HashCode) -> Option<Alloc::Pointer<NodeHeader<K, V, Alloc>>> {
        debug_assert!(self.level() <= MAX_LEVEL);

        let index = hash_bits_for_level(hash, self.level());

        let occupied = (self.bitmap >> index & 1) == 1;

        if !occupied {
            // The key is not in the map.
            return Some(unsafe { Alloc::ptr_from_ref_reinterpret(self) });
        }

        let masked_bitmap = ((1 << index) - 1) & self.bitmap;
        let pop = masked_bitmap.count_ones() as usize;

        let next = self.children[pop].remove(key, hash);

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

            // If we ended up with zero children after the removal, this node is pruned and we can return None.
            let new_len = self.children.len() - 1;
            if new_len == 0 {
                None
            } else if new_len == 1 {
                // If we ended up with one child after the removal, we can just return it.
                Some(self.children[if pop == 0 { 1 } else { 0 }].clone())
            } else {
                let new_bitmap = self.bitmap & !(1 << index);

                let new_inner = unsafe {
                    Alloc::allocate::<Self>(new_len, |inner| {
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
    // path: Path,
    values: [Alloc::WrappedKvp],
}

impl<K: Eq + Hash, V, Alloc: HamtAllocator<K, V>> LeafNode<K, V, Alloc> {
    fn hash(&self) -> HashCode {
        self._header.hash
    }

    fn get(&self, key: &K, hash: HashCode) -> Option<&Alloc::WrappedKvp> {
        if hash != self.hash() {
            None
        } else {
            self.values.iter().find(|kvp| kvp.key() == key)
        }
    }

    fn insert(&self, key: K, value: V, hash: HashCode) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        // debug_assert!(path.level == self.path.level);
        // eprintln!(
        //     "Inserting hash {:#066b} into leaf node with hash {:#066b}",
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
                Alloc::allocate::<InnerNode<K, V, Alloc>>(2, move |inner| {
                    write(
                        &mut inner._header,
                        NodeHeader::new::<InnerNode<K, V, Alloc>>(next_level, 2, hash),
                    );
                    inner.bitmap = bitmap;

                    let new_leaf = Self::create_with_pair(key, value, hash);
                    let self_leaf = Alloc::ptr_from_ref_reinterpret(self);

                    match new_bits_for_next_level.cmp(&self_bits_for_next_level) {
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
                })
            };

            unsafe { Alloc::downgrade_ptr(new_inner) }
        }
    }

    fn remove(&self, key: &K, hash: HashCode) -> Option<Alloc::Pointer<NodeHeader<K, V, Alloc>>> {
        // debug_assert_eq!(
        //     hash_bits_for_level(path.hash, path.level),
        //     hash_bits_for_level(self.hash, path.level)
        // );

        if hash != self.hash() {
            // The key is not in the map.
            return Some(unsafe { Alloc::ptr_from_ref_reinterpret(self) });
        }

        let cur_value = self
            .values
            .iter()
            .enumerate()
            .find(|(_, kvp)| kvp.key() == key);

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
                            NodeHeader::new::<Self>(MAX_LEVEL, self.values.len() - 1, hash),
                        );
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

    fn create_with_pair(
        key: K,
        value: V,
        hash: HashCode,
    ) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        unsafe {
            Alloc::downgrade_ptr({
                Alloc::allocate::<LeafNode<K, V, Alloc>>(1, |leaf| {
                    write(
                        &mut leaf._header,
                        NodeHeader::new::<Self>(MAX_LEVEL, 1, hash),
                    );

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

    fn add_pair(&self, key: K, value: V) -> Alloc::Pointer<NodeHeader<K, V, Alloc>> {
        let cur_value = self
            .values
            .iter()
            .enumerate()
            .find(|(_, kvp)| kvp.key() == &key);

        if let Some((idx, _)) = cur_value {
            unsafe {
                Alloc::downgrade_ptr(Alloc::allocate::<Self>(self.values.len(), move |leaf| {
                    write(&mut leaf._header, self._header.clone());
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
                        NodeHeader::new::<Self>(MAX_LEVEL, self.values.len() + 1, self.hash()),
                    );
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
    use crate::hamt::{CloneKvpArcGlobal, HamtAllocator, LeafNode};

    use super::{DefaultGlobal, HamtVec, InnerNode};

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

        hamts[hamts.len() - 1].print();
        hamts[hamts.len() - 2].print();

        for (hamt_idx, hamt) in hamts.iter().enumerate() {
            for i in 0..=hamt_idx {
                assert_eq!(hamt.get(&(i as u64)), Some(&(i as u64 + 1)))
            }
        }

        hamts[hamts.len() - 1].print();

        let mut last_hamt = hamts[hamts.len() - 1].clone();

        for i in 0..=(HAMTS / 2) {
            last_hamt = last_hamt.remove(&(i * 2));

            assert_eq!(last_hamt.get(&(i * 2)), None);
        }

        last_hamt.print();

        for idx in 0..HAMTS {
            if idx % 2 == 0 {
                assert_eq!(last_hamt.get(&idx), None);
            } else {
                assert_eq!(last_hamt.get(&idx), Some(&(idx + 1)));
            }
        }
    }

    #[test]
    fn hamt_vec() {
        let mut hv = HamtVec::<i32, CloneKvpArcGlobal>::new();

        hv = hv.push(0);

        assert_eq!(hv.len(), 1);

        hv.push(1);

        assert_eq!(hv.len(), 1);

        hv = hv.push(1);

        hv.print();

        assert_eq!(hv.len(), 2);

        for i in 2..64 {
            hv = hv.push(i);
        }

        hv.print();

        assert_eq!(hv.len(), 64);

        hv = hv.push(64);

        for i in 65..128 {
            hv = hv.push(i);
        }

        hv.print();

        assert_eq!(hv.len(), 128);

        hv = hv.push(128);

        hv.print()
    }

    #[test]
    fn hv_iter() {
        let v = [0, 1, 2, 3, 4, 5, 6, 7];
        let mut hv = HamtVec::<i32, CloneKvpArcGlobal>::new();

        for i in v.iter() {
            hv = hv.push(*i);
        }

        for (idx, v) in hv.iter().enumerate() {
            // eprintln!("Checking index: {} with value {}", idx, v);
            assert_eq!(*v, idx as i32);
        }
    }

    #[test]
    fn check_align() {
        // Check that the offset of _header within the node type is 0 i.e. that a pointer
        // to the node type is also a pointer to the header.

        let data = DefaultGlobal::allocate::<LeafNode<(), (), DefaultGlobal>>(0, |leaf| {
            unsafe {
                core::ptr::write(
                    &mut leaf._header,
                    crate::hamt::NodeHeader::new::<LeafNode<(), (), DefaultGlobal>>(
                        super::MAX_LEVEL,
                        0,
                        0,
                    ),
                )
            };
        });

        let node_ptr = data.as_ref() as *const _ as *const ();
        let node_header_ptr = &data._header as *const _ as *const ();

        assert_eq!(node_ptr as usize, node_header_ptr as usize);

        let data = DefaultGlobal::allocate::<InnerNode<(), (), DefaultGlobal>>(0, |inner| {
            unsafe {
                core::ptr::write(
                    &mut inner._header,
                    crate::hamt::NodeHeader::new::<InnerNode<(), (), DefaultGlobal>>(
                        super::MAX_LEVEL,
                        0,
                        0,
                    ),
                )
            };
        });

        let node_ptr = data.as_ref() as *const _ as *const ();
        let node_header_ptr = &data._header as *const _ as *const ();

        assert_eq!(node_ptr as usize, node_header_ptr as usize);
    }
}
