use core::mem::MaybeUninit;
use core::ptr::{null, Pointee};
use core::{alloc::Layout, hash::Hash};

use alloc::rc::Rc;
use alloc::sync::Arc;

use crate::node::NodeHeader;
use crate::{node::HamtNode, HamtConfig};

/// # Safety
///
/// Don't screw this up. Only for internal use.
pub unsafe trait RefCountedPtr<K: Eq + Hash, V, Config: HamtConfig<K, V>> {
    type UninitSlice;

    fn new_zeroed_slice_in(len: usize, alloc: Config::Allocator) -> Self::UninitSlice;

    fn into_ptr(this: Self::UninitSlice) -> *const ();

    fn from_ptr(ptr: *const NodeHeader<K, V, Config>, a: Config::Allocator) -> Self;

    fn increment_strong_count(ptr: *const NodeHeader<K, V, Config>);
}

unsafe impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> RefCountedPtr<K, V, Config>
    for Arc<NodeHeader<K, V, Config>, Config::Allocator>
{
    type UninitSlice = Arc<[MaybeUninit<usize>], Config::Allocator>;

    fn new_zeroed_slice_in(len: usize, alloc: Config::Allocator) -> Self::UninitSlice {
        Arc::<[usize], Config::Allocator>::new_zeroed_slice_in(len, alloc)
    }

    fn into_ptr(this: Self::UninitSlice) -> *const () {
        Arc::into_raw(this) as *const ()
    }

    fn from_ptr(ptr: *const NodeHeader<K, V, Config>, a: Config::Allocator) -> Self {
        unsafe { Arc::<NodeHeader<K, V, Config>, Config::Allocator>::from_raw_in(ptr, a) }
    }

    fn increment_strong_count(ptr: *const NodeHeader<K, V, Config>) {
        unsafe { Arc::increment_strong_count(ptr) }
    }
}

unsafe impl<K: Eq + Hash, V, Config: HamtConfig<K, V>> RefCountedPtr<K, V, Config>
    for Rc<NodeHeader<K, V, Config>, Config::Allocator>
{
    type UninitSlice = Rc<[MaybeUninit<usize>], Config::Allocator>;

    fn new_zeroed_slice_in(len: usize, alloc: Config::Allocator) -> Self::UninitSlice {
        Rc::<[usize], Config::Allocator>::new_zeroed_slice_in(len, alloc)
    }

    fn into_ptr(this: Self::UninitSlice) -> *const () {
        Rc::into_raw(this) as *const ()
    }

    fn from_ptr(ptr: *const NodeHeader<K, V, Config>, a: Config::Allocator) -> Self {
        unsafe { Rc::<NodeHeader<K, V, Config>, Config::Allocator>::from_raw_in(ptr, a) }
    }

    fn increment_strong_count(ptr: *const NodeHeader<K, V, Config>) {
        unsafe { Rc::increment_strong_count(ptr) }
    }
}

pub fn allocate_refcounted<
    K: Eq + Hash,
    V,
    Config: HamtConfig<K, V>,
    T: HamtNode<K, V, Config> + ?Sized,
    Ptr: RefCountedPtr<K, V, Config>,
>(
    allocator: Config::Allocator,
    metadata: <T as Pointee>::Metadata,
    init: impl FnOnce(&mut T),
) -> Ptr {
    let layout = unsafe { Layout::for_value_raw(core::ptr::from_raw_parts::<T>(null(), metadata)) };

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
        let arc = Ptr::new_zeroed_slice_in(len, allocator.clone());
        let thin_ptr = Ptr::into_ptr(arc);
        let ptr = core::ptr::from_raw_parts_mut::<T>(thin_ptr as *mut (), metadata);

        // Check the ptr alignment matches the layout alignment
        debug_assert!((ptr as *const () as usize) % layout.align() == 0);

        ptr
    };

    init(unsafe { &mut *data });

    unsafe { Ptr::from_ptr((*data).header(), allocator) }
}

pub fn upgrade_refcounted<
    K: Eq + Hash,
    V,
    Config: HamtConfig<K, V>,
    T: HamtNode<K, V, Config> + ?Sized,
    Ptr: RefCountedPtr<K, V, Config>,
>(
    allocator: Config::Allocator,
    ptr: &T,
) -> Ptr {
    let raw = ptr.header() as *const _ as *const NodeHeader<K, V, Config>;
    Ptr::increment_strong_count(raw);
    Ptr::from_ptr(raw, allocator)
}
