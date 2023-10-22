use core::{
    alloc::Layout,
    hash::Hash,
    ptr::{null, Pointee},
};

use alloc::{alloc::Global, rc::Rc};

use crate::hamt::node::{HamtNode, NodeHeader};

use super::HamtConfig;

#[derive(Clone, Copy)]
pub struct RcGlobal;

unsafe impl<K: Eq + Hash, V> HamtConfig<K, V> for RcGlobal {
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