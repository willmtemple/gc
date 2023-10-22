pub mod kvp;
pub use kvp::*;

pub mod arc;
pub use arc::ArcConfig;

pub mod rc;
pub use rc::RcGlobal;

pub mod clone;
pub use clone::CloningConfig;

mod common;

use core::{hash::Hash, ops::Deref, ptr::Pointee};

use super::node::{HamtNode, NodeHeader};

/// The default configuration for HAMTs. This configuration uses `Arc` for both pointers and key-value pairs, and it
/// allocates all nodes on the heap using the global allocator.
pub type DefaultConfig = ArcConfig;

/// # Safety
///
/// This trait is somewhat intricate and must be implemented with great care to avoid memory errors, undefined behavior,
/// etc.
pub unsafe trait HamtConfig<K: Eq + Hash, V>: Copy {
    /// The type of a pointer to a HAMT node.
    type NodeStore: Clone + Deref<Target = NodeHeader<K, V, Self>>;

    // The type of a key-value pair stored in the HAMT.
    type Kvp: Kvp<K, V>;

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
    ) -> Self::NodeStore;

    /// Reinterpret a ref to a HAMT node into a pointer to a Node header and convert it to a Ptr.
    ///
    /// # Safety
    ///
    /// This amounts to transmutation. This function yields undefined behavior if the underlying ref
    /// was not taken from a pointer that was obtained by calling `deref` on the result of `downgrade_ptr`.
    ///
    /// This should return a NEW pointer. In case of refcounting or other memory management schemes, treat
    /// this as if it creates a NEW, ADDITIONAL pointer.
    unsafe fn upgrade_ref<T: HamtNode<K, V, Self> + ?Sized>(ptr: &T) -> Self::NodeStore;
}
