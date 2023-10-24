pub mod kvp;
pub use kvp::*;

pub mod arc;
pub use arc::ArcConfig;

pub mod rc;
pub use rc::RcConfig;

pub mod clone;
pub use clone::CloningConfig;
pub use clone::CloningRcConfig;

mod common;

use core::{
    alloc::Allocator,
    hash::{BuildHasher, Hash},
    ops::Deref,
    ptr::Pointee,
};

use super::node::{HamtNode, NodeHeader};

/// The default configuration for HAMTs. This configuration uses `Arc` for both pointers and key-value pairs, and it
/// allocates all nodes on the heap using the global allocator. It uses the default hasher.
pub type DefaultConfig = ArcConfig;

/// The default hasher used by the default HAMT configuration.
///
/// When the `"std"` feature is enabled, this is an alias for the default HashMap hasher.
#[cfg(feature = "std")]
pub type DefaultHasher = core::hash::BuildHasherDefault<std::collections::hash_map::DefaultHasher>;

/// Configuration for a HAMT. Configuration is part of every HAMT collection type ([`HamtMap`], [`HamtSet`], [`HamtVec`])
/// and describes the behaviors of the HAMT.
///
/// It defines the:
/// - storage for nodes.
/// - storage for key-value pairs.
/// - allocator used to allocate nodes.
/// - hasher used to hash keys.
///
/// And it implements methods to:
/// - allocate nodes.
/// - wrap key-value pairs.
/// - to transmute node references to managed node header pointers.
///
/// # Safety
///
/// This trait is somewhat intricate and must be implemented with great care to avoid memory errors, undefined behavior,
/// etc.
pub unsafe trait HamtConfig<K: Eq + Hash, V>: Clone + Sized {
    /// The type of allocator that this config uses.
    type Allocator: Allocator + Clone;

    /// The type of hasher that this config uses (represented as a BuildHasher).
    type BuildHasher: BuildHasher;

    /// The type of a pointer to a HAMT node.
    type NodeStore: Clone + Deref<Target = NodeHeader<K, V, Self>>;

    // The type of a key-value pair stored in the HAMT.
    type Kvp: Kvp<K, V, Self::Allocator>;

    /// Gets a copy of the Allocator used by this config.
    fn allocator(&self) -> Self::Allocator;

    /// Gets a copy of the BuildHasher used by this config.
    fn build_hasher(&self) -> Self::BuildHasher;

    /// Allocate a region of memory for a node of type `T` with the given `metadata`.
    ///
    /// # Safety
    /// This function must implement manual memory allocation and initialization.
    ///
    /// Returning a pointer to memory that has not been initialized to zero will cause
    /// undefined behavior.
    ///
    /// Failing to honor the `metadata` parameter will cause undefined behavior.
    fn allocate<T: HamtNode<K, V, Self> + ?Sized>(
        &self,
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
    fn upgrade_ref<T: HamtNode<K, V, Self> + ?Sized>(&self, ptr: &T) -> Self::NodeStore;

    fn new_kvp(&self, k: K, v: V) -> Self::Kvp {
        Self::Kvp::new(k, v, self.allocator())
    }
}
