use core::{alloc::Allocator, hash::Hash, ptr::Pointee};

use alloc::{alloc::Global, rc::Rc};

use crate::node::{HamtNode, NodeHeader};

use super::{common, HamtConfig};

#[derive(Clone, Copy)]
pub struct RcConfig<A: Allocator + Clone = Global> {
    allocator: A,
}

impl Default for RcConfig {
    fn default() -> Self {
        Self::new(Global)
    }
}

impl<A: Allocator + Clone> RcConfig<A> {
    pub fn new(allocator: A) -> Self {
        Self { allocator }
    }
}

unsafe impl<K: Eq + Hash, V> HamtConfig<K, V> for RcConfig {
    type NodeStore = Rc<NodeHeader<K, V, Self>>;

    type Kvp = Rc<(K, V)>;

    fn allocate<T: HamtNode<K, V, Self> + ?Sized>(
        &self,
        metadata: <T as Pointee>::Metadata,
        init: impl FnOnce(&mut T),
    ) -> Self::NodeStore {
        common::allocate_refcounted(metadata, init, self.allocator.clone())
    }

    fn upgrade_ref<T: HamtNode<K, V, Self> + ?Sized>(&self, ptr: &T) -> Self::NodeStore {
        common::upgrade_refcounted(ptr, self.allocator.clone())
    }
}
