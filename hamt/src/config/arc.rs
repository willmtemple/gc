use core::{hash::Hash, ptr::Pointee};

use alloc::{alloc::Global, sync::Arc};

use crate::node::{HamtNode, NodeHeader};

use super::{common, HamtConfig};

#[derive(Clone, Copy)]
pub struct ArcConfig;

unsafe impl<K: Eq + Hash, V> HamtConfig<K, V> for ArcConfig {
    type NodeStore = Arc<NodeHeader<K, V, Self>>;

    type Kvp = Arc<(K, V)>;

    fn allocate<T: HamtNode<K, V, Self> + ?Sized>(
        metadata: <T as Pointee>::Metadata,
        init: impl FnOnce(&mut T),
    ) -> Self::NodeStore {
        common::allocate_refcounted(metadata, init, Global)
    }

    fn upgrade_ref<T: HamtNode<K, V, Self> + ?Sized>(ptr: &T) -> Self::NodeStore {
        common::upgrade_refcounted(ptr, Global)
    }
}
