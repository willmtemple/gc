use core::{hash::Hash, ptr::Pointee};

use alloc::{alloc::Global, sync::Arc};

use crate::node::{HamtNode, NodeHeader};

use super::{common, HamtConfig, Pair};

#[derive(Copy, Clone)]
pub struct CloneKvpArcGlobal;

unsafe impl<K: Eq + Hash + Clone, V: Clone> HamtConfig<K, V> for CloneKvpArcGlobal {
    type NodeStore = Arc<NodeHeader<K, V, Self>>;

    type Kvp = Pair<K, V>;

    fn wrap_kvp(k: K, v: V) -> Self::Kvp {
        Pair::new(k, v)
    }

    fn allocate<T: HamtNode<K, V, Self> + ?Sized>(
        metadata: <T as Pointee>::Metadata,
        init: impl FnOnce(&mut T),
    ) -> Self::NodeStore {
        common::allocate_refcounted(metadata, init, Global)
    }

    unsafe fn upgrade_ref<T: HamtNode<K, V, Self> + ?Sized>(ptr: &T) -> Self::NodeStore {
        common::upgrade_refcounted(ptr, Global)
    }
}
