use core::{hash::Hash, ptr::Pointee};

use alloc::{alloc::Global, rc::Rc};

use crate::node::{HamtNode, NodeHeader};

use super::{common, HamtConfig};

#[derive(Clone, Copy)]
pub struct RcGlobal;

unsafe impl<K: Eq + Hash, V> HamtConfig<K, V> for RcGlobal {
    type NodeStore = Rc<NodeHeader<K, V, Self>>;

    type Kvp = Rc<(K, V)>;

    fn wrap_kvp(k: K, v: V) -> Self::Kvp {
        Rc::new((k, v))
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