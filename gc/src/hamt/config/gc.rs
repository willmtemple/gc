use crate::{
    hamt::node::NodeHeader,
    mark::Mark,
    ptr::{Gc, Root},
};

use core::hash::Hash;

use super::{HamtConfig, Kvp};

#[derive(Clone, Copy)]
pub struct GcConfig;

impl<K, V> Kvp<K, V> for Gc<(K, V)> {
    fn key(&self) -> &K {
        &self.0
    }

    fn value(&self) -> &V {
        &self.1
    }
}

unsafe impl<K: Eq + Hash + Mark, V: Mark> HamtConfig<K, V> for GcConfig {
    type NodeStore = Gc<NodeHeader<K, V, Self>>;

    type Kvp = Gc<(K, V)>;

    fn wrap_kvp(k: K, v: V) -> Self::Kvp {
        unsafe { Root::from((k, v)).as_unrooted() }
    }

    fn allocate<T: crate::hamt::node::HamtNode<K, V, Self> + ?Sized>(
        _metadata: <T as core::ptr::Pointee>::Metadata,
        _f: impl FnOnce(&mut T),
    ) -> Self::NodeStore {
        todo!()
    }

    unsafe fn upgrade_ref<T: crate::hamt::node::HamtNode<K, V, Self> + ?Sized>(
        _ptr: &T,
    ) -> Self::NodeStore {
        todo!();
    }
}
