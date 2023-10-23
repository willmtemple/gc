use core::{
    alloc::Allocator,
    hash::{BuildHasher, Hash},
    ptr::Pointee,
};

use alloc::{alloc::Global, sync::Arc};

use crate::node::{HamtNode, NodeHeader};

use super::{common, HamtConfig};

pub struct ArcConfig<H: BuildHasher + Clone = super::DefaultHasher, A: Allocator + Clone = Global> {
    build_hasher: H,
    allocator: A,
}

impl<H: BuildHasher + Clone, A: Allocator + Clone> Clone for ArcConfig<H, A> {
    fn clone(&self) -> Self {
        Self {
            build_hasher: self.build_hasher.clone(),
            allocator: self.allocator.clone(),
        }
    }
}

impl Default for ArcConfig {
    fn default() -> Self {
        Self::new(Default::default(), Global)
    }
}

impl<H: BuildHasher + Clone, A: Allocator + Clone> ArcConfig<H, A> {
    pub fn new(build_hasher: H, allocator: A) -> Self {
        Self {
            build_hasher,
            allocator,
        }
    }
}

unsafe impl<H: BuildHasher + Clone, A: Allocator + Clone, K: Eq + Hash, V> HamtConfig<K, V>
    for ArcConfig<H, A>
{
    type Allocator = A;
    type BuildHasher = H;

    type NodeStore = Arc<NodeHeader<K, V, Self>, Self::Allocator>;
    type Kvp = Arc<(K, V), A>;

    fn allocate<T: HamtNode<K, V, Self> + ?Sized>(
        &self,
        metadata: <T as Pointee>::Metadata,
        init: impl FnOnce(&mut T),
    ) -> Self::NodeStore {
        common::allocate_refcounted(self.allocator.clone(), metadata, init)
    }

    fn upgrade_ref<T: HamtNode<K, V, Self> + ?Sized>(&self, ptr: &T) -> Self::NodeStore {
        common::upgrade_refcounted(self.allocator.clone(), ptr)
    }

    fn allocator(&self) -> Self::Allocator {
        self.allocator.clone()
    }

    fn build_hasher(&self) -> Self::BuildHasher {
        self.build_hasher.clone()
    }
}
