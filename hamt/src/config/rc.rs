use core::{
    alloc::Allocator,
    hash::{BuildHasher, Hash},
    ptr::Pointee,
};

use alloc::{alloc::Global, rc::Rc};

use crate::node::{HamtNode, NodeHeader};

use super::{common, DefaultHasher, HamtConfig};

#[derive(Clone, Copy)]
pub struct RcConfig<H: BuildHasher + Clone = DefaultHasher, A: Allocator + Clone = Global> {
    build_hasher: H,
    allocator: A,
}

impl Default for RcConfig {
    fn default() -> Self {
        Self::new(DefaultHasher::default(), Global)
    }
}

impl<H: BuildHasher + Clone, A: Allocator + Clone> RcConfig<H, A> {
    pub fn new(build_hasher: H, allocator: A) -> Self {
        Self {
            build_hasher,
            allocator,
        }
    }
}

unsafe impl<H: BuildHasher + Clone, A: Allocator + Clone, K: Eq + Hash, V> HamtConfig<K, V>
    for RcConfig<H, A>
{
    type Allocator = A;

    type BuildHasher = H;

    type NodeStore = Rc<NodeHeader<K, V, Self>, A>;

    type Kvp = Rc<(K, V), A>;

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
