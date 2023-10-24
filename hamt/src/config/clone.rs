use core::{
    alloc::Allocator,
    hash::{BuildHasher, Hash},
    ptr::Pointee,
};

use alloc::{alloc::Global, rc::Rc, sync::Arc};

use crate::node::{HamtNode, NodeHeader};

use super::{common, DefaultHasher, HamtConfig, Pair};

#[derive(Copy, Clone)]
pub struct CloningConfig<H: BuildHasher + Clone = DefaultHasher, A: Allocator + Clone = Global> {
    allocator: A,
    build_hasher: H,
}

impl Default for CloningConfig {
    fn default() -> Self {
        Self {
            allocator: Global,
            build_hasher: DefaultHasher::default(),
        }
    }
}

unsafe impl<H: BuildHasher + Clone, A: Allocator + Clone, K: Eq + Hash + Clone, V: Clone>
    HamtConfig<K, V> for CloningConfig<H, A>
{
    type Allocator = A;
    type BuildHasher = H;

    type NodeStore = Arc<NodeHeader<K, V, Self>, Self::Allocator>;
    type Kvp = Pair<K, V>;

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

#[derive(Copy, Clone)]
pub struct CloningRcConfig<H: BuildHasher + Clone = DefaultHasher, A: Allocator + Clone = Global> {
    inner: CloningConfig<H, A>,
}

impl Default for CloningRcConfig {
    fn default() -> Self {
        Self {
            inner: CloningConfig::default(),
        }
    }
}

unsafe impl<H: BuildHasher + Clone, A: Allocator + Clone, K: Eq + Hash + Clone, V: Clone>
    HamtConfig<K, V> for CloningRcConfig<H, A>
{
    type Allocator = A;
    type BuildHasher = H;

    type NodeStore = Rc<NodeHeader<K, V, Self>, Self::Allocator>;
    type Kvp = Pair<K, V>;

    fn allocate<T: HamtNode<K, V, Self> + ?Sized>(
        &self,
        metadata: <T as Pointee>::Metadata,
        init: impl FnOnce(&mut T),
    ) -> Self::NodeStore {
        common::allocate_refcounted(self.inner.allocator.clone(), metadata, init)
    }

    fn upgrade_ref<T: HamtNode<K, V, Self> + ?Sized>(&self, ptr: &T) -> Self::NodeStore {
        common::upgrade_refcounted(self.inner.allocator.clone(), ptr)
    }

    fn allocator(&self) -> Self::Allocator {
        self.inner.allocator.clone()
    }

    fn build_hasher(&self) -> Self::BuildHasher {
        self.inner.build_hasher.clone()
    }
}
