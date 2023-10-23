use core::alloc::Allocator;

use alloc::{rc::Rc, sync::Arc};

pub trait Kvp<K, V, A: Allocator>: Clone {
    fn new(key: K, value: V, a: A) -> Self;

    fn key(&self) -> &K;

    fn value(&self) -> &V;

    fn key_value(&self) -> (&K, &V) {
        (self.key(), self.value())
    }
}

impl<K, V, A: Allocator + Clone> Kvp<K, V, A> for Arc<(K, V), A> {
    fn new(key: K, value: V, a: A) -> Self {
        Arc::new_in((key, value), a)
    }

    fn key(&self) -> &K {
        &self.0
    }

    fn value(&self) -> &V {
        &self.1
    }
}

impl<K, V, A: Allocator + Clone> Kvp<K, V, A> for Rc<(K, V), A> {
    fn new(key: K, value: V, a: A) -> Self {
        Rc::new_in((key, value), a)
    }

    fn key(&self) -> &K {
        &self.0
    }

    fn value(&self) -> &V {
        &self.1
    }
}

#[repr(C)]
#[derive(Clone)]
pub struct Pair<K, V> {
    key: K,
    value: V,
}

impl<K: Clone, V: Clone, A: Allocator> Kvp<K, V, A> for Pair<K, V> {
    fn new(key: K, value: V, _: A) -> Self {
        Self { key, value }
    }

    fn key(&self) -> &K {
        &self.key
    }

    fn value(&self) -> &V {
        &self.value
    }
}
