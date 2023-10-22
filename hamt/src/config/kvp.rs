use alloc::{rc::Rc, sync::Arc};

pub trait Kvp<K, V>: Clone {
    fn new(key: K, value: V) -> Self;

    fn key(&self) -> &K;

    fn value(&self) -> &V;

    fn key_value(&self) -> (&K, &V) {
        (self.key(), self.value())
    }
}

impl<K, V> Kvp<K, V> for Arc<(K, V)> {
    fn new(key: K, value: V) -> Self {
        Arc::new((key, value))
    }

    fn key(&self) -> &K {
        &self.0
    }

    fn value(&self) -> &V {
        &self.1
    }
}

impl<K, V> Kvp<K, V> for Rc<(K, V)> {
    fn new(key: K, value: V) -> Self {
        Rc::new((key, value))
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

impl<K: Clone, V: Clone> Kvp<K, V> for Pair<K, V> {
    fn new(key: K, value: V) -> Self {
        Self { key, value }
    }

    fn key(&self) -> &K {
        &self.key
    }

    fn value(&self) -> &V {
        &self.value
    }
}
