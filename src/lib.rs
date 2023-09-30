#![cfg_attr(not(feature = "std"), no_std)]
#![feature(allocator_api)]
#![feature(ptr_metadata)]
#![feature(layout_for_ptr)]
#![feature(decl_macro)]

extern crate alloc;

use core::{
    alloc::{AllocError, Allocator, Layout},
    ops::DerefMut,
    ptr::{null, NonNull, Pointee},
};

use alloc::{alloc::Global, sync::Arc};
use lazy_static::lazy_static;
use mark::Mark;
use obj::{AllocMetadata, AnyGcObject, GcObject, SizedMetadata};

pub mod mark;
pub mod obj;
pub mod ptr;

lazy_static! {
    pub static ref GC: Arc<<DefaultGarbageCollector as GarbageCollector>::Locked> =
        Arc::new(<<DefaultGarbageCollector as GarbageCollector>::Locked as GcLock>::new());
}

pub fn lock_default_gc<R, F: FnOnce(&mut DefaultGarbageCollector) -> R>(f: F) -> R {
    GC.lock_gc(f)
}

#[derive(Default)]
pub struct DefaultGarbageCollector {
    #[cfg(feature = "std")]
    roots: std::collections::HashSet<AnyGcObject>,
}

#[cfg(feature = "std")]
impl GarbageCollector for DefaultGarbageCollector {
    type Locked = std::sync::Mutex<Self>;

    fn root<T: Mark + ?Sized>(&mut self, ptr: NonNull<GcObject<T>>)
    where
        <GcObject<T> as Pointee>::Metadata: SizedMetadata,
    {
        self.roots.insert(AnyGcObject::new(ptr));
    }

    fn unroot<T: Mark + ?Sized>(&mut self, ptr: NonNull<GcObject<T>>)
    where
        <GcObject<T> as Pointee>::Metadata: SizedMetadata,
    {
        self.roots.remove(&AnyGcObject::new(ptr));
    }

    fn allocate<T: Mark + ?Sized>(
        &mut self,
        metadata: <GcObject<T> as core::ptr::Pointee>::Metadata,
    ) -> Result<NonNull<GcObject<T>>, AllocError> {
        let layout = unsafe {
            Layout::for_value_raw(
                // # Safety
                // This relies on the compiler to optimize &*null() by not _actually_ dereferencing it.
                core::ptr::from_raw_parts::<GcObject<T>>(null(), metadata),
            )
        };

        // GcObject<T> is a DST so we need to allocate it
        let gc_object = unsafe {
            let ptr = Global.allocate_zeroed(layout)?.cast::<()>();

            core::ptr::from_raw_parts_mut::<GcObject<T>>(ptr.as_ptr(), metadata)
                .as_mut()
                .unwrap()
        };

        gc_object.header.ptr_metadata = metadata;
        gc_object.header.alloc_metadata = AllocMetadata::new(false, layout.size());

        Ok(NonNull::from(gc_object))
    }
}

pub trait GarbageCollector: Default {
    type Locked: GcLock;

    fn root<T: Mark + ?Sized>(&mut self, ptr: NonNull<GcObject<T>>)
    where
        <GcObject<T> as Pointee>::Metadata: SizedMetadata;

    fn unroot<T: Mark + ?Sized>(&mut self, ptr: NonNull<GcObject<T>>)
    where
        <GcObject<T> as Pointee>::Metadata: SizedMetadata;

    fn allocate<T: Mark + ?Sized>(
        &mut self,
        metadata: <GcObject<T> as core::ptr::Pointee>::Metadata,
    ) -> Result<NonNull<GcObject<T>>, AllocError>;
}

pub trait GcLock {
    type Unlocked: GarbageCollector;
    fn new() -> Self;
    fn lock_gc<R, F: FnOnce(&mut Self::Unlocked) -> R>(&self, f: F) -> R;
}

#[cfg(feature = "std")]
impl GcLock for std::sync::Mutex<DefaultGarbageCollector> {
    type Unlocked = DefaultGarbageCollector;

    fn new() -> Self {
        std::sync::Mutex::new(Self::Unlocked::default())
    }

    fn lock_gc<R, F: FnOnce(&mut Self::Unlocked) -> R>(&self, f: F) -> R {
        f(self.lock().expect("failed to acquire lock").deref_mut())
    }
}

#[cfg(test)]
mod tests {
    use crate::ptr::GcRoot;

    #[test]
    fn test_gc() {
        let r = GcRoot::<str>::from("Hello world!");

        assert_eq!(r.len(), 12);

        assert_eq!(&*r, "Hello world!");
    }
}
