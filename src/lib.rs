#![cfg_attr(not(feature = "std"), no_std)]
#![feature(allocator_api)]
#![feature(ptr_metadata)]
#![feature(layout_for_ptr)]
#![feature(decl_macro)]
#![feature(associated_type_defaults)]
#![feature(new_uninit)]
#![feature(lazy_cell)]

extern crate alloc;

use core::{
    alloc::{AllocError, Allocator, Layout},
    ops::DerefMut,
    ptr::{null, NonNull, Pointee},
};

use alloc::alloc::Global;
use mark::Mark;
use obj::{AllocMetadata, AnyGcObject, GcObject, SizedMetadata};

pub mod mark;
pub mod obj;
pub mod ptr;

pub mod hamt;

#[cfg(not(feature = "std"))]
pub fn lock_default_gc<R, F: FnOnce(&mut DefaultGarbageCollector) -> R>(f: F) -> R {
    static GC: spin::Mutex<DefaultGarbageCollector> =
        spin::Mutex::new(DefaultGarbageCollector::new());

    f(GC.lock().deref_mut())
}

#[cfg(feature = "std")]
pub fn lock_default_gc<R, F: FnOnce(&mut DefaultGarbageCollector) -> R>(f: F) -> R {
    lazy_static::lazy_static!(
        static ref GC: std::sync::Mutex<DefaultGarbageCollector> =
            std::sync::Mutex::new(DefaultGarbageCollector::new());
    );

    f(GC.lock().expect("failed to acquire lock").deref_mut())
}

#[derive(Default)]
pub struct DefaultGarbageCollector {
    #[cfg(feature = "std")]
    roots: std::collections::HashSet<AnyGcObject>,
    #[cfg(not(feature = "std"))]
    roots: alloc::collections::BTreeSet<AnyGcObject>,
}

impl DefaultGarbageCollector {
    #[cfg(not(feature = "std"))]
    const fn new() -> Self {
        Self {
            roots: alloc::collections::BTreeSet::new(),
            roots: std::collections::HashSet::new(),
        }
    }

    #[cfg(feature = "std")]
    fn new() -> Self {
        Self {
            roots: std::collections::HashSet::new(),
        }
    }
}

#[cfg(not(feature = "std"))]
impl GarbageCollector for DefaultGarbageCollector {
    type Locked = spin::Mutex<Self>;

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
}

impl GcLock for spin::Mutex<DefaultGarbageCollector> {
    type Unlocked = DefaultGarbageCollector;

    fn new() -> Self {
        spin::Mutex::new(Self::Unlocked::default())
    }

    fn lock_gc<R, F: FnOnce(&mut Self::Unlocked) -> R>(&self, f: F) -> R {
        f(self.lock().deref_mut())
    }
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
}

pub trait GarbageCollector {
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
    ) -> Result<NonNull<GcObject<T>>, AllocError> {
        let layout = unsafe {
            Layout::for_value_raw(core::ptr::from_raw_parts::<GcObject<T>>(null(), metadata))
        };

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
