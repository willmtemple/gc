use core::{
    alloc::AllocError,
    ops::Deref,
    ptr::{NonNull, Pointee},
};

use crate::{
    lock_default_gc,
    mark::Mark,
    obj::{GcObject, GcObjectIdentity, UsizeMetadata},
    GarbageCollector,
};

#[repr(transparent)]
pub struct GcRoot<T: Mark + ?Sized>(pub(crate) Gc<T>)
where
    <GcObject<T> as Pointee>::Metadata: UsizeMetadata;

impl<T: Mark + ?Sized> Deref for GcRoot<T>
where
    <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Mark + ?Sized> GcRoot<T>
where
    <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
{
    /// # Safety
    /// Callers must ensure that the return value of this function is stored in a location that is connected to the GC
    /// root graph before dropping `self`, otherwise the object may be collected and the returned pointer may be
    /// invalidated.
    pub unsafe fn as_unrooted(&self) -> Gc<T> {
        self.0
    }

    unsafe fn new_uninitialized_with_metadata(
        metadata: <GcObject<T> as core::ptr::Pointee>::Metadata,
    ) -> Result<Self, AllocError> {
        lock_default_gc(|gc| {
            gc.allocate::<T>(metadata).map(|ptr| {
                gc.root(ptr);
                Self(Gc { ptr })
            })
        })
    }
}

impl<T: Mark> GcRoot<T> {
    unsafe fn new_uninitialized() -> Result<Self, AllocError> {
        Self::new_uninitialized_with_metadata(())
    }

    pub fn new(value: T) -> Result<Self, AllocError> {
        Ok(unsafe {
            let mut root = Self::new_uninitialized()?;

            root.0.ptr.as_mut().data = value;

            root
        })
    }
}

impl<T: Mark + ?Sized> GcRoot<T>
where
    <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
{
    unsafe fn from_raw(ptr: NonNull<GcObject<T>>) -> Self {
        lock_default_gc(|gc| {
            gc.root(ptr);
            Self(Gc { ptr })
        })
    }

    pub fn get_weak(&self) -> GcWeak<T> {
        self.0.get_weak()
    }
}

impl<T: Mark> From<T> for GcRoot<T> {
    fn from(value: T) -> Self {
        Self::new(value)
            .unwrap_or_else(|_| panic!("failed to allocate Gc<{}>", core::any::type_name::<T>()))
    }
}

impl From<&str> for GcRoot<str> {
    fn from(value: &str) -> Self {
        unsafe {
            let mut root =
                Self::new_uninitialized_with_metadata(value.len()).unwrap_or_else(|_| {
                    panic!("failed to allocate Gc<str> with length {}", value.len())
                });

            root.0
                .ptr
                .as_mut()
                .data
                .as_bytes_mut()
                .copy_from_slice(value.as_bytes());

            root
        }
    }
}

impl<T: Mark + ?Sized> Drop for GcRoot<T>
where
    <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
{
    fn drop(&mut self) {
        lock_default_gc(|gc| gc.unroot(self.0.ptr))
    }
}

#[repr(transparent)]
pub struct Gc<T: Mark + ?Sized> {
    pub(crate) ptr: NonNull<GcObject<T>>,
}

impl<T: Mark + ?Sized> Gc<T> {
    pub fn get_weak(&self) -> GcWeak<T> {
        GcWeak {
            ptr: self.ptr,
            identity: unsafe { &*self.ptr.as_ptr() }.header.identity,
        }
    }

    pub fn mark(&mut self) {
        unsafe { self.ptr.as_mut() }.mark()
    }
}

impl<T: Mark + ?Sized> Copy for Gc<T> {}

impl<T: Mark + ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Mark + ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &unsafe { self.ptr.as_ref() }.data
    }
}

pub struct GcWeak<T: Mark + ?Sized> {
    ptr: NonNull<GcObject<T>>,
    identity: GcObjectIdentity,
}

impl<T: Mark + ?Sized> Copy for GcWeak<T> {}

impl<T: Mark + ?Sized> Clone for GcWeak<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Mark + ?Sized> GcWeak<T> {
    pub fn upgrade(&self) -> Option<GcRoot<T>>
    where
        <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
    {
        if self.present() {
            Some(unsafe { GcRoot::from_raw(self.ptr) })
        } else {
            None
        }
    }

    pub fn upgrade_and_then<U>(&self, f: impl FnOnce(&T) -> U) -> Option<U>
    where
        <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
    {
        self.upgrade().map(|root| f(&root))
    }

    pub fn present(&self) -> bool {
        lock_default_gc(|gc| gc.is_present(self.ptr, self.identity))
    }
}
