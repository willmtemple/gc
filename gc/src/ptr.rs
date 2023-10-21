use core::{
    alloc::AllocError,
    ops::Deref,
    ptr::{NonNull, Pointee},
};

use crate::{
    lock_default_gc,
    mark::Mark,
    obj::{GcObjectIdentity, Object, UsizeMetadata},
    GarbageCollector,
};

/// A root pointer to a GC object.
///
/// Objects that root pointers refer to cannot be collected by the GC. Roots are atomically reference counted. Once all
/// references to the root object are disposed, the object may be eligible for collection.
///
/// This is the best type to use when working with GC objects on the Rust stack.
#[repr(transparent)]
pub struct Root<T: Mark + ?Sized>(pub(crate) Raw<T>)
where
    <Object<T> as Pointee>::Metadata: UsizeMetadata;

impl<T: Mark + ?Sized> Deref for Root<T>
where
    <Object<T> as Pointee>::Metadata: UsizeMetadata,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Mark + ?Sized> Root<T>
where
    <Object<T> as Pointee>::Metadata: UsizeMetadata,
{
    /// # Safety
    ///
    /// Callers must ensure that the return value of this function is stored in a location that is connected to the GC
    /// root graph before dropping `self`, otherwise the object may be collected and the returned pointer may be
    /// invalidated.
    pub unsafe fn as_unrooted(&self) -> Raw<T> {
        self.0
    }

    pub(crate) unsafe fn new_uninitialized_with_metadata(
        metadata: <Object<T> as core::ptr::Pointee>::Metadata,
    ) -> Result<Self, AllocError> {
        lock_default_gc(|gc| {
            gc.allocate::<T>(metadata).map(|ptr| {
                gc.root(ptr);
                Self(Raw { ptr })
            })
        })
    }
}

impl<T: Mark> Root<T> {
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

impl<T: Mark + ?Sized> Root<T>
where
    <Object<T> as Pointee>::Metadata: UsizeMetadata,
{
    /// Locks the GC and creates a new GC root from a raw, NonNull pointer.
    ///
    /// # Safety
    /// Callers must ensure that the pointer is valid and points to a GC object that has been initialized. The object
    /// is immediately rooted.
    pub unsafe fn from_raw(raw: Raw<T>) -> Self {
        Self(raw)
    }

    pub fn get_weak(&self) -> Weak<T> {
        self.0.get_weak()
    }
}

impl<T: Mark> From<T> for Root<T> {
    fn from(value: T) -> Self {
        Self::new(value)
            .unwrap_or_else(|_| panic!("failed to allocate Gc<{}>", core::any::type_name::<T>()))
    }
}

impl From<&str> for Root<str> {
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

impl<T: Mark + ?Sized> Clone for Root<T>
where
    <Object<T> as Pointee>::Metadata: UsizeMetadata,
{
    fn clone(&self) -> Self {
        lock_default_gc(|gc| {
            gc.root(self.0.ptr);
            Self(self.0)
        })
    }
}

impl<T: Mark + ?Sized> Drop for Root<T>
where
    <Object<T> as Pointee>::Metadata: UsizeMetadata,
{
    fn drop(&mut self) {
        lock_default_gc(|gc| gc.unroot(self.0.ptr))
    }
}

/// A raw GC pointer.
///
/// You should use this type when you are _certain_ that it is referred to by some rooted object.
///
/// # Safety
///
/// This object may be collected if no roots refer to it, and its liveness is not validated. For strict liveness
/// guarantees, use [`Root`] or [`Weak`].
#[repr(transparent)]
pub struct Raw<T: ?Sized> {
    pub(crate) ptr: NonNull<Object<T>>,
}

impl<T: Mark + ?Sized> Raw<T> {
    pub fn get_weak(&self) -> Weak<T> {
        Weak {
            ptr: *self,
            identity: unsafe { &*self.ptr.as_ptr() }.header.identity,
        }
    }
}

impl<T: Mark + ?Sized> Mark for Raw<T> {
    fn mark(&mut self) {
        unsafe { self.ptr.as_mut() }.mark()
    }
}

impl<T: ?Sized> Copy for Raw<T> {}

impl<T: ?Sized> Clone for Raw<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Deref for Raw<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &unsafe { self.ptr.as_ref() }.data
    }
}

pub struct Weak<T: Mark + ?Sized> {
    ptr: Raw<T>,
    identity: GcObjectIdentity,
}

impl<T: Mark + ?Sized> Copy for Weak<T> {}

impl<T: Mark + ?Sized> Clone for Weak<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Mark + ?Sized> Weak<T> {
    pub fn upgrade(&self) -> Option<Root<T>>
    where
        <Object<T> as Pointee>::Metadata: UsizeMetadata,
    {
        self.is_present_and(|root| root)
    }

    pub fn upgrade_and_then<U>(&self, f: impl FnOnce(&T) -> U) -> Option<U>
    where
        <Object<T> as Pointee>::Metadata: UsizeMetadata,
    {
        self.upgrade().map(|root| f(&root))
    }

    /// Checks if the object is present.
    ///
    /// NOTE: The object may be collected at any time, including immediately after this function returns.
    pub fn is_present(&self) -> bool {
        lock_default_gc(|gc| gc.is_present(self.ptr.ptr, self.identity))
    }

    /// Checks if the object is present and calls the given function with a [`Root`] to the object if it is.
    ///
    /// The object's presence is guaranteed as long as the [`Root`] is held.
    pub fn is_present_and<U>(&self, f: impl FnOnce(Root<T>) -> U) -> Option<U>
    where
        <Object<T> as Pointee>::Metadata: UsizeMetadata,
    {
        let root = lock_default_gc(|gc| {
            if gc.is_present(self.ptr.ptr, self.identity) {
                gc.root(self.ptr.ptr);
                Some(unsafe { Root::from_raw(self.ptr) })
            } else {
                None
            }
        })?;

        Some(f(root))
    }
}
