use core::{
    alloc::AllocError,
    ops::Deref,
    ptr::{NonNull, Pointee},
};

use crate::{
    lock_default_gc,
    mark::Mark,
    obj::{GcObject, SizedMetadata},
    GarbageCollector,
};

#[repr(transparent)]
pub struct GcRoot<T: Mark + ?Sized>(pub(crate) Gc<T>)
where
    <GcObject<T> as Pointee>::Metadata: SizedMetadata;

impl<T: Mark + ?Sized> Deref for GcRoot<T>
where
    <GcObject<T> as Pointee>::Metadata: SizedMetadata,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Mark + ?Sized> GcRoot<T>
where
    <GcObject<T> as Pointee>::Metadata: SizedMetadata,
{
    /// # Safety
    /// Callers must ensure that the return value of this function is stored in a location that is connected to the GC
    /// root graph before dropping `self`, otherwise the object may be collected and the returned pointer may be
    /// invalidated.
    pub unsafe fn get_unrooted(&self) -> Gc<T> {
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

impl<T: Mark> From<T> for GcRoot<T> {
    fn from(value: T) -> Self {
        Self::new(value).expect(&format!(
            "failed to allocate Gc<{}>",
            core::any::type_name::<T>()
        ))
    }
}

impl From<&str> for GcRoot<str> {
    fn from(value: &str) -> Self {
        unsafe {
            let mut root = Self::new_uninitialized_with_metadata(value.len()).expect(&format!(
                "failed to allocate Gc<str> with length {}",
                value.len()
            ));

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
    <GcObject<T> as Pointee>::Metadata: SizedMetadata,
{
    fn drop(&mut self) {
        lock_default_gc(|gc| gc.unroot(self.0.ptr))
    }
}

#[repr(transparent)]
pub struct Gc<T: Mark + ?Sized> {
    pub(crate) ptr: NonNull<GcObject<T>>,
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

// TODO: I eventually want to support weak refs, but I'm not sure how to implement them without refcounting.

// #[repr(transparent)]
// pub struct GcWeak<T: Mark + ?Sized> {
//     ptr: NonNull<GcObject<T>>,
// }

// impl<T: Mark + ?Sized> Copy for GcWeak<T> {}

// impl<T: Mark + ?Sized> Clone for GcWeak<T> {
//     fn clone(&self) -> Self {
//         *self
//     }
// }

// TODO: chatjippity offered this implementation of `upgrade`.
// impl<T: Mark + ?Sized> GcWeak<T> {
//     pub fn upgrade(&self) -> Option<Gc<T>> {
//         lock_default_gc(|gc| {
//             if gc.is_rooted(self.ptr) {
//                 Some(Gc { ptr: self.ptr })
//             } else {
//                 None
//             }
//         })
//     }
// }

// impl<T: Mark + ?Sized> Deref for GcWeak<T> {
//     type Target = T;

//     fn deref(&self) -> &Self::Target {
//         &unsafe { self.ptr.as_ref() }.data
//     }
// }
