use core::{
    alloc::AllocError,
    ops::Deref,
    ptr::{NonNull, Pointee},
};

use crate::{
    lock_default_gc,
    mark::Mark,
    obj::{As, GcObjectIdentity, Object},
    GarbageCollector,
};

/// A root pointer to a GC object.
///
/// Objects that root pointers refer to cannot be collected by the GC. Roots are atomically reference counted. Once all
/// references to the root object are disposed, the object may be eligible for collection.
///
/// This is the best type to use when working with GC objects on the Rust stack.
#[repr(transparent)]
pub struct Root<T: Mark + ?Sized>(pub(crate) Gc<T>)
where
    <Object<T> as Pointee>::Metadata: As<usize>;

impl<T: Mark + ?Sized> Deref for Root<T>
where
    <Object<T> as Pointee>::Metadata: As<usize>,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Mark + ?Sized> Root<T>
where
    <Object<T> as Pointee>::Metadata: As<usize>,
{
    /// # Safety
    ///
    /// Callers must ensure that the return value of this function is stored in a location that is connected to the GC
    /// root graph before dropping `self`, otherwise the object may be collected and the returned pointer may be
    /// invalidated.
    pub unsafe fn as_unrooted(&self) -> Gc<T> {
        self.0
    }

    pub(crate) unsafe fn new_uninitialized_with_metadata(
        metadata: <Object<T> as core::ptr::Pointee>::Metadata,
    ) -> Result<Self, AllocError> {
        lock_default_gc(|gc| {
            gc.allocate::<T>(metadata).map(|ptr| {
                gc.root(ptr);
                Self(Gc { ptr })
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
    <Object<T> as Pointee>::Metadata: As<usize>,
{
    /// Creates a new GC root from a raw, NonNull pointer.
    ///
    /// # Safety
    ///
    /// The argument to this function _MUST_ have come from a previous call to [`Self::into_raw`].
    ///
    /// # Panics
    ///
    /// Panics if the argument is null.
    pub unsafe fn from_raw(raw: *const T) -> Self
    where
        <T as Pointee>::Metadata: As<usize>,
    {
        let metadata = core::ptr::metadata(raw);
        let metadata = <<Object<T> as Pointee>::Metadata as As<usize>>::from(As::into(metadata));
        let obj_ptr = core::ptr::from_raw_parts::<Object<T>>(core::ptr::null(), metadata);

        let data_ptr = &(*obj_ptr).data;

        assert_eq!(
            obj_ptr as *const () as usize,
            raw as *const _ as *const () as usize
        );

        let offset = (data_ptr as *const _ as *const () as usize) - (obj_ptr as *const () as usize);

        let real_ptr = (raw as *const _ as *const () as usize - offset) as *mut ();

        Self(Gc {
            ptr: NonNull::new_unchecked(core::ptr::from_raw_parts_mut(real_ptr, metadata)),
        })
    }

    /// Consumes the GC root returning a raw pointer to its data.
    ///
    /// # Safety
    ///
    /// The object remains rooted. You must pass the pointer back to [`Self::from_raw`] to get a valid GC root again,
    /// otherwise you will leak the object.
    #[allow(clippy::unnecessary_cast)]
    pub unsafe fn into_raw(root: Self) -> *const T {
        let ptr = root.0.ptr.as_ptr();
        core::ptr::addr_of!((*ptr).data) as *const T
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
    <Object<T> as Pointee>::Metadata: As<usize>,
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
    <Object<T> as Pointee>::Metadata: As<usize>,
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
pub struct Gc<T: ?Sized> {
    pub(crate) ptr: NonNull<Object<T>>,
}

impl<T: Mark + ?Sized> Gc<T> {
    pub fn get_weak(&self) -> Weak<T> {
        Weak {
            ptr: *self,
            identity: unsafe { &*self.ptr.as_ptr() }.header.identity,
        }
    }
}

impl<T: Mark + ?Sized> Mark for Gc<T> {
    fn mark(&mut self) {
        unsafe { self.ptr.as_mut() }.mark()
    }
}

impl<T: ?Sized> Copy for Gc<T> {}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &unsafe { self.ptr.as_ref() }.data
    }
}

pub struct Weak<T: Mark + ?Sized> {
    ptr: Gc<T>,
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
        <Object<T> as Pointee>::Metadata: As<usize>,
    {
        self.is_present_and(|root| root)
    }

    pub fn upgrade_and_then<U>(&self, f: impl FnOnce(&T) -> U) -> Option<U>
    where
        <Object<T> as Pointee>::Metadata: As<usize>,
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
        <Object<T> as Pointee>::Metadata: As<usize>,
    {
        let root = lock_default_gc(|gc| {
            if gc.is_present(self.ptr.ptr, self.identity) {
                gc.root(self.ptr.ptr);
                Some(Root(self.ptr))
            } else {
                None
            }
        })?;

        Some(f(root))
    }
}
