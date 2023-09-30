use core::ptr::Pointee;

use crate::mark::Mark;

#[repr(C)]
pub struct GcObject<T: Mark + ?Sized> {
    pub(crate) header: GcObjectHeader<T>,
    pub(crate) data: T,
}

impl<T: Mark + ?Sized> GcObject<T> {
    pub(crate) fn mark(&mut self) {
        self.header.alloc_metadata.set_marked();
        self.data.mark();
    }
}

#[repr(C)]
pub struct GcObjectHeader<T: Mark + ?Sized> {
    pub(crate) ptr_metadata: <GcObject<T> as Pointee>::Metadata,
    pub(crate) alloc_metadata: AllocMetadata,
}

#[repr(transparent)]
pub struct AllocMetadata(usize);

impl AllocMetadata {
    const SHIFT: usize = (core::mem::size_of::<usize>() * 8 - 1);
    const MASK_MARKED: usize = 1 << Self::SHIFT;

    #[inline(always)]
    pub const fn new(marked: bool, size: usize) -> Self {
        Self(if marked {
            size | Self::MASK_MARKED
        } else {
            size
        })
    }

    #[inline(always)]
    pub fn is_marked(&self) -> bool {
        self.0 & Self::MASK_MARKED != 0
    }

    #[inline(always)]
    pub fn set_marked(&mut self) {
        self.0 |= Self::MASK_MARKED;
    }

    #[inline(always)]
    pub fn clear_marked(&mut self) {
        self.0 = self.get_size();
    }

    #[inline(always)]
    pub fn get_size(&self) -> usize {
        self.0 & !Self::MASK_MARKED
    }
}

pub use dynamic::*;

/// This module implements a custom type-erased dynamic dispatch for `GcObject<T>`'s `mark` method. We use this to allow
/// aliasing pointers to root GC objects of _any_ type for tracking of roots within the GC itself.
mod dynamic {
    use core::ptr::{NonNull, Pointee};

    use crate::mark::Mark;

    use super::GcObject;

    #[derive(Eq, PartialEq)]
    pub struct GcObjectImpl {
        mark: unsafe fn(NonNull<()>, usize),
    }

    #[derive(Eq, Hash, PartialEq)]
    #[repr(transparent)]
    struct GcObjectPtr(NonNull<()>);

    unsafe impl Send for GcObjectPtr {}
    unsafe impl Sync for GcObjectPtr {}

    #[derive(Eq, PartialEq)]
    pub struct AnyGcObject {
        data: GcObjectPtr,
        size: usize,
        vtable: &'static GcObjectImpl,
    }

    impl core::hash::Hash for AnyGcObject {
        fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
            self.data.hash(state);
            self.size.hash(state);
        }
    }

    pub trait SizedMetadata {
        fn as_usize(self) -> usize;

        fn from_usize(size: usize) -> Self;
    }

    impl SizedMetadata for usize {
        #[inline(always)]
        fn as_usize(self) -> usize {
            self
        }

        #[inline(always)]
        fn from_usize(size: usize) -> Self {
            size
        }
    }

    impl SizedMetadata for () {
        #[inline(always)]
        fn as_usize(self) -> usize {
            0
        }

        #[inline(always)]
        fn from_usize(_: usize) -> Self {
            ()
        }
    }

    impl AnyGcObject {
        pub fn new<T: Mark + ?Sized>(ptr: NonNull<GcObject<T>>) -> Self
        where
            <GcObject<T> as Pointee>::Metadata: SizedMetadata,
        {
            Self {
                data: GcObjectPtr(ptr.cast()),
                size: core::ptr::metadata(ptr.as_ptr()).as_usize(),
                vtable: &GcObjectImpl {
                    mark: |data, size| unsafe {
                        core::ptr::from_raw_parts_mut::<GcObject<T>>(
                            data.as_ptr(),
                            <GcObject<T> as Pointee>::Metadata::from_usize(size),
                        )
                        .as_mut()
                        .unwrap()
                        .mark()
                    },
                },
            }
        }

        pub fn mark(&self) {
            unsafe { (self.vtable.mark)(self.data.0, self.size) }
        }
    }
}
