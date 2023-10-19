use core::{alloc::Layout, ptr::Pointee};

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

    pub fn downgrade(&self) -> &GcObjectHeader<T> {
        &self.header
    }
}

#[repr(C)]
pub struct GcObjectHeader<T: Mark + ?Sized> {
    pub(crate) alloc_metadata: AllocMetadata,
    pub(crate) identity: GcObjectIdentity,
    pub(crate) ptr_metadata: <GcObject<T> as Pointee>::Metadata,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
#[repr(C)]
pub struct GcObjectIdentity {
    pub nonce: u64,
    #[cfg(feature = "std")]
    pub instant: std::time::Instant,
    #[cfg(not(feature = "std"))]
    pub instant: u64,
}

#[cfg(target_arch = "x86_64")]
#[repr(transparent)]

/// This struct packs the allocation metadata into a single `usize` for x86_64.
/// We know the size cannot be larger than 48 bits, so we use the MSB to store
/// the marked flag. The alignment must be a power of two, so we simply store
/// the exponent in the high 7 bits. This allows us to store alignments up to
/// 2^2^7 = 2^128 bytes, which in the immortal words of Bill Gates, "should be
/// enough for anyone." The remaining 56 bits are used to store the size, and
/// we even have 8 bits left for future use before we run into the 48 bits of
/// size space.
pub struct AllocMetadata(usize);

#[cfg(target_arch = "x86_64")]
impl AllocMetadata {
    const MASK_MARKED: usize = 1 << 63;
    const MASK_ALIGN: usize = 0b1111111 << 56;
    const MASK_SIZE: usize = !(Self::MASK_MARKED | Self::MASK_ALIGN);

    #[inline(always)]
    pub const fn new(marked: bool, layout: Layout) -> Self {
        Self(
            (layout.size() & Self::MASK_SIZE)
                | ((layout.align().trailing_zeros() as usize) << 56)
                | ((marked as usize) << 63),
        )
    }

    #[inline(always)]
    pub fn is_marked(&self) -> bool {
        (self.0 & Self::MASK_MARKED) != 0
    }

    #[inline(always)]
    pub fn set_marked(&mut self) {
        self.0 |= Self::MASK_MARKED;
    }

    #[inline(always)]
    pub fn clear_marked(&mut self) {
        self.0 &= !Self::MASK_MARKED;
    }

    #[inline(always)]
    pub fn get_size(&self) -> usize {
        self.0 & Self::MASK_SIZE
    }

    #[inline(always)]
    pub fn get_align(&self) -> usize {
        1 << ((self.0 & Self::MASK_ALIGN) >> 56)
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

    pub trait UsizeMetadata {
        fn to_usize(self) -> usize;

        fn from_usize(size: usize) -> Self;
    }

    impl UsizeMetadata for usize {
        #[inline(always)]
        fn to_usize(self) -> usize {
            self
        }

        #[inline(always)]
        fn from_usize(size: usize) -> Self {
            size
        }
    }

    impl UsizeMetadata for () {
        #[inline(always)]
        fn to_usize(self) -> usize {
            0
        }

        #[inline(always)]
        #[allow(clippy::unused_unit)]
        fn from_usize(_: usize) -> Self {
            ()
        }
    }

    impl AnyGcObject {
        pub fn new<T: Mark + ?Sized>(ptr: NonNull<GcObject<T>>) -> Self
        where
            <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
        {
            Self {
                data: GcObjectPtr(ptr.cast()),
                size: core::ptr::metadata(ptr.as_ptr()).to_usize(),
                vtable: &GcObjectImpl {
                    mark: |data, size| unsafe {
                        let o = core::ptr::from_raw_parts_mut::<GcObject<T>>(
                            data.as_ptr(),
                            <GcObject<T> as Pointee>::Metadata::from_usize(size),
                        )
                        .as_mut()
                        .unwrap();

                        if !o.header.alloc_metadata.is_marked() {
                            o.header.alloc_metadata.set_marked();
                            o.data.mark();
                        }
                    },
                },
            }
        }

        pub fn mark(&self) {
            unsafe { (self.vtable.mark)(self.data.0, self.size) }
        }
    }
}
