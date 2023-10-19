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
use obj::{AllocMetadata, AnyGcObject, GcObject, GcObjectHeader, GcObjectIdentity, UsizeMetadata};

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

#[derive(Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct Allocation(NonNull<GcObjectHeader<()>>);

unsafe impl Send for Allocation {}

unsafe impl Sync for Allocation {}

impl Allocation {
    fn deallocate(self) {
        let header = unsafe { &*self.0.as_ptr() };

        let layout = unsafe {
            Layout::from_size_align_unchecked(
                header.alloc_metadata.get_size(),
                header.alloc_metadata.get_align(),
            )
        };

        unsafe {
            Global.deallocate(
                NonNull::new_unchecked(header as *const GcObjectHeader<()> as *mut u8),
                layout,
            )
        }
    }
}

#[derive(Default)]
pub struct DefaultGarbageCollector {
    #[cfg(feature = "std")]
    roots: std::collections::HashMap<AnyGcObject, usize>,
    #[cfg(not(feature = "std"))]
    roots: alloc::collections::BTreeMap<AnyGcObject, usize>,
    #[cfg(feature = "std")]
    presence_set: std::collections::HashSet<Allocation>,
    #[cfg(not(feature = "std"))]
    presence_set: alloc::collections::BTreeSet<Allocation>,

    #[cfg(not(feature = "std"))]
    seq: u64,
}

impl DefaultGarbageCollector {
    #[cfg(not(feature = "std"))]
    const fn new() -> Self {
        Self {
            roots: alloc::collections::BTreeMap::new(),
            presence_set: alloc::collections::BTreeSet::new(),
            seq: 0,
        }
    }

    #[cfg(feature = "std")]
    fn new() -> Self {
        Self {
            roots: std::collections::HashMap::new(),
            presence_set: std::collections::HashSet::new(),
        }
    }

    pub fn is_present<T: Mark + ?Sized>(
        &self,
        ptr: NonNull<GcObject<T>>,
        identity: GcObjectIdentity,
    ) -> bool {
        let allocation = Allocation(unsafe {
            NonNull::new_unchecked(ptr.as_ref().downgrade() as *const _ as *mut GcObjectHeader<()>)
        });

        let is_in_presence_set = self.presence_set.contains(&allocation);

        is_in_presence_set && unsafe { ptr.as_ref() }.header.identity == identity
    }

    fn mark_roots(&mut self) {
        for (root, _) in self.roots.iter() {
            root.mark();
        }
    }

    fn sweep(&mut self) {
        // TODO/wtemple - bad clone
        for allocation in self.presence_set.clone().iter() {
            let header = unsafe { allocation.0.as_ref() };

            if !header.alloc_metadata.is_marked() {
                self.presence_set.remove(allocation);
                allocation.deallocate();
            }
        }
    }

    fn unmark_all(&mut self) {
        for allocation in self.presence_set.iter() {
            let header = unsafe { &mut *allocation.0.as_ptr() };
            header.alloc_metadata.clear_marked();
        }
    }

    #[cfg(not(feature = "std"))]
    fn rand(&self) -> u64 {
        use rand::{rngs::OsRng, RngCore};

        let mut key = [0u8; 16];
        OsRng.fill_bytes(&mut key);
        OsRng.next_u64()
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

impl GarbageCollector for DefaultGarbageCollector {
    #[cfg(feature = "std")]
    type Locked = std::sync::Mutex<Self>;
    #[cfg(not(feature = "std"))]
    type Locked = spin::Mutex<Self>;

    fn root<T: Mark + ?Sized>(&mut self, ptr: NonNull<GcObject<T>>)
    where
        <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
    {
        let obj = AnyGcObject::new(ptr);
        if let Some(count) = self.roots.get_mut(&obj) {
            *count += 1;
        } else {
            self.roots.insert(obj, 1);
        }
    }

    fn unroot<T: Mark + ?Sized>(&mut self, ptr: NonNull<GcObject<T>>)
    where
        <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
    {
        let obj = AnyGcObject::new(ptr);
        if let Some(count @ 2..) = self.roots.get_mut(&obj) {
            *count -= 1;
        } else {
            self.roots.remove(&obj);
        }
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

        let allocation = Allocation(unsafe {
            NonNull::new_unchecked(gc_object.downgrade() as *const _ as *mut GcObjectHeader<()>)
        });

        self.presence_set.insert(allocation);

        gc_object.header.ptr_metadata = metadata;
        gc_object.header.alloc_metadata = AllocMetadata::new(false, layout);
        gc_object.header.identity = GcObjectIdentity {
            #[cfg(feature = "std")]
            nonce: rand::random(),
            #[cfg(not(feature = "std"))]
            nonce: self.rand(),
            #[cfg(feature = "std")]
            instant: std::time::Instant::now(),
            #[cfg(not(feature = "std"))]
            instant: {
                self.seq += 1;
                self.seq
            },
        };

        Ok(NonNull::from(gc_object))
    }

    fn collect(&mut self) {
        self.mark_roots();

        self.sweep();

        self.unmark_all();
    }
}

pub trait GarbageCollector {
    type Locked: GcLock;

    fn root<T: Mark + ?Sized>(&mut self, ptr: NonNull<GcObject<T>>)
    where
        <GcObject<T> as Pointee>::Metadata: UsizeMetadata;

    fn unroot<T: Mark + ?Sized>(&mut self, ptr: NonNull<GcObject<T>>)
    where
        <GcObject<T> as Pointee>::Metadata: UsizeMetadata;

    fn allocate<T: Mark + ?Sized>(
        &mut self,
        metadata: <GcObject<T> as core::ptr::Pointee>::Metadata,
    ) -> Result<NonNull<GcObject<T>>, AllocError>;

    fn collect(&mut self);
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
    use alloc::string::{String, ToString};

    use crate::{
        lock_default_gc,
        mark::Mark,
        ptr::{Gc, GcRoot},
        GarbageCollector,
    };

    #[test]
    fn test_gc() {
        let r = GcRoot::<str>::from("Hello world!");

        assert_eq!(r.len(), 12);

        assert_eq!(&*r, "Hello world!");
    }

    #[test]
    fn test_weak() {
        let r = GcRoot::<str>::from("Hello world!");

        let weak = r.get_weak();

        assert!(weak.present());

        assert_eq!(r.len(), 12);
        assert_eq!(weak.upgrade_and_then(|r| r.len()), Some(12));

        assert_eq!(&*r, "Hello world!");
        assert_eq!(
            weak.upgrade_and_then(|r| String::from(r)),
            Some("Hello world!".to_string())
        );

        lock_default_gc(|gc| gc.collect());

        assert!(weak.present());

        assert_eq!(r.len(), 12);
        assert_eq!(weak.upgrade_and_then(|r| r.len()), Some(12));

        assert_eq!(&*r, "Hello world!");
        assert_eq!(
            weak.upgrade_and_then(|r| String::from(r)),
            Some("Hello world!".to_string())
        );

        core::mem::drop(r);

        lock_default_gc(|gc| gc.collect());

        assert!(!weak.present());

        assert!(weak.upgrade().is_none());
    }

    #[test]
    fn test_indirect() {
        struct Foo {
            bar: Gc<str>,
        }

        impl Mark for Foo {
            fn mark(&mut self) {
                self.bar.mark();
            }
        }

        let hello_world = GcRoot::<str>::from("Hello world!");
        let hw_weak = hello_world.get_weak();

        assert!(hw_weak.present());

        assert_eq!(hello_world.len(), 12);
        assert_eq!(hw_weak.upgrade_and_then(|r| r.len()), Some(12));

        assert_eq!(&*hello_world, "Hello world!");
        assert_eq!(
            hw_weak.upgrade_and_then(|r| String::from(r)),
            Some("Hello world!".to_string())
        );

        let foo = GcRoot::<Foo>::from(Foo {
            bar: unsafe { hello_world.as_unrooted() },
        });

        core::mem::drop(hello_world);

        lock_default_gc(|gc| gc.collect());

        assert!(hw_weak.present());

        assert_eq!(foo.bar.len(), 12);
        assert_eq!(hw_weak.upgrade_and_then(|r| r.len()), Some(12));

        assert_eq!(&*foo.bar, "Hello world!");
        assert_eq!(
            hw_weak.upgrade_and_then(|r| String::from(r)),
            Some("Hello world!".to_string())
        );

        core::mem::drop(foo);

        lock_default_gc(|gc| gc.collect());

        assert!(!hw_weak.present());
        assert!(hw_weak.upgrade().is_none());
    }
}
