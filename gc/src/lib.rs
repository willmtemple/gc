#![cfg_attr(not(feature = "std"), no_std)]
#![feature(allocator_api)]
#![feature(ptr_metadata)]
#![feature(layout_for_ptr)]
#![feature(decl_macro)]
#![feature(associated_type_defaults)]
#![feature(new_uninit)]
#![feature(lazy_cell)]
#![feature(offset_of)]

extern crate alloc;

use core::{
    alloc::{AllocError, Allocator, Layout},
    ops::DerefMut,
    ptr::{null, NonNull, Pointee},
    sync::atomic::{AtomicUsize, Ordering},
};

use alloc::alloc::Global;
use mark::Mark;
use obj::{AllocMetadata, AnyGcObject, As, GcObjectHeader, GcObjectIdentity, Object};

pub mod mark;
pub mod obj;
pub mod ptr;

#[cfg(not(feature = "std"))]
mod _gc {
    use core::ops::DerefMut;

    use crate::DefaultGarbageCollector;

    static GC: spin::Mutex<DefaultGarbageCollector> =
        spin::Mutex::new(DefaultGarbageCollector::new());

    pub fn lock_default_gc<R, F: FnOnce(&mut DefaultGarbageCollector) -> R>(f: F) -> R {
        f(GC.lock().deref_mut())
    }

    pub fn inhibit_collections<R>(f: impl FnOnce() -> R) -> R {
        GC.lock().inhibit();
        let r = f();
        GC.lock().uninhibit();
        r
    }
}

#[cfg(feature = "std")]
mod _gc {
    use core::ops::DerefMut;

    use crate::DefaultGarbageCollector;

    lazy_static::lazy_static!(
        static ref GC: std::sync::Mutex<DefaultGarbageCollector> =
            std::sync::Mutex::new(DefaultGarbageCollector::new());
    );

    pub fn lock_default_gc<R>(f: impl FnOnce(&mut DefaultGarbageCollector) -> R) -> R {
        f(GC.lock().expect("failed to acquire lock").deref_mut())
    }

    pub fn inhibit_collections<R>(f: impl FnOnce() -> R) -> R {
        GC.lock().expect("failed to acquire lock").inhibit();
        let r = f();
        GC.lock().expect("failed to acquire lock").uninhibit();
        r
    }
}

pub use _gc::*;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct Allocation(NonNull<GcObjectHeader<()>>);

unsafe impl Send for Allocation {}

unsafe impl Sync for Allocation {}

impl Allocation {
    fn deallocate(self) {
        let header = unsafe { &mut *self.0.as_ptr() };

        let layout = unsafe {
            Layout::from_size_align_unchecked(
                header.alloc_metadata.get_size(),
                header.alloc_metadata.get_align(),
            )
        };

        // Zero the identity nonce and instant so that even if this object is reallocated, existing weak pointers
        // will not be able to upgrade.
        header.identity = unsafe { core::mem::zeroed() };

        unsafe {
            Global.deallocate(
                NonNull::new_unchecked(header as *const GcObjectHeader<()> as *mut u8),
                layout,
            )
        }
    }
}

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

    inhibitors: AtomicUsize,
}

impl Default for DefaultGarbageCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl DefaultGarbageCollector {
    #[cfg(not(feature = "std"))]
    const fn new() -> Self {
        Self {
            roots: alloc::collections::BTreeMap::new(),
            presence_set: alloc::collections::BTreeSet::new(),
            seq: 0,
            inhibitors: AtomicUsize::new(0),
        }
    }

    #[cfg(feature = "std")]
    fn new() -> Self {
        Self {
            roots: std::collections::HashMap::new(),
            presence_set: std::collections::HashSet::new(),
            inhibitors: AtomicUsize::new(0),
        }
    }

    pub fn is_present<T: Mark + ?Sized>(
        &self,
        ptr: NonNull<Object<T>>,
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

    fn inhibit(&mut self) {
        self.inhibitors.fetch_add(1, Ordering::SeqCst);
    }

    fn uninhibit(&mut self) {
        self.inhibitors.fetch_sub(1, Ordering::SeqCst);
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

#[derive(Debug, Clone)]
pub enum GcError {
    Inhibited,
}

impl GarbageCollector for DefaultGarbageCollector {
    #[cfg(feature = "std")]
    type Locked = std::sync::Mutex<Self>;
    #[cfg(not(feature = "std"))]
    type Locked = spin::Mutex<Self>;

    fn root<T: Mark + ?Sized>(&mut self, ptr: NonNull<Object<T>>)
    where
        <Object<T> as Pointee>::Metadata: As<usize>,
    {
        let obj = AnyGcObject::new(ptr);
        if let Some(count) = self.roots.get_mut(&obj) {
            *count += 1;
        } else {
            self.roots.insert(obj, 1);
        }
    }

    fn unroot<T: Mark + ?Sized>(&mut self, ptr: NonNull<Object<T>>)
    where
        <Object<T> as Pointee>::Metadata: As<usize>,
    {
        let obj = AnyGcObject::new(ptr);
        if let Some(count @ 2..) = self.roots.get_mut(&obj) {
            *count -= 1;
        } else {
            self.roots.remove(&obj);
        }
    }

    fn allocate<T: ?Sized>(
        &mut self,
        metadata: <Object<T> as core::ptr::Pointee>::Metadata,
    ) -> Result<NonNull<Object<T>>, AllocError> {
        let layout = unsafe {
            Layout::for_value_raw(core::ptr::from_raw_parts::<Object<T>>(null(), metadata))
        };

        let gc_object = unsafe {
            let ptr = Global.allocate_zeroed(layout)?.cast::<()>();

            core::ptr::from_raw_parts_mut::<Object<T>>(ptr.as_ptr(), metadata)
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

    fn collect(&mut self) -> Result<(), GcError> {
        if self.inhibitors.load(Ordering::SeqCst) > 0 {
            return Err(GcError::Inhibited);
        }

        self.mark_roots();

        self.sweep();

        self.unmark_all();

        Ok(())
    }
}

pub trait GarbageCollector {
    type Locked: GcLock;

    fn root<T: Mark + ?Sized>(&mut self, ptr: NonNull<Object<T>>)
    where
        <Object<T> as Pointee>::Metadata: As<usize>;

    fn unroot<T: Mark + ?Sized>(&mut self, ptr: NonNull<Object<T>>)
    where
        <Object<T> as Pointee>::Metadata: As<usize>;

    fn allocate<T: ?Sized>(
        &mut self,
        metadata: <Object<T> as core::ptr::Pointee>::Metadata,
    ) -> Result<NonNull<Object<T>>, AllocError>;

    fn collect(&mut self) -> Result<(), GcError>;
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
        ptr::{Gc, Root},
        GarbageCollector,
    };

    #[test]
    fn test_gc() {
        let r = Root::<str>::from("Hello world!");

        assert_eq!(r.len(), 12);

        assert_eq!(&*r, "Hello world!");
    }

    #[test]
    fn test_weak() {
        let r = Root::<str>::from("Hello world!");

        let weak = r.get_weak();

        assert!(weak.is_present());

        assert_eq!(r.len(), 12);
        assert_eq!(weak.upgrade_and_then(|r| r.len()), Some(12));

        assert_eq!(&*r, "Hello world!");
        assert_eq!(
            weak.upgrade_and_then(|r| String::from(r)),
            Some("Hello world!".to_string())
        );

        lock_default_gc(|gc| gc.collect().unwrap());

        assert!(weak.is_present());

        assert_eq!(r.len(), 12);
        assert_eq!(weak.upgrade_and_then(|r| r.len()), Some(12));

        assert_eq!(&*r, "Hello world!");
        assert_eq!(
            weak.upgrade_and_then(|r| String::from(r)),
            Some("Hello world!".to_string())
        );

        core::mem::drop(r);

        lock_default_gc(|gc| gc.collect().unwrap());

        assert!(!weak.is_present());

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

        let hello_world = Root::<str>::from("Hello world!");
        let hw_weak = hello_world.get_weak();

        assert!(hw_weak.is_present());

        assert_eq!(hello_world.len(), 12);
        assert_eq!(hw_weak.upgrade_and_then(|r| r.len()), Some(12));

        assert_eq!(&*hello_world, "Hello world!");
        assert_eq!(
            hw_weak.upgrade_and_then(|r| String::from(r)),
            Some("Hello world!".to_string())
        );

        let foo = Root::<Foo>::from(Foo {
            bar: unsafe { hello_world.as_unrooted() },
        });

        core::mem::drop(hello_world);

        lock_default_gc(|gc| gc.collect().unwrap());

        assert!(hw_weak.is_present());

        assert_eq!(foo.bar.len(), 12);
        assert_eq!(hw_weak.upgrade_and_then(|r| r.len()), Some(12));

        assert_eq!(&*foo.bar, "Hello world!");
        assert_eq!(
            hw_weak.upgrade_and_then(|r| String::from(r)),
            Some("Hello world!".to_string())
        );

        core::mem::drop(foo);

        lock_default_gc(|gc| gc.collect().unwrap());

        assert!(!hw_weak.is_present());
        assert!(hw_weak.upgrade().is_none());
    }
}
