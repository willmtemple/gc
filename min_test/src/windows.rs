// Compile this with --no-default-features in gc.

// This is an experiment to see how small a driver program for the HAMT can be once compiled. Current results on
// x86_64-pc-windows-msvc are about 4.5 KiB, which is pretty good. The HAMT itself is around 3.5KiB of .text code. The
// rest is core functionality, the minimal bindings to HeapAllocate and the drop/clone implementations for Arc.

#![no_std]
#![no_main]
#![feature(strict_provenance)]
#![feature(decl_macro)]

#[used]
#[no_mangle]
pub static _fltused: i32 = 0;

#[panic_handler]
fn handle_panic(_: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[global_allocator]
static GLOBAL: wmem::System = wmem::System;

extern crate alloc;

use gc::hamt;
use io::write_stdout;

#[derive(Default)]
struct VecHasher(u64);

impl core::hash::Hasher for VecHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        // if bytes.len() != 4 {
        //     panic!("bytes.len() != 4");
        // }

        // self.0 = u32::from_ne_bytes(bytes.try_into().unwrap()) as u64;

        // Simple hash function for testing purposes. Multiply by 31 and do some stuff
        for byte in bytes {
            self.0 = self.0.wrapping_mul(31).wrapping_add(*byte as u64);
        }
    }
}

#[no_mangle]
extern "C" fn mainCRTStartup() -> i32 {
    main()
}

#[no_mangle]
extern "C" fn main() -> i32 {
    let hamt = {
        hamt::Hamt::<i32, u32, VecHasher>::new()
            .insert(0, 1)
            .insert(1, 2)
            .remove(&0)
    };

    // io::println!("Result: {:?}", hamt.get(&1));

    if hamt.get(&1).is_none() || hamt.get(&0).is_some() {
        write_stdout("Test failed!");
        1
    } else {
        write_stdout("Test passed!");
        0
    }
}

mod io {

    use super::win;

    const STD_OUTPUT_HANDLE: win::DWORD = -11i32 as win::DWORD;

    // pub macro println {
    //     ($($arg:tt)*) => {
    //         write_stdout(::alloc::format!($($arg)*));
    //         write_stdout("\r\n");
    //     }
    // }

    pub fn write_stdout(v: impl AsRef<str>) {
        let h_out = unsafe { GetStdHandle(STD_OUTPUT_HANDLE) };

        let mut written = 0u32;

        let v = v.as_ref();

        let result = unsafe {
            WriteConsoleA(
                h_out,
                v.as_ptr() as win::LPVOID,
                v.len() as win::DWORD,
                &mut written,
                core::ptr::null_mut(),
            )
        };

        if result == 0 {
            panic!("WriteConsoleA failed");
        }
    }

    #[link(name = "kernel32")]
    extern "system" {
        fn WriteConsoleA(
            hConsoleOutput: win::HANDLE,
            lpBuffer: win::LPVOID,
            nNumberOfCharsToWrite: win::DWORD,
            lpNumberOfCharsWritten: win::LPDWORD,
            lpReserved: win::LPVOID,
        ) -> win::BOOL;

        fn GetStdHandle(nStdHandle: win::DWORD) -> win::HANDLE;
    }
}

mod win {
    #![allow(non_camel_case_types)]

    pub type HANDLE = *mut ();
    pub type DWORD = u32;
    pub type SIZE_T = usize;
    pub type LPVOID = *mut ();
    pub type LPDWORD = *mut DWORD;
    pub type BOOL = i32;
}

mod wmem {
    // Heap memory management on Windows is done by using the system Heap API (heapapi.h)
    // See https://docs.microsoft.com/windows/win32/api/heapapi/

    use super::win;

    #[link(name = "kernel32")]
    extern "system" {
        // Get a handle to the default heap of the current process, or null if the operation fails.
        //
        // SAFETY: Successful calls to this function within the same process are assumed to
        // always return the same handle, which remains valid for the entire lifetime of the process.
        //
        // See https://docs.microsoft.com/windows/win32/api/heapapi/nf-heapapi-getprocessheap
        fn GetProcessHeap() -> win::HANDLE;

        // Allocate a block of `dwBytes` bytes of memory from a given heap `hHeap`.
        // The allocated memory may be uninitialized, or zeroed if `dwFlags` is
        // set to `HEAP_ZERO_MEMORY`.
        //
        // Returns a pointer to the newly-allocated memory or null if the operation fails.
        // The returned pointer will be aligned to at least `MIN_ALIGN`.
        //
        // SAFETY:
        //  - `hHeap` must be a non-null handle returned by `GetProcessHeap`.
        //  - `dwFlags` must be set to either zero or `HEAP_ZERO_MEMORY`.
        //
        // Note that `dwBytes` is allowed to be zero, contrary to some other allocators.
        //
        // See https://docs.microsoft.com/windows/win32/api/heapapi/nf-heapapi-heapalloc
        fn HeapAlloc(hHeap: win::HANDLE, dwFlags: win::DWORD, dwBytes: win::SIZE_T) -> win::LPVOID;

        // Free a block of memory behind a given pointer `lpMem` from a given heap `hHeap`.
        // Returns a nonzero value if the operation is successful, and zero if the operation fails.
        //
        // SAFETY:
        //  - `hHeap` must be a non-null handle returned by `GetProcessHeap`.
        //  - `dwFlags` must be set to zero.
        //  - `lpMem` must be a pointer to an allocated block returned by `HeapAlloc` or `HeapReAlloc`,
        //     that has not already been freed.
        // If the block was successfully freed, pointers pointing to the freed memory, such as `lpMem`,
        // must not be dereferenced ever again.
        //
        // Note that `lpMem` is allowed to be null, which will not cause the operation to fail.
        //
        // See https://docs.microsoft.com/windows/win32/api/heapapi/nf-heapapi-heapfree
        fn HeapFree(hHeap: win::HANDLE, dwFlags: win::DWORD, lpMem: win::LPVOID) -> win::BOOL;
    }

    // Cached handle to the default heap of the current process.
    // Either a non-null handle returned by `GetProcessHeap`, or null when not yet initialized or `GetProcessHeap` failed.
    static HEAP: core::sync::atomic::AtomicPtr<()> =
        core::sync::atomic::AtomicPtr::new(core::ptr::null_mut());

    // Get a handle to the default heap of the current process, or null if the operation fails.
    // If this operation is successful, `HEAP` will be successfully initialized and contain
    // a non-null handle returned by `GetProcessHeap`.
    #[inline]
    fn init_or_get_process_heap() -> win::HANDLE {
        let heap = HEAP.load(core::sync::atomic::Ordering::Relaxed);
        if heap.is_null() {
            // `HEAP` has not yet been successfully initialized
            let heap = unsafe { GetProcessHeap() };
            if !heap.is_null() {
                // SAFETY: No locking is needed because within the same process,
                // successful calls to `GetProcessHeap` will always return the same value, even on different threads.
                HEAP.store(heap, core::sync::atomic::Ordering::Release);

                // SAFETY: `HEAP` contains a non-null handle returned by `GetProcessHeap`
                heap
            } else {
                // Could not get the current process heap.
                // core::ptr::null_mut()
                panic!("Could not get process heap.")
            }
        } else {
            // SAFETY: `HEAP` contains a non-null handle returned by `GetProcessHeap`
            heap
        }
    }

    // Get a non-null handle to the default heap of the current process.
    // SAFETY: `HEAP` must have been successfully initialized.
    #[inline]
    unsafe fn get_process_heap() -> win::HANDLE {
        HEAP.load(core::sync::atomic::Ordering::Acquire)
    }

    // Header containing a pointer to the start of an allocated block.
    // SAFETY: Size and alignment must be <= `MIN_ALIGN`.
    #[repr(C)]
    struct Header(*mut u8);

    pub struct System;

    // Allocate a block of optionally zeroed memory for a given `layout`.
    // SAFETY: Returns a pointer satisfying the guarantees of `System` about allocated pointers,
    // or null if the operation fails. If this returns non-null `HEAP` will have been successfully
    // initialized.
    #[inline]
    unsafe fn allocate(layout: core::alloc::Layout) -> *mut u8 {
        let heap = init_or_get_process_heap();
        if heap.is_null() {
            // Allocation has failed, could not get the current process heap.
            return core::ptr::null_mut();
        }

        if layout.align() <= 1 {
            // SAFETY: `heap` is a non-null handle returned by `GetProcessHeap`.
            // The returned pointer points to the start of an allocated block.
            unsafe { HeapAlloc(heap, 0, layout.size()) as *mut u8 }
        } else {
            // Allocate extra padding in order to be able to satisfy the alignment.
            let total = layout.align() + layout.size();

            // SAFETY: `heap` is a non-null handle returned by `GetProcessHeap`.
            let ptr = unsafe { HeapAlloc(heap, 0, total) as *mut u8 };
            if ptr.is_null() {
                // Allocation has failed.
                return core::ptr::null_mut();
            }

            // Create a correctly aligned pointer offset from the start of the allocated block,
            // and write a header before it.

            let offset = layout.align() - (ptr.addr() & (layout.align() - 1));
            // SAFETY: `MIN_ALIGN` <= `offset` <= `layout.align()` and the size of the allocated
            // block is `layout.align() + layout.size()`. `aligned` will thus be a correctly aligned
            // pointer inside the allocated block with at least `layout.size()` bytes after it and at
            // least `MIN_ALIGN` bytes of padding before it.
            let aligned = unsafe { ptr.add(offset) };
            // SAFETY: Because the size and alignment of a header is <= `MIN_ALIGN` and `aligned`
            // is aligned to at least `MIN_ALIGN` and has at least `MIN_ALIGN` bytes of padding before
            // it, it is safe to write a header directly before it.
            unsafe { core::ptr::write((aligned as *mut Header).sub(1), Header(ptr)) };

            // SAFETY: The returned pointer does not point to the to the start of an allocated block,
            // but there is a header readable directly before it containing the location of the start
            // of the block.
            aligned
        }
    }

    unsafe impl core::alloc::GlobalAlloc for System {
        #[inline]
        unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
            unsafe { allocate(layout) }
        }

        #[inline]
        unsafe fn dealloc(&self, ptr: *mut u8, layout: core::alloc::Layout) {
            let block = {
                if layout.align() <= 1 {
                    ptr
                } else {
                    // The location of the start of the block is stored in the padding before `ptr`.

                    // SAFETY: Because of the contract of `System`, `ptr` is guaranteed to be non-null
                    // and have a header readable directly before it.
                    unsafe { core::ptr::read((ptr as *mut Header).sub(1)).0 }
                }
            };

            // SAFETY: because `ptr` has been successfully allocated with this allocator,
            // `HEAP` must have been successfully initialized.
            let heap = unsafe { get_process_heap() };

            // SAFETY: `heap` is a non-null handle returned by `GetProcessHeap`,
            // `block` is a pointer to the start of an allocated block.
            unsafe { HeapFree(heap, 0, block as win::LPVOID) };
        }
    }
}
