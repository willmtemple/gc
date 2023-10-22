// Persistent Hash Array Mapped Trie

#![cfg_attr(not(feature = "std"), no_std)]
#![feature(ptr_metadata)]
#![feature(allocator_api)]
#![feature(layout_for_ptr)]
#![feature(new_uninit)]

extern crate alloc;

#[cfg(not(target_arch = "x86_64"))]
const _: () = { compile_error!("hamt only supports x86_64") };

// pub mod gc;

pub mod config;
pub mod map;
pub mod set;
pub mod vec;

mod iter;
mod node;

pub use map::HamtMap;
pub use set::HamtSet;
pub use vec::HamtVec;

pub use config::HamtConfig;

pub mod imperative;
