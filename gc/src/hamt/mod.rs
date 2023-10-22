// Persistent Hash Array Mapped Trie

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

#[cfg(all(test, feature = "std"))]
mod tests;
