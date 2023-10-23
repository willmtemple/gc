# {name} - Efficient Hash Array Mapped Tries for Rust

A Hash Array Mapped Trie is a persistent, general-purpose, associative data structure. This crate implements a persistent version of Bagwell's Ideal Hash Trie with the following characteristics:

- **Persistent**: Inserting a new key-value pair into a HAMT returns a new HAMT with the new key-value pair inserted. The original HAMT is unmodified and the data are internally reference-counted to ensure memory safety.
- **General-purpose**: The HAMT is an associative trie that relates keys to values. The keys and values can be of any type, as long as the key implements the `Eq` and `Hash` traits.

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
{name} = "0.1"
```

This crate provides three types of HAMT:

- `HamtMap`, a hash map that implements a persistent version of the `std::collections::HashMap` interface (or at least, the parts that are not fundamentally about mutation).
- `HamtSet`, a hash set that likewise follows the `HashSet` interface.
- `HamtVec`, a dynamically growable, usize-indexable collection that implements a persistent subset of the `std::vec::Vec` interface.

### Examples

For example, to use a map:

```rs
use {name}::HamtMap;

// Note: `map` doesn't need to be mut. The `insert` method returns a new map.
let map = HamtMap::<&'static str, i32>::new();
let map = map.insert("foo", 42);
let map2 = map.insert("bar", 17);

assert_eq!(map.get("foo"), Some(&42));
assert_eq!(map2.get("bar"), Some(&17));

// The original map is unmodified.
assert_eq!(map.get("bar"), None);
```

Or to use a vec:

```rs
use {name}::HamtVec;

let vec = HamtVec::<i32>::new();
let vec = vec.push(42);
let vec2 = vec.push(17);

assert_eq!(vec[0], 42);
assert_eq!(vec2[1], 17);

// The original vec is unmodified.
assert_eq!(vec.len(), 1);
assert_eq!(vec2.len(), 2);
```

## Configuration

All `Hamt*` can be customized using a `HamtConfig`. The default configuration uses `Arc` to store both nodes and key-value pairs, and it uses the registered global allocator to allocate memory for nodes. The `Config` parameter of `Hamt*` types can be used to change these defaults. We provide the following configurations:

- `Default` defaults to `ArcConfig<Global>`
- `ArcConfig<A: Allocator>` uses `Arc` to store nodes and key-value pairs, using the provided allocator `A`. This results in a HAMT that is thread-safe.
- `RcConfig<A: Allocator>` uses `Rc` to store nodes and key-value pairs, using the provided allocator `A`. This avoids the overhead of atomic reference counting, but the resulting HAMT is not thread-safe.
- `CloningConfig<A: Allocator>` uses `Arc` to store nodes like `ArcConfig`, but stores the key-value pairs directly, cloning them when necessary. This requires that the key and value types implement `Clone`, which the other configurations do not require. This is a good choice for types that are cheap to clone or that implement `Copy`, such as integers, references, and small `Copy` structs.
- `CloningRcConfig<A: Allocator>` is like `CloningConfig`, but uses `Rc` instead of `Arc` to store nodes, with the same tradeoffs as `RcConfig`.

You may also implement your own `HamtConfig` type to further customize the behavior of the HAMT. However, this is not recommended as the `HamtConfig` trait is `unsafe` and must be implemented with care.
