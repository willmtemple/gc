#![feature(allocator_api)]

use gc::hamt;

fn main() {
    let hamt = hamt::Hamt::<i32, u32>::new().insert(0, 1).insert(1, 2);

    assert_eq!(hamt.get(&0), Some(&1));
    assert_eq!(hamt.get(&1), Some(&2));

    println!("Hello, world! {:?}", hamt.get(&0))
}
