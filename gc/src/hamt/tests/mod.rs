use crate::hamt::{
    config::{CloneKvpArcGlobal, DefaultGlobal, HamtConfig},
    node::{util::MAX_LEVEL, Collision, HamtNode, InnerNode, NodeHeader},
};

use super::HamtVec;

#[test]
fn print_trees() {
    let hamt_mt = super::HamtMap::<u64, u64>::new();

    assert!(hamt_mt.get(&0).is_none());

    let hamt_0 = hamt_mt.insert(0, 1);

    assert_eq!(hamt_mt.get(&0), None);
    assert_eq!(hamt_0.get(&0), Some(&1));

    let hamt_mt2 = hamt_0.remove(&0);

    assert_eq!(hamt_mt2.get(&0), None);

    let hamt_1 = hamt_0.insert(1, 2);

    assert_eq!(hamt_1.get(&0), Some(&1));
    assert_eq!(hamt_1.get(&1), Some(&2));
    assert_eq!(hamt_1.get(&2), None);
    assert_eq!(hamt_0.get(&1), None);
    assert_eq!(hamt_0.get(&0), Some(&1));

    let mut hamt = hamt_1.clone();

    let mut hamts = vec![hamt_0, hamt_1];

    const HAMTS: u64 = 1000;

    for i in 2..HAMTS {
        hamt = hamt.insert(i, i + 1);
        hamts.push(hamt.clone());

        assert_eq!(hamt.get(&i), Some(&(i + 1)));
    }

    hamts[hamts.len() - 1].print();
    hamts[hamts.len() - 2].print();

    for (hamt_idx, hamt) in hamts.iter().enumerate() {
        for i in 0..=hamt_idx {
            assert_eq!(hamt.get(&(i as u64)), Some(&(i as u64 + 1)))
        }
    }

    hamts[hamts.len() - 1].print();

    let mut last_hamt = hamts[hamts.len() - 1].clone();

    for i in 0..=(HAMTS / 2) {
        last_hamt = last_hamt.remove(&(i * 2));

        assert_eq!(last_hamt.get(&(i * 2)), None);
    }

    last_hamt.print();

    for idx in 0..HAMTS {
        if idx % 2 == 0 {
            assert_eq!(last_hamt.get(&idx), None);
        } else {
            assert_eq!(last_hamt.get(&idx), Some(&(idx + 1)));
        }
    }
}

#[test]
fn hamt_vec() {
    let mut hv = HamtVec::<i32, CloneKvpArcGlobal>::new();

    hv = hv.push(0);

    assert_eq!(hv.len(), 1);

    hv.push(1);

    assert_eq!(hv.len(), 1);

    hv = hv.push(1);

    hv.print();

    assert_eq!(hv.len(), 2);

    for i in 2..64 {
        hv = hv.push(i);
    }

    hv.print();

    assert_eq!(hv.len(), 64);

    hv = hv.push(64);

    for i in 65..128 {
        hv = hv.push(i);
    }

    hv.print();

    assert_eq!(hv.len(), 128);

    hv = hv.push(128);

    hv.print()
}

#[test]
fn hv_iter() {
    let v = [0, 1, 2, 3, 4, 5, 6, 7];
    let mut hv = HamtVec::<i32, CloneKvpArcGlobal>::new();

    for i in v.iter() {
        hv = hv.push(*i);
    }

    for (idx, v) in hv.iter().enumerate() {
        // eprintln!("Checking index: {} with value {}", idx, v);
        assert_eq!(*v, idx as i32);
    }
}

#[test]
fn check_align() {
    // Check that the offset of _header within the node type is 0 i.e. that a pointer
    // to the node type is also a pointer to the header.

    let data = DefaultGlobal::allocate::<Collision<(), (), DefaultGlobal>>(0, |collision| {
        unsafe {
            core::ptr::write(
                &mut collision._header,
                NodeHeader::new::<Collision<(), (), DefaultGlobal>>(MAX_LEVEL, 0, 0),
            )
        };
    });

    let node_ptr = data.as_ref() as *const _ as *const ();
    let node_header_ptr = data.header() as *const _ as *const ();

    assert_eq!(node_ptr as usize, node_header_ptr as usize);

    let data = DefaultGlobal::allocate::<InnerNode<(), (), DefaultGlobal>>(0, |inner| {
        unsafe {
            core::ptr::write(
                &mut inner._header,
                NodeHeader::new::<InnerNode<(), (), DefaultGlobal>>(MAX_LEVEL, 0, 0),
            )
        };
    });

    let node_ptr = data.as_ref() as *const _ as *const ();
    let node_header_ptr = data.header() as *const _ as *const ();

    assert_eq!(node_ptr as usize, node_header_ptr as usize);
}
