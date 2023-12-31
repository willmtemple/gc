use hamt::{config::CloningConfig, HamtMap, HamtVec};

#[test]
fn hm_test() {
    let hamt_mt = HamtMap::<u64, u64>::new();

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

    for (hamt_idx, hamt) in hamts.iter().enumerate() {
        for i in 0..=hamt_idx {
            assert_eq!(hamt.get(&(i as u64)), Some(&(i as u64 + 1)))
        }
    }

    let mut last_hamt = hamts[hamts.len() - 1].clone();

    for i in 0..=(HAMTS / 2) {
        last_hamt = last_hamt.remove(&(i * 2));

        assert_eq!(last_hamt.get(&(i * 2)), None);
    }

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
    let mut hv = HamtVec::new_with_config(CloningConfig::default());

    hv = hv.push(0);

    assert_eq!(hv.len(), 1);
    assert_eq!(hv[0], 0);

    hv.push(1);

    assert_eq!(hv.len(), 1);

    hv = hv.push(1);

    assert_eq!(hv.len(), 2);

    for i in 2..64 {
        hv = hv.push(i);
    }

    assert_eq!(hv.len(), 64);

    hv = hv.push(64);

    for i in 65..128 {
        hv = hv.push(i);
    }

    assert_eq!(hv.len(), 128);

    hv = hv.push(128);

    assert_eq!(hv.len(), 129);
}

#[test]
fn hv_iter() {
    let v = [0, 1, 2, 3, 4, 5, 6, 7];
    let mut hv = HamtVec::new_with_config(CloningConfig::default());

    for i in v.iter() {
        hv = hv.push(*i);
    }

    for i in &hv {
        assert_eq!(*i, v[*i as usize]);
    }

    let slice = hv.slice(1..5);

    assert_eq!(slice.len(), 4);
    assert_eq!(slice[0], 1);
    assert_eq!(slice[1], 2);
    assert_eq!(slice[2], 3);
    assert_eq!(slice[3], 4);

    for (idx, v) in (&slice).into_iter().enumerate() {
        assert_eq!(*v, idx as i32 + 1);
    }

    if HamtVec::<()>::new().iter().next().is_some() {
        unreachable!();
    }

    if HamtVec::<()>::new().slice(0..0).iter().next().is_some() {
        unreachable!();
    }
}

// #[test]
// fn check_align() {
//     // Check that the offset of _header within the node type is 0 i.e. that a pointer
//     // to the node type is also a pointer to the header.

//     let data = DefaultGlobal::allocate::<Collision<(), (), DefaultGlobal>>(0, |collision| {
//         unsafe {
//             core::ptr::write(
//                 &mut collision._header,
//                 NodeHeader::new::<Collision<(), (), DefaultGlobal>>(MAX_LEVEL, 0, 0),
//             )
//         };
//     });

//     let node_ptr = data.as_ref() as *const _ as *const ();
//     let node_header_ptr = data as *const _ as *const ();

//     assert_eq!(node_ptr as usize, node_header_ptr as usize);

//     let data = DefaultGlobal::allocate::<InnerNode<(), (), DefaultGlobal>>(0, |inner| {
//         unsafe {
//             core::ptr::write(
//                 &mut inner._header,
//                 NodeHeader::new::<InnerNode<(), (), DefaultGlobal>>(MAX_LEVEL, 0, 0),
//             )
//         };
//     });

//     let node_ptr = data.as_ref() as *const _ as *const ();
//     let node_header_ptr = data.header() as *const _ as *const ();

//     assert_eq!(node_ptr as usize, node_header_ptr as usize);
// }
