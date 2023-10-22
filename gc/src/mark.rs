pub use derive_mark::Mark;

pub trait Mark {
    fn mark(&mut self);
}

macro impl_mark_nop($($ty:ty),* $(,)?) {
    $(
        impl Mark for $ty {
            fn mark(&mut self) {}
        }
    )*
}

impl_mark_nop!(
    (),
    bool,
    char,
    f32,
    f64,
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    str,
    core::num::NonZeroU8,
    core::num::NonZeroU16,
    core::num::NonZeroU32,
    core::num::NonZeroU64,
    core::num::NonZeroU128,
    core::num::NonZeroUsize,
    core::num::NonZeroI8,
    core::num::NonZeroI16,
    core::num::NonZeroI32,
    core::num::NonZeroI64,
    core::num::NonZeroI128,
    core::num::NonZeroIsize,
);

impl<T: Mark> Mark for [T] {
    fn mark(&mut self) {
        for item in self {
            item.mark();
        }
    }
}

impl<T: Mark, const SIZE: usize> Mark for [T; SIZE] {
    fn mark(&mut self) {
        for item in self {
            item.mark();
        }
    }
}

impl<T: Mark> Mark for Option<T> {
    fn mark(&mut self) {
        if let Some(item) = self {
            item.mark();
        }
    }
}

impl<T: Mark, E: Mark> Mark for Result<T, E> {
    fn mark(&mut self) {
        match self {
            Ok(item) => item.mark(),
            Err(item) => item.mark(),
        }
    }
}

macro impl_mark_tuple($($ty:ident),*) {
    #[allow(non_snake_case)]
    impl <$($ty: Mark),*> Mark for ($($ty),*) {
        fn mark(&mut self) {
            let ($($ty,)*) = self;
            $($ty.mark();)*
        }
    }
}

// Generate for tuples up to length 26
impl_mark_tuple!(A, B);
impl_mark_tuple!(A, B, C);
impl_mark_tuple!(A, B, C, D);
impl_mark_tuple!(A, B, C, D, E);
impl_mark_tuple!(A, B, C, D, E, F);
impl_mark_tuple!(A, B, C, D, E, F, G);
impl_mark_tuple!(A, B, C, D, E, F, G, H);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y);
impl_mark_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);

#[cfg(test)]
mod tests {
    use crate::{
        lock_default_gc,
        mark::Mark,
        ptr::{Gc, Root},
        GarbageCollector,
    };

    #[test]
    fn derive_mark() {
        #[derive(Mark)]
        struct Foo {
            a: Gc<str>,
            b: [Bar; 3],
        }

        #[derive(Mark)]
        enum Bar {
            A,
            B(usize, Gc<usize>),
            C { a: Gc<str>, b: usize },
        }

        let rstr = Root::<str>::from("Hello world!");
        let rsize = Root::<usize>::from(0);

        let weak_str = rstr.get_weak();
        let weak_size = rsize.get_weak();

        let foo = Root::from(Foo {
            a: unsafe { rstr.as_unrooted() },
            b: [
                Bar::A,
                Bar::B(0, unsafe { rsize.as_unrooted() }),
                Bar::C {
                    a: unsafe { rstr.as_unrooted() },
                    b: 0,
                },
            ],
        });

        drop(rstr);
        drop(rsize);

        lock_default_gc(|gc| gc.collect().unwrap());

        assert!(weak_str.is_present());
        assert!(weak_size.is_present());

        drop(foo);

        lock_default_gc(|gc| gc.collect().unwrap());

        assert!(!weak_str.is_present());
        assert!(!weak_size.is_present());
    }
}
