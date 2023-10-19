pub trait Mark {
    fn mark(&mut self);
}

// impl<T: Mark + ?Sized> Mark for GcRoot<T>
// where
//     <GcObject<T> as Pointee>::Metadata: UsizeMetadata,
// {
//     fn mark(&mut self) {
//         // Roots do not mark their referent because they are the roots of the mark phase.
//     }
// }

// impl<T: Mark + ?Sized> Mark for Gc<T> {
//     fn mark(&mut self) {
//         unsafe { self.ptr.as_mut() }.mark()
//     }
// }

// impl<T: Mark + ?Sized> Mark for GcWeak<T> {
//     fn mark(&mut self) {
//         // Weak refs do not mark their referent.
//     }
// }

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
