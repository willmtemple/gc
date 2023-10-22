pub trait UsizeMetadata {
    fn to_usize(self) -> usize;

    fn from_usize(size: usize) -> Self;
}

impl UsizeMetadata for usize {
    #[inline(always)]
    fn to_usize(self) -> usize {
        self
    }

    #[inline(always)]
    fn from_usize(size: usize) -> Self {
        size
    }
}

impl UsizeMetadata for () {
    #[inline(always)]
    fn to_usize(self) -> usize {
        0
    }

    #[inline(always)]
    #[allow(clippy::unused_unit)]
    fn from_usize(_: usize) -> Self {
        ()
    }
}
