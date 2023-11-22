use std::marker::PhantomData;

use super::Value;

pub struct Type<T: Value> {
    _ph: PhantomData<T>,
}

impl<T: Value> Value for Type<T> {
    const NAME: &'static str = "type";

    fn egal(&self, _: &Self) -> bool {
        true
    }
}

impl<T: Value> Type<T> {
    pub const fn new() -> Self {
        Self { _ph: PhantomData }
    }
}
