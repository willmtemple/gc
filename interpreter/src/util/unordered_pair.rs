use std::hash::Hash;

pub struct UnorderedPair<T>(T, T);

impl<T: PartialEq> PartialEq for UnorderedPair<T> {
    fn eq(&self, other: &Self) -> bool {
        (self.0 == other.0 && self.1 == other.1) || (self.0 == other.1 && self.1 == other.0)
    }
}

impl<T: Eq> Eq for UnorderedPair<T> {}

impl<T: Ord + Hash> Hash for UnorderedPair<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if matches!(self.0.cmp(&self.1), std::cmp::Ordering::Less) {
            self.0.hash(state);
            self.1.hash(state);
        } else {
            self.1.hash(state);
            self.0.hash(state);
        }
    }
}

impl<T: Ord> UnorderedPair<T> {
    pub fn a(&self) -> &T {
        if matches!(self.0.cmp(&self.1), std::cmp::Ordering::Less) {
            &self.0
        } else {
            &self.1
        }
    }

    pub fn b(&self) -> &T {
        if matches!(self.0.cmp(&self.1), std::cmp::Ordering::Less) {
            &self.1
        } else {
            &self.0
        }
    }

    pub fn to_tuple(self) -> (T, T) {
        if matches!(self.0.cmp(&self.1), std::cmp::Ordering::Less) {
            (self.0, self.1)
        } else {
            (self.1, self.0)
        }
    }
}
