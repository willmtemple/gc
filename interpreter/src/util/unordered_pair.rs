use std::hash::Hash;

pub struct UnorderedPair<T>(pub T, pub T);

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

impl<T: Ord> PartialOrd for UnorderedPair<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord> Ord for UnorderedPair<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.a().cmp(other.a()) {
            std::cmp::Ordering::Equal => self.b().cmp(other.b()),
            other => other,
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

    pub fn into_tuple(self) -> (T, T) {
        if matches!(self.0.cmp(&self.1), std::cmp::Ordering::Less) {
            (self.0, self.1)
        } else {
            (self.1, self.0)
        }
    }
}
