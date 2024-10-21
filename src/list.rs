#[derive(Debug)]
pub struct List<T>(Vec<T>);

impl<T> Default for List<T> {
    fn default() -> Self {
        Self(Vec::default())
    }
}

impl<T> List<T> {
    pub fn add(&mut self, item: T) -> Id<T> {
        let id = Id(self.0.len(), std::marker::PhantomData);
        self.0.push(item);
        id
    }

    pub fn add_default(&mut self) -> Id<T>
    where
        T: Default,
    {
        self.add(Default::default())
    }
}

// #[derive(Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct Id<T>(usize, std::marker::PhantomData<T>);

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> std::hash::Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T> Eq for Id<T> {}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl<T> std::ops::Index<Id<T>> for List<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        &self.0[index.0]
    }
}

impl<T> std::ops::IndexMut<Id<T>> for List<T> {
    fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}
