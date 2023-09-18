use std::marker::PhantomData;

#[derive(Debug)]
pub struct Arena<T>(Vec<T>);

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self(Vec::with_capacity(8))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Id<T>(usize, PhantomData<fn() -> T>);

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Arena<T> {
    pub fn alloc(&mut self, t: T) -> Id<T> {
        self.0.push(t);
        Id(self.0.len() - 1, PhantomData)
    }

    pub fn get(&self, index: Id<T>) -> &T {
        &self.0[index.0]
    }
}
