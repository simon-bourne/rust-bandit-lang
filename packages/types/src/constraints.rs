use futures::future::{FutureExt, LocalBoxFuture};

use crate::SharedMut;

#[derive(Clone)]
pub struct Constraints<'a>(SharedMut<Vec<LocalBoxFuture<'a, ()>>>);

impl<'a> Constraints<'a> {
    pub fn empty() -> Self {
        Self(SharedMut::new(Vec::new()))
    }

    pub fn add(&self, constraint: impl Future<Output = ()> + 'a) {
        self.0.borrow_mut().push(constraint.boxed_local())
    }

    pub fn solve(&self) {
        // TODO: implement
    }
}
