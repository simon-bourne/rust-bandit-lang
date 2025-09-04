use std::task::{self, Waker};

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
        loop {
            let current = self.0.replace_with(Vec::new());
            let mut pending = Vec::new();
            let waker = Waker::noop();
            let mut any_solved = false;

            for mut future in current {
                if future
                    .as_mut()
                    .poll(&mut task::Context::from_waker(waker))
                    .is_pending()
                {
                    pending.push(future);
                } else {
                    any_solved = true;
                }
            }

            if pending.is_empty() || !any_solved {
                return;
            }

            self.0.replace_with(pending);
        }
    }
}
