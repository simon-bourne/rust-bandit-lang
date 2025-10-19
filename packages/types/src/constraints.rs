use std::task::{self, Waker};

use futures::future::{FutureExt, LocalBoxFuture};

use crate::{InferenceError, Result, SharedMut};

#[derive(Clone)]
pub struct Constraints<'a>(SharedMut<Vec<LocalBoxFuture<'a, Result<()>>>>);

impl<'a> Constraints<'a> {
    pub fn empty() -> Self {
        Self(SharedMut::new(Vec::new()))
    }

    pub fn add(&self, constraint: impl Future<Output = Result<()>> + 'a) {
        self.0.borrow_mut().push(constraint.boxed_local())
    }

    // TODO: Tidy
    pub fn solve(&self) -> Result<()> {
        loop {
            let current = self.0.replace_with(Vec::new());

            if current.is_empty() {
                return Ok(());
            }

            let mut pending = Vec::new();
            let waker = Waker::noop();
            let mut any_solved = false;

            for mut future in current {
                match future.as_mut().poll(&mut task::Context::from_waker(waker)) {
                    task::Poll::Ready(result) => {
                        result?;
                        any_solved = true;
                    }
                    task::Poll::Pending => pending.push(future),
                }
            }

            if self.0.borrow().is_empty() && pending.is_empty() {
                // We didn't solve any constraints, so we're stuck
                if !any_solved {
                    return Err(InferenceError);
                } else {
                    return Ok(());
                }
            }

            self.0.borrow_mut().extend(pending);
        }
    }
}
