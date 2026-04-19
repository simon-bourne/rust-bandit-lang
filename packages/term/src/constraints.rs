use std::{
    cell::{Cell, RefCell},
    task::{self, Waker},
};

use futures::future::{FutureExt, LocalBoxFuture};

use crate::{InferenceError, Result, sync::Latch};

#[derive(Default)]
pub struct Constraints<'a> {
    queue: RefCell<Vec<LocalBoxFuture<'a, Result<()>>>>,
    blocked_count: Cell<usize>,
}

impl<'a> Constraints<'a> {
    pub fn add(&self, constraint: impl Future<Output = Result<()>> + 'a) {
        self.queue.borrow_mut().push(constraint.boxed_local())
    }

    pub fn is_empty(&self) -> bool {
        self.queue.borrow().is_empty()
    }

    pub async fn wait(&self, latch: &Latch) {
        self.blocked_count.update(|count| count + 1);
        latch.wait().await;
        self.blocked_count.update(|count| count - 1);
    }

    pub fn solve(&self) -> Result<()> {
        loop {
            let queue = self.queue.take();

            if queue.is_empty() {
                return Ok(());
            }

            if self.blocked_count.get() >= queue.len() {
                return Err(InferenceError::CouldntInferAllTypes);
            }

            let mut pending = Vec::new();
            // TODO: Rather than `Waker::noop`, is there an executor we can use?
            let waker = Waker::noop();

            for mut future in queue {
                match future.as_mut().poll(&mut task::Context::from_waker(waker)) {
                    task::Poll::Ready(result) => result?,
                    task::Poll::Pending => pending.push(future),
                }
            }

            self.queue.borrow_mut().extend(pending);
        }
    }
}
