use std::cell::RefCell;

use async_executor::{LocalExecutor, Task};
use futures::executor::block_on;

use crate::{InferenceError, Result};

#[derive(Default)]
pub struct Constraints<'a> {
    executor: LocalExecutor<'a>,
    queue: RefCell<Vec<Task<Result<()>>>>,
}

impl<'a> Constraints<'a> {
    /// Add a constraint
    ///
    /// Be careful about blocking in `constraint`. If no constraints can make
    /// progress, `solve` returns an error saying it's blocked.
    pub fn add(&self, constraint: impl Future<Output = Result<()>> + 'a) {
        let task = self.executor.spawn(constraint);
        self.queue.borrow_mut().push(task);
    }

    pub fn solve(&self) -> Result<()> {
        while self.executor.try_tick() {}

        for task in self.queue.take() {
            if task.is_finished() {
                block_on(task)?;
            } else {
                return Err(InferenceError::CouldntInferAllTypes);
            }
        }

        Ok(())
    }
}
