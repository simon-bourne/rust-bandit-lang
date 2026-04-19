use std::cell::RefCell;

use async_executor::{LocalExecutor, Task};
use futures::executor::block_on;

use crate::{Result, sync::Latch};

#[derive(Default)]
pub struct Constraints<'a> {
    executor: LocalExecutor<'a>,
    queue: RefCell<Vec<Task<Result<()>>>>,
}

impl<'a> Constraints<'a> {
    pub fn add(&self, constraint: impl Future<Output = Result<()>> + 'a) {
        let task = self.executor.spawn(constraint);
        self.queue.borrow_mut().push(task);
    }

    pub async fn wait(&self, latch: &Latch) {
        latch.wait().await;
    }

    pub fn solve(&self) -> Result<()> {
        while self.executor.try_tick() {}

        for task in self.queue.take() {
            if task.is_finished() {
                block_on(task)?;
            } else {
                return Err(crate::InferenceError::CouldntInferAllTypes);
            }
        }

        Ok(())
    }
}
