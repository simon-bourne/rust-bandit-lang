use crate::Result;

#[derive(Clone)]
pub struct Constraints;

impl Constraints {
    pub fn empty() -> Self {
        Self
    }

    pub fn add(&self, _constraint: impl Future<Output = Result<()>>) {
        todo!()
    }

    pub fn solve(&self) {}
}
