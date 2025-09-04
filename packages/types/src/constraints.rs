#[derive(Clone)]
pub struct Constraints;

impl Constraints {
    pub fn empty() -> Self {
        Self
    }

    pub fn add(&self, _constraint: impl Future<Output = ()>) {
        // TODO: implement
    }

    pub fn solve(&self) {
        // TODO: implement
    }
}
