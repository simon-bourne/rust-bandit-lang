pub struct Constraints;

impl Constraints {
    pub fn empty() -> Self {
        Self
    }

    pub fn merge(&mut self, _other: Self) {}

    pub fn solve(&mut self) {}
}
