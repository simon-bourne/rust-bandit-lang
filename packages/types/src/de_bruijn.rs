#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Level(usize);

impl Level {
    pub fn top() -> Self {
        Self(0)
    }

    pub fn open(self) -> Self {
        Self(self.0 + 1)
    }
}
