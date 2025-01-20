use std::fmt;

use crate::ExpressionRef;

#[derive(Copy, Clone, Debug)]
pub struct DeBruijnLevel(usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct DeBruijnIndex(usize);

impl fmt::Display for DeBruijnIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Default)]
pub struct Context<'a> {
    local_variables: Vec<ExpressionRef<'a>>,
}

impl<'a> Context<'a> {
    pub fn push_type(&mut self, expr: ExpressionRef<'a>) -> DeBruijnLevel {
        let level = DeBruijnLevel(self.local_variables.len());
        self.local_variables.push(expr);
        level
    }

    pub fn pop(&mut self) {
        self.local_variables.pop();
    }

    pub fn get_type(&self, index: DeBruijnIndex) -> ExpressionRef<'a> {
        let len = self.local_variables.len();
        assert!(index.0 <= len);
        self.local_variables[len - index.0].clone()
    }

    pub fn de_bruijn_index(&self, level: DeBruijnLevel) -> DeBruijnIndex {
        let len = self.local_variables.len();
        assert!(level.0 < len);
        DeBruijnIndex(len - level.0)
    }
}
