use crate::ExpressionRef;

#[derive(Copy, Clone)]
pub struct DeBruijnLevel(usize);

#[derive(Copy, Clone)]
pub struct DeBruijnIndex(usize);

#[derive(Default)]
pub struct Context<'a> {
    local_variables: Vec<ExpressionRef<'a>>,
}

impl<'a> Context<'a> {
    pub fn push(&mut self, expr: ExpressionRef<'a>) -> DeBruijnLevel {
        let level = DeBruijnLevel(self.local_variables.len());
        self.local_variables.push(expr);
        level
    }

    pub fn pop(&mut self) {
        self.local_variables.pop();
    }

    pub fn get(&self, index: DeBruijnIndex) -> ExpressionRef<'a> {
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
