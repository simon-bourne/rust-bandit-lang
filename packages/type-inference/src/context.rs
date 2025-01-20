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

pub struct Variable<'a> {
    level: DeBruijnLevel,
    typ: ExpressionRef<'a>,
}

impl<'a> Variable<'a> {
    pub fn to_expression(&self, ctx: &Context<'a>) -> ExpressionRef<'a> {
        ExpressionRef::variable(ctx.de_bruijn_index(self.level), self.typ.clone())
    }
}

#[derive(Default)]
pub struct Context<'a> {
    local_variables: Vec<ExpressionRef<'a>>,
}

impl<'a> Context<'a> {
    pub fn with_variable<Output>(
        &mut self,
        typ: ExpressionRef<'a>,
        f: impl FnOnce(&mut Self, Variable<'a>) -> Output,
    ) -> Output {
        let level = self.push(typ.clone());
        let var = Variable { level, typ };
        let output = f(self, var);
        self.pop();
        output
    }

    pub fn get_type(&self, index: DeBruijnIndex) -> ExpressionRef<'a> {
        let len = self.local_variables.len();
        assert!(index.0 <= len);
        self.local_variables[len - index.0].clone()
    }

    fn de_bruijn_index(&self, level: DeBruijnLevel) -> DeBruijnIndex {
        let len = self.local_variables.len();
        assert!(level.0 < len);
        DeBruijnIndex(len - level.0)
    }

    // TODO: Make private
    fn push(&mut self, expr: ExpressionRef<'a>) -> DeBruijnLevel {
        let level = DeBruijnLevel(self.local_variables.len());
        self.local_variables.push(expr);
        level
    }

    fn pop(&mut self) {
        self.local_variables.pop();
    }
}
