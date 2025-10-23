use std::collections::HashMap;

use crate::{DeBruijnIndex, InferenceError, Result, ast, core};

pub struct Context<'src> {
    variables: HashMap<&'src str, Vec<Level>>,
    current_level: Level,
}

impl<'src> Context<'src> {
    pub fn new(_constants: impl IntoIterator<Item = (&'src str, ast::Term<'src>)>) -> Self {
        Self {
            variables: HashMap::new(),
            current_level: Level::top(),
        }
    }

    pub fn infer_types(&self) -> Result<()> {
        todo!()
    }

    pub(crate) fn in_scope<Output>(
        &mut self,
        name: Option<&'src str>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Output {
        let Some(name) = name else {
            return f(self);
        };

        self.variables
            .entry(name)
            .or_default()
            .push(self.current_level);
        self.current_level.push();
        let output = f(self);
        self.current_level.pop();

        if self.variables.get_mut(name).unwrap().pop().is_none() {
            self.variables.remove(name);
        }

        output
    }

    pub(crate) fn lookup(&mut self, name: &'src str) -> Result<core::Term<'src>> {
        self.lookup_local(name).ok_or(InferenceError)
    }

    fn lookup_local(&self, name: &'src str) -> Option<core::Term<'src>> {
        let variable_level = self.variables.get(name)?.last()?;
        let de_bruijn_index = variable_level.de_bruijn_index(self.current_level);

        Some(core::Term::variable(name, de_bruijn_index))
    }
}

#[derive(Copy, Clone)]
struct Level(usize);

impl Level {
    pub fn top() -> Self {
        Self(0)
    }

    pub fn push(&mut self) {
        self.0 += 1;
    }

    pub fn pop(&mut self) {
        self.0 -= 1;
    }

    pub fn de_bruijn_index(self, current_level: Self) -> DeBruijnIndex {
        DeBruijnIndex(current_level.0 - self.0 - 1)
    }
}
