use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{InferenceError, Level, Result, ast, core};

enum Term<'a> {
    Core(core::Term<'a>),
    Ast(ast::Term<'a>),
}

pub struct Context<'a> {
    variables: HashMap<&'a str, Vec<Level>>,
    current_level: Level,
}

impl<'src> Context<'src> {
    pub fn new(constants: impl IntoIterator<Item = (&'src str, ast::Term<'src>)>) -> Self {
        Self {
            variables: HashMap::new(),
            current_level: Level(0),
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
        let Some(name) = name else { return f(self) };

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
        self.lookup_local(name).ok_or_else(|| InferenceError)
    }

    fn lookup_local(&self, name: &'src str) -> Option<core::Term<'src>> {
        let level = self.variables.get(name)?.last()?;
        
        
        core::Term::variable(name, self.variables.get(name)?.last()?.clone())
    }
}
