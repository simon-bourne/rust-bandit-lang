use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{InferenceError, Result, ast, constraints::Constraints, core};

enum Term<'a> {
    Core(core::Term<'a>),
    Ast(ast::Term<'a>),
}

pub struct Context<'a> {
    variables: HashMap<&'a str, Vec<core::Term<'a>>>,
    constants: Rc<HashMap<&'a str, RefCell<Term<'a>>>>,
    constraints: Constraints<'a>,
}

impl<'a> Context<'a> {
    pub fn new(constants: impl IntoIterator<Item = (&'a str, ast::Term<'a>)>) -> Self {
        Self {
            variables: HashMap::new(),
            constants: Rc::new(
                constants
                    .into_iter()
                    .map(|(name, value)| (name, RefCell::new(Term::Ast(value))))
                    .collect(),
            ),
            constraints: Constraints::empty(),
        }
    }

    pub fn infer_types(&self) -> Result<()> {
        for value in self.constants.values() {
            self.desugar_constant(value)?;
        }

        self.constraints.solve()
    }

    pub fn constants(&self) -> impl Iterator<Item = (&'a str, Result<core::Term<'a>>)> {
        self.constants
            .iter()
            .map(|(name, value)| (*name, self.desugar_constant(value)))
    }

    pub fn constraints(&self) -> &Constraints<'a> {
        &self.constraints
    }

    pub(crate) fn in_scope<Output>(
        &mut self,
        mut variable: core::Term<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Output {
        let Some(name) = variable.variable_name() else {
            return f(self);
        };

        self.variables.entry(name).or_default().push(variable);
        let output = f(self);

        if self.variables.get_mut(name).unwrap().pop().is_none() {
            self.variables.remove(name);
        }

        output
    }

    pub(crate) fn lookup(&mut self, name: &'a str) -> Result<core::Term<'a>> {
        Ok(if let Some(local) = self.lookup_local(name) {
            local
        } else {
            core::Term::constant(
                name,
                self.desugar_constant(
                    self.constants
                        .get(name)
                        .ok_or(InferenceError::VariableNotFound)?,
                )?,
            )
        })
    }

    fn lookup_local(&self, name: &'a str) -> Option<core::Term<'a>> {
        Some(self.variables.get(name)?.last()?.clone())
    }

    fn desugar_constant(&self, term: &RefCell<Term<'a>>) -> Result<core::Term<'a>> {
        let mut term = term
            .try_borrow_mut()
            .map_err(|_| InferenceError::TopLevelCircularDependency)?;

        let typed_term = match &mut *term {
            Term::Core(term) => term.clone(),
            Term::Ast(term) => {
                let mut global_ctx = Context {
                    variables: HashMap::new(),
                    constants: self.constants.clone(),
                    constraints: self.constraints.clone(),
                };

                term.desugar(&mut global_ctx)?
            }
        };

        *term = Term::Core(typed_term.clone());

        Ok(typed_term)
    }
}
