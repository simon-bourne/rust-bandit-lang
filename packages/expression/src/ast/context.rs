use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{InferenceError, Result, ast, constraints::Constraints, typed};

enum Term<'a> {
    Core(typed::Term<'a>),
    Ast(ast::Term<'a>),
}

type MutableValue<'a> = Value<RefCell<Term<'a>>>;

pub struct Value<T> {
    pub value: T,
    pub typ: T,
}

impl<'src> Value<ast::Term<'src>> {
    pub fn new(value: ast::Term<'src>) -> Self {
        Self {
            value,
            typ: ast::Term::unknown(),
        }
    }
}

impl<'src> From<Value<ast::Term<'src>>> for MutableValue<'src> {
    fn from(value: Value<ast::Term<'src>>) -> Self {
        Value {
            value: RefCell::new(Term::Ast(value.value)),
            typ: RefCell::new(Term::Ast(value.typ)),
        }
    }
}

pub struct Context<'a> {
    variables: HashMap<&'a str, Vec<typed::Term<'a>>>,
    constants: Rc<HashMap<&'a str, MutableValue<'a>>>,
    constraints: Constraints<'a>,
}

impl<'a> Context<'a> {
    pub fn new(constants: impl IntoIterator<Item = (&'a str, Value<ast::Term<'a>>)>) -> Self {
        Self {
            variables: HashMap::new(),
            constants: Rc::new(
                constants
                    .into_iter()
                    .map(|(name, value)| (name, value.into()))
                    .collect(),
            ),
            constraints: Constraints::empty(),
        }
    }

    pub fn infer_types(&self) -> Result<()> {
        for Value { value, typ } in self.constants.values() {
            self.desugar_term(value)?
                .has_type(self.desugar_term(typ)?, &self.constraints);
        }

        self.constraints.solve()?;

        for Value { value, .. } in self.constants.values() {
            self.desugar_term(value)?.check_scope()?;
        }

        Ok(())
    }

    pub fn constants(&self) -> impl Iterator<Item = (&'a str, Result<typed::Term<'a>>)> {
        self.constants
            .iter()
            .map(|(name, Value { value, .. })| (*name, self.desugar_term(value)))
    }

    pub fn constraints(&self) -> &Constraints<'a> {
        &self.constraints
    }

    pub(crate) fn in_scope<Output>(
        &mut self,
        mut variable: typed::Term<'a>,
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

    pub(crate) fn lookup(&mut self, name: &'a str) -> Result<typed::Term<'a>> {
        Ok(if let Some(local) = self.lookup_local(name) {
            local
        } else {
            typed::Term::constant(
                name,
                self.desugar_term(
                    &self
                        .constants
                        .get(name)
                        .ok_or(InferenceError::VariableNotFound)?
                        .typ,
                )?,
            )
        })
    }

    fn lookup_local(&self, name: &'a str) -> Option<typed::Term<'a>> {
        Some(self.variables.get(name)?.last()?.clone())
    }

    fn desugar_term(&self, term: &RefCell<Term<'a>>) -> Result<typed::Term<'a>> {
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
