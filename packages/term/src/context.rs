use std::{cell::RefCell, collections::HashMap};

use crate::{InferenceError, Result, ast, constraints::Constraints, typed};

enum Term<'a> {
    Typed(typed::Term<'a>),
    Ast(ast::Term<'a>),
}

type MutableValue<'a> = Value<RefCell<Term<'a>>>;

pub struct Value<T> {
    value: T,
    typ: T,
}

impl<'src> Value<ast::Term<'src>> {
    pub fn new(value: ast::Term<'src>) -> Self {
        Self {
            value,
            typ: ast::Term::unknown(),
        }
    }

    pub fn with_type(value: ast::Term<'src>, typ: ast::Term<'src>) -> Self {
        Self { value, typ }
    }
}

struct Data<'src> {
    type_constructor: MutableConstant<'src>,
    value_constructors: Vec<(&'src str, MutableConstant<'src>)>,
}

impl<'src> Data<'src> {
    pub fn new(data: ast::Data<'src>) -> Self {
        let value_constructors = data
            .value_constructors
            .into_iter()
            .map(|constant| (constant.name(), Constant::new(constant)))
            .collect();

        Self {
            type_constructor: Constant::new(data.type_constructor),
            value_constructors,
        }
    }
}

enum Constant<'src> {
    Ast(ast::Constant<'src>),
    Typed(typed::Term<'src>),
}

// TODO: We need a better prefix than `Mutable`. `Compilable`?
type MutableConstant<'src> = RefCell<Constant<'src>>;

impl<'src> Constant<'src> {
    fn new(value: ast::Constant<'src>) -> MutableConstant<'src> {
        RefCell::new(Self::Ast(value))
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

#[derive(Default)]
pub struct LocalVariables<'a>(HashMap<&'a str, Vec<typed::Term<'a>>>);

impl<'a> LocalVariables<'a> {
    fn lookup(&self, name: &'a str) -> Option<typed::Term<'a>> {
        Some(self.0.get(name)?.last()?.clone())
    }

    fn push(&mut self, name: &'a str, variable: typed::Term<'a>) {
        self.0.entry(name).or_default().push(variable);
    }

    fn pop(&mut self, name: &'a str) {
        if self.0.get_mut(name).unwrap().pop().is_none() {
            self.0.remove(name);
        }
    }
}

pub struct Context<'a> {
    types: HashMap<&'a str, Data<'a>>,
    constants: HashMap<&'a str, MutableValue<'a>>,
}

impl<'a> Context<'a> {
    pub fn new(
        types: impl IntoIterator<Item = ast::Data<'a>>,
        constants: impl IntoIterator<Item = (&'a str, Value<ast::Term<'a>>)>,
    ) -> Self {
        Self {
            types: types
                .into_iter()
                .map(|data| (data.type_constructor.name(), Data::new(data)))
                .collect(),
            constants: constants
                .into_iter()
                .map(|(name, value)| (name, value.into()))
                .collect(),
        }
    }

    pub fn infer_types(&'a self, constraints: &mut Constraints<'a>) -> Result<()> {
        for Value { value, typ } in self.constants.values() {
            self.desugar_term(value, constraints)?.has_type(
                self,
                self.desugar_term(typ, constraints)?,
                constraints,
            );
        }

        for Value { value, .. } in self.constants.values() {
            self.desugar_term(value, constraints)?.check_scope()?;
        }

        for data in self.types.values() {
            let value_constructors: Result<Vec<_>> = data
                .value_constructors
                .iter()
                .map(|(_, term)| self.desugar_constant(term, constraints))
                .collect();
            self.desugar_constant(&data.type_constructor, constraints)?
                .type_constructor(self, value_constructors?, constraints);
        }

        constraints.solve()
    }

    pub fn constants(
        &'a self,
        constraints: &mut Constraints<'a>,
    ) -> impl Iterator<Item = (&'a str, Result<typed::Term<'a>>)> {
        self.constants
            .iter()
            .map(|(name, Value { value, .. })| (*name, self.desugar_term(value, constraints)))
    }

    pub(crate) fn in_scope<Output>(
        &self,
        local_variables: &mut LocalVariables<'a>,
        mut variable: typed::Term<'a>,
        f: impl FnOnce(&mut LocalVariables<'a>) -> Output,
    ) -> Output {
        let Some(name) = variable.variable_name() else {
            return f(local_variables);
        };

        local_variables.push(name, variable);
        let output = f(local_variables);
        local_variables.pop(name);

        output
    }

    pub(crate) fn lookup(
        &'a self,
        variables: &mut LocalVariables<'a>,
        name: &'a str,
        constraints: &mut Constraints<'a>,
    ) -> Result<typed::Term<'a>> {
        let term = if let Some(local) = variables.lookup(name) {
            local
        } else if let Some(constant) = self.constants.get(name) {
            typed::Term::constant(name, self.desugar_term(&constant.typ, constraints)?)
        } else if let Some(data) = self.types.get(name) {
            self.desugar_constant(&data.type_constructor, constraints)?
        } else {
            return Err(InferenceError::VariableNotFound)?;
        };

        Ok(term)
    }

    fn desugar_term(
        &'a self,
        term: &RefCell<Term<'a>>,
        constraints: &mut Constraints<'a>,
    ) -> Result<typed::Term<'a>> {
        let mut term = term
            .try_borrow_mut()
            .map_err(|_| InferenceError::TopLevelCircularDependency)?;

        let typed_term = match &mut *term {
            Term::Typed(term) => term.clone(),
            Term::Ast(term) => term.desugar(self, constraints)?,
        };

        *term = Term::Typed(typed_term.clone());

        Ok(typed_term)
    }

    fn desugar_constant(
        &'a self,
        constant: &MutableConstant<'a>,
        constraints: &mut Constraints<'a>,
    ) -> Result<typed::Term<'a>> {
        let mut constant = constant
            .try_borrow_mut()
            .map_err(|_| InferenceError::TopLevelCircularDependency)?;

        let typed_constant = match &mut *constant {
            Constant::Typed(constant) => constant.clone(),
            Constant::Ast(constant) => constant.desugar(self, constraints)?,
        };

        *constant = Constant::Typed(typed_constant.clone());

        Ok(typed_constant)
    }
}
