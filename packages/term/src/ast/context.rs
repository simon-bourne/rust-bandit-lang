use std::{cell::RefCell, collections::HashMap, rc::Rc};

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

pub struct Data<'src> {
    type_constructor: MutableConstant<'src>,
    value_constructors: Vec<(&'src str, MutableConstant<'src>)>,
}

impl<'src> Data<'src> {
    pub fn new(data: ast::Data<'src>) -> Self {
        let value_constructors = data
            .value_constructors
            .into_iter()
            .map(|constant| (constant.name, Constant::new(constant)))
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

pub struct Context<'a> {
    variables: HashMap<&'a str, Vec<typed::Term<'a>>>,
    types: Rc<HashMap<&'a str, Data<'a>>>,
    constants: Rc<HashMap<&'a str, MutableValue<'a>>>,
    constraints: Constraints<'a>,
}

impl<'a> Context<'a> {
    pub fn new(
        types: impl IntoIterator<Item = ast::Data<'a>>,
        constants: impl IntoIterator<Item = (&'a str, Value<ast::Term<'a>>)>,
    ) -> Self {
        Self {
            variables: HashMap::new(),
            types: Rc::new(
                types
                    .into_iter()
                    .map(|data| (data.type_constructor.name, Data::new(data)))
                    .collect(),
            ),
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

        for Value { value, .. } in self.constants.values() {
            self.desugar_term(value)?.check_scope()?;
        }

        for data in self.types.values() {
            let value_constructors: Result<Vec<_>> = data
                .value_constructors
                .iter()
                .map(|(_, term)| self.desugar_constant(term))
                .collect();
            self.desugar_constant(&data.type_constructor)?
                .type_constructor(value_constructors?, &self.constraints);
        }

        self.constraints.solve()
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
        let term = if let Some(local) = self.lookup_local(name) {
            local
        } else if let Some(constant) = self.constants.get(name) {
            typed::Term::constant(name, self.desugar_term(&constant.typ)?)
        } else if let Some(data) = self.types.get(name) {
            self.desugar_constant(&data.type_constructor)?
        } else {
            return Err(InferenceError::VariableNotFound)?;
        };

        Ok(term)
    }

    fn lookup_local(&self, name: &'a str) -> Option<typed::Term<'a>> {
        Some(self.variables.get(name)?.last()?.clone())
    }

    fn desugar_term(&self, term: &RefCell<Term<'a>>) -> Result<typed::Term<'a>> {
        let mut term = term
            .try_borrow_mut()
            .map_err(|_| InferenceError::TopLevelCircularDependency)?;

        let typed_term = match &mut *term {
            Term::Typed(term) => term.clone(),
            Term::Ast(term) => term.desugar(&mut self.global())?,
        };

        *term = Term::Typed(typed_term.clone());

        Ok(typed_term)
    }

    fn desugar_constant(&self, constant: &MutableConstant<'a>) -> Result<typed::Term<'a>> {
        let mut constant = constant
            .try_borrow_mut()
            .map_err(|_| InferenceError::TopLevelCircularDependency)?;

        let typed_constant = match &mut *constant {
            Constant::Typed(constant) => constant.clone(),
            Constant::Ast(constant) => typed::Term::constant(
                constant.name,
                constant
                    .typ
                    .take()
                    .unwrap_or_else(ast::Term::unknown)
                    .desugar(&mut self.global())?,
            ),
        };

        *constant = Constant::Typed(typed_constant.clone());

        Ok(typed_constant)
    }

    fn global(&self) -> Self {
        Self {
            variables: HashMap::new(),
            types: self.types.clone(),
            constants: self.constants.clone(),
            constraints: self.constraints.clone(),
        }
    }
}
