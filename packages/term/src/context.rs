use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::{InferenceError, Result, ast, constraints::Constraints, typed};

enum Term<'a> {
    Typed(typed::Term<'a>),
    Ast(ast::Term<'a>),
}

type MutableValue<'a> = Value<RefCell<Term<'a>>>;

/// The value of a constant: `constant : <type> = <value>`
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
    pub(crate) fn in_scope<Output>(
        &mut self,
        mut variable: typed::Term<'a>,
        f: impl FnOnce(&mut LocalVariables<'a>) -> Output,
    ) -> Output {
        let Some(name) = variable.variable_name() else {
            return f(self);
        };

        self.push(name, variable);
        let output = f(self);
        self.pop(name);

        output
    }

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

pub struct ContextData<'a> {
    types: HashMap<&'a str, Data<'a>>,
    constants: HashMap<&'a str, MutableValue<'a>>,
    constraints: RefCell<Constraints<'a>>,
}

pub struct ContextOwner<'a>(Rc<RefCell<ContextData<'a>>>);

impl<'a> ContextOwner<'a> {
    pub fn new(
        types: impl IntoIterator<Item = ast::Data<'a>>,
        constants: impl IntoIterator<Item = (&'a str, Value<ast::Term<'a>>)>,
    ) -> Self {
        Self(Rc::new(RefCell::new(ContextData {
            types: types
                .into_iter()
                .map(|data| (data.type_constructor.name(), Data::new(data)))
                .collect(),
            constants: constants
                .into_iter()
                .map(|(name, value)| (name, value.into()))
                .collect(),
            constraints: RefCell::default(),
        })))
    }

    pub fn handle(&self) -> Context<'a> {
        Context(Rc::downgrade(&self.0))
    }
}

#[derive(Clone)]
pub struct Context<'a>(Weak<RefCell<ContextData<'a>>>);

impl<'a> Context<'a> {
    pub fn constraint(&self, constraint: impl Future<Output = Result<()>> + 'a) {
        self.rc().borrow().constraints.borrow_mut().add(constraint)
    }

    pub fn infer_types(&self) -> Result<()> {
        let this = self.rc();

        for Value { value, typ } in this.borrow().constants.values() {
            self.desugar_term(value)?
                .has_type(self, self.desugar_term(typ)?);
        }

        for Value { value, .. } in this.borrow().constants.values() {
            self.desugar_term(value)?.check_scope()?;
        }

        for data in this.borrow().types.values() {
            let value_constructors: Result<Vec<_>> = data
                .value_constructors
                .iter()
                .map(|(_, term)| self.desugar_constant(term))
                .collect();
            self.desugar_constant(&data.type_constructor)?
                .type_constructor(self, value_constructors?);
        }

        this.borrow().constraints.borrow_mut().solve()
    }

    pub fn constants(&self) -> impl Iterator<Item = (&'a str, Result<typed::Term<'a>>)> {
        self.rc()
            .borrow()
            .constants
            .iter()
            .map(|(name, Value { value, .. })| (*name, self.desugar_term(value)))
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub(crate) fn lookup(
        &self,
        variables: &mut LocalVariables<'a>,
        name: &'a str,
    ) -> Result<typed::Term<'a>> {
        let this = self.rc();
        let this = this.borrow();

        let term = if let Some(local) = variables.lookup(name) {
            local
        } else if let Some(constant) = this.constants.get(name) {
            typed::Term::constant(name, self.desugar_term(&constant.typ)?)
        } else if let Some(data) = this.types.get(name) {
            self.desugar_constant(&data.type_constructor)?
        } else {
            return Err(InferenceError::VariableNotFound)?;
        };

        Ok(term)
    }

    fn desugar_term(&self, term: &RefCell<Term<'a>>) -> Result<typed::Term<'a>> {
        let mut term = term
            .try_borrow_mut()
            .map_err(|_| InferenceError::TopLevelCircularDependency)?;

        let typed_term = match &mut *term {
            Term::Typed(term) => term.clone(),
            Term::Ast(term) => term.desugar(self)?,
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
            Constant::Ast(constant) => constant.desugar(self)?,
        };

        *constant = Constant::Typed(typed_constant.clone());

        Ok(typed_constant)
    }

    fn rc(&self) -> Rc<RefCell<ContextData<'a>>> {
        self.0.upgrade().unwrap()
    }
}
