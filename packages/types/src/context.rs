use std::collections::HashMap;

use crate::{
    inference::Expression, type_annotated::indexed_locals, DeBruijnIndex, InferenceError, Result,
    Variable, VariableScope,
};

pub type GlobalValues<'a> = HashMap<&'a str, indexed_locals::Expression<'a>>;

pub struct Context<'a> {
    // TODO: Local variables with a fully known type annotation should be `link`ed at each lookup.
    local_variables: Vec<Expression<'a>>,
    global_variables: GlobalValues<'a>,
}

impl<'a> Context<'a> {
    pub fn new(global_variables: GlobalValues<'a>) -> Self {
        Self {
            local_variables: Vec::new(),
            global_variables,
        }
    }

    // TODO: We need to determine whether we're inferring the type, or checking
    // against it. We can only check against the type if it's fully known, but this
    // allows us to have a different type at each use site. For example, `id1` in
    // `let id1 : (∀a => a -> a) = id => (id1 Int an_int, id1 Float a_float)` can
    // be checked against, and each usage site can infer a different constraint for
    // `a`. `let id1 : (_ -> _) = id => (id1 Int an_int, id1 Float a_float)` must
    // have the unknowns inferred, to make sure this doesn't typecheck.
    pub(crate) fn with_variable<Output>(
        &mut self,
        name: &'a str,
        value: Expression<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Output {
        self.local_variables
            .push(Expression::variable(name, value.clone()));
        let output = f(self);
        self.local_variables.pop();
        output
    }

    pub(crate) fn lookup_value(&mut self, variable: Variable<'a>) -> Result<Expression<'a>> {
        match variable.scope {
            VariableScope::Local(index) => Ok(self.local_value(index)),
            VariableScope::Global => self.global_value(variable.name),
        }
    }

    fn local_value(&self, index: DeBruijnIndex) -> Expression<'a> {
        let len = self.local_variables.len();
        assert!(index.0 <= len);
        self.local_variables[len - index.0].clone()
    }

    fn global_value(&mut self, name: &'a str) -> Result<Expression<'a>> {
        let value = self
            .global_variables
            .get(name)
            .cloned()
            .ok_or(InferenceError)?
            // TODO: We need to be careful that the type of the global is fully known. Otherwise we
            // end up with a soundness hole where each use of the global can have a different type.
            // TODO: This should use global context only
            .link(self)?;

        Ok(value)
    }
}
