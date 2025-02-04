use std::collections::HashMap;

use crate::{
    inference::Expression, type_annotated::indexed_locals, DeBruijnIndex, InferenceError, Result,
    Variable, VariableScope,
};

pub type GlobalValues<'a> = HashMap<&'a str, indexed_locals::Expression<'a>>;

pub struct Context<'a> {
    local_variables: Vec<Expression<'a>>,
    global_variables: GlobalValues<'a>,
}

// TODO: At each variable use site, we want fresh variables, but not fresh
// unknowns:
//
// We should eagerly `link` each variable (local and global) so unknowns
// are linked between all references to a variable. We should have fresh child
// variables for each variable reference. This allows:
//
// `let id1 : (∀a => a -> a) = id => (id1 Int an_int, id1 Float a_float)`
//
// But not:
//
// `let id1 : (_ -> _ -> _) = id => (id1 Int an_int, id1 Float a_float)`
impl<'a> Context<'a> {
    pub fn new(global_variables: GlobalValues<'a>) -> Self {
        Self {
            local_variables: Vec::new(),
            global_variables,
        }
    }

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
            .link(self)?;

        Ok(value)
    }
}
