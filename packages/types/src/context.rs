use std::collections::HashMap;

use crate::{
    inference::InferenceExpression, source::NamesResolvedExpression, DeBruijnIndex, InferenceError,
    Result, Variable, VariableScope,
};

pub type GlobalValues<'a> = HashMap<&'a str, NamesResolvedExpression<'a>>;

pub struct Context<'a> {
    local_variables: Vec<InferenceExpression<'a>>,
    global_variables: GlobalValues<'a>,
}

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
        value: InferenceExpression<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Output {
        self.local_variables
            .push(InferenceExpression::variable(name, value.clone()));
        let output = f(self);
        self.local_variables.pop();
        output
    }

    pub(crate) fn lookup_value(&mut self, variable: Variable<'a>) -> Result<InferenceExpression<'a>> {
        match variable.scope {
            VariableScope::Local(index) => Ok(self.local_value(index)),
            VariableScope::Global => self.global_value(variable.name),
        }
    }

    fn local_value(&self, index: DeBruijnIndex) -> InferenceExpression<'a> {
        let len = self.local_variables.len();
        assert!(index.0 <= len);
        self.local_variables[len - index.0].clone()
    }

    fn global_value(&mut self, name: &'a str) -> Result<InferenceExpression<'a>> {
        let value = self
            .global_variables
            .get(name)
            .cloned()
            .ok_or(InferenceError)?
            .link(self)?;

        Ok(value)
    }
}
