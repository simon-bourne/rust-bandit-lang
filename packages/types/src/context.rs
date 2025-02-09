use std::{collections::HashMap, rc::Rc};

use crate::{
    inference, type_annotated::indexed_locals, DeBruijnIndex, InferenceError, Result, Variable,
    VariableScope,
};

pub type GlobalValues<'a> = HashMap<&'a str, indexed_locals::Expression<'a>>;

pub struct Context<'a> {
    local_variables: Vec<inference::Expression<'a>>,
    global_variables: Rc<GlobalValues<'a>>,
}

impl<'a> Context<'a> {
    pub fn new(global_variables: GlobalValues<'a>) -> Self {
        Self {
            local_variables: Vec::new(),
            global_variables: Rc::new(global_variables),
        }
    }

    pub(crate) fn with_variable<Output>(
        &mut self,
        variable_value: inference::Expression<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Result<Output> {
        self.local_variables.push(variable_value);
        let output = f(self);
        self.local_variables.pop();
        Ok(output)
    }

    pub(crate) fn lookup_value(
        &mut self,
        variable: Variable<'a>,
    ) -> Result<inference::Expression<'a>> {
        match variable.scope {
            VariableScope::Local(index) => Ok(self.local_value(index)),
            VariableScope::Global => self.global_value(variable.name),
        }
    }

    fn local_value(&mut self, index: DeBruijnIndex) -> inference::Expression<'a> {
        let len = self.local_variables.len();
        assert!(index.0 <= len);
        let debruijn_index = len - index.0;

        self.local_variables[debruijn_index].clone()
    }

    fn global_value(&mut self, name: &'a str) -> Result<inference::Expression<'a>> {
        let mut global_ctx = Context {
            local_variables: Vec::new(),
            global_variables: self.global_variables.clone(),
        };
        let value = self
            .global_variables
            .get(name)
            .cloned()
            .ok_or(InferenceError)?
            .link(&mut global_ctx)?;

        Ok(value)
    }
}
