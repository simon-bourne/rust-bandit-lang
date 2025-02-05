use std::{collections::HashMap, rc::Rc};

use crate::{
    inference, type_annotated::indexed_locals, DeBruijnIndex, InferenceError, Result, Variable,
    VariableScope,
};

pub type GlobalValues<'a> = HashMap<&'a str, indexed_locals::Expression<'a>>;

#[derive(Clone)]
enum LocalValue<'a> {
    Now(inference::Expression<'a>),
    OnLookup {
        name: &'a str,
        value: indexed_locals::Expression<'a>,
    },
}

pub struct Context<'a> {
    local_variables: Vec<LocalValue<'a>>,
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
        name: &'a str,
        value: indexed_locals::Expression<'a>,
        f: impl FnOnce(&mut Self, inference::Expression<'a>) -> Output,
    ) -> Result<Output> {
        let variable_value = value.link(self)?;

        let lookup_value = if value.is_type_annotated() {
            LocalValue::OnLookup { name, value }
        } else {
            LocalValue::Now(inference::Expression::variable(
                name,
                variable_value.clone(),
            ))
        };

        self.local_variables.push(lookup_value);
        let output = f(self, variable_value);
        self.local_variables.pop();
        Ok(output)
    }

    pub(crate) fn lookup_value(
        &mut self,
        variable: Variable<'a>,
    ) -> Result<inference::Expression<'a>> {
        match variable.scope {
            VariableScope::Local(index) => self.local_value(index),
            VariableScope::Global => self.global_value(variable.name),
        }
    }

    fn local_value(&mut self, index: DeBruijnIndex) -> Result<inference::Expression<'a>> {
        let len = self.local_variables.len();
        assert!(index.0 <= len);
        let debruijn_index = len - index.0;

        Ok(match &self.local_variables[debruijn_index] {
            LocalValue::Now(value) => value.clone(),
            LocalValue::OnLookup { name, value } => {
                let name = *name;
                let value = value.clone();

                // This is what the context looked like when we added the variable binding.
                let mut local_ctx = Self {
                    local_variables: self.local_variables[..debruijn_index].to_vec(),
                    global_variables: self.global_variables.clone(),
                };

                inference::Expression::variable(name, value.link(&mut local_ctx)?)
            }
        })
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
