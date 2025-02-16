use std::{collections::HashMap, rc::Rc};

use crate::{inference, source, InferenceError, Result};

pub type GlobalValues<'a> = HashMap<&'a str, source::Expression<'a>>;

pub struct Context<'a> {
    local_variables: HashMap<&'a str, Vec<inference::Expression<'a>>>,
    global_variables: Rc<GlobalValues<'a>>,
}

impl<'a> Context<'a> {
    pub fn new(global_variables: GlobalValues<'a>) -> Self {
        Self {
            local_variables: HashMap::new(),
            global_variables: Rc::new(global_variables),
        }
    }

    pub(crate) fn with_variable<Output>(
        &mut self,
        name: &'a str,
        variable_value: inference::Expression<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Result<Output> {
        self.local_variables
            .entry(name)
            .or_default()
            .push(variable_value);
        let output = f(self);
        self.local_variables.entry(name).and_modify(|vars| {
            vars.pop();
        });
        Ok(output)
    }

    pub(crate) fn lookup(&mut self, name: &'a str) -> Result<inference::Expression<'a>> {
        Ok(if let Some(local) = self.lookup_local(name) {
            local
        } else {
            self.global_value(name)?
        }
        .fresh_variables())
    }

    fn lookup_local(&self, name: &'a str) -> Option<inference::Expression<'a>> {
        Some(self.local_variables.get(name)?.last()?.clone())
    }

    fn global_value(&mut self, name: &'a str) -> Result<inference::Expression<'a>> {
        // TODO: Cache each global var once it's linked
        let mut global_ctx = Context {
            local_variables: HashMap::new(),
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
