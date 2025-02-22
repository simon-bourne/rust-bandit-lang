use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{InferenceError, Result, linked, source};

#[derive(Clone)]
enum Global<'a> {
    Typed(linked::Term<'a>),
    Source(source::Term<'a>),
}
pub type GlobalValues<'a> = HashMap<&'a str, source::Term<'a>>;

pub struct Context<'a> {
    local_variables: HashMap<&'a str, Vec<linked::Term<'a>>>,
    global_variables: Rc<HashMap<&'a str, RefCell<Global<'a>>>>,
}

impl<'a> Context<'a> {
    pub fn new(global_variables: impl IntoIterator<Item = (&'a str, source::Term<'a>)>) -> Self {
        Self {
            local_variables: HashMap::new(),
            global_variables: Rc::new(
                global_variables
                    .into_iter()
                    .map(|(name, value)| (name, RefCell::new(Global::Source(value))))
                    .collect(),
            ),
        }
    }

    pub(crate) fn with_variable<Output>(
        &mut self,
        name: &'a str,
        variable_value: linked::Term<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Result<Output> {
        self.local_variables
            .entry(name)
            .or_default()
            .push(variable_value);
        let output = f(self);

        if self.local_variables.get_mut(name).unwrap().pop().is_none() {
            self.local_variables.remove(name);
        }

        Ok(output)
    }

    pub(crate) fn lookup(&mut self, name: &'a str) -> Result<linked::Term<'a>> {
        Ok(if let Some(local) = self.lookup_local(name) {
            local
        } else {
            self.global_value(name)?
        }
        .fresh_variables())
    }

    fn lookup_local(&self, name: &'a str) -> Option<linked::Term<'a>> {
        Some(self.local_variables.get(name)?.last()?.clone())
    }

    fn global_value(&mut self, name: &'a str) -> Result<linked::Term<'a>> {
        let mut global_ctx = Context {
            local_variables: HashMap::new(),
            global_variables: self.global_variables.clone(),
        };
        let mut term = self
            .global_variables
            .get(name)
            .ok_or(InferenceError)?
            .try_borrow_mut()
            .map_err(|_| InferenceError)?;

        let typed_term = match &mut *term {
            Global::Typed(term) => term.clone(),
            Global::Source(term) => term.link(&mut global_ctx)?,
        };

        *term = Global::Typed(typed_term.clone());

        Ok(typed_term)
    }
}
