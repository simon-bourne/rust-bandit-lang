use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{InferenceError, Result, de_bruijn::Level, linked, source};

#[derive(Clone)]
enum Global<'a> {
    Linked(linked::Term<'a>),
    Source(source::Term<'a>),
}
pub type GlobalValues<'a> = HashMap<&'a str, source::Term<'a>>;

pub struct Context<'a> {
    scope: Level,
    local_variables: HashMap<&'a str, Vec<linked::Term<'a>>>,
    global_variables: Rc<HashMap<&'a str, RefCell<Global<'a>>>>,
}

impl<'a> Context<'a> {
    pub fn new(global_variables: impl IntoIterator<Item = (&'a str, source::Term<'a>)>) -> Self {
        Self {
            scope: Level::top(),
            local_variables: HashMap::new(),
            global_variables: Rc::new(
                global_variables
                    .into_iter()
                    .map(|(name, value)| (name, RefCell::new(Global::Source(value))))
                    .collect(),
            ),
        }
    }

    pub(crate) fn in_scope<Output>(
        &mut self,
        name: Option<&'a str>,
        variable_value: linked::Term<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Output {
        let current_scope = self.scope;
        self.scope = self.scope.open();

        let output = if let Some(name) = name {
            self.local_variables
                .entry(name)
                .or_default()
                .push(variable_value);
            let output = f(self);

            if self.local_variables.get_mut(name).unwrap().pop().is_none() {
                self.local_variables.remove(name);
            }

            output
        } else {
            f(self)
        };

        self.scope = current_scope;
        output
    }

    pub(crate) fn lookup(&mut self, name: &'a str) -> Result<linked::Term<'a>> {
        Ok(if let Some(local) = self.lookup_local(name) {
            local
        } else {
            self.global_value(name)?
        })
    }

    fn lookup_local(&self, name: &'a str) -> Option<linked::Term<'a>> {
        Some(self.local_variables.get(name)?.last()?.clone())
    }

    fn global_value(&mut self, name: &'a str) -> Result<linked::Term<'a>> {
        let mut global_ctx = Context {
            scope: Level::top(),
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
            Global::Linked(term) => term.clone(),
            Global::Source(term) => term.link(&mut global_ctx)?,
        };

        *term = Global::Linked(typed_term.clone());

        Ok(typed_term)
    }
}
