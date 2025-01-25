use std::{collections::HashMap, fmt};

use crate::{ExpressionRef, InferenceError, Result};

pub struct DeBruijnLevel(usize);

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct DeBruijnIndex(usize);

impl fmt::Display for DeBruijnIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Default)]
pub struct Context<'a> {
    local_variables: Vec<ExpressionRef<'a>>,
}

impl<'a> Context<'a> {
    pub fn with_variable<Output>(
        &mut self,
        typ: ExpressionRef<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Output {
        self.local_variables.push(typ.clone());
        let output = f(self);
        self.local_variables.pop();
        output
    }

    pub fn local_type(&self, index: DeBruijnIndex) -> ExpressionRef<'a> {
        let len = self.local_variables.len();
        assert!(index.0 <= len);
        self.local_variables[len - index.0].clone()
    }

    pub fn global_type(&self, name: &str) -> Result<ExpressionRef<'a>> {
        match name {
            "Type" => Ok(ExpressionRef::type_of_type()),
            _ => Err(InferenceError),
        }
    }
}

#[derive(Default)]
pub struct VariableLookup<'a> {
    variables: HashMap<&'a str, Vec<DeBruijnLevel>>,
    variable_count: usize,
}

impl<'a> VariableLookup<'a> {
    pub fn with_variable<Output>(
        &mut self,
        name: &'a str,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Output {
        let level = DeBruijnLevel(self.variable_count);
        self.variable_count += 1;
        self.variables.entry(name).or_default().push(level);
        let output = f(self);
        self.variables.entry(name).and_modify(|levels| {
            levels.pop();
        });
        self.variable_count -= 1;
        output
    }

    pub fn lookup(&self, name: &str) -> Option<DeBruijnIndex> {
        let level = self.variables.get(name)?.last()?;
        assert!(level.0 < self.variable_count);
        Some(DeBruijnIndex(self.variable_count - level.0))
    }
}
