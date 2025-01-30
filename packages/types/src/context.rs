use std::{collections::HashMap, fmt};

use crate::{source::NamesResolvedExpression, ExpressionRef, InferenceError, Result};

pub struct DeBruijnLevel(usize);

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct DeBruijnIndex(usize);

impl fmt::Display for DeBruijnIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub type GlobalValues<'a> = HashMap<&'a str, NamesResolvedExpression<'a>>;

pub struct Context<'a> {
    local_variables: Vec<ExpressionRef<'a>>,
    global_variables: GlobalValues<'a>,
}

impl<'a> Context<'a> {
    pub fn new(global_variables: GlobalValues<'a>) -> Self {
        Self {
            local_variables: Vec::new(),
            global_variables,
        }
    }

    pub fn with_variable<Output>(
        &mut self,
        name: &'a str,
        value: ExpressionRef<'a>,
        f: impl FnOnce(&mut Self) -> Output,
    ) -> Output {
        self.local_variables
            .push(ExpressionRef::variable(name, value.clone()));
        let output = f(self);
        self.local_variables.pop();
        output
    }

    pub fn lookup_value(&mut self, variable: Variable<'a>) -> Result<ExpressionRef<'a>> {
        match variable.scope {
            VariableScope::Local(index) => Ok(self.local_value(index)),
            VariableScope::Global => self.global_value(variable.name),
        }
    }

    fn local_value(&self, index: DeBruijnIndex) -> ExpressionRef<'a> {
        let len = self.local_variables.len();
        assert!(index.0 <= len);
        self.local_variables[len - index.0].clone()
    }

    fn global_value(&mut self, name: &'a str) -> Result<ExpressionRef<'a>> {
        let value = self
            .global_variables
            .get(name)
            .cloned()
            .ok_or(InferenceError)?
            .link(self)?;

        Ok(ExpressionRef::variable(name, value))
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

    pub fn lookup(&self, name: &'a str) -> Variable<'a> {
        let scope = if let Some(local_index) = self.lookup_local(name) {
            VariableScope::Local(local_index)
        } else {
            VariableScope::Global
        };

        Variable { name, scope }
    }

    fn lookup_local(&self, name: &str) -> Option<DeBruijnIndex> {
        let level = self.variables.get(name)?.last()?;
        assert!(level.0 < self.variable_count);
        Some(DeBruijnIndex(self.variable_count - level.0))
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Variable<'src> {
    name: &'src str,
    scope: VariableScope,
}

impl Variable<'_> {
    pub fn name(&self) -> &str {
        self.name
    }
}

impl fmt::Display for Variable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum VariableScope {
    Local(DeBruijnIndex),
    Global,
}
