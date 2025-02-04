use std::{cell::RefCell, rc::Rc};

use crate::{
    ExpressionReference, GenericExpression, SharedMut, VariableBinding, VariableReference,
};

mod pretty;

#[derive(Clone)]
pub struct Expression<'src>(SharedMut<GenericExpression<'src, Self>>);

impl<'src> Expression<'src> {
    pub fn reduce(&mut self) {
        let mut borrowed = self.0.borrow_mut();

        match &mut *borrowed {
            GenericExpression::TypeOfType => {}
            GenericExpression::Constant { typ, .. } => typ.reduce(),
            GenericExpression::Apply {
                function,
                argument,
                typ,
            } => {
                typ.reduce();
                function.reduce();
                argument.reduce();

                function.apply(argument);
            }
            GenericExpression::Let(variable_binding) => {
                variable_binding.reduce();
                let reduced = variable_binding.in_expression.clone();
                drop(borrowed);
                *self = reduced;
            }
            GenericExpression::Pi(variable_binding) => variable_binding.reduce(),
            GenericExpression::Lambda(variable_binding) => {
                variable_binding.variable_value.reduce();
                variable_binding.in_expression.reduce();
            }
            GenericExpression::Variable(variable) => {
                variable.value.reduce();
                let value = variable.value.clone();
                drop(borrowed);
                *self = value;
            }
        };
    }

    fn apply(&mut self, _argument: &Self) {
        let mut borrowed = self.0.borrow_mut();

        match &mut *borrowed {
            GenericExpression::TypeOfType => {}
            GenericExpression::Constant { .. } => todo!(),
            GenericExpression::Apply { .. } => {}
            GenericExpression::Let(_) => {}
            GenericExpression::Pi(_) => {}
            GenericExpression::Lambda(_variable_binding) => {
                todo!()
            }
            GenericExpression::Variable(_) => {}
        }
    }

    fn new(expression: GenericExpression<'src, Self>) -> Self {
        Self(Rc::new(RefCell::new(expression)))
    }
}

impl<'src> ExpressionReference<'src> for Expression<'src> {
    type Variable = VariableReference<'src, Self>;

    fn is_known(&self) -> bool {
        true
    }

    fn typ(&self) -> Self {
        self.0
            .borrow()
            .typ(Self::new, |variable| variable.value.typ())
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn reduce(&mut self) {
        self.variable_value.reduce();
        self.in_expression.reduce();
    }
}
