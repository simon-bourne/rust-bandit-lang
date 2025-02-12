use std::{cell::RefCell, rc::Rc};

use crate::{Binder, ExpressionReference, GenericExpression, SharedMut, VariableBinding};

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
            GenericExpression::VariableBinding(variable_binding) => match variable_binding.binder {
                Binder::Let => {
                    variable_binding.reduce();
                    let reduced = variable_binding.in_expression.clone();
                    drop(borrowed);
                    *self = reduced;
                }
                Binder::Pi => variable_binding.reduce(),
                Binder::Lambda => {
                    variable_binding.variable_value.reduce();
                    variable_binding.in_expression.reduce();
                }
            },
        };
    }

    fn apply(&mut self, _argument: &Self) {
        let mut borrowed = self.0.borrow_mut();

        match &mut *borrowed {
            GenericExpression::TypeOfType => {}
            GenericExpression::Constant { .. } => todo!(),
            GenericExpression::Apply { .. } => {}
            GenericExpression::VariableBinding(_) => {
                todo!()
            }
        }
    }

    fn new(expression: GenericExpression<'src, Self>) -> Self {
        Self(Rc::new(RefCell::new(expression)))
    }
}

impl<'src> ExpressionReference<'src> for Expression<'src> {
    fn is_known(&self) -> bool {
        true
    }

    fn typ(&self) -> Self {
        self.0.borrow().typ(Self::new)
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn reduce(&mut self) {
        self.variable_value.reduce();
        self.in_expression.reduce();
    }
}
