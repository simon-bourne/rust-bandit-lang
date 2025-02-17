use crate::{Binder, GenericTerm, SharedMut, TermReference, VariableBinding};

mod pretty;

#[derive(Clone)]
pub struct Term<'src>(SharedMut<GenericTerm<'src, Self>>);

impl<'src> Term<'src> {
    pub fn reduce(&mut self) {
        let mut borrowed = self.0.borrow_mut();

        match &mut *borrowed {
            GenericTerm::TypeOfType => {}
            GenericTerm::Constant { typ, .. } => typ.reduce(),
            GenericTerm::Apply {
                function,
                argument,
                typ,
            } => {
                typ.reduce();
                function.reduce();
                argument.reduce();

                function.apply(argument);
            }
            GenericTerm::VariableBinding(variable_binding) => match variable_binding.binder {
                Binder::Let => {
                    variable_binding.reduce();
                    let reduced = variable_binding.in_term.clone();
                    drop(borrowed);
                    *self = reduced;
                }
                Binder::Pi => variable_binding.reduce(),
                Binder::Lambda => {
                    variable_binding.variable_value.reduce();
                    variable_binding.in_term.reduce();
                }
            },
        };
    }

    fn apply(&mut self, _argument: &Self) {
        let mut borrowed = self.0.borrow_mut();

        match &mut *borrowed {
            GenericTerm::TypeOfType => {}
            GenericTerm::Constant { .. } => todo!(),
            GenericTerm::Apply { .. } => {}
            GenericTerm::VariableBinding(_) => {
                todo!()
            }
        }
    }

    fn new(term: GenericTerm<'src, Self>) -> Self {
        Self(SharedMut::new(term))
    }
}

impl<'src> TermReference<'src> for Term<'src> {
    fn is_known(&self) -> bool {
        true
    }

    fn typ(&self) -> Self {
        self.0.borrow().typ(Self::new)
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn reduce(&mut self) {
        self.variable_value.reduce();
        self.in_term.reduce();
    }
}
