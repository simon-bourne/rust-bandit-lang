use crate::{Inference, Expression};

#[derive(Copy, Clone)]
pub struct GlobalRef(usize);

#[derive(Copy, Clone)]
pub struct LocalRef(usize);

pub struct Context<'a> {
    local_variables: Vec<Expression<'a, Inference>>,
    global_variables: Vec<Expression<'a, Inference>>,
}
