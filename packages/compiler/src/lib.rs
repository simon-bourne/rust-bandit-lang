pub mod lex;
pub mod parse;

use chumsky::{
    extra,
    prelude::{Input, Rich},
};
pub use lex::lexer;

pub trait RichParser<'src, I, Output>:
    chumsky::Parser<'src, I, Output, extra::Err<Rich<'src, I::Token, I::Span>>>
where
    I: Input<'src>,
    I::Token: Eq + PartialEq,
{
}

impl<
        'src,
        I,
        Output,
        T: chumsky::Parser<'src, I, Output, extra::Err<Rich<'src, I::Token, I::Span>>>,
    > RichParser<'src, I, Output> for T
where
    I: Input<'src>,
    I::Token: Eq + PartialEq,
{
}
