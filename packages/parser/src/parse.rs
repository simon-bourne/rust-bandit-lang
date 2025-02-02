use bandit_types::type_annotated::named_locals::Expression;
pub use grammar::expr;
use winnow::{error::ContextError, token::one_of, PResult};

use crate::lex::{Keyword, NamedOperator, SrcToken, Token};

mod grammar;

pub type TokenList<'tok, 'src> = &'tok [SrcToken<'src>];

pub trait Parser<'tok, 'src: 'tok, Out>:
    winnow::Parser<TokenList<'tok, 'src>, Out, ContextError>
{
}

impl<'tok, 'src: 'tok, Out, T> Parser<'tok, 'src, Out> for T where
    T: winnow::Parser<TokenList<'tok, 'src>, Out, ContextError>
{
}

impl<'tok, 'src: 'tok> winnow::Parser<TokenList<'tok, 'src>, Token<'src>, ContextError>
    for Token<'src>
{
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> PResult<Self> {
        one_of(|t: SrcToken| t.0 == *self)
            .value(*self)
            .parse_next(input)
    }
}

impl<'tok, 'src: 'tok> winnow::Parser<TokenList<'tok, 'src>, NamedOperator, ContextError>
    for NamedOperator
{
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> PResult<NamedOperator> {
        Token::Operator(*self).value(*self).parse_next(input)
    }
}

impl<'tok, 'src: 'tok> winnow::Parser<TokenList<'tok, 'src>, Keyword, ContextError> for Keyword {
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> PResult<Keyword> {
        Token::Keyword(*self).value(*self).parse_next(input)
    }
}
