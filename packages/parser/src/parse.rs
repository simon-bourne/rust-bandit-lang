use bandit_types::ast::Term;
pub use grammar::{definitions, term};
use winnow::{Result, error::ContextError, token::one_of};

use crate::lex::{Keyword, Operator, SrcToken, Token};

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
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> Result<Self> {
        one_of(|t: SrcToken| t.0 == *self)
            .value(*self)
            .parse_next(input)
    }
}

impl<'tok, 'src: 'tok> winnow::Parser<TokenList<'tok, 'src>, Operator, ContextError> for Operator {
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> Result<Operator> {
        Token::Operator(*self).value(*self).parse_next(input)
    }
}

impl<'tok, 'src: 'tok> winnow::Parser<TokenList<'tok, 'src>, Keyword, ContextError> for Keyword {
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> Result<Keyword> {
        Token::Keyword(*self).value(*self).parse_next(input)
    }
}
