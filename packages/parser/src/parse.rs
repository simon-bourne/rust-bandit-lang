use std::{error::Error, fmt, ops::Range};

use annotate_snippets::{AnnotationKind, Level, Renderer, Snippet};
use bandit_term::ast::{Definition, Term};
use winnow::{
    Parser as _, Result,
    error::{ContextError, ParseError},
    token::one_of,
};

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

#[derive(Debug)]
pub struct PrettyError<'src> {
    message: String,
    span: Range<usize>,
    input: &'src str,
}

impl<'src> PrettyError<'src> {
    fn new<'tok>(input: &'src str, error: ParseError<TokenList<'tok, 'src>, ContextError>) -> Self {
        let message = error.inner().to_string();
        let tokens = error.input();

        let span = if tokens.is_empty() {
            0..usize::MAX
        } else {
            let start = tokens.first().unwrap().1.start;
            let end = tokens.last().unwrap().1.end;
            start..end
        };

        Self {
            message,
            span,
            input,
        }
    }
}

impl<'src> fmt::Display for PrettyError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = Level::ERROR.primary_title(&self.message).element(
            Snippet::source(self.input)
                .fold(true)
                .annotation(AnnotationKind::Primary.span(self.span.clone())),
        );
        let renderer = Renderer::plain();
        let rendered = renderer.render(&[message]);
        rendered.fmt(f)
    }
}

impl<'src> Error for PrettyError<'src> {}

pub fn term<'src>(input: &'src str) -> Result<Term<'src>, PrettyError<'src>> {
    let tokens = Token::layout(input);
    grammar::term
        .parse(&tokens)
        .map_err(|e| PrettyError::new(input, e))
}

pub fn definitions<'src>(input: &'src str) -> Result<Vec<Definition<'src>>, PrettyError<'src>> {
    let tokens = Token::layout(input);
    grammar::definitions()
        .parse(&tokens)
        .map_err(|e| PrettyError::new(input, e))
}
