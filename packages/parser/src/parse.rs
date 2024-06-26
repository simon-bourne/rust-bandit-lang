use chumsky::{
    extra, input,
    prelude::{Cheap, Rich},
    primitive::{self, Just},
    select, Parser,
};

use crate::{
    ast::{Identifier, Operator},
    lex::{Grouping, Keyword, NamedOperator, Span, Token},
};

pub mod grammar;

pub type SpannedInput<'src, T> = input::SpannedInput<T, Span, &'src [(T, Span)]>;

pub type FastError<'src> = extra::Err<Cheap<Span>>;
pub type RichError<'src> = extra::Err<Rich<'src, Token<'src>, Span>>;

pub type JustToken<'src> = Just<Token<'src>, SpannedInput<'src, Token<'src>>, RichError<'src>>;

pub trait TTParser<'src, Output>:
    Parser<'src, SpannedInput<'src, Token<'src>>, Output, RichError<'src>> + Clone + Sized + 'src
{
    fn keyword(self, kw: Keyword) -> impl TTParser<'src, Output> {
        self.then_ignore(keyword(kw))
    }

    fn operator(self, name: NamedOperator) -> impl TTParser<'src, Output> {
        self.then_ignore(operator(name))
    }
}

impl<'src, Output, T> TTParser<'src, Output> for T where
    T: Parser<'src, SpannedInput<'src, Token<'src>>, Output, RichError<'src>> + Clone + 'src
{
}

fn token<'src>(token: Token<'src>, label: &'static str) -> impl TTParser<'src, ()> {
    primitive::select(move |x, _| (x == token).then_some(())).labelled(label)
}

fn parenthesized<'src, T>(parser: impl TTParser<'src, T>) -> impl TTParser<'src, T> {
    grouped(parser, Grouping::Parentheses)
}

fn in_block<'src, T>(parser: impl TTParser<'src, T>) -> impl TTParser<'src, T> {
    grouped(parser, Grouping::Block)
}

fn grouped<'src, T>(parser: impl TTParser<'src, T>, grouping: Grouping) -> impl TTParser<'src, T> {
    let open = token(Token::Open(grouping), grouping.open_str());
    let close = token(Token::Close(grouping), grouping.close_str());

    open.ignore_then(parser).then_ignore(close)
}

fn identifier<'src>() -> impl TTParser<'src, Identifier<'src>> + Copy {
    select! {
        Token::Identifier(name) = ext => Identifier::new(name,ext.span())
    }
    .labelled("<identifier>")
}

fn operator<'src>(name: NamedOperator) -> impl TTParser<'src, Operator> + Copy {
    select! {
        Token::Operator(op_name) = ext if name == op_name => Operator::Named{name,span: ext.span()}
    }
    .labelled(name.as_str())
}

macro_rules! token {
    ($name:ident, $label:literal, $token:pat_param) => {
        fn $name<'src>() -> impl TTParser<'src, ()> + Copy {
            select! { $token => () }.labelled($label)
        }
    };
}

token!(line_end, "\\n", Token::LineEnd);
token!(comma, ",", Token::Comma);

fn keyword<'src>(kw: Keyword) -> impl TTParser<'src, ()> + Copy {
    primitive::select(move |x, _| (x == Token::Keyword(kw)).then_some(())).labelled(kw.as_str())
}

fn optional_line_end<'src, T>(parser: impl TTParser<'src, T>) -> impl TTParser<'src, T> {
    line_end().ignore_then(parser.clone()).or(parser)
}
