use chumsky::{
    extra, input,
    prelude::{Cheap, Rich},
    primitive::{self, Just},
    select, Parser,
};

use self::ast::{Identifier, Operator};
use crate::lex::{Delimiter, Keyword, Span, Token};

pub mod ast;
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

    fn open(self, delimiter: Delimiter) -> impl TTParser<'src, Output> {
        self.then_ignore(open(delimiter))
    }

    fn close(self, delimiter: Delimiter) -> impl TTParser<'src, Output> {
        self.then_ignore(
            primitive::select(move |x, _| (x == Token::Close(delimiter)).then_some(()))
                .labelled(delimiter.close_str()),
        )
    }

    fn close_block(self) -> impl TTParser<'src, Output> {
        self.then_ignore(close_block()).skip_line_ends()
    }

    fn skip_line_ends(self) -> impl TTParser<'src, Output> {
        self.then_ignore(line_end().repeated())
    }

    fn skip_operator(self, required_name: &'static str) -> impl TTParser<'src, Output> {
        self.then_ignore(    select! {
            Token::Operator(name) = ext if name == required_name => Operator::new(name,ext.span())
        }
        .labelled(required_name)
        )
    }
}

fn open<'src>(delimiter: Delimiter) -> impl TTParser<'src, ()> {
    token(Token::Open(delimiter), delimiter.open_str())
}

fn line_separator<'src>() -> impl TTParser<'src, ()> {
    primitive::select(move |x, _| (x == Token::LineSeparator || x == Token::Comma).then_some(()))
        .labelled("<line separator>")
}

fn token<'src>(token: Token<'src>, label: &'static str) -> impl TTParser<'src, ()> {
    primitive::select(move |x, _| (x == token).then_some(())).labelled(label)
}

fn parenthesized<'src, T>(parser: impl TTParser<'src, T>) -> impl TTParser<'src, T> {
    open(Delimiter::Parentheses)
        .ignore_then(parser)
        .close(Delimiter::Parentheses)
}

impl<'src, Output, T> TTParser<'src, Output> for T where
    T: Parser<'src, SpannedInput<'src, Token<'src>>, Output, RichError<'src>> + Clone + 'src
{
}

macro_rules! lexeme {
    ($name:ident, $label:literal, $token:ident) => {
        pub fn $name<'src>() -> impl TTParser<'src, $token<'src>> + Copy {
            select! { Token::$token(name) = ext => $token::new(name, ext.span()) }.labelled($label)
        }
    };
}

lexeme!(ident, "<identifier>", Identifier);

pub fn operator(name: &str) -> impl TTParser<Operator> + Copy {
    select! {
        Token::Operator(op_name) = ext if name == op_name => Operator::new(name,ext.span())
    }
    .labelled("<operator>")
}

macro_rules! token {
    ($name:ident, $label:literal, $token:ident) => {
        fn $name<'src>() -> impl TTParser<'src, ()> + Copy {
            select! { Token::$token => () }.labelled($label)
        }
    };
}

token!(line_end, "\\n", LineSeparator);
token!(close_block, ";", CloseBlock);

fn keyword<'src>(kw: Keyword) -> impl TTParser<'src, ()> + Copy {
    primitive::select(move |x, _| (x == Token::Keyword(kw)).then_some(())).labelled(kw.as_str())
}
