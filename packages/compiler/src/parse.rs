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
    Parser<'src, SpannedInput<'src, Token<'src>>, Output, RichError<'src>> + Sized + 'src
{
    fn kw(self, keyword: Keyword) -> impl TTParser<'src, Output> {
        self.then_ignore(
            primitive::select(move |x, _| (x == Token::Keyword(keyword)).then_some(()))
                .skip_line_ends()
                .labelled(keyword.as_str()),
        )
    }

    fn open(self, delimiter: Delimiter) -> impl TTParser<'src, Output> {
        self.then_ignore(
            primitive::select(move |x, _| (x == Token::Open(delimiter)).then_some(()))
                .skip_line_ends()
                .labelled(delimiter.open_str()),
        )
    }

    fn close(self, delimiter: Delimiter) -> impl TTParser<'src, Output> {
        self.then_ignore(
            primitive::select(move |x, _| (x == Token::Close(delimiter)).then_some(()))
                .labelled(delimiter.close_str()),
        )
    }

    fn statement_end(self) -> impl TTParser<'src, Output> {
        self.then_ignore(statement_end()).skip_line_ends()
    }

    fn skip_line_ends(self) -> impl TTParser<'src, Output> {
        self.then_ignore(line_end().repeated())
    }
}

impl<'src, Output, T> TTParser<'src, Output> for T
where
    T: Parser<'src, SpannedInput<'src, Token<'src>>, Output, RichError<'src>>,
    T: 'src,
{
}

macro_rules! lexeme {
    ($name:ident, $label:literal, $token:ident) => {
        fn $name<'src>() -> impl TTParser<'src, $token<'src>> + Copy {
            select! { Token::$token(name) = ext => $token::new(name, ext.span()) }.labelled($label)
        }
    };
}

lexeme!(ident, "<identifier>", Identifier);
lexeme!(operator, "<operator>", Operator);

macro_rules! token {
    ($name:ident, $label:literal, $token:ident) => {
        fn $name<'src>() -> impl TTParser<'src, ()> + Copy {
            select! { Token::$token => () }.labelled($label)
        }
    };
}

token!(line_end, "\\n", LineEnd);
token!(statement_end, ";", StatementEnd);
