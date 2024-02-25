use chumsky::{
    extra, input,
    prelude::{Cheap, Rich},
    primitive::{self, Just},
    select, IterParser, Parser,
};

use crate::lex::{Delimiter, Keyword, Span, Token};

pub type SpannedInput<'src, T> = input::SpannedInput<T, Span, &'src [(T, Span)]>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AST<'src> {
    pub items: Vec<Item<'src>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item<'src> {
    Function(Function<'src>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function<'src> {
    pub name: Identifier<'src>,
}

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
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $token<'src> {
            name: &'src str,
            span: Span,
        }

        fn $name<'src>() -> impl TTParser<'src, $token<'src>> + Copy {
            select! { Token::$token(name) = ext => $token{name, span: ext.span()} }.labelled($label)
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

pub fn parser<'src>() -> impl TTParser<'src, AST<'src>> {
    line_end().repeated().ignore_then(
        function()
            .map(Item::Function)
            .repeated()
            .collect()
            .map(|items| AST { items }),
    )
}

fn function<'src>() -> impl TTParser<'src, Function<'src>> {
    let name = ident()
        .open(Delimiter::Parentheses)
        .close(Delimiter::Parentheses)
        .skip_line_ends()
        .then_ignore(operator().filter(|op| op.name == "=").labelled("="))
        .skip_line_ends()
        .kw(Keyword::Do)
        .statement_end();
    name.map(|name| Function { name })
}

#[cfg(test)]
mod tests {
    use chumsky::{prelude::Input, Parser};

    use crate::{
        lex::{Span, Token},
        parse::{parser, Function, Identifier, Item, AST},
    };

    #[test]
    fn basic() {
        const SRC: &str = r#"
            my_function() = do
            ;
        "#;

        let tokens = Token::tokens(SRC).collect::<Vec<_>>();
        let ast = parser().parse(tokens.spanned(Span::new(SRC.len(), SRC.len())));

        // TODO: Pretty print the AST and use golden tests. Assert all the id's
        // are the same in their source to check the spans are correct.
        assert_eq!(
            ast.unwrap(),
            AST {
                items: vec![Item::Function(Function {
                    name: Identifier {
                        name: "my_function",
                        span: Span::new(13, 24)
                    },
                })]
            }
        )
    }
}
