use chumsky::{
    extra, input,
    prelude::{Cheap, Rich},
    primitive::{self, Just},
    select, Boxed, IterParser, Parser,
};

use crate::lex::{Delimiter, Keyword, Span, Token};

pub type SpannedInput<'src, T> = input::SpannedInput<T, Span, &'src [(T, Span)]>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AST {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item {
    Function(Function),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub name: Identifier,
}

pub type FastError<'src> = extra::Err<Cheap<Span>>;
pub type RichError<'src> = extra::Err<Rich<'src, Token, Span>>;

pub type JustToken<'src> = Just<Token, SpannedInput<'src, Token>, RichError<'src>>;

pub trait TTParser<'src, Output>:
    Parser<'src, SpannedInput<'src, Token>, Output, RichError<'src>>
{
    fn skip_line_ends(self) -> Boxed<'src, 'src, SpannedInput<'src, Token>, Output, RichError<'src>>
    where
        Self: Sized + 'src,
    {
        self.then_ignore(line_end().repeated()).boxed()
    }
}

impl<'src, Output, T> TTParser<'src, Output> for T where
    T: Parser<'src, SpannedInput<'src, Token>, Output, RichError<'src>>
{
}

macro_rules! lexeme {
    ($name:ident, $token:ident) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $token(Span);

        fn $name<'src>() -> impl TTParser<'src, $token> + Copy {
            select! { Token::$token = ext => $token(ext.span()) }.labelled(stringify!($name))
        }
    };
}

lexeme!(ident, Identifier);
lexeme!(operator, Operator);

macro_rules! token {
    ($name:ident, $token:ident) => {
        fn $name<'src>() -> impl TTParser<'src, ()> + Copy {
            select! { Token::$token => () }.labelled(stringify!($name))
        }
    };
}

token!(line_end, LineEnd);
token!(statement_end, StatementEnd);

// TODO: labelled
fn kw<'src>(keyword: Keyword) -> impl TTParser<'src, ()> {
    primitive::select(move |x, _| (x == Token::Keyword(keyword)).then_some(())).skip_line_ends()
}

// TODO: labelled
fn open<'src>(delimiter: Delimiter) -> impl TTParser<'src, ()> {
    primitive::select(move |x, _| (x == Token::Open(delimiter)).then_some(())).skip_line_ends()
}

// TODO: labelled
fn close<'src>(delimiter: Delimiter) -> impl TTParser<'src, ()> {
    primitive::select(move |x, _| (x == Token::Close(delimiter)).then_some(()))
}

pub fn parser<'src>() -> impl TTParser<'src, AST> {
    line_end().repeated().ignore_then(
        function()
            .map(Item::Function)
            .repeated()
            .collect()
            .map(|items| AST { items }),
    )
}

fn function<'src>() -> impl TTParser<'src, Function> {
    let name = ident()
        .skip_line_ends()
        .then_ignore(open(Delimiter::Parentheses))
        .then_ignore(close(Delimiter::Parentheses))
        .skip_line_ends()
        .then_ignore(operator())
        .skip_line_ends()
        .then_ignore(kw(Keyword::Do))
        .then_ignore(statement_end().skip_line_ends());
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
        // TODO: indoc
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
                    name: Identifier(Span::new(1, 12)),
                })]
            }
        )
    }
}
