use chumsky::{
    combinator::DelimitedBy,
    extra, input,
    prelude::{Cheap, Rich},
    primitive::{just, Just},
    select, IterParser, Parser,
};

use crate::lex::{Delimiter, Span, Token};

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
    pub name: Ident,
    pub body: Vec<Line>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Line(Ident);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident(Span);

pub type FastError<'src> = extra::Err<Cheap<Span>>;
pub type RichError<'src> = extra::Err<Rich<'src, Token, Span>>;

pub type JustToken<'src> = Just<Token, SpannedInput<'src, Token>, RichError<'src>>;

pub trait TTParser<'src, Output>:
    Parser<'src, SpannedInput<'src, Token>, Output, RichError<'src>>
{
    fn delimited(
        self,
        delimiter: Delimiter,
    ) -> DelimitedBy<Self, JustToken<'src>, JustToken<'src>, Token, Token>
    where
        Self: Sized,
    {
        self.delimited_by(just(Token::Open(delimiter)), just(Token::Close(delimiter)))
    }
}

impl<'src, Output, T> TTParser<'src, Output> for T where
    T: Parser<'src, SpannedInput<'src, Token>, Output, RichError<'src>>
{
}

fn ident<'src>() -> impl TTParser<'src, Ident> + Copy {
    select! { Token::Identifier = ext => Ident(ext.span()) }
}

pub fn parser<'src>() -> impl TTParser<'src, AST> {
    let ident = ident();
    let line = ident.map(Line).then_ignore(just(Token::LineEnd));
    let body = line.repeated().collect();
    let function = ident
        .then(body.delimited(Delimiter::Braces))
        .map(|(name, body)| Function { name, body });
    let item = function.map(Item::Function);

    item.repeated().collect().map(|items| AST { items })
}

#[cfg(test)]
mod tests {
    use chumsky::{prelude::Input, Parser};

    use crate::{
        lex::{Span, Token},
        parse::{parser, Function, Ident, Item, Line, AST},
    };

    #[test]
    fn basic() {
        // TODO: indoc
        const SRC: &str = r#"
my_function {
    call1;
    call2;
}
"#;

        let tokens = Token::tokens(SRC).collect::<Vec<_>>();
        let ast = parser().parse(tokens.spanned(Span::new(SRC.len(), SRC.len())));

        // TODO: Pretty print the AST and use golden tests. Assert all the id's
        // are the same in their source to check the spans are correct.
        assert_eq!(
            ast.unwrap(),
            AST {
                items: vec![Item::Function(Function {
                    name: Ident(Span::new(1, 12)),
                    body: vec![
                        Line(Ident(Span::new(19, 24))),
                        Line(Ident(Span::new(30, 35)))
                    ]
                })]
            }
        )
    }
}
