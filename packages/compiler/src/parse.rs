use chumsky::{
    extra, input,
    prelude::{Cheap, Rich},
    primitive::just,
    select, IterParser, Parser,
};

use crate::logos_lex::{BlockType, Delimiter, Span, Token};

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

pub trait TTParser<'src, Output>:
    Parser<'src, SpannedInput<'src, Token>, Output, RichError<'src>>
{
}

impl<'src, Output, T> TTParser<'src, Output> for T where
    T: Parser<'src, SpannedInput<'src, Token>, Output, RichError<'src>>
{
}

fn ident<'src>() -> impl TTParser<'src, Ident> + Copy + Clone {
    select! { Token::Identifier = ext => Ident(ext.span()) }
}

fn block<'src>(typ: BlockType) -> impl TTParser<'src, ()> {
    select! { Token::Block(block_type) if typ == block_type => () }
}

pub fn parser<'src>() -> impl TTParser<'src, AST> {
    let ident = ident();
    let line = ident.map(Line);
    let body = line.separated_by(just(Token::LineStart)).collect();
    let function = ident
        .then_ignore(block(BlockType::Do))
        .then(body.delimited_by(
            just(Token::Open(Delimiter::Indent)),
            just(Token::Close(Delimiter::Indent)),
        ))
        .map(|(name, body)| Function { name, body });
    let item = function.map(Item::Function);

    item.repeated().collect().map(|items| AST { items })
}

#[cfg(test)]
mod tests {
    use chumsky::{prelude::Input, primitive::just, Parser};

    use crate::{
        logos_lex::{Span, Token},
        parse::{parser, Function, Ident, Item, Line, AST},
    };

    #[test]
    fn basic() {
        // TODO: indoc
        // TODO: This generates an end of line at the end, with a span of (0, 0).
        const SRC: &str = r#"
my_function do
    call1
    call2
"#;

        let tokens = Token::tokens(SRC).collect::<Vec<_>>();

        // TODO: What to do about the spurious `LineStart`?
        let ast = just(Token::LineStart)
            .ignore_then(parser())
            .parse(tokens.spanned(Span::new(SRC.len(), SRC.len())));

        // TODO: Pretty print the AST and use golden tests. Assert all the id's
        // are the same in their source to check the spans are correct.
        assert_eq!(
            ast.unwrap(),
            AST {
                items: vec![Item::Function(Function {
                    name: Ident(Span::new(1, 12)),
                    body: vec![
                        Line(Ident(Span::new(20, 25))),
                        Line(Ident(Span::new(30, 35)))
                    ]
                })]
            }
        )
    }
}
