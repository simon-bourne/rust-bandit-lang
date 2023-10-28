use chumsky::{
    extra,
    prelude::{Cheap, Rich},
    primitive::just,
    select, select_ref, IterParser, Parser,
};

use crate::lex::{end_of_line, BlockType, Span, SpannedInput, Token, TokenTree};

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
    pub name: Ident<'src>,
    pub body: Vec<Line<'src>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Line<'src>(Ident<'src>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident<'src> {
    name: &'src str,
    span: Span,
}

pub type FastError<'src> = extra::Err<Cheap<Span>>;
pub type RichError<'src> = extra::Err<Rich<'src, TokenTree<'src>, Span>>;

pub trait TTParser<'src, Output>:
    Parser<'src, SpannedInput<'src, TokenTree<'src>>, Output, RichError<'src>>
{
}

impl<'src, Output, T> TTParser<'src, Output> for T where
    T: Parser<'src, SpannedInput<'src, TokenTree<'src>>, Output, RichError<'src>>
{
}

fn ident<'src>() -> impl TTParser<'src, Ident<'src>> + Copy + Clone {
    select! { TokenTree::Token(Token::Ident(name)) = ext => Ident{ name, span: ext.span()} }
}

fn block<'src>(typ: BlockType) -> impl TTParser<'src, SpannedInput<'src, TokenTree<'src>>> {
    select_ref! {
        TokenTree::Block(block_type, block) if typ == *block_type => block.spanned()
    }
}

pub fn parser<'src>() -> impl TTParser<'src, AST<'src>> {
    let ident = ident();
    let line = ident.map(Line).then_ignore(just(end_of_line()));
    let body = line.repeated().collect();
    let function = ident
        .then(body.nested_in(block(BlockType::Do)))
        .map(|(name, body)| Function { name, body });
    let item = function.map(Item::Function);

    item.repeated().collect().map(|items| AST { items })
}

#[cfg(test)]
mod tests {
    use chumsky::{primitive::just, Parser};

    use crate::{
        lex::{self, end_of_line, Span},
        lexer,
        parse::{parser, Function, Ident, Item, Line, AST},
    };

    #[test]
    fn basic() {
        // TODO: indoc
        // TODO: This generates an end of line at the end, with a span of (0, 0).
        let lines = lexer::<lex::RichError>().padded().parse(
            r#"
my_function do
    call1
    call2
"#,
        );
        assert_eq!(lines.errors().len(), 0);
        println!("{:#?}", lines.output());
        let ast = parser()
            .padded_by(just(end_of_line()).repeated())
            .parse(lines.output().unwrap().spanned());
        assert_eq!(
            ast.unwrap(),
            AST {
                items: vec![Item::Function(Function {
                    name: Ident {
                        name: "my_function",
                        span: Span::new(1, 12)
                    },
                    body: vec![
                        Line(Ident {
                            name: "call1",
                            span: Span::new(20, 25)
                        }),
                        Line(Ident {
                            name: "call2",
                            span: Span::new(30, 35)
                        })
                    ]
                })]
            }
        )
    }
}
