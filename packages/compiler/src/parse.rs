use chumsky::{
    extra,
    prelude::{Cheap, Rich},
    primitive::just,
    select, select_ref, IterParser, Parser,
};

use crate::lex::{BlockType, Span, SpannedInput, Token, TokenTree};

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
    pub name: &'src str,
    pub body: Vec<Line<'src>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Line<'src>(&'src str);

pub type FastError<'src> = extra::Err<Cheap<Span>>;
pub type RichError<'src> = extra::Err<Rich<'src, TokenTree<'src>, Span>>;

pub fn parser<'src>(
) -> impl Parser<'src, SpannedInput<'src, TokenTree<'src>>, AST<'src>, RichError<'src>> {
    let ident = select! { TokenTree::Token(Token::Ident(name)) => name };
    let do_block = select_ref! { TokenTree::Block(BlockType::Do, block) => block.spanned() };
    let end_of_line = just(TokenTree::Token(Token::EndOfLine));

    let line = ident.map(Line).then_ignore(end_of_line.clone());
    let body = line.repeated().collect();
    let function = ident
        .then(body.nested_in(do_block))
        .map(|(name, body)| Function { name, body });
    let item = function.map(Item::Function);

    item.repeated().collect().map(|items| AST { items })
}

#[cfg(test)]
mod tests {
    use chumsky::{primitive::just, Parser};

    use crate::{
        lex::{RichError, Token, TokenTree},
        lexer,
        parse::{parser, Function, Item, Line, AST},
    };

    #[test]
    fn basic() {
        // TODO: indoc
        // TODO: This generates an end of line at the end, with a span of (0, 0).
        let lines = lexer::<RichError>().padded().parse(
            r#"
my_function do
    call1
    call2
"#,
        );
        assert_eq!(lines.errors().len(), 0);
        println!("{:#?}", lines.output());
        // TODO: parser parameterized on error
        // TODO: Factor out `end_of_line`
        let ast = parser()
            .padded_by(just(TokenTree::Token(Token::EndOfLine)).repeated())
            .parse(lines.output().unwrap().spanned());
        assert_eq!(
            ast.unwrap(),
            AST {
                items: vec![Item::Function(Function {
                    name: "my_function",
                    body: vec![Line("call1"), Line("call2")]
                })]
            }
        )
    }
}
