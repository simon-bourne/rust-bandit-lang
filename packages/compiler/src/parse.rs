use chumsky::{select, select_ref, IterParser, Parser};

use crate::lex::{BlockType, SpannedInput, Token, TokenTree};

#[derive(Clone, Debug)]
pub struct AST<'src> {
    pub items: Vec<Item<'src>>,
}

#[derive(Clone, Debug)]
pub enum Item<'src> {
    Function(Function<'src>),
}

#[derive(Clone, Debug)]
pub struct Function<'src> {
    pub name: &'src str,
    pub body: Vec<Line<'src>>,
}

#[derive(Clone, Debug)]
pub struct Line<'src>(&'src str);

pub fn parser<'src>() -> impl Parser<'src, SpannedInput<'src, TokenTree<'src>>, AST<'src>> {
    let ident = select! { TokenTree::Token(Token::Ident(name)) => name };
    let do_block = select_ref! { TokenTree::Block(BlockType::Do, block) => block.spanned() };

    let line = ident.map(Line);
    let body = line.repeated().collect();
    let function = ident
        .then(body.nested_in(do_block))
        .map(|(name, body)| Function { name, body });
    let item = function.map(Item::Function);

    item.repeated().collect().map(|items| AST { items })
}

#[cfg(test)]
mod tests {
    #[test]
    fn basic() {}
}
