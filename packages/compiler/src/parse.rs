use chumsky::{select_ref, Parser};

use crate::lex::{SpannedInput, TokenTree};

#[derive(Clone, Debug)]
pub enum AST<'src> {
    // TODO: Remove
    Empty,
    // TODO: Remove
    Ident(&'src str),
}

pub fn parser<'src>() -> impl Parser<'src, SpannedInput<'src, TokenTree<'src>>, AST<'src>> {
    select_ref! { TokenTree::Delimited(_delimiter, line) => line.spanned() }.to(AST::Empty)
}
