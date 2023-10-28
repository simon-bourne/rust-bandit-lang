use chumsky::{select_ref, Parser};

use crate::lex::{SpannedInput, TokenTree};

#[derive(Clone, Debug)]
pub enum AST<'src> {
    // TODO: Remove
    Empty,
    // TODO: Remove
    Ident(&'src str),
}

// TODO: Use `RichParser``
// TODO: Flatten token tree from lexer. `nested_in` doesn't look like it'll deal
// with multiple layers of nesting, and the `Eq` constraint on tokens (from
// `Error` from `error::Rich`) isn't a good idea for tree tokens.
pub fn parser<'src>() -> impl Parser<'src, SpannedInput<'src, TokenTree<'src>>, AST<'src>> {
    select_ref! {TokenTree::Delimited(_delimiter, line) =>
    line.spanned()}
    .to(AST::Empty)
}
