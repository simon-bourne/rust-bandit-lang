use chumsky::{IterParser, Parser};

use super::{
    ast::{Function, Item, AST},
    ident, TTParser,
};
use crate::lex::Delimiter;

pub fn parser<'src>() -> impl TTParser<'src, AST<'src>> {
    function()
        .map(Item::Function)
        .repeated()
        .collect()
        .map(|items| AST { items })
}

fn function<'src>() -> impl TTParser<'src, Function<'src>> {
    let name = ident()
        .open(Delimiter::Parentheses)
        .close(Delimiter::Parentheses)
        .skip_operator("=")
        .open(Delimiter::Parentheses)
        .close(Delimiter::Parentheses);
    name.map(|name| Function { name })
}

#[cfg(test)]
mod tests {
    use chumsky::{prelude::Input, Parser};

    use crate::{
        lex::{Span, Token},
        parse::{
            ast::{Function, Identifier, Item, AST},
            grammar::parser,
        },
    };

    #[test]
    fn basic() {
        const SRC: &str = r#"
my_function() = ()
        "#;

        let tokens = Token::layout(SRC).collect::<Vec<_>>();
        let ast = parser().parse(tokens.spanned(Span::new(SRC.len(), SRC.len())));

        // TODO: Pretty print the AST and use golden tests. Assert all the id's
        // are the same in their source to check the spans are correct.
        assert_eq!(
            ast.unwrap(),
            AST {
                items: vec![Item::Function(Function {
                    name: Identifier {
                        name: "my_function",
                        span: Span::new(1, 12)
                    },
                })]
            }
        )
    }
}
