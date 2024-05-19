use chumsky::{recursive::recursive, IterParser, Parser};

use super::{
    ast::{DataDeclaration, Expression, Function, Item, WhereClause, AST},
    ident, keyword, open, TTParser,
};
use crate::lex::{Delimiter, Keyword};

pub fn parser<'src>() -> impl TTParser<'src, AST<'src>> {
    item().repeated().collect().map(|items| AST { items })
}

fn item<'src>() -> impl TTParser<'src, Item<'src>> {
    data_item()
        .map(Item::Data)
        .or(function().map(Item::Function))
}

fn data_item<'src>() -> impl TTParser<'src, DataDeclaration<'src>> {
    keyword(Keyword::Data)
        .ignore_then(ident())
        .then(expression().repeated().collect())
        .map(|(name, parameters)| DataDeclaration {
            name,
            parameters,
            where_clause: WhereClause(Vec::new()),
        })
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

// TODO: We need a parameter parser, because `a b c` would parse as an
// expression `(a $ b) $ c` (where `$` is function application.)
fn expression<'src>() -> impl TTParser<'src, Expression<'src>> {
    recursive(|expression| {
        let ident = ident().map(Expression::Variable);
        let parenthesized = open(Delimiter::Parentheses)
            .ignore_then(expression)
            .close(Delimiter::Parentheses);

        ident.or(parenthesized)
    })
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
