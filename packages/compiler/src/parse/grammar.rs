use chumsky::{
    pratt::{self, right, Associativity, Infix},
    recursive::recursive,
    IterParser, Parser,
};

use super::{
    ast::{DataDeclaration, Expression, Function, Item, Operator, OperatorName, WhereClause, AST},
    ident, keyword, open, operator, TTParser,
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

        let atom = ident.or(parenthesized);

        // Function application is an implicit operator, and has higher precedence than
        // everything else.
        let application =
            atom.clone()
                .foldl(atom.repeated(), |left, right| Expression::BinaryOperator {
                    name: OperatorName::Apply,
                    left: Box::new(left),
                    right: Box::new(right),
                });

        application
            .clone()
            .pratt((infix(right(0), "->"), infix(right(5), ":")))
    })
}

fn infix<'src>(
    associativity: Associativity,
    name: &'src str,
) -> Infix<
    impl TTParser<Operator>,
    impl Fn(Expression<'src>, Operator<'src>, Expression<'src>) -> Expression<'src> + Clone,
    Operator<'src>,
    (Expression<'src>, Operator<'src>, Expression<'src>),
> {
    pratt::infix::<_, _, Operator, (Expression, Operator, Expression)>(
        associativity,
        operator(name),
        |left, op, right| Expression::BinaryOperator {
            name: OperatorName::Named(op),
            left: Box::new(left),
            right: Box::new(right),
        },
    )
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use chumsky::{prelude::Input, Parser};
    use goldenfile::Mint;

    use crate::{
        lex::{Span, Token},
        parse::grammar::parser,
    };

    #[test]
    fn data_declaration() {
        parse(
            "data-declaration",
            r#"data MyType a (b : Type) (c : Type -> Type -> Type)"#,
        )
    }

    fn parse(name: &str, src: &str) {
        let tokens = Token::layout(src).collect::<Vec<_>>();
        let len = src.len();
        let ast = parser().parse(tokens.spanned(Span::new(len, len)));

        let mut mint = Mint::new("tests/goldenfiles");
        let mut output = mint.new_goldenfile(format!("{name}.txt")).unwrap();
        write!(output, "{ast:?}").unwrap();
    }

    #[test]
    fn function() {
        parse(
            "function",
            r#"
my_function() = ()
        "#,
        );
    }
}
