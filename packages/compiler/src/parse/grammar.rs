use chumsky::{
    pratt::{self, left, right, Associativity, Infix},
    recursive::recursive,
    IterParser, Parser,
};

use super::{
    ast::{DataDeclaration, Expression, Function, Item, Operator, OperatorName, WhereClause, AST},
    ident, keyword, line_separator, operator, parenthesized, TTParser,
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
        .then(type_parameter().repeated().collect())
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

/// Parse a type parameter
///
/// Like parsing an expression, but the top level is just an identifier or
/// parathesized expression.
fn type_parameter<'src>() -> impl TTParser<'src, Expression<'src>> {
    ident()
        .map(Expression::Variable)
        .or(parenthesized(expression()))
}

fn expression<'src>() -> impl TTParser<'src, Expression<'src>> {
    recursive(|expression| {
        let ident = ident().map(Expression::Variable);
        let parenthesized = parenthesized(expression.clone());
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

        let operators = application
            .clone()
            .pratt((infix(right(1), "->"), infix(left(5), "+")));

        // TODO: Add quantification
        let type_annotated = operators
            .clone()
            .then_ignore(operator(":"))
            .then(expression.clone())
            .then(where_clause(expression))
            .map(
                |((e, type_constraint), where_clause)| Expression::TypeAnnotation {
                    expression: Box::new(e),
                    type_expression: Box::new(type_constraint),
                    where_clause,
                },
            );

        type_annotated.or(operators)
    })
}

fn where_clause<'src>(
    expression: impl TTParser<'src, Expression<'src>>,
) -> impl TTParser<'src, WhereClause<'src>> {
    keyword(Keyword::Where)
        .ignore_then(
            expression
                .separated_by(line_separator())
                .allow_trailing()
                .collect(),
        )
        .close_block()
        .or_not()
        .map(|where_clause| WhereClause(where_clause.unwrap_or_default()))
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

// Windows line endings are a pain, so just skip the parser tests
#[cfg(all(test, not(target_os = "windows")))]
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

    #[test]
    fn function() {
        parse("function", r#"my_function() = ()"#);
    }

    fn parse(name: &str, src: &str) {
        let tokens = Token::layout(src).collect::<Vec<_>>();
        let len = src.len();
        let ast = parser().parse(tokens.spanned(Span::new(len, len)));

        let mut mint = Mint::new("tests/goldenfiles");
        let mut output = mint.new_goldenfile(format!("{name}.txt")).unwrap();
        write!(output, "{ast:#?}").unwrap();
    }
}
