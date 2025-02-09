use winnow::{
    combinator::{alt, delimited, opt, preceded, repeat, separated_foldr1, separated_pair},
    token::any,
    PResult, Parser as _,
};

use super::{Expression, Parser, TokenList};
use crate::lex::{Grouping, Keyword, NamedOperator, SrcToken, Token};

pub fn expr<'tok, 'src: 'tok>(input: &mut TokenList<'tok, 'src>) -> PResult<Expression<'src>> {
    type_annotations().parse_next(input)
}

fn type_annotations<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    separated_foldr1(
        function_types(),
        NamedOperator::HasType,
        |term, _op, typ| term.has_type(typ),
    )
}

fn function_types<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    separated_foldr1(
        function_applications(),
        NamedOperator::To,
        |input_type, _, output_type| {
            Expression::function_type(
                "_",
                Expression::unknown_value().has_type(input_type),
                output_type,
            )
        },
    )
}

fn function_applications<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    repeat(1.., primary()).map(|es: Vec<_>| es.into_iter().reduce(Expression::apply).unwrap())
}

fn primary<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    alt((
        unknown(),
        typ(),
        variable(),
        forall(),
        lambda(),
        let_binding(),
        parenthesized(expr),
    ))
}

fn typ<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    identifier().verify_map(|name| (name == "Type").then(Expression::type_of_type))
}

fn forall<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    preceded(
        Keyword::Forall,
        separated_pair(variable_binding(), Token::SuchThat, expr),
    )
    .map(|((var, value), expr)| Expression::function_type(var, value, expr))
}

fn let_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    preceded(
        Keyword::Let,
        (
            identifier(),
            opt(preceded(NamedOperator::HasType, expr)),
            NamedOperator::Assign,
            expr,
            Token::SuchThat,
            expr,
        ),
    )
    .map(
        |(var, typ, _assign, variable_value, _linend, in_expression)| {
            Expression::let_binding(
                var,
                if let Some(typ) = typ {
                    variable_value.has_type(typ)
                } else {
                    variable_value
                },
                in_expression,
            )
        },
    )
}

fn lambda<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    preceded(
        Token::Lambda,
        separated_pair(variable_binding(), Token::SuchThat, expr),
    )
    .map(|((var, value), expr)| Expression::lambda(var, value, expr))
}

// TODO: Multiple variable bindings
fn variable_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, (&'src str, Expression<'src>)> {
    (
        identifier(),
        opt(preceded(NamedOperator::HasType, expr)).map(|typ| {
            let value = Expression::unknown_value();

            if let Some(typ) = typ {
                value.has_type(typ)
            } else {
                value
            }
        }),
    )
}

fn unknown<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    Token::Unknown.map(|_| Expression::unknown_value())
}

fn variable<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expression<'src>> {
    identifier().map(Expression::variable)
}

fn identifier<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, &'src str> {
    any.verify_map(|t: SrcToken| {
        if let Token::Identifier(name) = t.0 {
            Some(name)
        } else {
            None
        }
    })
}

fn parenthesized<'tok, 'src: 'tok, T>(
    parser: impl Parser<'tok, 'src, T>,
) -> impl Parser<'tok, 'src, T> {
    grouped(Grouping::Parentheses, parser)
}

fn grouped<'tok, 'src: 'tok, T>(
    grouping: Grouping,
    parser: impl Parser<'tok, 'src, T>,
) -> impl Parser<'tok, 'src, T> {
    delimited(Token::Open(grouping), parser, Token::Close(grouping))
}
