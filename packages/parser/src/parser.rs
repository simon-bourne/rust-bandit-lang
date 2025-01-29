use bandit_types::source::SourceExpression;
use winnow::{
    combinator::{alt, delimited, opt, preceded, repeat, separated_foldr1, separated_pair},
    error::ContextError,
    token::{any, one_of},
    PResult, Parser as _,
};

use crate::lex::{Grouping, Keyword, NamedOperator, SrcToken, Token};

pub type Expr<'a> = SourceExpression<'a>;
pub type TokenList<'tok, 'src> = &'tok [SrcToken<'src>];

pub trait Parser<'tok, 'src: 'tok, Out>:
    winnow::Parser<TokenList<'tok, 'src>, Out, ContextError>
{
}

impl<'tok, 'src: 'tok, Out, T> Parser<'tok, 'src, Out> for T where
    T: winnow::Parser<TokenList<'tok, 'src>, Out, ContextError>
{
}

impl<'tok, 'src: 'tok> winnow::Parser<TokenList<'tok, 'src>, Token<'src>, ContextError>
    for Token<'src>
{
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> PResult<Self> {
        one_of(|t: SrcToken| t.0 == *self)
            .value(*self)
            .parse_next(input)
    }
}

impl<'tok, 'src: 'tok> winnow::Parser<TokenList<'tok, 'src>, NamedOperator, ContextError>
    for NamedOperator
{
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> PResult<NamedOperator> {
        Token::Operator(*self).value(*self).parse_next(input)
    }
}

impl<'tok, 'src: 'tok> winnow::Parser<TokenList<'tok, 'src>, Keyword, ContextError> for Keyword {
    fn parse_next(&mut self, input: &mut TokenList<'tok, 'src>) -> PResult<Keyword> {
        Token::Keyword(*self).value(*self).parse_next(input)
    }
}

pub fn expr<'tok, 'src: 'tok>(input: &mut TokenList<'tok, 'src>) -> PResult<Expr<'src>> {
    type_annotations().parse_next(input)
}

fn type_annotations<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    separated_foldr1(
        function_types(),
        NamedOperator::HasType,
        |term, _op, typ| term.has_type(typ),
    )
}

fn function_types<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    separated_foldr1(
        function_applications(),
        NamedOperator::To,
        |input_type, _, output_type| Expr::function_type("_", input_type, output_type),
    )
}

fn function_applications<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    repeat(1.., primary()).map(|es: Vec<_>| es.into_iter().reduce(Expr::apply).unwrap())
}

fn primary<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    alt((
        typ(),
        variable(),
        forall(),
        lambda(),
        let_binding(),
        parenthesized(expr),
    ))
}

fn typ<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    identifier().verify_map(|name| (name == "Type").then(Expr::type_of_type))
}

fn forall<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    preceded(
        Keyword::Forall,
        separated_pair(variable_binding(), Token::SuchThat, expr),
    )
    .map(|((var, typ), expr)| Expr::function_type(var, typ, expr))
}

fn let_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    preceded(
        Keyword::Let,
        (
            variable_binding(),
            NamedOperator::Assign,
            expr,
            Token::LineEnd,
            expr,
        ),
    )
    .map(
        |((var, typ), _assign, variable_value, _linend, in_expression)| {
            Expr::let_binding(var, variable_value.has_type(typ), in_expression)
        },
    )
}

fn lambda<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    preceded(
        Token::Lambda,
        separated_pair(variable_binding(), Token::SuchThat, expr),
    )
    .map(|((var, typ), expr)| Expr::lambda(var, typ, expr))
}

// TODO: Multiple variable bindings
fn variable_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, (&'src str, Expr<'src>)> {
    (
        identifier(),
        opt(preceded(NamedOperator::HasType, expr))
            .map(|typ| typ.unwrap_or_else(Expr::unknown_type)),
    )
}

fn variable<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    identifier().map(Expr::variable)
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

#[cfg(test)]
mod tests {
    use bandit_types::Pretty;
    use winnow::Parser;

    use crate::{
        lex::{SrcToken, Token},
        parser::expr,
    };

    #[test]
    fn pi() {
        parse("∀x ⇒ x", "∀x ⇒ x");
    }

    #[test]
    fn lambda() {
        // TODO: This should infer the type to be `Type`
        parse(r"(\x ⇒ x) Type", r"(\x ⇒ x) Type : _");
    }

    #[test]
    fn type_annotation() {
        parse("x : Int", "x : Int");
    }

    fn parse(input: &str, expected: &str) {
        let tokens: Vec<SrcToken> = Token::layout(input).collect();
        let expr = expr.parse(&tokens).unwrap();
        assert_eq!(expr.to_pretty_string(80), expected);
    }
}
