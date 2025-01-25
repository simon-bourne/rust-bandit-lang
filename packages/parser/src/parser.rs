use bandit_types::source::SourceExpression;
use winnow::{
    combinator::{alt, delimited, preceded, repeat, separated_foldr1, separated_pair},
    error::ContextError,
    token::{any, one_of},
    PResult, Parser as _,
};

use crate::lex::{Grouping, NamedOperator, Token};

pub type Expr<'a> = SourceExpression<'a>;

pub trait Parser<'src, Out>: winnow::Parser<&'src [Token<'src>], Out, ContextError> {}

impl<'src, Out, T> Parser<'src, Out> for T where
    T: winnow::Parser<&'src [Token<'src>], Out, ContextError>
{
}

pub fn expr<'src>(input: &mut &'src [Token<'src>]) -> PResult<Expr<'src>> {
    function_types().parse_next(input)
}

fn function_types<'src>() -> impl Parser<'src, Expr<'src>> {
    separated_foldr1(
        application(),
        operator(NamedOperator::Implies),
        |input_type, _, output_type| Expr::function_type("_", input_type, output_type),
    )
}

fn application<'src>() -> impl Parser<'src, Expr<'src>> {
    repeat(1.., primary()).map(|es: Vec<_>| {
        es.into_iter()
            .reduce(|function, argument| Expr::apply(function, argument, Expr::unknown()))
            .unwrap()
    })
}

fn primary<'src>() -> impl Parser<'src, Expr<'src>> {
    alt((typ(), variable(), lambda(), parenthesized(expr)))
}

fn typ<'src>() -> impl Parser<'src, Expr<'src>> {
    identifier().verify_map(|name| (name == "Type").then(Expr::type_of_type))
}

fn lambda<'src>() -> impl Parser<'src, Expr<'src>> {
    preceded(
        token(Token::Lambda),
        separated_pair(identifier(), operator(NamedOperator::Assign), expr),
    )
    .map(|(var, expr)| Expr::lambda(var, Expr::unknown(), expr))
}

fn variable<'src>() -> impl Parser<'src, Expr<'src>> {
    identifier().map(|name| Expr::variable(name, Expr::unknown()))
}

fn identifier<'src>() -> impl Parser<'src, &'src str> {
    any.verify_map(|t| {
        if let Token::Identifier(name) = t {
            Some(name)
        } else {
            None
        }
    })
}

fn parenthesized<'src, T>(parser: impl Parser<'src, T>) -> impl Parser<'src, T> {
    grouped(Grouping::Parentheses, parser)
}

fn grouped<'src, T>(grouping: Grouping, parser: impl Parser<'src, T>) -> impl Parser<'src, T> {
    delimited(
        token(Token::Open(grouping)),
        parser,
        token(Token::Close(grouping)),
    )
}

fn operator<'src>(name: NamedOperator) -> impl Parser<'src, ()> {
    token(Token::Operator(name))
}

fn token(token: Token<'_>) -> impl Parser<'_, ()> {
    one_of(move |t| t == token).void()
}

#[cfg(test)]
mod tests {
    use bandit_types::Pretty;
    use winnow::Parser;

    use crate::{lex::Token, parser::expr};

    #[test]
    fn expression() {
        // TODO: Can we avoid collecting the tokens?
        // TODO: Keep the span information
        let tokens = Token::layout("(\\x = x) Type")
            .map(|(t, _span)| t)
            .collect::<Vec<_>>();
        let expr = expr.parse(&tokens).unwrap();
        assert_eq!(expr.to_pretty_string(80), "((\\x = x) Type)");
    }
}
