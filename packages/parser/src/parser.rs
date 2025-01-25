use bandit_types::source::SourceExpression;
use winnow::{
    combinator::{alt, delimited, preceded, repeat, separated_foldr1, separated_pair},
    error::ContextError,
    token::{any, one_of},
    PResult, Parser as _,
};

use crate::lex::{Grouping, NamedOperator, SrcToken, Token};

pub type Expr<'a> = SourceExpression<'a>;
pub type TokenList<'src> = &'src [SrcToken<'src>];

pub trait Parser<'src, Out>: winnow::Parser<TokenList<'src>, Out, ContextError> {}

impl<'src, Out, T> Parser<'src, Out> for T where
    T: winnow::Parser<TokenList<'src>, Out, ContextError>
{
}

pub fn expr<'src>(input: &mut TokenList<'src>) -> PResult<Expr<'src>> {
    type_annotations().parse_next(input)
}

fn type_annotations<'src>() -> impl Parser<'src, Expr<'src>> {
    right_operators([NamedOperator::HasType], function_types())
}

fn function_types<'src>() -> impl Parser<'src, Expr<'src>> {
    separated_foldr1(
        function_applications(),
        operator(NamedOperator::Implies),
        // TODO: Optionally parse binding name
        |input_type, _, output_type| Expr::function_type("_", input_type, output_type),
    )
}

fn function_applications<'src>() -> impl Parser<'src, Expr<'src>> {
    repeat(1.., primary()).map(|es: Vec<_>| es.into_iter().reduce(Expr::apply).unwrap())
}

fn primary<'src>() -> impl Parser<'src, Expr<'src>> {
    alt((typ(), variable(), lambda(), parenthesized(expr)))
}

fn typ<'src>() -> impl Parser<'src, Expr<'src>> {
    identifier().verify_map(|name| (name == "Type").then(Expr::type_of_type))
}

// TODO: Parse let bindings

fn lambda<'src>() -> impl Parser<'src, Expr<'src>> {
    preceded(
        token(Token::Lambda),
        separated_pair(identifier(), operator(NamedOperator::Assign), expr),
    )
    // TODO: Optionally parse binding type
    .map(|(var, expr)| Expr::lambda(var, Expr::unknown(), expr))
}

fn variable<'src>() -> impl Parser<'src, Expr<'src>> {
    identifier().map(Expr::variable)
}

fn identifier<'src>() -> impl Parser<'src, &'src str> {
    any.verify_map(|t: SrcToken| {
        if let Token::Identifier(name) = t.0 {
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

fn right_operators<'src, const N: usize>(
    ops: [NamedOperator; N],
    tighter_binding_operators: impl Parser<'src, Expr<'src>>,
) -> impl Parser<'src, Expr<'src>> {
    separated_foldr1(
        tighter_binding_operators,
        matches_operators(ops),
        operator_expression,
    )
}

fn matches_operators<'src, const N: usize>(
    ops: [NamedOperator; N],
) -> impl Parser<'src, NamedOperator> {
    any.verify_map(move |t: SrcToken| ops.iter().copied().find(|op| Token::Operator(*op) == t.0))
}

fn operator<'src>(name: NamedOperator) -> impl Parser<'src, ()> {
    token(Token::Operator(name))
}

fn token(token: Token<'_>) -> impl Parser<'_, ()> {
    one_of(move |t: SrcToken| t.0 == token).void()
}

fn operator_expression<'src>(value: Expr<'src>, op: NamedOperator, typ: Expr<'src>) -> Expr<'src> {
    Expr::apply(
        Expr::apply_operator(Expr::variable(op.as_str()), value),
        typ,
    )
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
    fn expression() {
        let tokens: Vec<SrcToken> = Token::layout("(\\x = x) Type").collect();
        let expr = expr.parse(&tokens).unwrap();
        assert_eq!(expr.to_pretty_string(80), "((\\x = x) Type)");
    }

    #[test]
    fn type_annotation() {
        let tokens: Vec<SrcToken> = Token::layout("x : Int").collect();
        let expr = expr.parse(&tokens).unwrap();
        assert_eq!(expr.to_pretty_string(80), "((x :) Int)");
    }
}
