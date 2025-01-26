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

pub fn expr<'tok, 'src: 'tok>(input: &mut TokenList<'tok, 'src>) -> PResult<Expr<'src>> {
    type_annotations().parse_next(input)
}

fn type_annotations<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    right_operators([NamedOperator::HasType], function_types())
}

fn function_types<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    separated_foldr1(
        function_applications(),
        operator(NamedOperator::To),
        // TODO: Optionally parse binding name. Use:
        // - `∀name . `
        // - `∀name : Type . `
        // - `∀x y z . `
        // - `∀x (y : Type) (z : Type) . `
        //
        // This can be used for multi argument lambdas as well.
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
        lambda(),
        let_binding(),
        parenthesized(expr),
    ))
}

fn typ<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    identifier().verify_map(|name| (name == "Type").then(Expr::type_of_type))
}

fn let_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    preceded(
        token(Token::Keyword(Keyword::Let)),
        (
            variable_binding(),
            operator(NamedOperator::Assign),
            expr,
            token(Token::LineEnd),
            expr,
        ),
    )
    .map(
        |((var, typ), _assign, variable_value, _line, in_expression)| {
            Expr::let_binding(var, typ, variable_value, in_expression)
        },
    )
}

fn lambda<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Expr<'src>> {
    preceded(
        token(Token::Lambda),
        separated_pair(variable_binding(), operator(NamedOperator::Assign), expr),
    )
    .map(|((var, typ), expr)| Expr::lambda(var, typ, expr))
}

fn variable_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, (&'src str, Expr<'src>)> {
    (
        identifier(),
        opt(preceded(operator(NamedOperator::HasType), expr))
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
    delimited(
        token(Token::Open(grouping)),
        parser,
        token(Token::Close(grouping)),
    )
}

fn right_operators<'tok, 'src: 'tok, const N: usize>(
    ops: [NamedOperator; N],
    tighter_binding_operators: impl Parser<'tok, 'src, Expr<'src>>,
) -> impl Parser<'tok, 'src, Expr<'src>> {
    separated_foldr1(
        tighter_binding_operators,
        matches_operators(ops),
        operator_expression,
    )
}

fn matches_operators<'tok, 'src: 'tok, const N: usize>(
    ops: [NamedOperator; N],
) -> impl Parser<'tok, 'src, NamedOperator> {
    any.verify_map(move |t: SrcToken| ops.iter().copied().find(|op| Token::Operator(*op) == t.0))
}

fn operator<'tok, 'src: 'tok>(name: NamedOperator) -> impl Parser<'tok, 'src, ()> {
    token(Token::Operator(name))
}

fn token<'tok, 'src: 'tok>(token: Token<'src>) -> impl Parser<'tok, 'src, ()> {
    one_of(move |t: SrcToken| t.0 == token).void()
}

fn operator_expression<'src>(value: Expr<'src>, op: NamedOperator, typ: Expr<'src>) -> Expr<'src> {
    Expr::apply(Expr::apply(Expr::variable(op.as_str()), value), typ)
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
