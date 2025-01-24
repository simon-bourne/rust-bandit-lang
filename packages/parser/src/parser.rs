use bandit_types::source::SourceExpression;
use winnow::{
    ascii::{multispace0, multispace1},
    combinator::{alt, delimited, preceded, separated_foldl1, separated_pair},
    error::ContextError,
    stream::AsChar,
    token::{literal, one_of, take_while},
    PResult, Parser as _,
};

pub type Expr<'a> = SourceExpression<'a>;

pub trait Parser<'src, Out>: winnow::Parser<&'src str, Out, ContextError> {}

impl<'src, Out, T> Parser<'src, Out> for T where T: winnow::Parser<&'src str, Out, ContextError> {}

pub fn expr<'src>(input: &mut &'src str) -> PResult<Expr<'src>> {
    separated_foldl1(
        alt((
            delimited('(', ws(expr), ')'),
            typ(),
            variable(),
            lambda(),
            function_type(),
        )),
        multispace1,
        |function, _, argument| Expr::apply(function, argument, Expr::unknown()),
    )
    .parse_next(input)
}

fn typ<'src>() -> impl Parser<'src, Expr<'src>> {
    literal("Type").map(|_| Expr::type_of_type())
}

fn lambda<'src>() -> impl Parser<'src, Expr<'src>> {
    preceded(
        ('\\', multispace0),
        separated_pair(identifier(), ws("="), expr),
    )
    .map(|(var, expr)| Expr::lambda(var, Expr::unknown(), expr))
}

fn function_type<'src>() -> impl Parser<'src, Expr<'src>> {
    separated_pair(expr, ws("->"), expr)
        .map(|(input_type, output_type)| Expr::function_type("_", input_type, output_type))
}

fn variable<'src>() -> impl Parser<'src, Expr<'src>> {
    identifier().map(|name| Expr::variable(name, Expr::unknown()))
}

fn identifier<'src>() -> impl Parser<'src, &'src str> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_'),
    )
        .take()
}

fn ws<'src, F, O>(inner: F) -> impl Parser<'src, O>
where
    F: Parser<'src, O>,
{
    delimited(multispace0, inner, multispace0)
}

#[cfg(test)]
mod tests {
    use winnow::Parser;

    use crate::parser::expr;

    #[test]
    fn expression() {
        let expr = expr.parse("(\\x = x) Type").unwrap();
        assert_eq!(
            expr.render_to_string(80),
            "(((\\_:{unknown} = (x:{unknown})) Type):{unknown})"
        );
    }
}
