// TODO: `static x = 3 * 4`
// TODO: Newline to separate let binding and expression.
// TODO: Static and dynamic type construction `T → U`, `T ⇒ U`, `∀x : T → U`,
// and `∀x : T ⇒ U`
use bandit_types::Evaluation;
use winnow::{
    Parser as _, Result,
    combinator::{
        alt, delimited, opt, preceded, repeat, separated_foldl1, separated_foldr1, separated_pair,
    },
    token::any,
};

use super::{Parser, Term, TokenList};
use crate::lex::{Grouping, Keyword, NamedOperator, SrcToken, Token};

pub fn term<'tok, 'src: 'tok>(input: &mut TokenList<'tok, 'src>) -> Result<Term<'src>> {
    type_annotations().parse_next(input)
}

fn type_annotations<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    separated_foldr1(
        function_types(),
        NamedOperator::HasType,
        |term, _op, typ| term.has_type(typ),
    )
}

fn function_types<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    separated_foldr1(
        function_applications(),
        NamedOperator::To,
        |input_type, _, output_type| {
            Term::pi_type(None, input_type, output_type, Evaluation::Dynamic)
        },
    )
}

fn function_applications<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    repeat(1.., static_apply()).map(|es: Vec<_>| es.into_iter().reduce(Term::apply).unwrap())
}

fn static_apply<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    separated_foldl1(
        primary(),
        NamedOperator::StaticApply,
        |input_type, _, output_type| Term::static_apply(input_type, output_type),
    )
}

fn primary<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    alt((
        unknown(),
        typ(),
        variable(),
        forall(),
        lambda(),
        let_binding(),
        parenthesized(term),
    ))
}

fn typ<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    identifier().verify_map(|name| (name == "Type").then(Term::type_of_type))
}

fn forall<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    preceded(
        Keyword::Forall,
        separated_pair(variable_binding(), Token::SuchThat, term),
    )
    .map(|((var, typ), term)| Term::pi_type(Some(var), typ, term, Evaluation::Static))
}

fn let_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    preceded(
        Keyword::Let,
        (
            identifier(),
            opt(preceded(NamedOperator::HasType, term)),
            NamedOperator::Assign,
            term,
            Token::SuchThat,
            term,
        ),
    )
    .map(|(var, typ, _assign, variable_value, _linend, in_term)| {
        Term::let_binding(
            var,
            if let Some(typ) = typ {
                variable_value.has_type(typ)
            } else {
                variable_value
            },
            in_term,
        )
    })
}

fn lambda<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    preceded(
        Token::Lambda,
        separated_pair(variable_binding(), Token::SuchThat, term),
    )
    .map(|((var, typ), term)| Term::lambda(var, typ, term))
}

fn variable_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, (&'src str, Term<'src>)> {
    (
        identifier(),
        opt(preceded(NamedOperator::HasType, term))
            .map(|typ| typ.unwrap_or_else(Term::unknown_type)),
    )
}

fn unknown<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    Token::Unknown.map(|_| Term::unknown_value())
}

fn variable<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    identifier().map(Term::variable)
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
