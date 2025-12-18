// TODO: `static x = 3 * 4`
// TODO: Newline to separate let binding and expression.
// TODO: Static and dynamic type construction `T → U`, `T ⇒ U`, `∀x : T → U`,
// and `∀x : T ⇒ U`
use bandit_term::{
    Evaluation,
    ast::{Data, Definition, Function, ValueConstructor},
};
use winnow::{
    Parser as _, Result,
    combinator::{
        alt, delimited, opt, preceded, repeat, separated, separated_foldl1, separated_foldr1,
        separated_pair, terminated,
    },
    token::any,
};

use super::{Parser, Term, TokenList};
use crate::lex::{Grouping, Keyword, Operator, SrcToken, Token};

pub fn definitions<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Vec<Definition<'src>>> {
    separated(
        ..,
        alt((
            function_definition().map(Definition::Function),
            data_definition().map(Definition::Data),
        )),
        Token::LineEnd,
    )
}

fn function_definition<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Function<'src>> {
    (
        identifier(),
        opt(has_type()),
        preceded(Operator::Assign, term),
    )
        .map(|(name, typ, value)| Function::new(name, typ, value))
}

fn data_definition<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Data<'src>> {
    preceded(
        Keyword::Data,
        (identifier(), opt(has_type()), block(value_constructor())),
    )
    .map(|(name, typ, value_constructors)| Data::new(name, typ, value_constructors))
}

fn value_constructor<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, ValueConstructor<'src>> {
    (identifier(), opt(has_type())).map(|(name, typ)| ValueConstructor::new(name, typ))
}

fn has_type<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    preceded(Operator::HasType, term)
}

fn block<'tok, 'src: 'tok, T>(
    parse: impl Parser<'tok, 'src, T>,
) -> impl Parser<'tok, 'src, Vec<T>> {
    terminated(separated(.., parse, Token::LineEnd), Keyword::End)
}

pub fn term<'tok, 'src: 'tok>(input: &mut TokenList<'tok, 'src>) -> Result<Term<'src>> {
    type_annotations().parse_next(input)
}

fn type_annotations<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    separated_foldr1(function_types(), Operator::HasType, |term, _op, typ| {
        term.has_type(typ)
    })
}

fn function_types<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    separated_foldr1(
        function_applications(),
        Operator::To,
        |input_type, _, output_type| Term::function_type(input_type, output_type),
    )
}

fn function_applications<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    repeat(1.., static_apply()).map(|es: Vec<_>| es.into_iter().reduce(Term::apply).unwrap())
}

fn static_apply<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    separated_foldl1(
        primary(),
        Operator::StaticApply,
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
        separated_pair(term, Operator::Implies, term),
    )
    .map(|(variable, in_term)| Term::pi_type(variable, in_term, Evaluation::Static))
}

fn let_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    preceded(
        Keyword::Let,
        (term, Operator::Assign, term, Operator::Implies, term),
    )
    .map(|(var, _assign, value, _linend, in_term)| Term::let_binding(var, value, in_term))
}

fn lambda<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    preceded(Token::Lambda, separated_pair(term, Operator::Implies, term))
        .map(|(variable, in_term)| Term::lambda(variable, in_term))
}

fn unknown<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    Token::Unknown.map(|_| Term::unknown())
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
