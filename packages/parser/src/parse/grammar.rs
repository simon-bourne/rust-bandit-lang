// TODO: Newline to separate let binding and expression.
use bandit_term::{
    ArgumentStyle,
    ast::{Constant, Data, Definition, Function},
};
use winnow::{
    Parser as _, Result,
    combinator::{
        alt, delimited, opt, preceded, repeat, separated, separated_foldr1, separated_pair,
        terminated,
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
    (identifier(), opt(has_type()), preceded(Token::Assign, term))
        .map(|(name, typ, value)| Function::new(name, typ, value))
}

fn data_definition<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Data<'src>> {
    preceded(
        Keyword::Data,
        (identifier(), opt(has_type()), block(value_constructor())),
    )
    .map(|(name, typ, value_constructors)| Data::new(name, typ, value_constructors))
}

fn value_constructor<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Constant<'src>> {
    (identifier(), opt(has_type())).map(|(name, typ)| Constant::new(name, typ))
}

fn has_type<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    preceded(Operator::HasType, term)
}

fn block<'tok, 'src: 'tok, T>(
    parse: impl Parser<'tok, 'src, T>,
) -> impl Parser<'tok, 'src, Vec<T>> {
    terminated(repeat(.., preceded(Token::LineEnd, parse)), Token::BlockEnd)
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
        pi_argument_style(),
        |input_type, arg_style, output_type| {
            Term::function_type(input_type, output_type, arg_style)
        },
    )
}

fn function_applications<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    repeat(1.., primary()).map(|es: Vec<_>| es.into_iter().reduce(Term::apply).unwrap())
}

fn primary<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    alt((
        unknown(),
        typ(),
        variable(),
        forall(),
        lambda(),
        let_binding(),
        specify_implicits(term),
        parenthesized(term),
    ))
}

fn typ<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    identifier().verify_map(|name| (name == "Type").then(Term::type_of_type))
}

fn forall<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    let forall_binder = alt((variable(), parenthesized(binder())));
    preceded(Keyword::Forall, (forall_binder, pi_argument_style(), term))
        .map(|(variable, arg_style, in_term)| Term::pi_type(variable, in_term, arg_style))
}

fn let_binding<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    preceded(Keyword::Let, (term, Token::Assign, term, Keyword::In, term))
        .map(|(var, _assign, value, _linend, in_term)| Term::let_binding(var, value, in_term))
}

fn lambda<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    (
        lambda_argument_style(),
        separated_pair(term, Token::Assign, term),
    )
        .map(move |(arg_style, (variable, in_term))| Term::lambda(variable, in_term, arg_style))
}

fn unknown<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    Token::Unknown.map(|_| Term::unknown())
}

fn variable<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    identifier().map(Term::variable)
}

fn binder<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, Term<'src>> {
    // TODO: Limit this to var name with optional type annotation
    term
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

fn pi_argument_style<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, ArgumentStyle> {
    alt((
        Operator::To.value(ArgumentStyle::Explicit),
        Operator::Implies.value(ArgumentStyle::Implicit),
    ))
}

fn lambda_argument_style<'tok, 'src: 'tok>() -> impl Parser<'tok, 'src, ArgumentStyle> {
    alt((
        Token::Lambda.value(ArgumentStyle::Explicit),
        Token::ImplicitLambda.value(ArgumentStyle::Implicit),
    ))
}

fn specify_implicits<'tok, 'src: 'tok>(
    parser: impl Parser<'tok, 'src, Term<'src>>,
) -> impl Parser<'tok, 'src, Term<'src>> {
    grouped(Grouping::Brackets, parser).map(Term::specify_implicits)
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
