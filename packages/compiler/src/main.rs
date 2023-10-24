use chumsky::{
    prelude::*,
    text::{ascii::ident, inline_whitespace, Char},
};

#[derive(Clone, Debug, PartialEq)]
enum LayoutToken<'src> {
    Indentation(&'src str),
    Token(Token<'src>),
}

#[derive(Clone, Debug, PartialEq)]
enum Token<'src> {
    Do,
    Identifier(&'src str),
    Structure(char),
    LineEnd,
}

type Spanned<T> = (T, SimpleSpan<usize>);

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<LayoutToken<'src>>>, extra::Err<Rich<'src, char>>> {
    let identifier = ident().map(|ident| match ident {
        "do" => Token::Do,
        _ => Token::Identifier(ident),
    });
    let structure = one_of("()[]{}").map(Token::Structure);
    let line_end = just(';').to(Token::LineEnd);
    let single_indentation = text::newline().ignore_then(
        any()
            .filter(|c: &char| c.is_inline_whitespace())
            .repeated()
            .to_slice()
            .map(LayoutToken::Indentation),
    );
    let indentation = single_indentation.foldl(single_indentation.repeated(), |_x, y| y);
    let token = indentation.or(choice((line_end, structure, identifier))
        .padded_by(inline_whitespace())
        .map(LayoutToken::Token));

    token
        .map_with(|tok, e| (tok, e.span()))
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

// TODO: Write layout parser `[Spanned<LayoutToken>]` to `[Spanned<Token>]`
// TODO: Write parser of `[Spanned<Token>]`

fn main() {
    let tokens = lexer().parse(
        r#"identifier
identifier

identifier do
    identifier

    do
        identifier
        identifier
    identifier
identifier
"#,
    );
    println!("{:#?}", tokens.output());
    println!("{:?}", tokens.errors().collect::<Vec<_>>());
}
