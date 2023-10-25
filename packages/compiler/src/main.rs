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
enum Delimiter {
    Parentheses,
    Bracket,
    Brace,
}

#[derive(Clone, Debug, PartialEq)]
enum Token<'src> {
    Do,
    Open(Delimiter),
    Close(Delimiter),
    Identifier(&'src str),
    LineEnd,
    BlockEnd,
}

type Spanned<T> = (T, SimpleSpan<usize>);

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<LayoutToken<'src>>>, extra::Err<Rich<'src, char>>> {
    let identifier = ident().map(|ident| match ident {
        "do" => Token::Do,
        _ => Token::Identifier(ident),
    });

    let delimiter = one_of("()[]{}").map(|c| match c {
        '(' => Token::Open(Delimiter::Parentheses),
        ')' => Token::Close(Delimiter::Parentheses),
        '[' => Token::Open(Delimiter::Bracket),
        ']' => Token::Close(Delimiter::Bracket),
        '{' => Token::Open(Delimiter::Brace),
        '}' => Token::Close(Delimiter::Brace),
        _ => unreachable!(),
    });

    let line_end = just(';').to(Token::LineEnd);
    let single_indentation = text::newline().ignore_then(
        any()
            .filter(|c: &char| c.is_inline_whitespace())
            .repeated()
            .to_slice()
            .map(LayoutToken::Indentation),
    );
    let indentation = single_indentation.foldl(single_indentation.repeated(), |_x, y| y);
    let token = indentation.or(choice((line_end, delimiter, identifier))
        .padded_by(inline_whitespace())
        .map(LayoutToken::Token));

    token
        .map_with(|tok, e| (tok, e.span()))
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[derive(Eq, PartialEq)]
enum Context<'src> {
    LineLayout,
    BlockLayout(&'src str),
    // TODO: Should we check the delimiter type?
    Delimited,
}

fn layout<'src>(
    input: impl IntoIterator<Item = Spanned<LayoutToken<'src>>>,
) -> Vec<Spanned<Token<'src>>> {
    let mut output = Vec::new();
    let mut context = Vec::new();
    let mut input = input.into_iter();

    while let Some((mut token, mut span)) = input.next() {
        if token == LayoutToken::Token(Token::Do) {
            output.push((Token::Do, span));

            match input.next() {
                Some((next_token, next_span)) => {
                    match next_token.clone() {
                        LayoutToken::Indentation(indentation) => {
                            context.push(Context::BlockLayout(indentation))
                        }
                        LayoutToken::Token(t) => {
                            output.push((t, span));
                            context.push(Context::LineLayout);
                        }
                    }

                    token = next_token;
                    span = next_span;
                }
                None => todo!(),
            }
        }

        match token {
            // TODO: Should this be in an `else`, so we don't add an end of line before the first
            // line.
            LayoutToken::Indentation(indentation) => {
                let eol_span = SimpleSpan::splat(span.start());

                while let Some(Context::LineLayout) = context.last() {
                    output.extend([Token::LineEnd, Token::BlockEnd].map(|t| (t, eol_span)));
                    context.pop();
                }

                if let Some(Context::BlockLayout(current_indentation)) = context.last() {
                    while current_indentation.starts_with(indentation) {
                        if indentation == *current_indentation {
                            output.push((Token::LineEnd, eol_span));
                        } else {
                            output.extend([Token::LineEnd, Token::BlockEnd].map(|t| (t, eol_span)));
                            // TODO context.pop();
                            // TODO: Get next parent_indentation
                        }
                    }

                    // TODO: What about blocks with a line continuation after them?

                    // TODO: How to handle inconsistent indentation?
                    assert!(
                        indentation.starts_with(current_indentation)
                            || current_indentation.starts_with(indentation)
                    );
                }
            }
            LayoutToken::Token(Token::Open(_)) => {
                context.push(Context::Delimited);
            }
            LayoutToken::Token(Token::Close(_)) => {
                // TODO: Close all layout blocks
                context.pop();
            }
            LayoutToken::Token(token) => output.push((token, span)),
        }
    }

    // TODO: Close any open layout blocks

    // TODO: Handle:
    // - `do (`
    // - `do )`
    output
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
    println!("{:#?}", tokens.into_output().map(layout));
}
