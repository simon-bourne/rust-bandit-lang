use std::{cmp::Ordering, iter::Peekable};

use chumsky::span::{SimpleSpan, Span as _};
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Copy, Clone)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(subpattern ident = r"(\p{XID_Start}|_)\p{XID_Continue}*")]
pub enum Token<'src> {
    #[token("(", |_| Delimiter::Parentheses)]
    #[token("[", |_| Delimiter::Brackets)]
    #[token("{", |_| Delimiter::Braces)]
    Open(Delimiter),

    #[token(")", |_| Delimiter::Parentheses)]
    #[token("]", |_| Delimiter::Brackets)]
    #[token("}", |_| Delimiter::Braces)]
    Close(Delimiter),

    #[token(",")]
    Comma,

    #[regex(r"[ \t\r\f\n]*\n[ \t]*")]
    LineSeparator,

    #[token(";")]
    CloseBlock,

    #[token("and", |_| Keyword::And)]
    #[token("break", |_| Keyword::Break)]
    #[token("continue", |_| Keyword::Continue)]
    #[token("data", |_| Keyword::Data)]
    #[token("do", |_| Keyword::Do)]
    #[token("else", |_| Keyword::Else)]
    #[token("embody", |_| Keyword::Embody)]
    #[token("forall", |_| Keyword::Forall)]
    #[token("from", |_| Keyword::From)]
    #[token("if", |_| Keyword::If)]
    #[token("let", |_| Keyword::Let)]
    #[token("loop", |_| Keyword::Loop)]
    #[token("match", |_| Keyword::Match)]
    #[token("move", |_| Keyword::Move)]
    #[token("not", |_| Keyword::Not)]
    #[token("of", |_| Keyword::Of)]
    #[token("or", |_| Keyword::Or)]
    #[token("private", |_| Keyword::Private)]
    #[token("public", |_| Keyword::Public)]
    #[token("return", |_| Keyword::Return)]
    #[token("self", |_| Keyword::SelfValue)]
    #[token("then", |_| Keyword::Then)]
    #[token("trait", |_| Keyword::Trait)]
    #[token("type", |_| Keyword::Type)]
    #[token("use", |_| Keyword::Use)]
    #[token("where", |_| Keyword::Where)]
    #[token("while", |_| Keyword::While)]
    #[token("with", |_| Keyword::With)]
    Keyword(Keyword),

    #[token("|")]
    LambdaDelimiter,

    #[regex(r"(?&ident)", |lex| lex.slice())]
    Identifier(&'src str),
    #[regex(r"'(?&ident)", |lex| lex.slice())]
    Lifetime(&'src str),
    #[regex(r"[\$%\&\*\+\./<=>@\^\-\~]+", |lex| lex.slice())]
    Operator(&'src str),

    Error,
}

impl<'src> Token<'src> {
    pub fn layout(source: &'src str) -> impl Iterator<Item = SrcToken<'src>> + '_ {
        let tokens = Self::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(()) => (Self::Error, span.into()),
        });

        LayoutIter::new(tokens, source)
    }

    fn is_block_open(&self) -> bool {
        if let Token::Keyword(kw) = self {
            matches!(
                kw,
                Keyword::Do
                    | Keyword::Else
                    | Keyword::Private
                    | Keyword::Public
                    | Keyword::Then
                    | Keyword::Where
            )
        } else {
            false
        }
    }

    fn continues_line(&self) -> bool {
        matches!(self, Token::Close(_) | Token::Operator("="))
    }
}

struct LayoutIter<'src, I: Iterator<Item = SrcToken<'src>>> {
    iter: Peekable<I>,
    src: &'src str,
    last_span: Span,
    current_indent: Indent,
    indent_stack: Vec<Indent>,
}

impl<'src, I: Iterator<Item = SrcToken<'src>>> LayoutIter<'src, I> {
    pub fn new(iter: I, src: &'src str) -> Self {
        let mut iter = iter.peekable();
        let current_indent = if let Some((Token::LineSeparator, indent)) = iter.peek() {
            let indent = *indent;
            iter.next();
            Indent::new_line(src, indent)
        } else {
            Indent::MIN
        };

        Self {
            iter,
            src,
            last_span: Span::splat(0),
            current_indent,
            indent_stack: Vec::new(),
        }
    }

    fn open_block(&mut self) {
        self.indent_stack.push(self.current_indent);

        if let Some((Token::LineSeparator, span)) = self.iter.peek() {
            self.current_indent = Indent::new_line(self.src, *span);
            self.iter.next();
        } else {
            self.current_indent = Indent::same_line(self.current_indent);
        }
    }

    fn needs_block_close(&self) -> bool {
        Some(&self.current_indent) <= self.indent_stack.last()
    }

    fn close_block(&mut self, span: Span) -> Option<SrcToken<'src>> {
        self.indent_stack.pop();
        Some((Token::CloseBlock, span))
    }

    fn finish(&mut self) -> Option<SrcToken<'src>> {
        self.current_indent = Indent::MIN;

        if self.indent_stack.is_empty() {
            None
        } else {
            self.next()
        }
    }

    fn combine_else_if(&mut self, token: SrcToken<'src>) -> Option<SrcToken<'src>> {
        if token.0 == Token::Keyword(Keyword::Else) {
            if let Some((Token::Keyword(Keyword::If), if_span)) = self.iter.peek().copied() {
                self.iter.next();
                return Some((Token::Keyword(Keyword::ElseIf), if_span.union(token.1)));
            }
        }

        None
    }

    fn handle_indent(&mut self, token: SrcToken<'src>) -> Option<SrcToken<'src>> {
        let new_indent = Indent::new_line(self.src, token.1);

        match new_indent.cmp(&self.current_indent) {
            Ordering::Less => {
                self.current_indent = new_indent;
                self.next()
            }
            Ordering::Equal => match self.iter.peek() {
                Some(next_token) if !next_token.0.continues_line() => Some(token),
                _ => self.next(),
            },
            Ordering::Greater => self.next(),
        }
    }
}

impl<'src, I> Iterator for LayoutIter<'src, I>
where
    I: Iterator<Item = SrcToken<'src>>,
{
    type Item = SrcToken<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.needs_block_close() {
            return self.close_block(self.last_span);
        }

        let Some(token) = self.iter.next() else {
            return self.finish();
        };

        self.last_span = token.1;

        if let Some(t) = self.combine_else_if(token) {
            return Some(t);
        }

        match token.0 {
            Token::CloseBlock => self.close_block(token.1),
            Token::LineSeparator => self.handle_indent(token),
            _ => {
                if token.0.is_block_open() {
                    self.open_block();
                }

                Some(token)
            }
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
struct Indent {
    next_line: usize,
    same_line: usize,
}

impl Indent {
    const MIN: Self = Self {
        next_line: 0,
        same_line: 0,
    };

    fn new_line(src: &str, span: Span) -> Self {
        let s = &src[span.into_range()];
        Self {
            next_line: s.rfind('\n').map(|pos| (s.len() - pos) - 1).unwrap_or(0),
            same_line: 0,
        }
    }

    fn same_line(indent: Self) -> Self {
        Self {
            next_line: indent.next_line,
            same_line: indent.same_line + 1,
        }
    }
}

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
pub type SrcToken<'src> = Spanned<Token<'src>>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Delimiter {
    Parentheses,
    Brackets,
    Braces,
}

impl Delimiter {
    pub fn open_str(self) -> &'static str {
        match self {
            Delimiter::Parentheses => "(",
            Delimiter::Brackets => "[",
            Delimiter::Braces => "{",
        }
    }

    pub fn close_str(self) -> &'static str {
        match self {
            Delimiter::Parentheses => ")",
            Delimiter::Brackets => "]",
            Delimiter::Braces => "}",
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    And,
    Break,
    Continue,
    Data,
    Do,
    Else,
    ElseIf,
    Embody,
    Forall,
    From,
    If,
    Let,
    Loop,
    Match,
    Move,
    Not,
    Of,
    Or,
    Private,
    Public,
    Return,
    SelfValue,
    Then,
    Trait,
    Type,
    Use,
    Where,
    While,
    With,
}

impl Keyword {
    pub fn as_str(self) -> &'static str {
        use Keyword as KW;

        match self {
            KW::And => "and",
            KW::Break => "break",
            KW::Continue => "continue",
            KW::Data => "data",
            KW::Do => "do",
            KW::Else => "else",
            KW::ElseIf => "else if",
            KW::Embody => "embody",
            KW::Forall => "forall",
            KW::From => "from",
            KW::If => "if",
            KW::Let => "let",
            KW::Loop => "loop",
            KW::Match => "match",
            KW::Move => "move",
            KW::Not => "not",
            KW::Of => "of",
            KW::Or => "or",
            KW::Private => "private",
            KW::Public => "public",
            KW::Return => "return",
            KW::SelfValue => "self",
            KW::Then => "then",
            KW::Trait => "trait",
            KW::Type => "type",
            KW::Use => "use",
            KW::Where => "where",
            KW::While => "while",
            KW::With => "with",
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;

    use super::SrcToken;
    use crate::lex::Token;

    #[test]
    fn function() {
        test(r#"f() = x"#, "f ( ) = x");
    }

    #[test]
    fn if_then_else() {
        test(
            indoc!(
                r#"
                    if
                        a
                    then
                        x
                    else
                        y
                "#
            ),
            "if a , then x ; else y ;",
        );
    }

    #[test]
    fn dangling_else() {
        test(
            indoc!(
                r#"
                    if a then
                        x
                    else if b then
                        y
                    else
                        z
                "#
            ),
            "if a then x ; else if b then y ; else z ;",
        );
    }

    #[test]
    fn compact_if_else() {
        test(
            indoc!(
                r#"
                    if a then x
                    else y
                "#
            ),
            "if a then x ; else y ;",
        );
    }

    #[test]
    fn multi() {
        test(
            indoc!(
                r#"
                    if a then if b then if c then
                        x
                    else
                        y(
                            a
                            b
                        )
                        c
                "#
            ),
            "if a then if b then if c then x ; ; ; else y ( a b ) , c ;",
        );
    }

    fn test(src: &str, expected: &str) {
        print!("Source:\n\n{src}\n");
        let result = unlex(Token::layout(src), src);

        assert_eq!(expected, result);
        assert_eq!(expected, unlex(Token::layout(&result), &result));
    }

    fn unlex<'a>(layout: impl Iterator<Item = SrcToken<'a>>, src: &str) -> String {
        let layout: Vec<_> = layout.collect();
        println!("Tokens: {layout:?}");
        layout
            .into_iter()
            .map(|(token, span)| match token {
                Token::CloseBlock => ";",
                Token::LineSeparator => ",",
                _ => &src[span.into_range()],
            })
            .join(" ")
    }
}
