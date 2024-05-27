use std::{cmp::Ordering, iter::Peekable};

use chumsky::span::SimpleSpan;
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Copy, Clone)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(subpattern ident = r"(\p{XID_Start}|_)\p{XID_Continue}*")]
pub enum Token<'src> {
    #[token("(", |_| Grouping::Parentheses)]
    #[token("[", |_| Grouping::Brackets)]
    #[token("{", |_| Grouping::Braces)]
    Open(Grouping),

    #[token(")", |_| Grouping::Parentheses)]
    #[token("]", |_| Grouping::Brackets)]
    #[token("}", |_| Grouping::Braces)]
    Close(Grouping),

    #[token(",")]
    Comma,

    #[regex(r"[ \t\r\f\n]*\n[ \t]*")]
    LineEnd,

    #[token("and", |_| Keyword::And)]
    #[token("break", |_| Keyword::Break)]
    #[token("case", |_| Keyword::Case)]
    #[token("continue", |_| Keyword::Continue)]
    #[token("data", |_| Keyword::Data)]
    #[token("do", |_| Keyword::Do)]
    #[token("else", |_| Keyword::Else)]
    #[token("embody", |_| Keyword::Embody)]
    #[token("∀", |_| Keyword::Forall)]
    #[token("forall", |_| Keyword::Forall)]
    #[token("from", |_| Keyword::From)]
    #[token("if", |_| Keyword::If)]
    #[token("let", |_| Keyword::Let)]
    #[token("loop", |_| Keyword::Loop)]
    #[token("not", |_| Keyword::Not)]
    #[token("of", |_| Keyword::Of)]
    #[token("or", |_| Keyword::Or)]
    #[token("private", |_| Keyword::Private)]
    #[token("public", |_| Keyword::Public)]
    #[token("return", |_| Keyword::Return)]
    #[token("then", |_| Keyword::Then)]
    #[token("trait", |_| Keyword::Trait)]
    #[token("type", |_| Keyword::Type)]
    #[token("use", |_| Keyword::Use)]
    #[token("where", |_| Keyword::Where)]
    #[token("while", |_| Keyword::While)]
    Keyword(Keyword),

    #[token("λ")]
    #[token("\\")]
    Lambda,

    #[regex(r"(?&ident)")]
    Identifier(&'src str),

    // TODO: != not, and, or, <=, >=
    // TODO: Name all operators
    #[token(r"⇒")]
    #[token(r"→")]
    #[regex(r"[\$%\&\*\+\./<=>@\^\-\~:]+")]
    Operator(&'src str),

    #[regex(r"\.(?&ident)")]
    NamedOperator(&'src str),

    Error(Error),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Error {
    UnknownToken,
    Dedent,
}

struct ContinuedLines<'src, I>(Peekable<I>)
where
    I: Iterator<Item = SrcToken<'src>>;

impl<'src, I> Iterator for ContinuedLines<'src, I>
where
    I: Iterator<Item = SrcToken<'src>>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.0.next()?;

        if matches!(item.0, Token::LineEnd) {
            match self.0.peek() {
                Some(next) if next.0.continues_line() => return self.0.next(),
                None => return self.0.next(),
                _ => (),
            }
        }

        Some(item)
    }
}

impl<'src> Token<'src> {
    pub fn layout(source: &'src str) -> impl Iterator<Item = SrcToken<'src>> + '_ {
        let tokens = Self::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(()) => (Self::Error(Error::UnknownToken), span.into()),
        });

        LayoutIter::new(ContinuedLines(tokens.peekable()), source)
    }

    fn continues_line(&self) -> bool {
        matches!(self, Token::Operator(_) | Token::NamedOperator(_))
    }
}

struct LayoutIter<'src, I: Iterator<Item = SrcToken<'src>>> {
    iter: I,
    src: &'src str,
    last_span: Span,
    current_indent: Indent,
    indent_stack: Vec<Indent>,
}

impl<'src, I: Iterator<Item = SrcToken<'src>>> LayoutIter<'src, I> {
    pub fn new(iter: I, src: &'src str) -> Self {
        Self {
            iter,
            src,
            last_span: Span::splat(0),
            current_indent: Indent::default(),
            indent_stack: Vec::new(),
        }
    }

    fn try_close_block(&mut self, span: Span) -> Option<SrcToken<'src>> {
        let top = self.indent_stack.last()?;

        match self.current_indent.cmp(top) {
            Ordering::Less => {
                let result = self.close_block(span);

                if self
                    .indent_stack
                    .last()
                    .is_some_and(|top| self.current_indent > *top)
                {
                    return Some((Token::Error(Error::Dedent), span));
                }

                Some(result)
            }
            Ordering::Equal => Some(self.close_block(span)),
            Ordering::Greater => None,
        }
    }

    fn close_block(&mut self, span: Span) -> SrcToken<'src> {
        self.indent_stack.pop();
        (Token::Close(Grouping::Block), span)
    }

    fn finish(&mut self) -> Option<SrcToken<'src>> {
        self.current_indent = Indent::default();
        self.try_close_block(self.last_span)
    }

    fn handle_indent(&mut self, span: Span) -> SrcToken<'src> {
        let new_indent = Indent::new(self.src, span);

        match new_indent.cmp(&self.current_indent) {
            Ordering::Less => {
                self.current_indent = new_indent;
                self.try_close_block(span)
                    .unwrap_or((Token::Error(Error::Dedent), span))
            }
            Ordering::Equal => (Token::LineEnd, span),
            Ordering::Greater => {
                self.indent_stack.push(self.current_indent);
                self.current_indent = new_indent;
                (Token::Open(Grouping::Block), span)
            }
        }
    }
}

impl<'src, I> Iterator for LayoutIter<'src, I>
where
    I: Iterator<Item = SrcToken<'src>>,
{
    type Item = SrcToken<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.try_close_block(self.last_span) {
            return Some(token);
        }

        let Some(token) = self.iter.next() else {
            return self.finish();
        };

        self.last_span = token.1;

        let result = if token.0 == Token::LineEnd {
            self.handle_indent(token.1)
        } else {
            token
        };

        Some(result)
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Default)]
struct Indent(usize);

impl Indent {
    fn new(src: &str, span: Span) -> Self {
        Self(
            src[span.into_range()]
                .rsplit_once('\n')
                .map(|(_head, tail)| tail.len())
                .unwrap_or(0),
        )
    }
}

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
pub type SrcToken<'src> = Spanned<Token<'src>>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Grouping {
    Parentheses,
    Brackets,
    Braces,
    Block,
}

impl Grouping {
    pub fn open_str(self) -> &'static str {
        match self {
            Grouping::Parentheses => "(",
            Grouping::Brackets => "[",
            Grouping::Braces => "{",
            Grouping::Block => "<open block>",
        }
    }

    pub fn close_str(self) -> &'static str {
        match self {
            Grouping::Parentheses => ")",
            Grouping::Brackets => "]",
            Grouping::Braces => "}",
            Grouping::Block => "<close block>",
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    And,
    Break,
    Case,
    Continue,
    Data,
    Do,
    Else,
    Embody,
    Forall,
    From,
    If,
    Let,
    Loop,
    Not,
    Of,
    Or,
    Private,
    Public,
    Return,
    Then,
    Trait,
    Type,
    Use,
    Where,
    While,
}

impl Keyword {
    pub fn as_str(self) -> &'static str {
        use Keyword as KW;

        match self {
            KW::And => "and",
            KW::Break => "break",
            KW::Case => "case",
            KW::Continue => "continue",
            KW::Data => "data",
            KW::Do => "do",
            KW::Else => "else",
            KW::Embody => "embody",
            KW::Forall => "forall",
            KW::From => "from",
            KW::If => "if",
            KW::Let => "let",
            KW::Loop => "loop",
            KW::Not => "not",
            KW::Of => "of",
            KW::Or => "or",
            KW::Private => "private",
            KW::Public => "public",
            KW::Return => "return",
            KW::Then => "then",
            KW::Trait => "trait",
            KW::Type => "type",
            KW::Use => "use",
            KW::Where => "where",
            KW::While => "while",
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;

    use super::SrcToken;
    use crate::lex::{Grouping, Token};

    #[test]
    fn empty() {
        test("", "")
    }

    #[test]
    fn only_whitespace() {
        test("    ", "")
    }

    #[test]
    fn blank_lines() {
        test("\n\n", "")
    }

    #[test]
    fn no_indent() {
        test(
            indoc!(
                r#"
                    x
                    y
                    z
                "#
            ),
            "x , y , z",
        )
    }

    #[test]
    fn no_top_level() {
        test(
            r#"
                    x
                    y
                    z
                "#,
            "<< x , y , z >>",
        )
    }

    #[test]
    fn indent_dedent() {
        test(
            indoc!(
                r#"
                    x
                    y
                    z
                        a
                        b
                        c
                    p
                    q
                    r
                "#
            ),
            "x , y , z << a , b , c >> p , q , r",
        );
    }

    #[test]
    fn indent_without_dedent() {
        test(
            indoc!(
                r#"
                    x
                    y
                    z
                        a
                        b
                        c
                "#
            ),
            "x , y , z << a , b , c >>",
        );
    }

    #[test]
    fn line_continuation() {
        test(
            indoc!(
                r#"
                    x
                    y
                    z
                        + a
                        + b
                        + c
                "#
            ),
            "x , y , z + a + b + c",
        );
    }

    #[test]
    fn nested_blocks() {
        test(
            indoc!(
                r#"
                    x
                        y
                            z
                                a
                        b
                            c
                "#
            ),
            "x << y << z << a >> >> b << c >> >>",
        );
    }

    #[test]
    fn mismatched_dedent() {
        test(
            indoc!(
                r#"
                    a
                            b
                        c
                "#
            ),
            "a << b <error> c >>",
        );
    }

    #[test]
    fn mismatched_double_dedent() {
        test(
            indoc!(
                r#"
                    a
                        b
                                c
                                    d
                            e
                "#
            ),
            "a << b << c << d <error> e >> >>",
        );
    }

    #[test]
    fn mismatched_double_dedent_at_end() {
        test(
            indoc!(
                r#"
                    a
                            b
                                c
                        d
                "#
            ),
            "a << b << c <error> d >>",
        );
    }

    fn test(src: &str, expected: &str) {
        print!("Source:\n\n{src}\n");
        let result = unlex(Token::layout(src), src);

        assert_eq!(expected, result);
    }

    fn unlex<'a>(layout: impl Iterator<Item = SrcToken<'a>>, src: &str) -> String {
        let layout: Vec<_> = layout.collect();
        println!("Tokens: {layout:?}");
        layout
            .into_iter()
            .map(|(token, span)| match token {
                Token::Open(Grouping::Block) => "<<",
                Token::Close(Grouping::Block) => ">>",
                Token::LineEnd => ",",
                Token::Error(_) => "<error>",
                _ => &src[span.into_range()],
            })
            .join(" ")
    }
}
