use std::{cmp::Ordering, iter::Peekable};

use chumsky::span::SimpleSpan;
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

    #[token("alias", |_| Keyword::Alias)]
    #[token("and", |_| Keyword::And)]
    #[token("break", |_| Keyword::Break)]
    #[token("continue", |_| Keyword::Continue)]
    #[token("data", |_| Keyword::Data)]
    #[token("do", |_| Keyword::Do)]
    #[token("else", |_| Keyword::Else)]
    #[regex("else[ \t]+if", |_| Keyword::ElseIf)]
    #[token("embody", |_| Keyword::Embody)]
    #[token("forall", |_| Keyword::Forall)]
    #[token("if", |_| Keyword::If)]
    #[token("let", |_| Keyword::Let)]
    #[token("loop", |_| Keyword::Loop)]
    #[token("match", |_| Keyword::Match)]
    #[token("move", |_| Keyword::Move)]
    #[token("not", |_| Keyword::Not)]
    #[token("or", |_| Keyword::Or)]
    #[token("private", |_| Keyword::Private)]
    #[token("public", |_| Keyword::Public)]
    #[token("record", |_| Keyword::Record)]
    #[token("return", |_| Keyword::Return)]
    #[token("scope", |_| Keyword::Scope)]
    #[token("self", |_| Keyword::SelfValue)]
    #[token("then", |_| Keyword::Then)]
    #[token("trait", |_| Keyword::Trait)]
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

// TODO: What can we make private in this module?
impl<'src> Token<'src> {
    pub fn tokens(source: &'src str) -> impl Iterator<Item = SrcToken<'src>> + '_ {
        Self::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(()) => (Self::Error, span.into()),
        })
    }

    pub fn layout(source: &'src str) -> impl Iterator<Item = SrcToken<'src>> + '_ {
        LayoutIter::new(Self::tokens(source), source)
    }

    fn is_block_open(&self) -> bool {
        if let Token::Keyword(kw) = self {
            matches!(
                kw,
                Keyword::Else
                    | Keyword::Loop
                    | Keyword::Scope
                    | Keyword::Do
                    | Keyword::Then
                    | Keyword::Where
            )
        } else {
            false
        }
    }

    fn can_start_line(&self) -> bool {
        match self {
            Token::Close(_) => false,
            Token::Keyword(kw) => matches!(kw, Keyword::Loop | Keyword::Scope),
            _ => true,
        }
    }
}

pub struct LayoutIter<'src, I: Iterator<Item = SrcToken<'src>>> {
    iter: Peekable<I>,
    src: &'src str,
    current_indent: Indent,
    indent_stack: Vec<Indent>,
}

impl<'src, I: Iterator<Item = SrcToken<'src>>> LayoutIter<'src, I> {
    pub fn new(iter: I, src: &'src str) -> Self {
        Self {
            iter: iter.peekable(),
            src,
            current_indent: Indent::MIN,
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

    fn close_block(&mut self, new_indent: Indent) {
        self.current_indent = new_indent;
    }
}

impl<'src, I> Iterator for LayoutIter<'src, I>
where
    I: Iterator<Item = SrcToken<'src>>,
{
    type Item = SrcToken<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        if Some(&self.current_indent) <= self.indent_stack.last() {
            self.indent_stack.pop();
            return Some((Token::CloseBlock, Span::splat(self.src.len())));
        }

        let Some(token) = self.iter.next() else {
            self.current_indent = Indent::MIN;

            return if self.indent_stack.is_empty() {
                None
            } else {
                self.next()
            };
        };

        let new_indent = if token.0 == Token::LineSeparator {
            Indent::new_line(self.src, token.1)
        } else {
            if token.0.is_block_open() {
                self.open_block();
            }
            return Some(token);
        };

        match new_indent.cmp(&self.current_indent) {
            Ordering::Less => {
                self.close_block(new_indent);
                self.next()
            }
            Ordering::Equal => match self.iter.peek() {
                Some(next_token) if next_token.0.can_start_line() => Some(token),
                _ => self.next(),
            },
            Ordering::Greater => self.next(),
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
    Alias,
    And,
    Break,
    Continue,
    Data,
    Do,
    Else,
    ElseIf,
    Embody,
    Forall,
    If,
    Let,
    Loop,
    Match,
    Move,
    Not,
    Or,
    Private,
    Public,
    Record,
    Return,
    Scope,
    SelfValue,
    Then,
    Trait,
    Use,
    Where,
    While,
    With,
}

impl Keyword {
    pub fn as_str(self) -> &'static str {
        use Keyword as KW;

        match self {
            KW::Alias => "alias",
            KW::And => "and",
            KW::Break => "break",
            KW::Continue => "continue",
            KW::Data => "data",
            KW::Do => "do",
            KW::Else => "else",
            KW::ElseIf => "else if",
            KW::Embody => "embody",
            KW::Forall => "forall",
            KW::If => "if",
            KW::Let => "let",
            KW::Loop => "loop",
            KW::Match => "match",
            KW::Move => "move",
            KW::Not => "not",
            KW::Or => "or",
            KW::Private => "private",
            KW::Public => "public",
            KW::Record => "record",
            KW::Return => "return",
            KW::Scope => "scope",
            KW::SelfValue => "self",
            KW::Then => "then",
            KW::Trait => "trait",
            KW::Use => "use",
            KW::Where => "where",
            KW::While => "while",
            KW::With => "with",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: Proper tests
    #[test]
    fn layout() {
        test(
            r#"
if a then
    x
else
    y
"#,
        );
        test(
            r#"
if a then
    x
else if b then
    y
else
    z
"#,
        );
        test(
            r#"
if a then x
else y
"#,
        );
        test(
            r#"
if a then if b then if c then
    x
else
    y(
        a
        b
    )
    c
"#,
        );
    }

    fn test(src: &str) {
        print!("Source:\n\n{src}\n\nTokens: ");
        let tokens = Token::layout(src);

        for (token, span) in tokens {
            let s = match token {
                Token::CloseBlock => ";",
                Token::LineSeparator => ",",
                _ => &src[span.into_range()],
            };

            print!("{s} ");
        }

        println!("\n");
    }
}
