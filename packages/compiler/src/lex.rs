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

    #[token("break", |_| Keyword::Break)]
    #[token("continue", |_| Keyword::Continue)]
    #[token("else", |_| Keyword::Else)]
    #[token("if", |_| Keyword::If)]
    #[token("match", |_| Keyword::Match)]
    #[token("loop", |_| Keyword::Loop)]
    #[token("return", |_| Keyword::Return)]
    #[token("while", |_| Keyword::While)]
    #[token("and", |_| Keyword::And)]
    #[token("not", |_| Keyword::Not)]
    #[token("or", |_| Keyword::Or)]
    #[token("data", |_| Keyword::Data)]
    #[token("do", |_| Keyword::Do)]
    #[token("embody", |_| Keyword::Embody)]
    #[token("forall", |_| Keyword::Forall)]
    #[token("infer", |_| Keyword::Infer)]
    #[token("let", |_| Keyword::Let)]
    #[token("move", |_| Keyword::Move)]
    #[token("mut", |_| Keyword::Mut)]
    #[token("record", |_| Keyword::Record)]
    #[token("require", |_| Keyword::Require)]
    #[token("self", |_| Keyword::SelfValue)]
    #[token("Self", |_| Keyword::SelfType)]
    #[token("then", |_| Keyword::Then)]
    #[token("trait", |_| Keyword::Trait)]
    #[token("use", |_| Keyword::Use)]
    #[token("with", |_| Keyword::With)]
    #[token("where", |_| Keyword::Where)]
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
    pub fn tokens(source: &'src str) -> impl Iterator<Item = SrcToken<'src>> + '_ {
        Self::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(()) => (Self::Error, span.into()),
        })
    }

    fn is_block_open(&self) -> bool {
        if let Token::Keyword(kw) = self {
            matches!(
                kw,
                Keyword::Else | Keyword::Loop | Keyword::Do | Keyword::Then | Keyword::Where
            )
        } else {
            false
        }
    }

    fn can_start_line(&self) -> bool {
        match self {
            Token::Close(_) => false,
            Token::Keyword(kw) => matches!(kw, Keyword::Loop),
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
            current_indent: Indent::Line(0),
            indent_stack: Vec::new(),
        }
    }

    fn open_block(&mut self) {
        self.indent_stack.push(self.current_indent);

        if let Some((Token::LineSeparator, span)) = self.iter.peek() {
            self.current_indent = Indent::line(self.src, *span);
            self.iter.next();
        } else {
            self.current_indent = Indent::SingleLine;
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
            }
            else {
                self.next()
            }
        };

        let span = token.1;
        let new_indent = if token.0 == Token::LineSeparator {
            Indent::line(self.src, span)
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
enum Indent {
    Line(usize),
    SingleLine,
}

impl Indent {
    const MIN: Self = Self::Line(0);

    fn line(src: &str, span: Span) -> Self {
        let s = &src[span.into_range()];
        Self::Line(s.rfind('\n').map(|pos| (s.len() - pos) - 1).unwrap_or(0))
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
    Break,
    Continue,
    Else,
    If,
    Match,
    Loop,
    Return,
    While,
    And,
    Not,
    Or,
    Data,
    Do,
    Embody,
    Forall,
    Infer,
    Let,
    Move,
    Mut,
    Record,
    Require,
    SelfType,
    SelfValue,
    Then,
    Trait,
    Use,
    With,
    Where,
}

impl Keyword {
    pub fn as_str(self) -> &'static str {
        use Keyword as KW;

        match self {
            KW::Break => "break",
            KW::Continue => "continue",
            KW::Else => "else",
            KW::If => "if",
            KW::Match => "match",
            KW::Loop => "loop",
            KW::Return => "return",
            KW::While => "while",
            KW::And => "and",
            KW::Not => "not",
            KW::Or => "or",
            KW::Data => "data",
            KW::Do => "do",
            KW::Embody => "embody",
            KW::Forall => "forall",
            KW::Infer => "infer",
            KW::Let => "let",
            KW::Move => "move",
            KW::Mut => "mut",
            KW::Record => "record",
            KW::Require => "require",
            KW::SelfType => "Self",
            KW::SelfValue => "self",
            KW::Then => "then",
            KW::Trait => "trait",
            KW::Use => "use",
            KW::With => "with",
            KW::Where => "where",
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
    }

    fn test(src: &str) {
        print!("Source:\n\n{src}\n\nTokens: ");
        let tokens = LayoutIter::new(Token::tokens(src), src);

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
