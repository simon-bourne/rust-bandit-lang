use std::fmt::{self, Display, Formatter};

use chumsky::span::SimpleSpan;
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Copy, Clone)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(subpattern ident = r"(\p{XID_Start}|_)\p{XID_Continue}*")]
pub enum Token {
    #[token("(", |_| Delimiter::Parentheses)]
    #[token("[", |_| Delimiter::Brackets)]
    #[token("{", |_| Delimiter::Braces)]
    Open(Delimiter),

    #[token(")", |_| Delimiter::Parentheses)]
    #[token("]", |_| Delimiter::Brackets)]
    #[token("}", |_| Delimiter::Braces)]
    Close(Delimiter),

    #[token(";")]
    LineEnd,

    #[token("alias", |_| Keyword::Alias)]
    #[token("data", |_| Keyword::Data)]
    #[token("else", |_| Keyword::Else)]
    #[token("embody", |_| Keyword::Embody)]
    #[token("forall", |_| Keyword::Forall)]
    #[token("if", |_| Keyword::If)]
    #[token("infer", |_| Keyword::Infer)]
    #[token("let", |_| Keyword::Let)]
    #[token("loop", |_| Keyword::Loop)]
    #[token("match", |_| Keyword::Match)]
    #[token("module", |_| Keyword::Module)]
    #[token("return", |_| Keyword::Return)]
    #[token("Self", |_| Keyword::SelfType)]
    #[token("trait", |_| Keyword::Trait)]
    #[token("use", |_| Keyword::Use)]
    #[token("where", |_| Keyword::Where)]
    #[token("while", |_| Keyword::While)]
    Keyword(Keyword),

    #[token("\\")]
    Lambda,

    #[regex(r"(?&ident)")]
    Identifier,
    #[regex(r"'(?&ident)")]
    Lifetime,
    #[regex(r"\$%\&\*\+\./<=>@\^\-\~\|")]
    Operator,

    Error,
}

impl Token {
    pub fn tokens(source: &str) -> impl Iterator<Item = Spanned<Self>> + '_ {
        Self::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(()) => (Self::Error, span.into()),
        })
    }
}

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Delimiter {
    Parentheses,
    Brackets,
    Braces,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    Alias,
    Data,
    Else,
    Embody,
    Forall,
    If,
    Infer,
    Let,
    Loop,
    Match,
    Module,
    Return,
    SelfType,
    Trait,
    Use,
    Where,
    While,
}

impl From<Keyword> for &'static str {
    fn from(value: Keyword) -> Self {
        use Keyword as K;

        match value {
            K::Alias => "alias",
            K::Data => "data",
            K::Else => "else",
            K::Embody => "embody",
            K::Forall => "forall",
            K::If => "if",
            K::Infer => "infer",
            K::Let => "let",
            K::Loop => "loop",
            K::Match => "match",
            K::Module => "module",
            K::Return => "return",
            K::SelfType => "SelfType",
            K::Trait => "trait",
            K::Use => "use",
            K::Where => "where",
            K::While => "while",
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(<&str>::from(*self))
    }
}
