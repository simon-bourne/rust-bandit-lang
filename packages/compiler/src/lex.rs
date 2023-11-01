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
    #[token("forall", |_| Keyword::Forall)]
    #[token("if", |_| Keyword::If)]
    #[token("infer", |_| Keyword::Infer)]
    #[token("let", |_| Keyword::Let)]
    #[token("module", |_| Keyword::Module)]
    #[token("provide", |_| Keyword::Provide)]
    #[token("return", |_| Keyword::Return)]
    #[token("Self", |_| Keyword::SelfType)]
    #[token("trait", |_| Keyword::Trait)]
    #[token("type", |_| Keyword::Type)]
    #[token("use", |_| Keyword::Use)]
    #[token("while", |_| Keyword::While)]
    #[token("with", |_| Keyword::With)]
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
pub enum BlockType {
    Do,
    Else,
    Match,
    Loop,
    Then,
    Record,
    Where,
}

impl From<BlockType> for &'static str {
    fn from(value: BlockType) -> Self {
        match value {
            BlockType::Do => "do",
            BlockType::Else => "else",
            BlockType::Match => "match",
            BlockType::Loop => "loop",
            BlockType::Then => "then",
            BlockType::Record => "record",
            BlockType::Where => "where",
        }
    }
}

impl Display for BlockType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(<&str>::from(*self))
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Delimiter {
    Parentheses,
    Brackets,
    Braces,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    Alias,
    Forall,
    If,
    Infer,
    Let,
    Module,
    Provide,
    Return,
    SelfType,
    Trait,
    Type,
    Use,
    While,
    With,
}

impl From<Keyword> for &'static str {
    fn from(value: Keyword) -> Self {
        use Keyword as K;

        match value {
            K::Alias => "alias",
            K::Forall => "forall",
            K::If => "if",
            K::Infer => "infer",
            K::Let => "let",
            K::Module => "module",
            K::Provide => "provide",
            K::Return => "return",
            K::SelfType => "Self",
            K::Trait => "trait",
            K::Type => "type",
            K::Use => "use",
            K::While => "while",
            K::With => "with",
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(<&str>::from(*self))
    }
}
