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
