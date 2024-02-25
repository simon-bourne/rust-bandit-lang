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

    #[token("\n")]
    LineEnd,

    #[token(";")]
    StatementEnd,

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
    pub fn tokens(source: &'src str) -> impl Iterator<Item = Spanned<Self>> + '_ {
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
