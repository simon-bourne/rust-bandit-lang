use logos::{Logos, Span};

#[derive(Logos, Debug, PartialEq, Eq, Copy, Clone)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(subpattern ident = r"(\p{XID_Start}\p{XID_Continue}*)|(_\p{XID_Continue}+)")]
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
    #[regex(r"[ \t\r\f\n]*\n[ \t]*")]
    LineEnd,

    #[token(";")]
    BlockEnd,

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
    #[token("of", |_| Keyword::Of)]
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

    /// We don't allow `λ` as an alternative because it's allowed in
    /// identifiers. It doesn't make sense as a keyword, as we'd have to have a
    /// space between it and a variable name. For example, does `λx` mean a
    /// lambda that binds the variable `x`, or an identifier `λx`?
    #[token("\\")]
    Lambda,

    #[regex(r"(?&ident)")]
    Identifier(&'src str),

    #[token("_")]
    Unknown,

    // ASCII operators
    #[token("@", |_| Operator::StaticApply)]
    #[token("and", |_| Operator::And)]
    #[token("or", |_| Operator::Or)]
    #[token("not", |_| Operator::Not)]
    #[token("+", |_| Operator::Plus)]
    #[token("-", |_| Operator::Minus)]
    #[token("*", |_| Operator::Multiply)]
    #[token("/", |_| Operator::Divide)]
    #[token("==", |_| Operator::Equal)]
    #[token("/=", |_| Operator::NotEqual)]
    #[token("<", |_| Operator::LessThan)]
    #[token(">", |_| Operator::GreaterThan)]
    #[token("<=", |_| Operator::LessOrEqual)]
    #[token(">=", |_| Operator::GreaterOrEqual)]
    #[token("=", |_| Operator::Assign)]
    #[token(":", |_| Operator::HasType)]
    #[token("->", |_| Operator::To)]
    #[token("=>", |_| Operator::Implies)]
    // Unicode operators
    #[token("≠", |_| Operator::NotEqual)]
    #[token("≤", |_| Operator::LessOrEqual)]
    #[token("≥", |_| Operator::GreaterOrEqual)]
    #[token("→", |_| Operator::To)]
    #[token("⇒", |_| Operator::Implies)]
    Operator(Operator),

    #[regex(r"\.(?&ident)")]
    NamedOperator(&'src str),

    Error(Error),
}

impl<'src> Token<'src> {
    pub fn layout(source: &'src str) -> Vec<SrcToken<'src>> {
        let tokens = Self::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(()) => (Self::Error(Error::UnknownToken), span),
        });

        let mut layout_tokens = Vec::new();

        for token in tokens {
            match token.0 {
                Token::Operator(_) | Token::NamedOperator(_) | Token::BlockEnd => {
                    remove_trailing_line_ends(&mut layout_tokens)
                }
                _ => (),
            }

            layout_tokens.push(token);
        }

        remove_trailing_line_ends(&mut layout_tokens);

        layout_tokens
    }
}

fn remove_trailing_line_ends(layout_tokens: &mut Vec<(Token<'_>, std::ops::Range<usize>)>) {
    while let Some((Token::LineEnd, _)) = layout_tokens.last() {
        layout_tokens.pop();
    }
}

pub type Spanned<T> = (T, Span);
pub type SrcToken<'src> = Spanned<Token<'src>>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Grouping {
    Parentheses,
    Brackets,
    Braces,
}

impl Grouping {
    pub fn open_str(self) -> &'static str {
        match self {
            Grouping::Parentheses => "(",
            Grouping::Brackets => "[",
            Grouping::Braces => "{",
        }
    }

    pub fn close_str(self) -> &'static str {
        match self {
            Grouping::Parentheses => ")",
            Grouping::Brackets => "]",
            Grouping::Braces => "}",
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
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
    Of,
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
            KW::Of => "of",
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Operator {
    And,
    Or,
    Not,

    Plus,
    Minus,
    Multiply,
    Divide,

    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,

    Apply,
    StaticApply,
    Assign,
    HasType,
    To,
    Implies,
}

impl Operator {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::And => "and",
            Self::Or => "or",
            Self::Not => "not",

            Self::Plus => "+",
            Self::Minus => "-",
            Self::Multiply => "*",
            Self::Divide => "/",

            Self::Equal => "==",
            Self::NotEqual => "/=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessOrEqual => "<=",
            Self::GreaterOrEqual => ">=",

            Self::HasType => ":",
            Self::Assign => "=",
            Self::Apply => "<-",
            Self::StaticApply => "@",
            Self::To => "->",
            Self::Implies => "=>",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Error {
    UnknownToken,
    Dedent,
}
