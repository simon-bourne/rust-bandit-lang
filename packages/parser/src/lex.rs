use std::{cmp::Ordering, iter::Peekable};

use logos::{Logos, Span};

#[cfg(test)]
mod tests;

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
    Comma,

    #[regex(r"[ \t\r\f\n]*\n[ \t]*")]
    LineEnd,

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
    pub fn layout(source: &'src str) -> impl Iterator<Item = SrcToken<'src>> + 'src {
        let tokens = Self::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(()) => (Self::Error(Error::UnknownToken), span),
        });

        LayoutIter::new(ContinuedLines(tokens.peekable()), source)
    }

    fn continues_line(&self) -> bool {
        matches!(self, Token::Operator(_) | Token::NamedOperator(_))
    }
}

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
    Implies
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
            Self::Implies => "=>"
        }
    }
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

        if matches!(item.0, Token::LineEnd)
            && self.0.peek().is_none_or(|next| next.0.continues_line())
        {
            return self.0.next();
        }

        Some(item)
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
            last_span: Span { start: 0, end: 0 },
            current_indent: Indent::default(),
            indent_stack: Vec::new(),
        }
    }

    fn try_close_block(&mut self, span: Span) -> Option<SrcToken<'src>> {
        let top = self.indent_stack.last()?;

        match self.current_indent.cmp(top) {
            Ordering::Less => {
                let result = self.close_block(span.clone());

                if self
                    .indent_stack
                    .last()
                    .is_some_and(|top| self.current_indent > *top)
                {
                    // We skipped from `current_indent < top` to  `current_indent > top`, which
                    // means `current_indent` has no matching "block open".
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
        self.try_close_block(self.last_span.clone())
    }

    fn handle_indent(&mut self, span: Span) -> SrcToken<'src> {
        let new_indent = Indent::new(self.src, span.clone());

        match new_indent.cmp(&self.current_indent) {
            Ordering::Less => {
                self.current_indent = new_indent;
                self.try_close_block(span.clone())
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
        if let Some(token) = self.try_close_block(self.last_span.clone()) {
            return Some(token);
        }

        let Some(token) = self.iter.next() else {
            return self.finish();
        };

        self.last_span = token.1.clone();

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
            src[span]
                .rsplit_once('\n')
                .map(|(_head, tail)| tail.len())
                .unwrap_or(0),
        )
    }
}
