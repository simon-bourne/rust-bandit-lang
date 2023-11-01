use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
};

use chumsky::span::SimpleSpan;
use logos::{Filter, Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Eq)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(subpattern ident = r"(\p{XID_Start}|_)\p{XID_Continue}*")]
enum FlatToken {
    #[token("do", |_| BlockType::Do)]
    #[token("else", |_| BlockType::Else)]
    #[token("loop", |_| BlockType::Loop)]
    #[token("match", |_| BlockType::Match)]
    #[token("record", |_| BlockType::Record)]
    #[token("then", |_| BlockType::Then)]
    #[token("where", |_| BlockType::Where)]
    Block(BlockType),

    #[token("(", |_| Delimiter::Parentheses)]
    #[token("[", |_| Delimiter::Brackets)]
    #[token("{", |_| Delimiter::Braces)]
    Open(Delimiter),

    #[token(")", |_| Delimiter::Parentheses)]
    #[token("]", |_| Delimiter::Brackets)]
    #[token("}", |_| Delimiter::Braces)]
    Close(Delimiter),

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

    #[regex(r"\n[ \t]*", indentation)]
    Indentation,

    Error,
}

fn indentation(token: &mut Lexer<FlatToken>) -> Filter<()> {
    // It's really convenient to check the next token here, as the source is a
    // slice. Any work done here also slows down the lexer, so we do as little as
    // possible, and build the `Indent` later.
    if token.remainder().starts_with(['\n', '\r', '\x0C']) {
        Filter::Skip
    } else {
        Filter::Emit(())
    }
}

#[derive(Copy, Clone, Default, Debug, Eq, PartialEq, PartialOrd, Ord)]
struct IndentToken {
    width: usize,
}

impl IndentToken {
    fn from_source(source: &str, span: Span) -> Self {
        let token = source.get(span.into_range()).unwrap();

        let width = if token.contains('\t') {
            // TODO: Add an error saying only spaces are allowed for indents
            let tabs = token.matches('\t').count();
            let spaces = token.matches(' ').count();

            4 * tabs + spaces
        } else {
            token.len() - 1
        };

        Self { width }
    }
}

impl FlatToken {
    fn tokens(source: &str) -> impl Iterator<Item = Spanned<Self>> + '_ {
        let is_empty_indent =
            |token: &Spanned<Self>| IndentToken::from_source(source, token.1).width == 0;

        Self::lexer(source)
            .spanned()
            .map(|(tok, span)| match tok {
                Ok(tok) => (tok, Span::from(span)),
                Err(()) => (Self::Error, span.into()),
            })
            .skip_while(is_empty_indent)
    }

    fn indent_type(&self) -> IndentType {
        match self {
            Self::Block(_) => IndentType::Block,
            _ => IndentType::LineContinuation,
        }
    }
}

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Token {
    Block(BlockType),

    Open(Delimiter),
    Close(Delimiter),

    LineStart,

    Keyword(Keyword),

    Lambda,

    Identifier,
    Lifetime,
    Operator,

    Error,
}

impl Token {
    pub fn tokens(source: &str) -> impl Iterator<Item = Spanned<Self>> + '_ {
        TokenIter::new(source, FlatToken::tokens(source))
    }
}

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
    Indent,
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

#[derive(Copy, Clone, Eq, PartialEq)]
enum IndentType {
    Block,
    LineContinuation,
}

#[derive(Copy, Clone)]
struct Indent {
    typ: IndentType,
    indent: IndentToken,
}

impl Indent {
    fn new(typ: IndentType, indent: IndentToken) -> Self {
        Self { typ, indent }
    }
}

struct TokenIter<'src, I> {
    source: &'src str,
    flat_iter: I,
    indent_type: IndentType,
    indent_stack: Vec<Indent>,
    current_indent: Spanned<Indent>,
}

impl<'src, I> TokenIter<'src, I>
where
    I: Iterator<Item = Spanned<FlatToken>>,
{
    fn new(source: &'src str, flat_iter: I) -> Self {
        Self {
            source,
            flat_iter,
            indent_type: IndentType::Block,
            indent_stack: Vec::new(),
            current_indent: (
                Indent::new(IndentType::Block, IndentToken::default()),
                Span::new(0, 0),
            ),
        }
    }

    fn close_dangling_blocks(&mut self) -> Option<Spanned<Token>> {
        // TODO: Check current indent is consistent with stack
        while let Some(top) = self.indent_stack.last() {
            let (current_indent, current_indent_span) = self.current_indent;

            if current_indent.indent > top.indent {
                break;
            }

            let indent_type = current_indent.typ;
            self.current_indent.0 = *top;
            self.indent_stack.pop();

            if indent_type == IndentType::Block {
                return Some((Token::Close(Delimiter::Indent), current_indent_span));
            }
        }

        None
    }

    fn close_final_blocks(&mut self) -> Option<Spanned<Token>> {
        // TODO: What about current indent? Need to return a close for current indent,
        // then set current indent to the top of the stack. Also need to put spans on
        // the indent stack.
        while let Some(last_indent) = self.indent_stack.pop() {
            if last_indent.typ == IndentType::Block {
                let source_len = self.source.len();

                return Some((
                    Token::Close(Delimiter::Indent),
                    Span::new(source_len, source_len),
                ));
            }
        }

        None
    }

    fn handle_indent(
        &mut self,
        last_indent_type: IndentType,
        span: Span,
    ) -> Option<Spanned<Token>> {
        let indent = IndentToken::from_source(self.source, span);
        let new_indent = Indent::new(last_indent_type, indent);

        let current_indent = self.current_indent.0;
        let item = match current_indent.indent.cmp(&new_indent.indent) {
            Ordering::Less => {
                self.current_indent = (new_indent, span);
                self.indent_stack.push(current_indent);

                if new_indent.typ == IndentType::LineContinuation {
                    return self.next();
                }

                Token::Open(Delimiter::Indent)
            }
            Ordering::Equal => Token::LineStart,
            Ordering::Greater => {
                self.current_indent = (self.indent_stack.pop().unwrap(), span);

                match current_indent.typ {
                    IndentType::Block => Token::Close(Delimiter::Indent),
                    IndentType::LineContinuation => Token::LineStart,
                }
            }
        };

        Some((item, span))
    }
}

impl<'src, I> Iterator for TokenIter<'src, I>
where
    I: Iterator<Item = Spanned<FlatToken>>,
{
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(value) = self.close_dangling_blocks() {
            return Some(value);
        }

        let Some((token, span)) = self.flat_iter.next() else {
            // Clean up when we run out of tokens.
            return self.close_final_blocks();
        };

        let last_indent_type = self.indent_type;
        self.indent_type = token.indent_type();

        use FlatToken as FT;
        use Token as T;

        let token = match token {
            FT::Block(block_type) => T::Block(block_type),
            FT::Open(delimiter) => T::Open(delimiter),
            FT::Close(delimiter) => T::Close(delimiter),
            FT::Keyword(keyword) => T::Keyword(keyword),
            FT::Lambda => T::Lambda,
            FT::Identifier => T::Identifier,
            FT::Lifetime => T::Lifetime,
            FT::Operator => T::Operator,
            FT::Indentation => return self.handle_indent(last_indent_type, span),
            FT::Error => T::Error,
        };

        Some((token, span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let source = r#"
        expr
        expr
        do
            (expr expr)
                expr
            expr expr (do
                    expr expr
                    expr
                expr)
            do
                    expr
                do
                    expr
        exprexpr
        expr do expr expr do
            expr
            expr
        "#;

        let mut indent = 0;

        let print_indent = |indent| {
            println!();

            for _i in 0..indent {
                print!("    ")
            }
        };

        for (token, _span) in Token::tokens(source) {
            match token {
                Token::Block(_) => print!("Block "),
                Token::Open(Delimiter::Indent) => {
                    indent += 1;
                    print_indent(indent);
                }
                Token::Close(Delimiter::Indent) => {
                    indent -= 1;
                    print_indent(indent);
                }
                Token::Open(_) => print!("Open "),
                Token::Close(_) => print!("Close "),
                Token::LineStart => {
                    print_indent(indent);
                    print!(":");
                }
                Token::Keyword(_) => print!("Keyword "),
                Token::Lambda => print!("Lambda "),
                Token::Identifier => print!("Identifier "),
                Token::Lifetime => print!("Lifetime "),
                Token::Operator => print!("Operator "),
                Token::Error => print!("Error "),
            }
        }
    }
}
