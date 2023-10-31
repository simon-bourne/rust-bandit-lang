use std::cmp::Ordering;

use logos::{Filter, Lexer, Logos};

use crate::lex::{BlockType, Delimiter, Keyword, Span, Spanned};

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(subpattern ident = r"(\p{XID_Start}|_)\p{XID_Continue}*")]
pub enum FlatToken {
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
    if token.remainder().starts_with(['\n', '\r', '\x0C']) {
        Filter::Skip
    } else {
        Filter::Emit(())
    }
}

impl FlatToken {
    fn tokens(source: &str) -> impl Iterator<Item = Spanned<Self>> + '_ {
        Self::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(()) => (Self::Error, span.into()),
        })
    }

    fn indent_type(&self) -> IndentType {
        match self {
            Self::Block(_) => IndentType::Block,
            _ => IndentType::LineContinuation,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
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

#[derive(Copy, Clone, Eq, PartialEq)]
enum IndentType {
    Block,
    LineContinuation,
}

#[derive(Copy, Clone)]
struct Indent<'src> {
    typ: IndentType,
    indent: &'src str,
}

impl<'src> Indent<'src> {
    fn from_source(typ: IndentType, source: &'src str, span: Span) -> Self {
        let indent = source.get(span.into_range()).unwrap();

        Self { typ, indent }
    }

    fn block(indent: &'src str) -> Self {
        Self {
            typ: IndentType::Block,
            indent,
        }
    }
}

struct TokenIter<'src, I> {
    source: &'src str,
    flat_iter: I,
    indent_type: IndentType,
    indent_stack: Vec<Indent<'src>>,
    current_indent: (Indent<'src>, Span),
}

impl<'src, I> TokenIter<'src, I> {
    fn new(source: &'src str, flat_iter: I) -> Self {
        Self {
            source,
            flat_iter,
            indent_type: IndentType::Block,
            indent_stack: Vec::new(),
            current_indent: (Indent::block("\n"), Span::new(0, 0)),
        }
    }

    fn close_dangling_blocks(&mut self) -> Option<Spanned<Token>> {
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
}

impl<'src, I> Iterator for TokenIter<'src, I>
where
    I: Iterator<Item = Spanned<FlatToken>>,
{
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        use FlatToken as FT;
        use Token as T;

        if let Some(value) = self.close_dangling_blocks() {
            return Some(value);
        }

        // Clean up when we run out of tokens.
        let Some((token, span)) = self.flat_iter.next() else {
            return self.close_final_blocks();
        };

        let current_indent = self.current_indent.0;
        let last_indent_type = self.indent_type;
        self.indent_type = token.indent_type();

        let token = match token {
            FT::Block(block_type) => T::Block(block_type),
            FT::Open(delimiter) => T::Open(delimiter),
            FT::Close(delimiter) => T::Close(delimiter),
            FT::Keyword(keyword) => T::Keyword(keyword),
            FT::Lambda => T::Lambda,
            FT::Identifier => T::Identifier,
            FT::Lifetime => T::Lifetime,
            FT::Operator => T::Operator,
            FT::Indentation => {
                let new_indent = Indent::from_source(last_indent_type, self.source, span);

                // TODO: Handle inconsistent indentation (add an `Indent` struct).
                match current_indent.indent.cmp(new_indent.indent) {
                    Ordering::Less => {
                        self.current_indent = (new_indent, span);
                        self.indent_stack.push(current_indent);

                        if last_indent_type == IndentType::LineContinuation {
                            return self.next();
                        }

                        T::Open(Delimiter::Indent)
                    }
                    Ordering::Equal => T::LineStart,
                    Ordering::Greater => {
                        self.current_indent = (self.indent_stack.pop().unwrap(), span);

                        match current_indent.typ {
                            IndentType::Block => T::Close(Delimiter::Indent),
                            IndentType::LineContinuation => T::LineStart,
                        }
                    }
                }
            }
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
