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

struct TokenIter<'src, I> {
    source: &'src str,
    flat_iter: I,
    continue_on_indent: bool,
    indent_stack: Vec<&'src str>,
    current_indent: (&'src str, Span),
}

impl<'src, I> TokenIter<'src, I> {
    fn new(source: &'src str, flat_iter: I) -> Self {
        Self {
            source,
            flat_iter,
            continue_on_indent: false,
            indent_stack: Vec::new(),
            current_indent: ("\n", Span::new(0, 0)),
        }
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

        let (current_indent, current_indent_span) = self.current_indent;

        if Some(current_indent) <= self.indent_stack.last().copied() {
            self.indent_stack.pop();
            return Some((T::Close(Delimiter::Indent), current_indent_span));
        }

        let Some((token, span)) = self.flat_iter.next() else {
            if self.indent_stack.pop().is_some() {
                let source_len = self.source.len();

                return Some((
                    T::Close(Delimiter::Indent),
                    Span::new(source_len, source_len),
                ));
            }

            return None;
        };

        // TODO: This needs to go on the stack for continued lines with nested blocks.
        let continue_on_indent = self.continue_on_indent;
        self.continue_on_indent = true;

        let token = match token {
            FT::Block(block_type) => {
                self.continue_on_indent = false;
                T::Block(block_type)
            }
            FT::Open(delimiter) => T::Open(delimiter),
            FT::Close(delimiter) => T::Close(delimiter),
            FT::Keyword(keyword) => T::Keyword(keyword),
            FT::Lambda => T::Lambda,
            FT::Identifier => T::Identifier,
            FT::Lifetime => T::Lifetime,
            FT::Operator => T::Operator,
            FT::Indentation => {
                let this_indent = self.source.get(span.into_range()).unwrap();

                // TODO: Handle inconsistent indentation (add an `Indent` struct).
                match current_indent.cmp(this_indent) {
                    Ordering::Less => {
                        if continue_on_indent {
                            // TODO: Put this in a loop, rather than on the stack.
                            return self.next();
                        }

                        self.indent_stack.push(current_indent);
                        self.current_indent = (this_indent, span);
                        T::Open(Delimiter::Indent)
                    }
                    Ordering::Equal => T::LineStart,
                    Ordering::Greater => {
                        self.current_indent = (this_indent, span);
                        self.indent_stack.pop();
                        T::Close(Delimiter::Indent)
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
