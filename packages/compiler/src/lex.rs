use std::fmt::{self, Display, Formatter, Write};

use chumsky::{
    container::Container,
    input,
    prelude::Input,
    primitive::{choice, just, one_of},
    recursive::recursive,
    span::{self, SimpleSpan},
    text::{
        ascii::{self, ident},
        inline_whitespace, newline, whitespace,
    },
    ConfigIterParser, IterParser, Parser,
};

use crate::RichParser;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
pub type SpannedInput<'src, T> = input::SpannedInput<T, Span, &'src [(T, Span)]>;

#[derive(Default, Clone, Debug)]
pub struct Line<'src>(Vec<Spanned<TokenTree<'src>>>);

impl<'src> Line<'src> {
    pub fn spanned(&self) -> SpannedInput<TokenTree> {
        let end_of_input = if let Some((_, last_span)) = self.0.last() {
            span::Span::to_end(last_span)
        } else {
            SimpleSpan::new(0, 0)
        };

        self.0.spanned(end_of_input)
    }

    fn is_continuation(&self) -> bool {
        self.0.first().is_some_and(|t| match t.0 {
            TokenTree::Token(_) | TokenTree::Delimited(_, _) => false,
            TokenTree::Block(block_type, _) => match block_type {
                // We merge these block types so you can put the block start on a new line. None of
                // these block types can start a line.
                BlockType::Do
                | BlockType::Else
                | BlockType::Match
                | BlockType::Then
                | BlockType::Record
                | BlockType::Where
                | BlockType::With => true,
                // Loop can start a line, so it can't be merged.
                BlockType::Loop => false,
            },
        })
    }

    fn pretty(&self, indent: usize, f: &mut Formatter<'_>) -> fmt::Result {
        for token in &self.0 {
            token.0.pretty(indent, f)?;
        }

        Ok(())
    }
}

#[derive(Default, Clone, Debug)]
pub struct Block<'src>(Vec<Line<'src>>);

impl<'src> Block<'src> {
    fn new(line: Line<'src>) -> Self {
        Self(vec![line])
    }

    fn pretty(&self, indent: usize, f: &mut Formatter<'_>) -> fmt::Result {
        for line in &self.0 {
            pretty_indent(indent, f)?;
            line.pretty(indent + 1, f)?;
            f.write_char('\n')?;
        }

        Ok(())
    }
}

impl<'src> Display for Block<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.pretty(0, f)
    }
}

impl<'src> Container<Line<'src>> for Block<'src> {
    fn push(&mut self, line: Line<'src>) {
        if let Some(last) = self.0.last_mut() {
            if line.is_continuation() {
                last.0.extend(line.0);
                return;
            }
        }

        self.0.push(line)
    }

    fn with_capacity(n: usize) -> Self {
        Self(Vec::with_capacity(n))
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BlockType {
    Do,
    Else,
    Match,
    Loop,
    Then,
    Record,
    Where,
    With,
}

impl Display for BlockType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            BlockType::Do => "do",
            BlockType::Else => "else",
            BlockType::Match => "match",
            BlockType::Loop => "loop",
            BlockType::Then => "then",
            BlockType::Record => "record",
            BlockType::Where => "where",
            BlockType::With => "with",
        };

        f.write_str(s)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Delimiter {
    Parentheses,
    Brackets,
    Braces,
}

#[derive(Copy, Clone, Debug)]
pub enum Keyword {
    If,
    Return,
    While,
    Alias,
    Forall,
    Infer,
    Module,
    Let,
    SelfType,
    Trait,
    Type,
    Use,
}

impl AsRef<str> for Keyword {
    fn as_ref(&self) -> &str {
        use Keyword as K;

        match self {
            K::If => "if",
            K::Return => "return",
            K::While => "while",
            K::Alias => "alias",
            K::Forall => "forall",
            K::Infer => "infer",
            K::Module => "module",
            K::Let => "let",
            K::SelfType => "Self",
            K::Trait => "trait",
            K::Type => "type",
            K::Use => "use",
        }
    }
}

impl<'a> TryFrom<&'a str> for Keyword {
    type Error = ();

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        use Keyword as K;

        match value {
            "if" => Ok(K::If),
            "return" => Ok(K::Return),
            "while" => Ok(K::While),
            "alias" => Ok(K::Alias),
            "forall" => Ok(K::Forall),
            "infer" => Ok(K::Infer),
            "module" => Ok(K::Module),
            "let" => Ok(K::Let),
            "Self" => Ok(K::SelfType),
            "trait" => Ok(K::Trait),
            "type" => Ok(K::Type),
            "use" => Ok(K::Use),
            _ => Err(()),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

#[derive(Clone, Debug)]
pub enum Token<'src> {
    Ident(&'src str),
    Keyword(Keyword),
    Lambda,
    Operator(&'src str),
    Lifetime(&'src str),
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(t) => write!(f, "id:{t}"),
            Token::Keyword(kw) => write!(f, "kw:{kw}"),
            Token::Lambda => f.write_str("lambda:\\"),
            Token::Operator(op) => write!(f, "op:{op}"),
            Token::Lifetime(lt) => write!(f, "'lt:{lt}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TokenTree<'src> {
    Token(Token<'src>),
    Block(BlockType, Block<'src>),
    Delimited(Delimiter, Line<'src>),
}

impl<'src> TokenTree<'src> {
    fn delimited(delimiter: Delimiter, line: Vec<Spanned<TokenTree<'src>>>) -> Self {
        Self::Delimited(delimiter, Line(line))
    }

    fn pretty(&self, indent: usize, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Token(t) => write!(f, "{t} "),
            TokenTree::Block(block_type, block) => {
                f.write_char('\n')?;
                pretty_indent(indent, f)?;
                writeln!(f, "{block_type}")?;
                block.pretty(indent + 1, f)?;
                pretty_indent(indent, f)
            }
            TokenTree::Delimited(delimiter, line) => match delimiter {
                Delimiter::Parentheses => write_brackets(f, indent, '(', ')', line),
                Delimiter::Brackets => write_brackets(f, indent, '[', ']', line),
                Delimiter::Braces => write_brackets(f, indent, '{', '}', line),
            },
        }
    }
}

fn write_brackets(
    f: &mut Formatter<'_>,
    indent: usize,
    open: char,
    close: char,
    line: &Line<'_>,
) -> fmt::Result {
    f.write_char(open)?;
    line.pretty(indent, f)?;
    f.write_char(close)
}

fn pretty_indent(indent: usize, f: &mut Formatter<'_>) -> fmt::Result {
    for _i in 0..indent {
        f.write_str("    ")?;
    }

    Ok(())
}

pub fn lexer<'src>() -> impl RichParser<'src, &'src str, Block<'src>> {
    let open_layout_block = choice((
        just("do").to(BlockType::Do),
        just("else").to(BlockType::Else),
        just("match").to(BlockType::Match),
        just("loop").to(BlockType::Loop),
        just("then").to(BlockType::Then),
        just("record").to(BlockType::Record),
        just("where").to(BlockType::Where),
        just("with").to(BlockType::With),
    ));
    let ident = ident().map(|ident: &str| {
        let token = match ident.try_into() {
            Ok(kw) => Token::Keyword(kw),
            Err(()) => Token::Ident(ident),
        };
        TokenTree::Token(token)
    });
    let lambda = just('\\').to(TokenTree::Token(Token::Lambda));
    let lifetime = just('\'')
        .ignore_then(ascii::ident())
        .map(|lt| TokenTree::Token(Token::Lifetime(lt)));
    let operator = one_of("$%&*+./<=>@^-~|")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|op| TokenTree::Token(Token::Operator(op)));

    let blank_lines = inline_whitespace().then(newline()).repeated();
    let indent = just(' ')
        .repeated()
        .configure(|cfg, parent_indent| cfg.exactly(*parent_indent));
    let token = choice((lambda, lifetime, operator, ident));
    let extra_indent = indent.then(inline_whitespace().at_least(1));
    let continue_line = newline()
        .then(blank_lines)
        .then(extra_indent)
        .repeated()
        .at_most(1);
    let token_separator = inline_whitespace().then(continue_line);
    let line_separator = newline().then(blank_lines).then(indent);

    let block = recursive(|block| {
        let layout_block = open_layout_block
            .then_ignore(newline())
            .then(block)
            .map(|(block_type, block)| TokenTree::Block(block_type, block));
        let inline_block = recursive(|inline_block| {
            open_layout_block
                .then_ignore(inline_whitespace())
                .then(
                    choice((layout_block.clone(), inline_block, token.clone()))
                        .map_with(|tt, extra| (tt, extra.span()))
                        .separated_by(inline_whitespace())
                        .at_least(1)
                        .collect()
                        .map(Line),
                )
                .map(|(block_type, line)| TokenTree::Block(block_type, Block::new(line)))
        });
        let token_tree = recursive(|token_tree| {
            let delimited = |open, close, delimiter| {
                token_tree
                    .clone()
                    .separated_by(whitespace())
                    .collect()
                    .delimited_by(just(open), just(close))
                    .map(move |line| TokenTree::delimited(delimiter, line))
            };
            choice((
                layout_block,
                inline_block,
                delimited('(', ')', Delimiter::Parentheses),
                delimited('[', ']', Delimiter::Brackets),
                delimited('{', '}', Delimiter::Braces),
                token,
            ))
            .map_with(|tt, extra| (tt, extra.span()))
        });
        let line = token_tree.separated_by(token_separator).collect().map(Line);

        whitespace()
            .count()
            .ignore_with_ctx(line.separated_by(line_separator).collect())
    });

    block.with_ctx(0)
}
