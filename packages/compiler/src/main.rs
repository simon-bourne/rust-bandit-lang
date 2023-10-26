use chumsky::{
    container::Container,
    primitive::{choice, just},
    recursive::recursive,
    text::{inline_whitespace, newline, whitespace},
    ConfigIterParser, IterParser, Parser,
};

#[derive(Default, Clone, Debug)]
struct Line<'src>(Vec<TreeToken<'src>>);

impl<'src> Line<'src> {
    fn is_continuation(&self) -> bool {
        self.0.first().is_some_and(|t| match t {
            TreeToken::Token(_) => false,
            TreeToken::Block(block_type, _) => match block_type {
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
}

#[derive(Default, Clone, Debug)]
struct Block<'src>(Vec<Line<'src>>);

impl<'src> Block<'src> {
    fn new(line: Line<'src>) -> Self {
        Self(vec![line])
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
enum BlockType {
    Do,
    Else,
    Match,
    Loop,
    Then,
    Record,
    Where,
    With,
}

#[derive(Clone, Debug)]
enum TreeToken<'src> {
    Token(&'src str),
    Block(BlockType, Block<'src>),
}

fn lexer<'a>() -> impl Parser<'a, &'a str, Block<'a>> {
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
    let token = just("expr").map(TreeToken::Token);

    let blank_lines = inline_whitespace().then(newline()).repeated();

    let block = recursive(|block| {
        let indent = just(' ')
            .repeated()
            .configure(|cfg, parent_indent| cfg.exactly(*parent_indent));
        let extra_indent = indent.then(inline_whitespace().at_least(1));
        let continue_line = newline()
            .then(blank_lines)
            .then(extra_indent)
            .repeated()
            .at_most(1);
        let token_separator = inline_whitespace().then(continue_line);
        let layout_block = open_layout_block
            .then_ignore(newline())
            .then(block)
            .map(|(block_type, block)| TreeToken::Block(block_type, block));
        let inline_block = recursive(|inline_block| {
            open_layout_block
                .then_ignore(inline_whitespace())
                .then(
                    choice((token, layout_block.clone(), inline_block))
                        .separated_by(inline_whitespace())
                        .collect()
                        .map(Line),
                )
                .map(|(block_type, line)| TreeToken::Block(block_type, Block::new(line)))
        });
        let line = token
            .or(inline_block)
            .or(layout_block)
            .separated_by(token_separator)
            .collect()
            .map(Line);
        let line_separator = newline().then(blank_lines).then(indent);

        whitespace()
            .count()
            .ignore_with_ctx(line.separated_by(line_separator).collect())
    });

    block.with_ctx(0)
}

fn main() {
    // TODO: Benchmarks
    // TODO: Tests
    // TODO: Pretty printer
    // TODO: Other tokens
    // TODO: Brackets

    let lines = lexer().padded().parse(
        r#"
expr
expr
do
    expr expr
     expr
    expr expr do
            expr expr
            expr
        expr
    do
            expr
        do
            expr
exprexpr
expr do expr expr do
    expr
    expr
"#,
    );
    println!("{:#?}", lines.output());
    println!("{:?}", lines.errors().collect::<Vec<_>>());
}
