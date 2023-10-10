# Markup

## Goals

- Similar to markdown.
- Document looks good as plain text.
- Reduce number of special characters/escaping required.
- Integration with programming languages. Code links and expressions.

## Escaping

Any character can be escaped with `\`. Outside `[]` and `{}`, the characters ``[]()`\*`` have special meaning, and require escaping. `#` requires escaping at the start of a paragraph. `-` requires escaping at the start of a line. Line breaks can be escaped to produce hard line breaks (`<br>`), and space can be escaped for a non-breaking space.

## Links

URL is URL encoded when rendering. Markup special characters should be escaped with `\\`.

- `[(URL)]` is a link to `URL`.
- `[title(URL)]` is a link to `URL` with an explicit title.

### Heading Links

- `[#heading]` is a link to a heading.
- `[title[#heading]]` is a link to a heading with an explicit title.

### Reference Links

- `[name]` is a reference link.
- `[title[name]]` is a reference link with an explicit title.
- `[:name]` is a reference link definition, and must be the start of a paragraph.

## Code Links

- ``[`item`]`` is a link to a code item. It is looked up in code reference links first.
- ``[:`name`]`` is a code reference link definition, and must be the start of a paragraph. It specifies the path to `name`.
- ``[`item`[qualified.path]]`` is a qualified link to a code item.

## Footnotes

- `[^footnote]` is a footnode link.
- `[:^footnote]` is a footnote definition, and must be the start of a paragraph.

### Inline images

- `[!(URL)]`
- `[!title(URL)]`
- `[!name]`
- `[!title[name]]`

## Code Fences

Start with 3 or more backticks and end with the same number.

## Text style

- `{=highlight=}`
- `{^superscript^}`
- `{~subscript~}`
- `{+ins+}`
- `{-del-}`

Text style can be nested and contain links. If you want curly quotes, use Unicode.

`*` is used for emphasis. `**` and `***` are interpreted as double and triple emphasis respectively. Emphasis must be matched in the same paragraph, and non overlapping. Other uses of `*` must be escaped.

## Spans and Divs

`{{text}}` renders a `<span>`.
`{{` and `}}` on their own separate line renders a `<div>`. It can contain many paragraphs.

## Lists

Unordered lists use `-`. Ordered lists use `-#`. Definition lists start with `-:`. All must be followed by at least 1 space.

## Block Quotes

Block quotes start lines with `>` and at least 1 space, and must be in their own paragraph.

## Embedded Languages

- `{$KaTeX$}` will embed KaTeX in inline mode.
- `{$$KaTeX$$}` will embed KaTeX in display mode.
- `{|html|}` will embed HTML.
- ``{`bandit expression`}`` will embed a Bandit expression.
- `{@apply bandit expression@}` will apply a Bandit expression to the preceding item, or if at the start of a block, to the whole block.

Zero or more `#` characters can follow the language identifier (`$`, `|`, `` ` `` or `@`). The expression must be closed with the same number of `#`s. For example, `{$###KaTeX###$}`. This means the renderer doesn't need to parse the embedded language.

Bandit expressions can use anything public in a `doc` module in the current module.

## Attributes

`{attributes}`

Attributes at the start of a paragraph/list block apply to that block. Attributes following links, styling or spanned text apply to that element.

Attribute syntax is the same as djot, including comments.

## Headings

A heading has an ID generated from the heading hierarchy. Headings can be linked with reference links beginning with a `#`. E.g. `[my link[#My Title/My Subtitle]]`. The main document title is excluded from the heading hierarchy, and doesn't have an id. `_` and `-` are escaped with a preceding `_`. Spaces are replaced with `-`, and any other non-alphanumeric characters are escaped with `_Unicode ID_`. See [MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id) for recommendations on ID's. Duplicate headings at the same level are not allowed. There must be 0 or 1 main headings. If included, it must be the first thing in the document, so doesn't need an anchor. ID attributes are checked for uniqueness with the headings.

## Backend

Renders to a defined subset of HTML + whatever embedded languages generate.
