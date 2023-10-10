# Markup

## Goals

- Similar to markdown, but:
  - more obvious and consistent syntax.
  - stricter. If there's an error it should be reported.
- Document looks good as plain text.
- Minimize escaping, especially in the free text outside `[]` and `{}`.
- Integration with programming languages. Code links and expressions.

## Escaping

- Any character can be escaped with `\`
- ``[]{}`\`` characters must always be escaped to include them in the text.
- `#>` characters must be escaped at the start of a paragraph.
- `-` must be escaped at the start of a line.
- Styling characters (`/*=^~+-"'`) must be escaped if they're the first character in styled text.
- Link special characters (`#^!`) must be escaped directly after `[`. `:` must be escaped before `]`. `()` must be escaped inside `[]`.
- Some [punctuation](#punctuation) combinations must be escaped.
- Line breaks can be escaped to produce hard line breaks (`<br>`), and space can be escaped for a non-breaking space.

## Links

URL is URL encoded when rendering. Markup special characters should be escaped with `\`.

- `[(URL)]` is a link to `URL`.
- `[title(URL)]` is a link to `URL` with an explicit title.

### Heading Links

- `[#heading]` is a link to a heading.
- `[title[#heading]]` is a link to a heading with an explicit title.

### Reference Links

- `[name]` is a reference link.
- `[title[name]]` is a reference link with an explicit title.
- `[name:]` is a reference link definition, and must be the start of a paragraph.

### Code Links

- ``[`item`]`` is a link to a code item. It is looked up in code reference links first.
- ``[`name`:]`` is a code reference link definition, and must be the start of a paragraph. It specifies the path to `name`.
- ``[`item`[qualified.path]]`` is a qualified link to a code item.

### Footnotes

- `[^footnote]` is a footnode link.
- `[^footnote:]` is a footnote definition, and must be the start of a paragraph.

### Inline images

- `[!(URL)]`
- `[!title(URL)]`
- `[!name]`
- `[!title[name]]`

## Code Fences

Start with 3 or more backticks and end with the same number.

## Text style

- `{/italic/}`
- `{*bold*}`
- `{=highlight=}`
- `{^superscript^}`
- `{~subscript~}`
- `{+ins+}`
- `{-del-}`
- `{"curly double quotes"}`
- `{'curly single quotes'}`

Text style can be nested and can contain links. As a shortcut, nesting can be done with `{/*bold italic*/}` for example. Styles can't be empty.

## Spans and Divs

`{{text}}` renders a `<span>`.
`{{` and `}}` on their own separate line renders a `<div>`. It can contain many paragraphs.

## Lists

Unordered lists use `-`. Ordered lists use `-#`. Definition lists start with `-:`. All must be followed by at least 1 space. Use `[x]` for task lists.

## Block Quotes

Block quotes start lines with `>` and at least 1 space, and must be in their own paragraph.

## Embedded Languages

Embedded languages can be specified after a code fence with `` `expression`{language_name}`` to evaluate the expression and put the result inline. For example, `` `x^2`{math}``.

Supported languages are:

- `math` for inline KaTeX.
- `Math` for display KaTeX.
- `html` for HTML.
- `bandit` for Bandit.

Some languages support applying an expression to a block. This is done with `` `expression`{@language_name}``. The expression is applied to the block if at the start of a block, otherwise the preceding block.

- `@html` for HTML opening tags applied to the block. For example, `` `<div><p>`{@html}`` would surround the block with `<div><p>block</p></div>`.
- `@bandit` for a Bandit expression applied to the block.

Bandit expressions can use anything public in a `doc` module in the current module.

## Attributes

Attributes are supported via an embedded `attr` language. For example, `` `attributes`{@attr}``.

## Named Entities

Some Emojis and HTML entities are supported, for example:

- `{smiley}` for a smiley.
- `{&copy;}` for the `&copy;` HTML entity.

## Punctuation

- `...` is an ellipsis. More than 3 consecutive dots is not interpreted as an ellipsis.
- `--` is an en-dash.
- `---` is an em-dash.
- `----` is a horizontal rule. It is only allowed in a paragraph on its own. More than 4 consecutive hyphens is not interpreted in any special way.
- `{--}`, `{---}`, `{----}` and `{...}` are escapes for the above.
- `{'}` is a curly apostrophe.

## Headings

A heading has an ID generated from the heading hierarchy. Headings can be linked with reference links beginning with a `#`. E.g. `[my link[#My Title/My Subtitle]]`. The main document title is excluded from the heading hierarchy, and doesn't have an id. `_` and `-` are escaped with a preceding `_`. Spaces are replaced with `-`, and any other non-alphanumeric characters are escaped with `_Unicode ID_`. See [MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id) for recommendations on ID's. Duplicate headings at the same level are not allowed. There must be 0 or 1 main headings. If included, it must be the first thing in the document, so doesn't need an anchor. ID attributes are checked for uniqueness with the headings.

## Backend

Renders to a defined subset of HTML + whatever embedded languages generate.
