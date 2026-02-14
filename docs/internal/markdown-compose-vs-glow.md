# Markdown Compose Mode vs Glow: Rendering Comparison

Comparison of Fresh's markdown compose mode plugin (`plugins/markdown_compose.ts`)
against Glow 2.0 (charmbracelet/glow), a widely-used terminal markdown renderer
built on the Glamour library.

## Rendering Differences

| Feature | Fresh Compose | Glow |
|---------|--------------|------|
| **Headings h1** | `#` marker + cyan text | Bold yellow on purple background, `#` hidden |
| **Headings h2-h5** | `##`-`#####` markers + cyan text | Bold blue text, `##` markers shown |
| **Heading h6** | Same color as h2-h5 | Green (non-bold), distinct from h2-h5 |
| **Bold** | Blue text, `**` markers visible | SGR bold attribute (`[1m`), markers hidden |
| **Italic** | White text, `*` markers visible | SGR italic attribute (`[3m`), markers hidden |
| **Bold-italic** | White text, `***` markers visible | SGR bold+italic (`[1;3m`), markers hidden |
| **Strikethrough** | White text, `~~` markers visible | SGR strikethrough (`[9m`), markers hidden |
| **Inline code** | Green text, backticks visible | Pink on dark gray background, backticks hidden, padding added |
| **Links** | URL yellow, `[]()` syntax visible | Link text shown, URL shown separately, syntax hidden |
| **Reference links** | `[ref1]` label blue, syntax visible | Resolved to full URL inline |
| **Images** | Raw `![alt](path)` with colored path | `Image: alt text → url` format |
| **Unordered list bullets** | Raw `-`/`*`/`+` characters | Unicode `•` bullet |
| **List wrap indent** | Hanging indent under content start | Flush left (no hanging indent) |
| **Ordered list numbers** | Original numbers preserved (`10.`, `100.`) | Renumbered sequentially (`4.`, `5.`) |
| **Task list markers** | Raw `- [ ]`/`- [x]` | `[ ]`/`[✓]` with checkmark |
| **Blockquote marker** | `> ` on first line only, 2-space indent on continuation | `│` bar on every line including continuations |
| **Nested blockquotes** | `> > ` raw syntax | `│ │` nested Unicode bars |
| **Code block content** | Not wrapped by compose; editor soft-wrap handles overflow | Wrapped within block at width |
| **Code highlighting** | Tree-sitter (editor's highlighter) | Chroma library |
| **Horizontal rules** | Raw `---`/`***`/`___` preserved | Replaced with uniform `--------` |
| **Tables (narrow)** | Raw pipe `|` syntax | Box-drawing characters (`─`, `│`, `┼`) with column sizing |
| **Tables (wide)** | Rows wrap mid-cell, pipe structure breaks | Columns auto-sized to fit width |
| **Table alignment** | Raw `:---|:---:|---:` syntax | Actual left/center/right text alignment in cells |
| **Hard breaks** | Honored (both trailing spaces and `\`) | Honored |
| **Word wrap** | Configurable width via command palette | `-w WIDTH` flag |
| **Editability** | Source remains editable | Read-only viewer |

## Architectural Difference

Fresh compose mode is a soft-wrap transformation layer over editable source text,
so all markdown syntax remains visible and modifiable. Glow is a read-only
renderer that transforms markdown into a separate visual representation with
syntax markers removed and replaced by terminal formatting attributes and Unicode
characters.

## Other Terminal Markdown Renderers

| Tool | Language | Notable Traits |
|------|----------|----------------|
| **mdcat** | Rust | OSC 8 clickable hyperlinks, inline images (Kitty/iTerm2/Sixel), 4-bit ANSI only, no stylesheet customization |
| **Rich** | Python | OSC 8 hyperlinks, Panel-boxed h1, Pygments code highlighting, no task list support, headings centered |
| **bat** | Rust | Not a renderer — syntax-highlights raw markdown source, does not transform output |
| **mdless** | Ruby | Built-in pager, inline images via chafa/imgcat, themeable |
| **mcat** | Go | Multi-format viewer (images/video/PDF), Kitty/iTerm2/Sixel images, interactive zoom |
