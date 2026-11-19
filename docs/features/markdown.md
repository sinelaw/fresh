# Markdown

Fresh edits Markdown as source, and can also show it as a rendered page — with Mermaid diagrams and math drawn in place — or both side by side. All of it comes from built-in plugins that are on by default.

- **[Smart editing](#smart-editing)**: lists continue and indent as you type.
- **[Compose mode](#compose-mode)**: a distraction-free, rendered view of the file, still editable.
- **[Dual mode](#source-compose-and-dual-mode)**: source and rendered view side by side in one tab, scrolling together.
- **[Diagrams and math](#diagrams-and-math)**: Mermaid, LaTeX and any other code-block language drawn by an external command you choose.
- **[Contents panel](#contents-panel)**: a heading outline in the sidebar.

## Smart Editing

Smart editing for Markdown files (provided by the built-in `markdown_source` plugin, enabled by default):

- Enter continues list items: the same bullet, the next number (`9.` → `10.`), or a new unchecked checkbox, at the same indentation
- Enter on an empty list marker removes it
- Enter on any other indented line keeps its indentation
- Tab on an empty bullet or checkbox item indents it one level (your `tab_size` / `use_tabs`) and cycles the bullet `*` → `-` → `+`; Shift+Tab takes the level back off and cycles the other way. Elsewhere Tab and Shift+Tab work as usual, and Tab with a selection indents the selected lines
- Single-quote auto-close is disabled so apostrophes don't interfere

It applies at every cursor, keeps up with typing however fast (or a paste of keystrokes), and each Enter or Tab is one undo step. It works the same in vi mode's insert mode and in compose mode. It applies to buffers whose language is Markdown (`.md`, `.markdown`, `README`); an `.mdx` file is plain text unless you add `mdx` to `languages.markdown.extensions` (or define an `mdx` language). To turn it off, disable the `markdown_source` plugin.

## Compose Mode

"Markdown: Toggle Compose/Preview" from the command palette turns the current file into a distraction-free, preview-style page: markup is hidden and headings, lists, quotes, code blocks (highlighted in their own language) and tables are styled, in a centered column; adjust its width with "Markdown: Set Compose Width" (or **Page Width** in Settings, 80 columns by default). Headings are also marked on the scrollbar. "Markdown: Toggle Compose/Preview (All Files)" does the same for every open and future Markdown file.

Compose mode reads the document the way markdown defines it, so a paragraph, list item or quote written across several source lines is re-flowed into one block at the page width — a hard-wrapped file reads as prose rather than as its source layout. The things markdown treats as their own block still are: a blank line, a heading, a thematic break, a table row, a fenced code block, a new list marker, and a hard break (two trailing spaces or a trailing backslash).

## Source, Compose and Dual Mode

Three commands switch the pane in focus between views of a Markdown file:

| Command | View |
|---------|------|
| Markdown: Source Mode | The raw Markdown |
| Markdown: Rendered Mode | The composed document |
| Markdown: Source/Compose Dual Mode | The source and the composed document side by side, in the file's tab; the two scroll together |

"Markdown: Cycle View Mode" steps through Source, Rendered and dual mode, and "Markdown: Toggle Source/Compose Dual Mode" switches dual mode on and off. Dual mode shows in the file's own tab, so the file still has one tab, and you can edit it in either side. Leaving dual mode brings the tab back to the plain file; closing the tab closes the file, as usual. Other panes you have open on the file are left alone.

## Diagrams and Math

Compose mode can show a code block as a drawing instead of its source. It doesn't draw anything itself: the `renderers` setting maps a code block's language to a command and its arguments, and compose shows what that command prints, framed, in place of the block. The frame's title names the language and the program, such as `mermaid · termaid`.

![A Markdown file in compose mode: inline math, a formula drawn by utftex, and a Mermaid flowchart drawn by termaid](/images/markdown-preview-rendered.png)

The defaults cover Mermaid diagrams and LaTeX math. Install the tool for what you want drawn; Fresh finds it on your `PATH`:

| Language | Command | Install |
|---|---|---|
| `mermaid` | `termaid {file} --width {width}` | [`termaid`](https://github.com/fasouto/termaid): `pip install termaid` |
| `math`, `latex`, `katex` | `utftex --line-width {width} -- {source}` | [`utftex`](https://github.com/bartp5/libtexprintf) |

The `math` entry also draws math written in the delimiters MathJax, KaTeX, GitHub and GitLab use:
- inline: `$x^2$`, `\(x^2\)`, or GitLab's ``$`x^2`$``;
- display: `$$ … $$`, or `\[ … \]` (on one line, or with `\[` and `\]` on lines of their own).

Inline math is replaced only when the command draws it on one row, since a stacked fraction can't sit inside a sentence. Prices such as "$10 and $20" are not taken for math.

When a language has no command, the command isn't installed, or it fails on a block, the source is shown as written: a fenced block as a code block, display math as its lines. The first time a missing command is needed, the status bar says what to install.

Put the cursor in a drawn block to edit its source; it is drawn again when the cursor leaves. In Source/Compose dual mode, the source side shows the Markdown and the compose side the drawings.

![Source/Compose dual mode: the Markdown source on the left, the drawn formula and diagram on the right](/images/markdown-preview-dual.png)

### Configuring the renderers

Change the commands in Settings, under *Plugins → markdown_compose → Renderers*. There is a row per language, showing its command:

![The Renderers setting in the Settings UI: a row per language with its command](/images/markdown-preview-settings.png)

A row opens to edit the command and its arguments. The help under the fields lists the placeholders the arguments can use:

![Editing the mermaid entry: its command, its arguments as a list, and the placeholders help](/images/markdown-preview-settings-entry.png)

| Placeholder | Replaced with |
|---|---|
| `{file}` | a temporary file holding the block's text, named with the language as its extension (`fresh-md-….mermaid`) |
| `{source}` | the block's text itself |
| `{width}` | the number of columns available inside the frame |
| `{lang}` | the block's language, as written after the fence |

To add a language, use **[+] Add new** under Renderers and give it the language name as written after the fence (for example `dot`); to stop drawing one, delete its row or clear its command.

::: details Show JSON
In `config.json` the setting is `plugins.markdown_compose.settings.renderers`, a map from language to `command` and `args`. These are the defaults:

```json
{
  "plugins": {
    "markdown_compose": {
      "settings": {
        "renderers": {
          "mermaid": { "command": "termaid", "args": ["{file}", "--width", "{width}"] },
          "math":    { "command": "utftex",  "args": ["--line-width", "{width}", "--", "{source}"] },
          "latex":   { "command": "utftex",  "args": ["--line-width", "{width}", "--", "{source}"] },
          "katex":   { "command": "utftex",  "args": ["--line-width", "{width}", "--", "{source}"] }
        }
      }
    }
  }
}
```

A map set in `config.json` replaces the defaults as a whole, so list every language you want drawn. Some changes, each shown as the `renderers` value:

Draw Graphviz ` ```dot ` blocks too, with [`graph-easy`](https://metacpan.org/pod/Graph::Easy):

```json
"renderers": {
  "mermaid": { "command": "termaid", "args": ["{file}", "--width", "{width}"] },
  "math":    { "command": "utftex",  "args": ["--line-width", "{width}", "--", "{source}"] },
  "dot":     { "command": "graph-easy", "args": ["--as=boxart", "{file}"] }
}
```

Use [`mmaid`](https://github.com/aaronsb/mmaid-go) for Mermaid instead, and stop drawing math (an empty command shows that language as code):

```json
"renderers": {
  "mermaid": { "command": "mmaid", "args": ["{file}"] },
  "math":    { "command": "", "args": [] }
}
```
:::

A command not on your `PATH` can be given by its full path, e.g. `"command": "/opt/tools/bin/utftex"`. Put `--` before `{source}` when the tool takes options, as the defaults do, so a formula starting with `-` is not read as one.

Any program that prints its drawing as text works, in colour too: the colours a command prints with ANSI escape codes (`delta`, `bat --color=always`, `chafa`) are drawn, and its other escape codes are dropped. A row of a drawing that falls on a blank line inside the block is drawn in the frame's colour. `mmaid` covers 31 Mermaid diagram types, but has no width option, so a pie chart can run wider than the page.

## Contents Panel

The `markdown_toc` plugin adds a **Contents** section to the sidebar, under the file explorer, listing the headings of the active Markdown file as a tree — one row per heading, indented by level, with the `#` markers stripped. Headings inside fenced code blocks are not listed. The section belongs to the file it lists: it shows while that file is the active one in its window, waits out of sight while another file or workspace is active, and goes away when the file closes — another workspace's outline never appears in yours. "Markdown: Toggle Table of Contents" opens or closes it by hand, showing the sidebar if it was hidden; "Markdown: Focus Contents" shows it and moves the keyboard into it, and `Alt+Shift+N` / `Alt+Shift+P` cycle the keyboard through the sidebar's sections from anywhere.

- The highlighted row is the heading whose section contains the cursor. When the pane showing the file does not have the keyboard (you are in the file explorer or a sidebar section, or reading the file in another split), the row follows the top of the viewport instead. "Markdown: Contents — Follow Cursor/Scroll" pins one or the other for the session.
- Click a row to put the cursor on that heading and centre it in the pane; focus stays in the sidebar. Press Enter on a row to jump and move focus to the pane. Up/Down browse the rows, Left/Right fold and unfold them.
- The disclosure glyphs collapse and expand the outline. With `fold_buffer` on, collapsing a heading also folds its section in the buffer.

The panel works the same way in source and compose mode: compose conceals the markers and re-flows the text but does not move it, so the rows, the highlight and the jump target are identical in both.

Settings live under *Plugins → markdown_toc* (`plugins.markdown_toc.settings` in `config.json`):

| Setting | Default | Meaning |
|---------|---------|---------|
| `follow` | `"cursor"` | `"cursor"` highlights the heading containing the cursor; `"scroll"` the one at the top of the viewport |
| `fold_buffer` | `false` | Collapsing a heading in the panel also folds its section in the buffer |
| `auto_open` | `true` | Open the panel for Markdown files and close it for other files automatically (it appears while the sidebar is shown) |
| `rows` | `10` | Rows the section asks for; dragging the section divider overrides it |

Files above the `editor.large_file_threshold_bytes` limit keep the headings last scanned and the section title reads "Contents (stale)".
