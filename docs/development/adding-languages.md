# Adding a Built-in Language

Fresh supports a language through two independent systems — **syntax
highlighting** and **auto-indentation**. You can add either on its own. Both are
connected through the grammar catalog (`GrammarRegistry`), which maps a file to a
language; detection (extension, filename, glob, shebang, configured default)
lives in `crates/fresh-editor/src/primitives/detected_language.rs`.

| System | What powers it |
|--------|----------------|
| **Highlighting** | a syntect (TextMate/Sublime) grammar, or a language pack |
| **Auto-indent** | the regex **indent-rules** tier (language *families*) |

## Adding a language

Pick only the pieces you need.

### Highlighting

1. **Syntect grammar.** Add a self-contained `.sublime-syntax` file under
   `crates/fresh-editor/src/grammars/` and register it with the catalog
   (`primitives/grammar/loader.rs`).
2. **Language pack.** Ship the same grammar as an installable pack — no recompile
   and no core change. See [Language Packs](/plugins/development/language-packs).

### Auto-indentation

1. **Map to a family.** If the language fits an existing family (below), that's a
   one-line addition to the family table in `indent_rules.rs`.
2. **Custom rules from config.** Users (and language packs) can define or tune a
   language's indent rules in config — no recompile. Full reference:
   [Configuration → Customize Auto-Indentation](/configuration/#customize-auto-indentation).
3. **New family.** If the language's block syntax matches none of the existing
   ones, add a family to `indent_rules.rs`.

### Detection and LSP

- **Detection** (extensions, filenames, globs) comes from the catalog entry or a
  `[languages.<id>]` config block.
- **LSP** is orthogonal to both systems: a server config under `[lsp.<id>]` (or a
  pack's `fresh.lsp`).

## How auto-indentation works

When you press Enter or type a closing bracket, Fresh chooses an indent through a
tiered fallback:

1. **Regex indent-rules tier** — the primary path. Each language belongs to a
   *family* of simple rules (`primitives/indent_rules.rs`).
2. **Tree-sitter AST** — used for the few languages that still ship a bundled
   grammar (`primitives/indent.rs`).
3. **Generic bracket heuristic** — a language-agnostic fallback for unknown
   files (`primitives/indent_pattern.rs`).

The rules tier applies **scope masking**: before matching, it blanks out comment
and string spans (reusing the highlighter's existing output), so a bracket or
keyword inside a string or comment doesn't trigger an indent.

### Language families

A **family** is a shared set of indent rules describing one class of block
syntax. Most languages can be pointed at an existing family rather than needing
bespoke logic. The families (defined in `indent_rules.rs`) are:

- **CurlyBrace** — `{ } [ ] ( )` block structure (C, Rust, JS/TS, Go, JSON, CSS…).
- **Python** — layout-defined: `:` opens a block, indentation *is* the structure.
- **RubyLike** — `def…end`, `do…end`, with midblock keywords.
- **LuaLike** — `function…end`, `if…then…end`, `for…do…end`.
- **BashLike** — `if…then…fi`, `for…do…done`, `case…esac`.
- **PascalLike** — `begin…end`, `case…of…end`.

A family captures the usual signals: what opens a deeper level, what closes one,
one-shot indent/dedent for things like a braceless `if` or a Python `return`, and
a "self-close" rule so one-liners (`def f; end`) don't over-indent. The exact
patterns are data in `indent_rules.rs`; the user-facing equivalents are
documented in [Configuration → Customize Auto-Indentation](/configuration/#customize-auto-indentation).

## A note on tree-sitter

Fresh leans toward the syntect and indent-rules paths above rather than
tree-sitter. Each tree-sitter grammar adds a sizable parse table to the binary
(around 18 MB was reclaimed by dropping the ones that weren't essential), so it's
reserved for languages syntect can't render, and a new language usually doesn't
need a tree-sitter indent query — the rules tier covers it.

## Where things live

| Concern | Location |
|---------|----------|
| Indent families & rules | `crates/fresh-editor/src/primitives/indent_rules.rs` |
| Generic bracket fallback | `crates/fresh-editor/src/primitives/indent_pattern.rs` |
| Tree-sitter indent | `crates/fresh-editor/src/primitives/indent.rs` |
| Syntect grammars | `crates/fresh-editor/src/grammars/` + `primitives/grammar/loader.rs` |
| Language detection / catalog | `crates/fresh-editor/src/primitives/detected_language.rs`, `primitives/grammar/` |
| User-facing indent config | [Configuration guide](/configuration/#customize-auto-indentation) |
| Language packs (no recompile) | [Language Packs](/plugins/development/language-packs) |

## Background

The design rationale and the per-language support matrix live in the repo's
internal docs:
[`indentation-rules-design.md`](https://github.com/sinelaw/fresh/blob/master/docs/internal/indentation-rules-design.md)
and
[`language-support-review.md`](https://github.com/sinelaw/fresh/blob/master/docs/internal/language-support-review.md).
