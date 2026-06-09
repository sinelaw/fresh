# Adding a Built-in Language

This guide explains how Fresh supports a language's **syntax highlighting** and
**auto-indentation**, and how to add a new one.

## Prime directive: avoid tree-sitter

Fresh deliberately does **not** use tree-sitter for most languages, and new
languages should not add it. Tree-sitter grammars cost multiple megabytes of
parse tables each (≈18 MB was removed by dropping them), and they don't work in
the WASM/min-size builds. Fresh keeps tree-sitter only for the handful of
languages nothing else can serve, and treats it as legacy for everything else.

Instead, the two systems each have a lightweight, tree-sitter-free path that you
should prefer:

| System | Preferred mechanism | Tree-sitter? |
|--------|--------------------|--------------|
| **Highlighting** | a syntect (TextMate/Sublime) grammar, or a language pack | only when syntect genuinely can't render the language |
| **Auto-indent** | the regex **indent-rules** tier (language *families*) | avoid — the rules tier is the primary path |

Both systems are independent: you can add highlighting without indentation, or
vice versa. They are wired together through the grammar catalog
(`GrammarRegistry`), which maps a file to a language; detection (extension,
filename, glob, shebang, configured default) lives in
`crates/fresh-editor/src/primitives/detected_language.rs`.

## How auto-indentation is decided

When you press Enter or type a closing bracket, Fresh chooses an indent through a
tiered fallback, in this order of preference:

1. **Regex indent-rules tier** — the primary, tree-sitter-free path. Each
   language belongs to a *family* of simple rules. Lives in
   `crates/fresh-editor/src/primitives/indent_rules.rs`.
2. **Tree-sitter AST** — legacy, used only for the few languages that still ship
   a bundled grammar. Not something a new language should opt into.
   (`primitives/indent.rs`.)
3. **Generic bracket heuristic** — a language-agnostic last resort for unknown
   files. (`primitives/indent_pattern.rs`.)

The rules tier's one important guarantee is **scope masking**: before matching,
it blanks out comment and string spans (reusing the highlighter's existing
output), so a bracket or keyword inside a string or comment never triggers an
indent. This is what keeps regex-based indentation from being glitchy, and it's
why the rules tier is good enough to replace tree-sitter for indentation.

## Language families

A **family** is a shared set of indent rules describing one class of block
syntax. Most languages just need to be pointed at the right family — no bespoke
logic. The families (defined in `indent_rules.rs`) are:

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

## Adding a language

Pick only the pieces you need.

### Highlighting

In order of preference:

1. **Syntect grammar (preferred).** Add a self-contained `.sublime-syntax` file
   under `crates/fresh-editor/src/grammars/` and register it with the catalog
   (`primitives/grammar/loader.rs`). No parse-table weight, works everywhere.
2. **Language pack.** Ship the same grammar as an installable pack — no recompile
   and no core change at all. See [Language Packs](/plugins/development/language-packs).
3. **Tree-sitter (last resort).** Only if syntect truly can't render the language
   and the cost is justified. This is the path we're trying *not* to grow.

### Auto-indentation

Prefer the rules tier — never add a tree-sitter indent query for a new language.

1. **Map to a family.** If the language fits an existing family, that's a
   one-line addition to the family table in `indent_rules.rs`.
2. **Custom rules from config.** Users (and language packs) can define or tune a
   language's indent rules entirely in config — no recompile. Full reference:
   [Configuration → Customize Auto-Indentation](/configuration/#customize-auto-indentation).
3. **New family.** Only if the language's block syntax matches none of the
   existing ones; add a family to `indent_rules.rs`.

### Detection and LSP

- **Detection** (extensions, filenames, globs) comes from the catalog entry or a
  `[languages.<id>]` config block.
- **LSP** is orthogonal to both systems: a server config under `[lsp.<id>]` (or a
  pack's `fresh.lsp`).

## Where things live

| Concern | Location |
|---------|----------|
| Indent families & rules | `crates/fresh-editor/src/primitives/indent_rules.rs` |
| Generic bracket fallback | `crates/fresh-editor/src/primitives/indent_pattern.rs` |
| Legacy tree-sitter indent | `crates/fresh-editor/src/primitives/indent.rs` |
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
