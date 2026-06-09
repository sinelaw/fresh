# Adding a Built-in Language

This is the contributor's guide to teaching Fresh about a new language *in the
source tree* — syntax highlighting, auto-indentation, and the indent
**families** — plus how those systems decide what to do at runtime.

Two audiences, two paths:

- **Adding a built-in language** (this page) — you're editing Fresh's Rust
  source: an embedded grammar, a tree-sitter parser, or a row in the indent
  family table. Requires a rebuild.
- **Shipping a language pack or tweaking config** — no recompile. See
  [Language Packs](/plugins/development/language-packs) and the
  [Configuration guide](/configuration/#customize-auto-indentation). Most
  niche-language needs are best met this way.

The deeper design rationale lives in the repo's internal docs:
[`indentation-rules-design.md`](https://github.com/sinelaw/fresh/blob/master/docs/internal/indentation-rules-design.md)
(why the regex indentation tier exists and the binary-size win it bought) and
[`language-support-review.md`](https://github.com/sinelaw/fresh/blob/master/docs/internal/language-support-review.md)
(the current per-language support matrix).

---

## Mental model: two independent systems

Fresh treats *how a language looks* and *how it indents* as separate concerns,
resolved separately. You can add one without the other.

| System | What it does | Engines |
|--------|--------------|---------|
| **Highlighting** | colors tokens (keywords, strings, comments) | Syntect (TextMate/Sublime grammars) — primary; tree-sitter — fallback for ~18 languages |
| **Auto-indent** | picks the indent of a new line on Enter / when typing a closer | Tree-sitter AST (bundled grammars) → per-language regex rules → generic bracket heuristic |

A language can be highlighted by syntect while indented by the regex rules tier,
with no tree-sitter grammar at all — that is the common case (Kotlin, Swift,
Dart, …). Both systems hang off the **unified grammar catalog**
(`GrammarRegistry`), which maps a file path / name to a `GrammarEntry` recording
which engines can serve it. Detection order (filename → glob → extension →
first-line/shebang → configured default) lives in
`crates/fresh-editor/src/primitives/detected_language.rs`.

---

## How auto-indentation works

When you press Enter (or type a closing `}` / `]` / `)`), Fresh computes the new
line's indent through a **three-tier dispatch** in
`crates/fresh-editor/src/input/actions.rs` (the Enter path around
`insert_newline_with_indent`, and `calculate_closing_delimiter_indent` for typed
closers). Tiers are tried in order; the first that produces an answer wins:

```
1. Tree-sitter AST tier   — only for languages with a *bundled* grammar
                            (Go, JSON/JSONC, TypeScript, JavaScript, Templ).
                            Parses the block structure, exact on strings/comments.
2. Regex rules tier        — every other language. Keyed by the syntect syntax
                            name, scope-masked. This is where families live.
3. Generic bracket heuristic — unknown syntaxes (.txt, …). C-style { } [ ] (  ) +
                            trailing ':'. Language-agnostic last resort.
```

Which tier applies hinges on one check: `state.highlighter.language()` returns a
tree-sitter `Language` *and* `language.ts_language().is_some()` (its grammar is
compiled into this build). If so → tier 1; otherwise → tier 2, falling through to
tier 3 when no rules match. Most grammars were deliberately dropped from the
default build to save ~18 MB of parse tables, so in practice **most languages
indent via the regex rules tier.**

### The regex rules tier (tier 2)

Source: `crates/fresh-editor/src/primitives/indent_rules.rs`. Pure Rust, WASM-safe
(no tree-sitter dependency), so it works in min-size and browser builds too.

Each language is described by a small set of anchored regexes
(`IndentRulesDef`). Every field is optional (`None` = "never matches"):

| Field | Tested against | Effect |
|-------|----------------|--------|
| `increase` | the **reference line** (line being split, or nearest non-blank above) | new line is **+1** level |
| `decrease` | the **new line's tail** (text that moves down past the cursor) | that line is **−1** level |
| `indent_next_line` | the reference line | **+1** for the immediately following line only (one-shot; braceless `if (x)`) |
| `dedent_next_line` | the reference line | **−1** for the following line (one-shot; Python flow-exit `return`/`pass`/…) |
| `self_close` | the reference line | **cancels** `increase` when the same line also closes its block (one-liner `def f; end`) |
| `indentation_significant` (bool) | — | layout-defined languages (Python, YAML): on a blank line, keep the cursor's current column instead of re-deriving, so a manual dedent sticks |

The algorithm (`IndentRules::calculate_indent`): take the reference line's visual
indent as `base`; add a unit if it `increase`s (and isn't `self_close`d) or
matches `indent_next_line`, else subtract a unit if it matches
`dedent_next_line`; then subtract a unit if the new line's tail `decrease`s
(unless the opener was on this same line — the `{│}` bracket-expansion case). The
unit is one `tab_size`.

#### Scope masking — the key anti-glitch mechanism

The classic regex-indentation bug is triggering on a `{` inside a string or an
`end` inside a comment. Fresh avoids it without lookarounds (the `regex` crate is
RE2 — linear, no look-around/backrefs). Before matching, each line is turned into
a **code view**: every byte the highlighter reports as `Comment` or `String` is
replaced with a space (`byte_is_code` in `actions.rs` sources this from the
highlighter's *already-computed* render spans — no second parse). So
`let x = "{"` and `// end` have no trigger characters left to match. When no
scope info is cached (line outside the viewport, plain buffer), the code view is
the raw line, degrading to plain regex matching rather than misbehaving.

---

## Language families

Most languages don't need bespoke patterns — they need *a correct family*. A
**family** is a shared `IndentRulesDef` covering a class of block syntax. The
enum and the per-language mapping live in `indent_rules.rs`:

```rust
pub enum Family {
    CurlyBrace,  // block structure is { } [ ] ( )
    Python,      // ':' opens a block; flow-exit dedents next line
    RubyLike,    // def…end, do…end, midblock else/when/rescue
    LuaLike,     // function…end, if…then…end, for…do…end, repeat…until
    BashLike,    // if…then…fi, for/while…do…done, case…esac, { }
    PascalLike,  // begin…end, case…of…end, repeat…until
}
```

Each family's actual patterns (the `const CURLY_BRACE`, `const PYTHON`, … defs):

| Family | `increase` (opener) | `decrease` (closer) | Notable extras |
|--------|---------------------|---------------------|----------------|
| **CurlyBrace** | line ends with `{ [ (` | line starts with `} ] )` | `indent_next_line` for braceless `if/for/while(…)`/`else` |
| **Python** | line ends with `:` | moved-down `elif/else/except/finally/case` | `dedent_next_line` for `return/pass/raise/break/continue`; `indentation_significant = true` |
| **RubyLike** | `def/class/module/if/case/…` at line start, or trailing `do`/`do \|x\|` | `end` + midblock `else/elsif/when/in/rescue/ensure` | `self_close = \bend\b` (one-liners) |
| **LuaLike** | `function/if/elseif/for/while/repeat`, or trailing `do`/`then` | `end/else/elseif/until` | `self_close = \bend\b` |
| **BashLike** | trailing `then`/`do`, `case … in`, function-body `{` | `fi/done/esac/else/elif/}` | `(` deliberately **excluded** (subshell, not a block) |
| **PascalLike** | `begin/case/record/try/repeat/asm`, or trailing `begin`/`of` | `end/until/except/finally` | `self_close = \bend\b` |

Study the Python row when adding any **layout-defined** language (YAML, etc.): it
has no real closing tokens, so it leans on `dedent_next_line` and the
`indentation_significant` flag rather than `decrease`.

### The mapping is just data

`family_for_id` is a single `match` from a normalized language id to a family —
the one extension point for the common case:

```rust
fn family_for_id(id: &str) -> Option<Family> {
    let f = match id {
        "rust" | "c" | "cpp" | "csharp" | "java" | "go" | "javascript"
        | "typescript" | "php" | "swift" | "kotlin" | "dart" | "scala"
        | "json" | "css" | "scss" | "less" /* … */ => Family::CurlyBrace,
        "python" => Family::Python,
        "ruby"   => Family::RubyLike,
        "lua"    => Family::LuaLike,
        "bash" | "sh" | "shell" | "shellscript" => Family::BashLike,
        "pascal" => Family::PascalLike,
        _ => return None,   // → falls through to the generic bracket heuristic
    };
    Some(f)
}
```

Languages absent here fall through to tier 3 — the universal default never goes
away. Lookup is keyed by a **string id**, not the tree-sitter `Language` enum, so
syntect-only languages get rules and the whole tier survives with the
`tree-sitter` feature off. `rules_for_syntax_name` maps a syntect display name
(`"C++"`, `"Bourne Again Shell (bash)"`) to an id before calling `rules_for_id`.

---

## Recipes: how to add a language

Pick the dimensions you need. They are independent.

### A. Just indentation, existing family — one line

The language already highlights (syntect default or a pack) and behaves like an
existing family. Add an arm to `family_for_id` in `indent_rules.rs`:

```rust
"zig" | "odin" => Family::CurlyBrace,
```

Add a golden test in the same file's `tests` module and you're done. No grammar,
no recompile path for users (they can do the same via config — see D).

### B. Syntax highlighting via an embedded syntect grammar

For a language syntect doesn't ship (Kotlin, Swift, Nix, …):

1. Drop a self-contained `.sublime-syntax` file into
   `crates/fresh-editor/src/grammars/`. (No `extends:` — Fresh supports a subset;
   only internal `include` works. See the language-packs page's compatibility
   notes.)
2. Register it in `GrammarRegistry::add_embedded_grammars`
   (`crates/fresh-editor/src/primitives/grammar/loader.rs`) with its extensions /
   filenames. The catalog picks it up on the next rebuild.
3. If the language should also indent, add it to a family (recipe A).

### C. Syntax highlighting + structural features via tree-sitter

Only when tree-sitter buys something syntect can't (e.g. TypeScript, which
syntect has no grammar for, or precise indent where the regex rules aren't good
enough). This is the **heavy** option — each grammar adds multi-MB parse tables —
so it's gated behind build features and reserved for the must-keep set.

1. Add a variant to `fresh_languages::Language`
   (`crates/fresh-languages/src/lib.rs`) with its extensions, display name, and
   highlight config; bundle the `tree-sitter-<lang>` crate behind the appropriate
   feature.
2. Add `crates/fresh-editor/queries/<lang>/indents.scm` for AST-driven indent.
3. The catalog auto-creates a tree-sitter entry if no syntect grammar matches.

Keep a **parity test** (the `parity` module in `indent_rules.rs`): the regex
rules for the language must agree with the tree-sitter output, so the grammar can
be dropped from default builds without regressing indentation.

### D. Indentation or detection from config — no recompile

Users (and language packs) add or tune a language entirely in config. This is the
right answer for niche languages.

```toml
[languages.kotlin]
extensions = ["kt", "kts"]
comment_prefix = "//"

[languages.kotlin.indent]
increase_indent_pattern = "[\\{\\[\\(]\\s*$"
decrease_indent_pattern = "^\\s*[\\}\\]\\)]"
```

The `[languages.<id>.indent]` block maps field-for-field onto the rule set
(`increase_indent_pattern`, `decrease_indent_pattern`, `indent_next_line_pattern`,
`dedent_next_line_pattern`, `self_close_pattern`). An omitted pattern **inherits
from the language's built-in family**, so you can override just one thing; a
language with no family starts from blank rules, which is how config adds
indentation for an otherwise-unknown language. Full reference with examples:
[Configuration → Customize Auto-Indentation](/configuration/#customize-auto-indentation).

A language **pack** manifest (`package.json`) only exposes an `autoIndent`
boolean — the per-pattern `indent` block is a config feature. A pack that needs
custom patterns documents them for the user's config, or relies on the built-in
family.

### E. LSP

Orthogonal to both systems. Add a server config block (`command`, `args`,
`autoStart`) under `[lsp.<id>]` in config or in a pack's `fresh.lsp`. See
[Language Packs](/plugins/development/language-packs) for common servers.

---

## Where to make each change — quick map

| Goal | File / location | Effort |
|------|-----------------|--------|
| Map a language to an existing indent family | `family_for_id` in `primitives/indent_rules.rs` | one line |
| New indent family | add a `Family` variant + `const <FAMILY>` def + `def_for_family`/`FAMILY_RULES` arms in `indent_rules.rs` | small |
| Embedded syntect highlighting | `.sublime-syntax` in `src/grammars/` + register in `grammar/loader.rs` | medium |
| Tree-sitter grammar + AST indent | `fresh_languages::Language` + `queries/<lang>/indents.scm` (feature-gated) | high |
| Extension / filename / glob detection | `GrammarEntry` in the catalog, or `[languages.<id>]` config | low |
| Indent without recompiling | `[languages.<id>.indent]` in config, or a language pack | low |
| LSP | `[lsp.<id>]` config or pack `fresh.lsp` | low |

## Testing

- **Unit (rules tier):** table-driven golden tests in `indent_rules.rs`'s `tests`
  module, run with the `tree-sitter` feature off so they exercise exactly the
  rules tier. Always include the **anti-glitch corpus**: a brace in a string, a
  keyword in a comment, a one-liner `def f; end`, a braceless `if (x)`.
- **Parity (when adding a tree-sitter grammar):** the `parity` module asserts the
  rules tier matches tree-sitter on a corpus, gated on the `tree-sitter` feature —
  the safety net for dropping a grammar from the default build.
- **Highlighting:** open a sample file and confirm tokens color.
