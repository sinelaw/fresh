# VS Code–Style Indentation Rules — Design

## Motivation

Today Fresh has two auto-indent tiers:

1. **Tree-sitter** (`primitives/indent.rs`) — an AST-driven `indents.scm` per
   language. Accurate, but every grammar that exists *only* to power
   indentation costs a multi-megabyte `ts_parse_table` in the binary. A
   measured release build is **43.9 MB with the grammars vs 23.4 MB without —
   a 20.5 MB delta, ~45% of the binary**, and ~20 MB of that is grammar parse
   tables (`ts_parse_table` / `ts_small_parse_table` in `.rodata`, which
   `strip` does not remove).
2. **Generic bracket heuristic** (`primitives/indent_pattern.rs`) — the
   WASM / min-size fallback. Language-agnostic: it only understands
   C-style `{ } [ ] ( )` and a trailing `:`. It mis-indents keyword-delimited
   languages (Ruby `def…end`, Lua `function…end`, Bash `if…fi`) because it
   treats `(` as a block opener — the exact cross-contamination that issue
   #1425 / PR #1819 fought in the tree-sitter path.

This document designs a **third tier**: per-language, regex-based indentation
rules in the style of VS Code's `language-configuration.json#indentationRules`
(itself inherited from TextMate / Sublime). It is the long-standing TODO at
`input/actions.rs:491`.

The goal is to make indentation for the ~12 "indent-only" tree-sitter
languages (Go, C, C++, C#, Java, PHP, Ruby, Lua, Bash, HTML, CSS, Pascal —
all of which syntect already highlights) good enough *without* a grammar, so
those grammars can be dropped from default builds, and to give correct
indentation to the ~100 syntect-only languages that have **no** tier-1/2
support today (Kotlin, Swift, Dart, YAML, …).

Tree-sitter stays for the things it is uniquely good at: highlighting
TypeScript/Templ (syntect ships no grammar) and scope-aware reference
highlighting.

## Design principles

- **Correctness per language** — the failure mode to avoid is "glitchy"
  indentation: indenting on a `{` inside a string, dedenting on an `end`
  inside a comment, adding a level after `if (cond)` that already had braces.
  We get this right two ways: (a) per-language *families* of rules instead of
  one universal heuristic, and (b) **scope-masking** the line via the
  highlighter before matching, so comments/strings never trigger.
- **Lightness** — no new heavy dependency. The `regex` crate is already an
  always-on dependency (`fresh-editor/Cargo.toml`). Rule tables are a few KB
  of `&'static str`. Per-keystroke cost is 2–3 regex matches on a single line.
- **Extensibility** — adding a language is one table row (point it at a
  family) or a 2–4 line `IndentRulesDef`. No recompile path exists too: rules
  are overridable from `[languages.<id>]` config and shippable in language
  packs, mirroring the existing grammar-pack mechanism.

## Background: how VS Code's `indentationRules` work

Four regexes per language, all evaluated against a single line's text:

| Field | When it fires | Effect |
|-------|---------------|--------|
| `increaseIndentPattern` (required) | matches line *above* | next line **+1** |
| `decreaseIndentPattern` (required) | matches the line *itself* | that line **−1** |
| `indentNextLinePattern` (optional) | matches line above | next line **+1**, one-shot (e.g. braceless `if (x)`) |
| `unIndentedLinePattern` (optional) | matches a line | line is *ignored* as an indent reference (e.g. C preprocessor, labels) |

On Enter (`getGoodIndentForLine`):

```
P    = nearest previous line that is non-blank and not unIndented
base = visualIndent(P)
if increase.matches(P):            base += unit
else if indentNextLine.matches(P): base += unit      # one-shot
if decrease.matches(newLineTail):  base -= unit       # newLineTail = text after the split point
return base
```

On typing a closing bracket (`onType` re-indent): re-derive `base` for the
current line from its predecessor, then apply `decrease` to the line itself.

## Where Fresh diverges from VS Code (deliberately)

1. **No regex lookarounds.** VS Code's stock patterns lean on negative
   lookahead (`(?!\/\/)`, `(?!.*\/\*)`) to dodge comments and strings. The
   `regex` crate is RE2 — no lookaround, no backrefs — and we *want* to keep
   it (fast, linear, already linked) rather than pull in `fancy-regex`
   everywhere. We delete those lookarounds and replace their job with
   scope-masking (below), which is both lighter and more correct than the
   regex approximation.

2. **Scope-masking instead of in-pattern comment dodging.** Before matching,
   build the line's *code view*: the line text with any byte whose
   `HighlightEngine::category_at_position` is `Comment` or `String` replaced
   by a space. The highlighter already computes these spans for the viewport,
   and the indent decision always concerns lines at/near the cursor, so the
   data is hot. Result: `let x = "{"` and `// end` simply have no trigger
   characters to match. This is the single most important anti-glitch
   mechanism.

3. **Keep Fresh's `dedent_after`.** Fresh already models Python flow-exit
   dedent (`return`/`pass`/`raise`/`break`/`continue` → next line −1, issue
   #2192). VS Code has no equivalent. We carry it as an extra optional
   `dedentNextLinePattern` so the rules tier reaches parity with the
   tree-sitter tier it replaces.

## Data model

```rust
// primitives/indent_rules.rs   (pure Rust, WASM-safe)

/// String form — what lives in the static table and in user config.
pub struct IndentRulesDef {
    pub increase: &'static str,                  // required
    pub decrease: &'static str,                  // required
    pub indent_next_line: Option<&'static str>,  // one-shot +1
    pub dedent_next_line: Option<&'static str>,  // one-shot -1 (Fresh extension)
    pub unindented: Option<&'static str>,        // ignore as reference
}

/// Compiled form — cached per language id.
pub struct IndentRules {
    increase: Regex,
    decrease: Regex,
    indent_next_line: Option<Regex>,
    dedent_next_line: Option<Regex>,
    unindented: Option<Regex>,
}
```

Patterns are anchored and operate on the *trimmed code view* of one line, so
they stay short. Examples (final spellings TBD in tests):

```text
# Curly-brace family (C, C++, Java, C#, JS, TS, Go, Rust, PHP, Swift,
# Kotlin, Dart, JSON, CSS, SCSS, …)
increase   = r"[\{\[\(]\s*$"          # line ends opening a block/group
decrease   = r"^\s*[\}\]\)]"          # line starts closing one
indent_next_line = r"^\s*\b(if|else|for|while)\b[^\{]*\)\s*$"   # braceless control head

# Python family
increase   = r":\s*$"                 # block opener
decrease   = None-equivalent: r"$^"   # never auto-dedent (no close tokens)
dedent_next_line = r"^\s*(return|pass|raise|break|continue)\b"

# Keyword-delimited family (Ruby / Lua / Bash / Pascal / Elixir …),
# spelled per language because the keyword sets differ:
# Ruby:
increase   = r"^\s*\b(def|class|module|if|unless|case|while|until|for|begin|do)\b.*$"
decrease   = r"^\s*\b(end|else|elsif|when|rescue|ensure)\b"
```

The keyword-delimited family is exactly where the generic tier-2 heuristic
fails today; giving each such language its own opener/closer keyword set is
the bulk of the correctness win.

## Families and the registry

Most languages do not need bespoke patterns — they need *a* correct family.

```rust
enum Family { CurlyBrace, Python, RubyLike, LuaLike, BashLike, PascalLike, Markup }

// One row per language. Adding a language is usually one line.
static LANGUAGE_RULES: &[(&str, Family)] = &[
    ("rust", Family::CurlyBrace), ("c", Family::CurlyBrace),
    ("cpp", Family::CurlyBrace),  ("java", Family::CurlyBrace),
    ("csharp", Family::CurlyBrace), ("go", Family::CurlyBrace),
    ("javascript", Family::CurlyBrace), ("typescript", Family::CurlyBrace),
    ("kotlin", Family::CurlyBrace), ("swift", Family::CurlyBrace),
    ("dart", Family::CurlyBrace), ("css", Family::CurlyBrace),
    ("json", Family::CurlyBrace),
    ("python", Family::Python),
    ("ruby", Family::RubyLike),  ("lua", Family::LuaLike),
    ("bash", Family::BashLike),  ("pascal", Family::PascalLike),
    ("markdown", Family::Markup),
    // …
];
```

`Family` resolves to a shared `IndentRulesDef`, so the per-language table is
data, not code. Languages absent from the table fall through to the generic
bracket heuristic (tier 2) — the universal default never goes away.

### Language identification (works without tree-sitter)

The lookup key is a normalized id resolved in priority order, all available in
a min-size/WASM build:

1. `HighlightEngine::language()` → tree-sitter `Language::id()` when present.
2. else `HighlightEngine::syntax_name()` → mapped to an id (reuse
   `Language::from_name` plus a small alias table for syntect-only names like
   "Kotlin", "Swift").
3. else `None` → tier-2 generic heuristic.

Keying off a `&str` id (not the tree-sitter `Language` enum) is what lets
syntect-only languages get rules and lets the whole tier survive when the
`tree-sitter` feature is off.

### User / pack extensibility

`apply_language_config` already merges a `[languages.<id>]` section into the
catalog. We extend it with an optional indent block:

```toml
[languages.kotlin.indent]
increase_indent_pattern = "[\\{\\[\\(]\\s*$"
decrease_indent_pattern = "^\\s*[\\}\\]\\)]"
```

A user override replaces the built-in compiled `IndentRules` for that id.
Language packs ship the same block, so adding indentation for a new language
needs no Fresh release — symmetric with how grammars are added.

## Algorithm

```rust
// inside IndentCalculator, new primary tier
fn indent_via_rules(&self, ctx: &IndentCtx, rules: &IndentRules) -> usize {
    // 1. nearest previous non-blank line that is not `unindented`
    let p = ctx.prev_significant_line(rules);
    let mut base = p.visual_indent;

    // 2. opener on the reference line
    if rules.increase.is_match(&p.code_view) {
        base += ctx.unit;
    } else if rules.matches_indent_next_line(&p.code_view) {
        base += ctx.unit;                 // one-shot, see below
    } else if rules.matches_dedent_next_line(&p.code_view) {
        base = base.saturating_sub(ctx.unit);
    }

    // 3. closer at the start of the *new* line's tail (text after the split)
    if rules.decrease.is_match(&ctx.new_line_tail_code_view) {
        base = base.saturating_sub(ctx.unit);
    }
    base
}
```

`code_view` = the line with Comment/String spans masked (§Design principles).
`indent_next_line` is genuinely one-shot: because we always re-derive from the
*nearest* significant line and that line's own opener is re-tested each Enter,
a braceless `if` adds exactly one level for the immediately following line and
nothing after — no sticky state to track.

Closing-delimiter typing (`calculate_dedent_for_delimiter`) reuses the same
machinery: re-derive `base` for the current line from its predecessor, then
apply `decrease` to the current line.

### Revised dispatch in `calculate_indent`

```
1. indent_for_cursor_in_leading_ws            (unchanged, #1425)
2. RULES TIER  (NEW, primary):  look up IndentRules by language id
      → if found, return indent_via_rules(...)
3. tree-sitter tier  (only if grammar still compiled AND no rules matched)
4. generic bracket heuristic                  (existing tier-2 default)
```

Once the rules tier covers an "indent-only" language, step 3 for that language
is dead and its grammar can leave the default feature set. Languages with no
rules and no grammar still get step 4.

## Performance

- Regexes compiled lazily on first use of a language, cached in a
  `HashMap<&'static str, IndentRules>` on `IndentCalculator` (mirrors the
  existing `configs` cache).
- Per Enter: one backward scan for the reference line (already done today) +
  2–3 single-line regex matches + one small allocation for the masked
  `code_view`. O(line length); no parse, no tree.
- Binary: `regex` already linked; tables are a few KB. Versus ~20 MB of parse
  tables removed. WASM build gains real per-language indentation it never had.

## Testing strategy

Correctness is enforced by table-driven golden tests, run with the
`tree-sitter` feature **off** so they exercise exactly the rules tier:

- **Anti-glitch corpus** (the headline cases):
  - `let x = "{";\n│`            → no extra indent (brace in string)
  - `// open {\n│`               → no extra indent (brace in comment)
  - `s = "end"\n│` (Ruby)        → no dedent (keyword in string)
  - `if (x) {\n│`                → +1 once, not +2
- **Per-family parity**: port the existing `tests/e2e/auto_indent.rs` and
  `tests/semantic/migrated_auto_indent_extras.rs` oracles and assert the rules
  tier matches the tree-sitter tier's output on the same inputs (Ruby `end`,
  Lua `end`, Bash `fi`, Python `:` + `return`).
- **Round-trip property test**: on balanced generated code, Enter-then-type-
  closer returns to the opener's indent.

A CI guard (`indent_rules::parity`, gated on the `tree-sitter` feature) asserts
the rules-tier and tree-sitter-tier outputs agree, so dropping a grammar can
never silently regress indentation. **Scope matters:** the guard only covers
languages where tree-sitter is an authoritative oracle — curly-brace families
and Python (also the largest grammars). Keyword-delimited families
(Ruby/Lua/Bash/Pascal) are *excluded* from the parity guard and pinned by
golden unit tests instead, because on incomplete mid-edit input tree-sitter
cannot form a block node and the current editor already falls back to
copy-the-line indent — so the rules tier is a strict improvement there, and
demanding parity with tree-sitter would wrongly forbid the better behavior.

## Rollout

1. **Done.** Landed `indent_rules.rs` + the family table + tests; wired it
   into the no-tree-sitter paths so syntect-only languages (Kotlin, Swift,
   Dart, …) gained language-aware indentation immediately.
2. **Done.** Flipped the dispatch so the rules tier runs *before* tree-sitter
   (scope-masked, keyed by syntax name); tree-sitter is consulted only for
   grammar-only languages the rules tier doesn't recognise (Templ). Parity
   verified by the `indent_rules::parity` guard.
3. **Done.** Reduced the bundled grammars to the must-keep set —
   JavaScript, TypeScript, JSON(C), Templ, Go — via the
   `fresh-languages/bundled-languages` feature (the editor `tree-sitter`
   feature now points at it). The other 14 grammars moved behind the opt-in
   `tree-sitter-all` editor feature (`fresh-languages/all-languages`).
   Default release binary: **43.9 MB → 25.8 MB (−18.1 MB)**. All grammar
   access is centralized behind `Language::ts_language()`, so fresh-editor is
   grammar-agnostic and a `None` cleanly routes to the rules/syntect
   fallbacks. The full-corpus parity guard still passes under
   `--features tree-sitter-all`, confirming the dropped languages' rules
   match what tree-sitter produced.
4. **TODO.** Wire the `[languages.<id>.indent]` config + language-pack
   override path so users can add/tune rules without a rebuild.
