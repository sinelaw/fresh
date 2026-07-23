# Syntax Highlighting Architecture

Purpose: document Fresh's syntax-highlighting engine — grammar selection, the
syntect (TextMate) checkpoint/incremental engine, the tree-sitter fallback, how
spans reach the renderer, and the bracket- and reference-highlight overlays that
layer on top.

Status legend: **[impl]** verified in code · **[plan]** designed/aspirational ·
**[flag]** discrepancy between code and a source doc.

---

## 1. Engine choice: syntect-first, tree-sitter for the gaps

Fresh highlights with **syntect** (TextMate / Sublime `.sublime-syntax`
grammars) as the primary engine and **tree-sitter** only for the handful of
languages syntect can't serve well. **syntect is the default.** There is no
runtime use of a separate "syntect crate" abstraction beyond the `syntect`
dependency itself; tree-sitter grammars come through the languages crate.

Engine dispatch goes through a `HighlightEngine` enum with three states —
syntect (TextMate), tree-sitter, and none. A single factory implements the
**"prefer syntect, else tree-sitter, else None"** rule: it picks syntect if the
catalog entry has a syntect index, else tree-sitter if it has a tree-sitter
language, else none. File- and syntax-name lookups are thin wrappers over this
rule; the older highlighter-preference API has been removed. **[impl]**

### Why this split (mined from git history)

- **syntect covers breadth cheaply.** Roughly a hundred-plus built-in/embedded
  grammars ship in a build-time binary dump; adding a language is a
  `.sublime-syntax` file, no parse table linked. The language selector was
  deliberately switched from the small fixed-variant tree-sitter language enum to
  "all syntect syntaxes" so the selector reflects everything syntect can
  highlight. **[impl]**
- **tree-sitter is heavy.** A bundling change dropped most tree-sitter grammars,
  cutting the default release binary by roughly half (on the order of an 18 MB
  reduction). Only grammars syntect *can't* highlight are kept.
- **Bundled tree-sitter set** (the `bundled-languages` feature backing the
  editor's `tree-sitter` feature): **JavaScript, TypeScript, JSON (also serving
  JSONC), Templ, and Go** (Go only because Templ's grammar extends it). Reasons:
  - **TypeScript**: syntect ships no TS grammar → tree-sitter only.
  - **JavaScript**: syntect's JS grammar leaks template-literal state past the
    closing backtick and paints the rest of the file as a string; JS is routed
    through tree-sitter by *skipping* the syntect "JavaScript" entry when
    building the catalog. (syntect's JS grammar is still reachable by name for
    markdown-popup code highlighting.)
  - **JSONC**: no JSONC tree-sitter crate exists; the JSON grammar recovers past
    comments/trailing commas well enough.
  - **Templ**: Go + components/HTML/CSS.
  A `tree-sitter-all` editor feature re-enables the full grammar set for users
  who want AST indent / scope-aware refs for everything. **[impl]**
- tree-sitter is *also* retained for non-highlighting structural features even on
  syntect-highlighted buffers: precise indentation, scope-aware reference
  highlighting, semantic highlighting. The TextMate engine keeps an optional
  tree-sitter language handle purely for that; it never produces spans.

### What the oxc-based plugin crate is — and is NOT

The oxc-based parser crate (`oxc_parser`, `oxc_allocator`, `oxc_span`,
`oxc_ast`, `oxc_transformer`, `oxc_codegen`, `oxc_semantic`,
`oxc_isolated_declarations`) **has nothing to do with syntax highlighting.** It
is the plugin toolchain: TypeScript→JS transpile, `.d.ts` emission, plugin
dependency extraction + topological load ordering, and ES-module bundling so
plugin source (incl. config-as-code `init.ts`) can run in the QuickJS plugin
runtime. JS/TS *buffers* are highlighted by the tree-sitter grammars in the
languages crate. The only editor-side caller is a CLI plugin-check helper; the
real consumer is the plugin runtime. The oxc deps in the editor crate are pulled
by the `plugins` feature to syntax-check `init.ts`, not for highlighting.
**[impl]** **[flag]**: the oxc deps may *look* highlighting-related but are not.

---

## 2. Grammar loading & language detection

### Grammar catalog and registry

The grammar registry owns a unified catalog. Each grammar entry records which
engines can serve a language (syntect index and/or tree-sitter language). Three
lookup methods:

- **by path** — filename → glob → extension, then syntect first-line regex, then
  shebang.
- **by name** — display name / id / short alias, case-insensitive.
- **by extension**.

### Build-time syntect dump (no runtime `.sublime-syntax` parsing on the hot path)

The bundled `.sublime-syntax` files are **compiled at build time** into a binary
`SyntaxSet` dump, embedded in the binary and loaded from uncompressed data at
startup. A fast path uses the dump directly when there are no user / language-
pack / bundle / plugin grammars; otherwise those layers are added on and the set
is rebuilt. Grammar provenance is tracked (user, language pack, bundle, plugin).
**[impl]**

### `DetectedLanguage` — the per-buffer source of truth

`DetectedLanguage` is a struct (not an enum) bundling the canonical LSP/config
id, the display name (matching the syntect syntax name), the resolved highlight
engine, and an optional tree-sitter language, so the highlighter and language
state stay in sync through one type. Resolution from a path proceeds: filename →
glob → extension → catalog → shebang/first-line → config fallback. The
LSP/config language id is resolved **independently** of the grammar catalog, so
the config id is correct even when the grammar registry is empty. **[impl]**

### Shebang detection

Shebang detection is the final fallback in path-based lookup, used when
filename/extension and syntect's first-line regex all miss (e.g. fish, Lua,
PowerShell have no first-line regex). It handles `env` indirection
(`-S`/`-i`/`VAR=val`) and strips version suffixes (`python3.11`→`python`).
tree-sitter-backed targets reuse the tree-sitter language id; syntect-only ones
use named constants. `awk`/unknown → plain text. **[impl]**

---

## 3. The syntect checkpoint / incremental engine (the TextMate engine)

Syntect's parser is a **sequential state machine**: it must process bytes in
order from a known `(ParseState, ScopeStack)` to correctly track multi-line
constructs (comments, strings) and embedded-language transitions (CSS-in-HTML,
code-in-markdown-fences). The engine makes scrolling and editing cheap with a
span cache + periodic parse-state checkpoints + convergence-based incremental
re-highlight. Mixed-language files (Markdown fences and friends) are handled
by an embedded-region layer on top of this state machine — see
[embedded-language-highlighting.md](embedded-language-highlighting.md) for
that mechanism and for when to use grammar-level embedding instead. This is the **implemented v2 design**; the source design doc
describes both the superseded v1 (a flat checkpoint vector at a coarse byte
interval, discarded after every edit) and the v2 approach the code now realizes.

### Runtime vs WASM engines

There are **two parallel implementations** with the same design:

- the **runtime** engine — used by the editor.
- a sibling **WASM-compatible** engine, explicitly mirroring the runtime design.
  `DetectedLanguage` references the runtime type. **[impl]**

The rest of this section describes the runtime engine.

### State stored

The TextMate engine holds:

- the resolved grammar (a shared `SyntaxSet` plus the syntax index).
- checkpoint *positions* held as markers in Fresh's AVL interval-tree marker
  system; they auto-shift in logarithmic time on every edit so they never go
  stale.
- the saved parser snapshot — a `(ParseState, ScopeStack)` per checkpoint, keyed
  by marker id.
- the earliest byte touched since the last render (the dirty point).
- the span cache.
- bookkeeping: last buffer length, the optional tree-sitter language, stats, and
  a memoised scope→category cache (never invalidated, because syntect scope
  atoms are append-only).

The span cache holds a byte range, a vector of cached spans (each a range plus a
`HighlightCategory`, **theme-independent** so theme changes don't invalidate the
cache), and an optional tail parse state at the cache's end that powers forward
extension (absent when the last mutation didn't end exactly at the cache end).

A **checkpoint** = a marker (byte offset, auto-shifted) + a full
`(ParseState, ScopeStack)` snapshot keyed by marker id.

### Constants

- A **small/large file threshold** of about 1 MiB bounds any single parse range
  and decides whether the whole file may be parsed at once.
- A **checkpoint interval** — a fixed byte interval — places a checkpoint at the
  first line boundary after each interval (roughly every few lines). A
  few-hundred-KB file produces on the order of a thousand markers, within the
  marker tree's comfort zone. The interval was deliberately set far smaller than
  the v1 value, trading many more markers for proportionally faster convergence.
- A **convergence budget** — a bounded number of bytes — caps how far past the
  dirty point a partial update parses in one pass, so a pathological edit (e.g.
  opening an unclosed `/*`) can't degenerate into a whole-file reparse.

### The four render-time paths

The viewport-highlight entry point selects among four paths:

1. **Cache hit** — cache covers the viewport, no dirty edit, buffer length
   unchanged → filter the cached spans. Zero parse work. For files under the
   large-file threshold, the first render parses the whole file so every later
   scroll is filter-only (Phase 1; profiling showed most scroll CPU was full
   re-parse before this).
2. **Forward extension** — no dirty edit, cache covers the viewport start but
   ends before the parse target and a tail state is present → resume from the
   tail state and parse only the uncovered tail bytes (Phase 3). This is
   steady-state scroll on large files.
3. **Partial update** — cache covers the viewport AND a dirty edit lies before
   the parse target → run the convergence walk (see below).
4. **Cold start / fallback** — resume from the nearest checkpoint (or a fresh
   parse state for large files with no nearby checkpoint, bounded by the
   large-file threshold), parse to the target, and build a fresh cache.

For files **over the large-file threshold**, the parse range is a viewport
window (viewport ± a context margin); scroll cost stays bounded via path 2. (An
earlier fixed window broke embedded languages whose opening tag sat above the
viewport by more than the window size — the whole motivation for checkpoints.)

### Incremental re-highlight: convergence algorithm

The partial-update path is a VSCode-style convergence walk:

1. **Resume**: pick the checkpoint marker with the greatest start *before* the
   dirty position (search bounded to the large-file threshold); clone its
   `(state, scopes)`. If none exists and the file fits, start fresh at byte 0;
   else fall back to a full parse.
2. **Markers ahead**: collect checkpoint markers between the dirty position and
   the parse target, sorted, as convergence candidates.
3. **Forward parse**: parse line by line, creating new checkpoints every
   checkpoint interval.
4. **Convergence**: when the running offset reaches a candidate marker, compare
   the *recomputed* `(state, scopes)` to the stored one. If **equal** →
   converged; everything downstream is still valid, stop. If not → update the
   stored state and continue. Most single-char edits converge within one or two
   checkpoints (the cursor is still "inside CSS" / "inside a function").
5. **Budget bound**: once the distance past the dirty point reaches the
   convergence budget, stop and *retain* the dirty marker so the next render
   resumes from here — spreading pathological reparses across frames.
6. **Splice**: retain cached spans outside the reparsed range, splice in the new
   spans, merge/sort, extend the cache range, and clear the tail state.

(The WASM engine mirrors this as its own convergence walk.)

### Edits: insert/delete notifications

On insert/delete the engine:
- Shifts checkpoint markers via the marker list's insert/delete adjustment.
- Sets the dirty point to the minimum of the existing dirty point and the edit
  position.
- Shifts / clamps / drops cached span byte offsets **in place** (no reparse) so
  the cache stays usable until the next render.
- Clears the cache tail state when the edit lands before the cache end.

A subtlety guarded by tests: the full-parse and forward-extension paths never
commit cache state *past the last newline*, recomputing spans on a trailing
partial line next pass — this fixes a streaming-`Diff`-grammar artifact where
end-of-input had already popped `markup.inserted`. **[impl]**

### No background threads — bounded synchronous work

There is **no async / background-thread highlighting.** All highlighting is
synchronous, on the render thread, on demand per viewport-highlight call. The
only shared-concurrency type is a read-only shared `SyntaxSet`. Scalability is
achieved by (a) viewport-only parsing for huge files, (b) whole-file-cache-then-
filter for small files, (c) forward extension on scroll, and (d) the convergence
budget spread across frames — *not* by offloading to a thread. The checkpoint
design doc explicitly **rejected** the Sublime-style background-thread model as
"more complex for marginal benefit over convergence." **[impl]** **[flag]**: the
tree-sitter backend's module doc says it "must work instantly when loading a 1GB
file"; the syntect engine silently returns empty spans if a single parse range
would exceed the large-file threshold — the 1 GB claim holds only because
parsing is viewport-windowed, not because the engine can parse 1 GB.

---

## 4. The tree-sitter backend (the highlighter)

The tree-sitter backend is used when the engine resolves to tree-sitter.
Viewport-only parsing with a per-viewport highlight cache (storing categories,
not colors). On a cache miss it slices a context margin around the viewport,
runs the tree-sitter highlighter, and walks the event stream keeping a
**highlight stack** — collapsing to a single `Option` strips the parent capture
off `Source` events after a closing inner capture (the `` `${expr}` ``
template-literal case). The large-file threshold bounds a single parse;
over-large ranges return empty. Categories come from the languages crate:

- The highlight configuration is built from grammar-crate query **constants**
  (`HIGHLIGHT_QUERY` / `HIGHLIGHTS_QUERY` / `LOCALS_QUERY`), not `.scm` files —
  except **Templ**, whose `highlights.scm` is vendored (the only `.scm` in the
  crate). TS concatenates the TS + JS queries; Templ concatenates Go + Templ.
- A capture-index map turns the capture index into a `HighlightCategory` via a
  default-index table or a TypeScript-index table (TS has extra builtins).
  **[impl]**

---

## 5. HighlightCategory → theme color mapping

`HighlightCategory` is defined in both the languages crate and the editor
primitives (the editor re-exports the languages-crate one): a fixed set of
variants — `Attribute, Comment, Constant, Function, Keyword, Number, Operator,
PunctuationBracket, PunctuationDelimiter, Property, String, Type, Variable,
VariableBuiltin` plus three diff variants `Inserted, Deleted, Changed`.

Two mapping layers:

1. **Scope/capture → category.**
   - syntect: a scope-to-category function — a long `starts_with` cascade over
     TextMate scope strings (comment/string/markup/diff/keyword/punctuation/
     entity/storage/constant/variable…), memoised per scope.
   - tree-sitter: the capture-index tables in the languages crate.
2. **Category → color.** Foreground and background lookups map a category against
   the active theme (mirrored across both engines). Foreground maps to the
   theme's `syntax_*` fields (e.g. `Keyword`→`syntax_keyword`,
   `String`→`syntax_string`, `Number`/`Constant`/`Attribute`→`syntax_constant`,
   `Property`→`syntax_variable`). A category→theme-key string drives the theme
   inspector.

### Diff categories (background wash)

`Inserted`/`Deleted`/`Changed` are produced from syntect's bundled `Diff`
grammar (`markup.inserted/deleted.diff`, `meta.diff.range.unified`). They are
**background-fill** categories: the background lookup returns the theme's
diff add/remove/modify backgrounds, the foreground stays at the editor
foreground, and these categories report that their background extends to the
line end so the renderer paints the whole row even past the scope's
end-of-line. This reuses the same colors live-diff / side-by-side diff use.
**[impl]**

Caching stores **categories, not colors**, in both engines, so switching themes
re-resolves colors without reparsing.

---

## 6. How highlights reach the renderer

The integration point is the per-frame decoration context in the split-rendering
overlay path. In order:

1. **Syntax spans** — the highlighter's viewport-highlight call. The render range
   is the viewport ± one viewport-size for multi-line context. It returns
   highlight spans (range, color, background, category).
2. **Reference (same-symbol) overlays** — the reference-highlight overlay update.
3. **Bracket overlays** — the bracket-highlight overlay update. The
   comment/string spans from step 1 are collected, sorted, and passed as
   bracket-skip ranges so brackets inside prose/data are not matched or
   colorized.
4. **Semantic-token overlays** (LSP) are converted from overlays into highlight
   spans and merged.

Syntax spans are foreground colors applied per cell; reference and bracket
results are **overlays** (marker-backed, so they track edits) layered on top by
priority in the render loop. **[impl]**

---

## 7. Reference (same-symbol) highlighting

The reference highlighter highlights every occurrence of the identifier under
the cursor within the viewport. The public entry takes the buffer, cursor,
viewport bounds, and a context margin and returns highlight spans, with a
**three-tier fallback**:

1. **Locals (scope-aware)** when a locals query is available — runs a
   per-language tree-sitter `locals` query, resolves the cursor target's
   definition by walking containing scopes innermost-first, highlights the
   definition + references inside its scope, and suppresses shadowed names.
   Queries exist for **Rust, Python, JS, TS, Go, C, C++**.
2. **Tree-sitter identifier match** when tree-sitter is available — collects
   identifier nodes and filters by text equality (not scope-aware).
3. **Text matching** otherwise — whole-word matching over the viewport, guarded
   by a bounded search range (about 1 MiB) and a minimum word length.

A **pure-text, WASM/no-tree-sitter fallback** highlighter implements the same
word-match algorithm with no tree-sitter dependency; the engine falls back to it
when grammars aren't compiled in. The primitives are stateless and recomputed
each call (no cache).

The **debounce and overlay lifecycle live in the view layer**, not the
primitive: the reference-highlight overlay debounces by a fixed delay (on the
order of 150 ms) — overlays for the previous word stay (auto-adjusting via
markers) until the cursor rests on a new word, then highlights are recomputed and
background overlays created with theme key `ui.semantic_highlight_bg`. Toggled by
the `highlight_occurrences` editor config. **[impl]**

---

## 8. Bracket matching & rainbow colorization

Bracket highlighting is **not** in the highlighting primitives — it lives in the
view layer as a bracket-highlight overlay. Two overlay namespaces:
`bracket-highlight` (the matching pair under the cursor) and
`bracket-colorization` (rainbow by depth). **[impl]** **[flag]**: an earlier
scope note located bracket matching under primitives — it is actually a
view-layer overlay.

- **Pairs**: `()`, `[]`, `{}`, `<>`.
- **Match under cursor**: if the cursor sits on a bracket, a matching-bracket
  scan walks forward/backward with a depth counter (chunked reads), bounded by a
  maximum search size (about 1 MiB) so huge files don't hang. Both the cursor
  bracket and its match get a foreground overlay, colored by nesting depth when
  rainbow is on.
- **Rainbow colorization**: scans the viewport (+ one viewport-size of lead-in
  for correct depth), pushing/popping a bracket stack and emitting a
  depth-colored overlay per bracket, cycling through the theme's
  bracket-rainbow colors.
- **Skip ranges**: brackets inside comment/string spans are excluded from
  matching, depth, and colorization via a binary search over the sorted skip
  ranges supplied by the decoration context. **[impl]**

These are recomputed only when the cursor or theme changes; fully synchronous.

---

## 9. Implemented vs planned summary

**Implemented:** syntect-first highlighting with marker-based checkpoints +
convergence-based incremental re-highlight (v2); whole-file-cache (Phase 1),
forward-extension (Phase 3), partial-update, and cold-start paths; viewport-
windowed parsing for files over the large-file threshold; tree-sitter backend
for JS/TS/JSON(C)/Go/Templ with stack-correct event handling; build-time syntect
packdump + runtime user/pack/bundle/plugin grammar layering; shebang/first-line
detection; three-tier reference highlighting + WASM text fallback (debounced at
the view layer); bracket matching + rainbow colorization with comment/string
skip ranges; diff bg categories; theme-independent (category) caching.

**Planned / not present:** background-thread highlighting (explicitly rejected in
the checkpoint design doc); per-line (every-`\n`) state caching (rejected in
favor of byte-interval + markers); bracket matching is *not* tree-sitter-aware
(it's a byte scan with syntax-derived skip ranges, not AST-pair matching).

**Discrepancies flagged:** (1) the oxc-based plugin crate is plugin tooling, not
a highlighter; (2) bracket matching is in the view layer, not the highlighting
primitives; (3) the tree-sitter backend's "1 GB file" claim relies on viewport
windowing, not raw throughput; (4) the runtime and WASM TextMate engines are two
separate implementations of the same design.
