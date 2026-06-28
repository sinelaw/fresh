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
dependency itself; tree-sitter grammars come through the `fresh-languages` crate.

The dispatch type is the `HighlightEngine` enum
(`crates/fresh-editor/src/primitives/highlight_engine.rs:253`):

```
enum HighlightEngine { TreeSitter(Box<Highlighter>), TextMate(Box<TextMateEngine>), None }
```

`HighlightEngine::from_entry(&GrammarEntry, &GrammarRegistry)`
(`highlight_engine.rs:1277`) is the single place the **"prefer syntect, else
tree-sitter, else None"** rule lives — picks syntect if the catalog entry has a
syntect index, else tree-sitter if it has a `fresh_languages::Language`, else
`None`. `for_file` and `for_syntax_name` are thin wrappers
(`highlight-engine-cleanup.md` documents this; the dead
`HighlighterPreference`/`for_language` API has been removed). **[impl]**

### Why this split (mined from git history)

- **syntect covers breadth cheaply.** ~115 built-in/embedded grammars ship in a
  build-time binary dump; adding a language is a `.sublime-syntax` file, no parse
  table linked. The language selector was deliberately switched from the
  18-variant tree-sitter `Language` enum to "all syntect syntaxes (100+)" in
  commit `d0df8e5d3` ("use syntect grammars for language selection instead of
  tree-sitter enum"). **[impl]**
- **tree-sitter is heavy.** Commit `898f36e53` ("bundle only must-have grammars,
  ~18 MB smaller binary") dropped 14 tree-sitter grammars — default release
  binary 43.9 MB → 25.8 MB. Only grammars syntect *can't* highlight are kept.
- **Bundled tree-sitter set** (`crates/fresh-languages/Cargo.toml`,
  `bundled-languages` feature = the editor's `tree-sitter` feature): **JavaScript,
  TypeScript, JSON (also serving JSONC), Templ, and Go** (Go only because Templ's
  grammar extends it). Reasons:
  - **TypeScript**: syntect ships no TS grammar → tree-sitter only.
  - **JavaScript**: syntect's JS grammar leaks template-literal state past the
    closing backtick and paints the rest of the file as a string (issue #899);
    commit `7db007b40` routes JS through tree-sitter by *skipping* the syntect
    "JavaScript" entry when building the catalog. (syntect's JS grammar is still
    reachable by name for markdown-popup code highlighting.)
  - **JSONC**: no JSONC tree-sitter crate exists; the JSON grammar recovers past
    comments/trailing commas well enough (`fresh-languages/src/lib.rs:282`).
  - **Templ**: Go + components/HTML/CSS (issue #463 test).
  The `tree-sitter-all` editor feature re-enables the full grammar set for users
  who want AST indent / scope-aware refs for everything. **[impl]**
- tree-sitter is *also* retained for non-highlighting structural features even on
  syntect-highlighted buffers: precise indentation, scope-aware reference
  highlighting, semantic highlighting. `TextMateEngine` keeps a
  `ts_language: Option<Language>` purely for that (`highlight_engine.rs:273`,
  exposed via `language()`); it never produces spans.

### What `fresh-parser-js` (oxc) is — and is NOT

`crates/fresh-parser-js/` is **oxc**-based (`oxc_parser`, `oxc_allocator`,
`oxc_span`, `oxc_ast`, `oxc_transformer`, `oxc_codegen`, `oxc_semantic`,
`oxc_isolated_declarations`) but **has nothing to do with syntax highlighting.**
It is the plugin toolchain: TypeScript→JS transpile, `.d.ts` emission, plugin
dependency extraction + topological load ordering, and ES-module bundling so
plugin source (incl. config-as-code `init.ts`) can run in the QuickJS plugin
runtime. JS/TS *buffers* are highlighted by the tree-sitter grammars in
`fresh-languages`. The only editor-side caller is a CLI plugin-check helper
(`crates/fresh-editor/src/main.rs:1846`); the real consumer is
`fresh-plugin-runtime`. The oxc deps in `fresh-editor/Cargo.toml` (lines 144-146)
are pulled by the `plugins` feature to syntax-check `init.ts`, not for
highlighting. **[impl]** **[flag]**: the oxc deps may *look* highlighting-related
but are not.

---

## 2. Grammar loading & language detection

### Grammar catalog and registry

`GrammarRegistry` (defined in `primitives/grammar/types.rs`; builder/factory impls
in `primitives/grammar/loader.rs`) owns the unified catalog. Each `GrammarEntry`
records which engines can serve a language (syntect index and/or tree-sitter
`Language`). Three lookup methods:

- `find_by_path(path, first_line)` — filename → glob → extension, then
  syntect first-line regex, then shebang (`primitives/grammar/shebang.rs`).
- `find_by_name(name)` — display name / id / short alias, case-insensitive.
- `find_by_extension(ext)`.

### Build-time syntect dump (no runtime `.sublime-syntax` parsing on the hot path)

The `.sublime-syntax` files under `crates/fresh-editor/src/grammars/` are
**compiled at build time** into a binary `SyntaxSet` dump loaded via
`include_bytes!(concat!(env!("OUT_DIR"), "/default_syntaxes.packdump"))` and
`syntect::dumps::from_uncompressed_data` (`loader.rs:160-179`). A fast path
(`loader.rs:155-171`) uses the packdump directly when there are no user / language-
pack / bundle / plugin grammars; otherwise `into_builder()` layers those on and
rebuilds (`loader.rs:172-284`). Grammar provenance is tracked
(`GrammarSource::{User, LanguagePack, Bundle, Plugin}`). **[impl]**

### `DetectedLanguage` — the per-buffer source of truth

`primitives/detected_language.rs:18` — a struct (not an enum) bundling
`name` (canonical LSP/config id), `display_name` (matches syntect syntax name),
`highlighter: HighlightEngine`, and `ts_language: Option<Language>` so the
highlighter and language state stay in sync through one type. `from_path`
resolves: filename → glob → extension → catalog → shebang/first-line → config
fallback. The LSP/config language id is resolved **independently** of the grammar
catalog (`services::lsp::manager::detect_language`) so the config id is correct
even when the grammar registry is empty. **[impl]**

### Shebang detection

`primitives/grammar/shebang.rs` — final fallback in `find_by_path` when
filename/extension and syntect's first-line regex all miss (issue #2357, e.g.
fish/Lua/PowerShell have no first-line regex). Handles `env` indirection
(`-S`/`-i`/`VAR=val`) and strips version suffixes (`python3.11`→`python`).
tree-sitter-backed targets reuse `fresh_languages::Language::id()`; syntect-only
ones use named consts. `awk`/unknown → plain text. **[impl]**

---

## 3. The syntect checkpoint / incremental engine (`TextMateEngine`)

Syntect's parser is a **sequential state machine**: it must process bytes in
order from a known `(ParseState, ScopeStack)` to correctly track multi-line
constructs (comments, strings) and embedded-language transitions (CSS-in-HTML,
code-in-markdown-fences). The engine makes scrolling and editing cheap with a
span cache + periodic parse-state checkpoints + convergence-based incremental
re-highlight. This is the **implemented v2 design**; the source doc
`syntax-highlighting-checkpoint-design.md` describes both the superseded v1
(`Vec<ParseCheckpoint>` at ~4 KB, discard-after-edit) and the v2 plan the code
now realizes.

### Runtime vs WASM engines

There are **two parallel implementations** with the same design:

- `primitives/highlight_engine.rs` — the **runtime** engine (`TextMateEngine`,
  `highlight_engine.rs:264`). Used by the editor.
- `primitives/textmate_engine.rs` — a sibling **WASM-compatible** engine
  (`TextMateEngine`, `textmate_engine.rs:33`), explicitly mirroring the runtime
  design (its doc points to `highlight_engine.rs` for the detailed write-up).
  `DetectedLanguage` references the `highlight_engine.rs` type. **[impl]**

The rest of this section describes the runtime engine.

### State stored

`TextMateEngine` (`highlight_engine.rs:264`) holds:

- `syntax_set: Arc<SyntaxSet>`, `syntax_index: usize` — the resolved grammar.
- `checkpoint_markers: MarkerList` — checkpoint *positions* as markers in Fresh's
  AVL interval-tree marker system; they auto-shift O(log n) on every edit so they
  never go stale.
- `checkpoint_states: HashMap<MarkerId, (ParseState, ScopeStack)>` — the saved
  parser snapshot per checkpoint.
- `dirty_from: Option<usize>` — earliest byte touched since last render.
- `cache: Option<TextMateCache>` — the span cache.
- `last_buffer_len`, `ts_language`, `stats`, `scope_category_cache` (memoised
  scope→category; never invalidated because syntect Scope atoms are append-only).

`TextMateCache` (`highlight_engine.rs:296`): `range: Range<usize>`, `spans:
Vec<CachedSpan>` (each is `range + HighlightCategory`, **theme-independent** so
theme changes don't invalidate), and `tail_state: Option<(ParseState,
ScopeStack)>` — parse state at `range.end`, powering forward extension; `None`
when the last mutation didn't end at `range.end`.

A **checkpoint** = a marker (byte offset, auto-shifted) + a full
`(ParseState, ScopeStack)` snapshot keyed by `MarkerId`.

### Constants

- `MAX_PARSE_BYTES = 1024 * 1024` (1 MiB) — `highlight_engine.rs:311`. Small/large
  file threshold (= `LARGE_FILE_THRESHOLD_BYTES`).
- `CHECKPOINT_INTERVAL = 256` bytes — `highlight_engine.rs:314`. Checkpoints land
  at the first line boundary after each 256-byte interval (~every few lines). A
  200 KB file ≈ 800 markers, within the marker tree's comfort zone. 256 was chosen
  over 4 KB for 16× faster convergence at 16× more markers
  (`syntax-highlighting-checkpoint-design.md`).
- `CONVERGENCE_BUDGET = 64 * 1024` (64 KiB) — `highlight_engine.rs:318`. Per-pass
  cap on how far past the dirty point a partial update parses, so a pathological
  edit (e.g. opening an unclosed `/*`) can't degenerate into a whole-file reparse.

### The four render-time paths

`TextMateEngine::highlight_viewport` (`highlight_engine.rs:584`) selects:

1. **Cache hit** (`:614`) — cache covers the viewport, no dirty, buffer length
   unchanged → `filter_cached_spans` (`:676`). Zero parse work. For files ≤
   `MAX_PARSE_BYTES`, the first render parses the whole file (`0..len`) so every
   later scroll is filter-only (Phase 1, commit `1a9dc1e44` — profiling showed 63%
   of scroll CPU was full re-parse before this).
2. **Forward extension** (`:620`) — no dirty, cache covers the viewport start but
   `cache.range.end < parse_end` and `tail_state` present → `extend_cache_forward`
   (`:868`): resume from `tail_state`, parse only the uncovered tail bytes (Phase
   3, commit `4b03f7c7c`). Steady-state scroll on large files.
3. **Partial update** (`:638`) — cache covers the viewport AND a dirty edit lies
   before `parse_end` → `try_partial_update` (`:700`). See below.
4. **Cold start / fallback** (`:664`) — `full_parse` (`:999`): resume via
   `find_parse_resume_point` (`:1141`) from the nearest checkpoint (or a fresh
   `ParseState` for large files with no nearby checkpoint, bounded by
   `MAX_PARSE_BYTES`), parse to `parse_end`, build a fresh cache.

For files **> `MAX_PARSE_BYTES`**, the parse range is a viewport window
`[viewport_start − context_bytes, viewport_end + context_bytes]` (`:593-599`);
scroll cost stays bounded via path 2. (The original 10 KB fixed window broke
embedded languages whose opening tag was >10 KB above the viewport — the whole
motivation for checkpoints.)

### Incremental re-highlight: convergence algorithm

`try_partial_update` (`highlight_engine.rs:700`), VSCode-style:

1. **Resume** (`:713`): pick the checkpoint marker with max start *before*
   `dirty_pos` (search bounded to `MAX_PARSE_BYTES`); clone its `(state, scopes)`.
   If none and the file fits, start fresh at byte 0; else return `None` → full
   parse.
2. **Markers ahead** (`:735`): collect checkpoint markers in `[dirty_pos,
   parse_end)`, sorted, as convergence candidates.
3. **Forward parse** (`:763`): parse line-by-line via `state.parse_line(...)`,
   creating new checkpoints every `CHECKPOINT_INTERVAL`.
4. **Convergence** (`:800`): when the running offset reaches a candidate marker,
   compare the *recomputed* `(state, scopes)` to the stored one. If **equal** →
   converged; everything downstream is still valid, stop (`convergences += 1`). If
   not → update the stored state and continue. Most single-char edits converge in
   1–2 checkpoints (you're still "inside CSS" / "inside a function").
5. **Budget bound** (`:823`): if `current_offset − dirty_pos >=
   CONVERGENCE_BUDGET`, stop and *retain* the dirty marker so the next render
   resumes from here — spreads pathological reparses across frames.
6. **Splice** (`:833-857`): retain cached spans outside the reparsed range, splice
   in the new spans, merge/sort, extend `cache.range.end`, set `tail_state = None`.

(`textmate_engine.rs` mirrors this as `run_convergence_walk`, `:283-409`.)

### Edits: `notify_insert` / `notify_delete`

`notify_insert` (`:436`) / `notify_delete` (`:458`):
- Shift checkpoint markers via `MarkerList::adjust_for_insert/delete`.
- Set `dirty_from = min(existing, position)`.
- Shift / clamp / drop cached span byte offsets **in place** (no reparse) so the
  cache stays usable until the next render.
- Invalidate `cache.tail_state = None` when the edit lands before `cache.range.end`.

A subtlety guarded by tests: `full_parse` and `extend_cache_forward` never commit
cache state *past the last newline* (`safe_offset`/`safe_state`), recomputing
spans on a trailing partial line next pass — fixes a streaming-`Diff`-grammar
artifact where end-of-input had already popped `markup.inserted` (commit
`e18ea1fc7`). **[impl]**

### No background threads — bounded synchronous work

There is **no async / background-thread highlighting.** All highlighting is
synchronous, on the render thread, on demand per `highlight_viewport` call. The
only shared-concurrency type is `Arc<SyntaxSet>` (read-only). Scalability is
achieved by (a) viewport-only parsing for huge files, (b) whole-file-cache-then-
filter for small files, (c) forward extension on scroll, and (d) the
`CONVERGENCE_BUDGET` spread across frames — *not* by offloading to a thread. The
checkpoint design doc explicitly **rejected** the Sublime-style background-thread
model as "more complex for marginal benefit over convergence."
**[impl]** **[flag]**: the module doc of `highlighter.rs` (the tree-sitter
backend) says "Must work instantly when loading a 1GB file"; `highlight_engine.rs`
silently returns empty spans if a single parse range would exceed `MAX_PARSE_BYTES`
(`highlighter.rs:172-180`) — the 1 GB claim holds only because parsing is
viewport-windowed, not because the engine can parse 1 GB.

---

## 4. The tree-sitter backend (`Highlighter`)

`primitives/highlighter.rs` — used when `HighlightEngine::TreeSitter`. Viewport-
only parsing with a per-viewport `HighlightCache` (`:93`, stores categories, not
colors). On cache miss it slices `±context_bytes` around the viewport, runs
`tree_sitter_highlight::Highlighter::highlight`, and walks the event stream
keeping a **highlight stack** (`:207`) — collapsing to a single `Option` strips the
parent capture off `Source` events after a closing inner capture (the
`` `${expr}` `` template-literal case, commit `c0801f5db`). `MAX_PARSE_BYTES`
(1 MiB) bounds a single parse; over-large ranges return empty (`:172`). Categories
come from `fresh-languages`:

- `Language::highlight_config()` (`fresh-languages/src/lib.rs:204`) builds a
  `HighlightConfiguration` from grammar-crate query **constants**
  (`HIGHLIGHT_QUERY` / `HIGHLIGHTS_QUERY` / `LOCALS_QUERY`), not `.scm` files —
  except **Templ**, whose `highlights.scm` is vendored
  (`fresh-languages/queries/templ/highlights.scm`, the only `.scm` in the crate).
  TS concatenates the TS + JS queries; Templ concatenates Go + Templ.
- `Language::highlight_category(idx)` maps the capture index to a
  `HighlightCategory` via `from_default_index` (14 captures) or
  `from_typescript_index` (23 captures, TS has extra builtins). **[impl]**

---

## 5. HighlightCategory → theme color mapping

`HighlightCategory` (defined in both `fresh-languages/src/lib.rs:23` and
`primitives/highlight_types.rs:14`; the editor re-exports the `fresh-languages`
one through `highlighter.rs`): 18 variants — `Attribute, Comment, Constant,
Function, Keyword, Number, Operator, PunctuationBracket, PunctuationDelimiter,
Property, String, Type, Variable, VariableBuiltin` plus three diff variants
`Inserted, Deleted, Changed`.

Two mapping layers:

1. **Scope/capture → category.**
   - syntect: `scope_to_category(scope)` (`highlight_engine.rs:52`) — a long
     `starts_with` cascade over TextMate scope strings (comment/string/markup/
     diff/keyword/punctuation/entity/storage/constant/variable…), memoised per
     scope (`scope_stack_to_category`, `:1187`).
   - tree-sitter: capture-index tables in `fresh-languages`.
2. **Category → color.** `highlight_color(category, theme)` and
   `highlight_bg(category, theme)` (`primitives/highlighter.rs:26,56`; mirrored in
   `highlight_types.rs:121,150`). Foreground maps to `theme.syntax_*` fields
   (e.g. `Keyword`→`syntax_keyword`, `String`→`syntax_string`,
   `Number`/`Constant`/`Attribute`→`syntax_constant`,
   `Property`→`syntax_variable`). The category→`theme_key()` string
   (`highlight_types.rs:61`) drives the theme inspector.

### Diff categories (background wash)

`Inserted`/`Deleted`/`Changed` are produced from syntect's bundled `Diff`
grammar (`markup.inserted/deleted.diff`, `meta.diff.range.unified`). They are
**background-fill** categories: `highlight_bg` returns
`theme.diff_{add,remove,modify}_bg`, foreground stays at `editor_fg`, and
`bg_extends_to_line_end()` (`highlight_types.rs:115`) is true for them so the
renderer paints the whole row even past the scope's end-of-line (commit
`0d28d3f7b`). This reuses the same colors live-diff / side-by-side diff use.
**[impl]**

Caching stores **categories, not colors**, in both engines, so switching themes
re-resolves colors without reparsing.

---

## 6. How highlights reach the renderer

The integration point is `decoration_context`
(`crates/fresh-editor/src/view/ui/split_rendering/orchestration/overlays.rs:86`),
called per render frame. In order:

1. **Syntax spans** — `state.highlighter.highlight_viewport(buffer,
   highlight_start, highlight_end, theme, context_bytes)` (`overlays.rs:109`).
   The render range is the viewport ± one viewport-size for multi-line context
   (`:103-107`). Returns `Vec<HighlightSpan>` (`range`, `color`, `bg`, `category`).
2. **Reference (same-symbol) overlays** — `reference_highlight_overlay.update(...)`
   (`:119`).
3. **Bracket overlays** — `bracket_highlight_overlay.update(...)` (`:150`). The
   comment/string spans from step 1 are collected, sorted, and passed as
   `bracket_skip_ranges` so brackets inside prose/data are not matched or
   colorized (issue #2405, `:136-147`).
4. **Semantic-token overlays** (LSP) are converted from overlays into
   `HighlightSpan`s and merged (`:166-181`).

Syntax spans are foreground colors applied per cell; reference and bracket
results are **overlays** (marker-backed, so they track edits) layered on top by
priority in the render loop. **[impl]**

---

## 7. Reference (same-symbol) highlighting

`primitives/reference_highlighter.rs` highlights every occurrence of the
identifier under the cursor within the viewport. Public entry:
`ReferenceHighlighter::highlight_occurrences(buffer, cursor, viewport_start,
viewport_end, context_bytes) -> Vec<HighlightSpan>` (`:333`). **Three-tier
fallback** (`:345-374`):

1. **Locals (scope-aware)** if `has_locals()` — runs a per-language tree-sitter
   `locals` query, resolves the cursor target's definition by walking containing
   scopes innermost-first, highlights the definition + references inside its scope,
   suppressing shadowed names. Queries exist for **Rust, Python, JS, TS, Go, C,
   C++** (`get_locals_query`, `:75`).
2. **Tree-sitter identifier match** if `has_tree_sitter()` — collects
   `(identifier) @id` nodes and filters by text equality (not scope-aware).
3. **Text matching** otherwise — whole-word `match_indices` over the viewport,
   guarded by `MAX_SEARCH_RANGE = 1 MiB` and `min_word_length` (default 2).

`primitives/reference_highlight_text.rs` is the **pure-text, WASM/no-tree-sitter
fallback** (`TextReferenceHighlighter`) — the same word-match algorithm with no
tree-sitter dependency; `set_language` falls back to it when grammars aren't
compiled in (`reference_highlighter.rs:210`). The primitives are stateless and
recomputed each call (no cache).

The **debounce and overlay lifecycle live in the view layer**, not the primitive:
`view/reference_highlight_overlay.rs` debounces by `DEFAULT_DEBOUNCE_MS = 150`
(`:15`) — overlays for the previous word stay (auto-adjusting via markers) until
the cursor rests 150 ms on a new word, then `apply_highlights` recomputes and
creates `OverlayFace::Background` overlays at priority 5 with theme key
`ui.semantic_highlight_bg`. (The "debounced" note in `overlays.rs:117` is
accurate — the debounce is here, not in the primitive.) Toggled by
`config.editor.highlight_occurrences`. **[impl]**

---

## 8. Bracket matching & rainbow colorization

Bracket highlighting is **not** in `primitives/` — it lives in
`view/bracket_highlight_overlay.rs` (`BracketHighlightOverlay`). Two overlay
namespaces: `bracket-highlight` (the matching pair under the cursor) and
`bracket-colorization` (rainbow by depth). **[impl]** **[flag]**: the SCOPE prompt
located bracket matching under primitives — it is actually a view-layer overlay.

- **Pairs**: `('(',')'), ('[',']'), ('{','}'), ('<','>')` (`:35`).
- **Match under cursor** (`update`, `:129`): if the cursor sits on a bracket,
  `find_matching_bracket` (`:318`) scans forward/backward with a depth counter
  (chunked reads of `BRACKET_SCAN_CHUNK = 16 KiB`), bounded by
  `MAX_BRACKET_SEARCH_BYTES = 1_000_000` so huge files don't hang. Both the
  cursor bracket and its match get a foreground overlay (priority 10), colored by
  nesting depth when rainbow is on.
- **Rainbow colorization** (`update_colorization`, `:403`): scans the viewport
  (+ one viewport-size of lead-in for correct depth), pushing/popping a bracket
  stack and emitting a depth-colored overlay (priority 6) per bracket; 6-color
  cycle from `theme.bracket_rainbow_1..6`.
- **Skip ranges** (issue #2405): brackets inside comment/string spans are excluded
  from matching, depth, and colorization via `pos_in_ranges` (binary search over
  the sorted skip ranges supplied by `decoration_context`). **[impl]**

These are recomputed only when the cursor or theme changes (`last_cursor_pos`
gate); fully synchronous.

---

## 9. Implemented vs planned summary

**Implemented:** syntect-first highlighting with marker-based checkpoints +
convergence-based incremental re-highlight (v2); whole-file-cache (Phase 1),
forward-extension (Phase 3), partial-update, and cold-start paths; viewport-
windowed parsing for >1 MiB files; tree-sitter backend for JS/TS/JSON(C)/Go/Templ
with stack-correct event handling; build-time syntect packdump + runtime user/
pack/bundle/plugin grammar layering; shebang/first-line detection; three-tier
reference highlighting + WASM text fallback (debounced at the view layer); bracket
matching + rainbow colorization with comment/string skip ranges; diff bg
categories; theme-independent (category) caching.

**Planned / not present:** background-thread highlighting (explicitly rejected in
the checkpoint design doc); per-line (every-`\n`) state caching (rejected for
byte-interval+markers); bracket matching is *not* tree-sitter-aware (it's a byte
scan with syntax-derived skip ranges, not AST-pair matching).

**Discrepancies flagged:** (1) oxc/`fresh-parser-js` is plugin tooling, not a
highlighter; (2) bracket matching is in `view/`, not `primitives/`; (3) the
tree-sitter backend's "1 GB file" claim relies on viewport windowing, not raw
throughput; (4) the runtime and WASM `TextMateEngine` are two separate
implementations of the same design.
