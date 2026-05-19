# Plan: scalable diff folding + principled diff syntax highlighting

## Context

The git-log detail panel is a file-backed buffer streaming `git show
--patch` output (see `PLAN-git-log-streaming.md`). Two features are
partially landed but don't scale and don't use existing infra
correctly:

1. **Folding by file / hunk** — currently scans the entire buffer
   text once after the spawn settles and calls `addFold` per range,
   pre-collapsing them. Doesn't scale to a 2 GB diff and bypasses the
   existing toggleable fold path (`apply_folding_ranges_response` →
   `state.folding_ranges` → standard `toggle_fold` keybinding).
2. **Diff highlighting** — currently uses plugin-side per-line
   `addOverlay`s, gated at 256 KB. Same approach `live_diff.ts`
   uses, but the editor already runs syntect over the buffer and
   syntect's bundled `Diff` grammar already scopes `+`/`-`/`@@`
   lines. We're paying overlay cost for something syntect would do
   for free if the theme/render path knew how to honour the scopes.

This plan covers both.

---

## Part 1: Incremental, scalable folding

### Goal

Per-file and per-hunk folds available on any diff up to "as large as
the buffer-backing file allows", with cost proportional to *bytes
the user has actually visited*, not total buffer size.

### Constraints

- The host's `state.folding_ranges` is line-indexed
  (`lsp_types::FoldingRange { start_line, end_line }`), not byte-
  indexed. We need real line numbers, which we have for the streamed
  buffer because `extend_streaming` now counts newlines per
  appended chunk.
- `setFoldingRanges` replaces the prior set — there is no append
  primitive. For incremental publication we either re-publish a
  superset on each extension, or add a `mergeFoldingRanges` host
  primitive.
- For a 2 GB diff with thousands of hunks, the published range set
  itself is small (~tens of KB of struct data). The expense is the
  byte-walk that discovers the headers.

### Proposed design

**Three-layer incremental scan**:

1. **Per-`extend_streaming` head scan** (driven by polling, while
   git is writing).
   - Each call already knows `(old_size, new_size)`. The plugin
     reads only `[old_size, new_size)` via `getBufferText`, runs the
     header detector on it, and accumulates the new headers'
     positions into a JS-side `Map<sha, Header[]>`.
   - Cost per refresh: O(bytes appended). For a 2 GB final-size
     buffer streaming at ~10 MB/s, each 200 ms tick processes
     ~2 MB and finds maybe a few dozen headers.

2. **Range materialisation** (incremental, append-only).
   - Each newly-detected `diff --git` opens a file range whose end
     is `unknown`. Each newly-detected `@@` opens a hunk range
     similarly. When the *next* same-or-higher-level header arrives,
     the prior range's end becomes the new header's line minus one.
   - Closed ranges are appended to the JS-side range list and the
     plugin re-publishes via `setFoldingRanges`. (See "host
     primitive" below for the append alternative.)

3. **Viewport-driven validation** (optional, only when scrolling
   into never-touched territory).
   - If the user scrolls to a region the spawn never visited (e.g.
     opens an already-cached commit on next boot), the plugin
     doesn't know its fold structure. On scroll, the plugin asks
     `getBufferText` for the new range and runs the same detector.
   - The trigger is a `cursor_moved` / `viewport_changed` event
     (the plugin runtime already broadcasts the former). Plugin
     dedupes against already-scanned byte ranges.

### Host primitives needed

`SetFoldingRanges` (already landed in this branch) is enough if the
plugin re-publishes the full list each time. For very-large diffs
that's still cheap on the host side (a `Vec<FoldingRange>` of a few
thousand entries copied across the channel per refresh, ~hundreds of
KB at the worst).

If we want true append, add a sibling:

```rust
// fresh-core/src/api.rs
ExtendFoldingRanges {
    buffer_id: BufferId,
    ranges: Vec<lsp_types::FoldingRange>,
}
```

Handler: same path as `SetFoldingRanges` but
`folding_ranges.extend_from_lsp(...)` rather than
`folding_ranges.set_from_lsp(...)`. ~10 LOC if
`LspFoldRanges::set_from_lsp` is split into a clear-then-extend
shape.

Recommendation: ship `SetFoldingRanges` re-publication first,
profile, only add `ExtendFoldingRanges` if the re-publish cost
becomes a problem in practice.

### Plugin-side state

```ts
interface FoldScanState {
  // Bytes already scanned for headers. Allows the viewport-driven
  // path to skip duplicates and the streaming path to resume.
  scannedTo: number;
  // In-progress ranges: ranges where the closing header hasn't
  // arrived yet. Indexed by depth (0 = file, 1 = hunk).
  openFile: { startLine: number } | null;
  openHunk: { startLine: number } | null;
  // Closed ranges, published as setFoldingRanges' set.
  closed: lsp_types::FoldingRange[];
}
// One per buffer (per commit hash):
const foldScans: Map<number /* bufferId */, FoldScanState> = new Map();
```

### Edge cases

- **First line of the buffer is `diff --git`** (no commit message): the
  detector sees the header and opens a range immediately. No special
  case needed.
- **`@@` outside a `diff --git` section** (malformed): close the hunk
  at next header or at EOF; never open if no file is in flight. Worst
  case the hunk fold is at the file level.
- **Cache-hit revisits** (file already fully on disk): the first
  refresh sees `old_size = 0, new_size = file_size`. The streaming
  path handles this naturally — one giant scan. For 2 GB that's a
  one-shot 2 GB read which we don't want. Mitigation: cap each scan
  pass at, say, 4 MB and continue on the next tick / on viewport
  scroll. The plugin can drive this with a `while (scannedTo < total)
  { await editor.delay(0); scan_chunk(); }` loop that yields between
  chunks.

### Out of scope (for now)

- **Word-level diff** (live_diff has this). Not part of the structural
  fold story.
- **Folding lock-files / minified diffs by default**. Could be a
  follow-up that uses the same detector — when a file's diff exceeds
  N lines, auto-publish a "collapsed by default" hint. Requires a new
  field on the fold range or a separate `addFold` call alongside.

---

## Part 2: Syntect-driven diff highlighting

### Current state, mapped honestly

- Syntect's bundled `Diff` grammar **is loaded**
  (`SyntaxSet::load_defaults_newlines()` in
  `primitives/grammar/types.rs:1497`). It scopes:
  - `markup.inserted.diff` on `+` lines
  - `markup.deleted.diff` on `-` lines
  - `meta.diff.range.unified` on `@@` lines
  - `meta.diff.header.from-file` / `to-file` on `---` / `+++`
  - `meta.diff.header.git` on `diff --git`
- The editor's renderer asks syntect for spans, then maps each scope
  string to a `HighlightCategory` via `primitives/highlight_engine.rs`
  `scope_to_category`. That function **does not match any
  markup.inserted/deleted/meta.diff scope**, so they fall through to
  `None` and get no styling.
- `HighlightCategory` (`primitives/highlight_engine.rs`) is a tiny
  enum (Keyword/String/Comment/Function/Type/Variable/Constant/
  Operator/PunctuationBracket/PunctuationDelimiter). Each maps 1:1
  to a foreground colour in `Theme::syntax_*`. **There is no
  background-colour pathway** — every category is fg-only.
- Diff highlighting is fundamentally a **background** affair: the
  whole `+` line gets a green wash, the whole `-` line gets red.
  The `+`/`-` token itself could be coloured fg too, but the per-
  line wash is what's expected from the UX.

So the problem decomposes into:
1. Teach `scope_to_category` (or its replacement) about markup.*
   diff scopes.
2. Give the category system a way to carry a **background colour**,
   not just fg.
3. Map the new categories to theme keys, and add those keys to the
   theme schema + every built-in theme JSON.
4. At render time, apply per-token bg via the existing styling
   machinery — but **scoped to the whole line**, not just the `+`
   token's column. Syntect's `Diff` grammar scopes the *full line*
   under `markup.inserted.diff`, so this falls out for free if the
   renderer honours bg from the highlight span.

### Proposed design

Three layers, each independently shippable:

#### 2.1 `HighlightCategory` gains a `bg` variant

Today:

```rust
pub enum HighlightCategory {
    Keyword, String, Comment, Function, Type, Variable, Constant,
    Operator, PunctuationBracket, PunctuationDelimiter,
}
```

Two paths:

- **A** (smaller): Add fixed variants:
  ```rust
  Inserted, Deleted, Changed,
  ```
  and a parallel "category → background colour" lookup in `Theme`.
  Each new variant has a fg too (typically darker green/red), so the
  existing fg-lookup keeps working; we add a separate `Theme::bg_for`
  that returns `Option<Color>` per category.

- **B** (more general): Generalise the enum to carry an *optional*
  bg per variant, computed at theme-resolution time. Existing
  variants get `None`; new ones get `Some(theme.diff_add_bg)` etc.
  More invasive but doesn't constrain future markup support
  (markup.bold, markup.italic etc.).

Recommendation: **A**. The diff case is well-bounded; broader markup
support is a separate, larger conversation about what the editor's
syntax model wants to be.

#### 2.2 `scope_to_category` learns the diff scopes

In `primitives/highlight_engine.rs:52` after the existing
`markup.*` arms:

```rust
if scope_lower.starts_with("markup.inserted") {
    return Some(HighlightCategory::Inserted);
}
if scope_lower.starts_with("markup.deleted") {
    return Some(HighlightCategory::Deleted);
}
if scope_lower.starts_with("markup.changed")
   || scope_lower.starts_with("meta.diff.range")
{
    return Some(HighlightCategory::Changed);
}
// `meta.diff.header.*` — files / hashes / "diff --git". Keep them
// at category::Type or category::Keyword for visual distinction.
if scope_lower.starts_with("meta.diff.header") {
    return Some(HighlightCategory::Type);
}
```

That's ~12 LOC.

#### 2.3 Theme schema gains bg keys

`view/theme/types.rs` already has `diff_add_bg`, `diff_remove_bg`,
`diff_modify_bg` in `editor.*` (used by `live_diff` / side-by-side
diff). Two reasonable choices:

- **Reuse the existing keys** under their current names. The
  `Theme::bg_for(category)` lookup returns
  `theme.editor.diff_add_bg` for `Inserted` etc. No theme JSON
  changes needed — every shipped theme already has them.
- **Add dedicated `syntax.*` keys**. Cleaner separation between
  "the renderer's own diff display" (editor.diff_*) and "syntect-
  scoped markup highlighting" (syntax.markup_inserted_bg etc).
  Requires updating all 14 theme JSON files.

Recommendation: **reuse**. The colours are the same, the only
difference is *who* applies them. Saves ~50 LOC of theme schema
churn and keeps the user's customisation surface small.

#### 2.4 Render path honours bg from the highlight span

The highlight pipeline already produces `HighlightSpan { range, fg,
bg, bold, italic, ... }` triples (the runtime shape, not the public
plugin one). Today `bg` is essentially always `None`. After 2.1/2.2,
the bg is `Some(theme.diff_add_bg)` for `Inserted` etc.

Render-side change: when emitting cells, if the span has `bg`,
apply it. This is the **`extend_to_line_end` behaviour** in
`addOverlay` — without it the bg only fills the columns the token's
glyphs occupy, leaving the rest of the line uncoloured. Two
options:

- **Per-cell**: leave bg gaps. Simpler, but the `+` line shows a
  thin green strip the width of "+" (one column), not the full
  line. Wrong UX.
- **Whole-line**: when the span covers a complete logical line
  (which syntect's `Diff` does — `markup.inserted.diff` scopes
  `+...$` including the newline), the renderer fills the bg out
  to the viewport's right edge for that row.

Recommendation: **whole-line for bg, conditional on a per-category
flag** (`HighlightCategory::extends_to_line_end()` returns true for
Inserted/Deleted/Changed, false for everything else). Keeps the
existing fg-only behaviour for keywords/strings/etc.

### Where syntect runs (or doesn't) for our streamed buffer

There's a subtlety: the editor currently runs syntect lazily on
visible viewport. For an `Unloaded` chunk (the streamed buffer's
mid-buffer pieces), syntect can't tokenise until the bytes are
loaded. `chunk_split_and_load` materialises chunks on demand
(viewport read), so syntect runs on each chunk as it's first
viewed.

**Implication**: highlighting is **already incremental** for free
once 2.1–2.4 land. The user scrolls into a hunk, the chunk loads,
syntect runs over the chunk, and the diff lines get coloured. No
per-buffer "scan everything" pass is needed — that's the
"principled, efficient" part of the user's question.

### Where syntect's diff grammar might fall short

- **Trailing context lines** (whitespace-prefix ` ` rows) inside a
  hunk: syntect's Diff doesn't scope them. They render with the
  default bg, which is what we want — they're context, not changes.
- **`No newline at end of file`** marker (`\`-prefixed): not
  scoped. Renders default. Fine.
- **Word-level highlighting inside `+`/`-` lines**: syntect can't do
  this — it's a per-line scope, no character-level diff info. Live
  diff's word-range overlays remain the canonical mechanism if we
  want that later.

### Estimated LOC

| Area | Files | LOC |
|---|---|---|
| `HighlightCategory` new variants + bg lookup | `primitives/highlight_engine.rs`, `view/theme/types.rs` | ~40 |
| Scope mapping | `primitives/highlight_engine.rs` | ~12 |
| Render-side whole-line bg fill | `view/ui/split_rendering/*.rs` | ~30 |
| Plugin-side overlay code removal (`applyDiffHighlights`) | `plugins/git_log.ts` | -80 |
| Tests (syntax highlighting coverage for diff, theme key resolution) | `tests/e2e/syntax_highlighting_coverage.rs`, `tests/theme.rs` | ~40 |

Total: **~120 LOC net** of host changes, plus a ~80 LOC reduction
in plugin code.

### Migration & rollout

The two features are orthogonal — both can land in either order:

- **Folding first** is lower-risk: changes are localised to the
  plugin + one new host command. Doesn't touch the render pipeline.
- **Highlighting first** is higher-impact: every `.diff` file in
  the editor (live_diff side-by-side, git_log detail, any plain
  `.diff` open) immediately gets coloured. But touches the render
  path so the blast radius is larger.

Ship them as two PRs:

1. **PR A (folding)**: incremental scan in the plugin + the
   `SetFoldingRanges` host primitive (already landed). Plugin
   re-publishes growing range set per refresh; viewport-driven
   filling for cache-hit revisits. Drop `addFold` calls; the user
   gets toggleable folds via the standard fold key.

2. **PR B (highlighting)**: `HighlightCategory` variants + scope
   mapping + theme reuse + whole-line bg render path. Removes the
   plugin's `applyDiffHighlights` overlay code. Adds coverage tests
   so a future grammar/theme change doesn't silently break diff
   colours.

### Open questions

1. **Is whole-line bg fill already supported in another category?**
   If yes, factor that pathway out and reuse. If no, we're adding a
   new render-time behaviour and should think about which other
   future scopes might want it (markup.heading background tinting?
   etc.).
2. **For `--stat` rows** (` src/main.rs | 2 +-`): syntect scopes
   these as `meta.diff.range.unified`-ish in some grammars. Decide
   whether they get bg highlight or stay plain.
3. **Theme schema vs. live_diff**: live_diff uses `editor.diff_*_bg`
   *and* `editor.diff_*_collision_fg`. After this work, syntect-scoped
   diff highlighting uses the same bg but **probably wants its own fg**
   (so the `+` token itself is greener, distinct from context). Add
   `editor.diff_add_fg_syntect` (ugly) or rename the existing keys
   (breaks user themes) or just reuse the live_diff fg (simplest).
   Recommendation: reuse.
