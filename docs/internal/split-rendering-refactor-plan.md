# `split_rendering.rs` Refactor Plan

## Context

`crates/fresh-editor/src/view/ui/split_rendering.rs` is **8,635 lines** in a
single file. A single `impl SplitRenderer` block spans ~5,975 lines (L922–L6896)
and an inline `mod tests` block adds another ~1,740 lines. Several methods are
individually enormous:

| Method | Lines | Notes |
|---|---|---|
| `render_view_lines` | ~1,246 | Per-line character/span loop; the hot path |
| `render_content` | ~696 | Top-level entry: tabs, splits, hit areas |
| `render_composite_buffer` | ~436 | Composite (side-by-side) renderer |
| `apply_wrapping_transform` | ~323 | Hard-cap + soft wrap |
| `compute_buffer_layout` | ~282 | Layout phase for one buffer |
| `compute_char_style` | ~199 | 7-layer style precedence |
| `decoration_context` | ~174 | Aggregates overlays/diagnostics/indicators |

## Goal

Break the file into cohesive, independent modules such that:

1. The file becomes navigable (no file >~1,300 lines outside the irreducible
   inner loop).
2. As many modules as possible are **self-sustaining** — they take typed,
   concern-scoped inputs and return typed outputs, with **no** reliance on a
   shared "mega struct".
3. The remaining coupled code (the orchestration layer) is physically
   quarantined in its own subdirectory, so the coupling is visible at the
   filesystem level.

## Where the coupling actually is

Most methods already take small, typed parameter lists. Shared-context coupling
is concentrated in only **five** places:

1. `SplitRenderer` — a zero-field unit struct used as a namespace for ~50
   methods. Will be dissolved.
2. `LineRenderInput` — 20+ fields. Used by exactly one function
   (`render_view_lines`). Will become private and split internally.
3. `LeftMarginContext` — used by exactly one function (`render_left_margin`).
   Co-located and kept private.
4. `CharStyleContext` / `CharStyleOutput` — used by exactly one function
   (`compute_char_style`). Co-located and kept private.
5. `SelectionContext` / `DecorationContext` — the *only* genuinely shared
   carriers. Produced by two builder functions and consumed by the three big
   render functions. Quarantined into `orchestration/`.

## Final directory layout

```
crates/fresh-editor/src/view/ui/split_rendering/
├── mod.rs                       re-exports the public API from orchestration/
│
│   ─── self-sustaining (no shared mega-structs) ───
├── spans.rs
├── style.rs
├── char_style.rs                private CharStyleContext / CharStyleOutput
├── base_tokens.rs
├── transforms.rs                wrap, soft_breaks, conceal, virtual_lines
├── view_data.rs
├── folding.rs                   FoldIndicator + fold/diff indicator builders
├── scrollbar.rs
├── layout.rs                    split layout, viewport sync, anchor, compose, separator
├── gutter.rs                    private LeftMarginContext
├── post_pass.rs                 osc8, hyperlinks, column guides, ruler bg, line bg
│
└── orchestration/               ─── shares SelectionContext / DecorationContext ───
    ├── mod.rs                   pub fn render_content, pub fn compute_content_layout
    ├── contexts.rs              SelectionContext, DecorationContext (data only)
    ├── overlays.rs              selection_context, decoration_context (producers)
    ├── render_line.rs           render_view_lines + private LineRenderInput split
    ├── render_buffer.rs         compute_buffer_layout, draw_buffer_in_split,
    │                            render_buffer_in_split, render_view_line_content
    └── render_composite.rs      render_composite_buffer
```

- **11 self-sustaining files** at the top level. None import the shared carriers.
- **5 orchestration files** plus `orchestration/mod.rs`. This is the only place
  that may import `SelectionContext` / `DecorationContext`.
- Top-level `mod.rs` is a thin façade that re-exports the public API.

## Visibility rules (enforced by convention + grep)

| File set | May import | May NOT import |
|---|---|---|
| Top-level (11 self-sustaining files) | stdlib, ratatui, crate primitives, `ViewLine`, `Theme`, `Buffer`, `EditorState`, overlays, etc. | **Never** `orchestration::*`, **never** `SelectionContext` / `DecorationContext` |
| `orchestration/*` | everything above + top-level split_rendering modules + `contexts::*` | — |
| `mod.rs` | only `pub use orchestration::{render_content, compute_content_layout};` | anything else |

The invariant is cheap to lock in: a single grep
(`grep -n 'SelectionContext\|DecorationContext' <top-level files>`) should
return **zero** hits outside `orchestration/`.

## File-by-file content & size estimates

| # | File | Approx size | Contents |
|---|---|---|---|
| 1 | `mod.rs` | ~60 | `pub use` re-exports; optional compat `SplitRenderer` shim |
| 2 | `spans.rs` | ~300 | `push_span_with_map`, `SpanAccumulator`, `span_color_at`, `span_info_at`, `compress_chars`, `compute_inline_diff`, `debug_tag_style`, `push_debug_tag`, `DebugSpanTracker` |
| 3 | `style.rs` | ~150 | `dim_color_for_tilde`, `inline_diagnostic_style`, `fold_placeholder_style`, `append_fold_placeholder`, `create_virtual_line` |
| 4 | `char_style.rs` | ~250 | `compute_char_style` + private `CharStyleContext` / `CharStyleOutput` |
| 5 | `base_tokens.rs` | ~320 | `build_base_tokens`, `build_base_tokens_binary`, `build_base_tokens_for_hook`, `is_binary_unprintable`, `is_control_char` |
| 6 | `transforms.rs` | ~640 | `apply_wrapping_transform`, `apply_soft_breaks`, `apply_conceal_ranges`, `inject_virtual_lines` |
| 7 | `view_data.rs` | ~280 | `build_view_data`, `view_line_source_byte`, `is_hidden_byte` |
| 8 | `folding.rs` | ~260 | `apply_folding`, `fold_adjusted_visible_count`, `fold_indicators_for_viewport`, `diff_indicators_for_viewport`, `FoldIndicator` |
| 9 | `scrollbar.rs` | ~500 | `render_scrollbar`, `render_horizontal_scrollbar`, `render_composite_scrollbar`, `scrollbar_line_counts`, `scrollbar_visual_row_counts`, `compute_max_line_length` |
| 10 | `layout.rs` | ~290 | `split_layout`, `split_buffers_for_tabs`, `sync_viewport_to_content`, `resolve_view_preferences`, `calculate_view_anchor`, `calculate_compose_layout`, `calculate_viewport_end`, `resolve_cursor_fallback`, `render_separator`. Local types: `SplitLayout`, `ViewPreferences`, `ViewAnchor`, `ComposeLayout` |
| 11 | `gutter.rs` | ~230 | `render_left_margin` + private `LeftMarginContext`, `render_compose_margins` |
| 12 | `post_pass.rs` | ~240 | `render_column_guides`, `render_ruler_bg`, `apply_hyperlink_overlays`, `apply_osc8_to_cells`, `apply_background_to_lines` |
| 13 | `orchestration/mod.rs` | ~800 | `pub fn render_content`, `pub fn compute_content_layout` |
| 14 | `orchestration/contexts.rs` | ~30 | `SelectionContext`, `DecorationContext` — data only |
| 15 | `orchestration/overlays.rs` | ~260 | `selection_context`, `decoration_context` |
| 16 | `orchestration/render_line.rs` | ~1,300 | `render_view_lines` + private concern-scoped sub-structs replacing `LineRenderInput`; `LineRenderOutput` / `LastLineEnd` |
| 17 | `orchestration/render_buffer.rs` | ~550 | `compute_buffer_layout`, `draw_buffer_in_split`, `render_buffer_in_split`, `render_view_line_content`; `BufferLayoutOutput` |
| 18 | `orchestration/render_composite.rs` | ~440 | `render_composite_buffer` |
| 19 | `tests/` | ~1,740 | `cursor.rs`, `tokens_and_wrap.rs`, `post_pass.rs`, `folding_and_highlight.rs`, shared helpers in `tests/mod.rs` |

## Mega-struct locality recap

| Struct | New location | Used by |
|---|---|---|
| `SelectionContext` | `orchestration/contexts.rs` | `orchestration/overlays.rs` (producer), `render_line.rs`, `render_buffer.rs`, `render_composite.rs` |
| `DecorationContext` | `orchestration/contexts.rs` | same |
| `LineRenderInput` (split into `MarginArgs`, `CursorArgs`, `DecorArgs`, `CellMapArgs`) | **private** inside `orchestration/render_line.rs` | nowhere else |
| `BufferLayoutOutput`, `LineRenderOutput`, `LastLineEnd` | `orchestration/render_buffer.rs` / `render_line.rs` as private types | internal only |
| `CharStyleContext` / `CharStyleOutput` | **private** inside `char_style.rs` | nowhere else |
| `LeftMarginContext` | **private** inside `gutter.rs` | nowhere else |
| `FoldIndicator` | `folding.rs` | referenced as a field type by `DecorationContext` |
| `SplitRenderer` | deleted (or 4-line compat shim in `mod.rs`) | entry point only |

## Internal decomposition of `render_view_lines`

This function (~1,246 lines) cannot be moved cleanly without shrinking
`LineRenderInput`. Inside `orchestration/render_line.rs`:

```rust
// private to the file
struct MarginArgs<'a>  { /* state, theme, gutter_width, estimated_lines,
                           indicators, show_*, cursor_line_* */ }
struct CursorArgs<'a>  { /* session_mode, software_cursor_only, primary
                           cursor pos, is_active */ }
struct DecorArgs<'a>   { /* decorations, view_lines, selection */ }
struct CellMapArgs<'a> { /* cell_theme_map, screen_width */ }
```

The outer `render_view_lines` builds these from its single parameter list, then
delegates to:

- `render_line_chars(...)` — per-char inner loop (currently inlined).
- `append_inline_diagnostic(...)` — trailing diagnostic text.
- `CellThemeRecorder` — small helper mirroring `DebugSpanTracker` for
  cell-theme-map writes.
- ANSI parser threading — its own small function.

Nothing escapes the file.

## `mod.rs` final shape

```rust
//! Split pane layout and buffer rendering.
mod spans;
mod style;
mod char_style;
mod base_tokens;
mod transforms;
mod view_data;
mod folding;
mod scrollbar;
mod layout;
mod gutter;
mod post_pass;
mod orchestration;

pub use orchestration::{compute_content_layout, render_content};

// Optional API-compat shim — only if external callers currently use
// `SplitRenderer::…`. Can be removed once call sites are updated.
pub struct SplitRenderer;
impl SplitRenderer {
    pub fn render_content(/* … */) -> /* … */ { orchestration::render_content(/* … */) }
    pub fn compute_content_layout(/* … */) -> /* … */ { orchestration::compute_content_layout(/* … */) }
}
```

## Phased execution

Each phase is a standalone PR. Every phase except Phase 4 is a pure code move
(no signature changes visible outside the module). Existing inline tests cover
every phase up to Phase 5.

### Phase 1 — Leaves

Move to top-level files, converting `impl SplitRenderer` methods to free
`pub(super) fn`:

- `spans.rs`
- `style.rs`
- `char_style.rs` (fold its private context struct in)
- `post_pass.rs`

### Phase 2 — View pipeline & subsystems

- `base_tokens.rs`
- `transforms.rs`
- `view_data.rs`
- `folding.rs`
- `scrollbar.rs`
- `layout.rs` (includes `render_separator`)
- `gutter.rs` (fold in `LeftMarginContext`)

### Phase 3 — Quarantine orchestration

- Create `orchestration/`.
- Move `SelectionContext`, `DecorationContext` to `orchestration/contexts.rs`.
- Move their producers to `orchestration/overlays.rs`.
- Move the three big render functions into `orchestration/render_line.rs`,
  `render_buffer.rs`, `render_composite.rs`. No structural changes yet.

### Phase 4 — Decompose `render_view_lines`

The only phase that changes logic shape. Inside
`orchestration/render_line.rs`:

- Split `LineRenderInput` into the four concern-scoped private sub-structs.
- Extract the per-char inner loop, inline-diagnostic trailing text, ANSI
  threading, and cell-theme recording into named private helpers.
- Add targeted unit tests for cursor placement and cell-theme-map writes
  before refactoring.

### Phase 5 — `mod.rs` and the compat shim

- Shrink `mod.rs` to re-exports.
- Decide whether to keep the `SplitRenderer` shim or delete it along with
  updates to external call sites (all inside `fresh-editor`).

### Phase 6 — Tests

- Split `mod tests` across `tests/` submodules.
- Move shared helpers (`render_output_for`, `render_output_for_with_gutters`,
  `dump_render_output`, `count_all_cursors`, `check_typing_at_cursor`,
  `extract_token_offsets`, `strip_osc8`, `read_row`) to `tests/mod.rs`.
- Group tests by target module: `cursor.rs`, `tokens_and_wrap.rs`,
  `post_pass.rs`, `folding_and_highlight.rs`.

## Risks & mitigations

- **`render_view_lines` shares local bookkeeping** (ANSI parser state, cell
  theme cursor, secondary-cursor collector). Premature extraction can silently
  break cursor placement. Mitigation: cover with added unit tests before Phase
  4.
- **`compute_buffer_layout` and `render_composite_buffer`** share a lot of
  structure with slightly different fold/wrap assumptions. Merging them is
  tempting but should be deferred to a post-refactor follow-up.
- **`LineRenderInput` holds `&mut cell_theme_map`.** Keep it `pub(super)` at
  the module boundary; never re-export from `orchestration::mod`.

## Success criteria

- `crates/fresh-editor/src/view/ui/split_rendering.rs` no longer exists; it is
  replaced by the directory above.
- `grep -rn 'SelectionContext\|DecorationContext'
  crates/fresh-editor/src/view/ui/split_rendering/` matches only files under
  `orchestration/`.
- No file in the module is >1,400 lines (with the singular exception of
  `orchestration/render_line.rs` until Phase 4 completes).
- All existing tests pass at each phase boundary.
- Public API from the module is unchanged (`SplitRenderer::render_content`,
  `SplitRenderer::compute_content_layout`).
