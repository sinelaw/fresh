# Wrap architecture migration — working notes

Living handoff for the work on PR #2850, branch
`claude/single-line-file-perf-nzyqw6`. Read this first when resuming.

## The problem

Editing a 500 KB file with no newlines pins a CPU core. Of a 23.4 s capture,
90% is the main thread's event loop and 84% of that is `Editor::render`. Four
independent O(line-length) paths run per frame:

| share | path |
|---|---|
| 59% | `build_view_data` → `ViewLineIterator`: the renderer builds every row from byte 0 of the logical line and discards the ones above the viewport |
| 16.9% | `visual_row_index::ensure_built` → `count_visual_rows_for_text`: the exact scrollbar re-wraps the whole line on every keystroke |
| 1.4% | `Viewport::scroll_down_visual` + `apply_visual_scroll_limit`: each reads and `to_string`s the line, twice per wheel event |
| 2.3% | `position_to_lsp_position`: `get_line(0)` copies the whole line, then UTF-16-counts the prefix, on every hover-timer tick |

**Root cause.** A byte offset *inside* a logical line is not a coordinate
anywhere. Scroll position, the row index, both caches, and the LSP converter are
keyed on whole logical lines, so every per-line operation is O(line) — invisible
at 80 characters, catastrophic at 500,000. Both caches that should absorb it
(`LineWrapCache`, `Viewport::wrap_row_cache`) fold `buffer.version()` into their
keys and therefore die on every keystroke.

The character budget merged in #2849 bounds the *first screen* of a long line
and nothing after it. It is a workaround this work replaces.

## The design

Four pieces. The Python model in `tests/wrap_model/` is the executable
specification for all of them — when in doubt about intended behaviour, read the
model, not this document.

1. **`WrapMachine`** (`view/wrap_machine.rs`) — the one place row boundaries are
   decided, for all three rules: `Word` (soft wrap), `Grid` (terminal
   scroll-back, fresh#2649), `Chop` (wrap-off `MAX_SAFE_LINE_WIDTH` safety
   break). Two drivers read a run: the renderer takes `WrapOutput::tokens`
   (with `Break`s spliced in), the index takes `WrapOutput::rows`.
2. **`RowCarry`** — the complete resume state at a row boundary
   (`line_indent`, `on_continuation`, `ansi_in_escape`, `chars_in_row`). Its
   completeness is what licenses mid-line rendering *and* incremental repair.
3. **`WrapIndex`** (`view/wrap_index.rs`) — row starts per logical line plus a
   Fenwick tree of per-line counts. Answers `row_of_byte`, `byte_of_row`,
   `total_rows`. Keyed on geometry; **repaired** on edit via `damage_bytes`,
   not invalidated.
4. **`ViewAnchor`** — the viewport as a single byte plus a signed row offset,
   replacing `(top_byte, top_view_line_offset)`.

### Invariants that must survive

- **`RowCarry` completeness** — resuming at a row reproduces the rest of the run.
  Pinned by `wrap_machine::tests::resume_at_any_boundary_reproduces_the_run` and
  `test_wrap_machine.py::test_resume_at_any_boundary_reproduces_the_run`.
  Resume is defined over the **source** stream, never over wrapped output
  (wrapping is not idempotent).
- **`repair ≡ rebuild`** — `wrap_index::tests::repair_equals_rebuild`.
- **Canonical index** — built with no cursors, so cursor movement never damages
  it. Cursor-aware layout exists only inside the renderer's window. This
  codifies the existing `cursor_sig: 0` convention.
- **fresh#1574** (top_view_line_offset erasure), **fresh#2649** (grid row
  identity), **fresh#2843** (decoration byte window under soft wrap),
  **fresh#2610** (scrollbar stall) — each has e2e coverage; see the design
  discussion in the PR body.

### Resumability — the subtle part

A row that opens with *injected* content the carry cannot reconstruct — a soft
break's newline, an inline hint that wrapped onto it, a conceal's replacement —
**cannot be addressed by byte alone**, because whether that content belongs to
this row or the previous one is a decision the wrap made and a byte offset does
not record it. Hence `LineWrap::resumable`. A hanging indent *is* reconstructible
(it is in the carry), so indented continuations stay resumable.

Both the repair path and the renderer must walk back to the nearest resumable
row (`WrapIndex::resumable_row_at_or_before`) and skip forward. On a plain long
line — the case this exists for — every row is resumable and the walk-back is
zero.

Repair additionally requires:
- resync only over **unmodified text** (`old.row_starts[k] >= rel_end_old`);
  otherwise a coincidental alignment inside the edited region splices a bogus
  tail and rows are silently lost;
- resync only past **every decoration in the line**, since beyond such a point
  the overlay reattaches to different tokens and "same byte, same carry" no
  longer implies "same continuation";
- resume **one row before** the damaged row, because the break that ends a row is
  decided by the token that overflows it, which lives on the next row.

## Status

| phase | commit | state |
|---|---|---|
| Model + tests | `f8ca42e` | done — 993 tests, mypy strict, ruff clean |
| 1 — `WrapMachine` | `413eb19` | done — transforms are drivers |
| 2 — `WrapIndex` | `e015328` | done — not yet wired to consumers |
| 2b — scrollbar + scroll math onto the index | — | **next** |
| 3 — `ViewAnchor`, O(viewport) renderer | — | pending |
| 4 — delete `line_wrap_cache` + writeback | — | pending |

Baseline to keep green: `cargo test -p fresh-editor --lib` (3169 passing),
`cargo clippy -p fresh-editor --all-targets` clean, `cargo fmt --all`.
Model: `cd tests/wrap_model && .venv/bin/python -m pytest` (993).

## Phase 2b — plan

Goal: kill the 16.9% scrollbar path and the 1.4% scroll path.

1. `EditorState` gains `wrap_index: WrapIndex` alongside (initially) the existing
   `visual_row_index`.
2. The edit-application path calls `WrapIndex::damage_bytes` with an
   `EditDamage` built from **pre-edit** line coordinates (`line_before`,
   `line_start_before`, `line_end_before`) — the caller has the pre-edit buffer,
   so it supplies them for free.
3. Plugin manager version bumps (soft breaks, conceals, inline virtual text,
   virtual lines) call `damage_all`.
4. `scrollbar_line_counts` (`view/ui/split_rendering/scrollbar.rs`) reads
   `total_rows()` / `row_of_byte()` instead of calling
   `visual_row_index::ensure_built`. The `MAX_WRAP_SCROLLBAR_LINES` /
   `MAX_WRAP_SCROLLBAR_BYTES` guards become a build-cost gate rather than an
   accuracy downgrade.
5. `Viewport::scroll_{up,down}_visual`, `apply_visual_scroll_limit`,
   `find_max_visual_scroll_position`, `clamp_top_byte_wrapped` reduce to row
   arithmetic.

Deletes on completion: `view/visual_row_index.rs`, `count_visual_rows_for_text`,
`count_visual_rows_for_text_with_soft_breaks`, `count_segment_rows_with_indent`,
`count_visual_rows_for_text_grid`, `for_each_grid_row_start`,
`grid_segment_source_bytes`, `compute_wrap_row_count_for_text`,
`wrap_segment_source_bytes`, `Viewport::count_visual_rows_for_line`,
`Viewport::wrap_row_cache`, `count_visual_rows_via_pipeline`,
`placeholder_layout_for_row_count`.

## Phase 3 — plan

Replace `(top_byte, top_view_line_offset)` with `ViewAnchor { byte, row_offset }`
(`row_offset` signed: negative when the viewport starts on an injected row, which
owns no byte). See `tests/wrap_model/wrap_model/viewport.py` and `editor.py` for
the exact target shape.

- `LineIterator::from_mid_line` — no backward scan; the caller certifies the byte
  came from `byte_of_row`.
- `build_base_tokens` starts mid-line and stops on a **row** budget; the
  `char_budget` parameter and `base_char_budget` (with its cursor clause) are
  deleted — nothing is built to be discarded.
- `WrapMachine::resume(carry)` at the anchor; first row marked `AfterBreak` so
  the gutter prints no line number against a continuation.
- `ensure_visible` runs in row space *before* anything is built, collapsing
  `compute_buffer_layout`'s up-to-three `build_view_data` calls to one.
- Deletes: `snap_to_logical_line_start`, `scrolled_up_in_wrap`,
  `fine_tune_scroll_up`, `check_wrapped_visibility`, `scroll_to_cursor_wrapped`,
  the drawn-window slice in `render_buffer.rs`, `calculate_view_anchor`.

Watch: `_stable_anchor` in the model handles a cursor scope starting *above* the
anchor within the same logical line — bounded walk-back, capped at 2 viewports.

## Phase 4 — plan

`materialize_rows(state, row_range, cursors) -> Vec<ViewLine>` replaces
`LineWrapCache` entirely — including `cursor_sig`, `LineWrapKey`, and the ~130
line renderer writeback in `view_data.rs`. Movement and mouse call sites switch
to it. `pipeline_inputs_version` survives only as the `damage_all` trigger.

## Defects found while building the model

Four are fixed in Phase 1:

1. **Double-width overflow** — the char-split accepted a chunk's first cluster
   regardless of remaining width (`chunk_grapheme_count > 0` guard alone), so a
   CJK glyph with one column left overflowed the row and was clipped.
2. **Spurious empty row** — the word path's `BinaryByte` branch lacked the "row
   has content" guard the grid path has, so a `<XX>` escape on a pane narrower
   than 4 columns broke at column 0 and stranded an empty leading row.
3. **Grid tab measured before the break, applied after it** — wrap and render
   disagreed about that tab's width.
4. **Asymmetric full-row break** — a row filled exactly was followed by a break
   only on the split path. This also broke resume, which is how it surfaced.

Deliberately **not** fixed: **three tab stops** (`visual_layout::TAB_WIDTH` = 8
decides wrap positions, a hardcoded `4` measures hanging indent, `tab_size` = 4
draws). Unifying them changes visible rendering for every file and does not
belong in a refactor. The model has `TabPolicy` + `LEGACY_TAB_QUIRK` ready for
when it is done deliberately.

Latent, removed as a class by the index: the count-only mirrors wrapped a single
synthetic `Text` token, so the space-overflow back-up (#1363) — which only fires
on `Space` tokens — never ran in them, and their counts could already differ
from what the renderer drew.

## Gotchas

- `Buffer::get_line_number` returns `usize`, not `Option`. `delete` takes a
  `Range`, and neither `delete` nor `insert` returns a `Result`.
- `build_line_tokens` (in `base_tokens.rs`) reads a whole logical line through
  the real tokenizer; it asks for `len / MAX_LINE_BYTES + 1` pieces because a
  long line is yielded in `MAX_LINE_BYTES` chunks and each counts against the
  read budget.
- The model's Hypothesis database replays previously-failing examples; after
  fixing something, `rm -rf tests/wrap_model/.hypothesis` before concluding it
  is flaky.
- Model edits are generated on character boundaries deliberately — real edits
  are anchored to cursors, which sit on character boundaries.
