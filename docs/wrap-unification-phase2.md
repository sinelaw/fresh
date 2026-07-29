# Wrap unification, part 2 — finishing the design

Stacked on `claude/single-line-file-perf-nzyqw6` (PR #2850). That PR unified the
**wrap rule** and the **coordinate service**. It did not unify **viewport
placement**, and it left three gates where the new machinery steps aside for the
old. This branch removes all of it.

The target is not a judgement call: `tests/wrap_model/` already describes it, has
since before the Rust work started, and is 993 tests green. Every phase below is
"make the Rust do what `wrap_model/viewport.py` does".

## What #2850 left standing, and why

Three coexistence gates, each an admission that the index does not yet model
what is drawn:

| gate | where | what it hides from |
|---|---|---|
| `folds.is_empty()` | `compute_buffer_layout` | the index has no fold model |
| `wrap_index_models_layout()` | same, and `wrap_scroll_geometry` | soft breaks, conceals, inline virtual text |
| `inside_wrapped_line` | same | the layout pass's accumulated placement behaviour |

And three passes that can move the viewport, which is two too many:

1. byte-oriented pre-render `ensure_visible` — moves `top_byte` across logical lines
2. `ensure_visible_in_layout` — fine-tunes `top_view_line_offset` within the top
   line, reveals virtual lines, horizontal scroll
3. `ensure_visible_in_rows` — the row-space pass

They disagree, and they disagree *structurally*: (2) works in viewport-relative
coordinates, so its upward reach is bounded to the current top line and its
margin only applies inside a wrapped line. Those are not rules anyone chose;
they are artifacts of `top_view_line_offset` being an index into rows that start
at a logical line. Reproducing them in row space is the wrong move — #2850 tried
it twice and was wrong both times. They have to be *deleted*, and that means
deleting the coordinate that causes them.

## Phase A — `ViewAnchor` replaces the coordinate pair

`Viewport { top_byte, top_view_line_offset }` → `Viewport { anchor: ViewAnchor }`,
per `wrap_model/viewport.py`:

```rust
pub struct ViewAnchor {
    /// Byte of the first visible row, wherever it sits inside its logical line.
    pub byte: usize,
    /// Signed displacement from the row `byte` addresses. Zero for ordinary
    /// rows; negative when the viewport starts on an injected row (a virtual
    /// line drawn above its anchor), which owns no byte of its own.
    pub row_offset: isize,
}
```

Scale: 158 uses across 22 source files and 6 test files. Mechanical, but the
mechanical part is not the point — what it buys is that `top_byte` stops having
to be a logical line start, so there is no second coordinate to reconcile with
it, so the reconciliation bugs have nowhere to live.

Deletes outright: `snap_to_logical_line_start`, `scrolled_up_in_wrap`,
`calculate_view_anchor`'s forward re-skip, and the fresh#1574 patch pair
(`fine_tune_scroll_up` + the `top_byte`-unchanged special case) — which #2850
could only *bypass*.

Do not do this by find-and-replace. Convert one consumer at a time behind
`anchor.byte`/`anchor.row_offset` accessors, keeping the suite green at each
step; the semantic and e2e scroll tests are the contract.

## Phase B — one placement pass

With one coordinate, `ensure_visible` is `wrap_model/viewport.py::ensure_visible`
— eight lines. Retire, in order:

* `ensure_visible_in_layout`'s vertical phases: `fine_tune_scroll_up`,
  `scroll_cursor_within_margin`, `reveal_virtual_lines_above`
* the byte-oriented pre-render path: `ensure_visible`, `ensure_visible_with_virtual`,
  `check_wrapped_visibility`, `check_nowrap_visibility`, `scroll_to_cursor_wrapped`,
  `scroll_to_cursor_nowrap`
* the `rows_settled` / `inside_wrapped_line` coexistence rule, which has no
  meaning once there is one pass

Horizontal column scroll stays — it is a genuinely separate concern and is the
only part of `ensure_visible_in_layout` that survives.

The regression contract here is `anti_recenter_dropped_leaves_cursor_at_viewport_bottom`
and the fresh#1574 invariant tests. Expect the *characterization* tests to need
rewriting rather than satisfying: several pin behaviour that only exists because
of the coordinate pair (the recenter anti-test's band is chosen empirically
around it). Rewriting a characterization test is legitimate when the thing it
characterizes is deliberately removed — but say so in the commit, and check each
one against what the model says the answer should be.

## Phase C — the index models what is drawn

`WrapIndex::build_line` currently runs `build_line_tokens` → `WrapMachine`. It
must run the renderer's chain: soft breaks → conceals → inline virtual text →
wrap, which is what `wrap_model/row_layout.py` does and what `build_view_data`
already does per frame. Folds fold in the same way — as a row-level skip, per
`wrap_model/wrap_index.py`.

Removes: both decoration gates, the compose-mode carve-out, and with them the
last reason `VisualRowIndex`'s soft-break awareness was missed. Also unblocks
the exact scrollbar in compose mode (three tests currently `#[ignore]`d with
"needs `VisualRowIndex::position_at_row` virtual-row split").

Cost note: the per-line build stops being pure and needs the decoration
managers. That is what `ensure_built` already takes a `virtual_rows` closure
for — extend that pattern rather than handing the index a `&EditorState`.

## Phase D — delete the shadow pipelines

Only once B and C are in, because these all exist to answer questions the index
cannot yet answer:

* `LineWrapCache` and its writeback, replaced by `materialize_rows`
* `count_visual_rows_for_text{,_grid}`, `count_visual_rows_for_text_with_soft_breaks`,
  `count_segment_rows_with_indent`
* `Viewport::count_visual_rows_for_line`, `RowCountCache`, `wrap_row_cache`
* `layout_for_plain_text{,_grid}`, `wrap_segment_source_bytes`, `grid_segment_source_bytes`
* the scrollbar's approximate logical-line regime, with `MAX_WRAP_SCROLLBAR_LINES`
  and `MAX_WRAP_SCROLLBAR_BYTES` — `ScrollbarState` in the model is exact and
  O(log n), so the regime split has nothing left to trade off

## Order, and why

C first: independent of the others, removes two of the three gates, and is the
prerequisite for the scrollbar work in D. Then A, which is the keystone. Then B,
which A makes possible. Then D.

Keep green at every commit: `cargo test -p fresh-editor --lib`,
`--test semantic_tests`, `--test e2e_tests`, clippy at baseline, `cargo fmt`.
Model: `cd tests/wrap_model && .venv/bin/python -m pytest` (993).

When a Rust behaviour and the model disagree, the model is not automatically
right — but it is right about the *design*, so the disagreement is either a Rust
bug or a missing complication in the model. Fix whichever it is; do not paper
over it in the Rust.
