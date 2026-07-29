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

## Progress

| step | state |
|---|---|
| Model `ensure_visible` minimality + `recenter`, property + 160-cell matrix | done — `e9794ce` |
| C1 — `splice_inline_virtual_text` made pure (`resolve_inline_hints`) | done — `d5aa3f8` |
| C2 — index runs the decoration chain | done — `ee0a61f` |
| C3 — index models folds | done |
| P1 — repair adopts the post-edit version | done |
| P2 — `repair_line` reads from its resume row | done |
| P3 — ASCII fast path + pre-sized row buffers | done |
| A — `ViewAnchor` | next |
| B — one placement pass | |
| D — delete the shadow pipelines | |
| P4 — first build is still O(buffer) | not started |

All three gates from #2850 are gone except `inside_wrapped_line`, which needs A.

## What the profile of #2850 added (P1–P4)

Profiling a keystroke on the 500 KB single-line file, after #2850:

| | |
|---|---|
| 75.9% | render, of which 52.4% `build_view_data` and 15.6% a **full index rebuild** |
| 11.1% | `handle_key`, of which 10.5% `repair_wrap_indices` — **discarded** by that rebuild |

**P1 — the repair was pointless.** `pipeline_inputs_version` was written only
by `ensure_built`, and folds in `buffer.version()`, so every edit staled it.
`damage_bytes` updated `lines` and `rows` and left the version alone, so the
next `is_built_for` failed and rebuilt everything. ~26% of a keystroke doing the
same work twice. Fixed by adopting the post-edit version in `damage_bytes`;
sound because a repair only ever describes a text edit, and decoration versions
move through `damage_all` instead.

*Lesson worth keeping:* a cache whose validity check is separate from its
update path will silently stop being a cache. The repair tests all passed —
they compared repair against rebuild, which is exactly what still happened.
Nothing asserted that the repair was *used*. A metric-style test (`line_builds`
after a keystroke) would have caught it, and the Python model already counts
that; the Rust has no equivalent.

**P2 — `repair_line` tokenised the whole line to use its tail**, then cloned
the tail and dropped the prefix, per character typed, inside the routine whose
purpose is to be O(rows). Now reads from the resume byte.

**P3 — `InCB_Extend` at 7.81% self**, the hottest symbol in the profile: an
Indic-conjunct-break lookup per character on ASCII text. Plus five per-row
buffers growing from zero capacity once per rendered row. Both fixed.

**P4 — the unit of work is the logical line, everywhere.**
`build_line_tokens(buffer, line, ..)`, `build_line`, `repair_line`, and
`ensure_built`'s `for line in 0..line_count` all take a line index and process
it whole. With 10,000 short lines that is free; with one 500 KB line every
"per-line" operation is a full-buffer scan. P1 and P2 stop it happening *per
keystroke*, but the first build after opening the file is still O(buffer), and
so is any `damage_all` (any plugin decoration change).

Not yet planned, and it is the last structural O(buffer) left. The shape of a
fix: build lazily per line on first query rather than eagerly for the whole
buffer, with the Fenwick tree seeded from an estimate and corrected as lines are
built. That interacts with `total_rows()` being exact, which the scrollbar
depends on — so it needs its own design pass, and possibly a model change
first, rather than being improvised.

## Compose mode: what it actually is, and the design that covers it

Phase B's two remaining e2e failures (`issue_1574` up-arrow invariants,
compose table arrow-down reach) were first blamed on the plugin view-transform
API bypassing the index. **That diagnosis was wrong.** Reading
`plugins/markdown_compose.ts`: it migrated off `view_transform_request`
entirely (its own comment at :1755 — soft wrapping is now marker soft breaks).
Compose is built from exactly the decorations C2 taught the index:

| plugin call | namespace | what it makes |
|---|---|---|
| `addSoftBreak` | `md-wrap` | per-paragraph wrap at `composeWidth` |
| `addConceal` | `md-syntax` | syntax hiding, table chrome glyphs |
| `addVirtualLine` | table-border ns | table border rows |
| `setLayoutHints` | — | `composeWidth` |

All of it driven by `lines_changed` — an async, fire-and-forget hook that only
covers lines that have become visible. So the index *can* model compose; the
failures come from two narrower divergences, and the model already answers one
of them.

### Divergence A — cursor reveal (explains issue_1574)

Activation scopes live on the markers (`view/activation.rs`); the renderer
evaluates them with the real cursors, while the index and scroll math pass
`&[]` by documented design — the canonical, no-cursor-anywhere rendering
(`conceal.rs:273`). So the cursor's own line draws revealed — different row
count — while placement computed its target in canonical rows. Result: a
press scrolls by a canonically-correct amount that is wrong on screen, the
cursor lands rows below the margin, and minimality then correctly declines to
move — the observed "stall" at screen row 6.

The model solved this in `editor.py::_stable_anchor` and **the Rust never
ported it**: when an active scope starts above the render anchor inside the
same logical line, back the anchor up to the scope start and skip the delta
rows (capped at 2 × height). The canonical coordinate stays authoritative;
the renderer absorbs the reveal locally.

### Divergence B — decoration lag (explains compose reach)

`lines_changed` decorates lazily and only what has been visible. The
canonical index therefore undercounts rows for unvisited regions, placement
and the scroll clamp act on those stale counts — and since phase B deleted
the post-render pass, **nothing re-places when the decorations arrive**.
Placement now runs only on input events; the old layout pass incidentally
re-corrected every frame against rendered rows. The model does not model lag
at all: its decorations are synchronous.

### The design, model-first

* **M-A** — placement × activation cells in the matrix. Today's activation
  tests assert the index is undamaged by cursor movement and that revealing
  changes the drawn text; nothing asserts the *rendered* cursor is on screen.
  Add cells asserting the cursor byte appears in the rendered window
  (`char_source_bytes`) across the placement probes — this is the property
  that would have caught issue_1574 before any Rust was written.
* **M-B** — decoration-lag simulation: decorations arrive in deferred batches
  tied to render visits; property: once no batch is pending, ensure_visible +
  render shows the cursor (convergence), and the clamp admits the true end of
  the document.
* **E-A** — port `_stable_anchor` into the anchored build in `render_buffer`.
  **Done.**
* **E-B** — placement keyed to the index: build-if-stale in the render gate,
  then place, every frame. **Done** — and it is what fixed the compose reach
  test. Bounded by the scrollbar's size ceilings.
* ~~Kept as a hard boundary — the `view_transform` gate~~ **Dropped
  entirely.** Nothing used the API; markdown_compose migrated off it long
  ago. `ViewTransformPayload`, both plugin commands, the hook, the per-split
  storage and every render branch are gone, and the model's mirror went with
  them.

### What fresh#1574 actually was

Instrumenting the failing e2e disproved the reveal-divergence theory for
that test (`delta=0` throughout): the byte-oriented pre-render
`ensure_visible` — which phase B was supposed to retire and did not —
scrolled two rows for a one-row cursor move, and the row pass then found
the canonical cursor in-band and *correctly* declined to fix it. Two
authorities, again. The row pass now sets `row_pass_owns_placement` each
frame it places; the byte pass yields its vertical half while owned and
survives for exactly two jobs: horizontal column scroll with wrap off, and
files beyond the index's size ceilings, where no row space exists.

The reveal divergence is real all the same — the model's dense-reveal
fixture proves it, including the clamp case (a document that fits the
window canonically can exceed it revealed) — and the effective-row
placement (`CursorLineExpansion`, `cursor_visual_row`,
`ensure_cursor_visible`) handles it in both model and Rust.

### Remaining

* **P4** — first index build still O(buffer); `damage_all` (every compose
  `lines_changed` batch) now rebuilds with the decoration chain attached.
  The lag model (`test_arrival_is_a_rebuild_not_a_repair`) states the
  contract; the cost needs a design pass on lazy per-line builds with an
  exact `total_rows()`.
* Retire the byte pass's vertical half outright once the beyond-ceiling
  case has a row-space answer (a coarse index, or per-chunk builds).

Explicitly *not* the design: restoring the deleted layout-pass phases. The
canonical coordinate stays the single authority; A and B are a render-local
stitch and a recompute trigger, not a second opinion on placement.

Aside from the profile, not ours: the plugin thread shows `json_to_js_value` →
`JS_NewStringLen` → `utf8_scan` at 2.7% of the atom event. Off the critical
path, but if it ships the whole long line to plugins per keystroke it deserves
its own look.

### C2, concretely

`WrapIndex::build_line` currently does `build_line_tokens` → `WrapMachine::run`.
It must do what `wrap_model/wrap_index.py::_line_token_stream` does:

```
line_tokens → apply_soft_breaks → apply_conceal_ranges
            → splice_inline_virtual_text → WrapMachine::run
```

All three transforms are now pure, so the only question is how the decorations
reach a build that holds `&mut Buffer`. Snapshot them into plain data before the
borrow — extending the pattern `ensure_built`'s `virtual_rows` closure already
sets — rather than handing the index a `&EditorState`:

* soft breaks: `SoftBreakManager::query_viewport` over the whole buffer, with an
  empty cursor list. Cursor-blind is not an approximation here, it is the
  convention the index is already keyed on: scroll math must not change because
  a cursor moved, and `pipeline_inputs_version` deliberately does not include
  cursor position.
* conceals: `query_viewport_excluding`, same, with the `md-syntax` namespace
  excluded outside compose — the geometry already carries `view_mode`, so this
  is decidable at build time.
* inline hints: `resolve_inline_hints(state, None, ..)` — `None` theme, since
  the index measures and never draws.

Then delete `wrap_index_models_layout` and the compose carve-out in
`wrap_scroll_geometry`.

Watch: repair. `WrapIndex::repair` resyncs on unmodified text past every
decoration in the line, and the decoration list is currently consulted only for
that floor. Once decorations are *in* the built rows, an edit that moves a
decoration has to damage the line even when the text either side is unchanged —
`pipeline_inputs_version` already forces a full rebuild on any manager version
bump, so the repair path only needs to stay correct for pure-text edits, but
confirm that rather than assume it.

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
