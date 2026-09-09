# `wrap_model` — executable model of the end-state wrap architecture

A Python model of the design that replaces fresh's wrap/scroll/row-index layer.
It exists to be *wrong loudly and cheaply*: every structural claim the design
makes is a property test here, and every performance claim is an operation-count
test, so both are settled before any Rust is written.

```
uv venv .venv && uv pip install --python .venv/bin/python pytest hypothesis mypy ruff
.venv/bin/python -m pytest      # 129 tests
.venv/bin/python -m mypy        # strict
ruff check . && ruff format --check .
```

## The problem being modelled

Editing a 500 KB file with no newlines pins a CPU core. Four independent
O(line-length) paths run per frame: the renderer builds every row from byte 0 and
discards the ones above the viewport (59% of the profile); the scrollbar
re-wraps the whole line on every keystroke (16.9%); wheel scrolling reads the
line twice per event; the hover timer walks it per tick.

The root cause is that **a byte offset inside a logical line is not a coordinate
anywhere**. Scroll position, the row index, both caches, and the LSP converter
are keyed on whole logical lines, so every per-line operation is O(line). Both
caches that should absorb the cost are keyed on `buffer.version()` and therefore
die on every keystroke.

## What the model implements

| module | mirrors | note |
|---|---|---|
| `tokens.py` | `fresh_core::api::ViewTokenWire` | |
| `width.py` | `primitives::{display_width, visual_layout, ansi}` | reduced grapheme segmenter; see below |
| `buffer.py` | `model::buffer`, `primitives::line_iterator` | keeps `MAX_LINE_BYTES`, adds `from_mid_line` |
| `decorations.py` | soft breaks, conceals, inline virtual text, virtual lines, folds | incl. cursor-dependent activation |
| `base_tokens.py` | `split_rendering::base_tokens` | mid-line start; **no character budget** |
| `wrap_machine.py` | **new** — `view::wrap_machine` | the single wrap rule |
| `transforms.py` | `split_rendering::transforms` | drivers over the machine |
| `row_layout.py` | `view_pipeline::{ViewLineIterator, ViewLine}` | replaces `LineWrapCache` |
| `wrap_index.py` | **new** — `view::wrap_index` | replaces `visual_row_index` + both row caches |
| `viewport.py` | `view::viewport` | single anchor coordinate |
| `editor.py` | `split_rendering::view_data` | one frame |
| `metrics.py` | **new** | operation counters for complexity tests |

Four pieces carry the design:

1. **`WrapMachine`** — the one place row boundaries are decided, for all three
   rules (`WORD` soft wrap, `GRID` terminal scroll-back, `CHOP` = wrap-off's
   `MAX_SAFE_LINE_WIDTH` safety break). Today that decision is re-implemented in
   at least seven places kept in sync by convention and two agreement tests.
2. **`RowCarry`** — the complete resume state at a row boundary. Its completeness
   is what licenses mid-line rendering *and* incremental repair, and it is pinned
   by `test_resume_at_any_boundary_reproduces_the_run`.
3. **`WrapIndex`** — row starts per line plus Fenwick totals, keyed on geometry
   and **repaired** on edit rather than invalidated. Answers row→byte, byte→row,
   and total rows.
4. **`ViewAnchor`** — the viewport as a single byte (plus a signed row offset for
   injected rows), replacing the `(top_byte, top_view_line_offset)` pair whose
   reconciliation step caused fresh#1574.

## Tests

- `test_wrap_machine.py` — resume completeness, width post-conditions, token
  preservation, and each rule's specific behaviour (space-overflow back-up
  #1363, unbreakable words, hanging indent and its clamp, grid ANSI, chop).
- `test_wrap_index.py` — `repair ≡ rebuild` under randomised edits, with and
  without decorations; structural edits; Fenwick; laziness.
- `test_viewport.py` — anchored render equals the full render's window; scroll
  reversibility and clamping; `ensure_visible`; exact scrollbar; plugin
  view-transform bypass.
- `test_matrix.py` — 5 rules × 16 decoration subsets × 2 view modes, plus
  cursor-activation and fold cells, each held to the same six invariants.
- `test_complexity.py` — the scaling laws as counter assertions (below).

## Complexity, as tests

`metrics.py` counts the operations that scale. The claims are inequalities:

```python
with measure() as m:
    model.insert(len(model.buffer), "x")
assert m.rows_emitted <= 3        # keystroke cost is O(1) in line length
```

Pinned: keystroke cost flat in line length (append *and* insert-at-start, via
resync); render cost flat in scroll depth and document size; bytes read per
frame bounded; scrolling and scrollbar reads do zero wrapping; cursor movement
costs nothing; and a whole-frame budget (edit → ensure-visible → render →
scrollbar) that does not grow with the document.

## Findings — bugs the model surfaced in the current Rust

Building the model against the real algorithm turned up five defects. The model
implements the corrected behaviour and flags each at the site:

1. **Double-width overflow.** In the char-split path the first cluster of a chunk
   is accepted regardless of the remaining width (`chunk_grapheme_count > 0`
   guard), so a CJK glyph with one column left overflows the row and is clipped.
2. **Spurious empty row.** The word path's `BinaryByte` branch has no "row has
   content" guard, so a `<XX>` escape on a pane narrower than 4 columns emits a
   break at column 0 and an empty leading row. The grid path guards correctly.
3. **Tab measured before the break, applied after it.** In the grid rule a tab at
   a row boundary is accounted at its pre-break column and drawn at its
   post-break column, so wrap and render disagree about its width.
4. **Asymmetric full-row break.** A row filled exactly is followed by a break
   only on the split path, not when a whole token fills it — so a trailing
   newline lands on a different row depending on how the row was filled. This
   also breaks resume, which is how the model found it.
5. **Three tab stops.** `visual_layout::TAB_WIDTH` (8) decides wrap positions, a
   hardcoded `4` measures hanging indent, and the configurable `tab_size` (4)
   draws — so a tab can be measured at one width and drawn at another.

A sixth is latent rather than active: the count-only mirrors wrap a single
synthetic `Text` token, so the `Space`-overflow back-up (#1363) never fires in
them and their row counts can already differ from what the renderer draws. The
index eliminates the class by consuming the real tokenizer.

## Deliberate simplifications

- **Grapheme segmentation** is a reduced UAX #29 (combining marks, ZWJ, regional
  indicators). The real code uses `unicode-segmentation`. The model's contract is
  structural — one rule decides boundaries; resume and repair agree with a
  rebuild — and holds for any segmenter.
- **Word boundaries** are alnum/non-alnum transitions rather than full UAX #29.
- **Edits are character-aligned.** Real edits are anchored to cursors, which sit
  on character boundaries; generating mid-codepoint edits would test a
  precondition the system does not have.
- **Folds are line-granular**, matching `FoldManager::resolved_ranges`.
- **`damage_all` for plugin churn.** A soft-break/conceal/virtual-text version
  bump rebuilds lazily. Ranged damage from those managers is a later refinement;
  the contract shape is already in place.
- **Resync requires a decoration-free tail.** Splicing the unchanged tail after an
  edit assumes the stream past the resync point is the old one shifted by
  `delta`, which a decoration beyond that point would violate. Undecorated lines
  — the case this design exists for — always resync.
