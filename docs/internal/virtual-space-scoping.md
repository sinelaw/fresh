# Virtual Space — Feature Scoping for Fresh

Scoping analysis for adding **virtual space** (cursor movement and placement beyond the
end of a line) to Fresh, prompted by VSCode's long-stalled implementation
([microsoft/vscode#228680](https://github.com/microsoft/vscode/pull/228680), for issue
[#13960](https://github.com/microsoft/vscode/issues/13960)) and the fact that today only
traditional editors/IDEs (Visual Studio, JetBrains, Vim's `virtualedit`, Emacs) offer it.

## 1. What the feature is

With virtual space enabled, the cursor may occupy any column, including columns past the
last character of a line:

- **Arrow keys / page movement** may move the cursor into the empty region past EOL, and
  vertical movement through short lines keeps the cursor at its column instead of
  snapping to line end.
- **Mouse clicks** past the end of a line place the cursor at the clicked column instead
  of snapping to line end.
- **Typing** while in virtual space first pads the gap with real spaces, then inserts the
  typed text. The buffer is never mutated by mere movement — padding materializes only
  on edit.
- **Selections** may start or end in virtual space; block/column selections become true
  rectangles even across short lines.

### How VSCode's PR did it (relevant precedent)

The VSCode PR deliberately did **not** change the text model: model positions remain
clipped to real text, and the cursor state carries a `leftoverVisualColumns` field — the
number of visual columns the cursor sits past the line end. Virtual space is a
cursor/view-layer concept; the buffer only changes when typing materializes spaces. The
PR stalled for ~2 years not because the approach was wrong but on polish issues: cursor
behavior inconsistencies, scroll-width not accounting for virtual positions, and
view/model layering concerns. Those are exactly the risk areas we should budget for.

## 2. Where Fresh stands today

Fresh's editing core (`crates/fresh-editor`) is built on **byte offsets**: a cursor
position is always a valid byte index into the buffer
(`model/cursor.rs`, `docs/internal/text-model.md`). There is currently no way to
represent "column 20 on a 5-character line."

What already exists in our favor:

- **Goal column is already implemented.** `Cursor.sticky_column: Option<usize>`
  (`model/cursor.rs:31-36`) pins the desired *visual* column for vertical navigation,
  and `handle_vertical_up/down` (`input/actions.rs:1497/1547`) already preserve the
  visual goal across short lines. This is the same "column hint" machinery VSCode's PR
  leans on — the *desire* to be past EOL already survives movement; only the byte
  position collapses to line end.
- **Movement is event-based and centralized.** Movement emits `Event::MoveCursor`;
  clamping happens in a small number of known sites (see §4), not scattered through the
  code.
- **A clean single clamp primitive.** `byte_offset_at_visual_column`
  (`primitives/display_width.rs:48-57`) is where an over-long goal column collapses onto
  the line end. It is `#[inline]`, wide-char aware, and trivially extendable to a
  variant that reports overflow (how many visual columns past EOL the request was).

What works against us:

- **Rendering never draws a cursor off-text.** The cursor's screen position is found by
  matching a rendered cell's byte to the cursor byte during the per-cell sweep
  (`view/ui/split_rendering/orchestration/render_line/cells.rs:631-664`). There is no
  code path that places a cursor at `line_end + N`.
- **Block selection has no virtual-space scaffolding.** Despite being 2D
  (`SelectionMode::Block` + `Position2D`), every column is clamped to the line's byte
  length (`primitives/buffer_position.rs:26-36`, `input/actions.rs:398`), rectangles are
  truncated on short lines (`selection_sweep.rs:89-105` only tests cells that exist),
  and block copy yields ragged lines with no padding (`clipboard.rs:221-300`). Block
  selection is a *beneficiary* of this work, not a foundation for it.
- **Mouse clicks clamp.** `screen_to_buffer_position`
  (`app/click_geometry.rs:54`, closure at 109-133) snaps clicks past EOL to
  `line_end_byte`.
- **Multi-cursor normalization keys on bytes.** `Cursors::normalize`
  (`model/cursor.rs:339-361`) sorts and dedups by `(position, anchor)`; two cursors at
  the same byte with different virtual columns would incorrectly merge.

## 3. Design decision: phantom column vs. materialized spaces

**Option A — carry a virtual column on the cursor (recommended).** Add
`virtual_column: Option<usize>` (visual columns past EOL) to `Cursor`, alongside
`sticky_column`. The buffer stays untouched by movement; spaces materialize only when an
edit happens at a virtual position. This matches VSCode's design, Vim's `virtualedit`,
and Visual Studio. The invariant "cursor byte is always valid" is preserved; the
virtual column is view-layer state that edits consume.

**Option B — eagerly insert real spaces as the cursor moves.** Simpler (movement +
buffer only, no rendering or mouse changes), but it mutates files on mere navigation,
creates trailing whitespace, dirties undo history, and fights line-trimming logic. This
is not what users mean by virtual space; rejected.

The rest of this document scopes Option A.

## 4. Component-by-component impact

### 4.1 Cursor model — `model/cursor.rs` (small, but subtle)

- Add `virtual_column: Option<usize>` to `Cursor` (visual columns past line end;
  `None` = not in virtual space).
- `Cursors::normalize` (339): include `virtual_column` in the sort/dedup key so
  same-byte cursors with different virtual columns don't collapse.
- `adjust_for_edit` (165-188): decide reset semantics — an edit on the cursor's line
  should generally clear other cursors' virtual columns on that line (their gap width
  changed); edits elsewhere leave them alone.
- Session persistence: `SerializedCursor` (`app/window/mod.rs:2510-2522`) already
  serializes `sticky_column`; extend it if virtual columns should survive
  session restore (they probably should, for consistency).

### 4.2 Movement — `input/actions.rs` (largest single chunk)

All the clamp sites need to become virtual-space aware, gated on the setting:

| Site | Today | With virtual space |
|---|---|---|
| `handle_vertical_up/down` (1497/1547) | goal column collapses to line end via `byte_offset_at_visual_column` | set `virtual_column = goal - line_width` when goal exceeds line width |
| `MoveRight` / `MoveRightInLine` (2378/2458) | clamps at line end / buffer end | increment `virtual_column` past EOL instead of wrapping/stopping |
| `MoveLeft` (2365) | moves to previous byte | decrement `virtual_column` first; only move bytes when it reaches 0 |
| `MoveLineEnd` (2410) | lands at content end | unchanged (End goes to real EOL; clears virtual column) |
| `handle_page_up/down` (1599/1650) | `goal_column.min(line_len)` | same treatment as vertical up/down |

Supporting primitive: add a `byte_offset_at_visual_column_with_overflow` variant in
`primitives/display_width.rs` returning `(byte_offset, leftover_visual_columns)`.

Word movement, `MoveLineStart`, buffer start/end, go-to-line etc. should *clear*
`virtual_column` — that's the cheap, safe default for every movement not explicitly
taught about virtual space.

### 4.3 Editing — `input/actions.rs` + `clipboard.rs` (localized, must be exhaustive)

Every edit entry point that inserts at the cursor must first materialize padding when
`virtual_column > 0`: emit `Event::Insert { position: line_end, text: " ".repeat(n) }`
before the real insert, then clear the virtual column.

- `insert_char_events` (917) / `collect_insert_cursor_data` — typing.
- `handle_insert_newline` (1098) — Enter in virtual space should just insert `\n` at
  real line end (no padding), matching VS/Vim behavior.
- Tab insertion.
- Paste (`clipboard.rs`) — both linear and block paste.
- Backspace/Delete in virtual space: Backspace decrements the virtual column (no buffer
  change) until reaching real text; Delete joins with the next line without padding.
  These need explicit handling or they'll operate at line end while the cursor is
  drawn elsewhere — the #1 source of "cursor behavior inconsistencies" that stalled
  the VSCode PR.

Because padding is emitted as ordinary `Event::Insert`s, **undo/redo and multi-cursor
offset adjustment come for free** — one undo entry removes both the typed char and its
padding if grouped, which we should verify in tests.

### 4.4 Rendering — `view/ui/split_rendering/orchestration/render_line/` (the risky part)

- `cells.rs` `place_cell_cursor` (631-664) / `cell_screen_x` (707): when the primary
  cursor has a virtual column, screen X must be computed as
  `line_content_width + virtual_column` rather than found by byte-matching a cell.
- `trailing.rs`: the current post-content handling (trailing-space indicator, implicit
  EOF line at 61/147-151) is the natural place to plug in "cursor is N columns past
  content."
- Horizontal scrolling must account for the virtual position (the cursor must stay
  visible when it's far past EOL) — this is one of the exact issues (scroll width) that
  was unresolved in the VSCode PR. Fresh being a terminal editor with per-cell
  rendering makes this *easier* than VSCode's pixel-based scroll width, but the
  scroll-into-view logic still needs the virtual column.
- Selection painting into virtual space (`overlays.rs` `selection_context`,
  `selection_sweep.rs`): linear selections ending in virtual space and block-selection
  rectangles over short lines need synthesized "phantom cells" (or a post-content
  highlight rect) since the sweep only visits cells that exist.

### 4.5 Mouse — `app/click_geometry.rs` (small)

`position_from_mapping` (109-133): instead of returning `line_end_byte` for clicks past
content, return `(line_end_byte, clicked_col - content_width)` as byte + virtual column.
Drag-selection inherits this. Double/triple-click should keep snapping to real text.

### 4.6 Block selection (natural phase-2 beneficiary)

With virtual columns in place, block selection can become true-rectangle:

- `block_select_action` (`input/actions.rs:356-454`) and `pos_2d_to_byte`
  (`primitives/buffer_position.rs:26-36`): stop clamping columns to line length.
- `copy_block_selection_text` (`clipboard.rs:221-300`): pad short lines with spaces so
  copied rectangles are rectangular.
- Block *insert* (typing with a block selection across short lines) pads each line to
  the block column — this is the marquee use case (aligning trailing comments,
  columnar edits) and arguably the strongest user-facing motivation for the feature.

### 4.7 Config & settings UI (small)

- `config.rs` (~1290, "Editing" section): `pub virtual_space: bool` with
  `#[serde(default)]` (default **off**, matching every editor that has this feature),
  plus `partial_config.rs` override plumbing.
- Thread the resolved flag through `BufferSettings` (`state.rs:92-130`) so
  movement/insert/mouse/render code can read it; expose in the settings UI
  (`view/settings/items.rs`).
- Possible enum instead of bool, mirroring Vim's `virtualedit`:
  `off | block (block selection only) | on (everywhere)`. `block` is a low-risk,
  high-value middle tier and a good candidate for the default-visible option.

## 5. Interactions & edge cases to budget for

- **Line wrap:** virtual space and soft-wrap are contradictory past the wrap column.
  Simplest rule: virtual column is capped at the wrap width when wrap is on (or the
  feature only applies to the last visual row of a wrapped line). Needs a decision.
- **Wide characters / tabs:** virtual columns are *visual* columns; padding
  materialization must convert visual → spaces correctly after CJK/emoji and tabs.
  `char_width`/`display_width.rs` already gives us the machinery.
- **Multi-cursor:** normalize/dedup semantics (§4.1); also "add cursor below" in block
  mode should place virtual cursors on short lines instead of skipping/clamping.
- **LSP and plugins:** positions sent to LSP (UTF-16 based) and the plugin API must
  always use the clipped byte position — virtual columns must never leak outside the
  cursor/view layer. Worth an explicit invariant + test.
- **Trailing-whitespace hygiene:** materialized padding that ends up trailing (e.g. user
  types then deletes) should be handled by whatever trim-on-save logic exists; no new
  mechanism needed, but tests should cover it.
- **Search, go-to-definition, undo cursor restore:** anything that sets the cursor from
  a byte offset implicitly clears virtual column — fine, but should be uniform.

## 6. Suggested phasing

1. **Phase 0 — model + setting (1–2 days):** `virtual_column` on `Cursor`, config flag,
   normalize/dedup/adjust_for_edit semantics, overflow-reporting variant of
   `byte_offset_at_visual_column`. Pure plumbing, fully unit-testable.
2. **Phase 1 — vertical movement + rendering + typing (core, ~1 week):** sticky column
   no longer collapses on short lines; cursor renders past EOL; typing materializes
   padding; Backspace/Delete/Enter semantics; scroll-into-view. This alone delivers the
   headline feature.
3. **Phase 2 — mouse + horizontal movement (2–3 days):** click/drag past EOL,
   MoveRight into virtual space.
4. **Phase 3 — block selection true-rectangles (3–5 days):** unclamped block columns,
   rectangular copy/paste, block insert with per-line padding. Highest user value,
   builds directly on phases 0–2.
5. **Phase 4 — selections into virtual space for linear selections (optional):** VSCode
   punted on parts of this too; can ship without it.

Rough total: **2–3 weeks** of focused work for phases 0–3, dominated by rendering
(§4.4) and by making the edit-path coverage exhaustive (§4.3). The failure mode to avoid
is the one that killed the VSCode PR: shipping movement without airtight
typing/deletion/scroll behavior, then bleeding credibility on inconsistency bugs.
Fresh's centralized event-based editing and existing `sticky_column` machinery make the
scope meaningfully smaller than VSCode's.

## 7. Open questions

1. Default off with a bool, or a Vim-style `off | block | on` enum? (Recommend the
   enum, default `off`; consider `block` as the recommended setting in docs.)
2. Should virtual column survive session save/restore? (Lean yes — `sticky_column`
   already does.)
3. Behavior under soft line wrap (cap at wrap column vs. disable)?
4. Does Phase 3 change block-copy output for users with the feature *off*? (It must
   not — padding only when enabled.)
