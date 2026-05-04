# Scenario migration — behavioral findings

Behavioral asymmetries and probable bugs surfaced while migrating
`tests/e2e/*` to scenario form. Each entry follows the pattern:

> **Source test** • **Action sequence** • **e2e claim** vs
> **scenario observation** • **assessment**

Findings here are evidence, not conclusions: the migration runs the
scenario through the same `Editor` instance the e2e drives, just
without the keymap/render/screen-scrape layer, so if observations
differ it's the editor's behavior that differs, not the framework's.

---

## 1. `MoveLineEnd` parks cursor at last text cell, not one past

**Source:** layout / cursor scenarios.
**Sequence:** `Action::MoveLineEnd` on `"hello"`.
**Expectation:** cursor at byte 5 (one past the last char) and at
text col 5 in screen coordinates.
**Observation:** `EditorTestApi::primary_caret()` reports byte 5
(matches), but `cursor_screen_position()` reports
`(col=4, row=0)` — *one column short*.
**Assessment:** Possibly a confusion between "buffer cursor at
EOL byte position" and "screen column of cursor". When we render
to the terminal, a cursor at byte 5 of "hello" should sit at
column 5 (immediately after 'o'), not column 4 (on top of 'o').
Pinned in
`tests/semantic/migrated_layout_cursor.rs::layout_cursor_after_move_line_end_lands_one_past_last_char`.

## 2. `ToUpperCase` with no selection uppercases the entire buffer

**Source:** `tests/e2e/case_conversion.rs`-adjacent migrations.
**Sequence:** `Action::ToUpperCase` on `"hello"` with no
selection.
**Expectation (one of two reasonable):** (a) no-op, since
there's nothing selected; or (b) uppercase word/line under cursor.
**Observation:** Uppercases the entire buffer (`"hello"` →
`"HELLO"`) and parks the cursor at the *end* of the upcased range.
**Assessment:** Behavior is consistent (the editor's "upcase"
command falls back to whole-buffer when no selection exists), but
worth recording so a future change to "no-op without selection"
is surfaced. Pinned in
`migrated_bulk::bulk_uppercase_with_no_selection_uppercases_full_buffer`.

## 3. `SelectLeft` at byte 0 sets an empty anchor

**Source:** Various selection-edge cases.
**Sequence:** `Action::SelectLeft` on a fresh buffer with cursor
at byte 0.
**Expectation:** No-op (can't select left of byte 0).
**Observation:** Cursor stays at byte 0 but `anchor` becomes
`Some(0)`. The selection is non-`None` but empty (range 0..0).
**Assessment:** Either a real bug (anchor should not flip from
`None` to `Some(0)` for a no-op selection), or a deliberate
convention (deselect-on-move depends on `anchor.is_some()` to
decide whether to collapse). Pinned in
`migrated_misc::migrated_select_left_at_byte_zero_creates_empty_selection`.

## 4. `DeleteWordForward` consumes following whitespace

**Source:** Migration of `KillWord`-like behavior.
**Sequence:** `Action::DeleteWordForward` at byte 0 of
`"foo bar"`.
**Expectation:** Delete `"foo"` ⇒ `" bar"`.
**Observation:** Deletes `"foo "` (word + following whitespace)
⇒ `"bar"`.
**Assessment:** Both behaviors exist in the wild (Emacs
`kill-word` ≈ word + ws, vim `dw` ≈ word only). Pinning the
editor's choice. Not a bug.

## 5. `OpenLine` cursor position (already fixed)

**Source:** `tests/e2e/emacs_actions.rs::test_open_line_basic`.
**Sequence:** `Action::OpenLine` advances the cursor; e2e expected
it to stay put (Emacs `C-o` semantics).
**Status:** Found during the original Track-B migration; fixed
in commit `90ef113e` ("fix: OpenLine cursor advance — restore
Emacs C-o semantics") before the scenario plumbing existed.

## 6. Save-after-external-edit refuses to overwrite

**Source:** `migrated_persistence::migrated_external_edit_after_save_persists_until_buffer_resaves`.
**Sequence:** Save buffer, externally clobber the file with
different content, save again.
**Expectation:** Last-write-wins — second editor save overwrites
the external edit.
**Observation:** External content remains on disk after the
second save attempt; the editor refuses to overwrite a
changed-on-disk file (probably auto-revert / conflict-detection
guard).
**Assessment:** Conservative and probably correct behavior —
overwriting an external edit silently would lose data — but the
migrated test pins the *external content wins* outcome so future
behavior changes are flagged.

## 7. `SelectAll + SortLines` preserves anchor only when buffer unchanged

**Source:** `tests/e2e/sort_lines.rs::test_sort_lines_*`.
**Sequence:** `[SelectAll, SortLines]`.
**Expectation:** Either (a) anchor always preserved (selection
survives the sort) or (b) anchor always cleared (selection
collapses).
**Observation:** Asymmetric:
- Buffer changed (`basic`, `case_sensitive`,
  `preserves_trailing_newline`, `with_empty_lines`,
  `with_numbers`): cursor at byte len, **anchor cleared** (`None`).
- Buffer unchanged (`already_sorted`, `single_line_no_change`):
  cursor at byte len, **anchor preserved** (`Some(0)`).

**Assessment:** Probably a real asymmetry. The natural expectation
is "the action either preserves or clears selection consistently."
Pinned in `migrated_sort_lines_full::*`.

## 10. `AddCursorAbove` sticky-column drifts on cascading calls

**Source:** `tests/e2e/multicursor.rs::test_add_cursor_above`.
**Sequence:** `[MoveDocumentEnd, AddCursorAbove, AddCursorAbove]`
on a buffer of 3 equal-length lines `"Line 1\nLine 2\nLine 3"`
(each 6 bytes + newline).
**Expectation:** column-matched cursors at the same column on
each line: `{0, 6, 13, 20}`-based positions where each line's
cursor lands at the same column as the original.
**Observation:** Cursors land at `{0, 6, 20}` — the secondary on
Line 2 sits at byte 6 (start of Line 2), not byte 13
(column-matched end of "Line 2"). The sticky-column drifts as
the primary moves up, so the second `AddCursorAbove` adds a
cursor at the *new* primary's column (which has already drifted).
**Assessment:** Probably a real asymmetry — multi-cursor "add
above/below" usually preserves the column from the *original*
cursor, not from each intermediate primary. Pinned in
`migrated_multicursor_full::migrated_add_cursor_above_twice_yields_three_cursors`.

## 9. `InsertTab` on a selection advances both anchor and cursor

**Source:** `tests/e2e/indent_dedent.rs::test_tab_indent_multiple_lines_spaces`.
**Sequence:** `[SelectAll, InsertTab]` on `"Line 1\nLine 2\nLine 3"`.
**Expectation (naive):** anchor stays at 0 (selection grows to
include the prepended indent on the first line).
**Observation:** anchor advances by indent_width (4), so the
selection covers `"Line 1\n    Line 2\n    Line 3"` —
*excluding* the indent prepended to the first selected line.
**Assessment:** Probably deliberate: the user "selected
this content" and InsertTab logically operates on it without
expanding the selection to cover the freshly-inserted indent.
But it means subsequent `SelectAll`-then-asserting-on-selection
tests need to know about the off-by-N. Pinned in
`migrated_indent_dedent_full::migrated_tab_indent_multiple_lines_spaces`.

## 8. `DuplicateLine` lands cursor at start of duplicate

**Source:** `tests/e2e/duplicate_line.rs::test_duplicate_line_*`.
**Sequence:** `[<navigate to line>, DuplicateLine]` then optional
`InsertChar`.
**Expectation:** Original e2e tests asserted only buffer text;
cursor was unspecified. The e2e
`test_duplicate_line_cursor_on_new_line` proves "typing after
duplicate inserts on the new line" — equivalent to "cursor on
duplicated line."
**Observation:** Cursor lands at the *start* of the duplicated
line (byte = end-of-original-line + 1 newline).
**Assessment:** Sensible default. Pinned across
`migrated_duplicate_line_full::*` so a behavior change surfaces.

## 11. `RemoveSecondaryCursors` also clears active selection

**Source:** `tests/e2e/block_selection.rs::test_block_select_then_escape`.
**Sequence:** `[BlockSelectDown, RemoveSecondaryCursors]` on
`"line1 text\nline2 text\nline3 text"` (cursor at byte 0).
**Expectation (naive):** `RemoveSecondaryCursors` removes
secondary cursors but leaves the primary's selection untouched
(so cursor at 0 with selection 0..11 should remain).
**Observation:** Two things happen in `BlockSelectDown`+
`RemoveSecondaryCursors`:
- `BlockSelectDown` is implemented as an *extension of the
  primary's selection* (anchor=0, position=11), not as adding a
  secondary cursor.
- `RemoveSecondaryCursors` clears the active selection in
  addition to dropping secondaries — the surviving cursor is at
  byte 11 with `anchor=None`.
**Assessment:** Likely intentional (Esc-as-clear-all in a
multi-cursor block), but worth pinning so a future split between
"remove secondaries" and "clear selection" doesn't silently
break this. Pinned in
`migrated_block_selection_extras::migrated_block_select_then_remove_secondary_collapses`.

## 12. `AddCursorNextMatch` is three behaviors in one action

**Source:** `tests/e2e/issue_1697_ctrl_d_after_search.rs` and
`tests/e2e/multicursor.rs::test_add_cursor_next_match`.
**Observation:** `Action::AddCursorNextMatch` (Ctrl+D) does
different things depending on cursor + search state:
1. **No selection, active search** (after `Search` +
   `PromptConfirm`): selects the search match at the cursor's
   current position. Does NOT add a new cursor.
2. **No selection, no search**: selects the word at the cursor.
3. **Has selection** (from any source): adds a new cursor at
   the next occurrence of the selected text; the new cursor
   becomes primary, the original becomes secondary.
**Assessment:** This three-state behavior is intentional but
non-obvious. Pinned in
`migrated_search_modal_flows::migrated_ctrl_d_after_substring_search_selects_match_not_word`
(case 1) and
`migrated_multicursor_extras::migrated_add_cursor_next_match_with_*_selection`
(case 3).

---

## How to add a finding

1. Reproduce the asymmetry in a scenario (BufferScenario,
   LayoutScenario, etc.) and pin it as a passing test.
2. Add an entry above naming the source e2e file (if any), the
   action sequence, what the e2e *claimed*, what the scenario
   *observes*, and a short assessment.
3. If you suspect a bug, file an issue and link the scenario.
