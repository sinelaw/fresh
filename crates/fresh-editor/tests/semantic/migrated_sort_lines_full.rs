//! Faithful migration of `tests/e2e/sort_lines.rs`.
//!
//! The keymap dependency (Ctrl+P → "sort lines" → Enter) is
//! lifted to `Action::SortLines`. Each scenario also pins the
//! cursor + selection state at t=∞ — the original e2e tests did
//! not assert on cursor, so these scenarios add coverage.
//!
//! Coverage: basic full-buffer sort, single-line no-op, numeric
//! lexicographic ordering, trailing-newline preservation, undo
//! round-trip, already-sorted idempotence, case-sensitive ASCII
//! ordering, empty-line handling, and **partial selection** (only
//! the selected lines are sorted; outside lines untouched).
//!
//! **Finding pinned here** (see
//! `docs/internal/scenario-migration-findings.md` §7):
//! `SelectAll + SortLines` preserves the selection anchor when
//! the buffer is unchanged (already-sorted / single-line cases)
//! but clears it when the buffer is mutated. That asymmetry was
//! invisible to the original e2e tests; pinning it here so a
//! future change is flagged.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use crate::common::scenario::trace_scenario::{assert_trace_scenario, TraceScenario};
use fresh::test_api::Action;

#[test]
fn migrated_sort_lines_basic() {
    // Original: `tests/e2e/sort_lines.rs::test_sort_lines_basic`.
    // FINDING: anchor cleared on mutation (cursor at end-of-buffer,
    // anchor None). E2e didn't assert on this.
    assert_buffer_scenario(BufferScenario {
        description: "SortLines on 3 unsorted lines yields alphabetical order".into(),
        initial_text: "cherry\nbanana\napple".into(),
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "apple\nbanana\ncherry".into(),
        expected_primary: CursorExpect::at(19),
        ..Default::default()
    });
}

#[test]
fn migrated_sort_lines_single_line_no_change() {
    // Original: `test_sort_lines_single_line_no_change`.
    // FINDING: anchor preserved (Some(0)) — buffer didn't change.
    assert_buffer_scenario(BufferScenario {
        description: "SortLines on single-line buffer preserves SelectAll anchor".into(),
        initial_text: "only line".into(),
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "only line".into(),
        expected_primary: CursorExpect::range(0, 9),
        ..Default::default()
    });
}

#[test]
fn migrated_sort_lines_with_numbers_uses_lexicographic_order() {
    // Original: `test_sort_lines_with_numbers`.
    assert_buffer_scenario(BufferScenario {
        description: "SortLines uses lexicographic, not numeric, ordering".into(),
        initial_text: "10 items\n2 items\n1 item".into(),
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "1 item\n10 items\n2 items".into(),
        expected_primary: CursorExpect::at(23),
        ..Default::default()
    });
}

#[test]
fn migrated_sort_lines_preserves_trailing_newline() {
    // Original: `test_sort_lines_preserves_trailing_newline`.
    assert_buffer_scenario(BufferScenario {
        description: "SortLines preserves trailing newline".into(),
        initial_text: "zebra\napple\nmango\n".into(),
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "apple\nmango\nzebra\n".into(),
        expected_primary: CursorExpect::at(18),
        ..Default::default()
    });
}

#[test]
fn migrated_sort_lines_undo_restores_original_order() {
    // Original: `test_sort_lines_undo`.
    assert_trace_scenario(TraceScenario {
        description: "SortLines + Undo restores original ordering".into(),
        initial_text: "cherry\napple\nbanana".into(),
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "apple\nbanana\ncherry".into(),
        undo_count: 1,
    });
}

#[test]
fn migrated_sort_lines_already_sorted_is_noop() {
    // Original: `test_sort_lines_already_sorted`.
    // FINDING: anchor preserved (Some(0)) — buffer didn't change.
    assert_buffer_scenario(BufferScenario {
        description: "SortLines is idempotent on already-sorted input; anchor preserved".into(),
        initial_text: "apple\nbanana\ncherry".into(),
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "apple\nbanana\ncherry".into(),
        expected_primary: CursorExpect::range(0, 19),
        ..Default::default()
    });
}

#[test]
fn migrated_sort_lines_case_sensitive_ascii_ordering() {
    // Original: `test_sort_lines_case_sensitive`.
    // ASCII case ordering: uppercase comes first ('A'=0x41, 'a'=0x61).
    assert_buffer_scenario(BufferScenario {
        description: "SortLines uses case-sensitive ASCII order: uppercase first".into(),
        initial_text: "banana\nApple\ncherry\nBerry".into(),
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "Apple\nBerry\nbanana\ncherry".into(),
        expected_primary: CursorExpect::at(25),
        ..Default::default()
    });
}

#[test]
fn migrated_sort_lines_with_empty_lines() {
    // Original: `test_sort_lines_with_empty_lines`. Empty lines
    // sort to the top (empty string < anything else).
    assert_buffer_scenario(BufferScenario {
        description: "SortLines puts empty lines first".into(),
        initial_text: "cherry\n\napple\n\nbanana".into(),
        actions: vec![Action::SelectAll, Action::SortLines],
        expected_text: "\n\napple\nbanana\ncherry".into(),
        expected_primary: CursorExpect::at(21),
        ..Default::default()
    });
}

#[test]
fn migrated_sort_lines_partial_selection() {
    // Original: `test_sort_lines_partial_selection`. Cursor starts
    // at byte 0; MoveDown lands on line 2 ("zebra"), SelectLine
    // selects "zebra\n", two SelectDown extends the selection over
    // "apple\n" and "mango\n". SortLines then reorders only those
    // three selected lines; "first" (line 1) and "last" (line 5)
    // are outside the selection and stay put.
    //
    // Buffer layout (bytes):
    //   "first\n"  0..6
    //   "zebra\n"  6..12
    //   "apple\n"  12..18
    //   "mango\n"  18..24
    //   "last"     24..28
    assert_buffer_scenario(BufferScenario {
        description:
            "SortLines on partial selection sorts only selected lines; outside lines untouched"
                .into(),
        initial_text: "first\nzebra\napple\nmango\nlast".into(),
        actions: vec![
            Action::MoveDown,
            Action::SelectLine,
            Action::SelectDown,
            Action::SelectDown,
        ]
        .into_iter()
        .chain(std::iter::once(Action::SortLines))
        .collect(),
        expected_text: "first\napple\nmango\nzebra\nlast".into(),
        // After sort, the selection collapses (anchor cleared, matching
        // the "buffer changed ⇒ anchor None" leg of finding §7), and
        // the cursor parks at the end of the sorted range (byte 24,
        // start of "last").
        expected_primary: CursorExpect::at(24),
        ..Default::default()
    });
}

/// Anti-test: drops `Action::SortLines` from the partial-selection
/// sequence. Without it, the buffer keeps the original line order,
/// so the post-sort expectation must NOT match.
#[test]
fn anti_sort_lines_partial_selection_dropping_action_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: SortLines dropped — partial selection cannot reorder lines".into(),
        initial_text: "first\nzebra\napple\nmango\nlast".into(),
        actions: vec![
            Action::MoveDown,
            Action::SelectLine,
            Action::SelectDown,
            Action::SelectDown,
        ],
        expected_text: "first\napple\nmango\nzebra\nlast".into(),
        expected_primary: CursorExpect::at(24),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: selection setup alone cannot sort lines; \
         the reordered expectation must NOT match"
    );
}
