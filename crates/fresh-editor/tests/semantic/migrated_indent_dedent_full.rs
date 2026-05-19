//! Partial migration of `tests/e2e/indent_dedent.rs` — covers the
//! basic single-line / multi-line Tab + Shift-Tab cases in
//! spaces-mode. The e2e file has 16 tests; this migration covers
//! the 5 most-fundamental cases. The remaining 11 cases (partial
//! selection, tab-character files, mixed indentation, multi-cursor
//! indent, smart backspace, issue #1304 boundary tests) are
//! tracked in #2058 and remain guarded by the still-extant e2e
//! file.
//!
//! Originals open a real file, send `Tab` / `Shift+Tab`, assert on
//! buffer content. Scenario equivalent: load_buffer_from_text +
//! `Action::InsertTab` (Tab key) / `Action::DedentSelection`
//! (Shift+Tab key).
//!
//! All tests use the spaces-with-tab_size=4 default that the
//! original `harness_with_spaces` helper sets — that's also the
//! default of the scenario harness.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_tab_indent_single_line_spaces() {
    // Original: `test_tab_indent_single_line_spaces`. Cursor at
    // line start, press Tab, expect 4-space indent prepended.
    assert_buffer_scenario(BufferScenario {
        description: "Tab at line start indents with 4 spaces".into(),
        initial_text: "Hello world".into(),
        actions: vec![Action::MoveLineStart, Action::InsertTab],
        expected_text: "    Hello world".into(),
        // Cursor lands after the inserted indent.
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    });
}

#[test]
fn migrated_tab_indent_multiple_lines_spaces() {
    // Original: `test_tab_indent_multiple_lines_spaces`.
    // Select all 3 lines, press Tab, expect each indented.
    assert_buffer_scenario(BufferScenario {
        description: "Tab on multi-line selection indents each line by 4 spaces".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        actions: vec![Action::SelectAll, Action::InsertTab],
        expected_text: "    Line 1\n    Line 2\n    Line 3".into(),
        // FINDING: anchor advances past the inserted indent (4),
        // so the selection covers the post-indent content range
        // rather than starting at byte 0.
        expected_primary: CursorExpect::range(4, 32),
        expected_selection_text: Some("Line 1\n    Line 2\n    Line 3".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_shift_tab_dedent_single_line_spaces() {
    // Original: `test_shift_tab_dedent_single_line_spaces`.
    // Buffer pre-indented; Shift+Tab removes 4 spaces.
    assert_buffer_scenario(BufferScenario {
        description: "DedentSelection on '    Hello world' removes 4 leading spaces".into(),
        initial_text: "    Hello world".into(),
        actions: vec![Action::MoveLineStart, Action::DedentSelection],
        expected_text: "Hello world".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_shift_tab_dedent_multiple_lines_spaces() {
    // Original: `test_shift_tab_dedent_multiple_lines_spaces`.
    assert_buffer_scenario(BufferScenario {
        description: "DedentSelection on multi-line indented selection removes 4 spaces from each"
            .into(),
        initial_text: "    Line 1\n    Line 2\n    Line 3".into(),
        actions: vec![Action::SelectAll, Action::DedentSelection],
        expected_text: "Line 1\nLine 2\nLine 3".into(),
        expected_primary: CursorExpect::range(0, 20),
        expected_selection_text: Some("Line 1\nLine 2\nLine 3".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_shift_tab_dedent_no_indentation_no_op() {
    // Original: `test_shift_tab_dedent_no_indentation`. Dedent on
    // already-flush text should be a no-op.
    assert_buffer_scenario(BufferScenario {
        description: "DedentSelection on already-flush text is a no-op".into(),
        initial_text: "Hello world".into(),
        actions: vec![Action::MoveLineStart, Action::DedentSelection],
        expected_text: "Hello world".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}
