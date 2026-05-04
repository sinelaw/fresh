//! Faithful migration of `tests/e2e/tab_indent_selection.rs`.
//!
//! Issue #353: Tab/Shift-Tab on a selection should indent or
//! dedent every selected line. The existing
//! `migrated_indent_dedent_full.rs` covers the basic
//! single-/multi-line cases; this file picks up the remaining
//! claims that aren't already covered:
//!
//! - Go-language file uses tab character (not spaces) for indent.
//! - Tab without selection inserts indent at the cursor position.
//! - Multiple consecutive Tabs indent multiple levels.
//! - Tab with partial-line selection still indents the entire line.
//! - Indent on a multi-line selection preserves the selection.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_tab_indent_multiple_lines_with_tabs_in_go() {
    // Original: `test_tab_indent_selection_with_tabs`. Go file
    // configures `use_tabs=true` by default; Tab on a multi-line
    // selection should prepend a `\t` to each selected line.
    //
    // Original keys: Home, Down (cursor to start of line 2),
    // Shift+Down × 2 (selection covers line 2 + line 3 + first
    // byte of line 4), Tab.
    assert_buffer_scenario(BufferScenario {
        description: "Tab on multi-line selection in Go file uses tab character".into(),
        initial_text: "line 1\nline 2\nline 3\nline 4\n".into(),
        language: Some("test.go".into()),
        actions: vec![
            Action::MoveLineStart,
            Action::MoveDown,
            Action::SelectDown,
            Action::SelectDown,
            Action::InsertTab,
        ],
        // Lines 2 and 3 indented; line 4 unchanged because the
        // selection ends at its first byte.
        expected_text: "line 1\n\tline 2\n\tline 3\nline 4\n".into(),
        // Anchor advances past the inserted indent on line 2 (1 tab),
        // cursor is on line 4 col 0 (same line, but bytes shifted by
        // 2 inserted tabs).
        expected_primary: CursorExpect::range(8, 23),
        ..Default::default()
    });
}

#[test]
fn migrated_tab_without_selection_inserts_indent_at_eol() {
    // Original: `test_tab_without_selection_inserts_tab`.
    // Cursor at end-of-line-1 in a Rust file; Tab inserts 4
    // spaces at the cursor.
    assert_buffer_scenario(BufferScenario {
        description: "Tab with no selection inserts indent at cursor (Rust ⇒ 4 spaces)".into(),
        initial_text: "line 1\n".into(),
        language: Some("test.rs".into()),
        actions: vec![Action::MoveLineEnd, Action::InsertTab],
        expected_text: "line 1    \n".into(),
        expected_primary: CursorExpect::at(10),
        ..Default::default()
    });
}

#[test]
fn migrated_multiple_tabs_indent_multiple_levels() {
    // Original: `test_multiple_tabs_indent_multiple_levels`.
    // Select "line 2" via Home, Down, Shift+End; press Tab twice.
    // Expect 8 leading spaces on line 2.
    assert_buffer_scenario(BufferScenario {
        description: "Two consecutive Tabs on a selection indent two levels (8 spaces)".into(),
        initial_text: "line 1\nline 2\n".into(),
        language: Some("test.rs".into()),
        actions: vec![
            Action::MoveLineStart,
            Action::MoveDown,
            Action::SelectLineEnd,
            Action::InsertTab,
            Action::InsertTab,
        ],
        expected_text: "line 1\n        line 2\n".into(),
        // Anchor advances by total inserted indent (8); cursor stays
        // at end-of-line which has shifted by 8.
        expected_primary: CursorExpect::range(15, 21),
        ..Default::default()
    });
}

#[test]
fn migrated_tab_partial_line_selection_indents_full_line() {
    // Original: `test_tab_partial_line_selection_indents_full_lines`.
    // On line 2 select just "e" (one grapheme inside the line);
    // Tab still indents the entire line.
    assert_buffer_scenario(BufferScenario {
        description: "Tab on a partial-line selection indents the entire line".into(),
        initial_text: "line 1\nline 2\nline 3\n".into(),
        language: Some("test.rs".into()),
        actions: vec![
            Action::MoveLineStart,
            Action::MoveDown,
            Action::MoveRight,
            Action::MoveRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::InsertTab,
        ],
        expected_text: "line 1\n    line 2\nline 3\n".into(),
        // Selection follows the indent shift (+4): pre-indent
        // selection 9..11 ("ne") becomes 13..15 ("ne") after the
        // 4-space prefix is prepended to line 2.
        expected_primary: CursorExpect::range(13, 15),
        ..Default::default()
    });
}

#[test]
fn migrated_tab_indent_preserves_selection_across_lines() {
    // Original: `test_tab_indent_preserves_relative_cursor_position`.
    // Select from start of line 2 across to part of line 3
    // (Home, Down, Shift+End, Shift+Down). Tab indents line 2 + 3.
    // The selection must still exist after indent.
    assert_buffer_scenario(BufferScenario {
        description: "Tab indent preserves the cross-line selection".into(),
        initial_text: "line 1\nline 2\nline 3\nline 4\n".into(),
        language: Some("test.rs".into()),
        actions: vec![
            Action::MoveLineStart,
            Action::MoveDown,
            Action::SelectLineEnd,
            Action::SelectDown,
            Action::InsertTab,
        ],
        expected_text: "line 1\n    line 2\n    line 3\nline 4\n".into(),
        // Selection: anchor at line 2 start advances past inserted
        // indent (7→11, see finding #9). Cursor was at line 3 col 6
        // (byte 20) pre-indent; after line 3 gains a 4-space prefix,
        // the cursor at the same logical column lands at byte 28.
        expected_primary: CursorExpect::range(11, 28),
        ..Default::default()
    });
}

/// Anti-test: guards against the migration being structurally
/// inert. Reuses the expected output from
/// [`migrated_tab_indent_multiple_lines_with_tabs_in_go`] (which
/// asserts on tab characters) but switches `language` to Rust,
/// where the indent is 4 spaces. The expected `\t`-indented buffer
/// can no longer match, so `check_buffer_scenario` must return
/// `Err` — proving the test genuinely depends on Go's `use_tabs`
/// language profile rather than coincidentally matching whatever
/// indent the editor produces.
#[test]
fn anti_tab_indent_swapping_language_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: language swapped Go→Rust — indent must not be a tab".into(),
        initial_text: "line 1\nline 2\nline 3\nline 4\n".into(),
        // Rust uses 4 spaces, but the expectation below still
        // demands the Go-style \t indent — must mismatch.
        language: Some("test.rs".into()),
        actions: vec![
            Action::MoveLineStart,
            Action::MoveDown,
            Action::SelectDown,
            Action::SelectDown,
            Action::InsertTab,
        ],
        expected_text: "line 1\n\tline 2\n\tline 3\nline 4\n".into(),
        expected_primary: CursorExpect::range(8, 23),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: a Rust file should indent with spaces, so a Go-style \
         \\t-indented expectation must NOT match"
    );
}
