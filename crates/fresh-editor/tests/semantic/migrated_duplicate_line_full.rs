//! Faithful migration of `tests/e2e/duplicate_line.rs`.
//!
//! Each test mirrors the original action sequence, lifting
//! `Ctrl+P → "duplicate line" → Enter` to `Action::DuplicateLine`.
//!
//! **Finding pinned here** (see
//! `docs/internal/scenario-migration-findings.md` §8):
//! `DuplicateLine` lands the cursor at the *start* of the
//! duplicated line, not the end. The original e2e tests didn't
//! assert on cursor position; the
//! `test_duplicate_line_cursor_on_new_line` e2e proves only that
//! "typing after duplicate inserts on the new line" — consistent
//! with cursor at start.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use crate::common::scenario::trace_scenario::{assert_trace_scenario, TraceScenario};
use fresh::test_api::Action;

#[test]
fn migrated_duplicate_line_basic() {
    // Original: `test_duplicate_line_basic`. Type "hello world",
    // duplicate. Cursor at byte 12 (start of duplicated line).
    assert_buffer_scenario(BufferScenario {
        description: "DuplicateLine on 'hello world' produces 'hello world\\nhello world'".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('h'),
            Action::InsertChar('e'),
            Action::InsertChar('l'),
            Action::InsertChar('l'),
            Action::InsertChar('o'),
            Action::InsertChar(' '),
            Action::InsertChar('w'),
            Action::InsertChar('o'),
            Action::InsertChar('r'),
            Action::InsertChar('l'),
            Action::InsertChar('d'),
            Action::DuplicateLine,
        ],
        expected_text: "hello world\nhello world".into(),
        // FINDING: cursor lands at the start of the duplicated line
        // (byte 12), not the end.
        expected_primary: CursorExpect::at(12),
        ..Default::default()
    });
}

#[test]
fn migrated_duplicate_line_with_newline_first_line() {
    // Original: `test_duplicate_line_with_newline`.
    assert_buffer_scenario(BufferScenario {
        description: "DuplicateLine on first of three lines duplicates that line only".into(),
        initial_text: "first\nsecond\nthird".into(),
        actions: vec![Action::MoveDocumentStart, Action::DuplicateLine],
        expected_text: "first\nfirst\nsecond\nthird".into(),
        expected_primary: CursorExpect::at(6),
        ..Default::default()
    });
}

#[test]
fn migrated_duplicate_selected_lines() {
    // Original: `test_duplicate_selected_lines`.
    assert_buffer_scenario(BufferScenario {
        description: "DuplicateLine over selection of lines 2-3 duplicates both".into(),
        initial_text: "line one\nline two\nline three\nline four".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveDown,
            Action::SelectDown,
            Action::SelectDown,
            Action::DuplicateLine,
        ],
        expected_text: "line one\nline two\nline three\nline two\nline three\nline four".into(),
        // FINDING: multi-line duplicate collapses the selection;
        // cursor lands at byte 29 (the position the secondary
        // SelectDown operations had at t=before-DuplicateLine).
        expected_primary: CursorExpect::at(29),
        ..Default::default()
    });
}

#[test]
fn migrated_duplicate_line_cursor_lands_on_duplicate() {
    // Original: `test_duplicate_line_cursor_on_new_line`.
    // The e2e proves "typing after duplicate inserts on the
    // duplicated line" — equivalent to "cursor lands on the
    // duplicated line."
    assert_buffer_scenario(BufferScenario {
        description: "After DuplicateLine, typing inserts on the duplicated line".into(),
        initial_text: "first\nsecond\nthird".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::DuplicateLine,
            Action::InsertChar('X'),
        ],
        expected_text: "first\nXfirst\nsecond\nthird".into(),
        expected_primary: CursorExpect::at(7),
        ..Default::default()
    });
}

/// Anti-test: drops `DuplicateLine` from
/// `migrated_duplicate_line_with_newline_first_line`. Without
/// it, the buffer stays "first\nsecond\nthird" and the expected
/// duplicated "first\nfirst\nsecond\nthird" cannot match.
#[test]
fn anti_duplicate_line_dropping_action_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: DuplicateLine dropped — line 1 never duplicates".into(),
        initial_text: "first\nsecond\nthird".into(),
        actions: vec![Action::MoveDocumentStart],
        expected_text: "first\nfirst\nsecond\nthird".into(),
        expected_primary: CursorExpect::at(6),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without DuplicateLine the buffer stays at the initial 3 lines; \
         the duplicated 'first\\nfirst' prefix cannot appear"
    );
}

#[test]
fn migrated_duplicate_line_undo_restores_original() {
    // Original: `test_duplicate_line_undo`.
    assert_trace_scenario(TraceScenario {
        description: "DuplicateLine + Undo restores original buffer".into(),
        initial_text: "hello world".into(),
        actions: vec![Action::DuplicateLine],
        expected_text: "hello world\nhello world".into(),
        undo_count: 1,
    });
}
