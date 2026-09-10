//! Faithful migration of `tests/e2e/duplicate_line.rs`.
//!
//! Each test mirrors the original action sequence, lifting
//! `Ctrl+P → "duplicate line" → Enter` to `Action::DuplicateLine`.
//!

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use crate::common::scenario::trace_scenario::{assert_trace_scenario, TraceScenario};
use fresh::test_api::Action;

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
