//! Migrated from `tests/e2e/paste.rs`.
//!
//! Paste = clipboard-aware copy/cut/paste round-trip. The
//! semantic claim is "Copy then Paste leaves the buffer +
//! cursor in a known shape." Action::Copy /
//! Cut / Paste exist as semantic verbs
//! independent of any keymap.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_copy_then_paste_at_eof_duplicates_selection() {
    // Select first 5 chars, copy, move to end, paste.
    assert_buffer_scenario(BufferScenario {
        description: "Copy 5-byte selection then Paste at EOF appends 'hello'".into(),
        initial_text: "hello world".into(),
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::Copy,
            Action::MoveDocumentEnd,
            Action::Paste,
        ],
        expected_text: "hello worldhello".into(),
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    });
}

#[test]
fn migrated_cut_removes_selection_and_replaces_on_paste() {
    assert_buffer_scenario(BufferScenario {
        description: "Cut 5-byte selection then Paste at end re-attaches the cut text".into(),
        initial_text: "hello world".into(),
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::Cut,
            Action::MoveDocumentEnd,
            Action::Paste,
        ],
        // After cut: " world" (length 6). Cursor at 0. Move to end
        // = position 6. Paste "hello" → " worldhello" (len 11),
        // cursor at 11.
        expected_text: " worldhello".into(),
        expected_primary: CursorExpect::at(11),
        ..Default::default()
    });
}

/// Anti-test: drops the `Copy` action from
/// `migrated_copy_then_paste_at_eof_duplicates_selection`.
/// Without it, the clipboard is empty, so the final Paste at
/// EOF appends nothing — buffer stays "hello world" and the
/// expected "hello worldhello" cannot match.
#[test]
fn anti_paste_dropping_copy_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: Copy dropped — clipboard is empty, Paste is a no-op".into(),
        initial_text: "hello world".into(),
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::MoveDocumentEnd,
            Action::Paste,
        ],
        expected_text: "hello worldhello".into(),
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without Copy the clipboard is empty; \
         the trailing 'hello' from the Paste cannot appear"
    );
}

#[test]
fn migrated_paste_with_no_selection_inserts_at_cursor() {
    // Pre-load clipboard via copy first.
    assert_buffer_scenario(BufferScenario {
        description: "Paste at the cursor inserts without affecting other text".into(),
        initial_text: "abXYZcd".into(),
        actions: vec![
            // Select "XYZ" (positions 2..5), copy, then paste at position 7.
            Action::MoveRight, // 1
            Action::MoveRight, // 2
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::Copy,
            Action::MoveDocumentEnd,
            Action::Paste,
        ],
        expected_text: "abXYZcdXYZ".into(),
        expected_primary: CursorExpect::at(10),
        ..Default::default()
    });
}
