//! Regression test for <https://github.com/sinelaw/fresh/issues/1697>:
//!
//! When the user has performed a substring search (Whole Word OFF, no Regex)
//! and the cursor is at a search match, pressing Ctrl-D ("Add cursor at next
//! match") should select just the search match (the substring), not expand
//! to the surrounding word.  Otherwise, subsequent Ctrl-D presses would look
//! for the *whole word* and miss other substring occurrences.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Helper: open the search prompt, type the query, and commit with Enter so
/// `search_state` is populated and the cursor jumps to the first match.
fn run_search(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(query).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
}

#[test]
fn test_ctrl_d_after_search_uses_match_not_word() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    // The search query "foo" is a substring of "foobar" but is also its own
    // word later in the file.  After searching, the cursor lands on the first
    // match (inside "foobar" at position 0).  Pressing Ctrl-D should select
    // the 3-byte search match, NOT the whole word "foobar".
    std::fs::write(&file_path, "foobar foo foo").unwrap();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    run_search(&mut harness, "foo");

    // After search, cursor should be at the first match (position 0) with no
    // selection.
    {
        let primary = harness.editor().active_cursors().primary();
        assert_eq!(primary.position, 0, "cursor should be at first match");
        assert!(
            primary.anchor.is_none(),
            "search should not leave a selection on the cursor"
        );
    }

    // Press Ctrl-D — should select the search match "foo" (0..3), not the
    // whole word "foobar" (0..6).
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let primary = harness.editor().active_cursors().primary().clone();
    let selection = primary
        .selection_range()
        .expect("Ctrl-D should produce a selection");
    assert_eq!(
        selection,
        0..3,
        "Ctrl-D after a substring search should select just the search match, \
         not the surrounding word.  Got selection {:?} with cursor pos {}.",
        selection,
        primary.position
    );

    // Press Ctrl-D again — should add a cursor at the next "foo" substring
    // (positions 7..10), proving the pattern in use is the substring "foo".
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let count = harness.editor().active_cursors().iter().count();
    assert_eq!(count, 2, "expected a second cursor after second Ctrl-D");

    let positions: Vec<usize> = harness
        .editor()
        .active_cursors()
        .iter()
        .map(|(_, c)| c.position)
        .collect();
    assert!(
        positions.contains(&10),
        "expected new cursor at end of next 'foo' match (position 10); \
         got positions {:?}",
        positions
    );
}
