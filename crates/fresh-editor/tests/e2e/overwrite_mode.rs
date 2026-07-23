//! E2E tests for overwrite (type-over) mode (issue #1300).
//!
//! The Insert key toggles overwrite mode: typed characters replace the
//! character under the cursor instead of being inserted before it. At the
//! end of a line typing appends (the line ending is never consumed), and
//! toggling again returns to normal insert behavior.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Typing in overwrite mode replaces characters instead of inserting.
/// Without the fix this test fails: the Insert key does nothing and typing
/// inserts, leaving "XYabcdef" on screen instead of "XYcdef".
#[test]
fn test_overwrite_mode_replaces_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("abcdef").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Insert, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("XY").unwrap();

    harness.render().unwrap();
    harness.assert_screen_contains("XYcdef");
    harness.assert_screen_not_contains("XYabcdef");
}

/// Pressing Insert surfaces a status message naming the new mode, so the
/// toggle is discoverable even if the cursor-shape change is subtle.
#[test]
fn test_overwrite_mode_toggle_shows_status_message() {
    // Wide enough that the status-bar message area isn't truncated.
    let mut harness = EditorTestHarness::new(120, 24).unwrap();

    harness
        .send_key(KeyCode::Insert, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Overwrite mode");

    harness
        .send_key(KeyCode::Insert, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Insert mode");
}

/// At the end of a line, overwrite mode appends instead of consuming the
/// line ending: the next line must stay intact.
#[test]
fn test_overwrite_mode_appends_at_end_of_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("abc").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("def").unwrap();

    // Back to the end of the first line.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    harness
        .send_key(KeyCode::Insert, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("ZZ").unwrap();

    harness.render().unwrap();
    harness.assert_screen_contains("abcZZ");
    harness.assert_screen_contains("def");
}

/// Toggling overwrite mode off restores normal insertion.
#[test]
fn test_overwrite_mode_toggles_back_to_insert() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("abcd").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    harness
        .send_key(KeyCode::Insert, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap(); // replaces 'a' -> "Xbcd"

    harness
        .send_key(KeyCode::Insert, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Y").unwrap(); // inserts -> "XYbcd"

    harness.render().unwrap();
    harness.assert_screen_contains("XYbcd");
}

/// Replacing in overwrite mode is undoable: undo restores the replaced text.
#[test]
fn test_overwrite_mode_undo_restores_replaced_text() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("abcdef").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Insert, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap();

    harness.render().unwrap();
    harness.assert_screen_contains("Xbcdef");

    // The delete + insert pair from one keypress is a single undo step.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();

    harness.render().unwrap();
    harness.assert_screen_contains("abcdef");
    harness.assert_screen_not_contains("Xbcdef");
}

/// With a selection active, overwrite mode replaces the selection exactly
/// like insert mode does.
#[test]
fn test_overwrite_mode_replaces_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("abcdef").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Insert, KeyModifiers::NONE)
        .unwrap();

    // Select "abc".
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.type_text("X").unwrap();

    harness.render().unwrap();
    harness.assert_screen_contains("Xdef");
    harness.assert_screen_not_contains("abc");
}
