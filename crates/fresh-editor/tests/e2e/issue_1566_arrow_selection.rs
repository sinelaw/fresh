//! E2E tests for issue #1566: Pressing arrow keys when there's an active
//! selection doesn't behave the same as other editors, text input boxes in
//! browsers, etc.
//!
//! From the issue:
//! > if you select a region and click the right arrow, instead of putting
//! > the cursor on the right side of the selection, it just moves it to the
//! > right of whatever position it previously was.
//!
//! Expected behavior (matching VSCode, Sublime, browsers, etc.):
//! - With an active selection, **Right arrow** should collapse the selection
//!   and place the cursor at the RIGHT edge of the selection (the larger
//!   position), NOT advance by one character from the current cursor.
//! - With an active selection, **Left arrow** should collapse the selection
//!   and place the cursor at the LEFT edge of the selection (the smaller
//!   position), NOT retreat by one character from the current cursor.
//! - This applies regardless of which direction the selection was made
//!   (left-to-right or right-to-left).
//!
//! Current (buggy) behavior:
//! - `Action::MoveRight` always advances the cursor by one grapheme and
//!   clears the selection, even if a selection is present.
//! - `Action::MoveLeft` always retreats the cursor by one grapheme and
//!   clears the selection.
//!
//! <https://github.com/sinelaw/fresh/issues/1566>

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Open a file containing the given content and return the harness.
fn setup(content: &str) -> (EditorTestHarness, TempDir) {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    (harness, temp_dir)
}

/// Move cursor to a specific byte offset by pressing Right from the start.
fn move_to(harness: &mut EditorTestHarness, offset: usize) {
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..offset {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
}

/// Right arrow with a left-to-right selection should place the cursor at
/// the right edge of the selection and clear the selection.
#[test]
fn test_right_arrow_after_forward_selection_goes_to_right_edge() {
    // Content: "hello world"
    //                0....5.....
    // Start at position 2, select forward 3 chars -> selection [2..5], cursor at 5.
    let (mut harness, _tmp) = setup("hello world");
    move_to(&mut harness, 2);

    // Shift+Right three times -> select "llo" (range 2..5), cursor at 5.
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();
    assert!(
        harness.has_selection(),
        "Precondition: a selection should exist after three Shift+Right presses"
    );
    let sel = harness.get_selection_range().unwrap();
    assert_eq!(
        sel,
        2..5,
        "Precondition: selection should be positions 2..5"
    );
    assert_eq!(
        harness.cursor_position(),
        5,
        "Precondition: cursor should be at the right end of the selection"
    );

    // Press Right (no shift). Expected: cursor stays at 5 (right edge),
    // selection cleared. NOT cursor at 6 (which would be "advance from 5").
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        !harness.has_selection(),
        "Selection should be cleared after pressing Right"
    );
    assert_eq!(
        harness.cursor_position(),
        5,
        "Right arrow with forward selection should place cursor at the right \
         edge of the selection (position 5), not advance to 6"
    );
}

/// Right arrow with a BACKWARDS (right-to-left) selection should place the
/// cursor at the RIGHT edge of the selection (the anchor), not advance from
/// the current cursor position.
///
/// This is the core scenario from issue #1566:
/// > if you select a region and click the right arrow, instead of putting
/// > the cursor on the right side of the selection, it just moves it to
/// > the right of whatever position it previously was.
#[test]
fn test_right_arrow_after_backward_selection_goes_to_right_edge() {
    // Content: "hello world"
    // Start at position 5, select backward 3 chars -> selection [2..5], cursor at 2.
    let (mut harness, _tmp) = setup("hello world");
    move_to(&mut harness, 5);

    // Shift+Left three times -> select "llo" (range 2..5), cursor at 2.
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Left, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();
    assert!(
        harness.has_selection(),
        "Precondition: a selection should exist after three Shift+Left presses"
    );
    let sel = harness.get_selection_range().unwrap();
    assert_eq!(
        sel,
        2..5,
        "Precondition: selection should be positions 2..5"
    );
    assert_eq!(
        harness.cursor_position(),
        2,
        "Precondition: cursor should be at the left end of the selection"
    );

    // Press Right (no shift). Expected: cursor jumps to position 5 (the
    // right edge of the selection). Bug: cursor just advances to position 3
    // (one past its previous location).
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        !harness.has_selection(),
        "Selection should be cleared after pressing Right"
    );
    assert_eq!(
        harness.cursor_position(),
        5,
        "Right arrow with backward selection should jump to the right edge \
         of the selection (position 5), not move one character right from \
         the cursor's previous position (3). This is the exact bug described \
         in issue #1566."
    );
}

/// Left arrow with a FORWARD (left-to-right) selection should place the
/// cursor at the LEFT edge of the selection, not retreat by one from the
/// current cursor.
#[test]
fn test_left_arrow_after_forward_selection_goes_to_left_edge() {
    // Content: "hello world"
    // Start at position 2, select forward 3 chars -> selection [2..5], cursor at 5.
    let (mut harness, _tmp) = setup("hello world");
    move_to(&mut harness, 2);

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();
    assert!(harness.has_selection());
    assert_eq!(harness.get_selection_range().unwrap(), 2..5);
    assert_eq!(harness.cursor_position(), 5);

    // Press Left (no shift). Expected: cursor at position 2 (left edge).
    // Bug: cursor moves to position 4 (one left from its previous position 5).
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(
        !harness.has_selection(),
        "Selection should be cleared after pressing Left"
    );
    assert_eq!(
        harness.cursor_position(),
        2,
        "Left arrow with forward selection should jump to the left edge of \
         the selection (position 2), not retreat to position 4"
    );
}

/// Left arrow with a BACKWARDS (right-to-left) selection should also go to
/// the left edge (which is the current cursor position, so effectively stays
/// put and just clears the selection).
#[test]
fn test_left_arrow_after_backward_selection_goes_to_left_edge() {
    let (mut harness, _tmp) = setup("hello world");
    move_to(&mut harness, 5);

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Left, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();
    assert_eq!(harness.get_selection_range().unwrap(), 2..5);
    assert_eq!(harness.cursor_position(), 2);

    // Press Left (no shift). Expected: cursor at position 2 (left edge of
    // selection, which equals current cursor position), selection cleared.
    // Bug: cursor retreats to position 1.
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(!harness.has_selection());
    assert_eq!(
        harness.cursor_position(),
        2,
        "Left arrow with backward selection should stay at the left edge \
         of the selection (position 2), not retreat to position 1"
    );
}

// =============================================================================
// Up / Down with a selection.
//
// Standard editors (VSCode, Sublime, browsers) treat the cursor as "anchored"
// at the *active end* of the selection for vertical motion: Up collapses to
// the TOP (smaller position) of the selection and moves up from there; Down
// collapses to the BOTTOM (larger position) and moves down.
//
// Source layout for these tests (4 lines of 10 chars each, LF-terminated):
//   Line 0:  "aaaaaaaaaa\n"   bytes  0..10,  newline at 10
//   Line 1:  "bbbbbbbbbb\n"   bytes 11..21,  newline at 21
//   Line 2:  "cccccccccc\n"   bytes 22..32,  newline at 32
//   Line 3:  "dddddddddd"     bytes 33..43
// =============================================================================

const MULTI_LINE_CONTENT: &str = "aaaaaaaaaa\nbbbbbbbbbb\ncccccccccc\ndddddddddd";

/// Down arrow with a FORWARD selection should collapse to the right/bottom
/// edge and then move one line down from there.
#[test]
fn test_down_arrow_after_forward_selection_collapses_to_bottom_then_moves_down() {
    // Start at line 1 col 4 (byte 15), select forward to line 2 col 4 (byte 26).
    // Selection range: 15..26, cursor at 26.
    let (mut harness, _tmp) = setup(MULTI_LINE_CONTENT);
    move_to(&mut harness, 15);

    // Shift+Down once, then Shift+Right 0 times: cursor at line 2, col 4 (byte 26)
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.get_selection_range().unwrap(),
        15..26,
        "Precondition: selection should span 15..26 (forward)"
    );
    assert_eq!(harness.cursor_position(), 26);

    // Press Down (no shift). Expected: collapse to 26 (bottom), then move to
    // line 3 col 4 (byte 37). Bug (pre-fix): move from 26 down to line 3 col 4
    // keeping anchor, or advance without collapsing, etc.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(!harness.has_selection(), "Selection should be cleared");
    assert_eq!(
        harness.cursor_position(),
        37,
        "Down with forward selection should collapse to bottom (26) and \
         then move to line 3 col 4 (byte 37)"
    );
}

/// Down arrow with a BACKWARD selection should collapse to the right/bottom
/// edge (the anchor) and then move one line down from there — not down from
/// the cursor's current (top) position.
#[test]
fn test_down_arrow_after_backward_selection_collapses_to_bottom_then_moves_down() {
    // Start at line 2 col 4 (byte 26), select backward to line 1 col 4 (byte 15).
    // Selection range: 15..26, cursor at 15 (top), anchor at 26 (bottom).
    let (mut harness, _tmp) = setup(MULTI_LINE_CONTENT);
    move_to(&mut harness, 26);

    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_selection_range().unwrap(), 15..26);
    assert_eq!(harness.cursor_position(), 15);

    // Press Down (no shift). Expected: collapse to bottom (26) then move to
    // line 3 col 4 (byte 37). Bug: move from current 15 down to line 2 col 4
    // (byte 26 — inside the selection).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(!harness.has_selection());
    assert_eq!(
        harness.cursor_position(),
        37,
        "Down with backward selection should collapse to bottom (26) and \
         then move to line 3 col 4 (byte 37). Bug: cursor ends up at 26 \
         (inside the previously-selected range)."
    );
}

/// Up arrow with a FORWARD selection should collapse to the top (selection
/// start / smaller position) and then move one line up.
#[test]
fn test_up_arrow_after_forward_selection_collapses_to_top_then_moves_up() {
    // Start at line 2 col 4 (byte 26), select forward to line 3 col 4 (byte 37).
    // Selection: 26..37, cursor at 37.
    let (mut harness, _tmp) = setup(MULTI_LINE_CONTENT);
    move_to(&mut harness, 26);

    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_selection_range().unwrap(), 26..37);
    assert_eq!(harness.cursor_position(), 37);

    // Press Up (no shift). Expected: collapse to 26 (top), then move to line 1
    // col 4 (byte 15). Bug: cursor goes up from 37 to 26 (inside selection).
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(!harness.has_selection());
    assert_eq!(
        harness.cursor_position(),
        15,
        "Up with forward selection should collapse to top (26) and then \
         move to line 1 col 4 (byte 15). Bug: cursor lands at 26, which \
         is inside the previously-selected range."
    );
}

/// Up arrow with a BACKWARD selection should collapse to the top (cursor's
/// current position, which is the smaller end) and then move one line up.
#[test]
fn test_up_arrow_after_backward_selection_collapses_to_top_then_moves_up() {
    // Start at line 2 col 4 (byte 26), select backward to line 1 col 4 (byte 15).
    // Selection: 15..26, cursor at 15.
    let (mut harness, _tmp) = setup(MULTI_LINE_CONTENT);
    move_to(&mut harness, 26);

    harness.send_key(KeyCode::Up, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_selection_range().unwrap(), 15..26);
    assert_eq!(harness.cursor_position(), 15);

    // Press Up (no shift). Expected: collapse to 15 (top, == current
    // cursor), then move to line 0 col 4 (byte 4).
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(!harness.has_selection());
    assert_eq!(
        harness.cursor_position(),
        4,
        "Up with backward selection should collapse to top (15) and then \
         move to line 0 col 4 (byte 4)"
    );
}
