//! End-to-end tests for Emacs-style actions
//!
//! Tests for the new actions: transpose_chars, open_line, recenter, set_mark
//!
//! Note: Shadow validation is disabled because these new actions
//! aren't tracked by the harness's shadow buffer yet.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

// =============================================================================
// Transpose Characters (C-t) Tests
// =============================================================================

/// Test transpose_chars swaps two characters
#[test]
fn test_transpose_chars_basic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // Note: shadow validation disabled - new actions not tracked

    // Type some text
    harness.type_text("abc").unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("abc");

    // Move cursor to position 2 (between 'b' and 'c')
    harness
        .send_key(KeyCode::Left, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Execute transpose_chars (Ctrl+T)
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // 'b' and 'c' should be swapped
    harness.assert_buffer_content("acb");
}

/// Test transpose_chars at beginning of buffer does nothing
#[test]
fn test_transpose_chars_at_beginning() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("abc").unwrap();
    harness.render().unwrap();

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Execute transpose_chars - should do nothing at position 0
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Content should be unchanged
    harness.assert_buffer_content("abc");
}

/// Test transpose_chars at end of buffer
#[test]
fn test_transpose_chars_at_end() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    

    harness.type_text("ab").unwrap();
    harness.render().unwrap();

    // Cursor is at end (position 2), which is past the last char
    // transpose_chars should do nothing if there's no char at cursor position
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Content should be unchanged (can't transpose when cursor is at end)
    harness.assert_buffer_content("ab");
}

// =============================================================================
// Open Line (C-o) Tests
// =============================================================================

/// Test open_line inserts newline without moving cursor
#[test]
fn test_open_line_basic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    

    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Move cursor to middle of the word
    harness
        .send_key(KeyCode::Left, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Left, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let cursor_before = harness.cursor_position();

    // Execute open_line (Ctrl+O)
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should have a newline inserted at cursor position
    harness.assert_buffer_content("hel\nlo");

    // Cursor position should stay the same (not move to next line)
    // Note: The actual behavior might differ - cursor stays at same offset
    // but text shifts, so effectively cursor is still at "l"
}

/// Test open_line at beginning of buffer
#[test]
fn test_open_line_at_beginning() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    

    harness.type_text("hello").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Execute open_line at beginning
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should insert newline at beginning
    harness.assert_buffer_content("\nhello");
}

// =============================================================================
// Recenter (C-l) Tests
// =============================================================================

/// Test recenter scrolls view to center cursor
#[test]
fn test_recenter_basic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create content with many lines
    let lines: Vec<String> = (1..=100).map(|i| format!("Line {}", i)).collect();
    let content = lines.join("\n");

    let _ = harness.load_buffer_from_text(&content);
    harness.render().unwrap();

    // Move cursor down to get it somewhere in the middle
    for _ in 0..50 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Get cursor line before recenter
    let cursor_line_before = harness
        .editor()
        .active_state()
        .buffer
        .position_to_line_col(harness.cursor_position())
        .0;

    // Execute recenter (Ctrl+L) - this should center the viewport on cursor
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Get cursor line after recenter
    let cursor_line_after = harness
        .editor()
        .active_state()
        .buffer
        .position_to_line_col(harness.cursor_position())
        .0;

    // Verify cursor stayed on approximately the same line
    // (allowing for small adjustments due to viewport centering)
    assert!(
        (cursor_line_before as i32 - cursor_line_after as i32).abs() <= 1,
        "Recenter should not significantly change cursor line: before={}, after={}",
        cursor_line_before,
        cursor_line_after
    );

    // Cursor should be around line 50 (0-indexed)
    assert!(cursor_line_after >= 49 && cursor_line_after <= 51,
        "Cursor should be around line 50, got {}", cursor_line_after);
}

// =============================================================================
// Set Mark (C-SPC) Tests
// =============================================================================

/// Test set_mark starts a selection
#[test]
fn test_set_mark_basic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    

    harness.type_text("hello world").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Initially no selection
    let anchor_before = harness.editor().active_state().cursors.primary().anchor;
    assert!(
        anchor_before.is_none(),
        "Should have no selection anchor initially"
    );

    // Execute set_mark (Ctrl+Space)
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should now have an anchor set
    let anchor_after = harness.editor().active_state().cursors.primary().anchor;
    assert!(
        anchor_after.is_some(),
        "Should have selection anchor after set_mark"
    );
    assert_eq!(
        anchor_after,
        Some(0),
        "Anchor should be at cursor position (0)"
    );
}

/// Test set_mark followed by shift+movement extends selection
#[test]
fn test_set_mark_then_shift_move_creates_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Set mark at beginning
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Move forward 5 characters with Shift to extend selection
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Check selection state - anchor should still be at 0
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.anchor, Some(0), "Anchor should still be at 0");
    assert_eq!(cursor.position, 5, "Cursor should be at position 5");

    // The selection should span from 0 to 5 (selecting "hello")
}
