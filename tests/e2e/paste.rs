//! E2E tests for paste handling
//!
//! These tests verify paste behavior including:
//! - Paste with selection (should replace selection)
//! - Multi-cursor paste
//! - Paste undo atomicity
//!
//! Issue #372: External paste should behave like internal paste

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that paste replaces the current selection
/// Bug: Current paste() doesn't delete selection before inserting
#[test]
fn test_paste_replaces_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text
    harness.type_text("hello world").unwrap();
    harness.assert_buffer_content("hello world");

    // Select "world" (positions 6-11)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // Verify selection
    let primary = harness.editor().active_state().cursors.primary();
    assert_eq!(primary.position, 11, "Cursor should be at end of 'world'");
    assert_eq!(
        primary.anchor,
        Some(6),
        "Anchor should be at start of 'world'"
    );

    // Set clipboard content and paste (use test-only paste to avoid system clipboard interference)
    harness
        .editor_mut()
        .set_clipboard_for_test("universe".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();

    // "world" should be replaced with "universe"
    harness.assert_buffer_content("hello universe");
}

/// Test that paste works with multiple cursors
/// Bug: Current paste() only handles primary cursor
#[test]
fn test_paste_with_multiple_cursors() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("aaa\nbbb\nccc").unwrap();
    harness.assert_buffer_content("aaa\nbbb\nccc");

    // Go to start and add cursors on each line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Set clipboard and paste
    harness.editor_mut().set_clipboard_for_test("X".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();

    // Should have X inserted at start of each line
    let content = harness.get_buffer_content().unwrap();
    let x_count = content.matches('X').count();
    assert_eq!(
        x_count, 3,
        "Should have 3 X's (one per cursor), got {}. Buffer:\n{}",
        x_count, content
    );
    harness.assert_buffer_content("Xaaa\nXbbb\nXccc");
}

/// Test that paste with multiple cursors and selections replaces all selections
#[test]
fn test_paste_replaces_multiple_selections() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create text with repeated words
    harness.type_text("foo bar foo baz foo").unwrap();
    harness.assert_buffer_content("foo bar foo baz foo");

    // Select the first "foo" (positions 0-3)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();

    // Add cursor at next "foo" match (Ctrl+D behavior)
    harness.editor_mut().add_cursor_at_next_match();
    harness.render().unwrap();

    // Add cursor at third "foo" match
    harness.editor_mut().add_cursor_at_next_match();
    harness.render().unwrap();

    // Should have 3 cursors, each selecting "foo"
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Set clipboard and paste
    harness
        .editor_mut()
        .set_clipboard_for_test("XXX".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();

    // All "foo"s should be replaced with "XXX"
    harness.assert_buffer_content("XXX bar XXX baz XXX");
}

/// Test that paste is atomic for undo (single undo step)
#[test]
fn test_paste_undo_is_atomic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type initial text
    harness.type_text("hello").unwrap();
    harness.assert_buffer_content("hello");

    // Paste some text
    harness
        .editor_mut()
        .set_clipboard_for_test(" world".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();
    harness.assert_buffer_content("hello world");

    // Undo should remove entire paste in one step
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("hello");

    // Redo should restore entire paste in one step
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("hello world");
}

/// Test that multi-cursor paste is atomic for undo
#[test]
fn test_multi_cursor_paste_undo_is_atomic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("aaa\nbbb\nccc").unwrap();
    harness.assert_buffer_content("aaa\nbbb\nccc");

    // Go to start and add cursors
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Paste
    harness.editor_mut().set_clipboard_for_test("X".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();

    // Verify paste worked
    let content = harness.get_buffer_content().unwrap();
    let x_count = content.matches('X').count();
    assert_eq!(x_count, 3, "Should have 3 X's. Buffer:\n{}", content);

    // Single undo should remove ALL X's
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let content_after_undo = harness.get_buffer_content().unwrap();
    let x_count_after_undo = content_after_undo.matches('X').count();
    assert_eq!(
        x_count_after_undo, 0,
        "Single undo should remove all X's. Buffer:\n{}",
        content_after_undo
    );
    harness.assert_buffer_content("aaa\nbbb\nccc");
}

/// Test paste with selection replacement is atomic for undo
/// This is the most complex case: delete selection + insert = one undo step
#[test]
fn test_paste_with_selection_undo_is_atomic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text
    harness.type_text("hello world").unwrap();
    harness.assert_buffer_content("hello world");

    // Select "world"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // Paste to replace selection
    harness
        .editor_mut()
        .set_clipboard_for_test("universe".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();
    harness.assert_buffer_content("hello universe");

    // Single undo should restore "world" (undo both delete and insert)
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("hello world");

    // Redo should replace "world" with "universe" again
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("hello universe");
}

/// Test that pasting multiline text works correctly
#[test]
fn test_paste_multiline_text() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Start with empty buffer
    harness.assert_buffer_content("");

    // Paste multiline text
    harness
        .editor_mut()
        .set_clipboard_for_test("line1\nline2\nline3".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();

    harness.assert_buffer_content("line1\nline2\nline3");

    // Single undo should remove all three lines
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("");
}

/// Test that paste at end of line works correctly
#[test]
fn test_paste_at_end_of_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello").unwrap();
    harness.assert_buffer_content("hello");

    // Cursor is already at end of line after typing
    harness
        .editor_mut()
        .set_clipboard_for_test(" world".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();

    harness.assert_buffer_content("hello world");
}

/// Test that paste in middle of text works correctly
#[test]
fn test_paste_in_middle() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("helloworld").unwrap();
    harness.assert_buffer_content("helloworld");

    // Move to position 5 (between "hello" and "world")
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    harness.editor_mut().set_clipboard_for_test(" ".to_string());
    harness.editor_mut().paste_for_test();
    harness.render().unwrap();

    harness.assert_buffer_content("hello world");
}
