use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test adding cursor at next match with Ctrl+D
#[test]
fn test_add_cursor_next_match() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text with repeated words
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

    // Verify selection
    let primary = harness.editor().active_state().cursors.primary();
    assert_eq!(primary.position, 3);
    assert_eq!(primary.anchor, Some(0));

    // Press Ctrl+D to add cursor at next "foo"
    harness.editor_mut().add_cursor_at_next_match();
    harness.render().unwrap();

    // Should now have 2 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 2);

    // Press Ctrl+D again to add cursor at third "foo"
    harness.editor_mut().add_cursor_at_next_match();
    harness.render().unwrap();

    // Should now have 3 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 3);
}

/// Test adding cursor above with Ctrl+Alt+Up
#[test]
fn test_add_cursor_above() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Position cursor on Line 3
    harness.assert_buffer_content("Line 1\nLine 2\nLine 3");

    // Add cursor above (to Line 2)
    harness.editor_mut().add_cursor_above();
    harness.render().unwrap();

    // Should now have 2 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 2);

    // Add cursor above again (to Line 1)
    harness.editor_mut().add_cursor_above();
    harness.render().unwrap();

    // Should now have 3 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 3);
}

/// Test adding cursor below with Ctrl+Alt+Down
#[test]
fn test_add_cursor_below() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Position cursor on Line 1
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add cursor below (to Line 2)
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 2 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 2);

    // Add cursor below again (to Line 3)
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 3 cursors
    let cursors = &harness.editor().active_state().cursors;
    assert_eq!(cursors.iter().count(), 3);
}

/// Test multi-cursor typing
#[test]
fn test_multi_cursor_typing() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines with more content
    harness.type_text("aaa\nbbb\nccc\nddd").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add cursors - each time we add a cursor below, the new cursor becomes primary
    // So we can continue adding cursors below
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1 and 2
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1, 2, and 3

    // Should have 3 cursors
    let cursor_count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(cursor_count, 3, "Should have 3 cursors");

    // Type "X" with all three cursors
    harness.type_text("X").unwrap();

    // Each cursor should insert X at its position
    let result = harness.get_buffer_content();

    // Count how many X's were inserted
    let x_count = result.matches('X').count();
    assert_eq!(
        x_count, 3,
        "Should have inserted exactly 3 X's, one per cursor"
    );
}

/// Test removing secondary cursors with Esc
#[test]
fn test_remove_secondary_cursors() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create multiple lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Add cursors above
    harness.editor_mut().add_cursor_above();
    harness.editor_mut().add_cursor_above();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 3);

    // Remove secondary cursors
    harness
        .editor_mut()
        .active_state_mut()
        .cursors
        .remove_secondary();
    harness.render().unwrap();

    // Should have only 1 cursor now
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 1);
}

/// Test multi-cursor undo atomicity
/// When using multiple cursors, undo should undo all cursor actions in one step
#[test]
fn test_multi_cursor_undo_atomic() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines with more content (matching the working test)
    harness.type_text("aaa\nbbb\nccc\nddd").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add cursors - each time we add a cursor below, the new cursor becomes primary
    // So we can continue adding cursors below
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1 and 2
    harness.editor_mut().add_cursor_below(); // Now we have cursors on line 1, 2, and 3

    // Should have 3 cursors
    let cursor_count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(cursor_count, 3, "Should have 3 cursors");

    // Type "X" with all three cursors - this should create a batch event
    harness.type_text("X").unwrap();

    // Each cursor should insert X at its position
    let result = harness.get_buffer_content();

    // Count how many X's were inserted
    let x_count = result.matches('X').count();
    assert_eq!(
        x_count, 3,
        "Should have inserted exactly 3 X's, one per cursor. Buffer: {}", result
    );

    // Undo once - this should undo ALL three insertions atomically
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // All X's should be gone after a single undo
    let result_after_undo = harness.get_buffer_content();
    let x_count_after_undo = result_after_undo.matches('X').count();
    assert_eq!(
        x_count_after_undo, 0,
        "Should have removed all X's with single undo. Buffer: {}", result_after_undo
    );
    harness.assert_buffer_content("aaa\nbbb\nccc\nddd");

    // Redo once - this should redo ALL three insertions atomically
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // All X's should be back after a single redo
    let result_after_redo = harness.get_buffer_content();
    let x_count_after_redo = result_after_redo.matches('X').count();
    assert_eq!(
        x_count_after_redo, 3,
        "Should have restored all 3 X's with single redo. Buffer: {}", result_after_redo
    );
}

/// Test multi-cursor delete undo atomicity
#[test]
fn test_multi_cursor_delete_undo_atomic() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("aaa\nbbb\nccc").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add two more cursors
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.iter().count(), 3);

    // Delete forward at all three cursors - should delete 'a', 'b', 'c'
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();

    // Verify first character deleted from each line
    harness.assert_buffer_content("aa\nbb\ncc");

    // Undo once - should restore all three characters
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // All characters should be restored
    harness.assert_buffer_content("aaa\nbbb\nccc");
}

/// Test that adding cursors can be undone
#[test]
fn test_add_cursor_undo() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Should start with 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Add a cursor below
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Add another cursor below
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Undo - should remove the last cursor added
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Undo again - should remove the second cursor
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Redo - should add cursor back
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);
}

/// Test that removing cursors can be undone
#[test]
fn test_remove_cursor_undo() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("Line 1\nLine 2\nLine 3").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Add two cursors
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Remove secondary cursors (using Escape)
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Undo - should restore the secondary cursors
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 3 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 3);

    // Redo - should remove them again
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);
}

/// Test undo beyond cursor add removes the cursor and undoes the edit
#[test]
fn test_undo_beyond_cursor_add() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create three lines
    harness.type_text("aaa\nbbb\nccc").unwrap();

    // Go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // Should start with 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Add a cursor below
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();

    // Should now have 2 cursors
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Type "X" with both cursors
    harness.type_text("X").unwrap();

    // Should have X inserted at both positions
    let result = harness.get_buffer_content();
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 2, "Should have 2 X's. Buffer: {}", result);

    // Undo - should undo the batch insertion
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // X's should be gone, but we should still have 2 cursors
    let result = harness.get_buffer_content();
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 0, "Should have 0 X's. Buffer: {}", result);
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Undo again - should remove the second cursor
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should be back to 1 cursor
    assert_eq!(harness.editor().active_state().cursors.count(), 1);

    // Redo - should add the cursor back
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // Should have 2 cursors again
    assert_eq!(harness.editor().active_state().cursors.count(), 2);

    // Redo again - should redo the batch insertion
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();

    // X's should be back
    let result = harness.get_buffer_content();
    let x_count = result.matches('X').count();
    assert_eq!(x_count, 2, "Should have 2 X's back. Buffer: {}", result);
}
