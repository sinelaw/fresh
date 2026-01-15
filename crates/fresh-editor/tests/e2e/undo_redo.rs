use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that undo skips over readonly actions (like cursor movement) and only undoes write actions
///
/// This test demonstrates the expected behavior:
/// 1. Type some text
/// 2. Move cursor with arrow keys (readonly actions)
/// 3. Undo once should undo the cursor movements AND the last typed character
#[test]
fn test_undo_skips_readonly_movement_actions() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type "hello"
    harness.type_text("hello").unwrap();
    harness.assert_buffer_content("hello");

    // Cursor should be at end (position 5)
    assert_eq!(
        harness.editor().active_state().cursors.primary().position,
        5
    );

    // Move cursor left twice with arrow keys (readonly movements)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Now cursor should be between "hel" and "lo" (position 3)
    assert_eq!(
        harness.editor().active_state().cursors.primary().position,
        3
    );

    // Undo once - should undo the two cursor movements AND the last typed character 'o'
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Buffer should now be "hell" (last typed character removed)
    harness.assert_buffer_content("hell");

    // Cursor should be restored to where it was BEFORE the movements (position 4, end of "hell")
    // This is the key difference: cursor movements should be undone too!
    assert_eq!(
        harness.editor().active_state().cursors.primary().position,
        4,
        "Cursor should be restored to position before movements"
    );
}

/// Test that multiple undo steps skip over all readonly actions
#[test]
fn test_multiple_undo_skips_all_readonly_actions() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type "abc"
    harness.type_text("abc").unwrap();
    harness.assert_buffer_content("abc");

    // Do various readonly movements
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Undo once - should skip all movements and undo 'c'
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("ab");

    // Undo again - should undo 'b'
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("a");

    // Undo again - should undo 'a'
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("");
}

/// Test that redo also skips readonly actions
#[test]
fn test_redo_skips_readonly_movement_actions() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type "xyz"
    harness.type_text("xyz").unwrap();
    harness.assert_buffer_content("xyz");

    // Move cursor
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Undo - should undo 'z'
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("xy");

    // Redo - should skip the movement and redo 'z'
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("xyz");
}

/// Test undo/redo with mixed write and readonly actions
#[test]
fn test_undo_redo_with_mixed_actions() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type "ab"
    harness.type_text("ab").unwrap();

    // Move to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Type "x" at the beginning
    harness.type_text("x").unwrap();
    harness.assert_buffer_content("xab");

    // Move around
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Undo should skip movements and undo 'x'
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("ab");

    // Undo again should skip the Home movement and undo 'b'
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("a");
}

/// Test that undo to save point correctly marks buffer as not modified (issue #191)
///
/// The issue was that there's an extra undo step which moves cursor to top of screen
/// before the buffer becomes not-dirty. The buffer should become not-dirty exactly
/// when we undo back to the saved state.
#[test]
fn test_undo_to_save_point_marks_buffer_unmodified() {
    use crate::common::fixtures::TestFixture;

    // Create a test file
    let fixture = TestFixture::new("test_undo_save.txt", "initial").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file
    harness.open_file(&fixture.path).unwrap();
    harness.assert_buffer_content("initial");

    // After opening a file from disk, it should NOT be modified
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "Buffer should not be modified after opening"
    );

    // Type a single character to make a minimal change
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("initialX");

    // Now buffer should be modified
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "Buffer should be modified after typing"
    );

    // Undo the single change
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();

    // Content should be back to "initial"
    harness.assert_buffer_content("initial");

    // ISSUE #191: Buffer should be NOT modified immediately when content matches saved state
    // There should NOT be an extra undo step needed
    let is_modified = harness.editor().active_state().buffer.is_modified();
    let cursor_pos = harness.editor().active_state().cursors.primary().position;

    assert!(
        !is_modified,
        "Buffer should be NOT modified after undoing to saved state. \
         There should not be an extra undo step needed to reach unmodified state."
    );

    // Cursor should be within the text bounds, not at some unexpected position like 0
    assert!(
        cursor_pos <= 7,
        "Cursor should be within the text bounds, not at position {} (top of screen)",
        cursor_pos
    );
}

/// Test that undo after "Save As" correctly marks buffer as unmodified (issue #191)
///
/// Scenario from issue:
/// 1. Open a new empty buffer
/// 2. Type some text
/// 3. Save As... (first save)
/// 4. Type more text
/// 5. Undo back to state #3 -> should become not dirty when hitting saved state
///
/// The bug was that the buffer stayed dirty until undoing back to empty instead of
/// stopping at the save point.
#[test]
fn test_undo_after_save_as_marks_buffer_unmodified() {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Step 1: Start with a new empty buffer (already the default state)
    harness.assert_buffer_content("");

    // Step 2: Type some text
    harness.type_text("hello").unwrap();
    harness.assert_buffer_content("hello");

    // Buffer should be modified (not saved yet)
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "Buffer should be modified after typing"
    );

    // Step 3: Save As...
    // Trigger command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to search for Save As command
    harness.type_text("Save File As").unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show the Save As prompt
    harness.assert_screen_contains("Save as:");

    // Type the save path
    let save_path = temp_dir.path().join("test_save_as.txt");
    let save_path_str = save_path.to_str().unwrap();
    harness.type_text(save_path_str).unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify file was saved
    assert!(save_path.exists(), "File should have been saved");
    let saved_content = fs::read_to_string(&save_path).unwrap();
    assert_eq!(saved_content, "hello", "Saved content should match");

    // After Save As, buffer should NOT be modified
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "Buffer should NOT be modified immediately after Save As"
    );

    // Step 4: Type more text
    harness.type_text(" world").unwrap();
    harness.assert_buffer_content("hello world");

    // Buffer should be modified again
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "Buffer should be modified after typing more"
    );

    // Step 5: Undo back to the saved state
    // We typed " world" which is 6 characters, each would be an event
    // But they might be batched. Let's undo until we hit "hello"
    for _ in 0..10 {
        let content = harness.get_buffer_content().unwrap_or_default();
        if content == "hello" {
            break;
        }
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }

    // Should be back to "hello"
    harness.assert_buffer_content("hello");

    // KEY ASSERTION: Buffer should NOT be modified when we've undone back to the save point
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "Buffer should NOT be modified after undoing to the Save As point. \
         The Save As should have marked the event log position as saved."
    );

    // Additional check: undo one more time should change content and still be at a modified state
    // (since now we're before the save point)
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();

    // Content should have changed from "hello"
    let content_after_extra_undo = harness.get_buffer_content().unwrap_or_default();
    if content_after_extra_undo != "hello" {
        // If content changed, buffer should now be modified (we're before the save point)
        assert!(
            harness.editor().active_state().buffer.is_modified(),
            "Buffer should be modified when undoing past the save point"
        );
    }
}

/// Test that undo can go past the save point
///
/// Scenario:
/// 1. Open file with "initial"
/// 2. Edit to "initialX"
/// 3. Save
/// 4. Edit to "initialXY"
/// 5. Undo to "initialX" (at save point, not modified)
/// 6. Undo past save point to "initial" (should work, becomes modified again)
#[test]
fn test_undo_past_save_point() {
    use crate::common::fixtures::TestFixture;

    // Create a test file
    let fixture = TestFixture::new("test_undo_past_save.txt", "initial").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file
    harness.open_file(&fixture.path).unwrap();
    harness.assert_buffer_content("initial");

    // Step 2: Edit to "initialX"
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("initialX");
    assert!(harness.editor().active_state().buffer.is_modified());

    // Step 3: Save
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify saved
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "Should not be modified after save"
    );

    // Step 4: Edit to "initialXY"
    harness.type_text("Y").unwrap();
    harness.assert_buffer_content("initialXY");
    assert!(harness.editor().active_state().buffer.is_modified());

    // Step 5: Undo to "initialX" (at save point)
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("initialX");
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "Should not be modified at save point"
    );

    // Step 6: Undo past save point - THIS IS THE KEY TEST
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();

    // Should be back to "initial"
    harness.assert_buffer_content("initial");

    // Buffer should now be modified (we're before the save point)
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "Should be modified after undoing past save point"
    );
}

/// Test undoing all the way back to empty buffer after Save As
///
/// Scenario:
/// 1. Create new buffer (empty)
/// 2. Type "hello"
/// 3. Save As...
/// 4. Type " world"
/// 5. Undo repeatedly until back to empty buffer
#[test]
fn test_undo_to_empty_after_save_as() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Step 1: Start with empty buffer
    harness.assert_buffer_content("");

    // Step 2: Type "hello"
    harness.type_text("hello").unwrap();
    harness.assert_buffer_content("hello");

    // Step 3: Save As...
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Save File As").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let save_path = temp_dir.path().join("test_undo_empty.txt");
    harness.type_text(save_path.to_str().unwrap()).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify saved
    assert!(save_path.exists());
    assert!(!harness.editor().active_state().buffer.is_modified());

    // Record event log state immediately after Save As
    let event_log_len_after_save = harness.editor().active_event_log().len();
    println!(
        "Immediately after Save As: event_log len={}",
        event_log_len_after_save
    );

    // The event log should have all 5 events from typing "hello"
    assert_eq!(
        event_log_len_after_save, 5,
        "Event log should have 5 events immediately after Save As"
    );

    // Simulate what happens when the file watcher detects the file change.
    // This is the scenario that was causing the bug: auto-revert would clear
    // the event log even when the file content hadn't actually changed.
    harness
        .editor_mut()
        .handle_file_changed(save_path.to_str().unwrap());

    // Check event log state after file change notification
    let event_log_len = harness.editor().active_event_log().len();
    println!(
        "After file change notification: event_log len={}",
        event_log_len
    );

    // The event log should STILL have all 5 events - revert should be skipped
    // when the file content matches what's already in the buffer
    assert_eq!(event_log_len, 5, "Event log should still have 5 events - revert should not clear undo history when content is unchanged");

    // Step 4: Type " world"
    harness.type_text(" world").unwrap();
    harness.assert_buffer_content("hello world");
    assert!(harness.editor().active_state().buffer.is_modified());

    // Step 5: Undo repeatedly until empty
    // First undo back to save point
    for i in 0..20 {
        let content = harness.get_buffer_content().unwrap_or_default();
        println!(
            "Undo iteration {}: content='{}', modified={}",
            i,
            content,
            harness.editor().active_state().buffer.is_modified()
        );

        if content.is_empty() {
            break;
        }

        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }

    // Should be back to empty
    let final_content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        final_content.is_empty(),
        "Should be able to undo all the way back to empty buffer, but got: '{}'",
        final_content
    );
}
