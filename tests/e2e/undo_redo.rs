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
    assert_eq!(harness.editor().active_state().cursors.primary().position, 5);

    // Move cursor left twice with arrow keys (readonly movements)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();

    // Now cursor should be between "hel" and "lo" (position 3)
    assert_eq!(harness.editor().active_state().cursors.primary().position, 3);

    // Undo once - should undo the two cursor movements AND the last typed character 'o'
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
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
    harness.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Undo once - should skip all movements and undo 'c'
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.assert_buffer_content("ab");

    // Undo again - should undo 'b'
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.assert_buffer_content("a");

    // Undo again - should undo 'a'
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
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
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.assert_buffer_content("xy");

    // Redo - should skip the movement and redo 'z'
    harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL).unwrap();
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
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.assert_buffer_content("ab");

    // Undo again should skip the Home movement and undo 'b'
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL).unwrap();
    harness.assert_buffer_content("a");
}
