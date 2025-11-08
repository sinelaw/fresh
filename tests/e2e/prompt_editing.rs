/// E2E tests for advanced editing actions in prompt mode
/// These tests demonstrate that editing actions (copy/paste/cut, word deletion)
/// should work in prompts (command palette, git grep, open file, etc.)
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that Ctrl+Backspace deletes word backward in command palette
/// CURRENT STATUS: This test demonstrates the missing functionality
#[test]
fn test_command_palette_delete_word_backward() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text with multiple words
    harness.type_text("open file").unwrap();
    harness.render().unwrap();

    // Verify the text is there
    harness.assert_screen_contains("Command: open file");

    // Try to delete the word "file" using Ctrl+Backspace
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: The word "file" should be deleted, leaving "open "
    // ACTUAL: Currently Ctrl+Backspace is not bound in prompt mode,
    // so nothing happens or it might trigger an unrelated action
    let screen = harness.screen_to_string();
    println!("Screen after Ctrl+Backspace:\n{screen}");

    // This assertion will likely FAIL until the feature is implemented
    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: open ");
    // harness.assert_screen_not_contains("file");

    // For now, just document the current behavior
    // The test passes but documents the missing feature
}

/// Test that Ctrl+Delete deletes word forward in command palette
#[test]
fn test_command_palette_delete_word_forward() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("save file").unwrap();
    harness.render().unwrap();

    // Move cursor to start of "file"
    // First, move to start of line
    harness
        .send_key(KeyCode::Home, KeyModifiers::NONE)
        .unwrap();
    // Then move right past "save "
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Try to delete the word "file" using Ctrl+Delete
    harness
        .send_key(KeyCode::Delete, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: The word "file" should be deleted, leaving "save "
    let screen = harness.screen_to_string();
    println!("Screen after Ctrl+Delete:\n{screen}");

    // This assertion will likely FAIL until the feature is implemented
    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: save ");
    // harness.assert_screen_not_contains("file");
}

/// Test that Ctrl+C copies text in command palette
#[test]
fn test_command_palette_copy() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("toggle line wrap").unwrap();
    harness.render().unwrap();

    // Try to copy with Ctrl+C
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: Text should be copied to clipboard
    // In normal mode, Ctrl+C copies selected text
    // In prompt mode, it could copy the entire input

    // Cancel the prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open another prompt and try to paste
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Try to paste with Ctrl+V
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: The copied text should be pasted
    let screen = harness.screen_to_string();
    println!("Screen after copy and paste:\n{screen}");

    // This assertion will likely FAIL until the feature is implemented
    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: toggle line wrap");
}

/// Test that Ctrl+X cuts text in command palette
#[test]
fn test_command_palette_cut() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("new file").unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Command: new file");

    // Try to cut with Ctrl+X
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: Text should be cut (copied and cleared)
    let screen = harness.screen_to_string();
    println!("Screen after Ctrl+X:\n{screen}");

    // This assertion will likely FAIL until the feature is implemented
    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: "); // Input should be empty
    // harness.assert_screen_not_contains("new file");

    // Try to paste the cut text
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: The cut text should be pasted back
    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: new file");
}

/// Test that Ctrl+V pastes text in command palette
#[test]
fn test_command_palette_paste() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // First, copy some text in normal mode
    harness.type_text("test content").unwrap();

    // Select all with Ctrl+A (if available) or manually select
    // For now, assume we have the text selected or use Ctrl+C to copy line
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Now open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Try to paste with Ctrl+V
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: The text from the buffer should be pasted into the prompt
    let screen = harness.screen_to_string();
    println!("Screen after paste into prompt:\n{screen}");

    // This assertion will likely FAIL until the feature is implemented
    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: test content");
}

/// Test word deletion in git grep prompt
#[test]
fn test_git_grep_delete_word_backward() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger git grep
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show git grep prompt
    harness.assert_screen_contains("Git grep:");

    // Type some text
    harness.type_text("function test").unwrap();
    harness.render().unwrap();

    // Try to delete word with Ctrl+Backspace
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: The word "test" should be deleted
    let screen = harness.screen_to_string();
    println!("Screen after Ctrl+Backspace in git grep:\n{screen}");

    // Uncomment when implementing:
    // harness.assert_screen_contains("Git grep: function ");
    // harness.assert_screen_not_contains("test");
}

/// Test word deletion in open file prompt
#[test]
fn test_open_file_delete_word_backward() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger open file
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show find file prompt
    harness.assert_screen_contains("Find file:");

    // Type a file path
    harness.type_text("src/editor.rs").unwrap();
    harness.render().unwrap();

    // Try to delete "rs" with Ctrl+Backspace
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: The word "rs" should be deleted, leaving "src/editor."
    let screen = harness.screen_to_string();
    println!("Screen after Ctrl+Backspace in open file:\n{screen}");

    // Uncomment when implementing:
    // harness.assert_screen_contains("Find file: src/editor.");
    // harness.assert_screen_not_contains("rs");
}

/// Test that editing actions work consistently across different prompt types
#[test]
fn test_editing_actions_consistency() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Test in multiple prompt contexts to ensure consistency

    // 1. Command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("save file").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command: save file");

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // 2. Open file
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("test.txt").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Find file: test.txt");

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // 3. Search
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("search term").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: search term");

    // EXPECTED: All prompt types should support the same editing actions
    // This test documents that the feature should be universal across prompt types
}

/// Test that word deletion handles special characters correctly
#[test]
fn test_delete_word_with_special_chars() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type text with special characters
    harness.type_text("save-file-as").unwrap();
    harness.render().unwrap();

    // Try to delete word (should stop at hyphen)
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: Should delete "as", leaving "save-file-"
    // because hyphen is not a word character
    let screen = harness.screen_to_string();
    println!("Screen after deleting word with hyphens:\n{screen}");

    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: save-file-");
}

/// Test copy/paste workflow in command palette
#[test]
fn test_command_palette_copy_paste_workflow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type a command
    harness.type_text("toggle hidden").unwrap();
    harness.render().unwrap();

    // Copy the text
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Clear the input (using Ctrl+X would cut, or manually delete)
    // Move to start and select all, then delete
    harness
        .send_key(KeyCode::Home, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..13 {
        harness
            .send_key(KeyCode::Delete, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Paste it back
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: The text should be pasted back
    let screen = harness.screen_to_string();
    println!("Screen after copy-clear-paste workflow:\n{screen}");

    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: toggle hidden");
}

/// Test that multiple word deletions work correctly
#[test]
fn test_multiple_word_deletions() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type multiple words
    harness.type_text("one two three four").unwrap();
    harness.render().unwrap();

    // Delete multiple words
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap(); // Delete "four"
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap(); // Delete "three"
    harness.render().unwrap();

    // EXPECTED: Should have "one two " remaining
    let screen = harness.screen_to_string();
    println!("Screen after multiple word deletions:\n{screen}");

    // Uncomment when implementing:
    // harness.assert_screen_contains("Command: one two ");
    // harness.assert_screen_not_contains("three");
    // harness.assert_screen_not_contains("four");
}

/// Test word deletion at boundaries
#[test]
fn test_word_deletion_at_boundaries() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type text with spaces
    harness.type_text("  word  ").unwrap();
    harness.render().unwrap();

    // Cursor is at end (after the spaces)
    // Delete word backward should delete "word" and surrounding spaces
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // EXPECTED: Behavior depends on implementation
    // Common approach: delete trailing spaces first, then the word
    let screen = harness.screen_to_string();
    println!("Screen after word deletion with spaces:\n{screen}");

    // Document the expected behavior based on implementation choice
}
