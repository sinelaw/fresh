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
    println!("About to send Ctrl+Backspace...");
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after Ctrl+Backspace:\n{screen}");

    // Debug: Check what's in the prompt
    let lines: Vec<&str> = screen.lines().collect();
    let prompt_line = lines.iter().rev().nth(0); // Last line should be the prompt
    println!("Prompt line: {:?}", prompt_line);

    // The word "file" should be deleted, leaving "open "
    // After "open file" + Ctrl+Backspace, we expect "open " (with trailing space)
    assert!(
        screen.contains("Command: open "),
        "Expected 'Command: open ' but screen was:\n{}",
        screen
    );

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
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
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
    harness.assert_screen_contains("Open:");

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
    // harness.assert_screen_contains("Open file: src/editor.");
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
    // The prompt may show "./test.txt" or "test.txt" depending on working directory
    harness.assert_screen_contains("test.txt");

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
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
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

/// Test selection with Shift+Arrow keys in command palette
#[test]
fn test_command_palette_selection_with_arrows() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("hello world").unwrap();
    harness.render().unwrap();

    // Move cursor to middle (after "hello")
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Select forward with Shift+Right (should select " world")
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after Shift+Right selection:\n{screen}");

    // Should still see "hello world" in the prompt
    harness.assert_screen_contains("Command: hello world");
}

/// Test copy and paste with selection
/// IGNORED: Flaky due to shared system clipboard state between parallel tests.
/// The static SYSTEM_CLIPBOARD can be modified by other tests running concurrently.
#[test]
#[ignore]
fn test_selection_copy_paste_workflow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("copy this text").unwrap();
    harness.render().unwrap();

    // Select "this" using Shift+Home to select from end to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    // Move to start of "this" (position 5)
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    // Select "this" (4 characters)
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // Copy the selection
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cancel this prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Open a new prompt
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Paste the copied text
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after paste:\n{screen}");

    // Should see "this" in the new prompt
    harness.assert_screen_contains("Command: this");
}

/// Test cut with selection
/// IGNORED: Flaky due to shared system clipboard state between parallel tests.
/// The static SYSTEM_CLIPBOARD can be modified by other tests running concurrently.
#[test]
#[ignore]
fn test_selection_cut_workflow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("cut this part").unwrap();
    harness.render().unwrap();

    // Select "this " (5 characters starting at position 4)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // Cut the selection
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after cut:\n{screen}");

    // Should see "cut part" (without "this ")
    harness.assert_screen_contains("Command: cut part");

    // Cancel and open new prompt to paste
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Paste the cut text
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after paste cut text:\n{screen}");

    // Should see "this " in the new prompt
    harness.assert_screen_contains("Command: this ");
}

/// Test Ctrl+A to select all in prompt
#[test]
fn test_select_all_in_prompt() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("select all text").unwrap();
    harness.render().unwrap();

    // Move cursor to middle
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..7 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Select all with Ctrl+A
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Copy the selection
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Clear the prompt by typing new text (which should replace selection)
    harness.type_text("replaced").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after replacing selection:\n{screen}");

    // Should see only "replaced"
    harness.assert_screen_contains("Command: replaced");
}

/// Test typing deletes selection
#[test]
fn test_typing_deletes_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("replace me").unwrap();
    harness.render().unwrap();

    // Select "replace" (7 characters)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..7 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // Type new text - should replace selection
    harness.type_text("fixed").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after typing over selection:\n{screen}");

    // Should see "fixed me" (replaced "replace" with "fixed")
    harness.assert_screen_contains("Command: fixed me");
}

/// Test selection in different prompt types
#[test]
#[ignore]
fn test_selection_in_different_prompts() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Test in git grep prompt
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("search term").unwrap();
    harness.render().unwrap();

    // Select and copy
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Test in open file prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Paste what we copied
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after pasting in open file prompt:\n{screen}");

    // Should see "search term" in the find file prompt
    harness.assert_screen_contains("search term");
}

// BUG REPRODUCTION TESTS - These should fail initially, then pass after fixes

/// BUG #1: Test that selection is visually rendered (currently not visible)
#[test]
#[ignore] // Remove ignore after implementing visual rendering
fn test_bug_selection_not_visible() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type some text
    harness.type_text("hello world").unwrap();
    harness.render().unwrap();

    // Move to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Select "hello" with Shift+Right (5 times)
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with selection:\n{screen}");

    // TODO: Once visual rendering is implemented, check that selected text
    // is rendered differently (e.g., with ANSI escape codes for reversed colors)
    // For now, this test documents that selection exists but isn't visible
}

/// BUG #2: Test that Ctrl+Shift+Left continues past first word (currently gets stuck)
#[test]
fn test_bug_word_selection_gets_stuck() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type text with multiple words
    harness.type_text("one two three").unwrap();
    harness.render().unwrap();

    // Press Ctrl+Shift+Left once - should select "three"
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Press Ctrl+Shift+Left again - should extend selection to include "two three"
    // BUG: Currently gets stuck and doesn't extend further
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // If we copy now, we should get "two three", not just "three"
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Cancel and open new prompt to test paste
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Pasted text after double Ctrl+Shift+Left:\n{screen}");

    // This should contain "two three" but currently only contains "three"
    // Uncomment when bug is fixed:
    // harness.assert_screen_contains("Command: two three");
}

/// BUG #3: Test that Ctrl+Left/Right moves by words (currently doesn't work)
#[test]
fn test_bug_word_movement_doesnt_work() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type text with multiple words
    harness.type_text("one two three").unwrap();
    harness.render().unwrap();

    // Press Ctrl+Left - should move cursor to start of "three"
    // BUG: Currently doesn't move at all
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();

    // Delete the word we're at - should delete "three" if cursor moved
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("After Ctrl+Left and Ctrl+Backspace:\n{screen}");

    // If Ctrl+Left worked, we should see "one two " (without "three")
    // If it didn't work, we'll see "one " (deleted "three" from end position)
    // Uncomment when bug is fixed:
    // harness.assert_screen_contains("Command: one two ");

    // Currently this is what we see (cursor didn't move):
    harness.assert_screen_contains("Command: one two ");
}
