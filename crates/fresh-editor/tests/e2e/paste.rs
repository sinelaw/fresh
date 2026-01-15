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

// ============================================================================
// Prompt paste tests
// ============================================================================

/// Test that external paste (bracketed paste / Ctrl+Shift+V) goes to prompt when prompt is open
///
/// Bug: Previously, external paste always went to the editor buffer, ignoring the open prompt
#[test]
fn test_external_paste_goes_to_prompt() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text in the buffer first
    harness.type_text("buffer content").unwrap();
    harness.assert_buffer_content("buffer content");

    // Open the command palette prompt
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command:");

    // Simulate external paste (bracketed paste) - this should go to the prompt, not the buffer
    harness.editor_mut().paste_text("pasted text".to_string());
    harness.render().unwrap();

    // The pasted text should appear in the prompt
    harness.assert_screen_contains("Command: pasted text");

    // The buffer should NOT be modified
    harness.assert_buffer_content("buffer content");
}

/// Test that external paste works in the Open File prompt
#[test]
fn test_external_paste_in_open_file_prompt() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the "Open File" prompt
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open file:");

    // Simulate external paste of a file path
    harness
        .editor_mut()
        .paste_text("/path/to/file.txt".to_string());
    harness.render().unwrap();

    // The path should appear in the prompt
    harness.assert_screen_contains("/path/to/file.txt");
}

/// Test that external paste appends to existing text in prompt
#[test]
fn test_external_paste_appends_to_prompt() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open prompt and type some text
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("hello ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command: hello ");

    // Paste more text
    harness.editor_mut().paste_text("world".to_string());
    harness.render().unwrap();

    // Should see both typed and pasted text
    harness.assert_screen_contains("Command: hello world");
}

/// Test that Ctrl+V paste works in prompt
#[test]
fn test_ctrl_v_paste_in_prompt() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Set clipboard content
    harness
        .editor_mut()
        .set_clipboard_for_test("clipboard content".to_string());

    // Open prompt
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command:");

    // Press Ctrl+V to paste
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should see pasted text in prompt
    harness.assert_screen_contains("Command: clipboard content");
}

/// Test copy and paste workflow within prompt
#[test]
fn test_prompt_copy_paste_workflow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Enable internal-only clipboard to avoid system clipboard interference in parallel tests
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Open prompt and type some text
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("copy me").unwrap();
    harness.render().unwrap();

    // Copy all text with Ctrl+C
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Clear the prompt by selecting all and deleting
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The prompt should be empty now
    harness.assert_screen_contains("Command:");

    // Paste the copied text back
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should see the copied text
    harness.assert_screen_contains("Command: copy me");
}

/// Test cut and paste workflow in prompt
#[test]
fn test_prompt_cut_paste_workflow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Enable internal-only clipboard to avoid system clipboard interference in parallel tests
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Open prompt and type some text
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("cut me").unwrap();
    harness.process_async_and_render().unwrap();
    harness.assert_screen_contains("Command: cut me");

    // Cut all text with Ctrl+X
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // The prompt should be empty now (text was cut)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Command:") && !screen.contains("cut me"),
        "Prompt should be empty after cut. Screen:\n{}",
        screen
    );

    // Paste the cut text back
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should see the cut text pasted back
    harness.assert_screen_contains("Command: cut me");
}

/// Test that copy with selection only copies selected text
#[test]
fn test_prompt_copy_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Enable internal-only clipboard to avoid system clipboard interference in parallel tests
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Open prompt and type some text
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("hello world").unwrap();
    harness.render().unwrap();

    // Move to start and select "hello" (5 characters)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // Copy selection
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Cancel prompt and open a new one
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Paste - should only paste "hello", not "hello world"
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Command: hello");
    // Verify we didn't paste "world"
    let screen = harness.screen_to_string();
    // Find the Command: line
    let prompt_content = screen
        .lines()
        .find(|line| line.contains("Command:"))
        .unwrap_or("");
    assert!(
        !prompt_content.contains("world"),
        "Should only paste selected text 'hello', not 'world'. Line: {}",
        prompt_content
    );
}

/// Test that cut with selection only cuts selected text
#[test]
fn test_prompt_cut_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Enable internal-only clipboard to avoid system clipboard interference in parallel tests
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Open prompt and type some text
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("hello world").unwrap();
    harness.render().unwrap();

    // Move to start and select "hello " (6 characters)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // Cut selection
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should only have "world" remaining
    harness.assert_screen_contains("Command: world");

    // Cancel and open new prompt to verify cut text
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should paste "hello "
    harness.assert_screen_contains("Command: hello ");
}

/// Test paste replaces selection in prompt
#[test]
fn test_prompt_paste_replaces_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Set clipboard
    harness
        .editor_mut()
        .set_clipboard_for_test("replaced".to_string());

    // Open prompt and type some text
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("hello world").unwrap();
    harness.render().unwrap();

    // Select "world" (move to position 6, then select 5 chars)
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

    // Paste - should replace "world" with "replaced"
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Command: hello replaced");
}

/// Test external paste replaces selection in prompt
#[test]
fn test_external_paste_replaces_prompt_selection() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open prompt and type some text
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("old text").unwrap();
    harness.render().unwrap();

    // Select all
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();

    // External paste should replace selection
    harness.editor_mut().paste_text("new text".to_string());
    harness.render().unwrap();

    harness.assert_screen_contains("Command: new text");
    let screen = harness.screen_to_string();
    let prompt_line = screen
        .lines()
        .find(|l| l.contains("Command:"))
        .unwrap_or("");
    assert!(
        !prompt_line.contains("old"),
        "Old text should be replaced. Line: {}",
        prompt_line
    );
}

/// Test that paste in search prompt works
#[test]
fn test_paste_in_search_prompt() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some content to search in
    harness.type_text("find this text").unwrap();

    // Set clipboard
    harness
        .editor_mut()
        .set_clipboard_for_test("this".to_string());

    // Open search prompt
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search:");

    // Paste the search term
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Search: this");
}

// ============================================================================
// CRLF paste normalization tests (Issue #427)
// ============================================================================

/// Test that pasting CRLF text is normalized to the buffer's line ending format
/// Issue #427: On Windows, pasting multiline text collapsed into single line
#[test]
fn test_paste_crlf_text_normalized() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Start with empty buffer (default LF line endings)
    harness.assert_buffer_content("");

    // Paste text with Windows CRLF line endings
    harness
        .editor_mut()
        .paste_text("line1\r\nline2\r\nline3".to_string());
    harness.render().unwrap();

    // Should be normalized to LF (the buffer's default)
    harness.assert_buffer_content("line1\nline2\nline3");
}

/// Test that pasting CR-only text (old Mac) is normalized
#[test]
fn test_paste_cr_only_text_normalized() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Start with empty buffer
    harness.assert_buffer_content("");

    // Paste text with old Mac CR-only line endings
    harness
        .editor_mut()
        .paste_text("line1\rline2\rline3".to_string());
    harness.render().unwrap();

    // Should be normalized to LF
    harness.assert_buffer_content("line1\nline2\nline3");
}

/// Test that pasting into a CRLF buffer preserves CRLF format
#[test]
fn test_paste_into_crlf_buffer() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("crlf_paste_test.txt");

    // Create a file with CRLF line endings
    std::fs::write(&file_path, "existing\r\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move to end of buffer
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();

    // Paste text (even with LF, should convert to CRLF)
    harness.editor_mut().paste_text("new\nlines".to_string());
    harness.render().unwrap();

    // Buffer should now contain both original and pasted text with CRLF
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("\r\n"),
        "Pasted text should use CRLF in CRLF buffer"
    );
    assert!(
        content.contains("existing\r\nnew\r\nlines"),
        "Content should be: {:?}",
        content
    );
}

/// Test that mixed line endings in paste are all normalized
#[test]
fn test_paste_mixed_line_endings() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Paste text with mixed line endings (CRLF, CR, LF)
    harness
        .editor_mut()
        .paste_text("crlf\r\ncr\rlf\n".to_string());
    harness.render().unwrap();

    // All should be normalized to LF
    harness.assert_buffer_content("crlf\ncr\nlf\n");
}

/// Test that pasting CRLF into prompt works correctly
#[test]
fn test_paste_crlf_into_prompt() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the command palette prompt
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command:");

    // Paste text with CRLF (should be normalized to LF for prompt)
    harness
        .editor_mut()
        .paste_text("line1\r\nline2".to_string());
    harness.render().unwrap();

    // Prompt should contain the text (newlines may be shown differently in prompt)
    harness.assert_screen_contains("line1");
}
