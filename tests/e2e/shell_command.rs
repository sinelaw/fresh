//! E2E tests for shell command execution on buffer/region content.
//!
//! These tests verify:
//! - Running shell commands with buffer content as stdin
//! - Output to new buffer vs replace mode
//! - Selection vs entire buffer
//! - Command failure handling

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test running a shell command (sort) with output to a new buffer
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_to_new_buffer() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("unsorted.txt");
    std::fs::write(&file_path, "cherry\napple\nbanana\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify initial content
    harness.assert_buffer_content("cherry\napple\nbanana\n");

    // Open command palette and run shell command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap(); // Wait for shell command input prompt

    // Type the sort command
    harness.type_text("sort").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // A new buffer should be created with sorted output
    harness.assert_buffer_content("apple\nbanana\ncherry\n");

    // Status should indicate shell output buffer
    harness.assert_screen_contains("Shell: sort");
}

/// Test running a shell command with replace mode
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_replace_buffer() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("unsorted.txt");
    std::fs::write(&file_path, "cherry\napple\nbanana\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify initial content
    harness.assert_buffer_content("cherry\napple\nbanana\n");

    // Open command palette and run shell command (replace)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command (replace)").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap(); // Wait for shell command input prompt

    // Type the sort command
    harness.type_text("sort").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Buffer should be replaced with sorted content
    harness.assert_buffer_content("apple\nbanana\ncherry\n");

    // Should still be in the same buffer (not a new one)
    harness.assert_screen_contains("unsorted.txt");
}

/// Test shell command on selection only
/// Note: This test is complex due to selection mode behavior - skipping for now
#[test]
#[ignore = "Selection-based shell commands require more complex test setup"]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_on_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("mixed.txt");
    // First line should not be sorted, only the selected portion
    std::fs::write(&file_path, "header\ncherry\napple\nbanana\nfooter\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move to line 2 (skip header)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Select lines 2-4 (cherry, apple, banana)
    // Start visual line selection
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Move down 2 lines to include all three fruit lines
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open command palette and run shell command (replace)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("shell command (replace)").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Sort the selection
    harness.type_text("sort").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Only the selected portion should be sorted
    // Header and footer should remain unchanged
    harness.assert_buffer_content("header\napple\nbanana\ncherry\nfooter\n");
}

/// Test shell command failure handling
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_failure() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "some content\n").unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Open command palette and run shell command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap(); // Wait for shell command input prompt

    // Run a command that will fail (exit 1)
    harness.type_text("false").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Original buffer content should be unchanged when command fails
    // (Error message varies by platform, so we don't check the exact message)
    harness.assert_buffer_content("some content\n");
}

/// Test shell command with tr (character transformation)
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_tr_transform() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("lowercase.txt");
    std::fs::write(&file_path, "hello world\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Open command palette and run shell command (replace)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command (replace)").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap(); // Wait for shell command input prompt

    // Transform to uppercase
    harness.type_text("tr a-z A-Z").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Content should be uppercase
    harness.assert_buffer_content("HELLO WORLD\n");
}

/// Test shell command undo after replace
/// The shell command replace creates a Batch event for atomic undo
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_replace_undo() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Type original content in a new buffer (to have clean undo history)
    harness.type_text("original content").unwrap();
    harness.render().unwrap();

    // Verify initial content
    harness.assert_buffer_content("original content");

    // Open command palette and run shell command (replace)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command (replace)").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap(); // Wait for shell command input prompt

    // Replace with uppercase
    harness.type_text("tr a-z A-Z").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Verify replaced content
    harness.assert_buffer_content("ORIGINAL CONTENT");

    // Undo should restore original content (atomic undo via Event::Batch)
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("original content");
}

/// Test shell command with cat (identity transform)
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_cat_identity() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "line 1\nline 2\nline 3\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Open command palette and run shell command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap(); // Wait for shell command input prompt

    // cat should preserve content exactly
    harness.type_text("cat").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // New buffer should have same content
    harness.assert_buffer_content("line 1\nline 2\nline 3\n");
}

/// Test shell command with wc (word count)
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_wc() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("words.txt");
    std::fs::write(&file_path, "one two three\nfour five\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Open command palette and run shell command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap(); // Wait for shell command input prompt

    // Count words
    harness.type_text("wc -w").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Should show 5 words
    harness.assert_screen_contains("5");
}

/// Test that cursor position is preserved after shell command replace
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_replace_preserves_cursor_position() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\nbaz qux\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move cursor to middle of second line (position 18: after "hello world\nfoo ")
    // Line 1: "hello world\n" = 12 bytes
    // Line 2: "foo " = 4 bytes, so position 16
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Get cursor position before replacement
    let cursor_pos_before = harness.editor().active_state().cursors.primary().position;

    // Run shell command to uppercase everything (replace mode)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command (replace)").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap(); // Wait for shell command input prompt

    harness.type_text("tr a-z A-Z").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Verify content is uppercase
    harness.assert_buffer_content("HELLO WORLD\nFOO BAR\nBAZ QUX\n");

    // Verify cursor is at same position as before
    let cursor_pos_after = harness.editor().active_state().cursors.primary().position;
    assert_eq!(
        cursor_pos_before, cursor_pos_after,
        "Cursor position should be preserved after shell command replace"
    );
}

/// Test that cursor position is clamped when buffer gets shorter
#[test]
#[cfg_attr(not(unix), ignore = "Shell commands require Unix-like environment")]
fn test_shell_command_replace_clamps_cursor_when_buffer_shrinks() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("long.txt");
    std::fs::write(
        &file_path,
        "This is a very long line of text\nAnother line\nYet another line\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move cursor to middle of the buffer (line 2)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let cursor_pos_before = harness.editor().active_state().cursors.primary().position;
    assert!(
        cursor_pos_before > 20,
        "Cursor should be in the middle of the buffer"
    );

    // Run shell command that produces shorter output (replace mode)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("shell command (replace)").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for the shell command prompt specifically (not the command palette)
    harness
        .wait_for_screen_contains("Shell command (replace):")
        .unwrap();

    // Replace with just "short"
    harness.type_text("echo short").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for buffer to be updated with shell command output
    harness
        .wait_until(|h| h.get_buffer_content().as_deref() == Some("short\n"))
        .unwrap();

    // Verify content is replaced
    harness.assert_buffer_content("short\n");

    // Cursor should be clamped to new buffer length
    let cursor_pos_after = harness.editor().active_state().cursors.primary().position;
    let new_buffer_len = "short\n".len();
    assert!(
        cursor_pos_after <= new_buffer_len,
        "Cursor should be clamped to new buffer length"
    );
}
