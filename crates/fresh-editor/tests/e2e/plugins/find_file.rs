//! E2E tests for the Quick Open feature (native Rust implementation)
//!
//! Tests the Quick Open functionality which provides:
//! - Platform-agnostic file finding (git -> fd -> find -> manual)
//! - Unified finder with prefix mode switching (>, #, :)
//! - Buffer finder
//! - Frecency-based ranking
//!
//! Note: Quick Open now defaults to command mode (starts with ">")

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

// ============================================================================
// Command Mode Tests (> prefix - default mode)
// ============================================================================

/// Test command mode: Quick Open starts in command mode with > prefix
#[test]
fn test_quick_open_starts_in_command_mode() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test content\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open (defaults to command mode with >)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show commands immediately (already has > prefix)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Commands should be visible
            s.contains("Save") || s.contains("Open") || s.contains("Close") || s.contains("Quit")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test command mode: type command -> press Enter -> command executes
#[test]
fn test_quick_open_command_execute() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test content\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open (already in command mode)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type command to filter - look for "Go to Line" command
    harness.type_text("go to line").unwrap();

    // Should show filtered command
    harness
        .wait_until(|h| h.screen_to_string().contains("Go to Line"))
        .unwrap();

    // Press Enter to execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The "Go to Line" command should open the go-to-line prompt
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Line:") || s.contains("Go to line")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test command mode: filter commands by typing
#[test]
fn test_quick_open_command_filter() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to filter for "save"
    harness.type_text("save").unwrap();

    // Should show Save command
    harness
        .wait_until(|h| h.screen_to_string().contains("Save"))
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

// ============================================================================
// Go-to-Line Tests (: prefix)
// ============================================================================

/// Test go-to-line: type :N -> press Enter -> cursor moves to line N
#[test]
fn test_quick_open_goto_line_execute() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create file with many lines
    let content = (1..=20)
        .map(|i| format!("Line number {}\n", i))
        .collect::<String>();
    let test_file = project_root.join("multiline.txt");
    fs::write(&test_file, &content).unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Verify we start at line 1
    harness.assert_screen_contains("Ln 1");

    // Open Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Clear > and type :15 for go-to-line mode
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":15").unwrap();

    // Should show go-to-line suggestion
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Go to line 15") || s.contains("line 15")
        })
        .unwrap();

    // Press Enter to jump
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Verify cursor is now at line 15
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 15"))
        .unwrap();

    // Also verify the content of line 15 is visible
    harness.assert_screen_contains("Line number 15");
}

/// Test go-to-line with invalid input shows hint
#[test]
fn test_quick_open_goto_line_invalid() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Line 1\nLine 2\nLine 3\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open and try invalid line number
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":abc").unwrap();

    // Should show invalid line hint
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Invalid") || s.contains("Enter a line number")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test go-to-line with just colon shows hint
#[test]
fn test_quick_open_goto_line_hint() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open and switch to go-to-line mode
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":").unwrap();

    // Should show go-to-line hint
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("line") || s.contains("Line")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

// ============================================================================
// Buffer Finder Tests (# prefix)
// ============================================================================

/// Test buffer finder shows open buffers
#[test]
fn test_quick_open_buffer_list() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create and open multiple files
    let file1 = project_root.join("alpha.txt");
    let file2 = project_root.join("beta.txt");
    fs::write(&file1, "Alpha content\n").unwrap();
    fs::write(&file2, "Beta content\n").unwrap();

    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Open Quick Open and switch to buffer mode
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#").unwrap();

    // Should show both buffers
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("alpha") || s.contains("beta")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test buffer finder: select buffer switches to it
#[test]
fn test_quick_open_buffer_switch() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create files with distinctive content
    let file1 = project_root.join("first.txt");
    let file2 = project_root.join("second.txt");
    fs::write(&file1, "FIRST_FILE_CONTENT\n").unwrap();
    fs::write(&file2, "SECOND_FILE_CONTENT\n").unwrap();

    // Open both files (we'll end up on second)
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Verify we're on second file
    harness.assert_screen_contains("SECOND_FILE_CONTENT");

    // Open Quick Open, switch to buffer mode, find first file
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#first").unwrap();

    // Wait for buffer to appear in list
    harness
        .wait_until(|h| h.screen_to_string().contains("first"))
        .unwrap();

    // Select it
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should now show first file content
    harness
        .wait_until(|h| h.screen_to_string().contains("FIRST_FILE_CONTENT"))
        .unwrap();
}

// ============================================================================
// File Finder Tests (empty prefix)
// ============================================================================

// Note: File finder tests that rely on file discovery are unreliable in temp
// directories because git/fd/find may not find files quickly enough.
// The core file finder functionality is tested via buffer switching which
// uses the same code paths but with already-known buffer data.

// ============================================================================
// Mode Switching Tests
// ============================================================================

/// Test switching between modes by changing prefix
#[test]
fn test_quick_open_mode_switching() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open (starts in command mode with >)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify command mode (shows commands)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Save") || s.contains("Open")
        })
        .unwrap();

    // Switch to go-to-line mode: delete >, type :
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":").unwrap();

    // Should show go-to-line hint
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("line") || s.contains("Line")
        })
        .unwrap();

    // Switch to buffer mode: delete :, type #
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#").unwrap();

    // Should show buffer (test.txt is open)
    harness
        .wait_until(|h| h.screen_to_string().contains("test"))
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test Escape cancels Quick Open
#[test]
fn test_quick_open_cancel() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test content\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify prompt is visible
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Save") || s.contains("Open")
        })
        .unwrap();

    // Press Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should be back to normal editing
    harness.assert_screen_contains("Test content");
}

/// Test Ctrl+P again closes Quick Open (toggle behavior)
#[test]
fn test_quick_open_toggle() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test content\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify it's open (shows the hints line with >command)
    harness.assert_screen_contains(">command");

    // Press Ctrl+P again to close
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be closed, back to file content (hints should be gone)
    harness.assert_screen_contains("Test content");
    assert!(
        !harness.screen_to_string().contains(">command"),
        "Quick Open should be closed (hints gone)"
    );
}

/// Test buffer switch with # prefix and autocomplete by buffer name
#[test]
fn test_quick_open_buffer_autocomplete() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create and open two files with distinct names
    let file1 = project_root.join("alpha_file.txt");
    let file2 = project_root.join("beta_file.txt");
    fs::write(&file1, "ALPHA_CONTENT\n").unwrap();
    fs::write(&file2, "BETA_CONTENT\n").unwrap();

    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Verify we're on second file (beta)
    harness.assert_screen_contains("BETA_CONTENT");

    // Open Quick Open, clear the > prefix, type # to enter buffer mode with partial name
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#alp").unwrap();
    harness.render().unwrap();

    // Should show alpha_file in suggestions (matching by name, not index)
    harness.assert_screen_contains("alpha_file");

    // Press Enter to confirm selection (first match)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show alpha file content
    harness.assert_screen_contains("ALPHA_CONTENT");
}
