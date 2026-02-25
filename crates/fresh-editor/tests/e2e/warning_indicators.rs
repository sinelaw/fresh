//! E2E tests for warning indicator functionality
//!
//! Tests the warning domain system including:
//! - ShowWarnings command
//! - ShowLspStatus command
//! - ClearWarnings command
//! - Status bar warning badge display

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::io::Write;

/// Test that ShowWarnings command appears in command palette
#[test]
fn test_show_warnings_command_exists() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type to filter for warnings command
    harness.type_text("show warn").unwrap();
    harness.render().unwrap();

    // Should show the Show Warnings command
    harness.assert_screen_contains("Show Warnings");
}

/// Test that ShowLspStatus command appears in command palette
#[test]
fn test_show_lsp_status_command_exists() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type to filter for LSP status command
    harness.type_text("lsp status").unwrap();
    harness.render().unwrap();

    // Should show the Show LSP Status command
    harness.assert_screen_contains("Show LSP Status");
}

/// Test that ClearWarnings command appears in command palette
#[test]
fn test_clear_warnings_command_exists() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type to filter for clear warnings command
    harness.type_text("clear warn").unwrap();
    harness.render().unwrap();

    // Should show the Clear Warnings command
    harness.assert_screen_contains("Clear Warnings");
}

/// Test ShowWarnings command execution when no warnings exist
#[test]
fn test_show_warnings_no_warnings() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Execute Show Warnings command
    harness.type_text("Show Warnings").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "No warnings" status message
    harness.assert_screen_contains("No warnings");
}

/// Test ShowLspStatus command execution when no LSP active
#[test]
fn test_show_lsp_status_no_lsp() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Execute Show LSP Status command
    harness.type_text("Show LSP Status").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "No LSP server active" status message
    harness.assert_screen_contains("No LSP server active");
}

/// Test that status log buffer stays read-only after revert
///
/// Reproduces the bug where opening a log file via the status bar sets
/// editing_disabled, but when the file is updated and reverted,
/// the flag was lost because revert_file() replaces the entire EditorState.
#[test]
fn test_status_log_stays_read_only_after_revert() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let log_path = project_dir.join("status.log");

    // Create initial log file
    {
        let mut f = std::fs::File::create(&log_path).unwrap();
        f.write_all(b"2025-01-01 00:00:00 Initial status\n")
            .unwrap();
        f.sync_all().unwrap();
    }

    // Set the status log path and open it
    harness.editor_mut().set_status_log_path(log_path.clone());
    harness.editor_mut().open_status_log();
    harness.render().unwrap();

    // Verify the buffer is read-only
    assert!(
        harness.editor().is_editing_disabled(),
        "Status log buffer should be read-only immediately after opening"
    );

    // Update the file on disk (simulating new status messages being appended)
    {
        let mut f = std::fs::File::create(&log_path).unwrap();
        f.write_all(b"2025-01-01 00:00:00 Initial status\n2025-01-01 00:00:01 New status\n")
            .unwrap();
        f.sync_all().unwrap();
    }

    // Trigger a revert (this is what auto-revert does when it detects the file changed)
    let reverted = harness.editor_mut().revert_file().unwrap();
    assert!(reverted, "revert_file should succeed");
    harness.render().unwrap();

    // Verify the buffer content was updated
    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("New status"),
        "Buffer should contain reverted content"
    );

    // The key assertion: editing_disabled must survive the revert
    assert!(
        harness.editor().is_editing_disabled(),
        "Status log buffer should remain read-only after revert"
    );
}

/// Test ClearWarnings command execution
#[test]
fn test_clear_warnings_command() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Execute Clear Warnings command
    harness.type_text("Clear Warnings").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "Warnings cleared" status message
    harness.assert_screen_contains("Warnings cleared");
}
