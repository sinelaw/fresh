//! E2E tests for warning indicator functionality
//!
//! Tests the warning domain system including:
//! - ShowWarnings command
//! - ShowLspStatus command
//! - ClearWarnings command
//! - Status bar warning badge display

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that ShowWarnings command appears in command palette
#[test]
fn test_show_warnings_command_exists() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

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
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

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
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

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
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

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
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

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

/// Test ClearWarnings command execution
#[test]
fn test_clear_warnings_command() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

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
