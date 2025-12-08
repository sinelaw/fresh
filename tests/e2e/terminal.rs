//! End-to-end tests for terminal integration
//!
//! Tests the built-in terminal emulator functionality including:
//! - Opening/closing terminals
//! - Terminal buffer creation
//! - Terminal mode switching

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test opening a terminal creates a buffer and switches to it
#[test]
fn test_open_terminal() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Initially should have just the default buffer
    harness.render().unwrap();
    harness.assert_screen_contains("[No Name]");

    // Open a terminal using the direct method
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Should now show terminal tab
    harness.assert_screen_contains("*Terminal 0*");

    // Status bar should show terminal opened message
    harness.assert_screen_contains("Terminal");
}

/// Test closing a terminal
#[test]
fn test_close_terminal() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    // Close the terminal
    harness.editor_mut().close_terminal();
    harness.render().unwrap();

    // Terminal tab should be gone
    harness.assert_screen_not_contains("*Terminal 0*");

    // Status should indicate terminal closed
    harness.assert_screen_contains("closed");
}

/// Test terminal mode switching
#[test]
fn test_terminal_mode_toggle() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal (should enter terminal mode automatically)
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Should be in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Exit terminal mode via Ctrl+\
    harness.editor_mut().handle_terminal_key(
        KeyCode::Char('\\'),
        KeyModifiers::CONTROL,
    );
    harness.render().unwrap();

    // Should no longer be in terminal mode
    assert!(!harness.editor().is_terminal_mode());
    harness.assert_screen_contains("disabled");
}

/// Test multiple terminals can be opened
#[test]
fn test_multiple_terminals() {
    let mut harness = EditorTestHarness::new(120, 24).unwrap();

    // Open first terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    // Open second terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 1*");

    // Both tabs should be visible
    harness.assert_screen_contains("*Terminal 0*");
    harness.assert_screen_contains("*Terminal 1*");
}

/// Test terminal buffer is properly identified
#[test]
fn test_terminal_buffer_identification() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Get initial buffer ID
    let initial_buffer = harness.editor().active_buffer_id();

    // Initial buffer should not be a terminal
    assert!(!harness.editor().is_terminal_buffer(initial_buffer));

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Current buffer should now be a terminal
    let terminal_buffer = harness.editor().active_buffer_id();
    assert!(harness.editor().is_terminal_buffer(terminal_buffer));

    // Should have a valid terminal ID
    assert!(harness.editor().get_terminal_id(terminal_buffer).is_some());
}

/// Test closing terminal when not viewing one shows appropriate message
#[test]
fn test_close_terminal_not_viewing() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Try to close terminal when viewing regular buffer
    harness.editor_mut().close_terminal();
    harness.render().unwrap();

    // Should show "not viewing" message
    harness.assert_screen_contains("Not viewing");
}

/// Test Ctrl+\ exits terminal mode
#[test]
fn test_ctrl_backslash_exits_terminal() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Should be in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Send Ctrl+\ to exit terminal mode
    let handled = harness.editor_mut().handle_terminal_key(
        KeyCode::Char('\\'),
        KeyModifiers::CONTROL,
    );

    assert!(handled);
    assert!(!harness.editor().is_terminal_mode());
}

/// Test terminal dimensions are calculated correctly
#[test]
fn test_terminal_dimensions() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Get the terminal
    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();

    // Terminal manager should have this terminal
    let handle = harness.editor().terminal_manager().get(terminal_id);
    assert!(handle.is_some());

    let handle = handle.unwrap();
    let (cols, rows) = handle.size();

    // Dimensions should be reasonable (accounting for UI chrome)
    assert!(cols >= 40);
    assert!(rows >= 10);
}

/// Test terminal input is sent to PTY
#[test]
fn test_terminal_input() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Send some input
    harness.editor_mut().send_terminal_input(b"echo hello\n");

    // The input should have been sent (we can't easily verify the output
    // without async processing, but we verify no panic)
    assert!(harness.editor().is_terminal_mode());
}
