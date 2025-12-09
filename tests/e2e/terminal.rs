//! End-to-end tests for terminal integration
//!
//! Tests the built-in terminal emulator functionality including:
//! - Opening/closing terminals
//! - Terminal buffer creation
//! - Terminal mode switching
//! - ANSI escape sequence handling (cursor, colors, attributes)

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::terminal::TerminalState;

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

    // Exit terminal mode via Ctrl+]
    harness.editor_mut().handle_terminal_key(
        KeyCode::Char(']'),
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

/// Test Ctrl+] exits terminal mode
#[test]
fn test_ctrl_bracket_exits_terminal() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Should be in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Send Ctrl+] to exit terminal mode
    // Note: Ctrl+\ sends SIGQUIT on Unix, so we use Ctrl+] instead
    let handled = harness.editor_mut().handle_terminal_key(
        KeyCode::Char(']'),
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

/// Test terminal content rendering via get_terminal_content
#[test]
fn test_terminal_content_rendering() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Get terminal content for the buffer
    let buffer_id = harness.editor().active_buffer_id();
    let content = harness.editor().get_terminal_content(buffer_id);

    // Content should be available
    assert!(content.is_some());

    // Content should have rows
    let content = content.unwrap();
    assert!(!content.is_empty());

    // Each row should have cells
    assert!(!content[0].is_empty());
}

/// Test terminal handles ANSI escape sequences for cursor positioning
/// Uses direct terminal state processing (synchronous) instead of PTY
#[test]
fn test_terminal_ansi_cursor_positioning() {
    // Create a terminal state directly (bypassing PTY for synchronous testing)
    let mut state = TerminalState::new(80, 24);

    // Get initial cursor position
    let initial_pos = state.cursor_position();
    assert_eq!(initial_pos, (0, 0), "Initial cursor should be at origin");

    // Process ANSI escape sequence to move cursor to row 5, col 10
    // ESC [ 5 ; 10 H (1-indexed in ANSI, 0-indexed internally)
    state.process_output(b"\x1b[5;10H");

    // Check cursor moved (ANSI coordinates are 1-based, internal are 0-based)
    let new_pos = state.cursor_position();
    assert_eq!(new_pos.0, 9, "Cursor column should be 9 (10-1 for 0-indexing)");
    assert_eq!(new_pos.1, 4, "Cursor row should be 4 (5-1 for 0-indexing)");
}

/// Test terminal handles ANSI color codes
/// Uses direct terminal state processing (synchronous) instead of PTY
#[test]
fn test_terminal_ansi_colors() {
    // Create a terminal state directly (bypassing PTY for synchronous testing)
    let mut state = TerminalState::new(80, 24);

    // Process text with red color escape sequence
    // ESC[31m = set foreground red
    state.process_output(b"\x1b[31mRED TEXT\x1b[0m");

    // Get the first row which should contain the colored text
    let row = state.get_line(0);

    // Find the 'R' cell and verify it has red foreground
    let r_cell = &row[0];
    assert_eq!(r_cell.c, 'R');
    assert!(r_cell.fg.is_some(), "Cell should have foreground color");

    // Red color should be roughly (205, 49, 49) based on the ANSI palette
    let (r, g, b) = r_cell.fg.unwrap();
    assert!(r > 150, "Red component should be high");
    assert!(g < 100, "Green component should be low");
    assert!(b < 100, "Blue component should be low");
}

/// Test terminal mode key forwarding via handle_key
#[test]
fn test_terminal_key_forwarding() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Verify in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Send regular key through handle_key (should be forwarded to terminal)
    harness.editor_mut().handle_key(KeyCode::Char('x'), KeyModifiers::NONE).unwrap();

    // Should still be in terminal mode (key was forwarded, not processed)
    assert!(harness.editor().is_terminal_mode());
}

/// Test Ctrl+] via handle_key exits terminal mode
#[test]
fn test_ctrl_bracket_via_handle_key() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Verify in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Send Ctrl+] through handle_key (should exit terminal mode)
    // Note: Ctrl+\ sends SIGQUIT on Unix, so we use Ctrl+] instead
    harness.editor_mut().handle_key(KeyCode::Char(']'), KeyModifiers::CONTROL).unwrap();

    // Should have exited terminal mode
    assert!(!harness.editor().is_terminal_mode());
}

/// Test terminal state is initialized correctly after opening
#[test]
fn test_terminal_state_initialization() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Get terminal state
    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    let handle = harness.editor().terminal_manager().get(terminal_id).unwrap();

    // Terminal should be alive
    assert!(handle.is_alive());

    // Terminal state should be accessible
    let state = handle.state.lock().unwrap();

    // Cursor should be at a valid position
    let (col, row) = state.cursor_position();
    let (cols, rows) = state.size();
    assert!(col < cols);
    assert!(row < rows);

    // Cursor should be visible
    assert!(state.cursor_visible());
}

/// Test terminal bold text attribute
/// Uses direct terminal state processing (synchronous) instead of PTY
#[test]
fn test_terminal_bold_attribute() {
    // Create a terminal state directly (bypassing PTY for synchronous testing)
    let mut state = TerminalState::new(80, 24);

    // Process text with bold escape sequence
    // ESC[1m = set bold, ESC[0m = reset
    state.process_output(b"\x1b[1mBOLD\x1b[0m");

    // Get the first row which should contain the bold text
    let row = state.get_line(0);

    // Find the 'B' cell and verify it has bold attribute
    let b_cell = &row[0];
    assert_eq!(b_cell.c, 'B');
    assert!(b_cell.bold, "Cell should have bold attribute");

    // The 'O', 'L', 'D' cells should also be bold
    assert!(row[1].bold, "O should be bold");
    assert!(row[2].bold, "L should be bold");
    assert!(row[3].bold, "D should be bold");
}

/// Test terminal resize functionality
#[test]
fn test_terminal_resize() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();

    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();

    // Get initial size
    let handle = harness.editor().terminal_manager().get(terminal_id).unwrap();
    let (initial_cols, initial_rows) = handle.size();

    // Resize the terminal
    harness.editor_mut().resize_terminal(buffer_id, 120, 40);

    // Get new size
    let handle = harness.editor().terminal_manager().get(terminal_id).unwrap();
    let (new_cols, new_rows) = handle.size();

    // Size should have changed
    assert_eq!(new_cols, 120);
    assert_eq!(new_rows, 40);
    assert!(new_cols != initial_cols || new_rows != initial_rows);
}

/// Test that buffer content is synced when exiting terminal mode
#[test]
fn test_terminal_buffer_sync_on_exit() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();
    let buffer_id = harness.editor().active_buffer_id();

    // Write some content to the terminal
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    if let Some(handle) = harness.editor().terminal_manager().get(terminal_id) {
        if let Ok(mut state) = handle.state.lock() {
            state.process_output(b"Hello Terminal World\r\n");
            state.process_output(b"Line 2\r\n");
            state.process_output(b"Line 3\r\n");
        }
    }

    // Exit terminal mode
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();

    // Buffer should now have the synced content
    let buffer_content = harness.editor().get_buffer_content(buffer_id);
    assert!(
        buffer_content.is_some(),
        "Buffer should have content after sync"
    );

    let content = buffer_content.unwrap();
    assert!(
        content.contains("Hello") || content.contains("Terminal"),
        "Buffer should contain terminal output"
    );
}

/// Test cursor movement in terminal buffer when mode is disabled
#[test]
fn test_terminal_buffer_cursor_movement() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();
    let buffer_id = harness.editor().active_buffer_id();

    // Write some content to the terminal
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    if let Some(handle) = harness.editor().terminal_manager().get(terminal_id) {
        if let Ok(mut state) = handle.state.lock() {
            state.process_output(b"Line 1\r\n");
            state.process_output(b"Line 2\r\n");
            state.process_output(b"Line 3\r\n");
        }
    }

    // Exit terminal mode
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();

    assert!(!harness.editor().is_terminal_mode());

    // Get initial cursor position
    let initial_pos = harness.editor().get_cursor_position(buffer_id);

    // Move cursor up
    harness
        .editor_mut()
        .handle_key(KeyCode::Up, KeyModifiers::NONE)
        .unwrap();

    let pos_after_up = harness.editor().get_cursor_position(buffer_id);

    // Cursor should have moved (different position)
    assert_ne!(
        initial_pos, pos_after_up,
        "Cursor should move when pressing Up in disabled terminal mode"
    );
}

/// Test toggle back into terminal mode with same keybinding
#[test]
fn test_terminal_mode_toggle_back() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();
    assert!(harness.editor().is_terminal_mode());

    // Exit terminal mode
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Toggle back into terminal mode with same key
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "Should toggle back into terminal mode"
    );
}

/// Test Ctrl+Space toggles terminal mode both ways
#[test]
fn test_ctrl_space_toggle() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open a terminal
    harness.editor_mut().open_terminal();
    assert!(harness.editor().is_terminal_mode());

    // Exit with Ctrl+Space
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Re-enter with Ctrl+Space
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "Ctrl+Space should toggle back into terminal mode"
    );
}
