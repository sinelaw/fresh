//! End-to-end tests for terminal integration
//!
//! NOTE: These tests require a working PTY (/dev/ptmx). They will fail in
//! environments without PTY support (some containers/sandboxes). Run on a host
//! or CI with PTY support enabled. Tests will early-return (skip) if PTY cannot
//! be opened in the current environment.
//!
//! Tests the built-in terminal emulator functionality including:
//! - Opening/closing terminals
//! - Terminal buffer creation
//! - Terminal mode switching
//! - ANSI escape sequence handling (cursor, colors, attributes)

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::terminal::TerminalState;
use portable_pty::{native_pty_system, PtySize};

fn harness_or_skip(width: u16, height: u16) -> Option<EditorTestHarness> {
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return None;
    }

    EditorTestHarness::new(width, height).ok()
}

macro_rules! harness_or_return {
    ($w:expr, $h:expr) => {
        match harness_or_skip($w, $h) {
            Some(h) => h,
            None => return,
        }
    };
}

/// Test opening a terminal creates a buffer and switches to it
#[test]
fn test_open_terminal() {
    let mut harness = harness_or_return!(80, 24);

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
    let mut harness = harness_or_return!(80, 24);

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
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal (should enter terminal mode automatically)
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Should be in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Exit terminal mode via Ctrl+]
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(']'), KeyModifiers::CONTROL);
    harness.render().unwrap();

    // Should no longer be in terminal mode
    assert!(!harness.editor().is_terminal_mode());
    harness.assert_screen_contains("disabled");
}

/// Test multiple terminals can be opened
#[test]
fn test_multiple_terminals() {
    let mut harness = harness_or_return!(120, 24);

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
    let mut harness = harness_or_return!(80, 24);

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
    let mut harness = harness_or_return!(80, 24);

    // Try to close terminal when viewing regular buffer
    harness.editor_mut().close_terminal();
    harness.render().unwrap();

    // Should show "not viewing" message
    harness.assert_screen_contains("Not viewing");
}

/// Test Ctrl+] exits terminal mode
#[test]
fn test_ctrl_bracket_exits_terminal() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Should be in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Send Ctrl+] to exit terminal mode
    // Note: Ctrl+\ sends SIGQUIT on Unix, so we use Ctrl+] instead
    let handled = harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(']'), KeyModifiers::CONTROL);

    assert!(handled);
    assert!(!harness.editor().is_terminal_mode());
}

/// Test terminal dimensions are calculated correctly
#[test]
fn test_terminal_dimensions() {
    let mut harness = harness_or_return!(100, 30);

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Get the terminal
    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();

    // Terminal manager should have this terminal
    let handle = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle should exist");
    let (cols, rows) = handle.size();

    // Dimensions should be reasonable (accounting for UI chrome)
    assert!(cols >= 40);
    assert!(rows >= 10);
}

/// Test terminal input is sent to PTY
#[test]
fn test_terminal_input() {
    let mut harness = harness_or_return!(80, 24);

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
    let mut harness = harness_or_return!(80, 24);

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
    assert_eq!(
        new_pos.0, 9,
        "Cursor column should be 9 (10-1 for 0-indexing)"
    );
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
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Verify in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Send regular key through handle_key (should be forwarded to terminal)
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('x'), KeyModifiers::NONE)
        .unwrap();

    // Should still be in terminal mode (key was forwarded, not processed)
    assert!(harness.editor().is_terminal_mode());
}

/// Test Ctrl+] via handle_key exits terminal mode
#[test]
fn test_ctrl_bracket_via_handle_key() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Verify in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Send Ctrl+] through handle_key (should exit terminal mode)
    // Note: Ctrl+\ sends SIGQUIT on Unix, so we use Ctrl+] instead
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();

    // Should have exited terminal mode
    assert!(!harness.editor().is_terminal_mode());
}

/// Test terminal state is initialized correctly after opening
#[test]
fn test_terminal_state_initialization() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Get terminal state
    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    let handle = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle should exist");

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
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();

    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();

    // Get initial size
    let handle = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .unwrap();
    let (initial_cols, initial_rows) = handle.size();

    // Resize the terminal
    harness.editor_mut().resize_terminal(buffer_id, 120, 40);

    // Get new size
    let handle = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .unwrap();
    let (new_cols, new_rows) = handle.size();

    // Size should have changed
    assert_eq!(new_cols, 120);
    assert_eq!(new_rows, 40);
    assert!(new_cols != initial_cols || new_rows != initial_rows);
}

/// Test that buffer content is synced when exiting terminal mode
#[test]
#[cfg(not(windows))] // Uses Unix shell commands (echo with single quotes)
fn test_terminal_buffer_sync_on_exit() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    let buffer_id = harness.editor().active_buffer_id();

    // Send commands to the shell to generate output
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'SYNC_TEST_MARKER'\n");

    // Wait for the output to appear on screen
    harness
        .wait_until(|h| h.screen_to_string().contains("SYNC_TEST_MARKER"))
        .unwrap();

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
        content.contains("SYNC_TEST_MARKER"),
        "Buffer should contain terminal output, got: {}",
        &content[..content.len().min(200)]
    );
}

/// Test cursor movement in terminal buffer when mode is disabled
#[test]
fn test_terminal_buffer_cursor_movement() {
    let mut harness = harness_or_return!(80, 24);

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
    let mut harness = harness_or_return!(80, 24);

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

/// Test toggling back into terminal mode via 'q' when in read-only view
#[test]
fn test_terminal_mode_toggle_with_q() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal and exit to read-only mode
    harness.editor_mut().open_terminal();
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Press 'q' to return to terminal mode
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "Pressing 'q' in read-only terminal view should re-enter terminal mode"
    );
}

/// Test Ctrl+Space toggles terminal mode both ways
#[test]
fn test_ctrl_space_toggle() {
    let mut harness = harness_or_return!(80, 24);

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

// ============================================================================
// Bug reproduction tests - Known issues documented in docs/TERMINAL.md
// ============================================================================

/// BUG: "Read-only" mode should reject text input but doesn't
/// When terminal mode is disabled, the status bar says "read only" but
/// typing characters actually inserts them into the buffer.
#[test]
#[ignore] // Remove ignore when bug is fixed
fn test_bug_readonly_mode_rejects_input() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal and write some content
    harness.editor_mut().open_terminal();
    let buffer_id = harness.editor().active_buffer_id();

    // Write content to terminal
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    if let Some(handle) = harness.editor().terminal_manager().get(terminal_id) {
        if let Ok(mut state) = handle.state.lock() {
            state.process_output(b"Line 1\r\n");
            state.process_output(b"Line 2\r\n");
        }
    }

    // Exit terminal mode (enters "read-only" mode)
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Get buffer content before typing
    let content_before = harness.editor().get_buffer_content(buffer_id);

    // Try to type text - this SHOULD be rejected in read-only mode
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('x'), KeyModifiers::NONE)
        .unwrap();
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('z'), KeyModifiers::NONE)
        .unwrap();

    // Get buffer content after typing
    let content_after = harness.editor().get_buffer_content(buffer_id);

    // BUG: Content should be unchanged in read-only mode
    // Currently fails because text is being inserted
    assert_eq!(
        content_before, content_after,
        "Buffer content should not change in read-only terminal mode"
    );
}

/// BUG: Keybindings don't work in "read-only" terminal buffer mode
/// When terminal mode is disabled, pressing keys like 'g' twice (gg) should
/// navigate to the top, but instead the characters are typed into the buffer.
#[test]
#[ignore] // Remove ignore when bug is fixed
fn test_bug_keybindings_work_in_readonly_mode() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal and write multiple lines of content
    harness.editor_mut().open_terminal();
    let buffer_id = harness.editor().active_buffer_id();

    // Write content to terminal
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    if let Some(handle) = harness.editor().terminal_manager().get(terminal_id) {
        if let Ok(mut state) = handle.state.lock() {
            for i in 1..=20 {
                state.process_output(format!("Line {}\r\n", i).as_bytes());
            }
        }
    }

    // Exit terminal mode
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Get buffer content before attempting navigation
    let content_before = harness.editor().get_buffer_content(buffer_id);

    // Try to use 'gg' navigation (go to top of file)
    // This should be a navigation command, not text insertion
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('g'), KeyModifiers::NONE)
        .unwrap();
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('g'), KeyModifiers::NONE)
        .unwrap();

    // Get buffer content after
    let content_after = harness.editor().get_buffer_content(buffer_id);

    // BUG: Content should be unchanged - 'gg' is navigation not text
    // Currently fails because 'gg' is typed into the buffer
    assert_eq!(
        content_before, content_after,
        "Pressing 'gg' should navigate, not insert text"
    );
}

/// BUG: View doesn't scroll to cursor when resuming terminal mode from scrollback
/// After scrolling up in scrollback mode and resuming terminal mode, the view
/// should auto-scroll to show the cursor position (shell prompt).
///
/// NOTE: This test passes in the e2e harness but the bug was observed in real
/// tmux testing. The harness may not fully replicate the real UI render path.
/// Manual testing showed the view stays stuck at the scrolled position.
#[test]
#[cfg(not(windows))] // Uses Unix shell commands (seq, for loop, echo)
fn test_bug_view_scrolls_to_cursor_on_resume() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Generate lots of output via shell command (more than visible area)
    // Use printf to generate numbered lines
    harness
        .editor_mut()
        .send_terminal_input(b"for i in $(seq 1 100); do echo \"Line $i\"; done\n");

    // Wait for the last line to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Line 100"))
        .unwrap();

    // Add a unique marker at the prompt that we can search for
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'PROMPT_MARKER_XYZ'\n");

    // Wait for the marker to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("PROMPT_MARKER_XYZ"))
        .unwrap();

    // In terminal mode, we should see the prompt marker (bottom of terminal)
    harness.assert_screen_contains("PROMPT_MARKER_XYZ");

    // Exit terminal mode to enter scrollback
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Scroll up significantly (simulating user looking at history)
    for _ in 0..10 {
        harness
            .editor_mut()
            .handle_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // After scrolling up, the prompt marker should NOT be visible
    // (we're looking at earlier content like "Line 1", "Line 2", etc.)
    harness.assert_screen_not_contains("PROMPT_MARKER_XYZ");
    // But early lines should be visible
    harness.assert_screen_contains("Line 1");

    // Re-enter terminal mode
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(harness.editor().is_terminal_mode());
    harness.render().unwrap();

    // BUG: After resuming terminal mode, the prompt marker should be visible again
    // because the view should auto-scroll to show the cursor position.
    // Currently fails because view stays at the scrolled-up position.
    harness.assert_screen_contains("PROMPT_MARKER_XYZ");
}

/// Test that rendering doesn't panic when cursor is at the last row
/// Regression test for: panic "index outside of buffer: the area is Rect { x: 0, y: 0, width: 242, height: 60 } but index is (105, 60)"
///
/// The panic happens when:
/// 1. Terminal has 60 rows
/// 2. Content fills all rows with cursor at the end
/// 3. The cursor position reported by alacritty is y=60 (one past the last valid index 59)
#[test]
fn test_cursor_at_last_row_no_panic() {
    let mut harness = harness_or_return!(242, 64); // Width 242, extra height for status bar etc.

    // Open a terminal
    harness.editor_mut().open_terminal();
    let buffer_id = harness.editor().active_buffer_id();

    // Get the terminal and fill the screen to force cursor to the last row
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    if let Some(handle) = harness.editor().terminal_manager().get(terminal_id) {
        if let Ok(mut state) = handle.state.lock() {
            // Get the actual terminal size
            let (cols, rows) = state.size();
            eprintln!("Terminal size: {}x{}", cols, rows);

            // Fill every row to push cursor to the bottom
            for i in 0..rows {
                let line = format!("Line {:04}\r\n", i);
                state.process_output(line.as_bytes());
            }

            // Cursor position after filling should be at row=rows (past the last row)
            // or at row=rows-1. Either way, rendering should not panic.
            let (col, row) = state.cursor_position();
            eprintln!("Cursor position after fill: ({}, {})", col, row);
            eprintln!(
                "Terminal rows: {}, Cursor row == rows: {}",
                rows,
                row == rows
            );
        }
    }

    // This render should NOT panic even if cursor is at y=rows
    let result = harness.render();
    assert!(result.is_ok(), "Rendering should not panic");
}

/// Test that terminal rendering is robust when cursor position equals height
/// This simulates the exact conditions from the panic report
#[test]
fn test_terminal_cursor_boundary_condition() {
    let mut harness = harness_or_return!(242, 64);

    harness.editor_mut().open_terminal();
    let buffer_id = harness.editor().active_buffer_id();

    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    if let Some(handle) = harness.editor().terminal_manager().get(terminal_id) {
        if let Ok(mut state) = handle.state.lock() {
            let (_, rows) = state.size();

            // Use cursor movement escape codes to position cursor at the last row
            // ESC[H = move to home, ESC[<row>;<col>H = move to position
            // Move cursor to the last row
            let move_to_bottom = format!("\x1b[{};1H", rows);
            state.process_output(move_to_bottom.as_bytes());

            // Now write text that might push cursor past the bottom
            state.process_output(b"Text at bottom line\r\n");

            let (col, row) = state.cursor_position();
            eprintln!("After bottom line + newline: cursor at ({}, {})", col, row);
        }
    }

    // Should not panic
    harness.render().expect("render should not panic");
}

/// Test that terminal rendering handles resize correctly when cursor is at bottom
/// Regression test for: panic "index outside of buffer: the area is Rect { x: 0, y: 0, width: 242, height: 60 } but index is (105, 60)"
///
/// The bug could occur when:
/// 1. Terminal is larger (e.g., 70 rows)
/// 2. Cursor is at row 60
/// 3. Terminal is resized to 60 rows
/// 4. Cursor position isn't updated to be within new bounds
#[test]
fn test_terminal_resize_cursor_out_of_bounds() {
    let mut harness = harness_or_return!(242, 74); // Start larger

    harness.editor_mut().open_terminal();
    let buffer_id = harness.editor().active_buffer_id();

    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();

    // First, position cursor at row 60 in a 70-row terminal
    if let Some(handle) = harness.editor().terminal_manager().get(terminal_id) {
        if let Ok(mut state) = handle.state.lock() {
            let (cols, rows) = state.size();
            eprintln!("Initial terminal size: {}x{}", cols, rows);

            // Move cursor to row 61 (1-indexed, so row 60 in 0-indexed)
            state.process_output(b"\x1b[61;106H"); // Move to row 61, column 106

            let (col, row) = state.cursor_position();
            eprintln!("Cursor after move: ({}, {})", col, row);

            // Now resize terminal to smaller size (60 rows)
            state.resize(cols, 60);

            let (new_cols, new_rows) = state.size();
            eprintln!("After resize: {}x{}", new_cols, new_rows);

            let (col, row) = state.cursor_position();
            eprintln!(
                "Cursor after resize: ({}, {}), new_rows: {}",
                col, row, new_rows
            );

            // Check if cursor is out of bounds
            if row >= new_rows {
                eprintln!("BUG: Cursor row {} >= terminal rows {}", row, new_rows);
            }
        }
    }

    // This should not panic even if cursor is out of bounds
    let result = harness.render();
    assert!(result.is_ok(), "Rendering should not panic after resize");
}

// ============================================================================
// Session restoration tests
// ============================================================================

/// BUG: When session is saved with terminal as active tab, restoration shows
/// the terminal as selected but input goes to a different buffer.
///
/// Root cause: There are TWO sources of truth for "active buffer":
/// 1. split_manager's split tree (SplitNode::Leaf { buffer_id }) - used for RENDERING
/// 2. self.active_buffer field - used for INPUT HANDLING
///
/// During session restore:
/// - restore_split_view_state() correctly updates the split tree via set_split_buffer()
/// - BUT apply_session() then sets self.active_buffer from open_files[active_file_index]
/// - Terminals are NOT in open_files (only in open_tabs), so it falls back to first file
///
/// The fix should use active_tab_index with open_tabs (not active_file_index with open_files)
/// in apply_session() lines 391-405, just like restore_split_view_state() does.
#[test]
fn test_session_restore_terminal_active_buffer() {
    use fresh::config::Config;
    use portable_pty::{native_pty_system, PtySize};
    use tempfile::TempDir;

    // Skip if PTY not available
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping terminal session test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create a test file
    let file1 = project_dir.join("test.txt");
    std::fs::write(&file1, "File content here").unwrap();

    // First session: open file, open terminal, terminal should be active
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Open the file first
        harness.open_file(&file1).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("test.txt");

        // Now open a terminal - this should make terminal the active buffer
        harness.editor_mut().open_terminal();
        harness.render().unwrap();
        harness.assert_screen_contains("*Terminal 0*");

        // Verify terminal is active
        let active_buffer_before = harness.editor().active_buffer_id();
        assert!(
            harness.editor().is_terminal_buffer(active_buffer_before),
            "Terminal should be active buffer before save"
        );

        // Save session
        harness.editor_mut().save_session().unwrap();

        // Verify the session was captured with terminal as active
        let session = harness.editor().capture_session();
        let split_state = session.split_states.values().next().unwrap();
        eprintln!("Session open_tabs: {:?}", split_state.open_tabs);
        eprintln!(
            "Session active_tab_index: {:?}",
            split_state.active_tab_index
        );
        eprintln!("Session open_files: {:?}", split_state.open_files);
        eprintln!(
            "Session active_file_index: {}",
            split_state.active_file_index
        );

        // active_tab_index should point to the terminal
        assert_eq!(
            split_state.active_tab_index,
            Some(1),
            "active_tab_index should point to terminal (index 1)"
        );
    }

    // Second session: restore and verify terminal is still active
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should have been restored");
        harness.render().unwrap();

        // Check what buffer is active according to Editor's active_buffer field (for INPUT)
        let active_buffer_for_input = harness.editor().active_buffer_id();
        let input_is_terminal = harness.editor().is_terminal_buffer(active_buffer_for_input);

        eprintln!(
            "After restore: active_buffer (for input) = {:?}, is_terminal = {}",
            active_buffer_for_input, input_is_terminal
        );

        // Screen should show terminal as the visually active tab (rendering uses split tree)
        // The asterisks around "Terminal 0" indicate it's the selected tab
        harness.assert_screen_contains("*Terminal 0*");

        // BUG: The terminal tab is shown as selected (rendering is correct),
        // but active_buffer points to a file buffer (input target is wrong)!
        //
        // This means:
        // - User sees the terminal tab highlighted as "active"
        // - But any keystrokes go to the file buffer instead
        assert!(
            input_is_terminal,
            "BUG: active_buffer should be terminal but is file buffer {:?}. \
             The terminal appears selected but input goes elsewhere!",
            active_buffer_for_input
        );
    }
}

/// Test keyboard capture mode toggle with Ctrl+`
/// When keyboard capture is OFF (default), UI bindings work in terminal mode.
/// When keyboard capture is ON, all keys go to terminal.
#[test]
fn test_keyboard_capture_toggle() {
    use tracing_subscriber::EnvFilter;
    let _ = tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::from_default_env().add_directive(tracing::Level::TRACE.into()),
        )
        .with_test_writer()
        .try_init();

    let mut harness = harness_or_return!(120, 30);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // By default keyboard capture should be OFF
    assert!(
        !harness.editor().is_keyboard_capture(),
        "Keyboard capture should be OFF by default"
    );

    // Ctrl+P should open command palette when keyboard capture is OFF
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command:");
    // Close the command palette
    harness
        .send_key(KeyCode::Esc, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Toggle keyboard capture ON with F9
    tracing::info!("=== Toggling keyboard capture ON ===");
    harness
        .send_key(KeyCode::F(9), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.editor().is_keyboard_capture(),
        "Keyboard capture should be ON after F9"
    );
    harness.assert_screen_contains("Keyboard capture ON");

    // Ctrl+P should NOT open command palette when keyboard capture is ON
    // (key should go to terminal instead)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(
        !harness.screen_to_string().contains("Command:"),
        "Command palette should NOT open when keyboard capture is ON"
    );

    // Toggle keyboard capture OFF with F9
    tracing::info!("=== Toggling keyboard capture OFF ===");
    harness
        .send_key(KeyCode::F(9), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        !harness.editor().is_keyboard_capture(),
        "Keyboard capture should be OFF after second F9"
    );
    harness.assert_screen_contains("Keyboard capture OFF");

    // Ctrl+P should open command palette again now that keyboard capture is OFF
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Command:");
}

/// Test that UI bindings (like next_split with Alt+]) work in terminal mode
/// when keyboard capture is OFF.
#[test]
fn test_ui_bindings_work_in_terminal_mode() {
    let mut harness = harness_or_return!(120, 30);

    // Create a vertical split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open a terminal in the current (right) split
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    assert!(harness.editor().is_terminal_mode());
    assert!(
        !harness.editor().is_keyboard_capture(),
        "Keyboard capture should be OFF"
    );

    let terminal_buffer = harness.editor().active_buffer_id();
    assert!(harness.editor().is_terminal_buffer(terminal_buffer));

    // Use Alt+[ to switch to previous split (this should work in terminal mode
    // because it's a UI binding and keyboard capture is OFF)
    harness
        .send_key(KeyCode::Char('['), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Should have switched to the left split (non-terminal buffer)
    let new_buffer = harness.editor().active_buffer_id();
    assert!(
        !harness.editor().is_terminal_buffer(new_buffer),
        "Should have switched to non-terminal buffer via Alt+["
    );

    // Terminal mode should be OFF now (since we switched splits)
    assert!(
        !harness.editor().is_terminal_mode(),
        "Terminal mode should be OFF after switching splits"
    );
}

/// Test that UI bindings DON'T work when keyboard capture is ON
#[test]
fn test_ui_bindings_blocked_with_keyboard_capture() {
    let mut harness = harness_or_return!(120, 30);

    // Create a vertical split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open a terminal in the current split
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    assert!(harness.editor().is_terminal_mode());

    let terminal_buffer = harness.editor().active_buffer_id();

    // Turn keyboard capture ON with F9
    harness
        .send_key(KeyCode::F(9), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(harness.editor().is_keyboard_capture());

    // Now Alt+[ should NOT switch splits - it should go to terminal
    harness
        .send_key(KeyCode::Char('['), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Should still be in terminal mode with same buffer
    assert!(
        harness.editor().is_terminal_mode(),
        "Should still be in terminal mode (keyboard capture ON)"
    );
    assert_eq!(
        harness.editor().active_buffer_id(),
        terminal_buffer,
        "Should still have same terminal buffer (Alt+[ went to terminal, not processed as UI binding)"
    );
}

/// Test that command palette (Ctrl+P) works in terminal mode
/// This is a UI binding that should always work
#[test]
fn test_command_palette_works_in_terminal_mode() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Ctrl+P should open command palette
    // This tests the UI binding resolution in terminal mode
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The command palette should be open now
    // The prompt shows "Command:"
    harness.assert_screen_contains("Command:");
}

/// Test that typing in prompts works correctly when terminal buffer is active.
/// Regression test for: Letters typed in command palette were being sent to terminal
/// instead of the prompt input.
#[test]
fn test_prompt_typing_works_in_terminal_mode() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify command palette is open
    harness.assert_screen_contains("Command:");

    // Type something in the prompt - this should go to the prompt, not the terminal
    harness.type_text("quit").unwrap();
    harness.render().unwrap();

    // The prompt should show what we typed
    harness.assert_screen_contains("quit");
}

/// Test that switching from terminal split to another split exits terminal mode
/// and allows the new buffer to receive keystrokes.
///
/// Regression test for: When clicking on another split while in terminal mode,
/// terminal_mode stayed true but active buffer changed, causing keys to go nowhere.
#[test]
fn test_terminal_split_switch_exits_terminal_mode() {
    let mut harness = harness_or_return!(120, 30);

    // Create a vertical split via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Disable jump_to_end_on_output so terminal output doesn't re-enter terminal mode
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Now we have two splits. Open a terminal in the current (right) split
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Verify we're in terminal mode
    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    let terminal_buffer = harness.editor().active_buffer_id();
    assert!(
        harness.editor().is_terminal_buffer(terminal_buffer),
        "Active buffer should be terminal"
    );

    // Use command palette to switch to previous split (the left one with [No Name])
    // First exit terminal mode temporarily to access command palette
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(
        !harness.editor().is_terminal_mode(),
        "Should have exited terminal mode with Ctrl+Space"
    );

    // Re-enter terminal mode so we can test switching OUT of it
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "Should be back in terminal mode"
    );

    // Now simulate clicking on the left split by using "Previous Split" command
    // But we can't use command palette while in terminal mode...
    // Instead, let's click directly on the left side of the screen

    // Click on the left half of the screen (column 10, which should be in the left split)
    // The left split starts at x=0 for a 120-wide screen split vertically
    harness
        .send_mouse(crossterm::event::MouseEvent {
            kind: crossterm::event::MouseEventKind::Down(crossterm::event::MouseButton::Left),
            column: 10,  // column - well into left split
            row: 15,     // row - middle of content area
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    // Now verify terminal mode is OFF
    assert!(
        !harness.editor().is_terminal_mode(),
        "Terminal mode should be OFF after clicking on non-terminal split"
    );

    // Verify the active buffer is no longer the terminal
    let active_after_click = harness.editor().active_buffer_id();
    assert!(
        !harness.editor().is_terminal_buffer(active_after_click),
        "Active buffer should be non-terminal after clicking left split"
    );

    // Most importantly: verify that keystrokes work in the new buffer
    // Get buffer content before typing
    let content_before = harness
        .editor()
        .get_buffer_content(active_after_click)
        .unwrap_or_default();

    // Type some characters
    harness.type_text("hello").unwrap();

    // Get buffer content after typing
    let content_after = harness
        .editor()
        .get_buffer_content(active_after_click)
        .unwrap_or_default();

    // Content should have changed (text was inserted)
    assert_ne!(
        content_before, content_after,
        "Buffer content should change after typing - keys should work in new split. \
         Before: {:?}, After: {:?}",
        content_before, content_after
    );

    assert!(
        content_after.contains("hello"),
        "Buffer should contain 'hello' after typing, got: {:?}",
        content_after
    );
}

/// Test that closing a terminal tab transfers keyboard focus to remaining tab
#[test]
fn test_close_terminal_tab_transfers_focus_to_remaining_tab() {
    let mut harness = harness_or_return!(80, 24);

    // Create a temp file to work with
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    std::fs::write(&file1, "File content here").unwrap();

    // Open the file first
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");

    // Open a terminal - this should become the active tab
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    // Verify we're in terminal mode
    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    // Close the terminal tab using Alt+W (close_tab)
    // First exit terminal mode to be able to use normal keybindings
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(']'), KeyModifiers::CONTROL);
    harness.render().unwrap();

    // Now close the tab
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Terminal should be closed
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Terminal 0"),
        "Terminal tab should be closed. Screen:\n{}",
        screen
    );

    // file1 should now be active
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("File content here");

    // Should NOT be in terminal mode anymore
    assert!(
        !harness.editor().is_terminal_mode(),
        "Should not be in terminal mode after closing terminal"
    );

    // Type text to verify keyboard focus is on file1
    harness.type_text("TYPED").unwrap();
    harness.render().unwrap();

    // The typed text should appear in the buffer
    harness.assert_screen_contains("TYPED");

    // Save and verify the text was written to file1
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let file1_content = std::fs::read_to_string(&file1).unwrap();
    assert!(
        file1_content.contains("TYPED"),
        "Typed text should be saved to file1. Content: {}",
        file1_content
    );
}

/// Test that closing terminal tab via mouse click (while in terminal mode) transfers focus
#[test]
fn test_close_terminal_tab_in_terminal_mode_via_mouse() {
    use crate::common::harness::layout;

    let mut harness = harness_or_return!(80, 24);

    // Create a temp file to work with
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    std::fs::write(&file1, "File content here").unwrap();

    // Open the file first
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");

    // Open a terminal - this should become the active tab and enter terminal mode
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    // Verify we're in terminal mode
    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    // Find the × button for the terminal tab in the tab bar
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    // Find the position of the × for Terminal 0 tab (should be after "Terminal 0")
    // The tab bar shows tabs like: "file1.txt × | *Terminal 0* ×"
    // We want the second × (the one for the terminal tab)
    let terminal_x_pos = tab_row
        .rmatch_indices('×')
        .next()
        .map(|(pos, _)| pos)
        .expect("Could not find × close button for terminal tab");

    // Click on the × button while still in terminal mode
    harness
        .mouse_click(terminal_x_pos as u16, layout::TAB_BAR_ROW as u16)
        .unwrap();
    harness.render().unwrap();

    // Terminal should be closed
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Terminal 0"),
        "Terminal tab should be closed. Screen:\n{}",
        screen
    );

    // file1 should now be active
    harness.assert_screen_contains("file1.txt");
    harness.assert_screen_contains("File content here");

    // Should NOT be in terminal mode anymore
    assert!(
        !harness.editor().is_terminal_mode(),
        "Should not be in terminal mode after closing terminal via mouse"
    );

    // Type text to verify keyboard focus is on file1
    harness.type_text("TYPED").unwrap();
    harness.render().unwrap();

    // The typed text should appear in the buffer
    harness.assert_screen_contains("TYPED");

    // Save and verify the text was written to file1
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let file1_content = std::fs::read_to_string(&file1).unwrap();
    assert!(
        file1_content.contains("TYPED"),
        "Typed text should be saved to file1. Content: {}",
        file1_content
    );
}
