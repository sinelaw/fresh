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
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell commands (echo with single quotes)
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
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell commands (seq, for loop, echo)
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
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::TRACE.into()))
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
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Toggle keyboard capture ON with F9
    tracing::info!("=== Toggling keyboard capture ON ===");
    harness.send_key(KeyCode::F(9), KeyModifiers::NONE).unwrap();
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
    harness.send_key(KeyCode::F(9), KeyModifiers::NONE).unwrap();
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
    harness.send_key(KeyCode::F(9), KeyModifiers::NONE).unwrap();
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
            column: 10, // column - well into left split
            row: 15,    // row - middle of content area
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

/// Test clicking between splits with terminal preserves correct focus behavior
/// When terminal is active in one split and file in another, clicking between them
/// should properly transfer focus and clicking back on terminal should restore terminal mode.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell commands (echo)
fn test_click_between_splits_terminal_focus() {
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

    // Disable jump_to_end_on_output so terminal output doesn't interfere
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

    // Screen is 120 wide, split vertically means left split is ~60 cols, right split is ~60 cols
    // Left split content area starts around column 8 (after gutter)
    // Right split content area starts around column 68
    let left_split_col: u16 = 10;
    let right_split_col: u16 = 100;
    let content_row: u16 = 15;

    // Delay to avoid double-click detection (use config value * 2 for safety margin)
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);

    // Repeat the click cycle 3 times to ensure consistent behavior
    for iteration in 1..=3 {
        // Currently on terminal (right split), terminal mode is active
        assert!(
            harness.editor().is_terminal_mode(),
            "Iteration {}: Should be in terminal mode before clicking file split",
            iteration
        );
        assert!(
            harness
                .editor()
                .is_terminal_buffer(harness.editor().active_buffer_id()),
            "Iteration {}: Active buffer should be terminal before clicking file split",
            iteration
        );

        // Click on the left split (file buffer)
        harness
            .send_mouse(crossterm::event::MouseEvent {
                kind: crossterm::event::MouseEventKind::Down(crossterm::event::MouseButton::Left),
                column: left_split_col,
                row: content_row,
                modifiers: KeyModifiers::NONE,
            })
            .unwrap();
        harness.render().unwrap();

        // Terminal mode should be OFF (we clicked on a file split)
        assert!(
            !harness.editor().is_terminal_mode(),
            "Iteration {}: Terminal mode should be OFF after clicking on file split",
            iteration
        );

        // Active buffer should be the file (non-terminal)
        assert!(
            !harness
                .editor()
                .is_terminal_buffer(harness.editor().active_buffer_id()),
            "Iteration {}: Active buffer should be file (non-terminal) after clicking file split",
            iteration
        );

        // Wait to avoid double-click detection
        std::thread::sleep(double_click_delay);

        // Click back on the right split (terminal)
        harness
            .send_mouse(crossterm::event::MouseEvent {
                kind: crossterm::event::MouseEventKind::Down(crossterm::event::MouseButton::Left),
                column: right_split_col,
                row: content_row,
                modifiers: KeyModifiers::NONE,
            })
            .unwrap();
        harness.render().unwrap();

        // Terminal mode should be restored (we clicked on terminal split)
        assert!(
            harness.editor().is_terminal_mode(),
            "Iteration {}: Terminal mode should be restored after clicking back on terminal split",
            iteration
        );

        // Active buffer should be the terminal again
        assert!(
            harness
                .editor()
                .is_terminal_buffer(harness.editor().active_buffer_id()),
            "Iteration {}: Active buffer should be terminal after clicking terminal split",
            iteration
        );

        // Wait to avoid double-click detection between iterations
        std::thread::sleep(double_click_delay);
    }

    // Final verification: type in terminal to confirm it's truly active
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('e'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('c'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('h'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('o'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(' '), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('O'), KeyModifiers::SHIFT);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('K'), KeyModifiers::SHIFT);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Enter, KeyModifiers::NONE);

    // Wait for terminal output to appear (use real wall-clock time for async I/O)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("OK") || screen.contains("echo")
        })
        .unwrap();
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

/// Test switching between terminal and file tabs preserves terminal mode
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell commands (echo)
fn test_terminal_mode_preserved_when_switching_tabs() {
    let mut harness = harness_or_return!(80, 24);

    // Create a temp file to work with
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    std::fs::write(&file1, "File content").unwrap();

    // Open the file first
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("file1.txt");

    // Open a terminal - should enter terminal mode automatically
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    // Switch to file tab while in terminal mode (using Ctrl+PageUp which works in terminal mode)
    // This should temporarily exit terminal mode
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();

    // Verify we're on file1 and not in terminal mode
    harness.assert_screen_contains("File content");
    assert!(
        !harness.editor().is_terminal_mode(),
        "Should not be in terminal mode when viewing file"
    );

    // Switch back to terminal tab - should automatically restore terminal mode
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should see terminal tab is active
    harness.assert_screen_contains("*Terminal 0*");

    // Terminal mode should be automatically restored since we were in terminal mode before
    assert!(
        harness.editor().is_terminal_mode(),
        "Terminal mode should be restored when switching back to terminal"
    );

    // Now test executing a command in the terminal
    // Type a simple command (echo) - this tests that terminal input works
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('e'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('c'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('h'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('o'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(' '), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('H'), KeyModifiers::SHIFT);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('I'), KeyModifiers::SHIFT);
    harness.render().unwrap();

    // Execute the command
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Enter, KeyModifiers::NONE);

    // Wait for command to execute - use semantic waiting instead of fixed timer
    // The terminal should show "HI" in the output (from echo HI)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("HI") || screen.contains("echo")
        })
        .expect("Terminal should show command output or the typed command");

    // Test the full cycle again: switch away and back multiple times
    // Switch to file
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_screen_contains("File content");

    // Switch back to terminal - should restore terminal mode
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.editor().is_terminal_mode(),
        "Terminal mode should be restored after second switch back"
    );

    // === Now test switching tabs via mouse clicks ===
    use crate::common::harness::layout;

    // Get the tab bar to find tab positions
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    // Find the position of "file1.txt" in the tab bar (clicking on it should switch to it)
    // Tab format is something like: " file1.txt × | *Terminal 0* × "
    let file_tab_pos = tab_row
        .find("file1")
        .expect("Should find file1.txt tab in tab bar");

    // We're currently on terminal (in terminal mode), click on file tab
    harness
        .mouse_click(file_tab_pos as u16, layout::TAB_BAR_ROW as u16)
        .unwrap();
    harness.render().unwrap();

    // Should now be viewing file content
    harness.assert_screen_contains("File content");
    assert!(
        !harness.editor().is_terminal_mode(),
        "Should not be in terminal mode after clicking file tab"
    );

    // Get updated tab bar for terminal position
    let screen = harness.screen_to_string();
    let tab_row: String = screen
        .lines()
        .nth(layout::TAB_BAR_ROW)
        .unwrap_or("")
        .to_string();

    // Find terminal tab position (look for "Terminal" text)
    let terminal_tab_pos = tab_row
        .find("Terminal")
        .expect("Should find Terminal tab in tab bar");

    // Click on terminal tab to switch back
    harness
        .mouse_click(terminal_tab_pos as u16, layout::TAB_BAR_ROW as u16)
        .unwrap();
    harness.render().unwrap();

    // Should see terminal tab is active again
    harness.assert_screen_contains("*Terminal 0*");

    // Terminal mode should be restored when clicking back to terminal
    assert!(
        harness.editor().is_terminal_mode(),
        "Terminal mode should be restored when clicking terminal tab"
    );

    // Verify keyboard input works after clicking - type something in terminal
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('p'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('w'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char('d'), KeyModifiers::NONE);
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Enter, KeyModifiers::NONE);

    // Wait for pwd command to execute - use semantic waiting instead of fixed timer
    // The terminal should show pwd command was executed (shows path or "pwd")
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("pwd") || screen.contains("/")
        })
        .expect("Terminal should show pwd command or path output after click switch");
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

/// Test that terminal view follows output when cursor is at the very last line.
///
/// Reproduces the bug where pressing Enter many times in a terminal causes the
/// cursor to reach the bottom of the screen. Once the cursor is at the last line,
/// the view should continue to follow the cursor (output), but previously it would
/// stop updating until a resize event was triggered.
///
/// The fix adds `resize_visible_terminals()` call when entering terminal mode,
/// which ensures proper PTY sizing and view following.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_view_follows_output_at_bottom() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    assert!(harness.editor().is_terminal_mode());

    // Get terminal dimensions
    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    let (_, rows) = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .unwrap()
        .size();

    // Press Enter many times to push cursor to the bottom of the screen.
    // This fills the screen with shell prompts, pushing the cursor down.
    // We press more than the terminal rows to ensure cursor reaches bottom.
    for i in 0..(rows as usize + 5) {
        harness
            .editor_mut()
            .handle_terminal_key(KeyCode::Enter, KeyModifiers::NONE);

        // Give the shell time to respond every few iterations
        if i % 5 == 0 {
            harness.sleep(std::time::Duration::from_millis(20));
        }
    }

    // Wait for output to settle
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    // Now type a unique marker that we can search for
    harness
        .editor_mut()
        .send_terminal_input(b"echo BOTTOM_MARKER_XYZ\n");

    // Wait for the marker to appear on screen
    let result = harness.wait_until(|h| h.screen_to_string().contains("BOTTOM_MARKER_XYZ"));
    assert!(
        result.is_ok(),
        "Terminal view should show BOTTOM_MARKER_XYZ after pressing Enter many times. \
         The view should follow output to the cursor position at the bottom. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Test that terminal properly resizes when re-entering terminal mode.
///
/// This verifies that entering terminal mode triggers a resize to ensure
/// the PTY dimensions match the current split dimensions.
#[test]
fn test_terminal_resize_on_enter_mode() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Get terminal size after opening
    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
    let (cols1, rows1) = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .unwrap()
        .size();

    // Exit terminal mode
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(']'), KeyModifiers::CONTROL);
    assert!(!harness.editor().is_terminal_mode());

    // Re-enter terminal mode
    harness.editor_mut().enter_terminal_mode();
    assert!(harness.editor().is_terminal_mode());

    // Get terminal size after re-entering
    let (cols2, rows2) = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .unwrap()
        .size();

    // Size should be the same (resize should have been called to ensure consistency)
    assert_eq!(
        cols1, cols2,
        "Terminal columns should match after re-entering"
    );
    assert_eq!(rows1, rows2, "Terminal rows should match after re-entering");
}

/// Test that terminal scrollback content is restored when session is restored.
///
/// This verifies the bug where terminal scrollback was empty after session restore
/// because create_terminal_buffer_detached was overwriting the backing file.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_session_restore_terminal_scrollback() {
    use fresh::config::Config;
    use fresh::config_io::DirectoryContext;
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

    // Create temp directories that persist across both sessions
    let data_temp_dir = TempDir::new().unwrap();
    let project_temp_dir = TempDir::new().unwrap();
    let project_dir = project_temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create a shared DirectoryContext that both sessions will use
    let dir_context = DirectoryContext::for_testing(data_temp_dir.path());

    let backing_path_for_check: std::path::PathBuf;

    // First session: open terminal and generate scrollback content
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            80,
            24,
            Config::default(),
            project_dir.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Open a terminal
        harness.editor_mut().open_terminal();
        harness.render().unwrap();
        assert!(harness.editor().is_terminal_mode());

        // Generate unique scrollback content
        harness
            .editor_mut()
            .send_terminal_input(b"echo 'SCROLLBACK_MARKER_12345'\n");

        // Wait for the marker to appear
        let result =
            harness.wait_until(|h| h.screen_to_string().contains("SCROLLBACK_MARKER_12345"));
        assert!(
            result.is_ok(),
            "Terminal should show scrollback marker. Screen:\n{}",
            harness.screen_to_string()
        );

        // Exit terminal mode to enter scrollback view (this syncs content to backing file)
        harness
            .editor_mut()
            .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        assert!(!harness.editor().is_terminal_mode());

        // Verify content is in buffer before saving
        let buffer_id = harness.editor().active_buffer_id();
        let content_before_save = harness.editor().get_buffer_content(buffer_id);
        assert!(
            content_before_save
                .as_ref()
                .map(|c| c.contains("SCROLLBACK_MARKER_12345"))
                .unwrap_or(false),
            "Buffer should contain scrollback marker before save. Content: {:?}",
            content_before_save
        );

        // Save session
        harness.editor_mut().save_session().unwrap();

        // Get the backing file path for later verification
        let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
        backing_path_for_check = harness
            .editor()
            .terminal_backing_files()
            .get(&terminal_id)
            .cloned()
            .unwrap();

        // Verify backing file content after save
        let backing_content = std::fs::read_to_string(&backing_path_for_check).unwrap_or_default();
        assert!(
            backing_content.contains("SCROLLBACK_MARKER_12345"),
            "Backing file should contain marker after save"
        );
    }

    // Verify backing file still exists and has content before restore
    let pre_restore_content = std::fs::read_to_string(&backing_path_for_check).unwrap_or_default();
    assert!(
        pre_restore_content.contains("SCROLLBACK_MARKER_12345"),
        "Backing file should still contain marker before second session"
    );

    // Second session: restore and verify scrollback content is preserved
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            80,
            24,
            Config::default(),
            project_dir.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should have been restored");

        // Verify backing file was NOT overwritten during restore
        let post_restore_content =
            std::fs::read_to_string(&backing_path_for_check).unwrap_or_default();
        assert!(
            post_restore_content.contains("SCROLLBACK_MARKER_12345"),
            "Backing file should still contain marker after restore (must not be truncated)"
        );

        harness.render().unwrap();

        // Find the terminal buffer
        let buffer_id = harness.editor().active_buffer_id();
        let is_terminal = harness.editor().is_terminal_buffer(buffer_id);

        if is_terminal {
            // Get buffer content - CRITICAL: The scrollback content should be restored
            let content_after_restore = harness.editor().get_buffer_content(buffer_id);
            assert!(
                content_after_restore
                    .as_ref()
                    .map(|c| c.contains("SCROLLBACK_MARKER_12345"))
                    .unwrap_or(false),
                "BUG: Terminal scrollback should contain marker after restore. Content: {:?}",
                content_after_restore
            );
        } else {
            // If terminal wasn't the active buffer, the terminal tab should still exist
            let screen = harness.screen_to_string();
            assert!(
                screen.contains("Terminal"),
                "Terminal tab should be restored. Screen:\n{}",
                screen
            );
        }
    }
}

/// Test that NEW scrollback generated after session restore is captured.
///
/// This reproduces a bug where `backing_writer` is set to None when the backing file
/// already exists (from the first session), causing all new scrollback to be lost.
///
/// The test:
/// 1. First session: create terminal, generate scrollback with FIRST_MARKER, save
/// 2. Second session: restore, generate NEW scrollback with SECOND_MARKER
/// 3. Verify SECOND_MARKER appears in scrollback (proves new content is captured)
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_scrollback_captured_after_session_restore() {
    use fresh::config::Config;
    use fresh::config_io::DirectoryContext;
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

    // Create temp directories that persist across both sessions
    let data_temp_dir = TempDir::new().unwrap();
    let project_temp_dir = TempDir::new().unwrap();
    let project_dir = project_temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create a shared DirectoryContext that both sessions will use
    let dir_context = DirectoryContext::for_testing(data_temp_dir.path());

    let backing_path_for_check: std::path::PathBuf;

    // First session: open terminal and generate scrollback content
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            80,
            24,
            Config::default(),
            project_dir.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Open a terminal
        harness.editor_mut().open_terminal();
        harness.render().unwrap();
        assert!(harness.editor().is_terminal_mode());

        // Generate scrollback content with FIRST marker
        harness
            .editor_mut()
            .send_terminal_input(b"echo 'FIRST_SESSION_MARKER_AAA'\n");

        // Wait for the marker to appear
        let result =
            harness.wait_until(|h| h.screen_to_string().contains("FIRST_SESSION_MARKER_AAA"));
        assert!(
            result.is_ok(),
            "Terminal should show first marker. Screen:\n{}",
            harness.screen_to_string()
        );

        // Exit terminal mode to sync content to backing file
        harness
            .editor_mut()
            .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        assert!(!harness.editor().is_terminal_mode());

        // Save session
        harness.editor_mut().save_session().unwrap();

        // Get the backing file path for later verification
        let buffer_id = harness.editor().active_buffer_id();
        let terminal_id = harness.editor().get_terminal_id(buffer_id).unwrap();
        backing_path_for_check = harness
            .editor()
            .terminal_backing_files()
            .get(&terminal_id)
            .cloned()
            .unwrap();

        // Verify backing file has first marker
        let backing_content = std::fs::read_to_string(&backing_path_for_check).unwrap_or_default();
        assert!(
            backing_content.contains("FIRST_SESSION_MARKER_AAA"),
            "Backing file should contain first marker after save"
        );
    }

    // Second session: restore and generate NEW scrollback
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            80,
            24,
            Config::default(),
            project_dir.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should have been restored");
        harness.render().unwrap();

        // Re-enter terminal mode to interact with the restored terminal
        let buffer_id = harness.editor().active_buffer_id();
        if !harness.editor().is_terminal_mode() {
            harness
                .editor_mut()
                .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
                .unwrap();
            harness.render().unwrap();
        }
        assert!(
            harness.editor().is_terminal_mode(),
            "Should be in terminal mode"
        );

        // Generate enough output to push content into scrollback
        // Use many lines to ensure SECOND_MARKER gets pushed into scrollback history
        harness
            .editor_mut()
            .send_terminal_input(b"echo 'SECOND_SESSION_MARKER_BBB'\n");

        harness
            .wait_until(|h| h.screen_to_string().contains("SECOND_SESSION_MARKER_BBB"))
            .unwrap();

        // Generate more output to push SECOND_MARKER into scrollback
        harness
            .editor_mut()
            .send_terminal_input(b"for i in $(seq 1 50); do echo \"Post-restore line $i\"; done\n");

        harness
            .wait_until(|h| h.screen_to_string().contains("Post-restore line 50"))
            .unwrap();

        // Disable jump_to_end_on_output so we can stay in scrollback mode
        harness
            .editor_mut()
            .set_terminal_jump_to_end_on_output(false);

        // Exit terminal mode to enter scrollback view
        harness
            .editor_mut()
            .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        harness.sleep(std::time::Duration::from_millis(50));

        // Get the full buffer content
        let content = harness
            .editor()
            .get_buffer_content(buffer_id)
            .unwrap_or_default();

        // CRITICAL: The SECOND marker should be in the scrollback
        // This fails if backing_writer was None after restore
        assert!(
            content.contains("SECOND_SESSION_MARKER_BBB"),
            "BUG: Scrollback should contain SECOND marker (generated after restore).\n\
             This fails if backing_writer is None for restored sessions.\n\
             Content length: {}\nContent:\n{}",
            content.len(),
            &content[..content.len().min(2000)]
        );

        // Also verify first marker is still there
        assert!(
            content.contains("FIRST_SESSION_MARKER_AAA"),
            "Scrollback should still contain FIRST marker from original session.\nContent:\n{}",
            &content[..content.len().min(2000)]
        );
    }
}

/// Test that scrollback content is stable and accessible after repeated mode toggles.
///
/// This test verifies:
/// 1. Scrollback history is preserved across terminal mode toggles
/// 2. Content doesn't accumulate (no duplicate visible screens appended)
/// 3. User can scroll to the beginning of history using Ctrl+Home
///
/// The test fills the screen with numbered output lines, then repeatedly
/// toggles between terminal mode and scrollback mode, verifying each time
/// that the full history is accessible.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_scrollback_stable_after_multiple_mode_toggles() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Disable jump_to_end_on_output so we can stay in scrollback mode
    // while the shell may still be producing output
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    let buffer_id = harness.editor().active_buffer_id();

    // Generate enough output to fill the screen and create scrollback
    // Use a unique marker at the START that we can verify we can scroll back to
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'START_MARKER_12345'\n");

    // Wait for the start marker
    harness
        .wait_until(|h| h.screen_to_string().contains("START_MARKER_12345"))
        .unwrap();

    // Generate many lines to push the start marker into scrollback
    harness
        .editor_mut()
        .send_terminal_input(b"for i in $(seq 1 50); do echo \"Line $i of output\"; done\n");

    // Wait for the last line to appear (ensures command completed)
    harness
        .wait_until(|h| h.screen_to_string().contains("Line 50 of output"))
        .unwrap();

    // Add an end marker
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'END_MARKER_67890'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("END_MARKER_67890"))
        .unwrap();

    // Now toggle terminal mode ON and OFF multiple times, checking scrollback each time
    for i in 0..3 {
        // Exit terminal mode to enter scrollback view
        harness
            .editor_mut()
            .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        assert!(
            !harness.editor().is_terminal_mode(),
            "Iteration {}: Should be in scrollback mode after Ctrl+Space",
            i
        );

        // Small delay to ensure buffer sync completes
        harness.sleep(std::time::Duration::from_millis(50));
        harness.render().unwrap();

        // Get the full buffer content - it should contain both markers
        let content = harness
            .editor()
            .get_buffer_content(buffer_id)
            .unwrap_or_default();

        assert!(
            content.contains("START_MARKER_12345"),
            "Iteration {}: Scrollback should contain START marker. Content length: {}\nContent:\n{}",
            i,
            content.len(),
            &content[..content.len().min(500)]
        );

        assert!(
            content.contains("END_MARKER_67890"),
            "Iteration {}: Scrollback should contain END marker. Content:\n{}",
            i,
            &content[..content.len().min(500)]
        );

        // Use Ctrl+Home to scroll to the very beginning
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        // DEBUG: Check terminal mode after Ctrl+Home
        eprintln!(
            "DEBUG iteration {}: after Ctrl+Home, terminal_mode={}",
            i,
            harness.editor().is_terminal_mode()
        );

        // The screen should now show the START marker (near the top of history)
        let screen = harness.screen_to_string();
        assert!(
            screen.contains("START_MARKER_12345"),
            "Iteration {}: After Ctrl+Home, screen should show START marker.\nScreen:\n{}",
            i,
            screen
        );

        // Re-enter terminal mode
        harness
            .editor_mut()
            .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        assert!(
            harness.editor().is_terminal_mode(),
            "Iteration {}: Should be in terminal mode after second Ctrl+Space",
            i
        );
    }

    // Final check: exit one more time and verify content length is reasonable
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.sleep(std::time::Duration::from_millis(50));

    let final_content = harness
        .editor()
        .get_buffer_content(buffer_id)
        .unwrap_or_default();

    // Count how many times the START marker appears - should be exactly 2
    // (once from the echo command, once from the output)
    let start_count = final_content.matches("START_MARKER_12345").count();
    assert!(
        start_count <= 3, // Allow some variance for shell echo behavior
        "BUG: START marker appears {} times - content may be accumulating!\nContent:\n{}",
        start_count,
        &final_content[..final_content.len().min(1000)]
    );
}

/// Test that Open File dialog uses terminal's initial CWD, not the backing file directory
///
/// When the user opens the file dialog from a terminal buffer, it should start in
/// the terminal's working directory (project root by default), not the internal
/// backing file directory (~/.local/share/fresh/terminals/).
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_open_file_from_terminal_uses_correct_directory() {
    // Skip if PTY is not available
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
        return;
    }

    // Create harness with temp project so we have a known project root
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Open a terminal - it should start in the project root
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    // Exit terminal mode (Ctrl+])
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(']'), KeyModifiers::CONTROL);
    harness.render().unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Now open the file dialog (Ctrl+O)
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The file open dialog should show the project root directory, not the terminal
    // backing file directory (which would be something like ~/.local/share/fresh/terminals/)
    let screen = harness.screen_to_string();

    // The dialog should show the project root path
    assert!(
        screen.contains(&project_root.to_string_lossy().to_string()),
        "Open File dialog should show project root directory.\nScreen:\n{}",
        screen
    );

    // It should NOT show the terminals data directory
    assert!(
        !screen.contains("terminals/"),
        "Open File dialog should NOT show terminal backing file directory.\nScreen:\n{}",
        screen
    );
}

/// BUG: Re-entering scrollback mode after scrolling up jumps to old scroll position
///
/// When:
/// 1. Enter scrollback mode
/// 2. Scroll up to view history
/// 3. Exit scrollback (re-enter terminal mode)
/// 4. Enter scrollback mode again
///
/// Expected: Viewport should be at the bottom (showing cursor/prompt)
/// Actual: Viewport jumps to the previous scroll position from step 2
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_scrollback_viewport_resets_on_reentry() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Disable jump_to_end_on_output so terminal output doesn't affect our test
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Generate enough output to create scrollback history
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'HISTORY_START_MARKER'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("HISTORY_START_MARKER"))
        .unwrap();

    // Generate many lines to push the start marker into scrollback
    harness
        .editor_mut()
        .send_terminal_input(b"for i in $(seq 1 50); do echo \"History line $i\"; done\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("History line 50"))
        .unwrap();

    // Add an end marker that will be visible at the bottom
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'BOTTOM_MARKER_XYZ'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("BOTTOM_MARKER_XYZ"))
        .unwrap();

    // === First scrollback entry ===
    // Exit terminal mode to enter scrollback
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Should see the bottom marker (viewport at end)
    harness.assert_screen_contains("BOTTOM_MARKER_XYZ");

    // Scroll up significantly to view history
    for _ in 0..15 {
        harness
            .editor_mut()
            .handle_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // After scrolling up, bottom marker should NOT be visible
    harness.assert_screen_not_contains("BOTTOM_MARKER_XYZ");
    // But history start marker should be visible
    harness.assert_screen_contains("HISTORY_START_MARKER");

    // === Exit scrollback, re-enter terminal mode ===
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // === Second scrollback entry - this is where the bug manifests ===
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // BUG: The viewport should be at the bottom again, showing BOTTOM_MARKER_XYZ
    // But with the bug, it jumps to the old scroll position (showing HISTORY_START_MARKER)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("BOTTOM_MARKER_XYZ"),
        "BUG: After re-entering scrollback mode, viewport should be at the bottom.\n\
         Expected to see BOTTOM_MARKER_XYZ but got:\n{}",
        screen
    );
}

/// Same as test_scrollback_viewport_resets_on_reentry but using mouse scroll
///
/// This test uses mouse scroll which sets the skip_ensure_visible flag differently
/// than keyboard scrolling, which is the actual bug trigger in real usage.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_scrollback_viewport_resets_on_reentry_mouse_scroll() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Disable jump_to_end_on_output so terminal output doesn't affect our test
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Generate enough output to create scrollback history
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'HISTORY_START_MARKER'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("HISTORY_START_MARKER"))
        .unwrap();

    // Generate many lines to push the start marker into scrollback
    harness
        .editor_mut()
        .send_terminal_input(b"for i in $(seq 1 50); do echo \"History line $i\"; done\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("History line 50"))
        .unwrap();

    // Add an end marker that will be visible at the bottom
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'BOTTOM_MARKER_XYZ'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("BOTTOM_MARKER_XYZ"))
        .unwrap();

    // === First scrollback entry ===
    // Exit terminal mode to enter scrollback
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // Should see the bottom marker (viewport at end)
    harness.assert_screen_contains("BOTTOM_MARKER_XYZ");

    // Scroll up using MOUSE SCROLL (this sets skip_ensure_visible flag)
    // Mouse scroll at position in the content area (col 40, row 12)
    for _ in 0..50 {
        harness.mouse_scroll_up(40, 12).unwrap();
    }
    harness.render().unwrap();

    // After scrolling up, bottom marker should NOT be visible
    harness.assert_screen_not_contains("BOTTOM_MARKER_XYZ");
    // But history start marker should be visible
    harness.assert_screen_contains("HISTORY_START_MARKER");

    // === Exit scrollback, re-enter terminal mode ===
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // === Second scrollback entry - this is where the bug manifests ===
    harness
        .editor_mut()
        .handle_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // The viewport should be at the bottom again, showing BOTTOM_MARKER_XYZ
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("BOTTOM_MARKER_XYZ"),
        "After re-entering scrollback mode, viewport should be at the bottom.\n\
         Expected to see BOTTOM_MARKER_XYZ but got:\n{}",
        screen
    );
}

/// Test that terminal process exit keeps buffer open with exit message
///
/// When a terminal process exits (e.g., via 'exit' command):
/// 1. The final screen state should be preserved in the buffer
/// 2. An "[Terminal process exited]" message should be appended
/// 3. The buffer should remain open in read-only scrollback mode
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_exit_keeps_buffer_with_message() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    let buffer_id = harness.editor().active_buffer_id();

    // Generate some output before exiting
    harness
        .editor_mut()
        .send_terminal_input(b"echo 'BEFORE_EXIT_MARKER'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("BEFORE_EXIT_MARKER"))
        .unwrap();

    // Exit the terminal by typing 'exit'
    harness.editor_mut().send_terminal_input(b"exit\n");

    // Wait for terminal to exit and buffer to show the exit message
    harness
        .wait_until(|h| {
            let content = h.editor().get_buffer_content(buffer_id).unwrap_or_default();
            content.contains("[Terminal process exited]")
        })
        .unwrap();

    harness.render().unwrap();

    // Buffer should still be open - we can verify by checking active_buffer_id matches
    assert_eq!(
        harness.editor().active_buffer_id(),
        buffer_id,
        "Buffer should still be the active buffer after terminal exit"
    );

    // Should no longer be in terminal mode
    assert!(
        !harness.editor().is_terminal_mode(),
        "Should not be in terminal mode after terminal exit"
    );

    // Buffer should be read-only (editing disabled) - verify via is_editing_disabled
    assert!(
        harness.editor().is_editing_disabled(),
        "Buffer should be read-only after terminal exit"
    );

    // Buffer content should contain the exit marker and exit message
    let content = harness
        .editor()
        .get_buffer_content(buffer_id)
        .unwrap_or_default();
    assert!(
        content.contains("BEFORE_EXIT_MARKER"),
        "Buffer should preserve content from before exit.\nContent:\n{}",
        content
    );
    assert!(
        content.contains("[Terminal process exited]"),
        "Buffer should contain exit message.\nContent:\n{}",
        content
    );

    // Screen should show the exit message
    harness.assert_screen_contains("[Terminal process exited]");
}
