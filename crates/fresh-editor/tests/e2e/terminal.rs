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
use fresh::config::{Config, TerminalShellConfig};
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

/// Locate the 0-based column of `needle` on the (0-based) tab row (row 1).
fn tab_row_col_of(screen: &str, needle: char) -> Option<u16> {
    screen
        .lines()
        .nth(1)?
        .chars()
        .position(|c| c == needle)
        .map(|p| p as u16)
}

/// Locate the 0-based column where `needle` begins on the tab row (row 1).
fn tab_row_col_of_str(screen: &str, needle: &str) -> Option<u16> {
    let line = screen.lines().nth(1)?;
    let byte_idx = line.find(needle)?;
    Some(line[..byte_idx].chars().count() as u16)
}

/// Regression: with the keyboard focused on an *active terminal* buffer
/// (terminal mode owns the keyboard), opening the tab bar's "+" popup must
/// still hand the keyboard to the popup. Its navigation keys drive the menu
/// instead of leaking into the terminal's PTY child.
///
/// Without the popup being registered as a terminal-blocking overlay,
/// `dispatch_terminal_input` (which runs before the popup's key handler)
/// would swallow the keys: Enter would reach the shell, the popup would stay
/// open, and no new buffer would be created.
#[test]
fn plus_button_menu_grabs_keyboard_from_active_terminal() {
    let mut harness = harness_or_return!(120, 30);

    // Focus an active terminal buffer.
    harness.editor_mut().open_terminal();
    assert!(harness.editor().is_terminal_mode());
    harness.render().unwrap();

    // Open the tab bar's trailing "+" popup (a left-click on chrome that
    // leaves the terminal the active, focused buffer).
    let screen = harness.screen_to_string();
    let plus_col = tab_row_col_of(&screen, '+').unwrap_or_else(|| {
        panic!("expected a '+' new-tab button on the tab row. Screen:\n{screen}")
    });
    harness.mouse_click(plus_col, 1).unwrap();
    harness.assert_screen_contains("New Terminal");
    harness.assert_screen_contains("New File");

    // Drive the popup from the keyboard: Down highlights "New File", Enter
    // activates it. If the keys leaked into the terminal, the popup would
    // stay open and no new file buffer would appear.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("New Terminal"),
        "popup should close once Enter selects an item. Screen:\n{screen}"
    );
    harness.assert_screen_contains("[No Name] 2");
}

/// Regression: the tab right-click context menu must likewise grab the
/// keyboard from an active terminal. Opened over the terminal's own tab,
/// Esc dismisses it — without the fix Esc would reach the PTY instead and
/// the menu would stay open.
#[test]
fn tab_context_menu_grabs_keyboard_from_active_terminal() {
    let mut harness = harness_or_return!(120, 30);

    harness.editor_mut().open_terminal();
    assert!(harness.editor().is_terminal_mode());
    harness.render().unwrap();

    // Right-click the terminal's own tab so the terminal stays the active
    // buffer (and terminal mode keeps the keyboard).
    let screen = harness.screen_to_string();
    let term_col = tab_row_col_of_str(&screen, "Terminal").unwrap_or_else(|| {
        panic!("expected the '*Terminal 0*' tab on the tab row. Screen:\n{screen}")
    });
    harness.mouse_right_click(term_col, 1).unwrap();
    harness.assert_screen_contains("Close Others");

    // Esc must dismiss the menu rather than leak into the terminal.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Close Others"),
        "Esc should dismiss the tab context menu opened over a terminal. Screen:\n{screen}"
    );
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

/// Running "Open Terminal to the Right" from the command palette creates a
/// terminal in a new split beside the editor: both panes stay visible side
/// by side, with the terminal to the right of the still-visible editor
/// content. Drives the real palette flow (keyboard only) and asserts purely
/// on rendered output.
#[test]
fn test_open_terminal_to_the_right_via_palette() {
    let mut harness = harness_or_return!(120, 24);

    // Distinctive content so the editor pane is locatable on screen.
    harness.type_text("EDITORPANE").unwrap();
    harness.render().unwrap();

    // Ctrl+P opens the command palette already in command (">") mode.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("open terminal to the right").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // A terminal pane appeared alongside the still-visible editor content.
    let (term_col, _term_row) = harness
        .find_text_on_screen("*Terminal 0*")
        .expect("terminal tab should be visible after running the command");
    let (editor_col, _editor_row) = harness
        .find_text_on_screen("EDITORPANE")
        .expect("editor content should remain visible in its own pane");

    // Vertical split → side by side: the terminal sits to the right of the
    // editor content.
    assert!(
        term_col > editor_col,
        "terminal pane should be to the right (term col {term_col} vs editor col {editor_col})"
    );
}

/// Running "Open Terminal Below" from the command palette creates a terminal
/// in a new split stacked under the editor: the editor content stays visible
/// on top with the terminal below it. Keyboard-driven; asserts only on
/// rendered output.
#[test]
fn test_open_terminal_below_via_palette() {
    let mut harness = harness_or_return!(120, 24);

    harness.type_text("EDITORPANE").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("open terminal below").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let (_term_col, term_row) = harness
        .find_text_on_screen("*Terminal 0*")
        .expect("terminal tab should be visible after running the command");
    let (_editor_col, editor_row) = harness
        .find_text_on_screen("EDITORPANE")
        .expect("editor content should remain visible in its own pane");

    // Horizontal split → stacked: the terminal sits below the editor content.
    assert!(
        term_row > editor_row,
        "terminal pane should be below the editor (term row {term_row} vs editor row {editor_row})"
    );
}

/// Test closing a terminal
#[test]
fn test_close_terminal() {
    // 120×24 instead of 80×24: with `{remote}` on the default
    // status bar the trailing Messages element is truncated at
    // 80 cols ("closed" wouldn't fit alongside ` Local | ...`).
    let mut harness = harness_or_return!(120, 24);

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
    let mut harness = harness_or_return!(120, 24);

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
    harness.assert_screen_contains("Terminal mode");
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
    assert!(!harness
        .editor()
        .active_window()
        .is_terminal_buffer(initial_buffer));

    // Open a terminal
    harness.editor_mut().open_terminal();

    // Current buffer should now be a terminal
    let terminal_buffer = harness.editor().active_buffer_id();
    assert!(harness
        .editor()
        .active_window()
        .is_terminal_buffer(terminal_buffer));

    // Should have a valid terminal ID
    assert!(harness
        .editor()
        .active_window()
        .get_terminal_id(terminal_buffer)
        .is_some());
}

/// Test closing terminal when not viewing one shows appropriate message
#[test]
fn test_close_terminal_not_viewing() {
    // 120×24: see the comment on `test_close_terminal` above.
    let mut harness = harness_or_return!(120, 24);

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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();

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
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"echo hello\n");

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

/// A terminal tab auto-names from its foreground process, tmux-style: the
/// command running in the pty (here the shell itself, right after open)
/// replaces the default `*Terminal 0*` label. Asserted on the rendered tab
/// bar row, so it observes real output rather than inspecting model state.
///
/// Linux-only: the foreground process group is read via `tcgetpgrp` +
/// `/proc`, which other platforms don't implement (they fall back to the
/// OSC title / default).
#[cfg(target_os = "linux")]
#[test]
fn test_terminal_tab_title_follows_foreground_process() {
    let mut harness = harness_or_return!(80, 24);

    // Opt into tmux-style auto-naming (the harness disables it by default for
    // deterministic `*Terminal N*` tabs elsewhere).
    let mut cfg = harness.editor().config().clone();
    cfg.editor.terminal_auto_title = true;
    harness.editor_mut().set_config(cfg);

    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    let buffer_id = harness.editor().active_buffer_id();
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .expect("active buffer should be a terminal");

    // Semantic wait: the shell becomes the pty's foreground process group
    // shortly after spawn. Drive renders until auto-naming resolves; the
    // bound only guards against a hang (cargo nextest times out externally).
    let mut expected = None;
    for _ in 0..2000 {
        if let Some(name) = harness
            .editor()
            .terminal_manager()
            .get(terminal_id)
            .and_then(|h| h.foreground_process_name())
        {
            expected = Some(name);
            harness.render().unwrap();
            break;
        }
        harness.render().unwrap();
    }
    let expected = expected.expect("foreground process name should resolve on Linux");

    // The tab bar (row 1) now shows the foreground command, not the default.
    let tab_bar = harness.get_tab_bar();
    assert!(
        tab_bar.contains(&expected),
        "tab bar {tab_bar:?} should contain foreground command {expected:?}"
    );
    assert!(
        !tab_bar.contains("*Terminal 0*"),
        "tab bar {tab_bar:?} should no longer show the default label"
    );
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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();
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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();

    // Get initial size
    let handle = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .unwrap();
    let (initial_cols, initial_rows) = handle.size();

    // Resize the terminal
    harness
        .editor_mut()
        .active_window_mut()
        .resize_terminal(buffer_id, 120, 40);

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
        .active_window_mut()
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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();
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

    // Get initial cursor position. Exiting terminal mode anchors the cursor
    // at the top of the just-exited visible screen (see
    // `sync_terminal_to_buffer`), so navigate downward into the content.
    let initial_pos = harness.editor().get_cursor_position(buffer_id);

    // Move cursor down
    harness
        .editor_mut()
        .handle_key(KeyCode::Down, KeyModifiers::NONE)
        .unwrap();

    let pos_after_down = harness.editor().get_cursor_position(buffer_id);

    // Cursor should have moved (arrow keys do buffer navigation, not PTY input,
    // once terminal mode is disabled).
    assert_ne!(
        initial_pos, pos_after_down,
        "Cursor should move when pressing Down in disabled terminal mode"
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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();
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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();
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
        .active_window_mut()
        .send_terminal_input(b"for i in $(seq 1 100); do echo \"Line $i\"; done\n");

    // Wait for the last line to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Line 100"))
        .unwrap();

    // Add a unique marker at the prompt that we can search for
    harness
        .editor_mut()
        .active_window_mut()
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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();
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

    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();
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

    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();

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
            harness
                .editor()
                .active_window()
                .is_terminal_buffer(active_buffer_before),
            "Terminal should be active buffer before save"
        );

        // Save session
        harness.editor_mut().save_workspace().unwrap();

        // Verify the session was captured with terminal as active
        let session = harness.editor().capture_workspace();
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
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should have been restored");
        harness.render().unwrap();

        // Check what buffer is active according to Editor's active_buffer field (for INPUT)
        let active_buffer_for_input = harness.editor().active_buffer_id();
        let input_is_terminal = harness
            .editor()
            .active_window()
            .is_terminal_buffer(active_buffer_for_input);

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
    harness.assert_screen_contains(">command");
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
        !harness.screen_to_string().contains(">command"),
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
    harness.assert_screen_contains(">command");
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
    assert!(harness
        .editor()
        .active_window()
        .is_terminal_buffer(terminal_buffer));

    // Use Alt+[ to switch to previous split (this should work in terminal mode
    // because it's a UI binding and keyboard capture is OFF)
    harness
        .send_key(KeyCode::Char('['), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Should have switched to the left split (non-terminal buffer)
    let new_buffer = harness.editor().active_buffer_id();
    assert!(
        !harness
            .editor()
            .active_window()
            .is_terminal_buffer(new_buffer),
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
    // The prompt shows ">command"
    harness.assert_screen_contains(">command");
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
    harness.assert_screen_contains(">command");

    // Type something in the prompt - this should go to the prompt, not the terminal
    harness.type_text("quit").unwrap();
    harness.render().unwrap();

    // The prompt should show what we typed
    harness.assert_screen_contains("quit");
}

/// Regression test for fresh#2595: when the same terminal is shown in two
/// splits, dropping the focused split into read-only scrollback must NOT stop
/// the other split from following live output. Live-vs-scrollback is a
/// per-split property, not a shared window flag.
///
/// Drives: open a terminal, generate scrollback, split it vertically (both
/// splits now show the same terminal), then in the focused split enter
/// scrollback (`Ctrl+Space`) and scroll to the top of history (`Ctrl+Home`).
/// New output emitted afterwards must still appear on screen — rendered by the
/// *other* (unfocused) split's live grid — while the focused split stays parked
/// at the top of history. Without the fix both splits freeze at the top and the
/// new output is never rendered, so the `wait_until` below hangs (external
/// timeout = failure).
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_split_terminal_scrollback_does_not_freeze_other_split() {
    let mut harness = harness_or_return!(120, 30);

    // Open a terminal in the sole split; it starts in terminal mode.
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Stay in scrollback even while the shell keeps producing output.
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Emit an early marker, then enough lines to push it off the live grid and
    // into streamed scrollback history.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"echo 'TOP_HISTORY_MARKER'\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("TOP_HISTORY_MARKER"))
        .unwrap();
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"for i in $(seq 1 40); do echo \"FILL $i\"; done\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("FILL 40"))
        .unwrap();

    // Split vertically: both splits now show the same terminal buffer, both live.
    harness.editor_mut().split_pane_vertical();
    harness.render().unwrap();

    // In the focused split, drop to read-only scrollback and jump to the very
    // top of history. `send_key` routes through the real input dispatch.
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(
        !harness.editor().is_terminal_mode(),
        "Ctrl+Space should drop the focused split into read-only scrollback"
    );
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The focused split is now parked at the top of history, showing the early
    // marker.
    assert!(
        harness.screen_to_string().contains("TOP_HISTORY_MARKER"),
        "Focused split should be scrolled to the top of history. Screen:\n{}",
        harness.screen_to_string()
    );

    // Emit fresh output AFTER the focused split parked at the top. The focused
    // split must not follow it (jump_to_end is off, and it's in scrollback), so
    // the only way this marker reaches the screen is the *other* split's live
    // grid still following output.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"echo 'LIVE_TAIL_MARKER'\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("LIVE_TAIL_MARKER"))
        .unwrap();

    // Independence proof: the top-of-history marker (focused split, frozen) and
    // the newest output (unfocused split, live) are visible at the same time —
    // impossible if the two splits shared one mode.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("TOP_HISTORY_MARKER") && screen.contains("LIVE_TAIL_MARKER"),
        "One split should show top-of-history while the other follows live output. Screen:\n{}",
        screen
    );
}

/// fresh#2595, part two: live-vs-scrollback is remembered PER SPLIT, so a split
/// keeps its scrollback even after it loses focus, while another split on the
/// same terminal streams live. Proves the mode is not a single focused-split
/// flag but genuine per-split state.
///
/// Drives: split a terminal in two, drop the focused split into scrollback at
/// the top of history, then move focus to the OTHER split (leaving the first
/// unfocused) and emit new output. The now-unfocused split must still be parked
/// at the top of history (retained scrollback) while the focused split follows
/// the live output — both visible at once.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_unfocused_split_retains_scrollback_independently() {
    let mut harness = harness_or_return!(120, 30);

    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Early marker, then enough lines to push it into streamed scrollback.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"echo 'EARLY_HISTORY_MARKER'\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("EARLY_HISTORY_MARKER"))
        .unwrap();
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"for i in $(seq 1 40); do echo \"PAD $i\"; done\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("PAD 40"))
        .unwrap();

    // Two splits on the same terminal. The new split is focused.
    harness.editor_mut().split_pane_vertical();
    harness.render().unwrap();

    // Drop the focused split into scrollback and park it at the top of history.
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("EARLY_HISTORY_MARKER"),
        "the split just dropped into scrollback should show the top of history"
    );

    // Move focus to the OTHER split. In the old single-flag model this would
    // also flip the first split back to live; per-split state must keep it in
    // scrollback. The newly focused split is live.
    harness.editor_mut().prev_split();
    harness.render().unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "the newly focused split is a live terminal"
    );

    // New output: only the focused (live) split can show it.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"echo 'AFTER_REFOCUS_MARKER'\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("AFTER_REFOCUS_MARKER"))
        .unwrap();

    // The now-UNFOCUSED split is still parked at the top of history (retained
    // its scrollback across the focus change) while the focused split follows
    // live output — both on screen simultaneously.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("EARLY_HISTORY_MARKER") && screen.contains("AFTER_REFOCUS_MARKER"),
        "an unfocused split must retain scrollback while another follows live output. Screen:\n{}",
        screen
    );
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
        harness
            .editor()
            .active_window()
            .is_terminal_buffer(terminal_buffer),
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
        !harness
            .editor()
            .active_window()
            .is_terminal_buffer(active_after_click),
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
        harness
            .editor()
            .active_window()
            .is_terminal_buffer(terminal_buffer),
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
                .active_window()
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
                .active_window()
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
                .active_window()
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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();
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
        .active_window_mut()
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
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .unwrap();
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
            .active_window_mut()
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
        harness.editor_mut().save_workspace().unwrap();

        // Get the backing file path for later verification
        let terminal_id = harness
            .editor()
            .active_window()
            .get_terminal_id(buffer_id)
            .unwrap();
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
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
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
        let is_terminal = harness
            .editor()
            .active_window()
            .is_terminal_buffer(buffer_id);

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
            .active_window_mut()
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
        harness.editor_mut().save_workspace().unwrap();

        // Get the backing file path for later verification
        let buffer_id = harness.editor().active_buffer_id();
        let terminal_id = harness
            .editor()
            .active_window()
            .get_terminal_id(buffer_id)
            .unwrap();
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
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
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
            .active_window_mut()
            .send_terminal_input(b"echo 'SECOND_SESSION_MARKER_BBB'\n");

        harness
            .wait_until(|h| h.screen_to_string().contains("SECOND_SESSION_MARKER_BBB"))
            .unwrap();

        // Generate more output to push SECOND_MARKER into scrollback
        harness
            .editor_mut()
            .active_window_mut()
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
        .active_window_mut()
        .send_terminal_input(b"echo 'START_MARKER_12345'\n");

    // Wait for the start marker
    harness
        .wait_until(|h| h.screen_to_string().contains("START_MARKER_12345"))
        .unwrap();

    // Generate many lines to push the start marker into scrollback
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"for i in $(seq 1 50); do echo \"Line $i of output\"; done\n");

    // Wait for the last line to appear (ensures command completed)
    harness
        .wait_until(|h| h.screen_to_string().contains("Line 50 of output"))
        .unwrap();

    // Add an end marker
    harness
        .editor_mut()
        .active_window_mut()
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
        .active_window_mut()
        .send_terminal_input(b"echo 'HISTORY_START_MARKER'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("HISTORY_START_MARKER"))
        .unwrap();

    // Generate many lines to push the start marker into scrollback
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"for i in $(seq 1 50); do echo \"History line $i\"; done\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("History line 50"))
        .unwrap();

    // Add an end marker that will be visible at the bottom
    harness
        .editor_mut()
        .active_window_mut()
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
        .active_window_mut()
        .send_terminal_input(b"echo 'HISTORY_START_MARKER'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("HISTORY_START_MARKER"))
        .unwrap();

    // Generate many lines to push the start marker into scrollback
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"for i in $(seq 1 50); do echo \"History line $i\"; done\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("History line 50"))
        .unwrap();

    // Add an end marker that will be visible at the bottom
    harness
        .editor_mut()
        .active_window_mut()
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
        .active_window_mut()
        .send_terminal_input(b"echo 'BEFORE_EXIT_MARKER'\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("BEFORE_EXIT_MARKER"))
        .unwrap();

    // Exit the terminal by typing 'exit'
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"exit\n");

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
        harness.editor().active_window().is_editing_disabled(),
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

/// Test Windows terminal shows prompt and responds to commands
///
/// This test verifies that the DSR (Device Status Report) cursor position
/// response is working correctly on Windows. PowerShell sends `\x1b[6n` and
/// waits for a response before showing the prompt. Without the response,
/// the terminal shows a black screen.
#[test]
#[cfg(windows)]
fn test_windows_terminal_shows_prompt_and_executes_command() {
    let mut harness = harness_or_return!(100, 30);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Wait for the shell prompt to appear
    // On Windows, this requires the DSR response to work correctly
    let prompt_result = harness.wait_until(|h| {
        let screen = h.screen_to_string();
        // PowerShell prompt contains "PS " and ">"
        // cmd.exe prompt contains ">"
        screen.contains("PS ") || screen.contains(">")
    });

    assert!(
        prompt_result.is_ok(),
        "Terminal should show a shell prompt (PS or >). This may fail if DSR response is not working.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Send a simple Windows command: echo with a unique marker
    let marker = "FRESH_TERMINAL_TEST_12345";
    let command = format!("echo {}\r\n", marker);
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(command.as_bytes());

    // Wait for the echo output to appear
    let output_result = harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains(marker)
    });

    assert!(
        output_result.is_ok(),
        "Terminal should show echo output containing '{}'. Screen:\n{}",
        marker,
        harness.screen_to_string()
    );

    // Verify the screen contains the expected output
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(marker),
        "Screen should contain the echo marker '{}'. Screen:\n{}",
        marker,
        screen
    );
}

/// Test that bracket paste (external paste / CrosstermEvent::Paste) sends text
/// to the terminal PTY when in terminal mode.
///
/// When the editor is in terminal mode with an open terminal, a bracketed paste
/// event should route the pasted text to the PTY (just like typing it), not
/// insert it into the terminal's backing buffer as editor text.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell commands (cat)
fn test_bracket_paste_in_terminal_mode() {
    let mut harness = harness_or_return!(80, 24);

    // Open a terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Should be in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Wait for shell prompt to be ready by sending a simple command first
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"echo SHELL_READY\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("SHELL_READY"))
        .unwrap();

    // Now start `cat` which echoes stdin back to stdout
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"cat\n");

    // Wait for cat to start (it consumes the echo of the command)
    harness
        .wait_until(|h| h.screen_to_string().contains("cat"))
        .unwrap();

    // Simulate a bracket paste event (CrosstermEvent::Paste) by calling paste_text
    // directly, which is what the event loop does for bracketed paste
    harness
        .editor_mut()
        .paste_text("BRACKET_PASTE_MARKER\n".to_string());

    // Wait for the pasted text to appear in terminal output (cat echoes it)
    harness
        .wait_until(|h| h.screen_to_string().contains("BRACKET_PASTE_MARKER"))
        .unwrap();

    // Verify the text is visible on screen (came through the PTY, not buffer insertion)
    harness.assert_screen_contains("BRACKET_PASTE_MARKER");

    // Should still be in terminal mode
    assert!(harness.editor().is_terminal_mode());

    // Clean up: send Ctrl+D to exit cat
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"\x04");
}

/// Regression test: when an alternate-screen program is *tracking the mouse*
/// (mouse reporting enabled), wheel events must be forwarded to it as real
/// mouse reports — never converted into arrow keys by alternate-scroll mode.
///
/// `alacritty_terminal` enables `ALTERNATE_SCROLL` by default, so without a
/// `wants_mouse` guard in `send_terminal_mouse` every forwarded wheel event in
/// the alternate screen would be rewritten as Up/Down arrows. For a full-screen
/// program that scrolls its own viewport from wheel reports — e.g. Claude Code's
/// "no-flicker" mode — that arrow translation instead leaks into its input and
/// cycles prompt/message history rather than scrolling the viewport.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell commands (printf/stty/cat)
fn test_wheel_forwarded_as_mouse_report_when_mouse_tracked() {
    let mut harness = harness_or_return!(80, 24);

    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Wait for the shell prompt to be ready.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"echo SHELL_READY\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("SHELL_READY"))
        .unwrap();

    // Enter the alternate screen, enable SGR (1006) + click (1000) mouse
    // reporting, print a readiness marker, then run `cat -v` on a raw, no-echo
    // tty so any bytes we forward are echoed back verbatim (ESC shown as `^[`).
    //
    // The marker is assembled from two `printf` args (`CAT` + `_READY`) so the
    // echoed *command line* contains "CAT _READY" (with a space) while the
    // program's *output* is "CAT_READY" — the wait below then matches only the
    // real output, never the command echo.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(
            b"printf '\\033[?1049h\\033[?1006h\\033[?1000h%s%s\\n' CAT _READY; stty raw -echo; cat -v\n",
        );

    // Entering the alternate screen hides the main-screen command echo, so the
    // only way "CAT_READY" appears on screen is as the program's output —
    // printed right after the alt-screen + mouse-mode set sequences. Seeing it
    // is an observable signal that the program is now on the alternate screen
    // and tracking the mouse (no model accessors needed).
    harness
        .wait_until(|h| h.screen_to_string().contains("CAT_READY"))
        .unwrap();

    // Scroll the wheel up over the terminal content area.
    harness.mouse_scroll_up(10, 10).unwrap();

    // Send a plain sentinel after the wheel event. The PTY preserves order, so
    // once `cat -v` has echoed the sentinel we know the wheel bytes were already
    // echoed too — letting us assert deterministically (and fail fast rather
    // than hang) regardless of whether they were a mouse report or arrow keys.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"ZZ_END");
    harness
        .wait_until(|h| h.screen_to_string().contains("ZZ_END"))
        .unwrap();

    // The program should have received an SGR wheel report (button 64), echoed
    // by `cat -v` as `^[[<64;...M`. It must NOT have received Up-arrow keys
    // (`^[[A`).
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("[<64;"),
        "wheel should be forwarded as an SGR mouse report when the program \
         tracks the mouse. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("[A"),
        "wheel must not be converted to Up-arrow keys while the program tracks \
         the mouse — alternate-scroll must be suppressed. Screen:\n{}",
        screen
    );

    // Clean up: Ctrl+D exits cat.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"\x04");
}

/// Test that arrow keys work in programs that enable application cursor keys (DECCKM).
/// Programs like `less` and `git log` set DECCKM mode, which means arrow keys
/// must be sent as SS3 sequences (\x1bOA) instead of CSI (\x1b[A).
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_arrow_keys_in_less() {
    use std::time::{Duration, Instant};

    crate::common::tracing::init_tracing_from_env();

    let mut harness = harness_or_return!(80, 24);

    /// Helper: wait for `condition` with periodic screen dumps and a hard timeout.
    /// Panics with full screen contents if the timeout is reached.
    fn wait_until_with_logging(
        harness: &mut EditorTestHarness,
        label: &str,
        timeout: Duration,
        mut condition: impl FnMut(&EditorTestHarness) -> bool,
    ) {
        let start = Instant::now();
        let mut iter: u32 = 0;
        let wait_sleep = Duration::from_millis(50);

        tracing::info!("[arrow_keys] waiting: {}", label);
        eprintln!("[arrow_keys] waiting: {}", label);

        loop {
            harness.process_async_and_render().unwrap();
            if condition(harness) {
                let elapsed = start.elapsed();
                tracing::info!(
                    "[arrow_keys] ✓ {} — done after {:.1}s ({} iters)",
                    label,
                    elapsed.as_secs_f64(),
                    iter
                );
                eprintln!(
                    "[arrow_keys] ✓ {} — done after {:.1}s ({} iters)",
                    label,
                    elapsed.as_secs_f64(),
                    iter
                );
                return;
            }

            // Periodic progress logging (every ~5s)
            if iter % 100 == 0 && iter > 0 {
                let screen = harness.screen_to_string();
                let elapsed = start.elapsed();
                tracing::info!(
                    "[arrow_keys] still waiting: {} ({:.1}s)\n--- screen ---\n{}\n--- end screen ---",
                    label,
                    elapsed.as_secs_f64(),
                    screen
                );
                eprintln!(
                    "[arrow_keys] still waiting: {} ({:.1}s)\n--- screen ---\n{}\n--- end screen ---",
                    label,
                    elapsed.as_secs_f64(),
                    screen
                );
            }

            if start.elapsed() > timeout {
                let screen = harness.screen_to_string();
                tracing::error!(
                    "[arrow_keys] TIMEOUT waiting: {} after {:.1}s\n--- screen ---\n{}\n--- end screen ---",
                    label,
                    start.elapsed().as_secs_f64(),
                    screen
                );
                eprintln!(
                    "[arrow_keys] TIMEOUT waiting: {} after {:.1}s\n--- screen ---\n{}\n--- end screen ---",
                    label,
                    start.elapsed().as_secs_f64(),
                    screen
                );
                panic!(
                    "[arrow_keys] TIMEOUT after {:.1}s waiting for: {}\nScreen:\n{}",
                    start.elapsed().as_secs_f64(),
                    label,
                    screen
                );
            }

            std::thread::sleep(wait_sleep);
            harness.advance_time(wait_sleep);
            iter += 1;
        }
    }

    let timeout = Duration::from_secs(60);

    // Create a numbered file in an isolated temp directory
    let tmp = tempfile::TempDir::new().unwrap();
    let test_file = tmp.path().join("less_arrows.txt");
    let content: String = (1..=100)
        .map(|i| format!("TLINE_{}", i))
        .collect::<Vec<_>>()
        .join("\n");
    std::fs::write(&test_file, &content).unwrap();

    harness.editor_mut().open_terminal();
    eprintln!("[arrow_keys] terminal opened");

    // Open the file in less (this enters alternate screen and enables DECCKM)
    let less_cmd = format!("less {}\n", test_file.display());
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(less_cmd.as_bytes());
    eprintln!("[arrow_keys] sent less command: {}", less_cmd.trim());

    // Wait for less to show the file content
    wait_until_with_logging(&mut harness, "less shows TLINE_1", timeout, |h| {
        h.screen_to_string().contains("TLINE_1")
    });

    // Verify we see the first few lines
    harness.assert_screen_contains("TLINE_2");
    harness.assert_screen_contains("TLINE_3");

    // Verify we're NOT seeing lines near the end yet
    harness.assert_screen_not_contains("TLINE_100");
    eprintln!("[arrow_keys] initial screen verified, sending Down arrows");

    // Press Down arrow multiple times to scroll down.
    // In less with DECCKM, this requires SS3 sequences to work.
    // Send in batches with async processing to let less keep up under load.
    for batch in 0..4 {
        for _ in 0..10 {
            harness
                .editor_mut()
                .handle_key(KeyCode::Down, KeyModifiers::NONE)
                .unwrap();
        }
        harness.process_async_and_render().unwrap();
        eprintln!("[arrow_keys] Down batch {}/4 sent", batch + 1);
    }

    // After scrolling down 40 lines, line 41 should be visible
    wait_until_with_logging(
        &mut harness,
        "TLINE_41 visible after 40x Down",
        timeout,
        |h| h.screen_to_string().contains("TLINE_41"),
    );

    // The first line should no longer be visible
    harness.assert_screen_not_contains("TLINE_1");
    eprintln!("[arrow_keys] Down scroll verified, sending Up arrows");

    // Now press Up arrow to scroll back up
    for _ in 0..10 {
        harness
            .editor_mut()
            .handle_key(KeyCode::Up, KeyModifiers::NONE)
            .unwrap();
    }

    // After scrolling up 10 lines, line 31 should be visible
    wait_until_with_logging(
        &mut harness,
        "TLINE_31 visible after 10x Up",
        timeout,
        |h| h.screen_to_string().contains("TLINE_31"),
    );

    eprintln!("[arrow_keys] Up scroll verified, exiting less");

    // Exit less with 'q'
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();

    eprintln!("[arrow_keys] test complete");
}

/// Regression test for issue #1637: `terminal.shell` config overrides the
/// shell command used by the integrated terminal without having to
/// change `$SHELL`. Picks a command that is definitely not the user's
/// login shell — `/bin/cat` — and confirms the spawned terminal handle
/// reports it back.
#[test]
fn test_terminal_shell_config_override() {
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

    let override_cmd = "/bin/cat";
    if !std::path::Path::new(override_cmd).exists() {
        eprintln!("Skipping terminal test: {} not available", override_cmd);
        return;
    }

    let mut config = Config::default();
    config.terminal.shell = Some(TerminalShellConfig {
        command: override_cmd.to_string(),
        args: Vec::new(),
    });

    let mut harness = match EditorTestHarness::with_config(80, 24, config) {
        Ok(h) => h,
        Err(_) => return,
    };

    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    let terminal_buffer = harness.editor().active_buffer_id();
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(terminal_buffer)
        .expect("terminal buffer should have a terminal id");
    let shell = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle should exist")
        .shell()
        .to_string();

    assert_eq!(
        shell, override_cmd,
        "terminal should spawn with the config-overridden shell"
    );
}

/// Regression: a terminal hidden behind another tab during a window resize
/// should pick up the new dimensions when the user switches back to it,
/// rather than keeping the stale pre-resize PTY size.  Issue #1795.
#[test]
fn test_hidden_terminal_resyncs_pty_size_when_revealed() {
    let mut harness = harness_or_return!(120, 35);

    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    let terminal_buffer = harness.editor().active_buffer_id();
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(terminal_buffer)
        .expect("terminal buffer should have a terminal id");
    let (cols_before, rows_before) = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle should exist")
        .size();

    // Move the terminal off-screen by switching to a fresh empty buffer in the
    // same split.  The terminal is now hidden behind the new tab.
    harness.new_buffer().unwrap();
    let other_buffer = harness.editor().active_buffer_id();
    assert_ne!(other_buffer, terminal_buffer);
    assert!(!harness
        .editor()
        .active_window()
        .is_terminal_buffer(other_buffer));

    // Shrink the host terminal while the PTY is hidden.  Without the fix,
    // `resize_visible_terminals` skips the hidden buffer and the PTY keeps
    // its original geometry.
    harness.resize(80, 25).unwrap();

    // Sanity: the PTY child still reports the original (stale) dimensions.
    let (cols_hidden, rows_hidden) = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle should exist")
        .size();
    assert_eq!(
        (cols_hidden, rows_hidden),
        (cols_before, rows_before),
        "hidden terminal should not have been resized while off-screen"
    );

    // Bring the terminal tab back to the front; the PTY size should now
    // reflect the smaller window.
    harness.editor_mut().switch_buffer(terminal_buffer);
    harness.render().unwrap();

    let (cols_after, rows_after) = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle should exist")
        .size();

    assert!(
        cols_after < cols_before,
        "expected PTY cols to shrink after reveal: before={}, after={}",
        cols_before,
        cols_after
    );
    assert!(
        rows_after < rows_before,
        "expected PTY rows to shrink after reveal: before={}, after={}",
        rows_before,
        rows_after
    );
}

// --- Send selection to terminal (issue #1871) ---------------------------

/// Write directly to a terminal by id, bypassing `active_buffer()`
/// routing so it works while focus is on a non-terminal split.
fn write_to_terminal_by_buffer(
    harness: &EditorTestHarness,
    terminal_buffer: fresh::model::event::BufferId,
    bytes: &[u8],
) {
    let terminal_id = harness
        .editor()
        .active_window()
        .get_terminal_id(terminal_buffer)
        .expect("terminal id");
    harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle")
        .write(bytes);
}

/// Set up a vertical split with the current text buffer in one pane and a
/// live shell in the other, then move focus back to the text buffer.
/// Returns the terminal's buffer id.
fn setup_text_buffer_beside_terminal(
    harness: &mut EditorTestHarness,
) -> fresh::model::event::BufferId {
    harness.editor_mut().split_pane_vertical();
    harness.render().unwrap();

    harness.editor_mut().next_split();
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    let terminal_buffer = harness.editor().active_buffer_id();

    // Wait for the shell prompt to settle so later writes reach a live shell.
    write_to_terminal_by_buffer(harness, terminal_buffer, b"echo FRESH_READY\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("FRESH_READY"))
        .unwrap();

    // Move focus back to the text buffer split.
    harness.editor_mut().next_split();
    harness.render().unwrap();
    assert!(!harness
        .editor()
        .active_window()
        .is_terminal_buffer(harness.editor().active_buffer_id()));

    terminal_buffer
}

/// Selecting text and running "Send Selection to Terminal" from the
/// command palette executes the selection in the visible terminal split.
/// The buffer holds `echo SEL_$((6*7))_DONE`, so the expanded output
/// `SEL_42_DONE` can only come from the shell actually running the text.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell syntax
fn test_send_selection_to_terminal_runs_selection() {
    let mut harness = harness_or_return!(120, 30);
    harness
        .load_buffer_from_text("echo SEL_$((6*7))_DONE\nsecond line\n")
        .unwrap();

    let terminal_buffer = setup_text_buffer_beside_terminal(&mut harness);

    // Select the first line (cursor starts at offset 0).
    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Run the command through the palette, the way a user would.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(">command");
    harness.type_text("Send Selection to Terminal").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The shell expands $((6*7)) — output only the terminal can produce.
    harness
        .wait_until(|h| h.screen_to_string().contains("SEL_42_DONE"))
        .unwrap();

    // The send also moves focus into the terminal's split, in terminal
    // mode, so the user can keep typing at the prompt.
    assert_eq!(harness.editor().active_buffer_id(), terminal_buffer);
    assert!(harness.editor().is_terminal_mode());

    // Keystrokes now reach the PTY: type another command and see it run.
    harness.type_text("echo FOCUS_$((5*5))_OK").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("FOCUS_25_OK"))
        .unwrap();
}

/// With no selection, the command sends the cursor's current line. The
/// cursor sits on line 1; line 2 must not run.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell syntax
fn test_send_selection_to_terminal_sends_current_line_when_no_selection() {
    use fresh::test_api::Action;

    let mut harness = harness_or_return!(120, 30);
    harness
        .load_buffer_from_text("echo LINE_$((7*7))_RUN\necho OTHER_$((8*8))_LINE\n")
        .unwrap();

    let terminal_buffer = setup_text_buffer_beside_terminal(&mut harness);

    harness.api_mut().dispatch(Action::SendSelectionToTerminal);
    harness
        .wait_until(|h| h.screen_to_string().contains("LINE_49_RUN"))
        .unwrap();

    // Only the cursor's line ran — the second line stayed in the buffer.
    harness.assert_screen_not_contains("OTHER_64_LINE");

    // Focus followed the send into the terminal, in terminal mode.
    assert_eq!(harness.editor().active_buffer_id(), terminal_buffer);
    assert!(harness.editor().is_terminal_mode());
}

/// Without any open terminal the command reports it in the status bar
/// instead of failing silently.
///
/// Runs on the unnamed startup buffer on purpose: a fixture file's
/// temp-dir path (long on macOS and Windows) crowds the Messages
/// element out of the 120-col status bar, hiding the very message
/// under test. No PTY is needed since no terminal is ever opened.
#[test]
fn test_send_selection_to_terminal_without_terminal_shows_status() {
    use fresh::test_api::Action;

    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::SendSelectionToTerminal);
    harness.render().unwrap();

    harness.assert_screen_contains("No open terminal");
}

/// A terminal living in a background tab of the same split (not visible
/// in any pane) is still found via the newest-terminal fallback: the
/// send brings its tab to the front in terminal mode and the executed
/// line's output is on the live screen.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell syntax
fn test_send_selection_to_terminal_reaches_background_tab_terminal() {
    use fresh::test_api::Action;

    let mut harness = harness_or_return!(120, 30);
    harness
        .load_buffer_from_text("echo TAB_$((3*3))_FALLBACK\n")
        .unwrap();
    let text_buffer = harness.editor().active_buffer_id();

    // Open a terminal as a sibling tab in the same split.
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    let terminal_buffer = harness.editor().active_buffer_id();
    write_to_terminal_by_buffer(&harness, terminal_buffer, b"echo FRESH_READY\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("FRESH_READY"))
        .unwrap();

    // Leave terminal mode and bring the text buffer's tab to the front,
    // hiding the terminal in a background tab.
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(' '), KeyModifiers::CONTROL);
    harness.editor_mut().switch_buffer(text_buffer);
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::SendSelectionToTerminal);

    // The send brings the terminal's tab back to the front in terminal
    // mode on its own — no manual reveal — so the live screen shows the
    // executed line's output.
    harness
        .wait_until(|h| h.screen_to_string().contains("TAB_9_FALLBACK"))
        .unwrap();
    assert_eq!(harness.editor().active_buffer_id(), terminal_buffer);
    assert!(harness.editor().is_terminal_mode());
}

// --- Terminal-mode scrollbar visibility ---------------------------------

/// In terminal mode the live PTY grid hides the vertical scrollbar and
/// reclaims that column, so the terminal text uses the full split width.
/// Exiting terminal mode (into the read-only scrollback view) brings the
/// scrollbar back, and the content area shrinks by exactly that one column.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_mode_hides_scrollbar_and_reclaims_width() {
    let mut harness = harness_or_return!(80, 24);

    // Opening a terminal enters terminal mode (live PTY grid).
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    let terminal_buffer = harness.editor().active_buffer_id();

    // While in terminal mode the split shows no scrollbar column.
    let (live_content_width, live_scrollbar_width) = {
        let (_, _, content_rect, scrollbar_rect, _, _) = harness
            .editor()
            .get_split_areas()
            .iter()
            .find(|(_, buf, _, _, _, _)| *buf == terminal_buffer)
            .copied()
            .expect("terminal split should be present");
        (content_rect.width, scrollbar_rect.width)
    };
    assert_eq!(
        live_scrollbar_width, 0,
        "scrollbar column should be suppressed in terminal mode"
    );

    // The live PTY grid should span that reclaimed column too.
    let live_pty_cols = {
        let terminal_id = harness
            .editor()
            .active_window()
            .get_terminal_id(terminal_buffer)
            .unwrap();
        harness
            .editor()
            .terminal_manager()
            .get(terminal_id)
            .unwrap()
            .size()
            .0
    };

    // Exit terminal mode: the buffer flips to the read-only scrollback view,
    // which restores the scrollbar.
    harness
        .editor_mut()
        .handle_terminal_key(KeyCode::Char(']'), KeyModifiers::CONTROL);
    assert!(!harness.editor().is_terminal_mode());
    harness.render().unwrap();

    let (scrollback_content_width, scrollback_scrollbar_width) = {
        let (_, _, content_rect, scrollbar_rect, _, _) = harness
            .editor()
            .get_split_areas()
            .iter()
            .find(|(_, buf, _, _, _, _)| *buf == terminal_buffer)
            .copied()
            .expect("terminal split should be present");
        (content_rect.width, scrollbar_rect.width)
    };
    assert_eq!(
        scrollback_scrollbar_width, 1,
        "scrollbar column should reappear after exiting terminal mode"
    );
    assert_eq!(
        live_content_width,
        scrollback_content_width + 1,
        "terminal-mode content should reclaim the scrollbar's column"
    );

    // The live PTY was sized to the wider, scrollbar-free content area: one
    // column wider than the scrollback view reserves for its scrollbar.
    assert_eq!(
        live_pty_cols, scrollback_content_width,
        "live PTY width should span the scrollbar column the scrollback view reserves"
    );
}

/// Terminal buffers must never line-wrap, even with global line wrap enabled.
/// Wrapping a large terminal scrollback turns the scrollbar's visual-row index
/// into an O(all-lines) scan on every frame, freezing the UI (fresh#2608).
#[test]
fn test_terminal_buffers_never_line_wrap() {
    let mut harness = harness_or_return!(80, 24);
    harness.editor_mut().open_terminal();
    let term_id = harness.editor().active_buffer();

    // Global line wrap on: a regular buffer would wrap; a terminal must not.
    harness.editor_mut().config_mut().editor.line_wrap = true;

    assert!(
        !harness
            .editor()
            .active_window()
            .resolve_line_wrap_for_buffer(term_id),
        "terminal buffers must never resolve to line-wrap even when global line wrap is on"
    );
}

// --- Drag-to-select exit conditions (implicit scrollback) -----------------
//
// A drag on the live grid parks the split in *implicit* scrollback so the
// selection can exist (the grid has no selection model). Implicit scrollback
// ends automatically: copying the selection or a bare click resumes the live
// grid, while engaging with the scrollback as a view (scrolling) converts
// the visit to an explicit one that only ends by the explicit rules.

/// Locate the top-left screen cell of the row that contains `needle`.
fn screen_pos_of(harness: &EditorTestHarness, needle: &str) -> Option<(u16, u16)> {
    harness
        .screen_to_string()
        .lines()
        .enumerate()
        .find_map(|(row, line)| {
            line.find(needle)
                .map(|byte| (line[..byte].chars().count() as u16, row as u16))
        })
}

/// Whether the focused split's primary cursor carries an active selection.
fn primary_selection_active(harness: &EditorTestHarness) -> bool {
    let win = harness.editor().active_window();
    let Some((mgr, view_states)) = win.buffers.splits() else {
        return false;
    };
    view_states
        .get(&mgr.active_split())
        .map(|vs| {
            let c = vs.cursors.primary();
            c.anchor.is_some_and(|a| a != c.position)
        })
        .unwrap_or(false)
}

/// Drag-select from `(from_col, row)` to `(to_col, row)` with a render after
/// every event, like the interactive event loop (each event redraws). The
/// harness's `mouse_drag` sends the whole gesture without intermediate
/// renders, so drag steps after the live-grid→scrollback flip would resolve
/// against the pre-flip cached view-line mappings.
fn drag_select_row(
    harness: &mut EditorTestHarness,
    from_col: u16,
    to_col: u16,
    row: u16,
) -> anyhow::Result<()> {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
    let ev = |kind, column| MouseEvent {
        kind,
        column,
        row,
        modifiers: KeyModifiers::empty(),
    };
    harness.send_mouse(ev(MouseEventKind::Down(MouseButton::Left), from_col))?;
    harness.render()?;
    harness.send_mouse(ev(MouseEventKind::Drag(MouseButton::Left), to_col))?;
    harness.render()?;
    harness.send_mouse(ev(MouseEventKind::Up(MouseButton::Left), to_col))?;
    harness.render()?;
    Ok(())
}

/// Open a terminal, print `output` on its own row (the shell quoting keeps
/// the echoed *command* from containing the marker), and return the marker
/// row's screen position.
fn terminal_with_marker(harness: &mut EditorTestHarness, output: &str) -> (u16, u16) {
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    assert!(harness.editor().is_terminal_mode());

    // Type `echo 'X'MARKER` so the typed command line on screen never
    // contains the bare marker; only the output row does.
    let (head, tail) = output.split_at(1);
    let cmd = format!("echo '{}'{}\n", head, tail);
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(cmd.as_bytes());
    harness
        .wait_until(|h| screen_pos_of(h, output).is_some())
        .unwrap();
    screen_pos_of(harness, output).unwrap()
}

/// Copying a drag selection completes the gesture: the split must resume the
/// live grid on Ctrl+C, without waiting for new output or a manual
/// Ctrl+Space (the "stuck in scrollback after copy" complaint).
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_drag_select_copy_resumes_live() {
    let mut harness = harness_or_return!(120, 30);
    harness.editor_mut().set_clipboard_for_test(String::new());
    let (col, row) = terminal_with_marker(&mut harness, "XSELECT_COPY_ME");

    drag_select_row(&mut harness, col, col + 10, row).unwrap();
    assert!(
        !harness.editor().is_terminal_mode(),
        "drag on the live grid should park the split in read-only scrollback"
    );
    assert!(
        primary_selection_active(&harness),
        "drag should build a real selection over the terminal text"
    );

    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    let clip = harness.editor_mut().clipboard_content_for_test();
    assert!(
        clip.contains("XSELECT_CO"),
        "Ctrl+C should copy the dragged terminal text, got {clip:?}"
    );
    assert!(
        harness.editor().is_terminal_mode(),
        "copying the selection should resume the live terminal"
    );
}

/// A bare click on a drag-parked scrollback pane abandons the selection and
/// resumes the live grid immediately — no new output required.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_drag_select_click_away_resumes_live() {
    let mut harness = harness_or_return!(120, 30);
    let (col, row) = terminal_with_marker(&mut harness, "XSELECT_CLICK_AWAY");

    drag_select_row(&mut harness, col, col + 8, row).unwrap();
    assert!(!harness.editor().is_terminal_mode());

    harness.mouse_click(col + 20, row).unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "a bare click should abandon the drag selection and resume the live terminal"
    );
}

/// Scrolling the drag-parked scrollback converts the visit to an explicit
/// one: the user is reading history now, so neither copy nor click may yank
/// the view back to the live grid.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_drag_select_scroll_converts_to_explicit() {
    let mut harness = harness_or_return!(120, 30);
    harness.editor_mut().set_clipboard_for_test(String::new());
    let (col, row) = terminal_with_marker(&mut harness, "XSELECT_THEN_SCROLL");

    drag_select_row(&mut harness, col, col + 8, row).unwrap();
    assert!(!harness.editor().is_terminal_mode());

    harness.mouse_scroll_up(col + 2, row).unwrap();

    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        harness
            .editor_mut()
            .clipboard_content_for_test()
            .contains("XSELECT_"),
        "copy should still work after scrolling"
    );
    assert!(
        !harness.editor().is_terminal_mode(),
        "after scrolling, the visit is explicit: copy must not yank back to the live grid"
    );

    harness.mouse_click(col + 20, row).unwrap();
    assert!(
        !harness.editor().is_terminal_mode(),
        "after scrolling, a click places the cursor without resuming the live grid"
    );
}

/// An explicit scrollback visit (Ctrl+Space) keeps the manual model: a
/// selection copied there must NOT resume the live grid.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_explicit_scrollback_copy_does_not_resume() {
    let mut harness = harness_or_return!(120, 30);
    harness.editor_mut().set_clipboard_for_test(String::new());
    let (col, row) = terminal_with_marker(&mut harness, "XSELECT_EXPLICIT");

    // Let trailing shell output (prompt redraw) drain first: with no
    // selection pinning the view, `jump_to_end_on_output` would legitimately
    // resume the split right back out of the manual scrollback below.
    harness.wait_for_async_quiescence(3).unwrap();
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().is_terminal_mode());

    // The pinned scrollback view is pixel-identical to the grid: the marker
    // is still at the same screen position. Select part of it.
    drag_select_row(&mut harness, col, col + 8, row).unwrap();
    assert!(primary_selection_active(&harness));

    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        harness
            .editor_mut()
            .clipboard_content_for_test()
            .contains("XSELECT_"),
        "copy should work in explicit scrollback"
    );
    assert!(
        !harness.editor().is_terminal_mode(),
        "explicit scrollback must not be resumed by a copy"
    );
}

/// Regression: the drag's selection anchor must not outlive the visit. It
/// used to survive the resume, re-materialize as a phantom selection on the
/// next scrollback entry, and permanently suppress the output-driven
/// auto-resume in `handle_terminal_output`.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_no_phantom_selection_after_drag_copy_resume() {
    let mut harness = harness_or_return!(120, 30);
    harness.editor_mut().set_clipboard_for_test(String::new());
    let (col, row) = terminal_with_marker(&mut harness, "XSELECT_PHANTOM");

    // Drag + copy (resumes live), then re-enter scrollback manually.
    drag_select_row(&mut harness, col, col + 8, row).unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(harness.editor().is_terminal_mode());
    // Drain trailing shell output before the manual scrollback entry: an
    // output byte landing right after Ctrl+Space would auto-resume the split
    // (with no selection, that is now the intended behavior).
    harness.wait_for_async_quiescence(3).unwrap();
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    assert!(!harness.editor().is_terminal_mode());

    assert!(
        !primary_selection_active(&harness),
        "no phantom selection may survive into a fresh scrollback entry"
    );

    // With no selection pinning the view, new output must auto-resume the
    // terminal (jump_to_end_on_output defaults to true).
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"echo 'B'ACK_ALIVE\n");
    harness
        .wait_until(|h| h.editor().is_terminal_mode())
        .unwrap();
}

/// Double-click on the live grid selects the word under the pointer (via the
/// same implicit-scrollback detour as a drag); copying it resumes the grid.
#[test]
#[cfg(not(windows))] // Uses Unix shell
fn test_terminal_double_click_selects_word_and_copy_resumes() {
    let mut harness = harness_or_return!(120, 30);
    harness.editor_mut().set_clipboard_for_test(String::new());
    let (col, row) = terminal_with_marker(&mut harness, "XWORDSEL_TARGET");

    // Two clicks at the same cell within the double-click window.
    harness.mouse_click(col + 3, row).unwrap();
    assert!(
        harness.editor().is_terminal_mode(),
        "the first (bare) click must keep the terminal live"
    );
    harness.mouse_click(col + 3, row).unwrap();

    assert!(
        !harness.editor().is_terminal_mode(),
        "double-click should park the split in scrollback with the word selected"
    );
    assert!(
        primary_selection_active(&harness),
        "double-click should select the word under the pointer"
    );

    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(
        harness.editor_mut().clipboard_content_for_test(),
        "XWORDSEL_TARGET",
        "double-click + copy should yield exactly the word under the pointer"
    );
    assert!(
        harness.editor().is_terminal_mode(),
        "copying the double-click selection should resume the live terminal"
    );
}
