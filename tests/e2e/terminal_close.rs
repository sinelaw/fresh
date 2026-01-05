//! E2E tests for terminal close buffer behavior
//!
//! Tests that closing a terminal buffer properly handles focus,
//! cursor visibility, and keyboard input after close.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
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

/// Helper to run a command via command palette
fn run_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(command).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test: Open split, open terminal in split, close all other tabs, then close buffer
///
/// When terminal is the only tab in a split and we close it:
/// - Focus should go to the other split's buffer
/// - terminal_mode should be OFF
/// - Keyboard input should work in the new buffer
#[test]
fn test_close_terminal_as_only_tab_in_split() {
    let mut harness = harness_or_return!(120, 30);

    // Create a vertical split - now we have two splits with [No Name]
    run_command(&mut harness, "split vert");

    // Disable jump_to_end_on_output so terminal output doesn't interfere
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Open terminal in the current (right) split
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    // Exit terminal mode to use command palette
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Close the [No Name] buffer in this split so terminal is the only tab
    // First switch to it
    run_command(&mut harness, "prev buffer");
    // Then close it
    run_command(&mut harness, "close tab");

    // Now terminal is the only tab in the right split
    // The left split has [No Name]
    harness.assert_screen_contains("Terminal");

    // Close the terminal buffer
    run_command(&mut harness, "close buffer");

    // After closing:
    // 1. terminal_mode should be OFF
    assert!(
        !harness.editor().is_terminal_mode(),
        "terminal_mode should be OFF after closing terminal buffer"
    );

    // 2. Active buffer should NOT be a terminal
    let active_buffer = harness.editor().active_buffer_id();
    assert!(
        !harness.editor().is_terminal_buffer(active_buffer),
        "Active buffer should NOT be a terminal after close"
    );

    // 3. Keyboard input should work
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    let content = harness
        .editor()
        .get_buffer_content(active_buffer)
        .unwrap_or_default();

    assert!(
        content.contains("hello"),
        "Should be able to type after closing terminal. Buffer content: {:?}",
        content
    );

    // 4. Terminal should be gone from screen
    harness.assert_screen_not_contains("Terminal");
}

/// Test: Closing a buffer that's open in multiple splits leaves it in other splits
///
/// When a buffer is open in split A and split B, closing it in split A
/// should leave it still visible in split B.
#[test]
fn test_close_buffer_in_one_split_leaves_other_split() {
    let mut harness = harness_or_return!(120, 30);

    // Disable jump_to_end_on_output
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Open terminal in the initial split
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("Terminal 0");

    let terminal_buffer = harness.editor().active_buffer_id();

    // Exit terminal mode
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Create a vertical split - the new split will also show the terminal
    run_command(&mut harness, "split vert");

    // Both splits should show Terminal 0
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    let terminal_count = screen.matches("Terminal 0").count();
    assert!(
        terminal_count >= 2,
        "Terminal should appear in both splits, found {} occurrences. Screen:\n{}",
        terminal_count,
        screen
    );

    // Now close the terminal in the current split using close_tab
    // This should remove it from this split but leave it in the other split
    run_command(&mut harness, "close tab");

    harness.render().unwrap();

    // The terminal should still be visible in the left split
    harness.assert_screen_contains("Terminal 0");

    // The terminal buffer should still exist in the editor
    assert!(
        harness.editor().is_terminal_buffer(terminal_buffer),
        "Terminal buffer should still exist after closing tab in one split"
    );
}

/// Test: Closing a different buffer resumes terminal mode correctly
///
/// Bug reproduction: When a terminal is open in a tab and you close a different buffer,
/// the terminal should resume working correctly - content should be visible and
/// terminal mode should resume so you can type commands.
///
/// Steps:
/// 1. Create terminal, type "echo MARKER1"
/// 2. Open new file buffer (without exiting terminal mode)
/// 3. Close the new file buffer
/// 4. "MARKER1" should be visible in the terminal
/// 5. Typing "echo MARKER2" should work and "MARKER2" should be visible
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell commands
fn test_closing_other_buffer_resumes_terminal_correctly() {
    let mut harness = harness_or_return!(120, 30);

    // Open terminal
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // Close the initial [No Name] buffer so terminal is the only buffer
    // This matches the user scenario: terminal is open, then open file, close file
    run_command(&mut harness, "prev buffer");
    run_command(&mut harness, "close buffer");
    // Switch back to terminal
    run_command(&mut harness, "next buffer");
    harness.render().unwrap();

    // Type a command with a unique marker
    harness
        .editor_mut()
        .send_terminal_input(b"echo UNIQUEMARKER_ABC_123\n");

    // Wait for the output to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("UNIQUEMARKER_ABC_123"))
        .expect("Should see MARKER1 output in terminal");

    // Verify we're in terminal mode
    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode"
    );

    let terminal_buffer = harness.editor().active_buffer_id();

    // Open a new file (creates a new buffer) - this implicitly exits terminal mode
    // and adds the terminal to terminal_mode_resume
    let temp_file = tempfile::NamedTempFile::new().unwrap();
    std::fs::write(temp_file.path(), "test content\n").unwrap();
    harness.editor_mut().open_file(temp_file.path()).unwrap();
    harness.render().unwrap();

    let file_buffer = harness.editor().active_buffer_id();
    assert_ne!(
        file_buffer, terminal_buffer,
        "Should be on a different buffer after opening file"
    );

    // Terminal mode should be off now
    assert!(
        !harness.editor().is_terminal_mode(),
        "Should NOT be in terminal mode after opening file"
    );

    // Close the file buffer - should automatically switch back to terminal
    // AND resume terminal mode (since terminal was in terminal_mode_resume)
    run_command(&mut harness, "Close Buffer");
    harness.render().unwrap();

    // After closing, we should automatically be on the terminal buffer
    let active_after_close = harness.editor().active_buffer_id();
    assert_eq!(
        active_after_close, terminal_buffer,
        "Should automatically switch to terminal buffer after closing file. \
         Got buffer {:?} instead of terminal {:?}",
        active_after_close, terminal_buffer
    );

    // Terminal mode should have resumed automatically because the terminal
    // was added to terminal_mode_resume when we opened the file.
    assert!(
        harness.editor().is_terminal_mode(),
        "Terminal mode should automatically resume after closing file buffer \
         and switching back to terminal."
    );

    // The original marker should still be visible (requires terminal content sync)
    harness.assert_screen_contains("UNIQUEMARKER_ABC_123");

    // Type a second command to verify terminal is fully working
    harness
        .editor_mut()
        .send_terminal_input(b"echo SECONDMARKER_XYZ_789\n");

    // Wait for the second output
    harness
        .wait_until(|h| h.screen_to_string().contains("SECONDMARKER_XYZ_789"))
        .expect("Should see MARKER2 output after typing in resumed terminal");

    // Both markers should be visible
    harness.assert_screen_contains("UNIQUEMARKER_ABC_123");
    harness.assert_screen_contains("SECONDMARKER_XYZ_789");
}

/// Test: Closed terminal should not reappear after session restore
///
/// Bug reproduction: When a terminal is closed before quitting, then session is
/// restored, closing a file should not bring back the closed terminal.
///
/// Steps:
/// 1. Open file and terminal
/// 2. Close the terminal
/// 3. Save session and quit
/// 4. Start new editor, restore session
/// 5. Close the file
/// 6. Verify closed terminal does NOT become focused
#[test]
fn test_closed_terminal_not_restored_from_session() {
    use fresh::config::Config;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file = project_dir.join("test.txt");
    std::fs::write(&file, "Test file content").unwrap();

    // First session: open file, open terminal, close terminal, save session
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            30,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Open the file
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("test.txt");

        // Open terminal
        harness.editor_mut().open_terminal();
        harness.render().unwrap();
        harness.assert_screen_contains("Terminal 0");

        let terminal_buffer = harness.editor().active_buffer_id();
        assert!(
            harness.editor().is_terminal_buffer(terminal_buffer),
            "Should be on terminal buffer"
        );

        // Close the terminal
        run_command(&mut harness, "close buffer");
        harness.render().unwrap();

        // Terminal should be gone
        harness.assert_screen_not_contains("Terminal");

        // We should be on the file now
        harness.assert_screen_contains("test.txt");
        let active_after_close = harness.editor().active_buffer_id();
        assert!(
            !harness.editor().is_terminal_buffer(active_after_close),
            "Should NOT be on terminal buffer after closing it"
        );

        // Save session
        harness.editor_mut().save_session().unwrap();
    }

    // Second session: restore and verify terminal doesn't come back
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            30,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_session().unwrap();
        assert!(restored, "Session should have been restored");
        harness.render().unwrap();

        // File should be restored
        harness.assert_screen_contains("test.txt");

        // Terminal should NOT be restored (it was closed before saving)
        harness.assert_screen_not_contains("Terminal");

        // Close the file - this should NOT bring back the terminal
        run_command(&mut harness, "close buffer");
        harness.render().unwrap();

        // After closing the only file, should have a new [No Name] buffer
        // NOT the old closed terminal
        let active_buffer = harness.editor().active_buffer_id();
        assert!(
            !harness.editor().is_terminal_buffer(active_buffer),
            "BUG: Closed terminal should NOT become focused after closing file. \
             A closed terminal from a previous session was incorrectly restored."
        );

        // Double-check terminal is not visible anywhere
        harness.assert_screen_not_contains("Terminal");
    }
}
