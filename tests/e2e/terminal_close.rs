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

/// Test: Open terminal, open split, focus back on terminal split, close buffer
///
/// When switching back to a terminal split while terminal_mode is active,
/// then closing the terminal buffer:
/// - terminal_mode should be turned OFF
/// - Focus should go to a valid buffer
/// - Keyboard input should work
#[test]
fn test_close_terminal_after_switching_back_to_terminal_split() {
    let mut harness = harness_or_return!(120, 30);

    // Disable jump_to_end_on_output
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    // Open terminal in the initial split
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    assert!(
        harness.editor().is_terminal_mode(),
        "Should be in terminal mode after opening terminal"
    );

    // Exit terminal mode to create a split
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Create a vertical split (focus moves to new split)
    run_command(&mut harness, "split vert");

    // Now we're in the new (right) split with [No Name]
    assert!(
        !harness.editor().is_terminal_mode(),
        "Should NOT be in terminal mode in new split"
    );

    // Focus back on the first split (with terminal) - this re-enters terminal_mode
    run_command(&mut harness, "prev split");

    let active_buffer = harness.editor().active_buffer_id();
    assert!(
        harness.editor().is_terminal_buffer(active_buffer),
        "Should be on terminal buffer after switching to terminal split"
    );

    // The bug: terminal_mode becomes true when switching to terminal split
    // Close the terminal buffer - terminal_mode should be turned OFF
    run_command(&mut harness, "close buffer");

    // After closing:
    // 1. terminal_mode MUST be OFF (this is the bug being tested)
    assert!(
        !harness.editor().is_terminal_mode(),
        "terminal_mode MUST be OFF after closing the terminal buffer. \
         Bug: terminal_mode stays true even after the terminal is closed."
    );

    // 2. Active buffer should NOT be a terminal
    let active_after = harness.editor().active_buffer_id();
    assert!(
        !harness.editor().is_terminal_buffer(active_after),
        "Active buffer should NOT be terminal after close"
    );

    // 3. Keyboard input should work
    harness.type_text("world").unwrap();
    harness.render().unwrap();

    let content = harness
        .editor()
        .get_buffer_content(active_after)
        .unwrap_or_default();

    assert!(
        content.contains("world"),
        "Should be able to type after closing terminal. Content: {:?}",
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
