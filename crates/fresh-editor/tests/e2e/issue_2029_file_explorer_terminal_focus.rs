//! Regression test for issue #2029 (sub-issue 1: file-explorer focus is
//! stolen back to the terminal).
//!
//! Two sub-cases:
//!
//! 1a. `Ctrl+B` while a terminal is active: covered here as a
//!     deterministic unit-style test on `toggle_file_explorer`. The
//!     previous e2e version was racy under CI load — its screen-
//!     observable signal (Down → preview content) depended on the
//!     file explorer's async init *and* a keyboard-driven preview
//!     open, and timed out at 180s when either path stalled. The
//!     fix itself only changes which state `toggle_file_explorer`
//!     leaves the window in, so this test asserts that state
//!     directly. Per CONTRIBUTING §2 ("if an invariant isn't
//!     visible on screen, cover it with a unit test on the
//!     component") this is the appropriate shape.
//!
//! 1b. Click on a file in the explorer while a terminal is active:
//!     per the docstring at `click_handlers.rs:554-557`, a single
//!     click should "Open the file but keep focus on file
//!     explorer". Today the click handler's `key_context =
//!     FileExplorer` write is undone by `set_active_buffer`
//!     (`active_focus.rs:103-107`), which resets
//!     `key_context = Normal` because we were leaving a terminal
//!     buffer. End-to-end via the screen, using mouse_click so the
//!     test doesn't depend on the keyboard-Down → preview path.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::input::keybindings::KeyContext;
use portable_pty::{native_pty_system, PtySize};
use std::fs;

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

fn explorer_row_for(harness: &EditorTestHarness, name: &str) -> u16 {
    let screen = harness.screen_to_string();
    const FIRST_EXPLORER_ROW: usize = 2;
    for (row, line) in screen.lines().enumerate().skip(FIRST_EXPLORER_ROW) {
        let prefix: String = line.chars().take(40).collect();
        if prefix.contains(name) {
            return row as u16;
        }
    }
    panic!("file {name} not found in file explorer;\nscreen:\n{screen}");
}

/// 1a — `toggle_file_explorer` must clear `terminal_mode` and set
/// `key_context = FileExplorer` when called while the active window
/// is in terminal mode, so the next keystroke reaches the file
/// explorer instead of being swallowed by `dispatch_terminal_input`
/// and forwarded to the PTY child (issue #2029, sub-bug 1).
///
/// Unit-style test: drives the production `toggle_file_explorer`
/// path and asserts the two state invariants the fix establishes.
/// Doesn't open a real PTY — sets `terminal_mode = true` directly
/// on the window — so it's fast and deterministic regardless of
/// CI load.
#[test]
fn toggle_file_explorer_clears_terminal_mode() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();

    // Pre-condition: terminal mode is active (as it would be after
    // `open_terminal()`). `is_terminal_buffer(active)` is false here
    // because we didn't actually spawn a PTY child — that's fine,
    // the relevant `take_focus_for_file_explorer` branch only gates
    // the `terminal_mode_resume` insert on it; the `terminal_mode`
    // reset and the `key_context = FileExplorer` write both run
    // unconditionally when `terminal_mode` was true.
    {
        let win = harness.editor_mut().active_window_mut();
        win.terminal_mode = true;
        win.key_context = KeyContext::Terminal;
    }
    assert!(
        harness.editor().is_terminal_mode(),
        "precondition: window in terminal mode"
    );

    // The act under test: the same call that `Action::ToggleFileExplorer`
    // dispatches through.
    harness.editor_mut().toggle_file_explorer();

    // Both invariants the fix establishes.
    assert!(
        !harness.editor().is_terminal_mode(),
        "toggle_file_explorer must clear terminal_mode so \
         dispatch_terminal_input stops swallowing keys destined for \
         the file explorer (issue #2029)"
    );
    assert_eq!(
        harness.editor().get_key_context(),
        KeyContext::FileExplorer,
        "toggle_file_explorer must hand keyboard focus to the explorer"
    );
}

/// 1b — single-clicking a file in the explorer while a terminal is
/// the active buffer must keep keyboard focus on the file explorer
/// so the user can keep arrow-browsing previews. Today focus ends
/// up on the previewed editor buffer.
///
/// Screen-observable test: after a click on `alpha.txt`, pressing
/// `Down` should advance the explorer selection to `beta.txt` and
/// trigger its preview — `beta.txt`'s content must appear on
/// screen. With focus stolen to the editor (the bug), `Down` would
/// move the cursor inside the alpha.txt buffer and
/// `BETA_FILE_CONTENT` would never appear.
#[test]
fn click_in_explorer_while_terminal_active_keeps_focus_on_explorer() {
    if !pty_available() {
        eprintln!("Skipping: PTY not available in this environment");
        return;
    }

    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project = harness.project_dir().unwrap();
    fs::write(project.join("alpha.txt"), "ALPHA_FILE_CONTENT\n").unwrap();
    fs::write(project.join("beta.txt"), "BETA_FILE_CONTENT\n").unwrap();

    // Wait for the terminal tab to render before sending any keys —
    // without this gate, on heavily-loaded CI the Ctrl+B can race
    // ahead of the terminal's async setup.
    harness.editor_mut().open_terminal();
    harness.wait_for_screen_contains("*Terminal 0*").unwrap();

    // Open the file explorer and wait for both target files to
    // render so the click lands against a settled tree.
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("alpha.txt").unwrap();
    harness.wait_for_file_explorer_item("beta.txt").unwrap();

    // Single-click alpha.txt. Wait for the preview to render so the
    // next keypress is observed against a settled UI.
    let alpha_row = explorer_row_for(&harness, "alpha.txt");
    harness.mouse_click(10, alpha_row).unwrap();
    harness
        .wait_for_screen_contains("ALPHA_FILE_CONTENT")
        .unwrap();

    // Down should advance the *explorer* selection to beta.txt and
    // preview it. If focus leaked to the previewed editor buffer
    // (the bug), Down would move the cursor inside alpha.txt and
    // `BETA_FILE_CONTENT` would never appear.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .wait_for_screen_contains("BETA_FILE_CONTENT")
        .unwrap();
}
