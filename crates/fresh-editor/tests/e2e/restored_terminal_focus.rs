//! Regression test: focusing a restored terminal tab activates terminal mode.
//!
//! A terminal that was live when a session was saved is restored read-only
//! (showing the last rendered scrollback). Before the fix, focusing such a
//! terminal tab left it stuck in the read-only scrollback view — often a
//! blank screen with no prompt — instead of bringing the live terminal back.
//! A freshly opened terminal, by contrast, drops you straight into terminal
//! mode. This test pins the restored-terminal behavior to match.
//!
//! Requires a working PTY (/dev/ptmx); skips when unavailable, like the other
//! terminal e2e tests.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use portable_pty::{native_pty_system, PtySize};
use tempfile::TempDir;

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

/// Build a config with hot exit on and `jump_to_end_on_output` off.
///
/// Disabling `jump_to_end_on_output` is deliberate: it removes the
/// "new terminal output re-enters terminal mode" path so the *only* way a
/// restored terminal can end up in terminal mode after restore is the focus
/// path under test. That isolates the behavior and keeps the test
/// deterministic (no dependence on shell-prompt output timing).
fn session_config() -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.terminal.jump_to_end_on_output = false;
    config
}

/// Restore a session whose terminal tab was a *background* tab at save time,
/// then focus it via a normal tab switch. The restored terminal must come
/// back live (terminal mode active), not stuck in the read-only view.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell
fn test_focusing_restored_terminal_activates_terminal_mode() {
    if !pty_available() {
        eprintln!("Skipping restored-terminal test: PTY not available in this environment");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file = project_dir.join("hello.txt");
    std::fs::write(&file, "editor content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // ---- First session: open a file + a terminal, then leave the terminal
    // as a background tab and shut down (saving the workspace). ----
    {
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(session_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.editor_mut().set_session_mode(true);

        // A non-terminal tab to be the active tab at save time.
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("editor content");

        // Open a terminal (becomes active + terminal mode) and run a command
        // so the backing file has recognizable content to restore.
        harness.editor_mut().open_terminal();
        harness.render().unwrap();
        harness
            .editor_mut()
            .active_window_mut()
            .send_terminal_input(b"echo RESTORE_MARKER_42\n");
        harness
            .wait_until(|h| h.screen_to_string().contains("RESTORE_MARKER_42"))
            .expect("terminal command output should appear");
        assert!(
            harness.editor().is_terminal_mode(),
            "freshly opened terminal should be in terminal mode"
        );

        // Switch focus back to the file so the terminal is a *background*
        // tab when the workspace is saved (this also exits terminal mode).
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        assert!(
            !harness.editor().is_terminal_mode(),
            "terminal mode should be off after switching to the file tab"
        );

        harness.shutdown(true).unwrap();
    }

    // ---- Second session: restore, then focus the restored terminal tab. ----
    {
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(session_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "session should have been restored");
        harness.render().unwrap();

        // The file tab was active at save time, so the restored terminal is a
        // background tab and we are not in terminal mode yet.
        assert!(
            !harness.editor().is_terminal_mode(),
            "restored background terminal should not start in terminal mode"
        );

        // Focus the terminal tab the way a user would: cycle tabs until the
        // active buffer is the terminal.
        for _ in 0..8 {
            let active = harness.editor().active_buffer_id();
            if harness.editor().active_window().is_terminal_buffer(active) {
                break;
            }
            harness
                .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
                .unwrap();
            harness.render().unwrap();
        }
        let active = harness.editor().active_buffer_id();
        assert!(
            harness.editor().active_window().is_terminal_buffer(active),
            "should have focused the restored terminal tab"
        );

        // The fix: focusing a restored terminal activates terminal mode, so
        // the user lands on a live terminal instead of a read-only screen.
        // Visible on screen via the status message, and in the editor state.
        harness.assert_screen_contains("Terminal mode enabled");
        assert!(
            harness.editor().is_terminal_mode(),
            "focusing a restored terminal tab should activate terminal mode"
        );

        // The terminal is genuinely live: a new command's output appears.
        harness
            .editor_mut()
            .active_window_mut()
            .send_terminal_input(b"echo SECOND_SESSION_LIVE\n");
        harness
            .wait_until(|h| h.screen_to_string().contains("SECOND_SESSION_LIVE"))
            .expect("restored terminal should be live and produce output");
    }
}
