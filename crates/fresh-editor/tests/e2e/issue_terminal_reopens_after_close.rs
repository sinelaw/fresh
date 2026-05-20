//! E2E reproduction for the "closed terminal reappears after restart" bug
//! (reproduced manually in tmux):
//!
//! 1. Open a terminal, quit the editor.
//! 2. Reopen the editor — the terminal is restored (as expected).
//! 3. Close the terminal tab buffer ("Close Buffer"), quit the editor.
//! 4. Restart the editor — the terminal STILL appears, even though it was
//!    explicitly closed before quitting.
//!
//! Root cause: `Editor::save_workspace` refuses to overwrite a non-empty
//! on-disk workspace when only virtual buffers (the Dashboard placeholder)
//! remain — the issue #2027 guard that protects a Dashboard-only quit from
//! wiping the saved file list. After the user closes the only real buffer
//! (the terminal), that same guard wrongly preserves the stale terminal
//! entry on disk, so the next restart brings the terminal back.
//!
//! The session-restore harness pattern (shared `DirectoryContext` +
//! `shutdown`/`startup`) mirrors a real quit/relaunch cycle.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use portable_pty::{native_pty_system, PtySize};
use tempfile::TempDir;

/// Terminals need a real PTY. On sandboxed CI without `/dev/ptmx` we skip
/// rather than fail.
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

/// Run a command through the command palette by name.
fn run_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(command).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn session_config() -> Config {
    let mut config = Config::default();
    // Persist + restore the full workspace across our simulated restarts.
    config.editor.restore_previous_session = true;
    config
}

fn make_harness(
    project_dir: &std::path::Path,
    dir_context: &DirectoryContext,
) -> EditorTestHarness {
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(session_config())
            .with_working_dir(project_dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.editor_mut().set_session_mode(true);
    harness
}

#[test]
fn test_closed_terminal_does_not_reappear_after_restart() {
    if !pty_available() {
        eprintln!("Skipping terminal-persistence test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // --- Session 1: open a terminal, then quit cleanly. ---
    {
        let mut harness = make_harness(&project_dir, &dir_context);
        harness.render().unwrap();

        run_command(&mut harness, "Open Terminal");
        harness.assert_screen_contains("Terminal 0");

        harness.shutdown(true).unwrap();
    }

    // --- Session 2: the terminal is restored (as in the manual repro),
    //     then close it via "Close Buffer" and quit. ---
    {
        let mut harness = make_harness(&project_dir, &dir_context);
        harness.startup(true, &[]).unwrap();

        // Restored, matching the manual repro's "see the terminal" step.
        harness.assert_screen_contains("Terminal 0");

        // Close the terminal tab buffer.
        run_command(&mut harness, "Close Buffer");
        harness.assert_screen_not_contains("Terminal 0");

        harness.shutdown(true).unwrap();
    }

    // --- Session 3: the closed terminal must NOT come back. ---
    {
        let mut harness = make_harness(&project_dir, &dir_context);
        harness.startup(true, &[]).unwrap();

        let screen = harness.screen_to_string();
        assert!(
            !screen.contains("Terminal 0"),
            "A terminal that was explicitly closed before quitting reappeared \
             after restart. Screen:\n{screen}"
        );
    }
}
