//! Regression test: switching into a *restored* session via the Orchestrator
//! dock (a window switch) activates its terminal instead of leaving it in
//! read-only scrollback.
//!
//! A sibling test (`restored_terminal_focus.rs`) pins the *tab-switch* path:
//! focusing a restored terminal tab within one window re-enters terminal
//! mode. This test pins the *window-switch* path — the Orchestrator dock's
//! "dive": clicking/activating a workspace calls `setActiveWindow`, which
//! lands on `Editor::set_active_window`.
//!
//! Before the fix, `set_active_window` moved the active-window pointer but —
//! unlike the tab-switch path (`set_active_buffer`) — never re-synced
//! `terminal_mode`. So switching into a session whose active buffer was a
//! restored (live-but-read-only) terminal showed the stale read-only
//! scrollback view; the user had to type or wait for output to wake it. The
//! fix routes the window switch through the same mode authority
//! (`sync_terminal_mode_to_active_buffer`).
//!
//! Requires a working PTY (/dev/ptmx); skips when unavailable, like the other
//! terminal e2e tests.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::app::PluginTerminalSpec;
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

/// Hot exit on, `jump_to_end_on_output` off. Disabling the output-driven
/// auto-resume is deliberate: it removes the "new terminal output re-enters
/// terminal mode" path so the *only* way the restored terminal can end up in
/// terminal mode after the dive is the window-switch sync under test. That
/// isolates the behavior from shell-prompt output timing (and mirrors the
/// masking the real bug hides behind with the default config).
fn session_config() -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.terminal.jump_to_end_on_output = false;
    config
}

/// Spawn an ephemeral, command-carrying terminal into `window` the way
/// `create_window_with_terminal` does: an ephemeral PTY plus a
/// `terminal_commands` entry marking it as a restorable *session* terminal so
/// workspace-save persists it.
fn spawn_session_terminal(window: &mut fresh::app::window::Window, argv: &[&str]) {
    let argv: Vec<String> = argv.iter().map(|s| s.to_string()).collect();
    let (terminal_id, _buffer_id, _leaf) = window
        .create_plugin_terminal(PluginTerminalSpec {
            cwd: None,
            direction: None,
            ratio: None,
            focus: true,
            persistent: false,
            command: Some(argv.clone()),
            title: None,
            env: std::collections::HashMap::new(),
        })
        .expect("session terminal should spawn");
    window.terminal_commands.insert(terminal_id, argv);
}

#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell command
fn test_switching_into_restored_terminal_window_activates_it() {
    if !pty_available() {
        eprintln!("Skipping restored-terminal dock-activation test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // A long-lived command so the terminal is live (not exited) at save time.
    let argv = ["sh", "-c", "exec sleep 30"];

    // ---- Session 1: a session whose active buffer is a live terminal. ----
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

        spawn_session_terminal(harness.editor_mut().active_window_mut(), &argv);
        harness.render().unwrap();
        let active = harness.editor().active_buffer_id();
        assert!(
            harness.editor().active_window().is_terminal_buffer(active),
            "the spawned terminal should be the active buffer before save"
        );
        assert!(
            harness.editor().is_terminal_mode(),
            "a freshly spawned terminal is live (terminal mode on)"
        );

        harness.shutdown(true).unwrap();
    }

    // ---- Session 2: restart; the session comes back with the terminal as
    // its active buffer, restored read-only. Diving *away* to another window
    // and back is the dock's window-switch — it must re-activate the
    // terminal. ----
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

        let restored_win = harness.editor().active_session_id();
        let active = harness.editor().active_buffer_id();
        assert!(
            harness.editor().active_window().is_terminal_buffer(active),
            "restored session should come back with its terminal as the \
             active buffer"
        );
        // Precondition: the restored terminal starts read-only (scrollback),
        // exactly the state the dive must wake. If this ever changes, the
        // dive-back below would no longer exercise the transition.
        assert!(
            !harness.editor().is_terminal_mode(),
            "a restored terminal starts in read-only scrollback, not \
             terminal mode"
        );

        // A second, non-terminal window to switch to — the "other session"
        // in the dock. In-memory is enough; only the switch matters.
        let other_dir = temp_dir.path().join("other");
        std::fs::create_dir(&other_dir).unwrap();
        let other = harness
            .editor_mut()
            .create_window_at(other_dir, "other".to_string());
        assert_ne!(other, restored_win);

        // Switch away, then back into the terminal session — this is the
        // dock's activate path (`editor.setActiveWindow(id)` →
        // `Editor::set_active_window`).
        harness.editor_mut().set_active_window(other);
        harness.render().unwrap();
        assert!(
            !harness.editor().is_terminal_mode(),
            "the non-terminal 'other' window is not in terminal mode"
        );

        harness.editor_mut().set_active_window(restored_win);
        harness.render().unwrap();

        let active = harness.editor().active_buffer_id();
        assert!(
            harness.editor().active_window().is_terminal_buffer(active),
            "switching back should land on the restored terminal"
        );
        // The fix: the window switch activates the terminal. Before it, the
        // restored terminal stayed read-only scrollback and this was false.
        assert!(
            harness.editor().is_terminal_mode(),
            "switching into a restored session must activate its terminal, \
             not leave it in read-only scrollback"
        );
        // The read-only → live completion actually ran (editing re-enabled).
        assert!(
            !harness.editor().active_window().is_editing_disabled(),
            "activating the restored terminal must re-enable editing (live \
             PTY input), not leave the buffer read-only"
        );
    }
}
