//! Regression: a plugin acting on a *background* window's terminal by its
//! bare `TerminalId` (`editor.sendTerminalInput(id, …)` /
//! `editor.closeTerminal(id)`) must never reach the *active* window's
//! terminal.
//!
//! Terminal ids used to be numbered per window (every `TerminalManager`
//! counted from 0), so window A's first terminal and window B's first
//! terminal were both `TerminalId(0)`, and the plugin terminal commands
//! resolved the id against the active window. A plugin driving B's agent
//! while the user looked at A typed into — or closed — A's terminal instead.
//! Ids are now allocated editor-wide, so a bare id names exactly one
//! terminal, and the commands act in the window that owns it.
//!
//! Skips when the environment has no PTY.
#![cfg(unix)]

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh_core::api::PluginCommand;
use portable_pty::{native_pty_system, PtySize};

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

fn mk(dir: &std::path::Path, name: &str) -> std::path::PathBuf {
    let p = dir.join(name);
    std::fs::create_dir_all(&p).unwrap();
    p.canonicalize().unwrap()
}

/// Seed a new window whose only content is a terminal running `sh -c <script>`
/// — the same host entry point the Orchestrator uses for agent sessions.
fn window_with_terminal(
    h: &mut EditorTestHarness,
    root: &std::path::Path,
    label: &str,
    script: &str,
) -> (
    fresh_core::WindowId,
    fresh_core::TerminalId,
    fresh_core::BufferId,
) {
    let authority = h.editor().local_session_authority(root);
    h.editor_mut()
        .create_window_with_terminal(
            root.to_path_buf(),
            label.to_string(),
            Some(root.to_path_buf()),
            Some(vec!["sh".into(), "-c".into(), script.into()]),
            Some(label.to_string()),
            std::sync::Arc::new(fresh::services::authority::Connection::plain(authority)),
            None,
            None,
            false,
            None,
        )
        .expect("spawning the seeded terminal should succeed")
}

struct TwoWindows {
    h: EditorTestHarness,
    win_a: fresh_core::WindowId,
    term_a: fresh_core::TerminalId,
    buf_a: fresh_core::BufferId,
    win_b: fresh_core::WindowId,
    term_b: fresh_core::TerminalId,
    buf_b: fresh_core::BufferId,
    _base: tempfile::TempDir,
}

/// Window A (active) and window B (background), each with one terminal
/// running `script`.
fn two_windows(script: &str) -> TwoWindows {
    fresh::i18n::set_locale("en");
    let base = tempfile::tempdir().unwrap();
    let home = mk(base.path(), "home");
    let proj_a = mk(base.path(), "proj_a");
    let proj_b = mk(base.path(), "proj_b");

    let mut h = EditorTestHarness::create(
        120,
        36,
        HarnessOptions::new()
            .with_working_dir(home)
            .with_empty_plugins_dir(),
    )
    .unwrap();

    let (win_a, term_a, buf_a) = window_with_terminal(&mut h, &proj_a, "a", script);
    let (win_b, term_b, buf_b) = window_with_terminal(&mut h, &proj_b, "b", script);
    h.process_async_and_render().unwrap();
    assert_ne!(win_a, win_b);

    // The user goes back to A; B keeps running in the background.
    h.editor_mut().set_active_window(win_a);
    h.process_async_and_render().unwrap();
    assert_eq!(h.editor().active_window().id, win_a);

    TwoWindows {
        h,
        win_a,
        term_a,
        buf_a,
        win_b,
        term_b,
        buf_b,
        _base: base,
    }
}

#[test]
fn plugin_input_to_background_terminal_does_not_reach_active_terminal() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    // Each "agent" exits as soon as it reads a line, so whichever terminal the
    // input lands in reveals itself by exiting — no timer involved.
    let TwoWindows {
        mut h,
        win_a,
        term_a,
        buf_a,
        win_b,
        term_b,
        buf_b,
        _base,
    } = two_windows("read _line; exit 0");

    // A plugin drives B's agent by the id it got back when B's terminal was
    // created.
    h.editor_mut()
        .handle_plugin_command(PluginCommand::SendTerminalInput {
            terminal_id: term_b,
            data: "done\n".into(),
        })
        .unwrap();

    // Wait until one of the two terminals has exited: a correct dispatch
    // exits B; a misrouted one exits A. Either way this eventually holds, so
    // a regression fails by assertion rather than by hanging.
    h.wait_until(|h| {
        let a = h.editor().session(win_a).unwrap();
        let b = h.editor().session(win_b).unwrap();
        !a.is_terminal_buffer(buf_a) || !b.is_terminal_buffer(buf_b)
    })
    .unwrap();

    let a = h.editor().session(win_a).unwrap();
    assert!(
        a.is_terminal_buffer(buf_a),
        "window A's terminal must still be live — the input meant for B's \
         terminal was written to the active window's terminal"
    );
    assert!(
        a.terminal_manager
            .get(term_a)
            .is_some_and(|handle| handle.is_alive()),
        "window A's agent must not have received B's input"
    );
    let b = h.editor().session(win_b).unwrap();
    assert!(
        b.exited_terminals.contains_key(&buf_b),
        "window B's agent must have received the input and exited"
    );
}

#[test]
fn plugin_close_of_background_terminal_does_not_close_active_terminal() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let TwoWindows {
        mut h,
        win_a,
        term_a,
        buf_a,
        win_b,
        term_b,
        buf_b,
        _base,
    } = two_windows("sleep 600");

    h.editor_mut()
        .handle_plugin_command(PluginCommand::CloseTerminal {
            terminal_id: term_b,
        })
        .unwrap();

    {
        let a = h.editor().session(win_a).unwrap();
        assert!(
            a.is_terminal_buffer(buf_a),
            "window A's terminal buffer must survive a plugin closing B's terminal"
        );
        assert!(
            a.terminal_manager
                .get(term_a)
                .is_some_and(|handle| handle.is_alive()),
            "window A's `sleep 600` must not have been closed by a plugin \
             targeting B's terminal"
        );
        let b = h.editor().session(win_b).unwrap();
        assert!(
            b.terminal_manager.get(term_b).is_none(),
            "window B's terminal must be closed in B"
        );
    }

    // B's killed process reports its exit to B, which turns B's buffer into
    // read-only scrollback; A is untouched throughout.
    h.wait_until(|h| {
        !h.editor()
            .session(win_b)
            .unwrap()
            .is_terminal_buffer(buf_b)
    })
    .unwrap();
    let a = h.editor().session(win_a).unwrap();
    assert!(a.is_terminal_buffer(buf_a));
    assert!(a
        .terminal_manager
        .get(term_a)
        .is_some_and(|handle| handle.is_alive()));
}
