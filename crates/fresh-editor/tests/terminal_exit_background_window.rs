//! Regression: a terminal exiting in a *background* window must be torn down
//! in *that* window — never in whichever window happens to be active.
//!
//! Terminal ids are per-window (`TerminalManager::new` starts `next_id` at 0
//! for every `Window`), so the first terminal of window A and the first
//! terminal of window B are both `TerminalId(0)`. The `TerminalExited`
//! message is tagged with its owning window (`WindowTerminalId`), but
//! `handle_terminal_exited` ignored the tag for everything except the plugin
//! hook: it searched `active_window().terminal_buffers` for the id and called
//! `active_window_mut().terminal_manager.close(id)`. So when B's agent exits
//! while the user is looking at A:
//!   * A's live terminal buffer is flipped to read-only scrollback, removed
//!     from `terminal_buffers`, and renamed "(exited)";
//!   * `close()` sends `Shutdown` to A's writer thread, which KILLS A's shell
//!     (its own exit then fires `terminal_exit` for A → Orchestrator marks
//!     A's agent errored/ready);
//!   * B's buffer is never marked dead and B's handle is never closed.
//!
//! Skips when the environment has no PTY.
#![cfg(unix)]

use crate::common::harness::{EditorTestHarness, HarnessOptions};
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

#[test]
fn background_window_terminal_exit_does_not_kill_active_windows_terminal() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
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

    // Window A: a long-lived "agent".
    let (win_a, term_a, buf_a) = window_with_terminal(&mut h, &proj_a, "a", "sleep 600");
    // Window B: an "agent" that exits as soon as it gets a line of input, so
    // the test (not a timer) decides when the exit happens.
    let (win_b, term_b, buf_b) =
        window_with_terminal(&mut h, &proj_b, "b", "read _line; exit 0");
    h.process_async_and_render().unwrap();

    assert_ne!(win_a, win_b);
    assert_eq!(
        term_a, term_b,
        "precondition: terminal ids are per-window, so both first terminals \
         share an id — that collision is what the exit handler must not trip on"
    );

    // The user goes back to A; B keeps running in the background.
    h.editor_mut().set_active_window(win_a);
    h.process_async_and_render().unwrap();
    assert_eq!(h.editor().active_window().id, win_a);

    // B's agent finishes.
    h.editor()
        .session(win_b)
        .unwrap()
        .terminal_manager
        .get(term_b)
        .expect("B's terminal handle is live")
        .write(b"done\n");

    // Wait until *some* window has processed an exit: a correct handler marks
    // B's buffer dead; a misrouted one marks A's buffer dead instead. Either
    // way this condition eventually holds, so a regression fails by
    // assertion rather than by hanging.
    h.wait_until(|h| {
        let a = h.editor().session(win_a).unwrap();
        let b = h.editor().session(win_b).unwrap();
        !a.is_terminal_buffer(buf_a) || !b.is_terminal_buffer(buf_b)
    })
    .unwrap();

    let a = h.editor().session(win_a).unwrap();
    let b = h.editor().session(win_b).unwrap();

    assert!(
        a.is_terminal_buffer(buf_a),
        "window A's terminal must still be live — B's exit was applied to the \
         active window A"
    );
    assert!(
        !a.exited_terminals.contains_key(&buf_a),
        "window A's terminal must not be recorded as exited"
    );
    assert!(
        a.terminal_manager
            .get(term_a)
            .is_some_and(|handle| handle.is_alive()),
        "window A's `sleep 600` must not have been killed by B's exit \
         (misrouted `terminal_manager.close`)"
    );

    assert!(
        !b.is_terminal_buffer(buf_b),
        "window B's exited terminal must be torn down in B"
    );
    assert!(
        b.exited_terminals.contains_key(&buf_b),
        "window B's buffer must carry the exited-terminal record (restart path)"
    );
    assert!(
        b.terminal_manager.get(term_b).is_none(),
        "window B's dead handle must be closed/removed from B's manager"
    );
}
