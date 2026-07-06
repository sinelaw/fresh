//! Reproducer for issue #2570: switching to a persisted (dormant) SSH
//! workspace whose backend can no longer be reached.
//!
//! Before the fix, the dock's dive (`SetActiveWindow` on a dormant remote
//! session) kicked off the reconnect and, when it failed, left the editor on
//! the *previous* workspace while the dock had already moved its selection —
//! the UI claimed the SSH workspace was current, but the previous session's
//! buffers stayed on screen. Now a failed connect **commits the switch**: the
//! session gets an empty disconnected shell window (nothing restored — its
//! on-disk workspace stays authoritative), the shell becomes the active
//! window, and diving again retries the connect.
//!
//! The SSH failure is deterministic: the fake `ssh` shim in
//! `tests/fixtures/fake-ssh` fails instantly like an unreachable host, so no
//! network (or real ssh binary) is involved. Single test in this binary: the
//! persistence isolation sets the process-global `XDG_DATA_HOME` (see
//! `common::dormant_ssh::isolated_dir_context`).
//!
//! Plugins-gated: the dormant-session dive (`bring_dormant_remote_online` →
//! `start_remote_connect`) is a no-op without the plugin runtime, and the
//! `handle_plugin_command` entry point only exists with it. Linux-gated like
//! `remote_restore_terminal_e2e.rs`, which shares the same scaffolding
//! constraints: persistence isolation rides `XDG_DATA_HOME` (ignored by the
//! macOS/Windows data dirs) and the fake `ssh` is a Unix shell script.
#![cfg(all(target_os = "linux", feature = "plugins"))]

mod common;

use common::dormant_ssh::{
    canonical_mkdir, ensure_fake_ssh_on_path, isolated_dir_context, persist_previous_session,
};
use common::harness::{EditorTestHarness, HarnessOptions};
use fresh_core::api::PluginCommand;

/// Diving into a dormant SSH workspace whose connect fails must land the
/// editor in that workspace — as an empty disconnected shell — not leave it
/// sitting on the previous workspace while the dock claims otherwise. The
/// shell must never overwrite the session's on-disk workspace, and a later
/// dive must still work (retry, and switching back and forth).
#[test]
fn failed_dormant_reconnect_commits_switch_to_empty_shell() {
    common::tracing::init_tracing_from_env();
    ensure_fake_ssh_on_path();
    fresh::i18n::set_locale("en");

    let base = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(base.path());
    let project = canonical_mkdir(base.path(), "project");
    let remote_root = canonical_mkdir(base.path(), "remote-root");

    persist_previous_session(&dir_context, &project, &remote_root, false);

    // The persisted SSH workspace file — must survive the whole test
    // unchanged (the disconnected shell has nothing real to save).
    let ws_file = dir_context.data_dir.join("workspaces").join(format!(
        "{}.json",
        fresh::workspace::encode_path_for_filename(&remote_root)
    ));
    let ws_before = std::fs::read_to_string(&ws_file).expect("persisted remote workspace");
    assert!(
        ws_before.contains("remote_notes.txt") && ws_before.contains("RemoteAgent"),
        "sanity: the persisted remote workspace carries its content + backend spec"
    );

    // ---- The session under test: relaunch locally, dive into the dead
    //      SSH workspace. ----
    let mut h = EditorTestHarness::create(
        120,
        36,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context.clone())
            .with_empty_plugins_dir(),
    )
    .unwrap();
    // A visible buffer in the local workspace — the thing that must LEAVE
    // the screen when the dive commits. (Opened explicitly: the harness's
    // `create` doesn't run the production foreground-workspace restore.)
    h.open_file(&project.join("local_marker.txt")).unwrap();
    h.wait_for_screen_contains("local_marker.txt").unwrap();
    let local_id = h.editor().active_window_id();

    let dormant = h.editor().dormant_remote_sessions_for_test();
    let ssh_id = dormant
        .iter()
        .find(|(_, l)| l == "ssh-dead")
        .map(|(id, _)| *id)
        .expect("the SSH session must come back as a dormant descriptor");

    // Dive, exactly as the dock's live-switch does.
    h.editor_mut()
        .handle_plugin_command(PluginCommand::SetActiveWindow { id: ssh_id })
        .unwrap();

    // The connect fails (fake ssh) and the failure is surfaced...
    h.wait_until(|h| h.screen_to_string().contains("Connection failed"))
        .unwrap();
    // ...and the switch is COMMITTED: the previous workspace's buffer is no
    // longer on screen (before the fix the editor stayed on it forever).
    h.wait_until(|h| !h.screen_to_string().contains("local_marker.txt"))
        .unwrap();
    assert_eq!(
        h.editor().active_window_id(),
        ssh_id,
        "the tried workspace must be the active window after the failed connect"
    );
    // The shell is EMPTY: nothing of the persisted workspace can be restored
    // without its backend, so its file must not appear — and it renders as a
    // placeholder page (no "[No Name]" tab) showing the disconnected state.
    h.assert_screen_not_contains("remote_notes.txt");
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("Disconnected —")
            && scr.contains("This workspace is unavailable")
            && !scr.contains("[No Name]")
    })
    .unwrap();

    // Quit-style save: the disconnected shell must not clobber the real
    // on-disk workspace with its empty layout.
    h.editor_mut().save_all_windows_workspaces().unwrap();
    let ws_after = std::fs::read_to_string(&ws_file).unwrap();
    assert_eq!(
        ws_before, ws_after,
        "a disconnected shell must never overwrite the session's persisted workspace"
    );

    // Switching back to the local workspace still works...
    h.editor_mut()
        .handle_plugin_command(PluginCommand::SetActiveWindow { id: local_id })
        .unwrap();
    h.wait_for_screen_contains("local_marker.txt").unwrap();

    // ...and diving into the SSH workspace again retries + lands in the
    // shell again (the session stayed dormant behind it).
    h.editor_mut()
        .handle_plugin_command(PluginCommand::SetActiveWindow { id: ssh_id })
        .unwrap();
    h.wait_until(|h| h.editor().active_window_id() == ssh_id)
        .unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("local_marker.txt"))
        .unwrap();
}
