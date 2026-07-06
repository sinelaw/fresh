//! Reproducer for issue #2570's *connecting-limbo* variant (user report on
//! PR #2575): diving onto a dormant SSH workspace whose host **hangs** —
//! accepts the TCP connection but never completes the SSH handshake, the
//! shape of a shut-down host that drops packets — used to leave the editor
//! on the previous workspace for the whole (potentially minutes-long)
//! connect attempt, while the dock had already highlighted the SSH row and
//! the status bar said "Connecting…".
//!
//! Now the dive **commits the switch immediately**: the SSH session's empty
//! shell becomes the active window while the connect is still in flight, so
//! the dock's selection and the editor view always agree (an empty/progress
//! page counts; the previous workspace on screen does not).
//!
//! The hang is deterministic via the `tests/fixtures/fake-ssh-hang` shim —
//! no network involved. Single test in this binary: the persistence
//! isolation sets the process-global `XDG_DATA_HOME` (see
//! `common::dormant_ssh::isolated_dir_context`), and the PATH shim is
//! likewise process-global.
//!
//! Plugins-gated: the dormant-session dive only connects with the plugin
//! runtime present. Linux-gated like the sibling reproducers (XDG-based
//! isolation, Unix shell shim).
#![cfg(all(target_os = "linux", feature = "plugins"))]

mod common;

use common::dormant_ssh::{
    canonical_mkdir, ensure_hanging_fake_ssh_on_path, isolated_dir_context,
    persist_previous_session,
};
use common::harness::{EditorTestHarness, HarnessOptions};
use fresh_core::api::PluginCommand;

/// While the connect is still pending, the dive must already have switched
/// the editor into the SSH workspace's empty shell — Connecting state on
/// screen, previous workspace's buffers gone — and switching back must work
/// without waiting for the connect to resolve.
#[test]
fn dive_commits_switch_while_connect_is_still_pending() {
    common::tracing::init_tracing_from_env();
    ensure_hanging_fake_ssh_on_path();
    fresh::i18n::set_locale("en");

    let base = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(base.path());
    let project = canonical_mkdir(base.path(), "project");
    let remote_root = canonical_mkdir(base.path(), "remote-root");

    persist_previous_session(&dir_context, &project, &remote_root, false);

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
    // the screen the moment the dive commits.
    h.open_file(&project.join("local_marker.txt")).unwrap();
    h.wait_for_screen_contains("local_marker.txt").unwrap();
    let local_id = h.editor().active_window_id();

    let dormant = h.editor().dormant_remote_sessions_for_test();
    let ssh_id = dormant
        .iter()
        .find(|(_, l)| l == "ssh-dead")
        .map(|(id, _)| *id)
        .expect("the SSH session must come back as a dormant descriptor");

    // Dive, exactly as the dock's click/live-switch does. The fake ssh hangs
    // forever, so nothing below is reachable by waiting for the connect to
    // resolve — the switch has to happen up front.
    h.editor_mut()
        .handle_plugin_command(PluginCommand::SetActiveWindow { id: ssh_id })
        .unwrap();

    // The switch is committed immediately: the previous workspace's buffer
    // leaves the screen and the SSH session's shell is the active window,
    // presenting itself as Connecting.
    h.wait_until(|h| !h.screen_to_string().contains("local_marker.txt"))
        .unwrap();
    assert_eq!(
        h.editor().active_window_id(),
        ssh_id,
        "the dive must land in the tried workspace while the connect is pending"
    );
    h.wait_until(|h| h.screen_to_string().contains("Connecting"))
        .unwrap();
    // The shell is EMPTY — nothing restored without the backend — and it
    // renders as a placeholder page, not as an editable scratch buffer: no
    // "[No Name]" tab, a message instead.
    h.assert_screen_not_contains("remote_notes.txt");
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("This workspace is unavailable") && !scr.contains("[No Name]")
    })
    .unwrap();
    // Nothing is editable before the connection exists: typing must be
    // swallowed by the read-only placeholder, not land in a hidden buffer.
    h.type_text("XYZ").unwrap();
    h.process_async_and_render().unwrap();
    assert!(
        !h.get_buffer_content().unwrap_or_default().contains("XYZ"),
        "typing into the placeholder page must not edit anything"
    );
    h.assert_screen_not_contains("XYZ");

    // The user is not trapped in the pending shell: switching back to the
    // local workspace works while the connect is still in flight.
    h.editor_mut()
        .handle_plugin_command(PluginCommand::SetActiveWindow { id: local_id })
        .unwrap();
    h.wait_for_screen_contains("local_marker.txt").unwrap();
}
