//! Regression: arrow-navigating the orchestrator dock past a *disconnected*
//! remote (SSH) workspace must not auto-connect it. In the dock's live-switch
//! model, arrowing onto a row makes it the active window — but for a dormant
//! remote that means starting an SSH connect and re-pointing the editor's
//! active authority at that backend. Against an unreachable host that connect
//! stalls, so merely scrolling the selection onto such a row froze the whole
//! UI (the user's report). #2570 only made the deliberate *dive* non-blocking.
//!
//! Here the SSH host **hangs** (accepts the connection, never completes the
//! handshake — `tests/fixtures/fake-ssh-hang`), the deterministic shape of an
//! unreachable host. With the bug, arrow-Down onto the SSH row commits the
//! switch and lands in its "Connecting…" shell (leaving the local workspace);
//! with the fix the editor stays put on the local workspace and only an
//! explicit dive (Enter) connects.
//!
//! Single test in this binary: the fake-ssh PATH shim and
//! `isolated_dir_context`'s process-global `XDG_DATA_HOME` must not leak.
#![cfg(all(target_os = "linux", feature = "plugins"))]

mod common;

use common::dormant_ssh::{
    canonical_mkdir, ensure_hanging_fake_ssh_on_path, isolated_dir_context,
    persist_previous_session,
};
use common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn arrow_nav_does_not_connect_disconnected_remote() {
    common::tracing::init_tracing_from_env();
    ensure_hanging_fake_ssh_on_path();
    fresh::i18n::set_locale("en");

    let base = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(base.path());
    let project = canonical_mkdir(base.path(), "project");
    let remote_root = canonical_mkdir(base.path(), "remote-root");

    let plugins_dir = project.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    // Leaves behind a local project workspace + a dormant SSH session
    // (`ssh-dead`) whose host now hangs.
    persist_previous_session(&dir_context, &project, &remote_root, true);

    let mut cfg = fresh::config::Config::default();
    cfg.editor.animations = false;
    cfg.editor.cursor_jump_animation = false;
    let mut h = EditorTestHarness::create(
        140,
        40,
        HarnessOptions::new()
            .with_config(cfg)
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context.clone()),
    )
    .unwrap();
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all()
            .iter()
            .any(|c| c.get_localized_name() == "Orchestrator: Toggle Dock")
    })
    .unwrap();
    // A visible buffer in the local workspace — the thing that must STAY on
    // screen (the editor must not leave it for the SSH shell on arrow-nav).
    h.open_file(&project.join("local_marker.txt")).unwrap();
    h.wait_for_screen_contains("local_marker.txt").unwrap();

    // Open the dock.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("ssh-dead") && scr.contains("⇅")
    })
    .unwrap();

    let local_root = h.editor().active_window().root.clone();

    // Arrow onto the SSH row and let the (debounced) live-switch fire and the
    // screen settle. With the bug this commits into the SSH "Connecting…"
    // shell; with the fix the editor stays on the local workspace.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until_stable(|h| h.screen_to_string().contains("ssh-dead"))
        .unwrap();

    assert_eq!(
        h.editor().active_window().root,
        local_root,
        "arrow-nav must NOT auto-connect a disconnected remote — the editor \
         must stay on the local workspace. Screen:\n{}",
        h.screen_to_string(),
    );
    // The local buffer is still shown, and no connect was kicked off (no
    // "Connecting" shell was surfaced by arrow-nav).
    h.assert_screen_contains("local_marker.txt");
    assert!(
        !h.screen_to_string().contains("Connecting"),
        "arrow-nav must not start a connect to the disconnected remote. Screen:\n{}",
        h.screen_to_string(),
    );

    // The dock still lists the session — arrow-nav didn't drop or mutate it.
    h.assert_screen_contains("ssh-dead");

    // And the deliberate dive still works: Enter commits into the SSH shell,
    // which now shows its "Connecting…" state (the connect hangs).
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.editor().active_window().root == remote_root)
        .unwrap();
}
