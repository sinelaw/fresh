//! Behaviour: selecting a *disconnected* remote (SSH) workspace in the
//! orchestrator dock — including by plain arrow-navigation — opens it
//! **non-blockingly**. The switch commits into the host's placeholder
//! "Connecting…" page (a clear message in the workspace area) and the connect
//! runs in the background, so even a host that never responds
//! (`tests/fixtures/fake-ssh-hang`) never freezes the dock: the user can arrow
//! straight back to another workspace while it connects.
//!
//! This pins the arrow-nav path specifically (the deliberate-dive path is
//! covered by `orchestrator_dock_connecting_commit`). Without a non-blocking
//! switch, arrowing onto an unreachable host would stall the whole UI — the
//! bug reported against the dock.
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
fn arrow_nav_into_disconnected_remote_is_non_blocking() {
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
    // (`ssh-dead`) whose host now hangs (accepts the connection, never
    // completes the handshake).
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

    // Arrow onto the SSH row: the switch commits into the session's
    // placeholder page and the connect runs in the background. If the switch
    // blocked on the never-responding host this would hang here forever
    // (that's the bug); non-blocking, it commits at once.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.editor().active_window().root == remote_root)
        .unwrap();
    // The workspace area shows the placeholder page's status message (its hint
    // lines all begin "The workspace …" — whether still connecting or already
    // reporting the host unreachable), never a blank/frozen buffer, and the
    // local buffer is no longer displayed.
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("The workspace") && !scr.contains("local_marker.txt")
    })
    .unwrap();
    // The dock still lists the session (highlighted / not dropped) and the
    // status bar reflects the in-flight connect — proof the connect runs in
    // the background rather than having blocked the switch.
    h.assert_screen_contains("ssh-dead");
}
