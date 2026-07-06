//! Dock-level reproducer for issue #2570's display gaps: a dormant SSH
//! session restored from disk must be *presented* as a remote (disconnected)
//! workspace.
//!
//! Before the fix the dormant row carried no backend facet, so it rendered
//! exactly like a local session AND was swallowed by the dock's default
//! "hide trivial" filter — the user had to toggle "show empty" to even see
//! it. Driven end-to-end through the real dock UI (orchestrator plugin):
//! toggle the dock, see the badged row, arrow onto it, and observe the failed
//! dive land in the (empty) SSH workspace with the dock still listing the
//! session.
//!
//! Single test in this binary: the persistence isolation sets the
//! process-global `XDG_DATA_HOME` (see
//! `common::dormant_ssh::isolated_dir_context`).

mod common;

use common::dormant_ssh::{
    canonical_mkdir, ensure_fake_ssh_on_path, isolated_dir_context, persist_previous_session,
};
use common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
#[cfg_attr(target_os = "windows", ignore)] // fake-ssh shim is a Unix shell script
fn dock_lists_dormant_ssh_session_with_backend_badge_and_dive_commits() {
    common::tracing::init_tracing_from_env();
    ensure_fake_ssh_on_path();
    fresh::i18n::set_locale("en");

    let base = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(base.path());
    let project = canonical_mkdir(base.path(), "project");
    let remote_root = canonical_mkdir(base.path(), "remote-root");

    // The orchestrator plugin drives the dock; it lives in the launch
    // project's plugins dir.
    let plugins_dir = project.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    persist_previous_session(&dir_context, &project, &remote_root, true);

    // Animations off so the dock's live-switch slide completes instantly
    // under the test clock (same as the dock showcase test).
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
    // A visible buffer in the local workspace — the thing that must LEAVE
    // the screen when the dive commits. (Opened explicitly: the harness's
    // `create` doesn't run the production foreground-workspace restore.)
    h.open_file(&project.join("local_marker.txt")).unwrap();
    h.wait_for_screen_contains("local_marker.txt").unwrap();

    // Open the dock via the command palette.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The dormant SSH session is listed — WITHOUT flipping "show empty" —
    // and badged as a (disconnected) SSH backend: glyph + `user@host` detail
    // (the detail may be ellipsis-truncated on a narrow card, so match its
    // prefix).
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("ssh-dead") && scr.contains("⇅") && scr.contains("root@")
    })
    .unwrap();

    // Arrow onto the SSH row: the dock live-switches, the connect fails, and
    // the switch still commits — the editor lands in the (empty) SSH
    // workspace instead of silently staying on the local one.
    let local_root = h.editor().active_window().root.clone();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.editor().active_window().root != local_root)
        .unwrap();
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("Connection failed") && !scr.contains("local_marker.txt")
    })
    .unwrap();
    assert_eq!(
        h.editor().active_window().root,
        remote_root,
        "the dive must commit to the SSH workspace"
    );
    // The dock still lists the session (it must not vanish on failure).
    h.assert_screen_contains("ssh-dead");
}
