//! Behaviour: arrow-navigating the orchestrator dock onto a **slow / high-
//! latency** SSH workspace keeps the whole editor responsive while the session
//! connects and materializes in the background.
//!
//! This is the companion to `orchestrator_dock_ssh_navigation_nonblocking.rs`.
//! That test uses a host that never establishes the channel (`fake-ssh-hang`),
//! so `is_connected()` stays false and every remote request fails fast — the
//! switch is trivially non-blocking. The bug this test pins is the *opposite*
//! shape: a host that **does** connect but is bandwidth-throttled
//! (`fake-ssh-slow`, cf. `ProxyCommand … | pv -qL 20k`). There the channel
//! comes up and the agent answers, so promoting the dived-into session
//! re-reads its persisted buffers over the slow link. Done on the editor loop,
//! those reads froze the whole UI for seconds — the user could not arrow to
//! another window until the reads returned.
//!
//! The fix restores the session's persisted *file* buffers as empty
//! placeholders (no I/O on the editor loop) and loads their content off-loop,
//! filling each in via `AsyncMessage::RemoteBufferContentLoaded`. So the layout
//! and tabs come up instantly, the loop never blocks on a slow read, and the
//! content still appears once it arrives.
//!
//! Reproducer shape: the slow shim holds the file `read` open indefinitely (it
//! dribbles keepalive chunks so the request never times out). While it's held,
//! the editor stays responsive — driving frames all complete (loading on the
//! loop would freeze here and nextest's per-test cap would fail the test) and
//! the placeholder shows no content. Releasing the read then fills the buffer,
//! so the file's real content appears.
//!
//! The fake-ssh PATH shim and the `FAKE_SSH_SLOW_*` variables that configure
//! it are process-global, so both ride a `PathPin` that puts them back when
//! the test ends; persistence rides a thread-local data-dir pin
//! (`isolated_dir_context`).
#![cfg(all(target_os = "linux", feature = "plugins"))]

use crate::common::dormant_ssh::{
    canonical_mkdir, slow_fake_ssh_on_path, isolated_dir_context, persist_previous_session,
};
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn arrow_nav_onto_slow_remote_keeps_editor_responsive() {
    crate::common::tracing::init_tracing_from_env();
    let mut fake_ssh = slow_fake_ssh_on_path();
    fresh::i18n::set_locale("en");

    let base = tempfile::tempdir().unwrap();
    let (dir_context, _data_dir_pin) = isolated_dir_context(base.path());
    let project = canonical_mkdir(base.path(), "project");
    let remote_root = canonical_mkdir(base.path(), "remote-root");

    // Throttle the shim: hold every `read` open until this gate file is removed,
    // modelling a transfer that makes only trivial progress (the shim keeps the
    // request alive so it never times out). The gate lives under the per-test
    // temp tree, so it's cleaned up with everything else on teardown — which
    // also releases the held read on the connect worker.
    let gate = base.path().join("read.gate");
    std::fs::write(&gate, "hold").unwrap();
    // Through the pin, so both come back off when the shim does: left set,
    // they aim a *later* test's shim at a gate file under a temp directory
    // this one has already deleted.
    fake_ssh.set_env("FAKE_SSH_SLOW_METHODS", "read");
    fake_ssh.set_env("FAKE_SSH_SLOW_BLOCK_FILE", &gate);

    let plugins_dir = project.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    // Leaves behind a local project workspace + a dormant SSH session
    // (`ssh-dead`) with a real persisted buffer (`remote_notes.txt`) that
    // promoting the session will reopen over the (slow) link.
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

    // Arrow onto the SSH row: the switch commits into the session and the
    // connect resolves in the background. Promote restores the layout + tabs
    // immediately with the file buffer as an empty placeholder — no read on the
    // editor loop — so the tab for `remote_notes.txt` is up right away even
    // though its content read is still held open by the gate.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.editor().active_window().root == remote_root)
        .unwrap();
    h.wait_until(|h| h.screen_to_string().contains("remote_notes"))
        .unwrap();

    // The content read is held open by the gate, yet the editor loop keeps
    // turning — drive frames that must all complete. Loading content on the loop
    // would freeze here (the held read never returns) and nextest's per-test cap
    // would fail the test. Meanwhile the placeholder shows no content yet.
    for _ in 0..20 {
        h.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
    assert!(
        !h.screen_to_string().contains("REMOTE NOTES"),
        "file content must not appear while its read is still held"
    );

    // Release the read: the off-loop load completes and fills the placeholder,
    // so the file's real content appears. Proves the buffers do load — just
    // never on the editor loop.
    std::fs::remove_file(&gate).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("REMOTE NOTES"))
        .unwrap();
}
