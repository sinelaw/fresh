//! E2E: submitting a *remote* (SSH) workspace whose host isn't reachable yet
//! is non-blocking. Instead of parking the editor behind a modal
//! "Connecting… press Cancel to abort" dialog, the workspace shows up in the
//! orchestrator dock as its own "Connecting…" row while the connect proceeds
//! in the background — so the user can keep working or switch elsewhere even
//! though it may never connect.
//!
//! Deterministic via the `tests/fixtures/fake-ssh-hang` shim: the host
//! accepts the connection but never completes the handshake, so the connect
//! stays in-flight for the whole test with no network.
//!
//! Single test in this binary: the fake-ssh PATH shim and
//! `isolated_dir_context`'s process-global `XDG_DATA_HOME` must not leak into
//! other test binaries.
#![cfg(all(target_os = "linux", feature = "plugins"))]

mod common;

use common::dormant_ssh::{ensure_hanging_fake_ssh_on_path, isolated_dir_context};
use common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn ssh_submit_is_non_blocking_and_shows_connecting_row() {
    ensure_hanging_fake_ssh_on_path();
    fresh::i18n::set_locale("en");
    let base = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(base.path());
    let project = base.path().join("project");
    std::fs::create_dir_all(&project).unwrap();
    let project = project.canonicalize().unwrap();

    let plugins_dir = project.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    let mut h = EditorTestHarness::create(
        160,
        50,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context),
    )
    .unwrap();
    h.tick_and_render().unwrap();
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all()
            .iter()
            .any(|c| c.get_localized_name() == "Orchestrator: New Workspace")
    })
    .unwrap();

    // Open the form.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: New Workspace").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: New Workspace"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        h.screen_to_string()
            .contains("ORCHESTRATOR :: New Workspace")
    })
    .unwrap();

    // Switch "Run in:" from Local to SSH (Shift+Tab wraps focus onto the
    // selector, → advances to SSH and swaps the body), then Tab into the SSH
    // body's first field (Host) and type a host.
    h.send_key(KeyCode::BackTab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Host  ("))
        .unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.type_text("dead-host").unwrap();

    // Submit — the connect starts in the background and the form closes.
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();

    // A "Connecting…" dock row appears — not a modal.
    h.wait_until(|h| h.screen_to_string().contains("Connecting"))
        .unwrap();
    let s = h.screen_to_string();
    assert!(
        !s.contains("ORCHESTRATOR :: New Workspace") && !s.contains("press Cancel to abort"),
        "SSH submit must be non-blocking (a dock row, not a modal Cancel dialog). Screen:\n{s}",
    );
    assert!(
        s.contains("ssh:dead-host"),
        "the connecting SSH workspace should be listed by its host label. Screen:\n{s}",
    );
}
