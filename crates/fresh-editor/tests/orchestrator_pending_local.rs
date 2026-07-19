//! E2E: the *non-blocking* local New-Workspace flow. Submitting the form no
//! longer parks the whole editor behind a modal "Creating…" dialog with a
//! lone Cancel button — the form closes at once and the new workspace shows
//! up as its own row in the orchestrator dock while the rest of the UI stays
//! live. (The old blocking flow dove straight into the new terminal window,
//! so the dock's toolbar chrome never appeared — this times out against it.)
//!
//! Single test in this binary: `isolated_dir_context` sets the process-global
//! `XDG_DATA_HOME` so the session this test creates checkpoints into the
//! per-test temp tree, never the real user data dir.
#![cfg(all(target_os = "linux", feature = "plugins"))]

mod common;

use common::dormant_ssh::isolated_dir_context;
use common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn local_submit_closes_form_and_shows_dock_row() {
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

    // Open the New Workspace form.
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

    // Submit with the default project path (the workspace itself). Ctrl+Enter
    // submits from the initial text field.
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();

    // The form is gone and the dock (its "New Task…" toolbar) is showing —
    // the workspace now lives in the list, not behind a modal.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("New Task") && !s.contains("ORCHESTRATOR :: New Workspace")
    })
    .unwrap();

    // And the blocking modal's abort line is nowhere — the flow never parked
    // the editor behind a Cancel-only dialog.
    assert!(
        !h.screen_to_string().contains("press Cancel to abort"),
        "non-blocking submit must not show the modal connecting view. Screen:\n{}",
        h.screen_to_string(),
    );
}
