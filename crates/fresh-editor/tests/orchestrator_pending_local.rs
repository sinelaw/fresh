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

    // A distinct, non-git target dir for the new workspace, so its row carries
    // a name (`task_ws`) that can't be confused with the launch session or with
    // generic dock chrome.
    let task_dir = base.path().join("task_ws");
    std::fs::create_dir_all(&task_dir).unwrap();
    let task_dir = task_dir.canonicalize().unwrap();

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

    // Point the Project Path at the distinct non-git dir, then submit.
    // Ctrl+Enter submits from the text field.
    h.type_text(&task_dir.display().to_string()).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("task_ws"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::CONTROL).unwrap();

    // The form is gone and the dock is showing — the workspace now lives in the
    // list, not behind a modal. Crucially, assert the *row itself* is present:
    // the new workspace appears in the dock by name (`task_ws`), so a
    // regression where submit opens the dock but drops no pending/real row
    // would fail here — `New Task` alone is generic toolbar chrome that renders
    // whenever the dock opens. The non-blocking create resolves fast for a
    // plain (non-git, no-worktree) dir, so the row may already have swapped
    // from its transient `Creating…` state to the live session by now; either
    // way the workspace's own row must be listed.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("task_ws") && !s.contains("ORCHESTRATOR :: New Workspace")
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
