//! The Orchestrator Open dialog must list every session, regardless of
//! which project the active window happens to point at.
//!
//! Sessions are inherently cross-project — each row can have its own
//! `project_path` — so scoping the picker to "current project" hides
//! exactly the rows the user came to switch into. The visible symptom
//! is that opening the dialog, visiting a session in another project,
//! then reopening the dialog changes the visible row count, because
//! the dialog's notion of "current project" is recomputed from the
//! newly-active window every time it opens.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh_core::api::PluginCommand;
use fresh_core::WindowId;
use serde_json::Value;
use std::path::Path;

const WIDTH: u16 = 160;
const HEIGHT: u16 = 40;

const LABEL_B: &str = "zebra-beta-xr";

fn run_palette(harness: &mut EditorTestHarness, command_name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(command_name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn set_orch_project_path(harness: &mut EditorTestHarness, project_path: &Path) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetWindowState {
            plugin_name: "orchestrator".into(),
            key: "project_path".into(),
            value: Some(Value::String(project_path.to_string_lossy().into_owned())),
        })
        .unwrap();
}

#[test]
fn open_dialog_lists_sessions_from_all_projects() {
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();

    // Project A: the harness's temp project root, owned by the base
    // window (id 1, active at boot).
    let proj_a = harness.project_dir().unwrap().canonicalize().unwrap();
    set_orch_project_path(&mut harness, &proj_a);

    // Project B: a separate tempdir, owned by a second window we
    // create explicitly. Per-session plugin state always writes to
    // the *active* window, so we set B active before tagging.
    let proj_b_dir = tempfile::TempDir::new().unwrap();
    let proj_b = proj_b_dir.path().canonicalize().unwrap();
    let win_b = harness
        .editor_mut()
        .create_window_at(proj_b.clone(), LABEL_B.into());
    harness.editor_mut().set_active_window(win_b);
    set_orch_project_path(&mut harness, &proj_b);

    // Switch back to A so the dialog opens with currentProject = A —
    // the case the bug repro turns on.
    harness.editor_mut().set_active_window(WindowId(1));
    harness.render().unwrap();

    run_palette(&mut harness, "Orchestrator: Open");
    harness
        .wait_until(|h| h.screen_to_string().contains("Sessions ("))
        .expect("Orchestrator Open dialog should appear");

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(LABEL_B),
        "Project B's session must be listed in the open dialog while the \
         active window is in Project A — the picker is cross-project by \
         design.\nScreen:\n{}",
        screen,
    );
    assert!(
        screen.contains("Sessions (2)"),
        "Header count must reflect every session, not the project-filtered \
         subset.\nScreen:\n{}",
        screen,
    );
}
