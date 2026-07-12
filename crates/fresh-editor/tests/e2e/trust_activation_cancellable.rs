//! Regression: the workspace-trust prompt's secondary button follows *why* it
//! was raised.
//!
//! The mandatory open-time gate (nothing else to fall back to yet) offers
//! **Quit**. But when the prompt is raised by *activating another workspace* on
//! an already-running editor — opening/switching an Orchestrator session, or
//! changing the project — the secondary must be **Cancel**: dismissing it has
//! to leave the just-activated workspace Restricted, never tear the whole
//! editor (and every other open session) down.
//!
//! Before the fix both paths went through `show_workspace_trust_popup(false)`,
//! so activating a session that tripped the trust prompt showed "Quit" and
//! pressing it exited the editor.

use crate::common::harness::EditorTestHarness;

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;

/// Wire a real per-project trust store for the harness's current working dir
/// and drop a manifest so the folder has executable-content markers (which is
/// what makes an undecided folder raise the prompt).
fn arm_undecided_project_with_markers(harness: &mut EditorTestHarness) {
    let dir = harness.editor().working_dir().to_path_buf();
    std::fs::write(dir.join("Cargo.toml"), "[package]\nname = \"x\"\n").unwrap();
    let store_path = harness
        .editor()
        .dir_context()
        .project_state_dir(&dir);
    let store = fresh::services::workspace_trust::TrustStore::for_project_dir(&store_path);
    harness
        .editor()
        .authority()
        .workspace_trust
        .set_store(Some(store));
}

#[test]
fn activation_trust_prompt_offers_cancel_not_quit() {
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    arm_undecided_project_with_markers(&mut harness);

    // Raise the prompt the way an in-editor workspace activation does.
    harness.editor_mut().maybe_prompt_workspace_trust(true);
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("SECURITY WARNING"))
        .expect("an undecided project with a manifest must raise the trust prompt");

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Cancel"),
        "an activation-raised trust prompt must offer Cancel.\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("Quit"),
        "an activation-raised trust prompt must NOT offer Quit (that would kill \
         the editor and every other open session).\nScreen:\n{screen}"
    );
}

#[test]
fn open_time_gate_trust_prompt_offers_quit() {
    // The counterpart: the mandatory open-time gate (`cancellable = false`)
    // still offers Quit — there is nothing to fall back to before the first
    // trust decision is made.
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    arm_undecided_project_with_markers(&mut harness);

    harness.editor_mut().maybe_prompt_workspace_trust(false);
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("SECURITY WARNING"))
        .expect("an undecided project with a manifest must raise the trust prompt");

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Quit"),
        "the open-time gate must offer Quit.\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("Cancel"),
        "the open-time gate must NOT offer Cancel.\nScreen:\n{screen}"
    );
}
