//! Helpers for driving the orchestrator's launch form (New Workspace / Run
//! Agent) from tests.
//!
//! The form opens with focus in its prompt box (or on the agent selector,
//! when the agent takes no prompt), so a test that wants a particular field
//! walks there with [`focus_stop`] instead of assuming where focus starts.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// The New Workspace frame's title, as its top border draws it. The palette
/// row that opens the form reads `Orchestrator: New Workspace`, so the
/// frame's corner is what tells the two apart.
pub const FORM_TITLE: &str = "┌ New Workspace";

/// The same dialog in its Here shape.
pub const RUN_TITLE: &str = "┌ Run Agent";

/// Whether a control whose line contains `needle` has focus (carries the
/// `▸` marker).
pub fn is_focused(harness: &EditorTestHarness, needle: &str) -> bool {
    harness
        .screen_to_string()
        .lines()
        .any(|l| l.contains('▸') && l.contains(needle))
}

/// Tab until the focused control's line contains `needle`.
pub fn focus_stop(harness: &mut EditorTestHarness, needle: &str) {
    let mut guard = 0;
    while !is_focused(harness, needle) {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.tick_and_render().unwrap();
        guard += 1;
        assert!(
            guard < 30,
            "Tab never reached the {needle:?} stop. Screen:\n{}",
            harness.screen_to_string(),
        );
    }
}

/// Open the launch form from the command palette and wait for its frame.
/// A first launch has no agent chosen yet, so this picks `terminal` (see
/// [`choose_terminal_agent`]); focus is then on the agent selector.
pub fn open_new_workspace_form(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Orchestrator: New Workspace").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Orchestrator: New Workspace"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(FORM_TITLE))
        .unwrap();
    choose_terminal_agent(harness);
}

/// A first launch has no agent chosen (`Choose an agent…`), and Launch waits
/// for one. Pick `terminal` — the list's first real entry, under the
/// placeholder — so a test that launches can. A no-op once an agent is set.
pub fn choose_terminal_agent(harness: &mut EditorTestHarness) {
    if !harness.screen_to_string().contains("[Choose an agent…") {
        return;
    }
    focus_stop(harness, "▸ Agent:");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("│ terminal"))
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Agent: [terminal"))
        .unwrap();
}

/// Save an ssh machine, as Add Machine would, before the editor starts. The
/// orchestrator imports `<data dir>/orchestrator/machines/<id>.json` the first
/// time it reads the machine list. A workspace runs only on a saved machine,
/// so this is how a test gets a remote to pick.
pub fn plant_saved_ssh_machine(data_dir: &std::path::Path, id: &str, name: &str, target: &str) {
    let dir = data_dir.join("orchestrator").join("machines");
    std::fs::create_dir_all(&dir).unwrap();
    let json = serde_json::json!({ "id": id, "name": name, "kind": "ssh", "target": target });
    std::fs::write(dir.join(format!("{id}.json")), json.to_string()).unwrap();
}
