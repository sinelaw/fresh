//! E2E coverage for the Recent / Pinned Projects picker (issue #1895).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Seeding the recent-projects store and running "Open Recent Project" from the
/// command palette opens a picker listing the stored projects by name, with the
/// pinned one marked.
#[test]
fn test_open_recent_project_picker_lists_seeded_projects() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();

    // Seed the store the editor reads from. The picker loads it fresh each time
    // it opens, so writing after construction is fine. These paths are distinct
    // from the temp working directory, so neither is filtered out as "current".
    let data_dir = harness.editor().dir_context().data_dir.clone();
    std::fs::create_dir_all(&data_dir).unwrap();
    let store = r#"{
      "version": 1,
      "projects": [
        {"path": "/tmp/zzRecentAlphaProject", "pinned": false, "last_opened": 200},
        {"path": "/tmp/zzPinnedBetaProject",  "pinned": true,  "last_opened": 100}
      ]
    }"#;
    std::fs::write(data_dir.join("recent_projects.json"), store).unwrap();

    // Open the command palette and run the "Open Recent Project" command.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Open Recent Project").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The picker lists both projects by their directory name. The pinned one is
    // marked with a star and (being pinned) sorts to the top.
    harness.assert_screen_contains("zzRecentAlphaProject");
    harness.assert_screen_contains("zzPinnedBetaProject");
    harness.assert_screen_contains("★");
}
