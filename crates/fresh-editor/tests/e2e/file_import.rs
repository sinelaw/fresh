//! Import workflows driven through keyboard/paste events, observed on screen.
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

fn start_import(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text("Import Local Files").unwrap();
    // Immediate assertion also makes a missing command fail clearly on the
    // pre-feature tree, instead of waiting for a command that cannot appear.
    assert!(h.screen_to_string().matches("Import Local Files").count() >= 2);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.assert_screen_contains("Drop or paste absolute local file paths");
}

fn quoted(path: &std::path::Path) -> String {
    format!("\"{}\"", path.display())
}

#[test]
fn imports_multiple_dropped_files_and_refreshes_explorer() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let sources = tempfile::tempdir().unwrap();
    let first = sources.path().join("한글 image.txt");
    let second = sources.path().join("second.txt");
    std::fs::write(&first, "FIRST_IMPORTED_CONTENT").unwrap();
    std::fs::write(&second, "SECOND_IMPORTED_CONTENT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("File Explorer").unwrap();
    start_import(&mut h);
    h.send_paste(&format!("{} {}", quoted(&first), quoted(&second)))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 2 files; skipped 0")
        .unwrap();
    // TestBackend includes the trailing cell of each wide glyph as a space.
    assert!(h
        .screen_to_string()
        .replace(' ', "")
        .contains("한글image.txt"));
    h.assert_screen_contains("second.txt");
    // Completion selects the destination: Enter opens its imported content.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("SECOND_IMPORTED_CONTENT")
        .unwrap();
}

#[test]
fn import_conflicts_offer_skip_rename_and_overwrite() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("asset.txt");
    std::fs::write(&source, "NEW_ASSET_CONTENT").unwrap();
    std::fs::write(
        h.project_dir().unwrap().join("asset.txt"),
        "OLD_ASSET_CONTENT",
    )
    .unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("asset.txt").unwrap();
    start_import(&mut h);
    h.send_paste(&quoted(&source)).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Name Conflict").unwrap();
    h.assert_screen_contains("Overwrite");
    h.assert_screen_contains("Rename");
    h.send_key(KeyCode::Char('s'), KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 0 files; skipped 1")
        .unwrap();

    start_import(&mut h);
    h.send_paste(&quoted(&source)).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Name Conflict").unwrap();
    h.send_key(KeyCode::Char('r'), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text("renamed.txt").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.assert_screen_contains("asset.txt");
    h.assert_screen_contains("renamed.txt");

    start_import(&mut h);
    h.send_paste(&quoted(&source)).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Name Conflict").unwrap();
    h.send_key(KeyCode::Char('o'), KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("NEW_ASSET_CONTENT").unwrap();
}

#[test]
fn ordinary_path_paste_stays_text_and_import_prompt_can_cancel() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("ordinary.txt");
    std::fs::write(&source, "MUST_NOT_APPEAR").unwrap();
    h.send_paste(&quoted(&source)).unwrap();
    h.assert_screen_contains("ordinary.txt");
    h.assert_screen_not_contains("MUST_NOT_APPEAR");
    start_import(&mut h);
    h.send_paste(&quoted(&source)).unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.assert_screen_not_contains("Drop or paste absolute local file paths");
    h.assert_screen_not_contains("Imported 1 files");
    h.assert_screen_not_contains("MUST_NOT_APPEAR");
}

#[test]
fn import_uses_selected_directory_and_accepts_unbracketed_input() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let project = h.project_dir().unwrap().to_path_buf();
    std::fs::create_dir(project.join("assets")).unwrap();
    std::fs::write(project.join("assets/keep.txt"), "KEEP").unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("added.txt");
    std::fs::write(&source, "ADDED_TO_ASSETS").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("assets").unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("keep.txt").unwrap();
    start_import(&mut h);
    h.assert_screen_contains(&project.join("assets").display().to_string());
    h.type_text(&quoted(&source)).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.assert_screen_contains("added.txt");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("ADDED_TO_ASSETS").unwrap();
}

#[test]
fn cancelling_a_conflict_does_not_import_remaining_files() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    std::fs::write(h.project_dir().unwrap().join("existing.txt"), "KEEP").unwrap();
    let sources = tempfile::tempdir().unwrap();
    let conflict = sources.path().join("existing.txt");
    let later = sources.path().join("should-not-import.txt");
    std::fs::write(&conflict, "NEW").unwrap();
    std::fs::write(&later, "LATER").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("existing.txt").unwrap();
    start_import(&mut h);
    h.send_paste(&format!("{} {}", quoted(&conflict), quoted(&later)))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Name Conflict").unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.assert_screen_not_contains("Name Conflict");
    h.assert_screen_not_contains("should-not-import.txt");
    h.assert_screen_contains("Imported 0 files; skipped 0");
    // A cancelled batch leaves the command available for another import.
    start_import(&mut h);
}

#[test]
fn overwriting_refreshes_an_open_buffer_and_refuses_unsaved_edits() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let destination = h.project_dir().unwrap().join("asset.txt");
    std::fs::write(&destination, "OLD_CONTENT").unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("asset.txt");
    std::fs::write(&source, "NEW_CONTENT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("asset.txt").unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("OLD_CONTENT").unwrap();
    start_import(&mut h);
    h.send_paste(&quoted(&source)).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Name Conflict").unwrap();
    h.send_key(KeyCode::Char('o'), KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.wait_for_screen_contains("NEW_CONTENT").unwrap();
    h.assert_screen_not_contains("OLD_CONTENT");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.type_text("UNSAVED_").unwrap();
    start_import(&mut h);
    h.send_paste(&quoted(&source)).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Name Conflict").unwrap();
    h.send_key(KeyCode::Char('o'), KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("destination has unsaved changes")
        .unwrap();
    h.assert_screen_contains("UNSAVED_");
}

#[test]
fn imports_into_remote_workspace_through_agent() {
    use crate::common::harness::HarnessOptions;
    use fresh::services::remote::{spawn_local_agent, RemoteFileSystem};
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let channel = runtime.block_on(spawn_local_agent()).unwrap();
    channel.set_request_timeout(std::time::Duration::from_secs(60 * 60));
    let remote = std::sync::Arc::new(RemoteFileSystem::new(channel, "test@localhost".into()));
    let project = tempfile::tempdir().unwrap();
    let mut h = EditorTestHarness::create(
        180,
        32,
        HarnessOptions::new()
            .with_working_dir(project.path().to_path_buf())
            .with_filesystem(remote),
    )
    .unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("uploaded.txt");
    std::fs::write(&source, "UPLOADED_CONTENT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("[localhost]").unwrap();
    start_import(&mut h);
    h.send_paste(&quoted(&source)).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.assert_screen_contains("uploaded.txt");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("UPLOADED_CONTENT").unwrap();
}
