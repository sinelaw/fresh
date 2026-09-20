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
fn drop_into_focused_explorer_imports_without_a_command_or_enter() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let sources = tempfile::tempdir().unwrap();
    let first = sources.path().join("한글 image.txt");
    let second = sources.path().join("second.txt");
    std::fs::write(&first, "FIRST_DROPPED_CONTENT").unwrap();
    std::fs::write(&second, "SECOND_DROPPED_CONTENT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("File Explorer").unwrap();
    h.send_paste(&format!("{} {}", quoted(&first), quoted(&second)))
        .unwrap();
    // A pre-change build pastes the paths into the buffer instead. Assert
    // before waiting so the reproducer fails immediately on that build.
    h.assert_screen_not_contains(&sources.path().display().to_string());
    h.wait_for_screen_contains("Imported 2 files; skipped 0")
        .unwrap();
    assert!(h
        .screen_to_string()
        .replace(' ', "")
        .contains("한글image.txt"));
    h.assert_screen_contains("second.txt");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("SECOND_DROPPED_CONTENT")
        .unwrap();
}

#[test]
fn direct_drop_uses_selected_files_parent_and_prompts_for_conflicts() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let project = h.project_dir().unwrap().to_path_buf();
    std::fs::create_dir(project.join("assets")).unwrap();
    std::fs::write(project.join("assets/asset.txt"), "OLD_CONTENT").unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("asset.txt");
    std::fs::write(&source, "DIRECT_DROP_CONTENT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("assets").unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("asset.txt").unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("OLD_CONTENT").unwrap();
    h.send_paste(&quoted(&source)).unwrap();
    h.wait_for_screen_contains("Name Conflict").unwrap();
    h.send_key(KeyCode::Char('o'), KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("DIRECT_DROP_CONTENT").unwrap();
}

#[test]
fn explorer_rejects_non_path_pastes_without_editing_the_buffer() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    h.type_text("UNCHANGED_BUFFER").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("File Explorer").unwrap();
    h.send_paste("not a file drop").unwrap();
    h.assert_screen_contains("use absolute local file paths");
    h.assert_screen_not_contains("not a file drop");
    h.assert_screen_contains("UNCHANGED_BUFFER");
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
    h.send_paste(&quoted(&source)).unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.assert_screen_contains("uploaded.txt");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("UPLOADED_CONTENT").unwrap();
}

#[test]
fn cancelled_batch_refreshes_files_already_imported() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    std::fs::write(h.project_dir().unwrap().join("conflict.txt"), "KEEP").unwrap();
    let sources = tempfile::tempdir().unwrap();
    let first = sources.path().join("completed.txt");
    let conflict = sources.path().join("conflict.txt");
    std::fs::write(&first, "COMPLETED_BEFORE_CANCEL").unwrap();
    std::fs::write(&conflict, "NEW").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("conflict.txt").unwrap();
    h.send_paste(&format!("{} {}", quoted(&first), quoted(&conflict)))
        .unwrap();
    h.wait_for_screen_contains("Name Conflict").unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.assert_screen_contains("completed.txt");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("COMPLETED_BEFORE_CANCEL")
        .unwrap();
}

fn exercise_folder_drop(h: &mut EditorTestHarness) {
    let sources = tempfile::tempdir().unwrap();
    let folder = sources.path().join("folder with spaces");
    std::fs::create_dir_all(folder.join("nested/empty")).unwrap();
    std::fs::write(folder.join("nested/한글.txt"), "NESTED_DROP_CONTENT").unwrap();
    std::fs::write(folder.join("top.txt"), "TOP_DROP_CONTENT").unwrap();
    let extra = sources.path().join("extra.txt");
    std::fs::write(&extra, "EXTRA_DROP_CONTENT").unwrap();
    h.send_paste(&format!("{} {}", quoted(&folder), quoted(&extra)))
        .unwrap();
    // Also terminates on the old implementation's error summary, so this
    // reproducer fails without waiting for a success that cannot arrive.
    h.wait_for_screen_contains("Imported").unwrap();
    h.assert_screen_contains("Imported 3 files and 3 folders; skipped 0");
    h.assert_screen_contains("folder with spaces");
    h.assert_screen_contains("empty");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("EXTRA_DROP_CONTENT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    // Tree is expanded after the batch; find the nested file by rendered row.
    let row = h
        .screen_to_string()
        .lines()
        .position(|line| line.contains("한") && line.contains(".txt"))
        .unwrap();
    h.mouse_click(10, row as u16).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("NESTED_DROP_CONTENT").unwrap();
}

#[test]
fn folder_drop_copies_nested_and_empty_directories() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("File Explorer").unwrap();
    exercise_folder_drop(&mut h);
}

#[test]
fn folder_drop_uploads_nested_and_empty_directories_through_agent() {
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
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("[localhost]").unwrap();
    exercise_folder_drop(&mut h);
}

#[test]
fn folder_drop_merges_existing_tree_and_renames_conflicts_inside_it() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let project = h.project_dir().unwrap().to_path_buf();
    std::fs::create_dir_all(project.join("bundle/nested")).unwrap();
    std::fs::write(project.join("bundle/nested/data.txt"), "KEEP_EXISTING").unwrap();
    std::fs::write(project.join("bundle/nested/untouched.txt"), "UNRELATED").unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("bundle");
    std::fs::create_dir_all(source.join("nested")).unwrap();
    std::fs::write(source.join("nested/data.txt"), "RENAMED_NESTED_CONTENT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("bundle").unwrap();
    // The explorer compacts bundle/nested into one row. Expand it, then
    // click the rendered root to restore the drop destination and focus.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("untouched.txt").unwrap();
    let root_row = h
        .screen_to_string()
        .lines()
        .position(|line| line.contains("project_root"))
        .unwrap();
    h.mouse_click(5, root_row as u16).unwrap();
    h.send_paste(&quoted(&source)).unwrap();
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("Name Conflict") || screen.contains("Imported")
    })
    .unwrap();
    h.assert_screen_contains("Name Conflict");
    h.send_key(KeyCode::Char('r'), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text("renamed.txt").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files and 2 folders; skipped 0")
        .unwrap();
    h.assert_screen_contains("untouched.txt");
    h.assert_screen_contains("data.txt");
    h.assert_screen_contains("renamed.txt");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("RENAMED_NESTED_CONTENT")
        .unwrap();
}

#[test]
fn folder_drop_cancel_retains_created_empty_folders() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    let project = h.project_dir().unwrap().to_path_buf();
    std::fs::create_dir(project.join("bundle")).unwrap();
    std::fs::write(project.join("bundle/conflict.txt"), "KEEP").unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("bundle");
    std::fs::create_dir_all(source.join("a-empty")).unwrap();
    std::fs::write(source.join("conflict.txt"), "NEW").unwrap();
    std::fs::write(source.join("later.txt"), "MUST_NOT_IMPORT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("bundle").unwrap();
    h.send_paste(&quoted(&source)).unwrap();
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("Name Conflict") || screen.contains("Imported")
    })
    .unwrap();
    h.assert_screen_contains("Name Conflict");
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 0 files and 2 folders; skipped 0")
        .unwrap();
    h.assert_screen_contains("Paste cancelled");
    h.assert_screen_contains("a-empty");
    h.assert_screen_not_contains("later.txt");
}

#[test]
fn folder_drop_renames_a_folder_when_destination_is_a_file() {
    let mut h = EditorTestHarness::with_temp_project(180, 32).unwrap();
    std::fs::write(h.project_dir().unwrap().join("bundle"), "KEEP_FILE").unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("bundle");
    std::fs::create_dir_all(source.join("empty")).unwrap();
    std::fs::write(source.join("inside.txt"), "RENAMED_FOLDER_CONTENT").unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("bundle").unwrap();
    h.send_paste(&quoted(&source)).unwrap();
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("Name Conflict") || screen.contains("Imported")
    })
    .unwrap();
    h.assert_screen_contains("Name Conflict");
    h.send_key(KeyCode::Char('r'), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text("renamed-folder").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("Imported 1 files and 2 folders; skipped 0")
        .unwrap();
    h.assert_screen_contains("renamed-folder");
    h.assert_screen_contains("empty");
    h.assert_screen_contains("bundle");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("RENAMED_FOLDER_CONTENT")
        .unwrap();
}
