//! Reopen Closed File: a per-window list of the files closed in it.
//!
//! - It is reached from the command palette (and the File menu).
//! - It lists only files that are closed — not ones open in the window —
//!   most recently closed first.
//! - Enter reopens the selected file; a file deleted since is dropped from
//!   the list instead of being opened as an empty buffer.
//! - Preview tabs that get replaced are not recorded.
//! - The list survives a relaunch of the same workspace.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use fresh::input::keybindings::Action;
use fresh::view::prompt::PromptType;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tempfile::TempDir;

/// A harness on a temp project holding `names`, each file containing its
/// own name.
fn project_with(names: &[&str]) -> (EditorTestHarness, PathBuf) {
    let harness = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let project = harness.project_dir().unwrap();
    for name in names {
        fs::write(project.join(name), format!("{name} contents\n")).unwrap();
    }
    (harness, project)
}

/// Open `path`, then close it.
fn open_and_close(harness: &mut EditorTestHarness, path: &Path) {
    harness.open_file(path).unwrap();
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::Close);
    harness.render().unwrap();
}

/// The labels the Reopen Closed File prompt offers, or `None` when no such
/// prompt is open.
fn offered(harness: &EditorTestHarness) -> Option<Vec<String>> {
    let prompt = harness.editor().active_window().prompt.as_ref()?;
    if prompt.prompt_type != PromptType::ReopenClosedFile {
        return None;
    }
    Some(prompt.suggestions.iter().map(|s| s.text.clone()).collect())
}

fn reopen(harness: &mut EditorTestHarness) {
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::ReopenClosedFile);
    harness.render().unwrap();
}

#[test]
fn palette_command_with_nothing_closed_reports_it() {
    let (mut harness, _) = project_with(&[]);

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Reopen Closed File").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Reopen Closed File");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(offered(&harness), None, "no prompt with nothing closed");
    harness.assert_screen_contains("No recently closed files in this workspace");
}

#[test]
fn lists_only_closed_files_most_recent_first() {
    let (mut harness, project) = project_with(&["a.txt", "b.txt", "c.txt"]);

    open_and_close(&mut harness, &project.join("a.txt"));
    open_and_close(&mut harness, &project.join("b.txt"));
    // c.txt stays open: it must not be offered.
    harness.open_file(&project.join("c.txt")).unwrap();

    reopen(&mut harness);
    assert_eq!(
        offered(&harness),
        Some(vec!["b.txt".to_string(), "a.txt".to_string()])
    );
    harness.assert_screen_contains("Reopen closed file:");
}

#[test]
fn enter_reopens_the_most_recently_closed_file() {
    let (mut harness, project) = project_with(&["a.txt", "b.txt"]);
    open_and_close(&mut harness, &project.join("a.txt"));
    open_and_close(&mut harness, &project.join("b.txt"));

    reopen(&mut harness);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(offered(&harness), None, "prompt closes on Enter");
    harness.assert_buffer_content("b.txt contents\n");

    // Reopened, so it is no longer offered; a.txt still is.
    reopen(&mut harness);
    assert_eq!(offered(&harness), Some(vec!["a.txt".to_string()]));
}

#[test]
fn typing_filters_and_reopens_the_match() {
    let (mut harness, project) = project_with(&["alpha.txt", "beta.txt"]);
    open_and_close(&mut harness, &project.join("alpha.txt"));
    open_and_close(&mut harness, &project.join("beta.txt"));

    reopen(&mut harness);
    harness.type_text("alp").unwrap();
    harness.render().unwrap();
    assert_eq!(offered(&harness), Some(vec!["alpha.txt".to_string()]));
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("alpha.txt contents\n");
}

#[test]
fn typed_text_matching_nothing_opens_nothing() {
    let (mut harness, project) = project_with(&["a.txt"]);
    open_and_close(&mut harness, &project.join("a.txt"));
    let buffers_before = harness.editor().active_window().buffer_metadata.len();

    reopen(&mut harness);
    harness.type_text("zzz-no-such-file").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.editor().active_window().buffer_metadata.len(),
        buffers_before,
        "no buffer is created for text that matched no closed file"
    );
}

#[test]
fn deleted_file_is_dropped_instead_of_opened() {
    let (mut harness, project) = project_with(&["gone.txt", "kept.txt"]);
    open_and_close(&mut harness, &project.join("kept.txt"));
    open_and_close(&mut harness, &project.join("gone.txt"));
    fs::remove_file(project.join("gone.txt")).unwrap();

    reopen(&mut harness);
    assert_eq!(
        offered(&harness),
        Some(vec!["gone.txt".to_string(), "kept.txt".to_string()])
    );
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("File no longer exists");
    assert!(
        !project.join("gone.txt").exists(),
        "the deleted file must not be recreated"
    );
    reopen(&mut harness);
    assert_eq!(offered(&harness), Some(vec!["kept.txt".to_string()]));
}

#[test]
fn replaced_preview_tab_is_not_recorded() {
    let (mut harness, project) = project_with(&["alpha.txt", "beta.txt"]);
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("alpha.txt").unwrap();
    harness.wait_for_file_explorer_item("beta.txt").unwrap();

    // Single-click previews alpha, then beta, which replaces alpha's
    // preview tab.
    single_click_explorer_file(&mut harness, "alpha.txt");
    single_click_explorer_file(&mut harness, "beta.txt");
    assert!(harness.screen_row_text(1).contains("(preview)"));

    reopen(&mut harness);
    assert_eq!(
        offered(&harness),
        None,
        "a replaced preview is not 'closed'"
    );

    // A real tab closed is recorded.
    open_and_close(&mut harness, &project.join("alpha.txt"));
    reopen(&mut harness);
    assert_eq!(offered(&harness), Some(vec!["alpha.txt".to_string()]));
}

/// Single-click `name` in the file explorer (opens it as a preview tab).
fn single_click_explorer_file(harness: &mut EditorTestHarness, name: &str) {
    let dc_window = Duration::from_millis(
        harness
            .config()
            .editor
            .double_click_time_ms
            .saturating_mul(2),
    );
    harness.advance_time(dc_window);
    let screen = harness.screen_to_string();
    let row = screen
        .lines()
        .enumerate()
        .skip(2)
        .find(|(_, line)| line.chars().take(40).collect::<String>().contains(name))
        .map(|(row, _)| row as u16)
        .unwrap_or_else(|| panic!("{name} not in explorer;\n{screen}"));
    harness.mouse_click(10, row).unwrap();
    harness.render().unwrap();
}

#[test]
fn closed_files_survive_a_relaunch() {
    let temp_dir = TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    fs::write(project.join("a.txt"), "a contents\n").unwrap();
    fs::write(project.join("b.txt"), "b contents\n").unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let launch = || {
        EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_working_dir(project.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap()
    };

    {
        let mut harness = launch();
        harness.startup(true, &[]).unwrap();
        open_and_close(&mut harness, &project.join("a.txt"));
        // b.txt is open at quit: it comes back as a tab, not in the list.
        harness.open_file(&project.join("b.txt")).unwrap();
        harness.shutdown(true).unwrap();
    }

    let mut harness = launch();
    assert!(harness.startup(true, &[]).unwrap(), "workspace restores");
    reopen(&mut harness);
    assert_eq!(offered(&harness), Some(vec!["a.txt".to_string()]));
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("a contents\n");
}

#[test]
fn each_window_has_its_own_list() {
    let (mut harness, project) = project_with(&["first.txt"]);
    let first_window = harness.editor().active_window_id();
    open_and_close(&mut harness, &project.join("first.txt"));

    let second_root = TempDir::new().unwrap();
    fs::write(second_root.path().join("second.txt"), "second\n").unwrap();
    let second_window = harness
        .editor_mut()
        .create_window_at(second_root.path().to_path_buf(), "second".to_string());
    harness.editor_mut().set_active_window(second_window);

    reopen(&mut harness);
    assert_eq!(offered(&harness), None, "a new window starts with no list");

    open_and_close(&mut harness, &second_root.path().join("second.txt"));
    reopen(&mut harness);
    assert_eq!(offered(&harness), Some(vec!["second.txt".to_string()]));
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    harness.editor_mut().set_active_window(first_window);
    reopen(&mut harness);
    assert_eq!(offered(&harness), Some(vec!["first.txt".to_string()]));
}
