//! Recent Files: a per-window list of the files the user closed in it.
//!
//! Driven through keys (Alt+W closes a tab, the palette runs commands) and
//! observed on screen only:
//! - the palette and File menu reach it;
//! - it lists only closed files, most recently closed first, by path
//!   relative to the workspace;
//! - Enter reopens the selected file, typing filters, text matching nothing
//!   opens nothing;
//! - a file deleted since — outside the editor or from the explorer — is not
//!   offered or reopened;
//! - unnamed buffers and replaced preview tabs are not recorded;
//! - each window has its own list, and it survives a relaunch.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tempfile::TempDir;

const PROMPT_LABEL: &str = "Recent files:";

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

fn palette(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(command).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn key(harness: &mut EditorTestHarness, code: KeyCode) {
    harness.send_key(code, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Close the active tab the way a user does (Alt+W).
fn close_tab(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
}

/// Open `path`, then close its tab.
fn open_and_close(harness: &mut EditorTestHarness, path: &Path) {
    harness.open_file(path).unwrap();
    close_tab(harness);
}

fn reopen(harness: &mut EditorTestHarness) {
    palette(harness, "Recent Files");
}

/// The rows of the suggestion list drawn above the Recent Files
/// prompt, top to bottom, or `None` when that prompt is not on screen.
fn offered(harness: &EditorTestHarness) -> Option<Vec<String>> {
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let prompt_row = lines.iter().rposition(|l| l.starts_with(PROMPT_LABEL))?;
    let mut rows = Vec::new();
    for line in lines[..prompt_row].iter().rev() {
        let line = line.trim_end();
        if line.starts_with('└') {
            continue;
        }
        if line.starts_with('┌') {
            break;
        }
        let Some(inner) = line.strip_prefix('│') else {
            break;
        };
        rows.push(inner.trim_end_matches('│').trim().to_string());
    }
    rows.reverse();
    Some(rows)
}

fn names(list: &[&str]) -> Option<Vec<String>> {
    Some(list.iter().map(|s| s.to_string()).collect())
}

fn tab_bar(harness: &EditorTestHarness) -> String {
    harness.screen_row_text(1)
}

#[test]
fn with_nothing_closed_the_command_says_so() {
    let (mut harness, _) = project_with(&[]);
    reopen(&mut harness);
    assert_eq!(offered(&harness), None, "no prompt with nothing closed");
    harness.assert_screen_contains("No recently closed files in this workspace");
}

#[test]
fn file_menu_has_the_item() {
    let (mut harness, project) = project_with(&["a.txt"]);
    open_and_close(&mut harness, &project.join("a.txt"));

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Recent Files...");
    // New File, Open File..., Recent Files...
    key(&mut harness, KeyCode::Down);
    key(&mut harness, KeyCode::Down);
    key(&mut harness, KeyCode::Enter);

    assert_eq!(offered(&harness), names(&["a.txt"]));
}

#[test]
fn lists_only_closed_files_most_recent_first() {
    let (mut harness, project) = project_with(&["a.txt", "b.txt", "c.txt"]);
    fs::create_dir(project.join("src")).unwrap();
    fs::write(project.join("src/main.rs"), "fn main() {}\n").unwrap();

    open_and_close(&mut harness, &project.join("a.txt"));
    open_and_close(&mut harness, &project.join("src/main.rs"));
    open_and_close(&mut harness, &project.join("b.txt"));
    // c.txt stays open: it is not offered.
    harness.open_file(&project.join("c.txt")).unwrap();

    reopen(&mut harness);
    let nested = Path::new("src").join("main.rs").display().to_string();
    assert_eq!(
        offered(&harness),
        names(&["b.txt", nested.as_str(), "a.txt"])
    );
}

#[test]
fn enter_reopens_the_most_recently_closed_file() {
    let (mut harness, project) = project_with(&["a.txt", "b.txt"]);
    open_and_close(&mut harness, &project.join("a.txt"));
    open_and_close(&mut harness, &project.join("b.txt"));

    reopen(&mut harness);
    key(&mut harness, KeyCode::Enter);

    assert_eq!(offered(&harness), None, "prompt closes on Enter");
    assert!(tab_bar(&harness).contains("b.txt"));
    harness.assert_screen_contains("b.txt contents");

    // Open again, so no longer offered; a.txt still is.
    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["a.txt"]));
}

#[test]
fn typing_filters_and_reopens_the_match() {
    let (mut harness, project) = project_with(&["alpha.txt", "beta.txt"]);
    open_and_close(&mut harness, &project.join("alpha.txt"));
    open_and_close(&mut harness, &project.join("beta.txt"));

    reopen(&mut harness);
    harness.type_text("alp").unwrap();
    harness.render().unwrap();
    assert_eq!(offered(&harness), names(&["alpha.txt"]));
    key(&mut harness, KeyCode::Enter);

    harness.assert_screen_contains("alpha.txt contents");
}

#[test]
fn typed_text_matching_nothing_opens_nothing() {
    let (mut harness, project) = project_with(&["a.txt"]);
    open_and_close(&mut harness, &project.join("a.txt"));
    let tabs_before = tab_bar(&harness);

    reopen(&mut harness);
    harness.type_text("zzz-no-such-file").unwrap();
    key(&mut harness, KeyCode::Enter);

    assert_eq!(offered(&harness), None);
    assert_eq!(tab_bar(&harness), tabs_before, "no tab is opened");
    assert!(!project.join("zzz-no-such-file").exists());
}

#[test]
fn file_deleted_on_disk_is_dropped_instead_of_opened() {
    let (mut harness, project) = project_with(&["gone.txt", "kept.txt"]);
    open_and_close(&mut harness, &project.join("kept.txt"));
    open_and_close(&mut harness, &project.join("gone.txt"));
    fs::remove_file(project.join("gone.txt")).unwrap();

    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["gone.txt", "kept.txt"]));
    key(&mut harness, KeyCode::Enter);

    harness.assert_screen_contains("File no longer exists");
    assert!(!tab_bar(&harness).contains("gone.txt"));
    assert!(
        !project.join("gone.txt").exists(),
        "the deleted file must not be recreated"
    );
    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["kept.txt"]));
}

/// Delete the last entry of the explorer, `name`, as a user does: focus the
/// explorer, move to the bottom, Delete, confirm.
fn delete_last_explorer_entry(harness: &mut EditorTestHarness, name: &str) {
    palette(harness, "Focus File Explorer");
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item(name).unwrap();
    // More than there are rows: the selection stops on the last one.
    for _ in 0..8 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.assert_screen_contains(name);
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();
}

#[test]
fn deleting_an_open_file_in_the_explorer_does_not_record_it() {
    let (mut harness, project) = project_with(&["a_kept.txt", "b_victim.txt"]);
    open_and_close(&mut harness, &project.join("a_kept.txt"));
    harness.open_file(&project.join("b_victim.txt")).unwrap();
    harness.render().unwrap();
    assert!(tab_bar(&harness).contains("b_victim.txt"));

    delete_last_explorer_entry(&mut harness, "b_victim.txt");
    assert!(!tab_bar(&harness).contains("b_victim.txt"));
    assert!(!project.join("b_victim.txt").exists());

    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["a_kept.txt"]));
}

#[test]
fn deleting_a_closed_file_in_the_explorer_drops_it() {
    let (mut harness, project) = project_with(&["a_kept.txt", "b_victim.txt"]);
    open_and_close(&mut harness, &project.join("a_kept.txt"));
    open_and_close(&mut harness, &project.join("b_victim.txt"));

    delete_last_explorer_entry(&mut harness, "b_victim.txt");
    assert!(!project.join("b_victim.txt").exists());

    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["a_kept.txt"]));
}

#[test]
fn unnamed_buffer_is_not_recorded() {
    let (mut harness, _) = project_with(&[]);
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    close_tab(&mut harness);

    reopen(&mut harness);
    assert_eq!(offered(&harness), None);
    harness.assert_screen_contains("No recently closed files in this workspace");
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
fn replaced_preview_tab_is_not_recorded() {
    let (mut harness, project) = project_with(&["alpha.txt", "beta.txt"]);
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("alpha.txt").unwrap();
    harness.wait_for_file_explorer_item("beta.txt").unwrap();

    // Previewing beta replaces alpha's preview tab.
    single_click_explorer_file(&mut harness, "alpha.txt");
    single_click_explorer_file(&mut harness, "beta.txt");
    assert!(tab_bar(&harness).contains("(preview)"));
    assert!(!tab_bar(&harness).contains("alpha.txt"));

    reopen(&mut harness);
    assert_eq!(
        offered(&harness),
        None,
        "a replaced preview is not 'closed'"
    );

    // A real tab closed is recorded.
    open_and_close(&mut harness, &project.join("alpha.txt"));
    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["alpha.txt"]));
}

#[test]
fn file_outside_the_workspace_is_listed_by_absolute_path() {
    let (mut harness, _) = project_with(&[]);
    let outside = TempDir::new().unwrap();
    let file = outside.path().join("notes.txt");
    fs::write(&file, "notes\n").unwrap();
    open_and_close(&mut harness, &file);

    reopen(&mut harness);
    let shown = offered(&harness).expect("prompt is open");
    assert_eq!(shown.len(), 1);
    assert!(
        Path::new(&shown[0]).is_absolute() && shown[0].ends_with("notes.txt"),
        "listed by absolute path: {shown:?}"
    );
}

#[test]
fn each_window_has_its_own_list() {
    let (mut harness, project) = project_with(&["first.txt"]);
    open_and_close(&mut harness, &project.join("first.txt"));

    // The second window's root is spelled differently from where its files
    // resolve to — as with a symlinked folder, macOS's `/var` (really
    // `/private/var`) or Windows' `\\?\` prefix — and its files still list
    // relative to it.
    let second_dir = TempDir::new().unwrap();
    fs::write(second_dir.path().join("second.txt"), "second\n").unwrap();
    #[cfg(unix)]
    let second_root = {
        let link = second_dir.path().with_extension("link");
        std::os::unix::fs::symlink(second_dir.path(), &link).unwrap();
        link
    };
    #[cfg(not(unix))]
    let second_root = second_dir.path().to_path_buf();
    let second_window = harness
        .editor_mut()
        .create_window_at(second_root.clone(), "second".to_string());
    harness.editor_mut().set_active_window(second_window);
    harness.render().unwrap();

    reopen(&mut harness);
    assert_eq!(offered(&harness), None, "a new window starts with no list");

    open_and_close(&mut harness, &second_root.join("second.txt"));
    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["second.txt"]));
    key(&mut harness, KeyCode::Esc);

    palette(&mut harness, "Next Window");
    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["first.txt"]));
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
        // b.txt is open at quit: it comes back as a tab.
        harness.open_file(&project.join("b.txt")).unwrap();
        harness.shutdown(true).unwrap();
    }

    let mut harness = launch();
    assert!(harness.startup(true, &[]).unwrap(), "workspace restores");
    harness.render().unwrap();
    assert!(tab_bar(&harness).contains("b.txt"));
    reopen(&mut harness);
    assert_eq!(offered(&harness), names(&["a.txt"]));
    key(&mut harness, KeyCode::Enter);
    harness.assert_screen_contains("a contents");
}

/// A workspace restored after another workspace took one of its files comes
/// back without that tab (one buffer per file). The user never closed it,
/// so it is not offered as closed.
#[test]
#[cfg(target_os = "linux")] // the pinned data dir; see `isolated_dir_context`
fn tab_dropped_on_restore_for_a_file_open_elsewhere_is_not_recorded() {
    use crate::common::global_state::isolated_dir_context;

    let temp = TempDir::new().unwrap();
    let ws_a = temp.path().join("wsA");
    let ws_b = temp.path().join("wsB");
    for dir in [&ws_a, &ws_b] {
        fs::create_dir(dir).unwrap();
    }
    let shared = ws_b.join("shared.txt");
    fs::write(&shared, "shared\n").unwrap();
    let shared = shared.canonicalize().unwrap();
    let ws_b_root = ws_b.canonicalize().unwrap();
    let (dir_context, _pin) = isolated_dir_context(temp.path());
    let daemon_in = |dir: &Path| {
        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_working_dir(dir.to_path_buf())
                .with_shared_dir_context(dir_context.clone())
                .with_empty_plugins_dir(),
        )
        .unwrap();
        harness.editor_mut().set_session_mode(true);
        harness
            .editor_mut()
            .set_session_name(Some(fresh::server::ORCHESTRATOR_DAEMON.into()));
        harness.startup(true, &[]).unwrap();
        harness
    };

    // B is saved with the shared file open.
    {
        let mut daemon = daemon_in(&ws_a);
        let b = daemon
            .editor_mut()
            .create_window_at(ws_b.clone(), "wsB".to_string());
        daemon.editor_mut().set_active_window(b);
        daemon.open_file(&shared).unwrap();
        daemon.render().unwrap();
        daemon.shutdown(true).unwrap();
    }

    // A opens it first; B, shown after, comes back without its tab.
    let mut daemon = daemon_in(&ws_a);
    daemon.open_file(&shared).unwrap();
    daemon.render().unwrap();
    let b = daemon
        .editor_mut()
        .create_window_at(ws_b_root, "wsB".to_string());
    daemon.editor_mut().set_active_window(b);
    daemon.render().unwrap();
    assert!(!tab_bar(&daemon).contains("shared.txt"));

    reopen(&mut daemon);
    assert_eq!(offered(&daemon), None);
    daemon.assert_screen_contains("No recently closed files in this workspace");
}
