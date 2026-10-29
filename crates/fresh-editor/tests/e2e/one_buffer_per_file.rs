//! A file has one buffer across an editor's workspaces, and its unsaved work
//! is found by the file, from any project.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use std::path::Path;
use tempfile::TempDir;

fn launch_in(dir: &Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.editor.auto_recovery_save_interval_secs = 0;
    EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .with_empty_plugins_dir(),
    )
    .unwrap()
}

/// Two projects and a file outside both.
fn two_projects() -> (
    TempDir,
    std::path::PathBuf,
    std::path::PathBuf,
    std::path::PathBuf,
) {
    let temp = TempDir::new().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir(&a).unwrap();
    std::fs::create_dir(&b).unwrap();
    let file = temp.path().join("outside.txt");
    std::fs::write(&file, "on disk\n").unwrap();
    (temp, a, b, file)
}

/// Opening a file another workspace has open goes to it there; a
/// background open (a plugin's, say) is refused instead of making a
/// second buffer.
#[test]
fn opening_a_file_open_in_another_workspace_goes_there() {
    let (temp, a, b, file) = two_projects();
    let dir_context = DirectoryContext::for_testing(temp.path());
    let mut h = launch_in(&a, &dir_context);
    h.startup(true, &[]).unwrap();
    let home = h.editor().active_window_id();
    let first = h.editor_mut().open_file(&file).unwrap();

    let other = h.editor_mut().create_window_at(b, "b".to_string());
    h.editor_mut().set_active_window(other);
    h.render().unwrap();
    assert!(
        h.editor_mut().open_file_no_focus(&file).is_err(),
        "a background open made a second buffer"
    );
    assert_eq!(h.editor().active_window_id(), other);

    let again = h.editor_mut().open_file(&file).unwrap();
    assert_eq!(again, first, "the same buffer");
    assert_eq!(h.editor().active_window_id(), home, "and its workspace");
}

/// Work left on a file edited from project A — outside A — comes back when
/// the file is opened from project B: the file's entry finds it, wherever
/// it was edited.
#[test]
fn a_files_kept_work_comes_back_wherever_the_file_is_opened() {
    let (temp, a, b, file) = two_projects();
    let dir_context = DirectoryContext::for_testing(temp.path());
    {
        let mut h = launch_in(&a, &dir_context);
        h.startup(true, &[]).unwrap();
        h.open_file(&file).unwrap();
        h.type_text("EDITED-FROM-A ").unwrap();
        h.render().unwrap();
        h.editor_mut().auto_recovery_save_dirty_buffers().unwrap();
        // Crash
    }

    let mut h = launch_in(&b, &dir_context);
    h.startup(true, &[]).unwrap();
    h.render().unwrap();
    h.assert_screen_not_contains("EDITED-FROM-A");
    h.open_file(&file).unwrap();
    h.render().unwrap();
    h.assert_screen_contains("EDITED-FROM-A");
    assert!(h.editor().active_state().buffer.is_modified());
}
