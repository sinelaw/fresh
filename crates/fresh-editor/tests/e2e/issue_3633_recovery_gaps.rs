//! Two correctness gaps from #3633, reproduced against a live editor:
//!
//! * **A1**: a plugin save of a buffer that is not the active one finished
//!   off the *active* buffer instead: it deleted that buffer's recovery
//!   entry while its tab still showed unsaved changes, and cleared the flag
//!   that would have written it again, so a crash lost the edit.
//! * **A3**: a file's recovery entry was keyed by the file's path, so two
//!   workspaces editing one file shared a single entry. The last one backed
//!   up overwrote the other's work, and after a crash the wrong workspace got
//!   it.
//!
//! These tests look for backups by walking the recovery directory for the
//! content files of entries, wherever they sit, so they read the same way
//! against any store layout.

use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

fn hot_exit_config() -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    // Back up on every tick rather than waiting out the interval
    config.editor.auto_recovery_save_interval_secs = 0;
    config
}

fn launch(dir: &Path, dir_context: &DirectoryContext, config: Config) -> EditorTestHarness {
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

/// The content files (`<id>.chunk.N`) under `dir`, at any depth.
fn chunk_files(dir: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(next) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&next) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path
                .file_name()
                .and_then(|n| n.to_str())
                .is_some_and(|n| n.contains(".chunk."))
            {
                found.push(path);
            }
        }
    }
    found
}

/// The backups in the recovery store whose content holds `needle`.
fn backups_holding(dir_context: &DirectoryContext, needle: &str) -> Vec<PathBuf> {
    chunk_files(&dir_context.recovery_dir())
        .into_iter()
        .filter(|chunk| {
            std::fs::read(chunk).is_ok_and(|bytes| String::from_utf8_lossy(&bytes).contains(needle))
        })
        .collect()
}

/// The metadata file of the entry `chunk` belongs to.
fn metadata_of(chunk: &Path) -> String {
    let name = chunk.file_name().unwrap().to_str().unwrap();
    let id = &name[..name.find(".chunk.").unwrap()];
    std::fs::read_to_string(chunk.with_file_name(format!("{id}.meta.json"))).unwrap()
}

fn tab_bar(harness: &EditorTestHarness) -> String {
    harness.screen_row_text(layout::TAB_BAR_ROW as u16)
}

/// Save As the active buffer to `path`.
fn save_as(harness: &mut EditorTestHarness, path: &Path) {
    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::SaveAs);
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(path.to_str().unwrap()).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

// =========================================================================
// A1: a plugin save of a buffer that is not the active one
// =========================================================================

/// `fresh x.txt y.txt`, both edited and backed up; y is active and has more
/// unsaved edits since its last backup. A plugin saves x
/// (`editor.saveBufferToPath(xId, xPath)`). That finishes x's save, not y's:
/// x is the one marked saved and the one its on-save actions run on; y keeps
/// its entry, its pending backup and its text, and after a crash comes back
/// with every edit.
#[test]
#[cfg(feature = "plugins")] // the plugin command dispatcher
fn a_plugin_save_of_another_buffer_leaves_the_active_ones_unsaved_work() {
    let temp = TempDir::new().unwrap();
    let project = temp.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let x = project.join("x.txt");
    let y = project.join("y.txt");
    std::fs::write(&x, "x original\n").unwrap();
    std::fs::write(&y, "y original\n").unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());
    let mut config = hot_exit_config();
    // An on-save action that would show which buffer it ran on
    config.editor.trim_trailing_whitespace_on_save = true;

    {
        let mut harness = launch(&project, &dir_context, config.clone());
        harness.startup(true, &[x.clone(), y.clone()]).unwrap();

        harness.open_file(&x).unwrap();
        let x_id = harness.editor().active_buffer();
        harness.type_text("X-EDIT ").unwrap();
        // Whitespace x's on-save trim should remove
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" X-TAIL   ").unwrap();
        harness.open_file(&y).unwrap();
        harness.type_text("Y-EDIT ").unwrap();
        harness.render().unwrap();
        harness
            .editor_mut()
            .auto_recovery_save_dirty_buffers()
            .unwrap();
        assert_eq!(backups_holding(&dir_context, "X-EDIT").len(), 1);
        assert_eq!(backups_holding(&dir_context, "Y-EDIT").len(), 1);

        // More in y since its backup, ending in whitespace an on-save trim
        // would remove
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" Y-MORE   ").unwrap();
        harness.render().unwrap();
        assert!(harness.editor().is_active_buffer_recovery_dirty());

        harness
            .editor_mut()
            .handle_plugin_command(fresh_core::api::PluginCommand::SaveBufferToPath {
                buffer_id: x_id,
                path: x.clone(),
            })
            .unwrap();
        harness.render().unwrap();
        // x's on-save actions ran on x: its file was rewritten trimmed
        assert_eq!(
            std::fs::read_to_string(&x).unwrap(),
            "X-EDIT x original X-TAIL\n",
            "the saved buffer's on-save actions did not run on it"
        );

        // y — still active — keeps its unsaved work, on screen and on disk
        assert!(harness.editor().active_state().buffer.is_modified());
        assert!(
            harness.editor().is_active_buffer_recovery_dirty(),
            "the active buffer's pending backup was cleared by another buffer's save"
        );
        assert!(
            harness.get_buffer_content().unwrap().contains("Y-MORE   "),
            "an on-save action for x ran on y"
        );
        assert!(tab_bar(&harness).contains("y.txt*"));

        // The pending backup goes out, with the edits since the last one.
        // Checked here rather than straight after the plugin save: a delete
        // of y's entry would run off the main loop, so its absence then
        // proves nothing. A cleared pending flag means no backup is written
        // now, and a deleted entry means there is none to find either way.
        harness
            .editor_mut()
            .auto_recovery_save_dirty_buffers()
            .unwrap();
        assert_eq!(
            backups_holding(&dir_context, "Y-MORE").len(),
            1,
            "the active buffer's backup was deleted, or stopped, by another buffer's save"
        );

        // x was the one marked saved: an edit undone takes it back to clean.
        // Its buffer holds what the trim wrote, not the untrimmed text.
        harness.open_file(&x).unwrap();
        assert!(!harness.editor().active_state().buffer.is_modified());
        assert_eq!(
            harness.get_buffer_content().unwrap(),
            "X-EDIT x original X-TAIL\n"
        );
        harness.type_text("Z").unwrap();
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        assert!(
            !harness.editor().active_state().buffer.is_modified(),
            "x's undo history was never marked saved"
        );
        // Dropped without a shutdown: a crash
    }

    let mut harness = launch(&project, &dir_context, config);
    harness.startup(true, &[x.clone(), y.clone()]).unwrap();
    harness.open_file(&y).unwrap();
    harness.render().unwrap();
    let y_text = harness.get_buffer_content().unwrap();
    assert!(
        y_text.contains("Y-EDIT") && y_text.contains("Y-MORE"),
        "y's unsaved edits did not come back: {y_text:?}"
    );
    assert!(harness.editor().active_state().buffer.is_modified());

    harness.open_file(&x).unwrap();
    harness.render().unwrap();
    assert!(!harness.editor().active_state().buffer.is_modified());
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        std::fs::read_to_string(&x).unwrap()
    );
}

// =========================================================================
// A3: one file edited in two workspaces
// =========================================================================

/// Two workspaces in one Orchestrator daemon, `wsA` and `wsB`, both
/// editing `shared/F.txt`.
struct TwoWorkspaces {
    _temp: TempDir,
    ws_a: PathBuf,
    ws_b: PathBuf,
    file: PathBuf,
    dir_context: DirectoryContext,
}

impl TwoWorkspaces {
    fn new() -> Self {
        let temp = TempDir::new().unwrap();
        let ws_a = temp.path().join("wsA");
        let ws_b = temp.path().join("wsB");
        let shared = temp.path().join("shared");
        for dir in [&ws_a, &ws_b, &shared] {
            std::fs::create_dir(dir).unwrap();
        }
        let file = shared.join("F.txt");
        std::fs::write(&file, "base\n").unwrap();
        let dir_context = DirectoryContext::for_testing(temp.path());
        Self {
            _temp: temp,
            ws_a,
            ws_b,
            file,
            dir_context,
        }
    }

    /// The Orchestrator daemon, its first workspace on `dir`.
    fn daemon_in(&self, dir: &Path) -> EditorTestHarness {
        let mut harness = launch(dir, &self.dir_context, hot_exit_config());
        harness.editor_mut().set_session_mode(true);
        harness
            .editor_mut()
            .set_session_name(Some(fresh::server::ORCHESTRATOR_DAEMON.into()));
        harness.startup(true, &[]).unwrap();
        harness
    }

    /// Steps 1-2 of the report: A types into F, then B does. Returns the
    /// daemon and the two workspaces' window ids.
    fn edit_in_both(
        &self,
    ) -> (
        EditorTestHarness,
        fresh_core::WindowId,
        fresh_core::WindowId,
    ) {
        let mut daemon = self.daemon_in(&self.ws_a);
        let a = daemon.editor().active_window_id();
        daemon.open_file(&self.file).unwrap();
        daemon.type_text("A-EDIT ").unwrap();
        daemon.render().unwrap();
        daemon
            .editor_mut()
            .auto_recovery_save_dirty_buffers()
            .unwrap();

        let b = daemon
            .editor_mut()
            .create_window_at(self.ws_b.clone(), "wsB".to_string());
        daemon.editor_mut().set_active_window(b);
        daemon.render().unwrap();
        daemon.open_file(&self.file).unwrap();
        daemon.type_text("B-EDIT ").unwrap();
        daemon.render().unwrap();
        daemon
            .editor_mut()
            .auto_recovery_save_dirty_buffers()
            .unwrap();
        (daemon, a, b)
    }

    fn backups_holding(&self, needle: &str) -> Vec<PathBuf> {
        backups_holding(&self.dir_context, needle)
    }
}

/// The report's steps: A and B both edit F, A saves its F as G, the daemon
/// is killed, and each workspace is opened again. Each workspace's backup
/// of F is its own throughout; after the crash A never shows B's content, B
/// gets its own back, and nothing is restored twice.
#[test]
fn a_file_edited_in_two_workspaces_keeps_each_ones_work() {
    let setup = TwoWorkspaces::new();
    {
        let (mut daemon, a, _b) = setup.edit_in_both();

        let a_backups = setup.backups_holding("A-EDIT");
        let b_backups = setup.backups_holding("B-EDIT");
        assert_eq!(
            a_backups.len(),
            1,
            "workspace A's backup of F was overwritten by B's"
        );
        assert_eq!(b_backups.len(), 1);
        assert_ne!(a_backups, b_backups, "one backup for two workspaces' edits");

        // Step 3: back in A, Save As F -> wsA/G.txt
        daemon.editor_mut().set_active_window(a);
        daemon.render().unwrap();
        save_as(&mut daemon, &setup.ws_a.join("G.txt"));
        assert!(std::fs::read_to_string(setup.ws_a.join("G.txt"))
            .unwrap()
            .contains("A-EDIT"));
        daemon
            .editor_mut()
            .auto_recovery_save_dirty_buffers()
            .unwrap();
        assert!(
            setup.backups_holding("A-EDIT").is_empty(),
            "A's work is saved, so its backup goes"
        );
        let b_backups = setup.backups_holding("B-EDIT");
        assert_eq!(b_backups.len(), 1, "A's Save As took B's backup with it");
        assert!(
            !metadata_of(&b_backups[0]).contains("\"saving\""),
            "A's Save As was recorded in B's backup"
        );
        // Step 4: killed
    }

    // Step 5: a bare `fresh` in wsA
    {
        let mut a = setup.daemon_in(&setup.ws_a);
        a.render().unwrap();
        a.assert_screen_not_contains("B-EDIT");
        a.shutdown(true).unwrap();
    }

    // ... and in wsB, which gets its own work back, once
    let mut b = setup.daemon_in(&setup.ws_b);
    b.render().unwrap();
    b.assert_screen_contains("B-EDIT");
    b.assert_screen_not_contains("A-EDIT");
    assert_eq!(tab_bar(&b).matches("F.txt").count(), 1, "{}", tab_bar(&b));
    // Switching away and back restores nothing a second time
    let home = b.editor().active_window_id();
    let other = b
        .editor_mut()
        .create_window_at(setup.ws_a.clone(), "wsA".to_string());
    b.editor_mut().set_active_window(other);
    b.render().unwrap();
    b.editor_mut().set_active_window(home);
    b.render().unwrap();
    assert_eq!(tab_bar(&b).matches("F.txt").count(), 1, "{}", tab_bar(&b));
}
