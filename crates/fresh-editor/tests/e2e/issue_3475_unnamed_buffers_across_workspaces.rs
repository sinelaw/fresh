//! Issues #3475 and #3476 — switching between projects must not cost an
//! unsaved, never-named buffer, and must not duplicate it.
//!
//! #3475: opening project A from an editor launched elsewhere did not bring
//! A's unsaved buffer back, and the workspace save on the way out then
//! dropped the reference to it, so the next launch in A had nothing left to
//! restore.
//!
//! #3476: switching back to a workspace adopted its unsaved buffer again,
//! into a new tab each time.

use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

/// How many tabs the tab bar shows for unnamed buffers. Duplicates are
/// disambiguated by the tab bar ("[No Name] 1", "[No Name] 2", ...), so
/// counting the shared prefix counts the buffers.
fn no_name_tabs(harness: &EditorTestHarness) -> usize {
    harness
        .screen_row_text(layout::TAB_BAR_ROW as u16)
        .matches("No Name")
        .count()
}

/// #3475 — opening project A from an editor launched in project B must not
/// cost A its unsaved buffer.
///
/// The editor in B cannot see A's recovery entry: in standalone mode the store
/// is scoped to the launch directory (#1550). This test pins down what has to
/// follow from that — a session that could not restore a workspace does not
/// rewrite it. The save rebuilds `unnamed_buffers` and the split layout from
/// the live buffers, so it used to drop the reference and the tab.
#[test]
fn visiting_a_workspace_without_its_recovery_data_leaves_it_alone() {
    let temp_dir = TempDir::new().unwrap();
    let folder_a = temp_dir.path().join("folder_a");
    let folder_b = temp_dir.path().join("folder_b");
    std::fs::create_dir(&folder_a).unwrap();
    std::fs::create_dir(&folder_b).unwrap();

    // One user, one installation, two working directories.
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    let harness_in = |dir: &std::path::Path| {
        let mut config = Config::default();
        config.editor.hot_exit = true;
        EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(dir.to_path_buf())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap()
    };

    // Folder A: an unsaved, unnamed buffer, then a clean exit. Its workspace
    // now records the buffer and the recovery store holds its content.
    {
        let mut harness = harness_in(&folder_a);
        harness.startup(true, &[]).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text("FOLDER_A_UNSAVED").unwrap();
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    // Folder B: an editor launched elsewhere, which materializes folder A's
    // workspace (what clicking it in the Orchestrator dock does) and exits.
    // This is the step that used to drop the reference.
    {
        let mut harness = harness_in(&folder_b);
        harness.startup(true, &[]).unwrap();
        harness.editor_mut().materialize_all_windows();
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    // Folder A again: the unsaved buffer must still come back.
    {
        let mut harness = harness_in(&folder_a);
        harness.startup(true, &[]).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("FOLDER_A_UNSAVED");
    }
}

/// #3476 — switching away and back must not adopt the same unnamed buffer
/// again.
///
/// Adoption runs on every activation (issue #3189, so each workspace reclaims
/// its own unsaved work). It skipped entries whose file is already open, but
/// an unnamed buffer has no file, so nothing matched it and every switch back
/// opened another copy in another tab.
#[test]
fn switching_back_does_not_adopt_the_same_unnamed_buffer_twice() {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    // Save on the next tick rather than waiting out the interval; the entry
    // has to be on disk for adoption to find it at all.
    config.editor.auto_recovery_save_interval_secs = 0;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();

    harness.new_buffer().unwrap();
    harness.type_text("UNNAMED-KEEP").unwrap();
    harness.render().unwrap();
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();
    harness.render().unwrap();

    let home = harness.editor().active_window_id();
    let tabs_before = no_name_tabs(&harness);

    // A second workspace to switch away to, over a different root.
    let other_root = project_dir.parent().unwrap().join("workspace_b");
    std::fs::create_dir_all(&other_root).unwrap();
    let other = harness
        .editor_mut()
        .create_window_at(other_root, "workspace_b".to_string());

    // Several round trips: the duplication grew by one each time, so one
    // switch is not enough to tell a fix from an off-by-one.
    for _ in 0..3 {
        harness.editor_mut().set_active_window(other);
        harness.render().unwrap();
        harness.editor_mut().set_active_window(home);
        harness.render().unwrap();
    }

    assert_eq!(
        no_name_tabs(&harness),
        tabs_before,
        "switching back to a workspace must not open another copy of its \
         unsaved buffer.\nTabs: {}",
        harness.screen_row_text(layout::TAB_BAR_ROW as u16)
    );
    // The content is still there — deduplicating must not cost the buffer.
    harness.assert_screen_contains("UNNAMED-KEEP");
}
