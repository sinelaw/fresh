//! Regression coverage for issue #2415.
//!
//! With a Utility Dock open (Search & Replace panel, dock terminal, …),
//! closing the *editor* split collapses the tree so the dock leaf becomes
//! the window's sole root leaf. That leaf used to silently keep
//! `SplitRole::UtilityDock`: every later dock-routed open (panel, dock
//! terminal) then landed in the main leaf as a full-window tab instead of
//! a split — and workspace persistence made the state permanent (the
//! saved `"role": "UtilityDock"` was faithfully restored on relaunch;
//! only `rm -rf ~/.local/share/fresh` recovered).
//!
//! The invariant now enforced: the root leaf never carries a role tag —
//! a collapse clears a role stranded on the sole surviving leaf, and
//! workspace restore sanitizes the tree so already-poisoned workspace
//! files heal on load.
//!
//! Like `issue_2283_dock_last_tab_close.rs`, these assert on the
//! split-tree model directly (the role tag is not visible on screen).

use crate::common::harness::EditorTestHarness;
use fresh::input::keybindings::Action;
use fresh::view::split::SplitRole;
use std::fs;

/// Open a file, then a dock terminal (`OpenTerminalInDock` splits at the
/// root and tags the new leaf `UtilityDock`), then close the *editor*
/// split so the dock leaf becomes the sole root leaf.
fn collapse_editor_split_onto_dock(harness: &mut EditorTestHarness) {
    let editor_leaf = harness.editor().split_manager_for_tests().active_split();

    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::OpenTerminalInDock);
    let dock_leaf = harness
        .editor()
        .split_manager_for_tests()
        .find_leaf_by_role(SplitRole::UtilityDock)
        .expect("OpenTerminalInDock must create a dock leaf");
    assert_ne!(editor_leaf, dock_leaf);

    // Close the editor split (the same collapse the mouse × / Close Split
    // command performs).
    harness
        .editor_mut()
        .active_window_mut()
        .split_manager_mut()
        .expect("active window must have a populated split layout")
        .set_active_split(editor_leaf);
    harness.editor_mut().close_active_split();

    assert_eq!(
        harness
            .editor()
            .split_manager_for_tests()
            .root()
            .count_leaves(),
        1,
        "closing the editor split must leave the dock leaf as the sole leaf"
    );
}

#[test]
fn test_dock_role_cleared_when_editor_split_closes() {
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("main.txt");
    fs::write(&file, "hello world\n").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_file(&file).unwrap();

    collapse_editor_split_onto_dock(&mut harness);

    // The surviving root leaf must have shed the UtilityDock role: it is
    // the window's only pane now, holding regular content.
    assert_eq!(
        harness
            .editor()
            .split_manager_for_tests()
            .find_leaf_by_role(SplitRole::UtilityDock),
        None,
        "the sole root leaf must not keep the UtilityDock role after the collapse"
    );
}

#[test]
fn test_dock_reopens_as_split_after_editor_split_closed() {
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("main.txt");
    fs::write(&file, "hello world\n").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_file(&file).unwrap();

    collapse_editor_split_onto_dock(&mut harness);
    let root_leaf = harness.editor().split_manager_for_tests().active_split();

    // Reopening a dock-routed panel must create a NEW dock split — not
    // attach the panel as a full-window tab in the sole leaf (the stuck
    // state from the issue).
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::OpenTerminalInDock);

    let sm = harness.editor().split_manager_for_tests();
    assert_eq!(
        sm.root().count_leaves(),
        2,
        "the dock must reopen as its own split, not as a tab in the sole leaf"
    );
    let new_dock = sm
        .find_leaf_by_role(SplitRole::UtilityDock)
        .expect("the reopened dock must carry the UtilityDock role");
    assert_ne!(
        new_dock, root_leaf,
        "the dock role must be on the new split, not the former root leaf"
    );
}

/// A workspace file written by a pre-fix build can carry the poisoned
/// state (`"role": "UtilityDock"` on the sole leaf). Restoring it must
/// heal the role instead of re-applying it permanently.
#[test]
fn test_restore_heals_workspace_with_role_on_sole_leaf() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_dir = temp.path().join("project");
    fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("main.txt");
    fs::write(&file, "hello world\n").unwrap();

    // Session 1: recreate the poisoned state a pre-fix build persisted —
    // a sole leaf tagged UtilityDock while holding a regular file — and
    // save the workspace.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            fresh::config::Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();

        let sole_leaf = harness.editor().split_manager_for_tests().active_split();
        harness
            .editor_mut()
            .active_window_mut()
            .split_manager_mut()
            .expect("active window must have a populated split layout")
            .set_leaf_role(sole_leaf, Some(SplitRole::UtilityDock));

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore. The layout comes back, but the stranded role
    // must be sanitized away.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            fresh::config::Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();
        harness.assert_screen_contains("main.txt");

        assert_eq!(
            harness
                .editor()
                .split_manager_for_tests()
                .find_leaf_by_role(SplitRole::UtilityDock),
            None,
            "restore must not re-apply a UtilityDock role to the sole root leaf"
        );
    }
}
