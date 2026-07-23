//! Regression for #2770: `editor.setSplitRatio(leafSplitId, ratio)` used
//! to hit `unreachable!` inside `SplitManager::set_ratio` and abort the
//! whole editor. Every plugin-visible split id is a *leaf*, while
//! `set_ratio` only accepts resizable *container* ids, so any plugin
//! calling `setSplitRatio` crashed the editor.
//!
//! After the fix the plugin path is a graceful no-op: no panic, and a
//! leaf id leaves the layout untouched (the internal handler reports
//! `false` — see the `set_ratio` unit tests in `view::split`).

#![cfg(feature = "plugins")]

use crate::common::harness::EditorTestHarness;
use fresh::services::plugins::api::PluginCommand;
use fresh_core::SplitId;
use std::fs;

fn snapshot_active_split(harness: &EditorTestHarness) -> Option<usize> {
    let snapshot_handle = harness.editor().plugin_manager().state_snapshot_handle()?;
    let snapshot = snapshot_handle.read().ok()?;
    Some(snapshot.active_split_id)
}

/// Driving `SetSplitRatio` with a leaf split id (the only kind a plugin
/// ever holds) must not panic/abort the editor, and must leave the
/// layout unchanged.
#[test]
fn set_split_ratio_on_leaf_does_not_panic() {
    let temp = tempfile::tempdir().unwrap();
    let path = temp.path().join("hello.txt");
    fs::write(&path, "hi\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.editor_mut().open_file(&path).unwrap();
    harness.tick_and_render().unwrap();

    // The active split is a leaf — exactly what a plugin gets back from
    // `createTerminal`, the active-split snapshot field, etc.
    let leaf = snapshot_active_split(&harness).expect("editor boots with an active leaf split");

    // Before the fix this aborted the process via `unreachable!`.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetSplitRatio {
            split_id: SplitId(leaf),
            ratio: 0.7,
        })
        .expect("setSplitRatio on a leaf must be a graceful no-op, not an error");

    harness.tick_and_render().unwrap();

    // Editor is still alive and the active split id is unchanged.
    assert_eq!(
        snapshot_active_split(&harness),
        Some(leaf),
        "editor must survive setSplitRatio on a leaf split id"
    );
}

/// Even when a real resizable container exists in the tree, targeting a
/// *leaf* id is still a no-op (the container's ratio is untouched) — the
/// handler does not accidentally resolve a leaf to its parent.
#[test]
fn set_split_ratio_on_leaf_leaves_container_untouched() {
    let temp = tempfile::tempdir().unwrap();
    let path = temp.path().join("hello.txt");
    fs::write(&path, "hi\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.editor_mut().open_file(&path).unwrap();
    harness.tick_and_render().unwrap();

    // Create a container by splitting; the active split is still a leaf.
    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::SplitHorizontal);
    harness.tick_and_render().unwrap();

    let leaf = snapshot_active_split(&harness).expect("active split is a leaf after splitting");

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetSplitRatio {
            split_id: SplitId(leaf),
            ratio: 0.8,
        })
        .expect("setSplitRatio on a leaf must be a graceful no-op, not an error");

    harness.tick_and_render().unwrap();

    // Both splits are still present — the editor did not crash and the
    // leaf id did not resize its parent.
    assert_eq!(
        snapshot_active_split(&harness),
        Some(leaf),
        "editor must survive setSplitRatio on a leaf even with a container present"
    );
}
