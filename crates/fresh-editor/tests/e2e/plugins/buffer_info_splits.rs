//! Pins the invariant that `BufferInfo.splits` in the plugin state
//! snapshot reflects every split currently holding the buffer.
//!
//! Plugins like the devcontainer build-log opener read this to
//! implement "focus existing buffer if visible, else open new" —
//! the alternative is tracking split ids in module state, which
//! dies across editor restarts when split ids get reassigned.
//! Regression here would silently re-break the "Show Build Logs
//! spawns a new split every time" UX.

#![cfg(feature = "plugins")]

use crate::common::harness::EditorTestHarness;
use std::fs;

fn snapshot_splits_for_path(
    harness: &EditorTestHarness,
    path: &std::path::Path,
) -> Option<Vec<usize>> {
    let snapshot_handle = harness.editor().plugin_manager().state_snapshot_handle()?;
    let snapshot = snapshot_handle.read().ok()?;
    snapshot
        .buffers
        .values()
        .find(|b| b.path.as_deref() == Some(path))
        .map(|b| b.splits.iter().map(|s| s.0).collect())
}

fn snapshot_active_split(harness: &EditorTestHarness) -> Option<usize> {
    let snapshot_handle = harness.editor().plugin_manager().state_snapshot_handle()?;
    let snapshot = snapshot_handle.read().ok()?;
    Some(snapshot.active_split_id)
}

/// A buffer open in a single split should report exactly that
/// split id in `BufferInfo.splits`.
#[test]
fn buffer_info_splits_reports_single_split() {
    let temp = tempfile::tempdir().unwrap();
    let path = temp.path().join("hello.txt");
    fs::write(&path, "hi\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&path).unwrap();
    harness.tick_and_render().unwrap();

    let splits = snapshot_splits_for_path(&harness, &path)
        .expect("snapshot should know the buffer we just opened");
    assert_eq!(
        splits.len(),
        1,
        "Buffer open in one split must report exactly one split id. splits={:?}",
        splits
    );
}

/// Splitting the pane holding a buffer puts the same buffer in
/// both splits (the new split inherits the active buffer). The
/// snapshot must surface both split ids so a plugin's "already
/// visible?" check sees the full set.
#[test]
fn buffer_info_splits_reports_both_splits_after_split_horizontal() {
    let temp = tempfile::tempdir().unwrap();
    let path = temp.path().join("hello.txt");
    fs::write(&path, "hi\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&path).unwrap();
    harness.tick_and_render().unwrap();

    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::SplitHorizontal);
    harness.tick_and_render().unwrap();

    let splits = snapshot_splits_for_path(&harness, &path)
        .expect("snapshot should still know the buffer after the split");
    assert_eq!(
        splits.len(),
        2,
        "After split_horizontal, the buffer is in two splits. splits={:?}",
        splits
    );
    // The two split ids must be distinct.
    assert_ne!(
        splits[0], splits[1],
        "The two splits showing the buffer must have distinct ids. splits={:?}",
        splits
    );
}

/// A plugin can use `BufferInfo.splits` together with `focusSplit`
/// to implement the "focus existing buffer if visible" pattern.
/// Drive the exact sequence end-to-end: open a buffer in a split,
/// move focus to a different split, then simulate the plugin
/// reading the snapshot to refocus the original split.
#[test]
fn buffer_info_splits_drives_refocus_pattern() {
    let temp = tempfile::tempdir().unwrap();
    let hello = temp.path().join("hello.txt");
    let world = temp.path().join("world.txt");
    fs::write(&hello, "hello\n").unwrap();
    fs::write(&world, "world\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&hello).unwrap();
    harness.tick_and_render().unwrap();

    // Split horizontal and open world.txt in the new (now-active)
    // split. Two splits: top hello, bottom world.
    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::SplitHorizontal);
    harness.open_file(&world).unwrap();
    harness.tick_and_render().unwrap();

    // The active split is the bottom one (world.txt). Now simulate
    // a plugin that wants to focus the hello.txt split.
    let hello_splits = snapshot_splits_for_path(&harness, &hello)
        .expect("hello.txt must still be in the snapshot");
    assert_eq!(hello_splits.len(), 1, "hello.txt is in exactly one split");
    let hello_split = hello_splits[0];

    // Sanity: the active split is NOT the hello split (it's the
    // bottom world split).
    let active = snapshot_active_split(&harness).unwrap();
    assert_ne!(
        active, hello_split,
        "Sanity: active split should be the world.txt split, not hello's"
    );

    // Drive the focus via the same PluginCommand the plugin would
    // send.
    harness
        .editor_mut()
        .handle_plugin_command(fresh::services::plugins::api::PluginCommand::FocusSplit {
            split_id: fresh_core::SplitId(hello_split),
        })
        .unwrap();
    harness.tick_and_render().unwrap();

    let active_after = snapshot_active_split(&harness).unwrap();
    assert_eq!(
        active_after, hello_split,
        "focusSplit(hello_split) must make that split active. was {} now {}",
        active, active_after
    );
}
