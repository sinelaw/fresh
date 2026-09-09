//! Regression for #2770 and follow-up to #2774.
//!
//! History: `editor.setSplitRatio(leafSplitId, ratio)` first hit
//! `unreachable!` inside `SplitManager::set_ratio` and aborted the whole
//! editor (#2770). PR #2774 made that a graceful no-op. But every
//! plugin-visible split id is a *leaf* (`getActiveSplitId`, `listSplits`,
//! `BufferInfo.splits`, `createTerminal` all return leaf ids), while
//! `set_ratio` only mutates a resizable *container*, so `setSplitRatio`
//! could never actually resize anything — it always no-op'd.
//!
//! This follow-up makes it work: `handle_set_split_ratio` resolves a leaf
//! id to its parent split container and sets that container's ratio. These
//! tests assert (a) a lone top-level leaf with no parent is still a graceful
//! no-op (no panic), and (b) a leaf inside a split now resizes its parent.

#![cfg(feature = "plugins")]

use crate::common::harness::EditorTestHarness;
use fresh::services::plugins::api::PluginCommand;
use fresh_core::{LeafId, SplitId};
use std::fs;

fn snapshot_active_split(harness: &EditorTestHarness) -> Option<usize> {
    let snapshot_handle = harness.editor().plugin_manager().state_snapshot_handle()?;
    let snapshot = snapshot_handle.read().ok()?;
    Some(snapshot.active_split_id)
}

/// Driving `SetSplitRatio` with a *lone* top-level leaf (no parent
/// container to resize) must not panic/abort the editor, and must leave
/// the layout unchanged.
#[test]
fn set_split_ratio_on_lone_leaf_does_not_panic() {
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

/// When a leaf sits inside a split, `setSplitRatio` on that *leaf* id
/// resolves to the parent container and actually resizes it — the whole
/// point of the follow-up fix. Every id a plugin holds is a leaf, so this
/// is the real plugin path.
#[test]
fn set_split_ratio_on_leaf_resizes_parent_container() {
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

    // The parent container of the active leaf, and its ratio before.
    let parent = harness
        .editor()
        .split_manager_for_tests()
        .parent_container_of(LeafId(SplitId(leaf)))
        .expect("a split leaf must have a parent container");
    let ratio_before = harness
        .editor()
        .split_manager_for_tests()
        .get_ratio(parent.into())
        .expect("parent container has a ratio");
    assert_ne!(
        ratio_before, 0.8,
        "precondition: parent ratio differs from the value we will set"
    );

    // Drive the real plugin command with the LEAF id.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetSplitRatio {
            split_id: SplitId(leaf),
            ratio: 0.8,
        })
        .expect("setSplitRatio on a leaf resolves to its parent container");

    harness.tick_and_render().unwrap();

    // The parent container's ratio was actually updated (clamped range).
    let ratio_after = harness
        .editor()
        .split_manager_for_tests()
        .get_ratio(parent.into())
        .expect("parent container still has a ratio");
    assert_eq!(
        ratio_after, 0.8,
        "setSplitRatio on a leaf must resize its parent container"
    );

    // Editor is still alive and the active split id is unchanged.
    assert_eq!(
        snapshot_active_split(&harness),
        Some(leaf),
        "editor must survive setSplitRatio and keep the same active leaf"
    );
}

/// An out-of-range ratio resolved through a leaf is handled safely under the
/// min-pane-size model.
///
/// The stored ratio is no longer pinned to a fixed `0.1/0.9` percentage window;
/// it is only clamped to the raw `[0.0, 1.0]` range (so `5.0` stores as `1.0`).
/// The real guarantee — a sibling pane never collapsing — moved to layout time,
/// where `clamp_first_to_min` keeps every rendered pane at least `MIN_PANE_*`.
/// This test asserts both facets: the raw ratio clamp *and* the rendered floor.
#[test]
fn set_split_ratio_on_leaf_clamps_parent_ratio() {
    use fresh::view::split::MIN_PANE_HEIGHT;

    let temp = tempfile::tempdir().unwrap();
    let path = temp.path().join("hello.txt");
    fs::write(&path, "hi\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.editor_mut().open_file(&path).unwrap();
    harness.tick_and_render().unwrap();

    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::SplitHorizontal);
    harness.tick_and_render().unwrap();

    let leaf = snapshot_active_split(&harness).expect("active split is a leaf after splitting");
    let parent = harness
        .editor()
        .split_manager_for_tests()
        .parent_container_of(LeafId(SplitId(leaf)))
        .expect("a split leaf must have a parent container");

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetSplitRatio {
            split_id: SplitId(leaf),
            ratio: 5.0,
        })
        .expect("setSplitRatio on a leaf resolves to its parent container");
    harness.tick_and_render().unwrap();

    // (a) Raw-storage clamp: an out-of-range 5.0 is pinned to the [0.0, 1.0]
    // range, i.e. stored as 1.0 (no longer to the old 0.9 percentage floor).
    assert_eq!(
        harness
            .editor()
            .split_manager_for_tests()
            .get_ratio(parent.into()),
        Some(1.0),
        "an out-of-range ratio must clamp to the raw [0.0, 1.0] range (5.0 -> 1.0)"
    );

    // (b) The real guarantee: even with the parent ratio pushed to the extreme,
    // the layout-time min-size clamp keeps *both* rendered panes at least
    // MIN_PANE_HEIGHT rows tall — the sibling never collapses.
    let (content_first, content_last) = harness.content_area_rows();
    // A pane's split rectangle starts one row above where its text begins (the
    // tab bar); the bottom pane has no tab bar below it, so `content_last`
    // already aligns with the split-area bottom.
    let split_area_top = content_first.saturating_sub(1);
    let sep_y = harness.editor().get_separator_areas()[0].3 as usize;
    let first_pane_height = sep_y.saturating_sub(split_area_top);
    let second_pane_height = content_last.saturating_sub(sep_y);
    assert!(
        first_pane_height >= MIN_PANE_HEIGHT as usize,
        "first pane must stay at least {MIN_PANE_HEIGHT} rows, got {first_pane_height}"
    );
    assert!(
        second_pane_height >= MIN_PANE_HEIGHT as usize,
        "sibling pane must stay at least {MIN_PANE_HEIGHT} rows after an extreme ratio, got {second_pane_height}"
    );
}
