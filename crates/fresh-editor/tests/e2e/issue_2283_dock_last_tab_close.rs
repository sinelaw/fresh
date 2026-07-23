//! Regression coverage for issue #2283.
//!
//! Closing the last editor tab while a Utility Dock (holding terminals)
//! is present used to collapse the editor leaf and promote the dock's
//! terminals into the main tab bar — because the dock leaf was counted as
//! a "peer split" in `close_tab_in_split`.
//!
//! Desired behaviour: keep the editor leaf alive but EMPTY (a hidden
//! synthetic-placeholder buffer renders the centered empty-state), leave
//! the dock and its terminals untouched, and route a subsequently-opened
//! file back into the editor leaf (never the dock).
//!
//! These assert on the split-tree / view-state model directly (the same
//! style as the dock tests in `live_grep.rs`), so they don't depend on
//! terminal PTY rendering.

use crate::common::harness::EditorTestHarness;
use fresh::input::keybindings::Action;
use fresh::model::event::LeafId;
use fresh::view::split::SplitRole;
use std::fs;

/// Open a single file in the (sole) editor leaf, then open a terminal in a
/// freshly-created Utility Dock. Returns the harness, the kept-alive temp
/// dir, and the ids the tests assert against.
struct DockFixture {
    harness: EditorTestHarness,
    _temp: tempfile::TempDir,
    editor_leaf: LeafId,
    dock_leaf: LeafId,
    file_buffer: fresh::model::event::BufferId,
    terminal_buffer: fresh::model::event::BufferId,
}

fn setup_editor_with_dock_terminal() -> DockFixture {
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("main.txt");
    fs::write(&file, "hello world\n").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_file(&file).unwrap();

    let file_buffer = harness.editor().active_buffer();
    let editor_leaf = harness.editor().split_manager_for_tests().active_split();

    // Create the dock + terminal. `OpenTerminalInDock` splits at the root,
    // tags the new leaf `UtilityDock`, and seeds it with a terminal buffer.
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::OpenTerminalInDock);

    let (dock_leaf, terminal_buffer) = {
        let sm = harness.editor().split_manager_for_tests();
        let dock_leaf = sm
            .find_leaf_by_role(SplitRole::UtilityDock)
            .expect("OpenTerminalInDock must create a dock leaf");
        let terminal_buffer = sm
            .get_buffer_id(dock_leaf.into())
            .expect("dock leaf must hold the terminal buffer");
        (dock_leaf, terminal_buffer)
    };

    // Sanity: two distinct leaves; the terminal lives in the dock, and the
    // editor leaf holds the file (not the terminal).
    assert_ne!(editor_leaf, dock_leaf);
    assert!(harness.editor().active_window().is_terminal_buffer(terminal_buffer));
    assert!(
        editor_leaf_tabs(&harness, editor_leaf).contains(&file_buffer),
        "precondition: the editor leaf should hold the opened file"
    );
    assert!(
        !editor_leaf_tabs(&harness, editor_leaf).contains(&terminal_buffer),
        "precondition: the terminal must NOT be in the editor leaf"
    );

    DockFixture {
        harness,
        _temp: temp,
        editor_leaf,
        dock_leaf,
        file_buffer,
        terminal_buffer,
    }
}

/// Buffer-tab ids currently open in a leaf.
fn editor_leaf_tabs(
    harness: &EditorTestHarness,
    leaf: LeafId,
) -> Vec<fresh::model::event::BufferId> {
    harness
        .editor()
        .split_view_state_for_tests(leaf)
        .map(|vs| vs.buffer_tab_ids_vec())
        .unwrap_or_default()
}

fn leaf_exists(harness: &EditorTestHarness, leaf: LeafId) -> bool {
    harness
        .editor()
        .split_manager_for_tests()
        .root()
        .leaf_split_ids()
        .contains(&leaf)
}

/// Is `leaf`'s active buffer the hidden synthetic-placeholder that renders
/// the empty editor state?
fn leaf_shows_empty_placeholder(harness: &EditorTestHarness, leaf: LeafId) -> bool {
    let Some(buf) = harness
        .editor()
        .split_manager_for_tests()
        .get_buffer_id(leaf.into())
    else {
        return false;
    };
    harness
        .editor()
        .active_window()
        .buffer_metadata
        .get(&buf)
        .is_some_and(|m| m.synthetic_placeholder && m.hidden_from_tabs)
}

// ---------------------------------------------------------------------------
// Test 1: last editor tab close keeps an empty editor leaf; dock survives.
// ---------------------------------------------------------------------------

#[test]
fn test_closing_last_editor_tab_with_dock_keeps_empty_editor_leaf() {
    let mut fx = setup_editor_with_dock_terminal();

    // Close the sole editor tab (the file).
    let closed = fx
        .harness
        .editor_mut()
        .close_tab_in_split(fx.file_buffer, fx.editor_leaf);
    assert!(closed, "closing the file tab should succeed");

    // The editor leaf survives...
    assert!(
        leaf_exists(&fx.harness, fx.editor_leaf),
        "editor leaf must NOT collapse when a dock is present"
    );
    // ...but empty: only the hidden placeholder, no file/terminal tab.
    let tabs = editor_leaf_tabs(&fx.harness, fx.editor_leaf);
    assert!(
        !tabs.contains(&fx.file_buffer),
        "the closed file must be gone from the editor leaf, got {tabs:?}"
    );
    assert!(
        !tabs.contains(&fx.terminal_buffer),
        "a dock terminal must NOT be adopted into the empty editor leaf, got {tabs:?}"
    );
    assert!(
        leaf_shows_empty_placeholder(&fx.harness, fx.editor_leaf),
        "the editor leaf should render the synthetic empty-state placeholder"
    );

    // The dock and its terminal are untouched.
    assert!(
        leaf_exists(&fx.harness, fx.dock_leaf),
        "the dock leaf must survive"
    );
    assert_eq!(
        fx.harness
            .editor()
            .split_manager_for_tests()
            .find_leaf_by_role(SplitRole::UtilityDock),
        Some(fx.dock_leaf),
        "the dock role must still be carried by the same leaf"
    );
    assert!(
        editor_leaf_tabs(&fx.harness, fx.dock_leaf).contains(&fx.terminal_buffer),
        "the terminal must still live in the dock"
    );
}

// ---------------------------------------------------------------------------
// Test 2: reopening a file after the empty state lands in the editor leaf.
// ---------------------------------------------------------------------------

#[test]
fn test_reopen_file_after_empty_state_lands_in_editor_not_dock() {
    let mut fx = setup_editor_with_dock_terminal();

    fx.harness
        .editor_mut()
        .close_tab_in_split(fx.file_buffer, fx.editor_leaf);
    assert!(leaf_shows_empty_placeholder(&fx.harness, fx.editor_leaf));

    // Snapshot the dock tab list before reopening.
    let dock_tabs_before = editor_leaf_tabs(&fx.harness, fx.dock_leaf);

    // Reopen a (different) file.
    let file2 = fx._temp.path().join("second.txt");
    fs::write(&file2, "second file\n").unwrap();
    fx.harness.open_file(&file2).unwrap();
    let reopened = fx.harness.editor().active_buffer();

    // It must land in the editor leaf, never the dock.
    assert!(
        editor_leaf_tabs(&fx.harness, fx.editor_leaf).contains(&reopened),
        "the reopened file must land in the editor leaf"
    );
    assert!(
        !editor_leaf_tabs(&fx.harness, fx.dock_leaf).contains(&reopened),
        "the reopened file must NOT be mixed into the dock's tabs"
    );
    // The dock tab list is unchanged (no editor/terminal mixing).
    assert_eq!(
        editor_leaf_tabs(&fx.harness, fx.dock_leaf),
        dock_tabs_before,
        "the dock tab list must be unchanged after reopening a file"
    );
}

// ---------------------------------------------------------------------------
// Test 3: batch Close-All in the editor split with a dock present behaves the
// same — empty editor leaf, dock survives.
// ---------------------------------------------------------------------------

#[test]
fn test_close_all_editor_tabs_with_dock_keeps_empty_editor_leaf() {
    let mut fx = setup_editor_with_dock_terminal();

    // Add a second file so the editor split has multiple tabs to batch-close.
    // Opening a file while the dock is focused routes it to the editor leaf
    // (the dock-redirect), so no manual focus is needed.
    let file2 = fx._temp.path().join("extra.txt");
    fs::write(&file2, "extra file\n").unwrap();
    fx.harness.open_file(&file2).unwrap();
    assert!(editor_leaf_tabs(&fx.harness, fx.editor_leaf).len() >= 2);

    // Close every tab in the editor leaf at once.
    fx.harness
        .editor_mut()
        .close_all_tabs_in_split(fx.editor_leaf);

    // Same outcome as the interactive close: empty editor leaf, dock alive.
    assert!(leaf_exists(&fx.harness, fx.editor_leaf));
    assert!(
        leaf_shows_empty_placeholder(&fx.harness, fx.editor_leaf),
        "batch close must leave the editor leaf in the empty placeholder state"
    );
    let tabs = editor_leaf_tabs(&fx.harness, fx.editor_leaf);
    assert!(
        !tabs.contains(&fx.terminal_buffer),
        "batch close must not adopt a dock terminal into the editor leaf, got {tabs:?}"
    );
    assert!(leaf_exists(&fx.harness, fx.dock_leaf));
    assert!(editor_leaf_tabs(&fx.harness, fx.dock_leaf).contains(&fx.terminal_buffer));
}

// ---------------------------------------------------------------------------
// Test 4: regression — a dock must not make an editor split immortal. With
// two editor leaves, closing the last tab of ONE still collapses it.
// ---------------------------------------------------------------------------

#[test]
fn test_two_editor_leaves_with_dock_last_tab_still_collapses() {
    let temp = tempfile::TempDir::new().unwrap();
    let file1 = temp.path().join("a.txt");
    let file2 = temp.path().join("b.txt");
    fs::write(&file1, "aaa\n").unwrap();
    fs::write(&file2, "bbb\n").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_file(&file1).unwrap();
    let leaf_a = harness.editor().split_manager_for_tests().active_split();
    let file1_buffer = harness.editor().active_buffer();

    // Second editor leaf (side-by-side), then give it its own distinct file.
    harness.editor_mut().split_pane_vertical();
    let leaf_b = harness.editor().split_manager_for_tests().active_split();
    assert_ne!(leaf_a, leaf_b);
    harness.open_file(&file2).unwrap();
    let file2_buffer = harness.editor().active_buffer();
    // Drop the duplicated file1 tab from leaf B so file2 is its only tab and
    // is the *last viewport* of that buffer.
    harness
        .editor_mut()
        .close_tab_in_split(file1_buffer, leaf_b);
    assert_eq!(editor_leaf_tabs(&harness, leaf_b), vec![file2_buffer]);

    // Add the dock.
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::OpenTerminalInDock);
    let dock_leaf = harness
        .editor()
        .split_manager_for_tests()
        .find_leaf_by_role(SplitRole::UtilityDock)
        .expect("dock leaf must exist");

    // Close leaf B's last tab. Because another EDITOR leaf (A) exists, the
    // leaf must collapse — the dock does not keep it alive.
    harness
        .editor_mut()
        .close_tab_in_split(file2_buffer, leaf_b);

    assert!(
        !leaf_exists(&harness, leaf_b),
        "leaf B must collapse when another editor leaf exists"
    );
    assert!(leaf_exists(&harness, leaf_a), "leaf A must survive");
    assert!(leaf_exists(&harness, dock_leaf), "the dock must survive");
    assert_eq!(
        harness
            .editor()
            .split_manager_for_tests()
            .find_leaf_by_role(SplitRole::UtilityDock),
        Some(dock_leaf)
    );
}
