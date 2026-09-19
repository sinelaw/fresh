//! `file_explorer.follow_active_buffer`: the sidebar's selection follows the
//! file the user is editing — when the setting says so, and only then.
//!
//! **Assert on the highlight, never on presence.** Revealing a file expands
//! the directories above it, and an expanded directory *stays* expanded. So
//! once either test has opened both files, both names are on screen in both
//! outcomes, and "is `alpha.txt` visible in the tree" cannot tell the
//! setting's two states apart. The row carrying the selection background can,
//! which is what [`explorer_row_highlighted`] reads — by majority vote over
//! rendered cell backgrounds, so it holds under any theme and for either
//! highlight the explorer uses.
//!
//! Which buffer is active is likewise read off the screen: each file carries
//! a marker the other does not, and the tab is switched with the keyboard
//! until the editor is showing the marker asked for.

use crate::common::harness::EditorTestHarness;
use crate::e2e::file_explorer::{explorer_highlighted_rows, explorer_row_highlighted};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

/// Content markers. Distinct from the file names, so a tab label or a tree row
/// can never be mistaken for the editor showing the file.
const ALPHA: &str = "CONTENT_OF_THE_FIRST_FILE";
const BETA: &str = "CONTENT_OF_THE_SECOND_FILE";

/// A project with two files in separate directories, open in two tabs, the
/// sidebar showing, and the keyboard back on the editor.
///
/// The files sit in *different* directories, so following one has to expand a
/// subtree the other does not live in — a follow that only moved the highlight
/// inside an already-open directory would be a weaker thing to assert.
fn harness_with_two_tabs(follow: bool) -> EditorTestHarness {
    let mut config = Config::default();
    config.file_explorer.follow_active_buffer = follow;
    // Wide enough for the tree's indentation plus the longer name.
    config.file_explorer.width = fresh::config::ExplorerWidth::Columns(34);

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();
    let root = harness.project_dir().unwrap();
    fs::create_dir_all(root.join("one")).unwrap();
    fs::create_dir_all(root.join("two")).unwrap();
    let alpha = root.join("one/alpha.txt");
    let beta = root.join("two/beta.txt");
    fs::write(&alpha, format!("{ALPHA}\n")).unwrap();
    fs::write(&beta, format!("{BETA}\n")).unwrap();

    // Show the sidebar, then hand the keyboard back: with focus inside the
    // tree the follow is deliberately suppressed, and the first two tests are
    // about the other case.
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.editor_mut().active_window_mut().focus_editor();
    harness.render().unwrap();

    harness.open_file(&alpha).unwrap();
    harness.wait_for_file_explorer_item("alpha.txt").unwrap();
    harness.open_file(&beta).unwrap();
    harness.wait_for_file_explorer_item("beta.txt").unwrap();
    harness.render().unwrap();

    harness
}

/// Cycle the focused pane's tab with the keyboard until the editor is showing
/// `marker`. Observational on both ends: the only input is the next-buffer
/// key, and the only thing consulted is the screen.
fn switch_until_showing(harness: &mut EditorTestHarness, marker: &str) {
    // One pass around a handful of open buffers is generous; a marker that
    // never appears means the buffer is gone, which is a failure, not a wait.
    for _ in 0..12 {
        if harness.screen_to_string().contains(marker) {
            return;
        }
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }
    panic!(
        "cycling the tabs never reached a buffer showing {marker:?}.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// With the setting on, switching tabs moves the sidebar's highlight onto the
/// newly active file — in both directions, so a highlight that merely happened
/// to be parked on the right row cannot pass.
#[test]
fn follow_on_moves_the_highlight_when_the_tab_changes() {
    let mut harness = harness_with_two_tabs(true);

    switch_until_showing(&mut harness, ALPHA);
    harness
        .wait_until(|h| explorer_row_highlighted(h, "alpha.txt"))
        .unwrap();
    assert!(
        !explorer_row_highlighted(&harness, "beta.txt"),
        "the highlight moved to alpha.txt, so it is no longer on beta.txt.\nScreen:\n{}",
        harness.screen_to_string()
    );

    switch_until_showing(&mut harness, BETA);
    harness
        .wait_until(|h| explorer_row_highlighted(h, "beta.txt"))
        .unwrap();
    assert!(
        !explorer_row_highlighted(&harness, "alpha.txt"),
        "switching back moves the highlight off alpha.txt.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// With the setting off, the same tab switches leave the sidebar alone.
///
/// Both names are on screen here too — the fixture opened both files, and the
/// directories expanded to show them stay expanded — so the only thing
/// separating this from the test above is which row is *highlighted*.
#[test]
fn follow_off_leaves_the_highlight_where_it_was() {
    let mut harness = harness_with_two_tabs(false);

    let before = explorer_highlighted_rows(&harness);

    switch_until_showing(&mut harness, ALPHA);
    assert_eq!(
        explorer_highlighted_rows(&harness),
        before,
        "with following off, switching to alpha.txt must not move the highlight.\nScreen:\n{}",
        harness.screen_to_string()
    );

    switch_until_showing(&mut harness, BETA);
    assert_eq!(
        explorer_highlighted_rows(&harness),
        before,
        "…nor must switching back.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// The follow is suppressed while the keyboard is inside the tree: the user is
/// navigating it, and moving the selection under them would fight their own
/// cursor.
///
/// Driven by opening a file into the focused pane rather than by a tab key,
/// because with the tree holding the keyboard a tab key is the *tree's* — the
/// buffer would never change and the test would pass vacuously. Opening a file
/// is the other half of what following watches for, and it reaches the pane
/// whoever holds the keyboard.
#[test]
fn follow_is_suppressed_while_the_tree_holds_the_keyboard() {
    let mut harness = harness_with_two_tabs(true);
    let alpha = harness.project_dir().unwrap().join("one/alpha.txt");

    // Park the selection where the follow would have to move it from, then
    // give the tree the keyboard. (Focusing the sidebar performs the explicit
    // "show me where I am" reveal, which is why `before` is read after it.)
    switch_until_showing(&mut harness, BETA);
    harness
        .wait_until(|h| explorer_row_highlighted(h, "beta.txt"))
        .unwrap();
    harness.editor_mut().focus_file_explorer();
    harness.render().unwrap();
    let before = explorer_highlighted_rows(&harness);
    assert!(
        before.iter().any(|row| row.contains("beta.txt")),
        "fixture: the highlight should start on beta.txt, got {before:?}"
    );

    harness.open_file(&alpha).unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains(ALPHA),
        "the editor should now be showing alpha.txt.\nScreen:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        explorer_highlighted_rows(&harness),
        before,
        "the tree has the keyboard, so its selection must stay put.\nScreen:\n{}",
        harness.screen_to_string()
    );
}
