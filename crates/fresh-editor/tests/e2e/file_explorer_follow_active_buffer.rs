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
use fresh::config::Config;
use std::fs;

/// Content markers. Distinct from the file names, so a tab label or a tree row
/// can never be mistaken for the editor showing the file.
const ALPHA: &str = "CONTENT_OF_THE_FIRST_FILE";
const BETA: &str = "CONTENT_OF_THE_SECOND_FILE";

/// A project with two files at its root, open in two tabs, the sidebar
/// showing, and the keyboard back on the editor.
///
/// **Both files are at the root on purpose.** A file in a subdirectory is not
/// on screen until something expands that directory, and expanding it is
/// precisely what the "off" case must *not* do — so a fixture that waited for
/// a nested name to appear could only ever hang in the test that matters most.
/// At the root both rows are present from the first frame, which is what makes
/// "which row is highlighted" answerable in both directions. Revealing through
/// a collapsed subtree is the deliberate-reveal path's job and is covered by
/// its own tests.
fn harness_with_two_tabs(follow: bool) -> EditorTestHarness {
    let mut config = Config::default();
    config.file_explorer.follow_active_buffer = follow;
    // Wide enough for the tree's indentation plus the longer name.
    config.file_explorer.width = fresh::config::ExplorerWidth::Columns(34);

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();
    let root = harness.project_dir().unwrap();
    let alpha = root.join("alpha.txt");
    let beta = root.join("beta.txt");
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

/// Make `file` the file the user is looking at, and confirm the editor is
/// showing it.
///
/// **Opening rather than clicking its tab.** What the setting watches for is
/// the pane's buffer changing — `Window::set_pane_buffer`, the one write
/// allowed to change which buffer a pane shows — and a tab click, a
/// jump-to-definition and an open all funnel through it, so any of them
/// exercises the same fork. Opening is the one that does not also depend on
/// hit-testing the tab strip, which is a second thing to get right and not the
/// thing under test. (Driving it by tab click was tried: locating the tab in
/// the rendered row needs screen columns rather than byte or char offsets —
/// the panel border beside it is multi-byte box glyphs — and even with that
/// right the click landed somewhere that reshaped the panes.)
fn show_file(harness: &mut EditorTestHarness, name: &str, marker: &str) {
    let path = harness.project_dir().unwrap().join(name);
    harness.open_file(&path).unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains(marker),
        "the editor should be showing {name}.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// With the setting on, switching tabs moves the sidebar's highlight onto the
/// newly active file — in both directions, so a highlight that merely happened
/// to be parked on the right row cannot pass.
#[test]
fn follow_on_moves_the_highlight_when_the_tab_changes() {
    let mut harness = harness_with_two_tabs(true);

    show_file(&mut harness, "alpha.txt", ALPHA);
    harness
        .wait_until(|h| explorer_row_highlighted(h, "alpha.txt"))
        .unwrap();
    assert!(
        !explorer_row_highlighted(&harness, "beta.txt"),
        "the highlight moved to alpha.txt, so it is no longer on beta.txt.\nScreen:\n{}",
        harness.screen_to_string()
    );

    show_file(&mut harness, "beta.txt", BETA);
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
    // Without this the test could pass by there being no highlight at all:
    // "unchanged" only means something if there was something to change.
    assert!(
        !before.is_empty(),
        "fixture: some row should be highlighted to begin with.\nScreen:\n{}",
        harness.screen_to_string()
    );

    show_file(&mut harness, "alpha.txt", ALPHA);
    assert_eq!(
        explorer_highlighted_rows(&harness),
        before,
        "with following off, switching to alpha.txt must not move the highlight.\nScreen:\n{}",
        harness.screen_to_string()
    );

    show_file(&mut harness, "beta.txt", BETA);
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
    let alpha = harness.project_dir().unwrap().join("alpha.txt");

    // Park the selection where the follow would have to move it from, then
    // give the tree the keyboard. (Focusing the sidebar performs the explicit
    // "show me where I am" reveal, which is why `before` is read after it.)
    show_file(&mut harness, "beta.txt", BETA);
    harness
        .wait_until(|h| explorer_row_highlighted(h, "beta.txt"))
        .unwrap();
    harness.editor_mut().focus_file_explorer();
    // Focusing the sidebar performs the explicit reveal, which hands the tree
    // to an async expand and paints the panel blank until it lands — so wait
    // for the highlight to come back rather than sampling the empty frame.
    harness
        .wait_until(|h| explorer_row_highlighted(h, "beta.txt"))
        .unwrap();
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
