//! E2E: invoking Search & Replace twice while the panel is still opening
//! must still produce a fully rendered panel (issue #2953).
//!
//! `openPanel` in `plugins/search_replace.ts` awaits the host round-trip that
//! creates the panel's virtual buffer, so a second `Alt+A` arriving inside
//! that await used to see the half-built panel object, take the "already
//! open" branch, and mount the widget tree against `resultsBufferId === 0`.
//! The host has no buffer 0, so the mount failed, the panel stayed bound to
//! the bogus id, and every later update failed too — leaving a panel with no
//! search field, no replace field, no files field, no toggles and no footer,
//! with the terminal cursor stranded in the bottom-right chrome.
//!
//! The reproduction needs two key presses that are *not* separated by a
//! plugin drain, which is what `send_key_without_drain` is for: the normal
//! `send_key` settles all plugin work before returning and so can never
//! express "the user pressed the key again before the first press finished".

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Project directory with the search_replace plugin and a file to search.
fn setup_project() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "search_replace");

    fs::write(
        project_root.join("alpha.txt"),
        "hello world\nfoo bar\nhello again\n",
    )
    .unwrap();

    (temp_dir, project_root)
}

/// Assert the panel's control rows are all on screen. Any one of them
/// missing is the #2953 symptom.
fn assert_panel_fully_rendered(harness: &EditorTestHarness) {
    let screen = harness.screen_to_string();
    for label in ["Search:", "Replace:", "Files:", "Matches"] {
        assert!(
            screen.contains(label),
            "Search & Replace panel is missing the '{label}' row — the widget \
             tree carrying the inputs was never painted. Screen:\n{screen}"
        );
    }
}

/// Two `Alt+A` presses delivered before the first open finishes must leave a
/// complete panel with the caret in the search field.
#[test]
fn test_double_invocation_while_opening_still_renders_panel() {
    let (_temp_dir, project_root) = setup_project();
    let start_file = project_root.join("alpha.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    // Both presses reach the plugin thread before any of the work they queue
    // is drained — the second lands inside the first's `await`.
    harness
        .send_key_without_drain(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();
    harness
        .send_key_without_drain(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();

    // Semantic wait: the search input row is the thing the bug removes.
    // Without the fix this never appears and the test is killed externally.
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    // Let the second invocation land too — it must not undo the first.
    harness.wait_for_async_quiescence(3).unwrap();

    assert_panel_fully_rendered(&harness);

    // The caret belongs in the search field, not in the bottom-right chrome.
    let (search_col, search_row) = harness
        .find_text_on_screen("Search:")
        .expect("search label on screen");
    let cursor = harness
        .render_observing_cursor()
        .unwrap()
        .expect("a visible terminal cursor");
    assert_eq!(
        cursor.1, search_row,
        "caret should sit on the search input row ({search_row}), got {cursor:?}"
    );
    assert!(
        cursor.0 > search_col,
        "caret should sit after the 'Search:' label (col > {search_col}), got {cursor:?}"
    );

    // And the panel still works: typing goes into the search field.
    harness.type_text("hello").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("hello"))
        .unwrap();
    assert_panel_fully_rendered(&harness);
}

/// Control: a single invocation renders the same complete panel. Guards
/// against a re-entrancy guard that swallows the *first* open too.
#[test]
fn test_single_invocation_renders_panel() {
    let (_temp_dir, project_root) = setup_project();
    let start_file = project_root.join("alpha.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.wait_for_async_quiescence(3).unwrap();

    assert_panel_fully_rendered(&harness);
}
