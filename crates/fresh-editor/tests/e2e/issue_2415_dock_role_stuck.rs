//! Regression coverage for issue #2415: closing the editor split collapses
//! the tree onto the Utility Dock leaf, which used to keep its role tag.
//! Every later dock-routed open then landed in that leaf as another tab
//! instead of its own pane — and workspace persistence made it permanent.
//!
//! The screen is what tells the two apart: a real dock is a second pane
//! with its own tab bar, the stuck state is one tab bar carrying both. The
//! role tag itself is model state and is covered by the `SplitManager`
//! unit tests in `src/view/split.rs`.

use crate::common::harness::EditorTestHarness;
use std::fs;

/// Screen row of the line containing `needle`, or panic with the screen.
fn row_of(harness: &EditorTestHarness, needle: &str) -> usize {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("expected screen to contain {needle:?}\nScreen:\n{screen}"))
}

/// Open a terminal in the Utility Dock and wait for its tab to render.
fn open_dock_terminal(harness: &mut EditorTestHarness, label: &str) {
    harness
        .run_palette_command("Open Terminal in Utility Dock")
        .unwrap();
    harness.wait_for_screen_contains(label).unwrap();
}

/// Reopening the dock after the collapse must give the panel its own pane
/// again, rather than adding a tab to the leaf that outlived the split.
#[test]
fn test_dock_reopens_as_its_own_pane_after_editor_split_closed() {
    let temp = tempfile::TempDir::new().unwrap();
    let project_dir = temp.path().join("project");
    fs::create_dir(&project_dir).unwrap();
    fs::write(project_dir.join("main.txt"), "hello world\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_dir.clone(),
    )
    .unwrap();
    harness.open_file(&project_dir.join("main.txt")).unwrap();
    harness.render().unwrap();

    // Dock open: the editor and the dock are two panes, so their tab bars
    // are on different rows.
    open_dock_terminal(&mut harness, "*Terminal 0*");
    assert_ne!(
        row_of(&harness, "main.txt"),
        row_of(&harness, "*Terminal 0*"),
        "precondition: the dock is its own pane\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Close the editor split: click into the editor pane to focus it, then
    // run Close Split. The dock leaf is all that survives, and the editor's
    // tab moves onto its tab bar — one pane, one tab bar, both tabs.
    let editor_row = row_of(&harness, "hello world") as u16;
    harness.mouse_click(20, editor_row).unwrap();
    harness.run_palette_command("Close Split").unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .lines()
                .any(|l| l.contains("*Terminal 0*") && l.contains("main.txt"))
        })
        .unwrap();

    // Reopen the dock. With the role stranded on the surviving leaf, the new
    // terminal joins it as a second tab on the same tab bar; it must get its
    // own pane instead.
    open_dock_terminal(&mut harness, "*Terminal 1*");
    assert_ne!(
        row_of(&harness, "*Terminal 0*"),
        row_of(&harness, "*Terminal 1*"),
        "the reopened dock shares a tab bar with the former dock leaf instead of \
         opening as its own pane\nScreen:\n{}",
        harness.screen_to_string()
    );
}
