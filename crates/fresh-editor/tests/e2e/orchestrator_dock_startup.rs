//! E2E coverage for what Orchestrator mode shows *before* its dock exists.
//!
//! A bare `fresh` always opens the dock, but the plugin that mounts it does
//! so from the `ready` hook — fire-and-forget onto the plugin thread, queued
//! behind that thread's own plugin loading. So the first frames used to paint
//! a full-width editor and the dock shoved it aside a few hundred
//! milliseconds later, which is what a user sees as "the UI comes up, then
//! the dock pops in on the left".
//!
//! The host now carves the column when it fires the hook, at the width the
//! mount will ask for, and the dock fills it in place. These drive only the
//! rendered screen (CONTRIBUTING.md §2): where the dock's right-hand wall is
//! before the dock is there, and where it is after.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;

const COLS: u16 = 120;
const ROWS: u16 = 32;
/// `frame::dock_default_width(120)` — 0.28 of the frame, rounded. The wall is
/// the column's last cell, so it lands one to the left of that.
const DOCK_COLS: usize = 34;
const WALL: usize = DOCK_COLS - 1;

/// A git project with the orchestrator plugin (+ shared lib) installed.
fn setup_project() -> (tempfile::TempDir, PathBuf) {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("alphaproj");
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    let ok = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root)
        .status()
        .unwrap()
        .success();
    assert!(ok);
    (temp_dir, root)
}

/// The glyphs in column `WALL`, top to bottom.
fn wall_column(h: &EditorTestHarness) -> Vec<char> {
    h.screen_to_string()
        .lines()
        .map(|row| row.chars().nth(WALL).unwrap_or(' '))
        .collect()
}

/// Where the menu bar starts — the left edge of everything that is not the
/// dock. This is the number that used to change under the user when the dock
/// arrived.
fn chrome_left_edge(h: &EditorTestHarness) -> usize {
    let screen = h.screen_to_string();
    let bar = screen
        .lines()
        .find(|row| row.contains("File") && row.contains("Edit"))
        .unwrap_or_else(|| panic!("no menu bar on screen:\n{screen}"));
    bar.find("File").unwrap()
}

/// **The dock lands in a column that was already there.**
///
/// Two things, and the second is why the first is worth having: the column is
/// carved (walled, and the editor's chrome starts to the right of it) on the
/// frame that follows the `ready` hook — before any of the dock's own content
/// is on screen — and the chrome is in the same place once the dock has
/// filled in. A reservation at a width the mount disagreed with would pass
/// the first and fail the second, and the user would see the re-flow anyway.
#[test]
fn the_dock_lands_in_the_column_startup_carved_for_it() {
    let (_tmp, root) = setup_project();
    let mut h = EditorTestHarness::create(
        COLS,
        ROWS,
        HarnessOptions::new()
            .with_config(Config::default())
            .with_working_dir(root)
            .without_empty_plugins_dir()
            .with_orchestrator_mode(),
    )
    .unwrap();
    h.render().unwrap();

    h.editor_mut().fire_ready_hook();
    h.render().unwrap();
    let wall = wall_column(&h);
    assert!(
        wall.iter().all(|c| *c == '\u{2502}'),
        "the column must be walled from the first frame after `ready`, not \
         when the dock arrives — column {WALL} was:\n{wall:?}\n{}",
        h.screen_to_string()
    );
    let edge = chrome_left_edge(&h);
    assert!(
        edge >= DOCK_COLS,
        "the editor must start right of the reserved column, not across it \
         (menu bar at {edge}):\n{}",
        h.screen_to_string()
    );

    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
    assert_eq!(
        chrome_left_edge(&h),
        edge,
        "the dock moved the editor when it landed — it mounted at a width \
         other than the one held for it:\n{}",
        h.screen_to_string()
    );
}
