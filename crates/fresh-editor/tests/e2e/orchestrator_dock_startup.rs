//! E2E coverage for what a launch shows *before* the dock's content exists.
//!
//! The dock's content comes from the orchestrator plugin, mounted from its
//! `ready` hook — fire-and-forget onto the plugin thread, after every plugin
//! has loaded. Left to that, the first frames paint a full-width editor and
//! the dock shoves it aside when it lands: "the UI comes up, then the dock
//! pops in on the left".
//!
//! The host now knows about the dock before any plugin runs — the plugin
//! declares it in `orchestrator.manifest.json`, the user's last state is
//! remembered in `chrome.json` — and carves the column from the first
//! frame; the mount fills it in place. These drive only the rendered screen
//! (CONTRIBUTING.md §2): where the editor's chrome starts before the dock is
//! there, and where it starts after.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;

const COLS: u16 = 120;
const ROWS: u16 = 32;
/// The manifest's rule at 120 columns — 0.28 of the frame, rounded. The wall
/// is the column's last cell, so it lands one to the left of that.
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
    // A column, not a byte offset: the dock's own glyphs to the left of the
    // bar are multi-byte, and there are more of them once the dock is in.
    let cells: Vec<char> = bar.chars().collect();
    cells
        .windows(4)
        .position(|w| w == ['F', 'i', 'l', 'e'])
        .unwrap()
}

/// **The dock lands in a column that was already there.**
///
/// Two things, and the second is why the first is worth having: the column is
/// carved (walled, and the editor's chrome starts to the right of it) on the
/// very first frame — before the `ready` hook has even been fired, let alone
/// any of the dock's own content drawn — and the chrome is in the same place
/// once the dock has filled in. A column at a width the mount disagreed with
/// would pass the first and fail the second, and the user would see the
/// re-flow anyway.
fn the_dock_lands_in_the_column_startup_carved_for_it(orchestrator_mode: bool) {
    let (_tmp, root) = setup_project();
    let mut options = HarnessOptions::new()
        .with_config(Config::default())
        .with_working_dir(root)
        .without_empty_plugins_dir()
        .with_startup_chrome();
    if orchestrator_mode {
        options = options.with_orchestrator_mode();
    }
    let mut h = EditorTestHarness::create(COLS, ROWS, options).unwrap();
    h.render().unwrap();
    let wall = wall_column(&h);
    assert!(
        wall.iter().all(|c| *c == '\u{2502}'),
        "the column must be walled from the first frame, not when the dock \
         arrives — column {WALL} was:\n{wall:?}\n{}",
        h.screen_to_string()
    );
    let edge = chrome_left_edge(&h);
    assert!(
        edge >= DOCK_COLS,
        "the editor must start right of the reserved column, not across it \
         (menu bar at {edge}):\n{}",
        h.screen_to_string()
    );

    h.editor_mut().fire_ready_hook();
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

/// A bare `fresh` (Orchestrator mode): the dock always opens.
#[test]
fn orchestrator_mode_lands_the_dock_in_the_column_carved_for_it() {
    the_dock_lands_in_the_column_startup_carved_for_it(true);
}

/// An ordinary `fresh .`: the manifest's `open` (via `autoOpenDock`, on by
/// default) opens the dock — and this is the launch on which the plugin's
/// loading, not just its `ready` hook, used to stand between the first
/// frame and the dock.
#[test]
fn an_ordinary_launch_lands_the_dock_in_the_column_carved_for_it() {
    the_dock_lands_in_the_column_startup_carved_for_it(false);
}

/// **The dock comes back the way it was left.** Closed with Toggle Dock and
/// the editor quit and relaunched against the same data directory: no
/// column, and `ready` mounts nothing. Opened again, quit and relaunched:
/// the column is back on the first frame. (The quit is what records it —
/// see `Editor::save_dock_chrome`.)
#[test]
fn the_dock_is_remembered_across_launches() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::config_io::DirectoryContext;

    let (_tmp, root) = setup_project();
    let home = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(home.path());
    let launch = |dir_context: DirectoryContext| {
        EditorTestHarness::create(
            COLS,
            ROWS,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(root.clone())
                .with_shared_dir_context(dir_context)
                .without_empty_plugins_dir()
                .with_startup_chrome(),
        )
        .unwrap()
    };
    let toggle_dock = |h: &mut EditorTestHarness| {
        h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        h.wait_for_prompt().unwrap();
        h.type_text("Orchestrator: Toggle Dock").unwrap();
        h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
            .unwrap();
        h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    };

    // First launch: open by the manifest. Close it.
    let mut h = launch(dir_context.clone());
    h.render().unwrap();
    h.editor_mut().fire_ready_hook();
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
    toggle_dock(&mut h);
    h.wait_until(|h| !h.screen_to_string().contains("+ New"))
        .unwrap();
    assert_eq!(
        wall_column(&h).iter().filter(|c| **c == '\u{2502}').count(),
        0
    );
    h.shutdown(false).unwrap();
    drop(h);

    // Second launch: closed is remembered — no column, and nothing mounts.
    let mut h = launch(dir_context.clone());
    h.render().unwrap();
    assert!(
        chrome_left_edge(&h) < DOCK_COLS,
        "a dock the user closed must not be held open:\n{}",
        h.screen_to_string()
    );
    h.editor_mut().fire_ready_hook();
    // Let the hook round-trip; the dock must stay away.
    for _ in 0..20 {
        h.tick_and_render().unwrap();
    }
    assert!(
        !h.screen_to_string().contains("+ New"),
        "{}",
        h.screen_to_string()
    );
    // Open it again for the next launch.
    toggle_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
    h.shutdown(false).unwrap();
    drop(h);

    // Third launch: open is remembered — the column is on the first frame.
    let mut h = launch(dir_context);
    h.render().unwrap();
    assert!(
        wall_column(&h).iter().all(|c| *c == '\u{2502}'),
        "a dock the user left open comes back on the first frame:\n{}",
        h.screen_to_string()
    );
}
