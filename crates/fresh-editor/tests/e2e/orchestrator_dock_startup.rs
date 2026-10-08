//! What a launch shows *before* the dock's content exists. The plugin
//! mounts the dock from `ready`, after every plugin has loaded; the host
//! carves the column from the first frame (`orchestrator.manifest.json`,
//! `chrome.json`) and the mount fills it in place. These drive only the
//! rendered screen (CONTRIBUTING.md §2).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
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

/// Where the menu bar starts: the number that used to change when the dock
/// arrived.
fn chrome_left_edge(h: &EditorTestHarness) -> usize {
    let screen = h.screen_to_string();
    let bar = screen
        .lines()
        .find(|row| row.contains("File") && row.contains("Edit"))
        .unwrap_or_else(|| panic!("no menu bar on screen:\n{screen}"));
    // A column, not a byte offset: the dock's glyphs are multi-byte.
    let cells: Vec<char> = bar.chars().collect();
    cells
        .windows(4)
        .position(|w| w == ['F', 'i', 'l', 'e'])
        .unwrap()
}

/// The column is carved on the first frame, before `ready` has fired, and
/// the chrome is in the same place once the dock has filled it in.
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

/// An ordinary `fresh .`: the manifest's `open` (via `autoOpenDock`) opens it.
#[test]
fn an_ordinary_launch_lands_the_dock_in_the_column_carved_for_it() {
    the_dock_lands_in_the_column_startup_carved_for_it(false);
}

/// Closed with Toggle Dock, quit, relaunched: no column and `ready` mounts
/// nothing. Opened again, quit, relaunched: the column is back on the first
/// frame. Each open or close is recorded as `autoOpenDock` (`never` /
/// `always`), and every launch reads the config back from disk, so the walk
/// covers the setting the next real start obeys.
///
/// Driven in both launch modes: a bare `fresh` used to force the column open
/// regardless (#3442).
fn the_dock_is_remembered_across_launches(orchestrator_mode: bool) {
    let (_tmp, root) = setup_project();
    // No `autoOpenDock`: `auto`, the state before any instruction.
    let user = UserConfig::new(serde_json::json!({}));

    // First launch: open by default. Close it.
    let mut h = user.launch(&root, orchestrator_mode);
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
    user.wait_for(&mut h, "never");
    h.shutdown(false).unwrap();
    drop(h);

    // Second launch: closed is remembered — no column, and nothing mounts.
    let mut h = user.launch(&root, orchestrator_mode);
    h.render().unwrap();
    assert!(
        chrome_left_edge(&h) < DOCK_COLS,
        "a dock the user closed must not be held open:\n{}",
        h.screen_to_string()
    );
    assert_no_dock_after_ready(&mut h);
    // Open it again for the next launch.
    toggle_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
    user.wait_for(&mut h, "always");
    h.shutdown(false).unwrap();
    drop(h);

    // Third launch: open is remembered — the column is on the first frame.
    let mut h = user.launch(&root, orchestrator_mode);
    h.render().unwrap();
    assert!(
        wall_column(&h).iter().all(|c| *c == '\u{2502}'),
        "a dock the user left open comes back on the first frame:\n{}",
        h.screen_to_string()
    );
}

/// An ordinary `fresh .`.
#[test]
fn an_ordinary_launch_remembers_the_dock_across_launches() {
    the_dock_is_remembered_across_launches(false);
}

/// A bare `fresh` (Orchestrator mode) — issue #3442.
#[test]
fn orchestrator_mode_remembers_the_dock_across_launches() {
    the_dock_is_remembered_across_launches(true);
}

/// The user config every launch of the walk below resolves, and the
/// `autoOpenDock` in it.
struct UserConfig {
    dir_context: fresh::config_io::DirectoryContext,
    _home: tempfile::TempDir,
}

impl UserConfig {
    fn new(settings: serde_json::Value) -> Self {
        let home = tempfile::TempDir::new().unwrap();
        let dir_context = fresh::config_io::DirectoryContext::for_testing(home.path());
        fs::create_dir_all(&dir_context.config_dir).unwrap();
        let body = serde_json::json!({ "plugins": { "orchestrator": { "settings": settings } } });
        fs::write(
            dir_context.config_dir.join("config.json"),
            serde_json::to_vec_pretty(&body).unwrap(),
        )
        .unwrap();
        Self {
            dir_context,
            _home: home,
        }
    }

    /// A launch handed the config as it stands on disk — a bare `fresh`
    /// (Orchestrator mode) or an ordinary `fresh .`. The harness injects the
    /// config rather than resolving layers, so this is what a real launch
    /// would resolve.
    fn launch(&self, root: &std::path::Path, orchestrator_mode: bool) -> EditorTestHarness {
        let config =
            Config::load_from_file(self.dir_context.config_dir.join("config.json")).unwrap();
        let mut options = HarnessOptions::new()
            .with_config(config)
            .with_working_dir(root.to_path_buf())
            .with_shared_dir_context(self.dir_context.clone())
            .without_empty_plugins_dir()
            .with_startup_chrome();
        if orchestrator_mode {
            options = options.with_orchestrator_mode();
        }
        EditorTestHarness::create(COLS, ROWS, options).unwrap()
    }

    /// Wait for `autoOpenDock` on disk to read `want`. A file, not a model
    /// accessor (CONTRIBUTING §2): it is what the next launch reads, and the
    /// plugin's write is queued behind its thread, so this waits rather than
    /// sampling once.
    fn wait_for(&self, h: &mut EditorTestHarness, want: &str) {
        let path = self.dir_context.config_dir.join("config.json");
        h.wait_until(|_| {
            fs::read_to_string(&path)
                .ok()
                .and_then(|raw| serde_json::from_str::<serde_json::Value>(&raw).ok())
                .and_then(|v| {
                    v.pointer("/plugins/orchestrator/settings/autoOpenDock")
                        .cloned()
                })
                == Some(serde_json::json!(want))
        })
        .unwrap_or_else(|e| panic!("`autoOpenDock` never reached {want:?}: {e}"));
    }
}

/// Click a top-level menu, then one of its rows, by their rendered text.
fn click_menu_row(h: &mut EditorTestHarness, menu: &str, row: &str) {
    let (col, line) = h
        .find_text_on_screen(menu)
        .unwrap_or_else(|| panic!("no {menu:?} on screen:\n{}", h.screen_to_string()));
    h.mouse_click(col, line).unwrap();
    h.wait_until(|h| h.find_text_on_screen(row).is_some())
        .unwrap();
    let (col, line) = h.find_text_on_screen(row).unwrap();
    h.mouse_click(col, line).unwrap();
}

/// Run a palette command by its title.
fn run_command(h: &mut EditorTestHarness, title: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(title).unwrap();
    h.wait_until(|h| h.screen_to_string().contains(title))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
}

/// Flip the dock with the command the `View` menu row dispatches.
fn toggle_dock(h: &mut EditorTestHarness) {
    run_command(h, "Orchestrator: Toggle Dock");
}

/// Fire `ready`, then check nothing mounted. The hook is round-tripped
/// through the plugin thread with a command that does not touch the dock —
/// the Machines dialog, queued behind it — so a dock that wrongly mounted is
/// on screen by the time we look.
fn assert_no_dock_after_ready(h: &mut EditorTestHarness) {
    h.editor_mut().fire_ready_hook();
    run_command(h, "Orchestrator: Machines");
    // The dialog's own button, not the palette row that also says "Machines".
    h.wait_until(|h| h.screen_to_string().contains("Add machine"))
        .unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Add machine"))
        .unwrap();
    assert!(
        !h.screen_to_string().contains("+ New"),
        "{}",
        h.screen_to_string()
    );
}

/// The reported bug: a config holding the pre-#3442 `autoOpenDock: false`
/// (read, and rewritten, as `never`) kept the dock closed at every launch, and
/// opening it from `View ▸ Orchestrator Dock` lasted only until the restart.
/// The open is the user's latest instruction, so the next launch follows it.
#[test]
fn an_open_from_the_view_menu_outlives_never() {
    let (_tmp, root) = setup_project();
    let user = UserConfig::new(serde_json::json!({ "autoOpenDock": false }));

    let mut h = user.launch(&root, true);
    h.render().unwrap();
    assert!(
        chrome_left_edge(&h) < DOCK_COLS,
        "`never` holds no column:\n{}",
        h.screen_to_string()
    );
    assert_no_dock_after_ready(&mut h);
    click_menu_row(&mut h, "View", "Orchestrator Dock");
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
    user.wait_for(&mut h, "always");
    h.shutdown(false).unwrap();
    drop(h);

    let mut h = user.launch(&root, true);
    h.render().unwrap();
    assert!(
        wall_column(&h).iter().all(|c| *c == '\u{2502}'),
        "the dock the user opened comes back on the first frame:\n{}",
        h.screen_to_string()
    );
    h.editor_mut().fire_ready_hook();
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
}

/// The mirror case: under `always`, a close with the dock's own `×` is the
/// latest instruction, so the next launch starts without the dock.
#[test]
fn a_close_with_the_x_outlives_always() {
    let (_tmp, root) = setup_project();
    let user = UserConfig::new(serde_json::json!({ "autoOpenDock": "always" }));

    let mut h = user.launch(&root, true);
    h.render().unwrap();
    h.editor_mut().fire_ready_hook();
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
    // The header's `×` sits against the dock's wall.
    let (col, row) = h
        .find_text_on_screen("×\u{2502}")
        .unwrap_or_else(|| panic!("no dock `×`:\n{}", h.screen_to_string()));
    h.mouse_click(col, row).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("+ New"))
        .unwrap();
    user.wait_for(&mut h, "never");
    h.shutdown(false).unwrap();
    drop(h);

    let mut h = user.launch(&root, true);
    h.render().unwrap();
    assert!(
        chrome_left_edge(&h) < DOCK_COLS,
        "the dock the user closed must not be held open:\n{}",
        h.screen_to_string()
    );
    assert_no_dock_after_ready(&mut h);
}
