//! The dock narrows with the terminal, as the file explorer does.
//!
//! Its width was a fixed floor of 24 columns under its share of the frame,
//! and a dragged width was kept in columns — so on a small terminal, or after
//! a drag on a big one, the dock took most of the screen, its title strip
//! ("Orchestrator  Menu ▾") crowding the editor's menu bar off it (#3517).
//! A width is a share of the frame now, dragged or not.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;

const ROWS: u16 = 32;

/// A git project with the orchestrator plugin (+ shared lib) installed, and
/// a config tree of its own so `chrome.json` is this test's.
struct Project {
    root: PathBuf,
    dir_context: fresh::config_io::DirectoryContext,
    _tmp: tempfile::TempDir,
}

impl Project {
    fn new() -> Self {
        let tmp = tempfile::TempDir::new().unwrap();
        let root = tmp.path().join("alphaproj");
        fs::create_dir(&root).unwrap();
        let plugins_dir = root.join("plugins");
        fs::create_dir(&plugins_dir).unwrap();
        copy_plugin_lib(&plugins_dir);
        copy_plugin(&plugins_dir, "orchestrator");
        fs::write(root.join("readme.txt"), "hello\n").unwrap();
        assert!(std::process::Command::new("git")
            .args(["init", "-q"])
            .current_dir(&root)
            .status()
            .unwrap()
            .success());
        let dir_context = fresh::config_io::DirectoryContext::for_testing(&tmp.path().join("home"));
        fs::create_dir_all(&dir_context.config_dir).unwrap();
        fs::create_dir_all(&dir_context.data_dir).unwrap();
        Self {
            root,
            dir_context,
            _tmp: tmp,
        }
    }

    fn chrome_json(&self) -> PathBuf {
        self.dir_context.data_dir.join("chrome.json")
    }

    /// A bare `fresh` (Orchestrator mode), `cols` wide, dock mounted.
    fn launch(&self, cols: u16) -> EditorTestHarness {
        let options = HarnessOptions::new()
            .with_config(Config::default())
            .with_working_dir(self.root.clone())
            .with_shared_dir_context(self.dir_context.clone())
            .without_empty_plugins_dir()
            .with_startup_chrome()
            .with_orchestrator_mode();
        let mut h = EditorTestHarness::create(cols, ROWS, options).unwrap();
        h.render().unwrap();
        h.editor_mut().fire_ready_hook();
        h.wait_until(|h| h.screen_to_string().contains("+ New"))
            .unwrap();
        h
    }
}

/// The dock's width on screen: its wall is the first `│` on the top row.
fn dock_cols(h: &EditorTestHarness) -> u16 {
    let cols = h.screen_row_text(0).chars().count() as u16;
    (0..cols)
        .find(|&c| h.get_cell(c, 0).as_deref() == Some("│"))
        .map(|wall| wall + 1)
        .unwrap_or_else(|| panic!("no dock wall on the top row:\n{}", h.screen_to_string()))
}

fn resize(h: &mut EditorTestHarness, cols: u16) {
    h.resize(cols, ROWS).unwrap();
    h.render().unwrap();
}

/// With nothing dragged: 0.28 of the frame, down past the old 24-column
/// floor on a small terminal.
#[test]
fn the_dock_narrows_with_the_terminal() {
    let project = Project::new();
    let mut h = project.launch(120);
    assert_eq!(dock_cols(&h), 34, "0.28 of 120");
    resize(&mut h, 80);
    assert_eq!(
        dock_cols(&h),
        22,
        "0.28 of 80 — it used to stay at 24\n{}",
        h.screen_to_string()
    );
    resize(&mut h, 120);
    assert_eq!(dock_cols(&h), 34, "and widens back");
}

/// A dragged width is the share of the frame it was dragged to, and is
/// remembered as one.
#[test]
fn a_dragged_dock_keeps_its_share_of_the_terminal() {
    let project = Project::new();
    let mut h = project.launch(120);
    let wall = dock_cols(&h) - 1;
    h.mouse_drag(wall, 6, 59, 6).unwrap();
    h.render().unwrap();
    assert_eq!(dock_cols(&h), 60, "dragged to half of 120");

    resize(&mut h, 80);
    assert_eq!(
        dock_cols(&h),
        40,
        "half of 80 — it used to keep its 60 columns\n{}",
        h.screen_to_string()
    );

    let saved: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(project.chrome_json()).unwrap()).unwrap();
    assert_eq!(saved["dock"]["width_percent"], 50, "{saved}");
    assert!(saved["dock"].get("width").is_none(), "{saved}");
}

/// A width an older Fresh saved in columns is read as its share of the
/// terminal it is first read on.
#[test]
fn a_width_saved_in_columns_becomes_a_share() {
    let project = Project::new();
    fs::write(
        project.chrome_json(),
        r#"{ "dock": { "open": true, "width": 60 } }"#,
    )
    .unwrap();
    let mut h = project.launch(120);
    assert_eq!(dock_cols(&h), 60, "60 columns, as saved");
    resize(&mut h, 80);
    assert_eq!(dock_cols(&h), 40, "half of 80");
}
