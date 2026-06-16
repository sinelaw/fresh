//! Web/TUI scene parity: one `Editor`, two renderers must agree.
//!
//! The whole non-terminal-UI design rests on a single source of truth — the
//! editor's semantic model — that the web renders as HTML and the terminal
//! renders as cells. This test drives scenarios on the *same* editor the web
//! bridge uses (`webui::build_editor` + `apply_step`) and asserts that the
//! chrome the web scene reports (`scene_value`) also appears in the TUI's cell
//! rendering (`render_tui_cells`). If the two renderers ever diverged on what
//! the chrome *is*, this fails.

use fresh::webui::{apply_step, build_editor, render_tui_cells, scene_value};
use serde_json::{json, Value};
use std::path::PathBuf;

const COLS: u16 = 140;
const ROWS: u16 = 44;

fn manifest_file(rel: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(rel)
}

/// Drain async work (plugin load, menu build, dir scans) by ticking.
fn settle(ed: &mut fresh::app::Editor) {
    for _ in 0..12 {
        apply_step(ed, &json!({}));
    }
}

fn first_item_label(menu: &Value) -> Option<String> {
    menu["items"].as_array()?.iter().find_map(|it| {
        if it.get("kind").and_then(|k| k.as_str()) == Some("action") {
            it.get("label").and_then(|l| l.as_str()).map(str::to_string)
        } else {
            None
        }
    })
}

#[test]
fn web_scene_and_tui_cells_agree() {
    let mut ed =
        build_editor(COLS, ROWS, &[manifest_file("src/view/scene.rs")]).expect("build editor");
    settle(&mut ed);

    // ── tab parity: the opened file's tab label is in the scene AND the cells ──
    {
        let scene = scene_value(&mut ed, COLS, ROWS);
        let cells = render_tui_cells(&mut ed, COLS, ROWS);
        let tabs = scene["regions"]["tabBar"]["tabs"].as_array();
        let label = tabs
            .and_then(|t| t.iter().find_map(|x| x["label"].as_str()))
            .map(str::to_string);
        if let Some(label) = label {
            assert!(
                cells.contains(&label),
                "tab '{label}' in the web scene must also appear in the TUI cells\n{cells}"
            );
        }
    }

    // ── status-bar parity: a stable segment (language for a .rs file) agrees ──
    {
        let scene = scene_value(&mut ed, COLS, ROWS);
        let cells = render_tui_cells(&mut ed, COLS, ROWS);
        let segs = scene["regions"]["statusbar"]["segments"]
            .as_array()
            .expect("status segments in scene");
        // The language segment ("Rust") is deterministic for scene.rs and shows
        // in both renderers.
        let has_rust_seg = segs.iter().any(|s| s["text"].as_str() == Some("Rust"));
        assert!(
            has_rust_seg,
            "scene status bar should report the Rust language segment"
        );
        assert!(
            cells.contains("Rust"),
            "the status language the scene reports must also be in the TUI cells\n{cells}"
        );
    }

    // ── menu parity: open the File menu; bar label + a dropdown item agree ──
    {
        apply_step(&mut ed, &json!({"key": "f", "alt": true}));
        let scene = scene_value(&mut ed, COLS, ROWS);
        let cells = render_tui_cells(&mut ed, COLS, ROWS);

        let menus = scene["regions"]["menus"]
            .as_array()
            .expect("menus in scene");
        let file = menus
            .iter()
            .find(|m| m["label"].as_str() == Some("File"))
            .expect("File menu in the scene");
        assert!(
            cells.contains("File"),
            "menu bar 'File' must be in the TUI cells"
        );

        assert!(
            !scene["regions"]["menuOpen"].is_null(),
            "the scene must report a menu open after Alt+F"
        );
        let item = first_item_label(file).expect("File menu has an action item");
        assert!(
            cells.contains(&item),
            "open-dropdown item '{item}' from the scene must appear in the TUI cells\n{cells}"
        );

        apply_step(&mut ed, &json!({"key": "Escape"}));
    }
}
