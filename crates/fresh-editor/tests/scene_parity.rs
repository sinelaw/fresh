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
        // Tabs live per-pane (`regions.panes[].tabs`), not at `regions.tabBar`.
        let label = scene["regions"]["panes"]
            .as_array()
            .and_then(|panes| {
                panes
                    .iter()
                    .flat_map(|p| p["tabs"].as_array().into_iter().flatten())
                    .find_map(|x| x["label"].as_str())
            })
            .map(str::to_string)
            .expect("the opened file's tab must appear in the web scene");
        // The tab label is the filename the TUI draws, not the workspace-relative
        // path (regression for the web tab showing e.g. `src/view/scene.rs`).
        assert_eq!(
            label, "scene.rs",
            "web tab label must be the bare filename, matching the TUI"
        );
        assert!(
            cells.contains(&label),
            "tab '{label}' in the web scene must also appear in the TUI cells\n{cells}"
        );
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

    // ── file-explorer parity: the sidebar tree's rows agree ──
    {
        apply_step(&mut ed, &json!({"action": "toggle_file_explorer"}));
        settle(&mut ed); // async directory scan
        let scene = scene_value(&mut ed, COLS, ROWS);
        let cells = render_tui_cells(&mut ed, COLS, ROWS);
        if let Some(rows) = scene["regions"]["fileExplorer"]["rows"].as_array() {
            let names: Vec<String> = rows
                .iter()
                .filter_map(|r| r["name"].as_str())
                .map(str::to_string)
                .collect();
            if !names.is_empty() {
                // The scene reports untruncated names; the TUI truncates each to
                // the sidebar width and which row sorts first is filesystem-
                // dependent across platforms. So assert agreement on *some* row
                // (case-insensitively) rather than the first row verbatim — the
                // strict-first-row form was flaky on macOS, where the root folder
                // ("fresh-editor") led and didn't appear literally in the cells.
                let cells_lc = cells.to_lowercase();
                assert!(
                    names.iter().any(|n| cells_lc.contains(&n.to_lowercase())),
                    "at least one file-explorer row from the scene must appear in \
                     the TUI cells; rows={names:?}\n{cells}"
                );
            }
        }
        apply_step(&mut ed, &json!({"action": "toggle_file_explorer"}));
    }

    // ── file-browser parity: the Open File dialog agrees ──
    // The web renders this popup natively from `file_browser_view`; the TUI
    // paints it as cells. Both must describe the same dialog — the same
    // directory and the same entries.
    {
        apply_step(&mut ed, &json!({"action": "open"}));
        // The directory read is async (a worker thread posts the entries back),
        // so tick with a little wall-clock until the browser stops loading
        // rather than assuming a fixed number of passes is enough.
        for _ in 0..100 {
            settle(&mut ed);
            let s = scene_value(&mut ed, COLS, ROWS);
            if s["regions"]["palette"]["browser"]["loading"] == json!(false) {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(20));
        }
        let scene = scene_value(&mut ed, COLS, ROWS);
        let cells = render_tui_cells(&mut ed, COLS, ROWS);
        let browser = &scene["regions"]["palette"]["browser"];
        assert!(
            !browser.is_null(),
            "the Open File prompt must project a file browser"
        );
        let rows = browser["rows"].as_array().expect("browser rows in scene");
        assert!(
            !rows.is_empty(),
            "the browser should list entries: {browser}"
        );
        // Entry names are truncated to the column width in the cells, so match
        // on a row the TUI had room to print in full.
        let names: Vec<String> = rows
            .iter()
            .filter_map(|r| r["name"].as_str())
            .filter(|n| n.len() < 20 && *n != "..")
            .map(str::to_string)
            .collect();
        if !names.is_empty() {
            assert!(
                names.iter().any(|n| cells.contains(n)),
                "at least one browser row from the scene must appear in the TUI \
                 cells; rows={names:?}\n{cells}"
            );
        }
        // Every interactive element carries the cell span the TUI laid it out
        // at — that is what lets the native frontend route clicks back into
        // the editor's own hit-tests.
        for key in ["toggles", "shortcuts", "columns"] {
            let items = browser[key].as_array().unwrap_or_else(|| {
                panic!("browser must project {key}");
            });
            assert!(!items.is_empty(), "browser {key} should not be empty");
            for it in items {
                assert!(
                    it["w"].as_u64().unwrap_or(0) > 0,
                    "every {key} entry needs a non-empty cell span: {it}"
                );
            }
        }
        // The status bar yields the bottom row to the prompt — it must not be
        // projected while the browser is up, or the web draws a stale one
        // under its prompt line.
        assert!(
            scene["regions"]["statusbar"].is_null(),
            "the status bar must not project while the prompt owns its row"
        );
        apply_step(&mut ed, &json!({"key": "Escape"}));
    }

    // ── settings parity: the category tree agrees ──
    {
        apply_step(&mut ed, &json!({"action": "open_settings"}));
        let scene = scene_value(&mut ed, COLS, ROWS);
        let cells = render_tui_cells(&mut ed, COLS, ROWS);
        let cats = scene["regions"]["settings"]["categories"]
            .as_array()
            .expect("settings categories in scene");
        assert!(!cats.is_empty(), "settings should report categories");
        let cat = cats[0]["name"].as_str().expect("category name");
        assert!(
            cells.contains(cat),
            "settings category '{cat}' from the scene must appear in the TUI cells\n{cells}"
        );
        apply_step(&mut ed, &json!({"key": "Escape"}));
    }
}
