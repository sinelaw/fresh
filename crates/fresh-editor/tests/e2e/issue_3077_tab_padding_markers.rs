//! Issue #3077 — a tab's padding columns are not spaces, so they carry no
//! space marker.
//!
//! With `whitespace_spaces_leading` on, every column of an expanded tab used
//! to draw the space dot: one tab rendered `→···` — three dots for columns
//! holding no spaces — and a tab followed by four real spaces rendered
//! `→·······`, which made the tab/space mix the markers exist to expose
//! unreadable.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// The indent cells of the row carrying `needle`, left to right.
fn indent_cells(harness: &EditorTestHarness, needle: &str, width: u16) -> Vec<String> {
    let (col, row) = harness
        .find_text_on_screen(needle)
        .unwrap_or_else(|| panic!("expected `{needle}` on screen"));
    let start = col.saturating_sub(width);
    (start..col)
        .map(|x| harness.get_cell(x, row).unwrap_or_default())
        .collect()
}

fn harness_with_markers(file: &std::path::Path) -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.tab_size = 4;
    config.editor.whitespace_show = true;
    config.editor.whitespace_tabs_leading = true;
    config.editor.whitespace_spaces_leading = true;
    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(file).unwrap();
    harness.render().unwrap();
    harness
}

#[test]
fn tab_padding_columns_carry_no_space_marker() {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("ws.py");
    // One tab; eight real spaces; one tab then four real spaces.
    std::fs::write(&file, "def f():\n\tx = 1\n        y = 2\n\t    z = 3\n").unwrap();

    let harness = harness_with_markers(&file);

    assert_eq!(
        indent_cells(&harness, "x = 1", 4),
        vec!["→", " ", " ", " "],
        "a lone tab is one arrow and three blank padding columns\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        indent_cells(&harness, "y = 2", 8),
        vec!["·"; 8],
        "real spaces still get one dot each\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        indent_cells(&harness, "z = 3", 8),
        vec!["→", " ", " ", " ", "·", "·", "·", "·"],
        "a tab followed by four spaces reads as one arrow and four dots\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn tab_padding_stays_blank_inside_a_selection() {
    // `whitespace_in_selection` draws indicators over a selection regardless
    // of the per-position settings (issue #2797). That path dotted the
    // padding too, so a block-selected tab indent read as `→···` even with
    // every space setting off.
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("sel.txt");
    std::fs::write(&file, "\tabcdefgh\n").unwrap();

    let mut config = Config::default();
    config.editor.tab_size = 4;
    config.editor.whitespace_show = true;
    config.editor.whitespace_tabs_leading = true;
    config.editor.whitespace_spaces_leading = false;
    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    // Select the whole line: the indent is inside the selection.
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        indent_cells(&harness, "abcdefgh", 4),
        vec!["→", " ", " ", " "],
        "selected tab padding keeps its blank columns\n{}",
        harness.screen_to_string()
    );
}
