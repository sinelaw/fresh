//! Issue #3079 — an indentation guide no longer swallows the tab marker.
//!
//! With `indentation_guide: "all"` every `→` was replaced by `▏`, at every
//! indent level, so while guides were on there was no way to tell tab
//! indentation from space indentation. The two share the expansion now: the
//! guide keeps the tab stop and the marker moves one column right.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::{Config, IndentationGuideMode};
use tempfile::TempDir;

/// The `width` cells to the left of `needle` on its row.
fn indent_cells(harness: &EditorTestHarness, needle: &str, width: u16) -> Vec<String> {
    let (col, row) = harness
        .find_text_on_screen(needle)
        .unwrap_or_else(|| panic!("expected `{needle}` on screen"));
    let start = col.saturating_sub(width);
    (start..col)
        .map(|x| harness.get_cell(x, row).unwrap_or_default())
        .collect()
}

fn harness_for(file: &std::path::Path, guide: IndentationGuideMode) -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.tab_size = 4;
    config.editor.whitespace_show = true;
    config.editor.whitespace_tabs_leading = true;
    config.editor.indentation_guide = guide;
    config.editor.indentation_guide_glyph = "▏".to_string();
    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(file).unwrap();
    harness.render().unwrap();
    harness
}

fn tab_file(dir: &TempDir) -> std::path::PathBuf {
    let file = dir.path().join("tabs.py");
    std::fs::write(&file, "def f():\n\tif x:\n\t\tif y:\n\t\t\tz = 3\n").unwrap();
    file
}

#[test]
fn guides_and_tab_markers_share_the_expansion() {
    let temp_dir = TempDir::new().unwrap();
    let file = tab_file(&temp_dir);
    let harness = harness_for(&file, IndentationGuideMode::All);

    assert_eq!(
        indent_cells(&harness, "if x:", 4),
        vec!["▏", "→", " ", " "],
        "the guide keeps the tab stop and the marker takes the next column\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        indent_cells(&harness, "z = 3", 12),
        vec!["▏", "→", " ", " ", "▏", "→", " ", " ", "▏", "→", " ", " "],
        "every indent level shows both its guide and its tab\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn tab_markers_are_unmoved_without_guides() {
    let temp_dir = TempDir::new().unwrap();
    let file = tab_file(&temp_dir);
    let harness = harness_for(&file, IndentationGuideMode::None);

    assert_eq!(
        indent_cells(&harness, "if x:", 4),
        vec!["→", " ", " ", " "],
        "with guides off the marker keeps the tab's first column\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn space_indentation_is_distinguishable_from_tab_indentation() {
    // The reporter's actual use case: spotting mixed indentation while guides
    // are on. A tab line and a space line must not render identically.
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("mixed.py");
    std::fs::write(&file, "def f():\n\tx = 1\n    y = 2\n").unwrap();

    let mut config = Config::default();
    config.editor.tab_size = 4;
    config.editor.whitespace_show = true;
    config.editor.whitespace_tabs_leading = true;
    config.editor.whitespace_spaces_leading = true;
    config.editor.indentation_guide = IndentationGuideMode::All;
    config.editor.indentation_guide_glyph = "▏".to_string();
    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let tabbed = indent_cells(&harness, "x = 1", 4);
    let spaced = indent_cells(&harness, "y = 2", 4);
    assert_eq!(tabbed, vec!["▏", "→", " ", " "]);
    assert_eq!(spaced, vec!["▏", "·", "·", "·"]);
    assert_ne!(
        tabbed,
        spaced,
        "tab and space indentation must not render alike\n{}",
        harness.screen_to_string()
    );
}
