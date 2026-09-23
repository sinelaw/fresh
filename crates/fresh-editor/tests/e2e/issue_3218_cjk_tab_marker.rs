//! Issue #3218: a tab after CJK text hid the character before it.
//! `你好\tworld` drew as `你→    world`.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh_editor_core::primitives::display_width::str_width;
use tempfile::TempDir;

/// The painted text around `anchor`: `before` columns, the anchor, `after`
/// columns. A wide glyph's second cell carries no symbol, so it is skipped.
fn painted(harness: &EditorTestHarness, anchor: &str, before: u16, after: u16) -> String {
    let (col, row) = harness
        .find_text_on_screen(anchor)
        .unwrap_or_else(|| panic!("expected `{anchor}` on screen"));
    let start = col.saturating_sub(before);
    let end = col + str_width(anchor) as u16 + after;
    let mut out = String::new();
    let mut skip = 0usize;
    for x in start..end {
        let sym = harness.get_cell(x, row).unwrap_or_default();
        if skip > 0 {
            skip -= 1;
            continue;
        }
        skip = str_width(&sym).saturating_sub(1);
        out.push_str(&sym);
    }
    out
}

fn harness_with(file: &std::path::Path, tab_size: usize) -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.tab_size = tab_size;
    // These are the defaults, set here so the test does not follow them if
    // they change.
    config.editor.whitespace_show = true;
    config.editor.whitespace_tabs_leading = true;
    config.editor.whitespace_tabs_inner = true;
    config.editor.whitespace_tabs_trailing = true;
    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(file).unwrap();
    harness.render().unwrap();
    harness
}

/// The reporter's fixture, unchanged.
#[test]
fn cjk_before_a_tab_keeps_every_character() {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("cjk_tab.txt");
    std::fs::write(&file, "你好\tworld\n测试文字\tabc\nplain\ttab\n").unwrap();

    let harness = harness_with(&file, 4);
    let screen = harness.screen_to_string();

    assert_eq!(
        painted(&harness, "world", 8, 0),
        "你好→   world",
        "`好` must survive and the marker must sit on the tab\n{screen}"
    );
    assert_eq!(
        painted(&harness, "abc", 12, 0),
        "测试文字→   abc",
        "all four of 测试文字 must be drawn before the marker\n{screen}"
    );
    assert_eq!(
        painted(&harness, "plain", 0, 3),
        "plain→  ",
        "ASCII tab expansion is unchanged\n{screen}"
    );
}

/// A tab between two wide characters, and a tab that expands to one column.
#[test]
fn tabs_among_mixed_width_characters() {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("mixed.txt");
    std::fs::write(&file, "你\t好tail\na你\tZ\n").unwrap();

    let harness = harness_with(&file, 4);
    let screen = harness.screen_to_string();

    // Before the fix the marker was dropped here and the row read `你  好`.
    assert_eq!(
        painted(&harness, "tail", 6, 0),
        "你→ 好tail",
        "a tab between two wide characters keeps both\n{screen}"
    );
    assert_eq!(
        painted(&harness, "Z", 4, 0),
        "a你→Z",
        "a one-column expansion after a wide char is just the marker\n{screen}"
    );
}

/// The same fixture at a different tab size.
#[test]
fn cjk_before_a_tab_survives_other_tab_sizes() {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("cjk_tab8.txt");
    std::fs::write(&file, "你好\tworld\n测试文字\tabc\n").unwrap();

    let harness = harness_with(&file, 8);
    let screen = harness.screen_to_string();

    assert_eq!(
        painted(&harness, "world", 8, 0),
        "你好→   world",
        "tab_size 8: `好` survives\n{screen}"
    );
    assert_eq!(
        painted(&harness, "abc", 16, 0),
        "测试文字→       abc",
        "tab_size 8: all of 测试文字 survives\n{screen}"
    );
}
