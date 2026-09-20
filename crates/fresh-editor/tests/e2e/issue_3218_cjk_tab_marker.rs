//! Issue #3218 — a tab after CJK text made a wide character disappear.
//!
//! `tab_starts` is keyed by character index, but the cell pass probed it with
//! `col_offset`, the *visual* column. The two agree only while every character
//! on the row is one column wide. Put a double-width glyph before the tab and
//! the column runs ahead of the character index, so the row's tab-start index
//! matched some earlier CJK character, whose glyph was replaced by the `→`
//! marker: `你好\tworld` drew as `你→    world`, with `好` gone from the screen
//! while sitting intact in the buffer.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh_editor_core::primitives::display_width::str_width;
use tempfile::TempDir;

/// What a reader sees in the columns around `anchor`: the `before` columns
/// preceding it, the anchor, then the `after` columns following it.
///
/// A double-width glyph owns two terminal columns but only the first carries
/// its symbol, so the continuation cell is dropped rather than contributing a
/// stray blank. Reading painted cells — rather than the buffer — is the whole
/// point here: the characters this bug lost were intact in the file.
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
    // The defaults, restated: these are the marker settings the bug rendered
    // under, and the test should not silently follow them if they change.
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

/// The reporter's fixture, verbatim. Every CJK character survives the render,
/// and the marker sits on the tab's own first column rather than on a glyph.
#[test]
fn cjk_before_a_tab_keeps_every_character() {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("cjk_tab.txt");
    std::fs::write(&file, "你好\tworld\n测试文字\tabc\nplain\ttab\n").unwrap();

    let harness = harness_with(&file, 4);
    let screen = harness.screen_to_string();

    // 你好 is 4 columns, so the tab at column 4 expands to a full 4-column
    // stop: `→` and three padding columns. Before the fix: `你→    world`.
    assert_eq!(
        painted(&harness, "world", 8, 0),
        "你好→   world",
        "`好` must survive and the marker must sit on the tab\n{screen}"
    );
    // 测试文字 is 8 columns; the tab at column 8 expands by another 4.
    // Before the fix: `测试→字    abc`, with 文 gone and 字 stranded.
    assert_eq!(
        painted(&harness, "abc", 12, 0),
        "测试文字→   abc",
        "all four of 测试文字 must be drawn before the marker\n{screen}"
    );
    // The ASCII line was never affected: `plain` is 5 columns, so the tab
    // expands by 3 to reach the stop at column 8.
    assert_eq!(
        painted(&harness, "plain", 0, 3),
        "plain→  ",
        "ASCII tab expansion is unchanged\n{screen}"
    );
}

/// The neighbouring shapes: a tab *between* two wide characters, and a tab
/// after a mix that leaves the expansion one column wide.
#[test]
fn tabs_among_mixed_width_characters() {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("mixed.txt");
    std::fs::write(&file, "你\t好tail\na你\tZ\n").unwrap();

    let harness = harness_with(&file, 4);
    let screen = harness.screen_to_string();

    // 你 is 2 columns; the tab expands by 2 to reach the stop at column 4.
    // Before the fix the marker vanished entirely here — `tab_starts` held
    // char index 1 while the tab's column was 2 — and the row read `你  好`.
    assert_eq!(
        painted(&harness, "tail", 6, 0),
        "你→ 好tail",
        "a tab between two wide characters keeps both\n{screen}"
    );
    // `a你` is 3 columns, so the tab lands one column short of its stop and
    // expands to a single cell: the marker, with no padding behind it.
    assert_eq!(
        painted(&harness, "Z", 4, 0),
        "a你→Z",
        "a one-column expansion after a wide char is just the marker\n{screen}"
    );
}

/// The same fixture at a non-default tab size: the stops move, the characters
/// stay.
#[test]
fn cjk_before_a_tab_survives_other_tab_sizes() {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("cjk_tab8.txt");
    std::fs::write(&file, "你好\tworld\n测试文字\tabc\n").unwrap();

    let harness = harness_with(&file, 8);
    let screen = harness.screen_to_string();

    // Column 4 to the next multiple of 8: a 4-column expansion.
    assert_eq!(
        painted(&harness, "world", 8, 0),
        "你好→   world",
        "tab_size 8: `好` survives\n{screen}"
    );
    // Column 8 is already a stop, so the tab expands by a full 8.
    assert_eq!(
        painted(&harness, "abc", 16, 0),
        "测试文字→       abc",
        "tab_size 8: all of 测试文字 survives\n{screen}"
    );
}
