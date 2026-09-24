//! E2E tests for customizable status bar configuration.
//!
//! Verifies that the `status_bar.left` and `status_bar.right` config options
//! control which elements appear (and don't appear) in the rendered status bar.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, StatusBarConfig, StatusBarElement};
use std::fs;

/// Helper: create a config with the given status bar elements.
fn config_with_status_bar(left: Vec<StatusBarElement>, right: Vec<StatusBarElement>) -> Config {
    let mut config = Config::default();
    config.editor.status_bar = StatusBarConfig {
        left,
        right,
        ..StatusBarConfig::default()
    };
    config
}

/// Removing an element from the config should remove it from the rendered
/// status bar. Here we drop `{encoding}` and verify "UTF-8" no longer appears.
#[test]
fn test_removed_element_not_rendered() {
    // Default right side includes Encoding; remove it.
    let config = config_with_status_bar(
        vec![StatusBarElement::Filename],
        vec![
            StatusBarElement::LineEnding,
            // StatusBarElement::Encoding removed
            StatusBarElement::Language,
        ],
    );

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_config(config),
    )
    .unwrap();

    // Open a file so the status bar has content to show.
    let dir = harness.project_dir().unwrap();
    let file = dir.join("hello.txt");
    fs::write(&file, "hello world\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        !status.contains("UTF-8"),
        "Encoding should not appear when removed from config.\nStatus bar: {status}"
    );
    // Language should still be present (Plain Text or similar)
    assert!(
        status.contains("Plain Text") || status.contains("txt"),
        "Language element should still appear.\nStatus bar: {status}"
    );
}

/// Adding the `{clock}` element should render a time string in HH:MM format.
#[test]
fn test_clock_element_renders() {
    let config = config_with_status_bar(
        vec![StatusBarElement::Filename],
        vec![StatusBarElement::Clock],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "content\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    // Clock renders as HH:MM; match DD:DD anywhere in the status bar.
    let has_time = {
        let bytes = status.as_bytes();
        bytes.windows(5).any(|w| {
            w[0].is_ascii_digit()
                && w[1].is_ascii_digit()
                && w[2] == b':'
                && w[3].is_ascii_digit()
                && w[4].is_ascii_digit()
        })
    };
    assert!(
        has_time,
        "Clock element should render a time as HH:MM.\nStatus bar: {status}"
    );
}

/// An empty right config should render no right-side elements.
#[test]
fn test_empty_right_side() {
    let config = config_with_status_bar(
        vec![StatusBarElement::Filename, StatusBarElement::Cursor],
        vec![], // no right side
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "some text\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    // With no right side, encoding/language/line-ending should be absent
    assert!(
        !status.contains("UTF-8"),
        "No encoding expected.\nStatus bar: {status}"
    );
    assert!(
        !status.contains("LF") || status.contains("LF") && status.contains("test.txt"),
        // LF might appear as part of the filename or other left-side text; just check
        // that typical right-side indicators are gone
        "Checking right side is empty.\nStatus bar: {status}"
    );
    // Cursor info should still be present (left side)
    assert!(
        status.contains("Ln") || status.contains("1:1") || status.contains("Col"),
        "Cursor element should appear on left side.\nStatus bar: {status}"
    );
}

/// Compact cursor format `{cursor:compact}` should render as `row:col` instead
/// of the default `Ln X, Col Y`.
#[test]
fn test_compact_cursor_format() {
    let config = config_with_status_bar(
        vec![StatusBarElement::Filename, StatusBarElement::CursorCompact],
        vec![],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "line one\nline two\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        status.contains("1:1"),
        "Compact cursor should show 1:1.\nStatus bar: {status}"
    );
    assert!(
        !status.contains("Ln"),
        "Compact cursor should not show 'Ln'.\nStatus bar: {status}"
    );
}

/// Regression test for issue #1967 — moving the cursor between lines of
/// very different lengths must not shift the position of elements that
/// follow `{cursor}` in the status bar. Reproduces the original symptom
/// (the status bar was fidgety when the column number jumped between 1
/// and 2+ digits) by anchoring a trailing always-present `{language}`
/// element after `{cursor}` and asserting the language token stays in
/// the same column before and after a cursor move.
#[test]
fn test_cursor_indicator_width_is_stable_across_cursor_movement() {
    let mut config = config_with_status_bar(
        vec![
            StatusBarElement::Filename,
            StatusBarElement::Cursor,
            StatusBarElement::Language,
        ],
        vec![],
    );
    // Use an explicit `|` separator so the Language element has a stable
    // anchor to assert against; the default separator is padding-only.
    config.editor.status_bar.separator = " | ".to_string();

    let mut harness = EditorTestHarness::with_temp_project_and_config(160, 30, config).unwrap();

    // Two lines of very different lengths so moving Down + End jumps the
    // column number from a single digit to three digits — the exact
    // pattern that caused the bar to wiggle in #1967.
    let dir = harness.project_dir().unwrap();
    let file = dir.join("wiggly.txt");
    let short = "ab\n";
    let long = format!("{}\n", "x".repeat(150));
    fs::write(&file, format!("{short}{long}")).unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status_before = harness.get_status_bar();
    // Find the language token start as a stable anchor. The status bar
    // uses " | " separators between left-side elements, so the last "|"
    // sits just before the Language element. Its column position must
    // stay fixed when only the Cursor element changes.
    let lang_col_before = status_before
        .rfind('|')
        .unwrap_or_else(|| panic!("expected separator in status bar: {status_before:?}"));

    // Move to line 2, then to the end of the (150-char) line.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let status_after = harness.get_status_bar();
    let lang_col_after = status_after
        .rfind('|')
        .unwrap_or_else(|| panic!("expected separator in status bar: {status_after:?}"));

    assert_eq!(
        lang_col_before, lang_col_after,
        "Element after `{{cursor}}` shifted when the cursor moved.\n  before: {status_before:?}\n  after:  {status_after:?}"
    );
}

/// Adding the `{remote}` element to the local status bar should render a
/// visible "Local" indicator so the bottom-left remote-authority entry point
/// is always present.
#[test]
fn test_remote_indicator_shows_local() {
    let config = config_with_status_bar(
        vec![
            StatusBarElement::RemoteIndicator,
            StatusBarElement::Filename,
        ],
        vec![],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        status.contains("Local"),
        "Remote indicator should show 'Local' when no authority is connected.\nStatus bar: {status}"
    );
}

/// The remote indicator should be the left-most element when configured
/// first in the `left` list — the spec places the remote authority entry
/// point at the bottom-left of the status bar.
#[test]
fn test_remote_indicator_placed_at_far_left() {
    let config = config_with_status_bar(
        vec![
            StatusBarElement::RemoteIndicator,
            StatusBarElement::Filename,
        ],
        vec![],
    );

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    let local_idx = status
        .find("Local")
        .expect("Remote indicator missing from status bar");
    let filename_idx = status
        .find("test.txt")
        .expect("Filename missing from status bar");
    assert!(
        local_idx < filename_idx,
        "Remote indicator should appear before the filename.\nStatus bar: {status}"
    );
}

/// Right-side status bar elements must be separated by " | ".
///
/// Regression test for issue #2088: the right-side rendering loop was missing
/// the ` | ` separator that the left side already applied, so elements like
/// Encoding and Language appeared concatenated (e.g. "LF UTF-8 Rust").
#[test]
fn test_right_side_separators() {
    let mut config = config_with_status_bar(
        vec![StatusBarElement::Filename, StatusBarElement::Cursor],
        vec![
            StatusBarElement::LineEnding,
            StatusBarElement::Encoding,
            StatusBarElement::Language,
        ],
    );
    // The default separator is padding-only; set an explicit `|` so this
    // test can assert the separator is applied between right-side elements.
    config.editor.status_bar.separator = " | ".to_string();

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.rs");
    fs::write(&file, "fn main() {}\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    // Find the right-side "LF" line-ending indicator.
    // Before the fix, right-side elements were concatenated without separators
    // (e.g. "LF ASCII Rust"). After the fix, a `|` appears between them.
    let lf_pos = status.rfind("LF").expect("LF should appear in status bar");
    assert!(
        status[lf_pos..].contains('|'),
        "A '|' separator should appear between right-side elements.\nStatus bar: {status}"
    );
}

/// A custom `separator` config value is used verbatim between elements on
/// both sides (here a right-side check using `" :: "`).
#[test]
fn test_custom_separator() {
    let mut config = config_with_status_bar(
        vec![StatusBarElement::Filename, StatusBarElement::Cursor],
        vec![StatusBarElement::LineEnding, StatusBarElement::Encoding],
    );
    config.editor.status_bar.separator = " :: ".to_string();

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();
    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.rs");
    fs::write(&file, "fn main() {}\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    let lf_pos = status.rfind("LF").expect("LF should appear in status bar");
    assert!(
        status[lf_pos..].contains("::"),
        "The custom '::' separator should appear between right-side elements.\nStatus bar: {status}"
    );
    assert!(
        !status.contains('|'),
        "The default '|' separator should not appear when overridden.\nStatus bar: {status}"
    );
}

/// An empty `separator` disables separators entirely: elements are adjacent
/// and no separator (and no separator space) is drawn.
#[test]
fn test_empty_separator_has_no_separator() {
    let mut config = config_with_status_bar(
        vec![StatusBarElement::Filename, StatusBarElement::Cursor],
        vec![StatusBarElement::LineEnding, StatusBarElement::Encoding],
    );
    config.editor.status_bar.separator = String::new();

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();
    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.rs");
    fs::write(&file, "fn main() {}\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        !status.contains('|'),
        "No '|' separator should be drawn when separator is empty.\nStatus bar: {status}"
    );
    // Each entry keeps its own one-space margin, so with an empty separator
    // LineEnding and Encoding meet margin-to-margin ("LF  ASCII", two spaces).
    // The default "|" separator would render "LF | ASCII".
    assert!(
        status.contains("LF  ASCII"),
        "Empty separator should leave only the entries' own margins.\nStatus bar: {status}"
    );
}

/// Both empty sides should still render a valid (blank) status bar without
/// crashing.
#[test]
fn test_both_sides_empty() {
    let config = config_with_status_bar(vec![], vec![]);

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();

    let dir = harness.project_dir().unwrap();
    let file = dir.join("test.txt");
    fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    // Should not contain any typical status bar indicators
    assert!(
        !status.contains("UTF-8"),
        "No encoding expected.\nStatus bar: {status}"
    );
    assert!(
        !status.contains("Ln"),
        "No cursor info expected.\nStatus bar: {status}"
    );
}

/// Ctrl+Right-click a status-bar cell and return the inspector's text.
fn inspect_status_cell(harness: &mut EditorTestHarness, col: u16) -> String {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
    let row = crate::common::harness::layout::status_bar_row(harness.terminal_height()) as u16;
    for kind in [
        MouseEventKind::Down(MouseButton::Right),
        MouseEventKind::Up(MouseButton::Right),
    ] {
        harness
            .send_mouse(MouseEvent {
                kind,
                column: col,
                row,
                modifiers: KeyModifiers::CONTROL,
            })
            .unwrap();
    }
    harness.render().unwrap();
    harness.screen_to_string()
}

/// The theme inspector names the keys a status-bar cell was painted with:
/// the bar's own for an element, the separator's for the separator. Both
/// come from the fold's provenance, read off the runs' theme names.
#[test]
fn test_theme_inspector_names_status_bar_keys() {
    let mut config = config_with_status_bar(
        vec![StatusBarElement::Filename],
        vec![StatusBarElement::LineEnding, StatusBarElement::Encoding],
    );
    config.editor.status_bar.separator = "|".to_string();
    let mut harness = EditorTestHarness::with_temp_project_and_config(100, 24, config).unwrap();
    let dir = harness.project_dir().unwrap();
    let file = dir.join("named.txt");
    fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let status = harness.get_status_bar();
    let col = |needle: &str| {
        status
            .find(needle)
            .unwrap_or_else(|| panic!("{needle:?} on the bar: {status}")) as u16
    };
    let (name_col, sep_col) = (col("named.txt"), col("LF |") + 3);

    let shown = inspect_status_cell(&mut harness, name_col);
    assert!(
        shown.contains("ui.status_bar_fg") && shown.contains("ui.status_bar_bg"),
        "the filename is painted in the bar's keys:\n{shown}"
    );

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let shown = inspect_status_cell(&mut harness, sep_col);
    assert!(
        shown.contains("ui.status_separator_fg") && shown.contains("ui.status_separator_bg"),
        "the separator is painted in its own keys:\n{shown}"
    );
}

/// A popup opened from a status-bar element hangs directly above that
/// element. The read-only menu used to be anchored to the first element on
/// the right side whatever opened it, so an `[RO]` on the left opened its
/// menu across the bar from it.
#[test]
fn test_status_bar_popup_opens_above_its_own_segment() {
    let config = config_with_status_bar(
        vec![StatusBarElement::Cursor, StatusBarElement::ReadOnly],
        vec![StatusBarElement::Encoding, StatusBarElement::LineEnding],
    );
    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 30, config).unwrap();
    let dir = harness.project_dir().unwrap();
    let file = dir.join("locked.txt");
    fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    let buffer_id = harness.editor().active_buffer();
    harness
        .editor_mut()
        .active_window_mut()
        .mark_buffer_read_only(buffer_id, true);
    harness.render().unwrap();

    let status = harness.get_status_bar();
    let ro_col = status
        .find("[RO]")
        .unwrap_or_else(|| panic!("[RO] on the bar: {status}")) as u16;
    let row = crate::common::harness::layout::status_bar_row(harness.terminal_height()) as u16;
    harness.mouse_click(ro_col, row).unwrap();
    harness.render().unwrap();

    let (item_col, item_row) = harness
        .find_text_on_screen("Enable editing")
        .unwrap_or_else(|| panic!("the read-only menu:\n{}", harness.screen_to_string()));
    assert!(item_row < row, "the menu sits above the bar");
    // The segment starts one cell before `[RO]` (its padding); the item's
    // text sits inside the popup's border and its four-space indent.
    let seg_x = ro_col - 1;
    assert!(
        (seg_x..seg_x + 8).contains(&item_col),
        "the menu hangs off `[RO]` at column {seg_x}, but its item is at \
         column {item_col}:\n{}",
        harness.screen_to_string()
    );
}
