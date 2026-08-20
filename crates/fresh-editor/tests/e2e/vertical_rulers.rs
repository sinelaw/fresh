//! E2E tests for vertical rulers feature.
//!
//! Tests: rendering at correct columns, per-buffer/per-view independence,
//! horizontal scroll behavior, and command palette add/remove.
//!
//! Rulers are rendered as a background color tint (not a character), so tests
//! check `get_cell_style().bg` rather than `get_cell()` for character content.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use ratatui::style::Color;
use tempfile::TempDir;

/// Helper: query the rendered gutter width from the active buffer's margin state.
/// For a <=99-line buffer this is 1 (indicator) + 2 (digits) + 3 (" │ ") = 6.
fn gutter_width(harness: &EditorTestHarness) -> u16 {
    harness.editor().active_state().margins.left_total_width() as u16
}

/// The default ruler background color: Rgb(50, 50, 50)
const RULER_BG: Color = Color::Rgb(50, 50, 50);

/// Helper to check if a cell has the ruler background color.
fn has_ruler_bg(harness: &EditorTestHarness, x: u16, y: u16) -> bool {
    harness
        .get_cell_style(x, y)
        .map(|s| s.bg == Some(RULER_BG))
        .unwrap_or(false)
}

/// Screen x of the cell a ruler at 1-based `column` must mark: the cell that
/// holds the `column`-th character of the line, i.e. `column - 1` cells after
/// the gutter.
fn ruler_x(harness: &EditorTestHarness, column: u16) -> u16 {
    gutter_width(harness) + column - 1
}

/// All screen x positions on `row` that carry the ruler background.
fn ruler_cells_in_row(harness: &EditorTestHarness, row: u16, width: u16) -> Vec<u16> {
    (0..width)
        .filter(|&x| has_ruler_bg(harness, x, row))
        .collect()
}

/// A line whose every 10th character is a letter marker:
/// `123456789A123456789B…` — so the `n`-th group's letter sits at column
/// `n * 10`, letting a test name the character a ruler is expected to mark.
fn marker_line(columns: usize) -> String {
    (1..=columns)
        .map(|col| {
            if col % 10 == 0 {
                // 10 -> 'A', 20 -> 'B', ...
                char::from(b'A' + ((col / 10 - 1) % 26) as u8)
            } else {
                char::from(b'0' + (col % 10) as u8)
            }
        })
        .collect()
}

/// Helper to run a command from the command palette.
fn run_command(harness: &mut EditorTestHarness, command_name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(command_name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test that rulers render background color at the correct column positions.
#[test]
fn test_rulers_render_at_correct_columns() {
    let mut config = Config::default();
    config.editor.rulers = vec![10, 20];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Ruler at column 10 should have ruler bg
    assert!(
        has_ruler_bg(&harness, ruler_x(&harness, 10), row),
        "Ruler bg should appear at column 10"
    );

    // Ruler at column 20 should have ruler bg
    assert!(
        has_ruler_bg(&harness, ruler_x(&harness, 20), row),
        "Ruler bg should appear at column 20"
    );

    // Column 15 should NOT have ruler bg
    assert!(
        !has_ruler_bg(&harness, ruler_x(&harness, 15), row),
        "Column 15 should not have ruler bg"
    );

    // Rulers should preserve text content (not overwrite with │)
    let cell_10 = harness.get_cell(ruler_x(&harness, 10), row);
    assert_eq!(
        cell_10.as_deref(),
        Some("X"),
        "Ruler should preserve existing text content"
    );
}

/// Test that rulers span the full content height (all visible rows).
#[test]
fn test_rulers_span_full_height() {
    let mut config = Config::default();
    config.editor.rulers = vec![10];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = "Hello World\n".repeat(30);
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let (content_first_row, content_last_row) = harness.content_area_rows();
    let ruler_screen_x = ruler_x(&harness, 10);

    for row in content_first_row..=content_last_row {
        assert!(
            has_ruler_bg(&harness, ruler_screen_x, row as u16),
            "Ruler bg should appear on row {row}"
        );
    }
}

/// Regression (#2631): the ruler must span the full editor height even when the
/// buffer has fewer lines than the viewport. Previously it stopped at the last
/// written line, leaving the empty area below without the guide — so a buffer
/// with a single short line showed the ruler on just one row.
#[test]
fn test_rulers_span_full_height_short_buffer() {
    let mut config = Config::default();
    config.editor.rulers = vec![10];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    // Only three lines of text in a 24-row terminal: most of the pane is empty.
    let _fixture = harness
        .load_buffer_from_text("line one\nline two\nline three")
        .unwrap();
    harness.render().unwrap();

    let (content_first_row, content_last_row) = harness.content_area_rows();
    let ruler_screen_x = ruler_x(&harness, 10);

    // The ruler must appear on every content row, including the empty rows
    // below the last line of text.
    for row in content_first_row..=content_last_row {
        assert!(
            has_ruler_bg(&harness, ruler_screen_x, row as u16),
            "Ruler bg should appear on row {row} (short buffer, {content_first_row}..={content_last_row})"
        );
    }
}

/// Test that rulers scroll horizontally with content.
#[test]
fn test_rulers_horizontal_scroll() {
    let mut config = Config::default();
    // Place a single ruler at column 5, disable line wrap to enable horizontal scroll
    config.editor.rulers = vec![5];
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(200)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Initially ruler at column 5 should be visible at screen x = gutter + 5
    let ruler_screen_x = ruler_x(&harness, 5);
    assert!(
        has_ruler_bg(&harness, ruler_screen_x, row),
        "Ruler at col 5 should be visible initially"
    );

    // Move cursor far right so viewport scrolls past column 5
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // After viewport scroll, column 5 is off-screen so gutter+5 should NOT
    // have ruler bg. (There's no ruler at whatever column gutter+5 now maps to.)
    assert!(
        !has_ruler_bg(&harness, ruler_screen_x, row),
        "Ruler at col 5 should have scrolled off-screen"
    );
}

/// Test no rulers when config is empty (default).
#[test]
fn test_no_rulers_by_default() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    for col_offset in [10u16, 20, 30, 40] {
        assert!(
            !has_ruler_bg(&harness, gutter_width(&harness) + col_offset, row),
            "No ruler should exist at column {col_offset} with default config"
        );
    }
}

/// Regression: virtual buffers (Dashboard, *Diagnostics*, grep results, ...)
/// must not paint the config-driven column rulers. They aren't source code,
/// and the ruler stripes would otherwise overlay plugin chrome.
#[test]
fn test_no_rulers_on_virtual_buffer() {
    let mut config = Config::default();
    config.editor.rulers = vec![10, 20];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    let dashboard_buffer = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("Dashboard".to_string(), "dashboard".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(
            dashboard_buffer,
            vec![fresh::primitives::text_property::TextPropertyEntry::text(
                &"X".repeat(60),
            )],
        )
        .unwrap();
    harness.editor_mut().switch_buffer(dashboard_buffer);
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    assert!(
        !has_ruler_bg(&harness, ruler_x(&harness, 10), row),
        "Virtual buffer should not paint a ruler at column 10"
    );
    assert!(
        !has_ruler_bg(&harness, ruler_x(&harness, 20), row),
        "Virtual buffer should not paint a ruler at column 20"
    );
}

/// Test that ruler uses the theme's ruler_bg color.
#[test]
fn test_ruler_uses_theme_color() {
    let mut config = Config::default();
    config.editor.rulers = vec![10];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let ruler_screen_x = ruler_x(&harness, 10);

    let style = harness.get_cell_style(ruler_screen_x, content_first_row as u16);
    assert!(style.is_some(), "Ruler cell should have a style");

    let style = style.unwrap();
    assert_eq!(
        style.bg,
        Some(RULER_BG),
        "Ruler cell should have the ruler_bg background color"
    );
}

/// Test per-buffer ruler independence: buffers opened with config rulers
/// should each independently have rulers.
#[test]
fn test_per_buffer_ruler_independence() {
    let mut config = Config::default();
    config.editor.rulers = vec![15];

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();

    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    std::fs::write(&file1, "A".repeat(60)).unwrap();
    std::fs::write(&file2, "B".repeat(60)).unwrap();

    // Open first file
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    let ruler_screen_x = ruler_x(&harness, 15);

    assert!(
        has_ruler_bg(&harness, ruler_screen_x, row),
        "File1 should have a ruler at column 15"
    );

    // Open second file - should also get rulers from config
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    assert!(
        has_ruler_bg(&harness, ruler_screen_x, row),
        "File2 should also have rulers initialized from config"
    );

    // Switch back to file1 - rulers should still be there
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        has_ruler_bg(&harness, ruler_screen_x, row),
        "File1 should still have ruler after switching back"
    );
}

/// Test adding a ruler via command palette.
#[test]
fn test_add_ruler_command() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    let ruler_screen_x = ruler_x(&harness, 25);

    // Before: no ruler at column 25
    assert!(
        !has_ruler_bg(&harness, ruler_screen_x, row),
        "No ruler should exist at column 25 initially"
    );

    // Add ruler at column 25 via command palette
    run_command(&mut harness, "Add Ruler");

    // Now at the "Add ruler at column:" prompt — type the column number
    harness.type_text("25").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify ruler now exists at column 25
    assert!(
        has_ruler_bg(&harness, ruler_screen_x, row),
        "Ruler should appear at column 25 after Add Ruler command"
    );
}

/// Test removing a ruler via command palette.
#[test]
fn test_remove_ruler_command() {
    let mut config = Config::default();
    config.editor.rulers = vec![10, 20];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    let ruler_x_10 = ruler_x(&harness, 10);
    let ruler_x_20 = ruler_x(&harness, 20);

    // Verify both rulers exist
    assert!(
        has_ruler_bg(&harness, ruler_x_10, row),
        "Ruler at column 10 should exist before removal"
    );
    assert!(
        has_ruler_bg(&harness, ruler_x_20, row),
        "Ruler at column 20 should exist before removal"
    );

    // Remove ruler at column 10 via command palette
    run_command(&mut harness, "Remove Ruler");

    // The prompt shows current rulers as suggestions.
    // The first suggestion should be "Column 10", press Enter to select it.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify ruler at column 10 is gone
    assert!(
        !has_ruler_bg(&harness, ruler_x_10, row),
        "Ruler at column 10 should be removed"
    );

    // Verify ruler at column 20 still exists
    assert!(
        has_ruler_bg(&harness, ruler_x_20, row),
        "Ruler at column 20 should still exist after removing column 10"
    );
}

/// Test removing a specific ruler by navigating the suggestion list.
#[test]
fn test_remove_ruler_selects_specific() {
    let mut config = Config::default();
    config.editor.rulers = vec![10, 20];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    let ruler_x_10 = ruler_x(&harness, 10);
    let ruler_x_20 = ruler_x(&harness, 20);

    // Both rulers exist
    assert!(has_ruler_bg(&harness, ruler_x_10, row));
    assert!(has_ruler_bg(&harness, ruler_x_20, row));

    // Open Remove Ruler prompt
    run_command(&mut harness, "Remove Ruler");

    // Navigate down to select the second suggestion ("Column 20")
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Ruler at column 20 should be removed
    assert!(
        !has_ruler_bg(&harness, ruler_x_20, row),
        "Ruler at column 20 should be removed"
    );

    // Ruler at column 10 should still exist
    assert!(
        has_ruler_bg(&harness, ruler_x_10, row),
        "Ruler at column 10 should still exist"
    );
}

/// Test that "Remove Ruler" with no rulers shows no prompt (nothing to remove).
#[test]
fn test_remove_ruler_none_configured() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text("test content").unwrap();
    harness.render().unwrap();

    // Try to remove a ruler when none exist — should not crash
    run_command(&mut harness, "Remove Ruler");

    // Editor should still be functional
    harness.type_text("hello").unwrap();
    harness.assert_screen_contains("hello");
}

/// Test adding a ruler with invalid input doesn't crash.
#[test]
fn test_add_ruler_invalid_input() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    run_command(&mut harness, "Add Ruler");

    // Type an invalid value
    harness.type_text("abc").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // No ruler should have been added
    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    for col in [10u16, 20, 30] {
        assert!(
            !has_ruler_bg(&harness, gutter_width(&harness) + col, row),
            "No ruler should exist after invalid input"
        );
    }
}

/// Test add ruler, then remove with bad input (rejected), then remove with correct value.
#[test]
fn test_add_then_remove_ruler_bad_then_good_input() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(90)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    let ruler_screen_x = ruler_x(&harness, 80);

    // Step 1: Add a ruler at column 80 via command palette
    assert!(
        !has_ruler_bg(&harness, ruler_screen_x, row),
        "No ruler at column 80 before adding"
    );

    run_command(&mut harness, "Add Ruler");
    harness.type_text("80").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the ruler renders at column 80
    assert!(
        has_ruler_bg(&harness, ruler_screen_x, row),
        "Ruler should render at column 80 after adding"
    );

    // Step 2: Try to remove ruler, but type bad value "32" (not a configured ruler)
    run_command(&mut harness, "Remove Ruler");
    // Type "32" which doesn't match the ruler at 80
    harness.type_text("32").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Ruler at 80 should still be there — "32" was rejected
    // Escape the still-open prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(
        has_ruler_bg(&harness, ruler_screen_x, row),
        "Ruler at 80 should still exist after rejected remove with '32'"
    );

    // Step 3: Remove the ruler with the correct value
    run_command(&mut harness, "Remove Ruler");
    // Type "80" which matches the configured ruler
    harness.type_text("80").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Ruler at 80 should now be gone
    assert!(
        !has_ruler_bg(&harness, ruler_screen_x, row),
        "Ruler at 80 should be removed after correct input"
    );
}

/// Test adding a ruler at column 0 doesn't add a ruler.
#[test]
fn test_add_ruler_zero_column() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    run_command(&mut harness, "Add Ruler");

    harness.type_text("0").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    for col in [10u16, 20, 30] {
        assert!(
            !has_ruler_bg(&harness, gutter_width(&harness) + col, row),
            "No ruler should exist after adding column 0"
        );
    }
}

/// A `0` reaching the renderer through the *config* (rather than the "Add
/// Ruler" prompt, which rejects it) is not a valid 1-based column and must be
/// dropped, leaving the other rulers untouched.
///
/// Before #2928 the renderer treated config values as 0-based screen offsets,
/// so `[0, 10]` tinted two cells: the first content column and the 11th. Now
/// only column 10 is drawn.
#[test]
fn test_config_ruler_zero_column_is_skipped() {
    let mut config = Config::default();
    config.editor.rulers = vec![0, 10];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&marker_line(40)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 80);
    assert_eq!(
        marked,
        vec![ruler_x(&harness, 10)],
        "only the ruler at column 10 should be tinted; 0 is not a valid \
         1-based column, got {marked:?}"
    );
    assert_eq!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("A"),
        "the surviving ruler must still mark the 10th character"
    );
}

/// Regression (#2928): a ruler at column N must mark the cell holding the
/// N-th character, not the one after it. Ruler columns are 1-based (the
/// "Add Ruler" prompt rejects 0, and the status bar numbers columns from 1),
/// so a ruler at 80 is the "line must not exceed 80 characters" guide.
///
/// Before the fix the tinted cell was one to the right: on a line whose 80th
/// character is `H`, the tint landed on the `1` that starts the next group.
#[test]
fn test_ruler_marks_configured_column_not_the_next_one() {
    let mut config = Config::default();
    config.editor.rulers = vec![80];

    let mut harness = EditorTestHarness::with_config(120, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&marker_line(100)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 120);
    assert_eq!(
        marked.len(),
        1,
        "exactly one cell should carry the ruler tint, got {marked:?}"
    );
    assert_eq!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("H"),
        "the ruler at column 80 must tint the 80th character ('H'), \
         not its neighbour"
    );
}

/// The ruler column must not depend on the length of the line it crosses:
/// a line shorter than the ruler column, and an empty line, keep the guide in
/// the same screen column as the line that reaches past it.
#[test]
fn test_ruler_column_is_stable_on_short_and_empty_lines() {
    let mut config = Config::default();
    config.editor.rulers = vec![80];

    let mut harness = EditorTestHarness::with_config(120, 24, config).unwrap();
    let text = format!("{}\nshort\n\ntail", marker_line(100));
    let _fixture = harness.load_buffer_from_text(&text).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let long_row = content_first_row as u16;

    let marked_long = ruler_cells_in_row(&harness, long_row, 120);
    assert_eq!(marked_long.len(), 1, "long line: got {marked_long:?}");
    assert_eq!(
        harness.get_cell(marked_long[0], long_row).as_deref(),
        Some("H"),
        "the ruler must tint the 80th character of the long line"
    );

    for (offset, what) in [(1u16, "short line"), (2, "empty line"), (3, "last line")] {
        let row = long_row + offset;
        let marked = ruler_cells_in_row(&harness, row, 120);
        assert_eq!(
            marked, marked_long,
            "{what}: ruler should stay in the same screen column"
        );
    }
}

/// With horizontal scrolling the ruler must keep marking the same buffer
/// column: it scrolls with the text rather than staying put or drifting by one.
#[test]
fn test_ruler_tracks_its_character_when_scrolled_horizontally() {
    let mut config = Config::default();
    config.editor.rulers = vec![40];
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(60, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&marker_line(100)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let before = ruler_cells_in_row(&harness, row, 60);
    assert_eq!(before.len(), 1, "unscrolled: got {before:?}");
    assert_eq!(
        harness.get_cell(before[0], row).as_deref(),
        Some("D"),
        "the ruler at column 40 must tint the 40th character ('D')"
    );

    // Walk the cursor past the right edge of the viewport so the content
    // scrolls horizontally, while keeping column 40 on screen.
    for _ in 0..60 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let after = ruler_cells_in_row(&harness, row, 60);
    assert_eq!(after.len(), 1, "scrolled: got {after:?}");
    assert!(
        after[0] < before[0],
        "the view should have scrolled horizontally (ruler at {after:?}, was {before:?})"
    );
    assert_eq!(
        harness.get_cell(after[0], row).as_deref(),
        Some("D"),
        "after scrolling, the ruler must still tint the 40th character ('D')"
    );
}

/// Double-width characters: the ruler counts *display* columns, not
/// characters. `你好世界` puts `你` on display columns 1-2 and `好` on 3-4, so
/// a ruler at column 3 tints the cell holding `好`.
///
/// Nothing snaps a ruler to a grapheme boundary — column 3 simply happens to
/// be the leading half of a double-width cell here. See
/// `test_ruler_on_even_column_over_wide_characters` for what an odd column
/// does.
#[test]
fn test_ruler_column_with_wide_characters() {
    let mut config = Config::default();
    config.editor.rulers = vec![3];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text("你好世界").unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 80);
    assert_eq!(marked.len(), 1, "got {marked:?}");
    assert_eq!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("好"),
        "the ruler should sit on the character starting at display column 3"
    );
}

/// Pins the documented coordinate space: ruler columns are *display* columns
/// (screen cells), not the grapheme count the status bar reports.
///
/// With `tab_size = 4` the two leading tabs of `\t\tab…` cover display columns
/// 1-8, so display column 10 holds `b` — the *fourth* character of the line.
/// A ruler at 10 therefore marks `b`, which is what the config docs must say;
/// "matching the status bar" would have promised the 10th character instead.
#[test]
fn test_ruler_counts_display_columns_not_characters() {
    let mut config = Config::default();
    config.editor.rulers = vec![10];
    config.editor.tab_size = 4;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text("\t\tabcdefghij").unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 80);
    assert_eq!(
        marked,
        vec![ruler_x(&harness, 10)],
        "the ruler should tint display column 10, got {marked:?}"
    );
    assert_eq!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("b"),
        "display column 10 is the 4th character of `\\t\\tab…` at tab_size 4"
    );
}

/// Documents a **known limitation** (pre-existing, not introduced by #2928):
/// rulers address display columns and are not snapped to grapheme
/// boundaries, so over full-width text an *even* ruler column falls on the
/// trailing half of a double-width cell.
///
/// `你` occupies display columns 1-2, so a ruler at 2 tints the continuation
/// cell — which ratatui has `reset()` to a blank. The tint is present in the
/// frame buffer (asserted below), but `Buffer::diff` skips the cells covered
/// by a preceding wide symbol, so it is not guaranteed to reach the terminal.
///
/// The assertion here is deliberately a description of today's behaviour, not
/// a statement that it is desirable; fixing it means snapping rulers to the
/// grapheme that covers the column, which is out of scope for #2928.
#[test]
fn test_ruler_on_even_column_over_wide_characters() {
    let mut config = Config::default();
    config.editor.rulers = vec![2];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text("你好世界").unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 80);
    assert_eq!(
        marked,
        vec![ruler_x(&harness, 2)],
        "the ruler at column 2 tints display column 2, got {marked:?}"
    );
    assert_ne!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("你"),
        "column 2 is the trailing half of `你`, not the cell carrying its symbol"
    );
}
