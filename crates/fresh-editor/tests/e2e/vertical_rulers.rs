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

/// A line of repeated full-width graphemes, for the pair of tests below.
/// Every `你` spans two display columns, so leading cells are the **odd**
/// display columns (1, 3, 5 …) and continuation cells are the **even** ones.
fn wide_line() -> String {
    "你".repeat(50)
}

/// Control for the test that follows: a ruler on the *leading* cell of a
/// double-width grapheme has always rendered.
///
/// On `wide_line()` the 2nd `你` covers display columns 3-4, so a ruler at 3
/// sits on the cell that carries the symbol and the tint is emitted.
///
/// Note what these assertions read: `EditorTestHarness::buffer()` returns
/// `Terminal::backend().buffer()`, and `TestBackend::draw` only writes the
/// cells `Buffer::diff` chose to emit. So this is the *emitted* screen, not
/// the frame buffer — which is exactly the distinction the next test needs.
#[test]
fn test_ruler_on_leading_cell_of_wide_grapheme_renders() {
    let mut config = Config::default();
    config.editor.rulers = vec![3];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&wide_line()).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 80);
    assert_eq!(
        marked,
        vec![ruler_x(&harness, 3)],
        "a ruler on the leading cell of a wide grapheme must be visible, \
         got {marked:?}"
    );
    assert_eq!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("你"),
        "display column 3 is the leading cell of the 2nd `你`"
    );
}

/// Regression: a ruler whose display column lands on the *continuation* cell
/// of a double-width grapheme must still be visible.
///
/// On `wide_line()` the 2nd `你` covers display columns 3-4, so a ruler at 4
/// addresses the continuation cell. Setting a background there is a no-op the
/// user never sees: `Buffer::diff` sets `to_skip = symbol.width() - 1` after a
/// wide symbol and gates every update on `to_skip == 0`, so the cell is never
/// emitted. Before the fix this test found **no tinted cell at all** — the
/// guide silently vanished on wide text.
///
/// The ruler now marks the leading cell of the grapheme that occupies the
/// column, so it stays visible and still points at the character sitting at
/// that column.
#[test]
fn test_ruler_on_trailing_cell_of_wide_grapheme_still_renders() {
    let mut config = Config::default();
    config.editor.rulers = vec![4];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&wide_line()).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 80);
    assert_eq!(
        marked,
        vec![ruler_x(&harness, 3)],
        "a ruler at column 4 falls inside the `你` covering columns 3-4 and \
         must be painted on its leading cell (column 3) to be visible at all, \
         got {marked:?}"
    );
    assert_eq!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("你"),
        "the tinted cell must be the one carrying the grapheme's symbol"
    );
}

/// The same snap, but with the column arithmetic exercised somewhere other
/// than a uniform row: two ASCII cells then full-width text.
///
/// `ab你你你` lays out as `a`=1, `b`=2, then `你` on 3-4, 5-6, 7-8. A ruler at
/// 6 is the continuation cell of the `你` covering 5-6, so it must snap to 5.
///
/// Deliberately configured with **only** the column that needs snapping. An
/// earlier draft of this test set `rulers = [5, 6]`; because the ruler at 5
/// already rendered, the row still had exactly one tinted cell at column 5
/// before the fix and the test passed vacuously. With `[6]` alone the pre-fix
/// row has no tinted cell at all.
#[test]
fn test_ruler_snaps_to_leading_cell_on_mixed_line() {
    let mut config = Config::default();
    config.editor.rulers = vec![6];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text("ab你你你").unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 80);
    assert_eq!(
        marked,
        vec![ruler_x(&harness, 5)],
        "a ruler at 6 belongs to the `你` covering columns 5-6 and must mark \
         its leading cell at column 5, got {marked:?}"
    );
    assert_eq!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("你"),
        "column 5 is the leading cell of the 2nd `你` on `ab你你你`"
    );
}

/// The user-visible symptom, in the shape it was reported: a ruler bar that
/// runs cleanly down the blank rows of a pane but **vanishes where a line of
/// full-width text crosses it**.
///
/// This form needs no claim about terminal emission at all — it compares the
/// same ruler across rows of a single frame. Before the fix the blank rows
/// carried a tinted cell and the CJK row carried **none**, so the bar had a
/// hole in it exactly where the text was.
///
/// Note precisely what is asserted, because it is not "the bar is perfectly
/// straight". Snapping to the covering grapheme's leading cell means that on a
/// row where the ruler falls inside a full-width character the tint sits one
/// cell to the left of where it sits on blank rows. That is the deliberate
/// trade of this fix — a guide one cell left is legible, a guide that is not
/// emitted at all is not — and it is what the config docs now describe. The
/// invariant is therefore: **every row shows the guide**, within one cell of
/// the nominal column.
///
/// Measured on this fixture: blank rows tint screen x 25, the wide-text row
/// tints 24 (the leading half of the `你`-class grapheme covering display
/// columns 19-20). Before the fix the wide-text row tinted nothing.
#[test]
fn test_ruler_bar_is_continuous_across_wide_text_rows() {
    let mut config = Config::default();
    // Column 20 is even, so on a line of `你好世界…` starting at column 1 it
    // lands on the continuation cell of the 10th grapheme (columns 19-20).
    config.editor.rulers = vec![20];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    // Blank rows above and below the wide-text row are the control: whatever
    // the ruler does there is what "correct" looks like.
    //
    // The wide line is kept to 20 graphemes / 40 display columns so it fits
    // the content area without wrapping. An earlier draft used 80 graphemes;
    // at 160 display columns that line wrapped, the row below it was a
    // *continuation* of the same wide line rather than the blank line 3, and
    // the control assertion below caught it.
    let text = format!("\n\n{}\n\n", "你好世界".repeat(5));
    let _fixture = harness.load_buffer_from_text(&text).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    // Buffer lines: 0 blank, 1 blank, 2 wide text, 3 blank.
    let blank_row = content_first_row as u16;
    let wide_row = content_first_row as u16 + 2;
    let blank_row_below = content_first_row as u16 + 3;

    let on_blank = ruler_cells_in_row(&harness, blank_row, 80);
    let on_wide = ruler_cells_in_row(&harness, wide_row, 80);
    let on_blank_below = ruler_cells_in_row(&harness, blank_row_below, 80);

    // Control first: establish what an uninterrupted bar looks like.
    assert_eq!(
        on_blank.len(),
        1,
        "control: a blank row must carry exactly one ruler cell, got {on_blank:?}"
    );
    assert_eq!(
        on_blank_below, on_blank,
        "control: the blank row below must match the blank row above"
    );

    // The symptom: before the fix this row carried no tinted cell at all, so
    // the bar had a hole in it exactly where the wide text was.
    assert_eq!(
        on_wide.len(),
        1,
        "the ruler must still be visible on the row of full-width text; \
         before the fix this row carried no tinted cell at all. blank rows \
         -> {on_blank:?}, wide-text row -> {on_wide:?}"
    );

    // It may sit one cell left of the nominal column, because it snaps to the
    // leading cell of the grapheme covering that column. It must not drift
    // further than that.
    let nominal = on_blank[0];
    assert!(
        on_wide[0] == nominal || on_wide[0] + 1 == nominal,
        "the guide may snap at most one cell left onto the covering \
         grapheme, but sat at {} against a nominal {nominal}",
        on_wide[0]
    );

    // And it must land on a cell that actually carries a symbol — the whole
    // point is that a tint on a continuation cell is never emitted.
    let glyph = harness.get_cell(on_wide[0], wide_row);
    assert!(
        glyph.as_deref().is_some_and(|g| g
            .chars()
            .next()
            .is_some_and(|c| ('\u{4e00}'..='\u{9fff}').contains(&c))),
        "the tinted cell on the wide row must carry the CJK grapheme's \
         symbol, got {glyph:?}"
    );
}

/// A combining sequence is *not* affected by the same class of problem, and
/// this test pins that rather than leaving it to assumption.
///
/// `e` + U+0301 is one grapheme of display width 1, so it occupies a single
/// cell and has no continuation cell for a ruler to be lost on. The ruler
/// lands on it directly and nothing snaps.
#[test]
fn test_ruler_on_combining_sequence_needs_no_snap() {
    let mut config = Config::default();
    config.editor.rulers = vec![2];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    // "ae\u{0301}c" -> `a`, `é` (e + combining acute), `c` on columns 1, 2, 3.
    let _fixture = harness.load_buffer_from_text("ae\u{0301}c").unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    let marked = ruler_cells_in_row(&harness, row, 80);
    assert_eq!(
        marked,
        vec![ruler_x(&harness, 2)],
        "a width-1 combining sequence has no continuation cell, so the ruler \
         stays on display column 2, got {marked:?}"
    );
    assert_eq!(
        harness.get_cell(marked[0], row).as_deref(),
        Some("e\u{0301}"),
        "column 2 holds the whole combining sequence in one cell"
    );
}
