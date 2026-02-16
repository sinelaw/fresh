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

/// Helper: gutter width for a small buffer is 1 (indicator) + 4 (digits) + 3 (" │ ") = 8
const SMALL_BUFFER_GUTTER: u16 = 8;

/// The default ruler background color: Rgb(50, 50, 50)
const RULER_BG: Color = Color::Rgb(50, 50, 50);

/// Helper to check if a cell has the ruler background color.
fn has_ruler_bg(harness: &EditorTestHarness, x: u16, y: u16) -> bool {
    harness
        .get_cell_style(x, y)
        .map(|s| s.bg == Some(RULER_BG))
        .unwrap_or(false)
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
        has_ruler_bg(&harness, SMALL_BUFFER_GUTTER + 10, row),
        "Ruler bg should appear at column 10"
    );

    // Ruler at column 20 should have ruler bg
    assert!(
        has_ruler_bg(&harness, SMALL_BUFFER_GUTTER + 20, row),
        "Ruler bg should appear at column 20"
    );

    // Column 15 should NOT have ruler bg
    assert!(
        !has_ruler_bg(&harness, SMALL_BUFFER_GUTTER + 15, row),
        "Column 15 should not have ruler bg"
    );

    // Rulers should preserve text content (not overwrite with │)
    let cell_10 = harness.get_cell(SMALL_BUFFER_GUTTER + 10, row);
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
    let ruler_x = SMALL_BUFFER_GUTTER + 10;

    for row in content_first_row..=content_last_row {
        assert!(
            has_ruler_bg(&harness, ruler_x, row as u16),
            "Ruler bg should appear on row {row}"
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
    let ruler_screen_x = SMALL_BUFFER_GUTTER + 5;
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
            !has_ruler_bg(&harness, SMALL_BUFFER_GUTTER + col_offset, row),
            "No ruler should exist at column {col_offset} with default config"
        );
    }
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
    let ruler_x = SMALL_BUFFER_GUTTER + 10;

    let style = harness.get_cell_style(ruler_x, content_first_row as u16);
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
    let ruler_x = SMALL_BUFFER_GUTTER + 15;

    assert!(
        has_ruler_bg(&harness, ruler_x, row),
        "File1 should have a ruler at column 15"
    );

    // Open second file - should also get rulers from config
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    assert!(
        has_ruler_bg(&harness, ruler_x, row),
        "File2 should also have rulers initialized from config"
    );

    // Switch back to file1 - rulers should still be there
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    assert!(
        has_ruler_bg(&harness, ruler_x, row),
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
    let ruler_x = SMALL_BUFFER_GUTTER + 25;

    // Before: no ruler at column 25
    assert!(
        !has_ruler_bg(&harness, ruler_x, row),
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
        has_ruler_bg(&harness, ruler_x, row),
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
    let ruler_x_10 = SMALL_BUFFER_GUTTER + 10;
    let ruler_x_20 = SMALL_BUFFER_GUTTER + 20;

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
    let ruler_x_10 = SMALL_BUFFER_GUTTER + 10;
    let ruler_x_20 = SMALL_BUFFER_GUTTER + 20;

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
            !has_ruler_bg(&harness, SMALL_BUFFER_GUTTER + col, row),
            "No ruler should exist after invalid input"
        );
    }
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
            !has_ruler_bg(&harness, SMALL_BUFFER_GUTTER + col, row),
            "No ruler should exist after adding column 0"
        );
    }
}
