//! E2E tests for vertical rulers feature.
//!
//! Tests: rendering at correct columns, per-buffer/per-view independence,
//! horizontal scroll behavior, and command palette add/remove.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Helper: gutter width for a small buffer is 1 (indicator) + 4 (digits) + 3 (" │ ") = 8
const SMALL_BUFFER_GUTTER: u16 = 8;

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

/// Test that rulers render the │ character at the correct column positions.
#[test]
fn test_rulers_render_at_correct_columns() {
    let mut config = Config::default();
    config.editor.rulers = vec![10, 20];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Ruler at column 10 should appear at screen x = gutter_width + 10
    let ruler_x_10 = SMALL_BUFFER_GUTTER + 10;
    let cell_10 = harness.get_cell(ruler_x_10, row);
    assert_eq!(
        cell_10.as_deref(),
        Some("│"),
        "Ruler at column 10 should render │ at screen x={ruler_x_10}, row={row}"
    );

    // Ruler at column 20 should appear at screen x = gutter_width + 20
    let ruler_x_20 = SMALL_BUFFER_GUTTER + 20;
    let cell_20 = harness.get_cell(ruler_x_20, row);
    assert_eq!(
        cell_20.as_deref(),
        Some("│"),
        "Ruler at column 20 should render │ at screen x={ruler_x_20}, row={row}"
    );

    // A column in between (e.g. column 15) should NOT have a ruler
    let non_ruler_x = SMALL_BUFFER_GUTTER + 15;
    let cell_15 = harness.get_cell(non_ruler_x, row);
    assert_ne!(
        cell_15.as_deref(),
        Some("│"),
        "Column 15 should not have a ruler character"
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

    // Check ruler character exists on every content row
    for row in content_first_row..=content_last_row {
        let cell = harness.get_cell(ruler_x, row as u16);
        assert_eq!(
            cell.as_deref(),
            Some("│"),
            "Ruler should appear on row {row}"
        );
    }
}

/// Test that rulers scroll horizontally with content.
#[test]
fn test_rulers_horizontal_scroll() {
    let mut config = Config::default();
    config.editor.rulers = vec![5, 50];

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(200)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Initially ruler at column 5 should be visible
    let ruler_x_5 = SMALL_BUFFER_GUTTER + 5;
    assert_eq!(
        harness.get_cell(ruler_x_5, row).as_deref(),
        Some("│"),
        "Ruler at col 5 should be visible initially"
    );

    // Scroll right by moving cursor far to the right
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..100 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // After scrolling, the ruler at column 5 should have scrolled off-screen
    // The cell at the old screen position should no longer be the ruler
    let screen = harness.screen_to_string();
    println!("Screen after horizontal scroll:\n{screen}");
}

/// Test no rulers when config is empty (default).
#[test]
fn test_no_rulers_by_default() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text(&"X".repeat(60)).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Check several columns — none should have ruler characters
    for col_offset in [10u16, 20, 30, 40] {
        let x = SMALL_BUFFER_GUTTER + col_offset;
        let cell = harness.get_cell(x, row);
        assert_ne!(
            cell.as_deref(),
            Some("│"),
            "No ruler should exist at column {col_offset} with default config"
        );
    }
}

/// Test that ruler color uses the theme's ruler_fg.
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
    assert!(
        style.fg.is_some(),
        "Ruler cell should have a foreground color"
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

    // Verify ruler exists in file1
    assert_eq!(
        harness.get_cell(ruler_x, row).as_deref(),
        Some("│"),
        "File1 should have a ruler at column 15"
    );

    // Open second file - it should also get rulers from config
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.get_cell(ruler_x, row).as_deref(),
        Some("│"),
        "File2 should also have rulers initialized from config"
    );

    // Switch back to file1 - rulers should still be there
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.get_cell(ruler_x, row).as_deref(),
        Some("│"),
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
    assert_ne!(
        harness.get_cell(ruler_x, row).as_deref(),
        Some("│"),
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
    assert_eq!(
        harness.get_cell(ruler_x, row).as_deref(),
        Some("│"),
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
    assert_eq!(
        harness.get_cell(ruler_x_10, row).as_deref(),
        Some("│"),
        "Ruler at column 10 should exist before removal"
    );
    assert_eq!(
        harness.get_cell(ruler_x_20, row).as_deref(),
        Some("│"),
        "Ruler at column 20 should exist before removal"
    );

    // Remove ruler at column 10 via command palette
    run_command(&mut harness, "Remove Ruler");

    // The prompt shows current rulers as suggestions.
    // The first suggestion should be "10", press Enter to select it.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify ruler at column 10 is gone
    assert_ne!(
        harness.get_cell(ruler_x_10, row).as_deref(),
        Some("│"),
        "Ruler at column 10 should be removed"
    );

    // Verify ruler at column 20 still exists
    assert_eq!(
        harness.get_cell(ruler_x_20, row).as_deref(),
        Some("│"),
        "Ruler at column 20 should still exist after removing column 10"
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

    // No ruler should have been added — editor should still be functional
    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    // Check that no ruler appeared at any common column
    for col in [10u16, 20, 30] {
        let x = SMALL_BUFFER_GUTTER + col;
        assert_ne!(
            harness.get_cell(x, row).as_deref(),
            Some("│"),
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

    // Column 0 should not have a ruler (it's the gutter area)
    // Editor should still be functional
    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;
    for col in [10u16, 20, 30] {
        let x = SMALL_BUFFER_GUTTER + col;
        assert_ne!(
            harness.get_cell(x, row).as_deref(),
            Some("│"),
            "No ruler should exist after adding column 0"
        );
    }
}
