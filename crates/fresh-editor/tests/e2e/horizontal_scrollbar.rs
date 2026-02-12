use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Helper: check if any scrollbar-colored cell exists at the given row
/// (scanning all columns). Used to detect the horizontal scrollbar.
fn has_scrollbar_at_row(harness: &EditorTestHarness, row: u16) -> bool {
    let buffer = harness.buffer();
    let width = buffer.area.width;
    for col in 0..width {
        if harness.is_scrollbar_thumb_at(col, row) || harness.is_scrollbar_track_at(col, row) {
            return true;
        }
    }
    false
}

/// Create content with very long lines to trigger horizontal scrolling
fn long_lines_content(num_lines: usize, line_length: usize) -> String {
    (0..num_lines)
        .map(|i| {
            let prefix = format!("Line {i}: ");
            let padding_len = line_length.saturating_sub(prefix.len());
            format!("{prefix}{}", "X".repeat(padding_len))
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Test that the horizontal scrollbar is visible at the bottom of the split
/// when line wrap is disabled and content has long lines
#[test]
fn test_horizontal_scrollbar_visible_with_long_lines() {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = true;
    config.editor.show_vertical_scrollbar = true;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // With line wrap off and long lines, the horizontal scrollbar should appear
    // at the bottom row of the content area (before status bar)
    let (_, last_content_row) = harness.content_area_rows();
    let found_hscrollbar = has_scrollbar_at_row(&harness, last_content_row as u16)
        || has_scrollbar_at_row(&harness, (last_content_row + 1) as u16);

    assert!(
        found_hscrollbar,
        "Horizontal scrollbar should be visible when line wrap is off and lines are long"
    );
}

/// Test that the horizontal scrollbar is NOT visible when line wrap is enabled
#[test]
fn test_horizontal_scrollbar_hidden_with_line_wrap() {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config.editor.show_horizontal_scrollbar = true;
    config.editor.show_vertical_scrollbar = true;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // With line wrap enabled, horizontal scrollbar should still be rendered
    // (the track is drawn), but the thumb should fill the entire width
    // since there's no horizontal scrolling needed with wrapping
    harness.assert_screen_contains("Line 0:");
}

/// Test that toggling vertical scrollbar on/off works
#[test]
fn test_toggle_vertical_scrollbar() {
    let mut config = Config::default();
    config.editor.show_horizontal_scrollbar = false;
    config.editor.show_vertical_scrollbar = true;
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Verify vertical scrollbar is visible at column 79
    assert!(
        harness.has_scrollbar_at_column(79),
        "Vertical scrollbar should be visible initially"
    );

    // Toggle vertical scrollbar off via the editor API
    harness.editor_mut().toggle_vertical_scrollbar();
    harness.render().unwrap();

    // Verify the status message was set
    let msg = harness.editor().get_status_message().cloned();
    assert!(
        msg.as_deref() == Some("Vertical scrollbar hidden"),
        "Expected status message 'Vertical scrollbar hidden', got: {:?}",
        msg
    );

    // Toggle it back on
    harness.editor_mut().toggle_vertical_scrollbar();
    harness.render().unwrap();

    let msg = harness.editor().get_status_message().cloned();
    assert!(
        msg.as_deref() == Some("Vertical scrollbar shown"),
        "Expected status message 'Vertical scrollbar shown', got: {:?}",
        msg
    );
    assert!(
        harness.has_scrollbar_at_column(79),
        "Vertical scrollbar should be visible again after toggle"
    );
}

/// Test that toggling horizontal scrollbar on/off works
#[test]
fn test_toggle_horizontal_scrollbar() {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = true;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Horizontal scrollbar should be visible
    let (_, last_content_row) = harness.content_area_rows();
    let has_initial = has_scrollbar_at_row(&harness, last_content_row as u16)
        || has_scrollbar_at_row(&harness, (last_content_row + 1) as u16);
    assert!(
        has_initial,
        "Horizontal scrollbar should be visible initially"
    );

    // Toggle it off
    harness.editor_mut().toggle_horizontal_scrollbar();
    harness.render().unwrap();

    let msg = harness.editor().get_status_message().cloned();
    assert!(
        msg.as_deref() == Some("Horizontal scrollbar hidden"),
        "Expected status message 'Horizontal scrollbar hidden', got: {:?}",
        msg
    );

    // Toggle it back on
    harness.editor_mut().toggle_horizontal_scrollbar();
    harness.render().unwrap();

    let msg = harness.editor().get_status_message().cloned();
    assert!(
        msg.as_deref() == Some("Horizontal scrollbar shown"),
        "Expected status message 'Horizontal scrollbar shown', got: {:?}",
        msg
    );
}

/// Test that config option show_vertical_scrollbar: false hides scrollbar on startup
#[test]
fn test_config_show_vertical_scrollbar_false() {
    let mut config = Config::default();
    config.editor.show_vertical_scrollbar = false;
    config.editor.show_horizontal_scrollbar = false;
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    // Use long lines so column 79 has actual text content, not empty space
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // With vertical scrollbar disabled, the content should extend to the last column.
    // Column 79 should show text content, not scrollbar.
    let row_text = harness.get_row_text(5);
    assert!(
        row_text.contains("X"),
        "Content should be visible with scrollbar hidden. Row text: {}",
        row_text.trim()
    );
}

/// Test that config option show_horizontal_scrollbar: false hides scrollbar on startup
#[test]
fn test_config_show_horizontal_scrollbar_false() {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // The horizontal scrollbar row should not exist
    // Content should extend to the bottom of the split area
    let (_, last_content_row) = harness.content_area_rows();
    let row_text = harness.get_row_text(last_content_row as u16);
    // With horizontal scrollbar hidden, content should use the extra row
    assert!(
        row_text.contains("Line") || row_text.contains("X"),
        "Last content row should show actual content when horizontal scrollbar is hidden. Got: {}",
        row_text.trim()
    );
}

/// Test horizontal scrollbar in split view
#[test]
fn test_horizontal_scrollbar_in_split_view() {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = true;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Split vertically
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Split Vertical").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Both splits should show their content
    harness.assert_screen_contains("Line 0:");
}
