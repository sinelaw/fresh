use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::event::{Event, OverlayFace};
use fresh::view::overlay::OverlayNamespace;
use ratatui::style::Color;

fn diagnostic_overlay(range: std::ops::Range<usize>, priority: i32, message: &str) -> Event {
    Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
        range,
        face: OverlayFace::Background {
            color: (60, 20, 20),
        },
        priority,
        message: Some(message.to_string()),
        extend_to_line_end: false,
        url: None,
    }
}

#[test]
fn test_inline_diagnostic_display() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    harness.type_text("let x: i32 = \"hello\";").unwrap();
    harness.render().unwrap();

    // Add error diagnostic on "hello" (bytes 14-21)
    harness
        .apply_event(diagnostic_overlay(
            14..21,
            100,
            "expected `i32`, found `&str`",
        ))
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("expected `i32`, found `&str`");
}

#[test]
fn test_inline_diagnostic_highest_severity_wins() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    harness.type_text("let x = invalid;").unwrap();
    harness.render().unwrap();

    // Add warning (priority 50) on "x"
    harness
        .apply_event(diagnostic_overlay(4..5, 50, "unused variable"))
        .unwrap();

    // Add error (priority 100) on "invalid"
    harness
        .apply_event(diagnostic_overlay(8..15, 100, "unknown identifier"))
        .unwrap();

    harness.render().unwrap();

    // Error (highest severity) should win
    harness.assert_screen_contains("unknown identifier");
    // Warning on the same line should not appear
    harness.assert_screen_not_contains("unused variable");
}

#[test]
fn test_inline_diagnostic_disabled_by_default() {
    // Default config has diagnostics_inline_text = false
    let mut harness = EditorTestHarness::new(80, 10).unwrap();
    harness.new_buffer().unwrap();
    harness.type_text("let x = bad;").unwrap();
    harness.render().unwrap();

    harness
        .apply_event(diagnostic_overlay(8..11, 100, "type error here"))
        .unwrap();
    harness.render().unwrap();

    // Diagnostic text should NOT appear when disabled
    harness.assert_screen_not_contains("type error here");
}

#[test]
fn test_inline_diagnostic_truncation() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    // Use a narrow viewport (40 columns)
    let mut harness = EditorTestHarness::with_config(40, 10, config).unwrap();
    harness.new_buffer().unwrap();
    harness.type_text("let x = bad_value;").unwrap();
    harness.render().unwrap();

    // Add a very long diagnostic message
    harness
        .apply_event(diagnostic_overlay(
            8..17,
            100,
            "this is a very long diagnostic message that should be truncated",
        ))
        .unwrap();
    harness.render().unwrap();

    // The full message should NOT appear (truncated)
    harness.assert_screen_not_contains("should be truncated");
    // But the beginning should appear (truncated with ellipsis)
    harness.assert_screen_contains("this is a very lon…");
}

/// When the cursor is on a line with an inline diagnostic, the current-line
/// highlight should extend across the full width of the line — including the
/// padding gap and the diagnostic text itself — not stop at the end of the
/// source code.
#[test]
fn test_current_line_highlight_extends_through_inline_diagnostic() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.highlight_current_line = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    // Type short text so there's plenty of room for the diagnostic
    harness.type_text("let x = bad;").unwrap();
    harness.render().unwrap();

    // Add error diagnostic
    harness
        .apply_event(diagnostic_overlay(8..11, 100, "type error"))
        .unwrap();
    harness.render().unwrap();

    // Cursor is on line 0 (the only line), so current_line_bg should apply.
    // Default theme is high-contrast, where current_line_bg = (20, 20, 20).
    let current_line_bg = Color::Rgb(20, 20, 20);

    // Find the content row that contains "let x = bad;"
    let (code_x, content_row) = harness
        .find_text_on_screen("let x")
        .expect("should find code text on screen");

    // Check a cell in the padding area between code end and diagnostic text.
    // Code "let x = bad;" is 13 chars wide, so col code_x+20 is in the padding.
    let pad_style = harness.get_cell_style(code_x + 20, content_row).unwrap();
    assert_eq!(
        pad_style.bg,
        Some(current_line_bg),
        "Padding between code and inline diagnostic should have current_line_bg on cursor line"
    );

    // Also check that the diagnostic text itself has the current_line_bg.
    let (diag_x, diag_row) = harness
        .find_text_on_screen("type error")
        .expect("should find diagnostic text on screen");
    let diag_style = harness.get_cell_style(diag_x, diag_row).unwrap();
    assert_eq!(
        diag_style.bg,
        Some(current_line_bg),
        "Inline diagnostic text should have current_line_bg on cursor line"
    );
}

/// Reproduces the bug where a multi-line diagnostic highlight overlay disappears
/// entirely when part of the highlighted region is scrolled out of the viewport.
/// The overlay should still be visible on the lines that remain in view.
#[test]
fn test_multiline_diagnostic_highlight_visible_when_partially_scrolled() {
    let mut config = fresh::config::Config::default();
    config.editor.line_numbers = false;

    // Use a small viewport so we can scroll easily
    // Height 10: menu bar (1) + tab bar (1) + content (6) + status (1) + prompt (1)
    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();

    // Create 20 lines of content so we have enough to scroll
    for i in 1..=20 {
        harness.type_text(&format!("line {}", i)).unwrap();
        if i < 20 {
            harness
                .send_key(KeyCode::Enter, KeyModifiers::empty())
                .unwrap();
        }
    }
    harness.render().unwrap();

    // Move cursor back to top
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Calculate byte offsets for lines 1-4 (each "line N\n" is 7 bytes for 1-digit, 8 for 2-digit)
    // "line 1\nline 2\nline 3\nline 4\n" = bytes 0..28
    // We'll add a background overlay spanning lines 1 through 4
    let line1_start = 0; // "line 1"
    let line4_end = "line 1\nline 2\nline 3\nline 4".len(); // end of "line 4"

    // Add a bright background overlay across lines 1-4
    harness
        .apply_event(Event::AddOverlay {
            namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
            range: line1_start..line4_end,
            face: OverlayFace::Background {
                color: (60, 20, 20),
            },
            priority: 100,
            message: Some("multi-line error".to_string()),
            extend_to_line_end: false,
            url: None,
        })
        .unwrap();
    harness.render().unwrap();

    // Verify the overlay is visible on line 1 (content row 2) before scrolling
    let style_before = harness.get_cell_style(0, 2).unwrap();
    assert_eq!(
        style_before.bg,
        Some(Color::Rgb(60, 20, 20)),
        "Overlay background should be visible on line 1 before scrolling"
    );

    // Scroll down 1 event (3 lines by default), so lines 4-9+ are visible.
    // Lines 1-3 go out of view, but line 4 is still visible with the overlay.
    harness.mouse_scroll_down(40, 5).unwrap();
    harness.render().unwrap();

    // After scrolling down 3 lines, line 4 should be at the top of content area (row 2)
    let row_text = harness.get_row_text(2);
    assert!(
        row_text.contains("line 4"),
        "Expected line 4 at top of content area, got: {:?}",
        row_text.trim()
    );

    // The overlay spans lines 1-4, so line 4 should still show the highlight
    // even though lines 1-3 (including the overlay start) are scrolled out of view.
    let style_after = harness.get_cell_style(0, 2).unwrap();
    assert_eq!(
        style_after.bg,
        Some(Color::Rgb(60, 20, 20)),
        "Overlay background should still be visible on line 4 after scrolling lines 1-3 out of view"
    );
}
