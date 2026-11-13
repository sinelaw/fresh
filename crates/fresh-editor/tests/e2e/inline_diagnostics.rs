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
    let code = "let x: i32 = \"hello\";";
    harness.type_text(code).unwrap();
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

    let (code_x, content_row) = harness
        .find_text_on_screen("let x")
        .expect("should find code text on screen");
    let (diag_x, diag_row) = harness
        .find_text_on_screen("expected `i32`")
        .expect("should find diagnostic text on screen");
    assert_eq!(content_row, diag_row);
    let code_len = code.len() as u16;
    assert_eq!(
        diag_x,
        code_x + code_len + 2,
        "Diagnostic should be left-aligned 2 spaces after code"
    );

    // Default theme is high-contrast, where error_fg = (255, 80, 80) and error_bg = (50, 0, 0)
    let error_fg = Color::Rgb(255, 80, 80);
    let error_bg = Color::Rgb(50, 0, 0);
    let style = harness.get_cell_style(diag_x, diag_row).unwrap();
    assert_eq!(
        style.fg,
        Some(error_fg),
        "Diagnostic pill should have error_fg"
    );
    assert_eq!(
        style.bg,
        Some(error_bg),
        "Diagnostic pill should have error_bg"
    );
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
/// 2-space gap and the trailing line space — while the diagnostic pill retains
/// its severity background.
#[test]
fn test_current_line_highlight_extends_through_inline_diagnostic() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.highlight_current_line = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    // Type short text so there's plenty of room for the diagnostic
    let code = "let x = bad;";
    harness.type_text(code).unwrap();
    harness.render().unwrap();

    // Add error diagnostic
    harness
        .apply_event(diagnostic_overlay(8..11, 100, "type error"))
        .unwrap();
    harness.render().unwrap();

    // Cursor is on line 0 (the only line), so current_line_bg should apply.
    // Default theme is high-contrast, where current_line_bg = (20, 20, 20)
    // and error_bg = (50, 0, 0).
    let current_line_bg = Color::Rgb(20, 20, 20);
    let error_bg = Color::Rgb(50, 0, 0);

    // Find the content row that contains "let x = bad;"
    let (code_x, content_row) = harness
        .find_text_on_screen("let x")
        .expect("should find code text on screen");

    let code_len = code.len() as u16;

    // Gap cell at code_x + code_len should have current_line_bg
    let gap_style = harness
        .get_cell_style(code_x + code_len, content_row)
        .unwrap();
    assert_eq!(
        gap_style.bg,
        Some(current_line_bg),
        "Gap between code and inline diagnostic should have current_line_bg on cursor line"
    );

    // Diagnostic pill cell at diag_x should have error_bg (pill styling takes precedence)
    let (diag_x, diag_row) = harness
        .find_text_on_screen("type error")
        .expect("should find diagnostic text on screen");
    assert_eq!(content_row, diag_row);
    assert_eq!(diag_x, code_x + code_len + 2);
    let diag_style = harness.get_cell_style(diag_x, diag_row).unwrap();
    assert_eq!(
        diag_style.bg,
        Some(error_bg),
        "Inline diagnostic pill should have error_bg on cursor line (pill styling takes precedence)"
    );

    // Trailing cell after diagnostic should have current_line_bg
    let trailing_x = diag_x + "type error".len() as u16 + 2;
    let trailing_style = harness.get_cell_style(trailing_x, content_row).unwrap();
    assert_eq!(
        trailing_style.bg,
        Some(current_line_bg),
        "Trailing space after inline diagnostic should have current_line_bg on cursor line"
    );
}

/// Reproduces the bug where a multi-line diagnostic highlight overlay disappears
/// entirely when part of the highlighted region is scrolled out of the viewport.
/// The overlay should still be visible on the lines that remain in view.
#[test]
fn test_multiline_diagnostic_highlight_visible_when_partially_scrolled() {
    let mut config = fresh::config::Config::default();
    config.editor.line_numbers = false;
    // Ctrl+Home and the post-scroll cursor reposition both cross the
    // cursor-jump threshold (dy >= 3); without this, the trail can paint
    // cells with blended cursor/bg colors and race the assertion below.
    config.editor.animations = false;

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

/// A brand-new file is empty, and an empty buffer is a single row holding no
/// characters — so it has no source byte of its own, and with no preceding
/// newline it is not the trailing-empty-line case either. A diagnostic on it
/// was counted but never drawn: no gutter marker and no inline message, until
/// a typed line break brought it out (issue #3484).
#[test]
fn test_inline_diagnostic_on_empty_buffer() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    // Deliberately type nothing: the buffer stays zero bytes, exactly as a
    // newly created file is when a language server first reports on it.
    harness.render().unwrap();

    // The only position an empty buffer can carry a diagnostic at.
    harness
        .apply_event(diagnostic_overlay(0..0, 100, "main function not found"))
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("main function not found");
}

/// The gutter marker is the half of #3484 that shows under the default config,
/// where the inline message is switched off.
#[test]
fn test_diagnostic_gutter_marker_on_empty_buffer() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = false;
    config.editor.line_numbers = true;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    harness.render().unwrap();

    harness
        .apply_event(diagnostic_overlay(0..0, 100, "main function not found"))
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("●");
}

#[test]
fn test_inline_diagnostic_severities_pill_styling() {
    let severities = [
        (
            50,
            "warning message",
            Color::Rgb(255, 255, 0),
            Color::Rgb(35, 30, 0),
        ),
        (
            30,
            "info message",
            Color::Rgb(0, 255, 255),
            Color::Rgb(0, 25, 55),
        ),
        (
            10,
            "hint message",
            Color::Rgb(255, 255, 255),
            Color::Rgb(30, 30, 30),
        ),
    ];

    for (priority, message, expected_fg, expected_bg) in severities {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = true;
        config.editor.line_numbers = false;

        let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
        harness.new_buffer().unwrap();
        let code = "let val = 42;";
        harness.type_text(code).unwrap();
        harness.render().unwrap();

        harness
            .apply_event(diagnostic_overlay(4..7, priority, message))
            .unwrap();
        harness.render().unwrap();

        let (code_x, content_row) = harness.find_text_on_screen("let val").unwrap();
        let (diag_x, diag_row) = harness.find_text_on_screen(message).unwrap();
        assert_eq!(content_row, diag_row);
        let code_len = code.len() as u16;
        assert_eq!(diag_x, code_x + code_len + 2);

        let style = harness.get_cell_style(diag_x, diag_row).unwrap();
        assert_eq!(
            style.fg,
            Some(expected_fg),
            "Priority {priority} should have expected fg"
        );
        assert_eq!(
            style.bg,
            Some(expected_bg),
            "Priority {priority} should have expected bg pill"
        );
    }
}

#[test]
fn test_inline_diagnostic_mouse_click_does_not_move_cursor_past_code() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    let code = "let x = 1;";
    harness.type_text(code).unwrap();
    harness.render().unwrap();

    harness
        .apply_event(diagnostic_overlay(0..1, 100, "sample error"))
        .unwrap();
    harness.render().unwrap();

    let (diag_x, diag_row) = harness.find_text_on_screen("sample error").unwrap();

    // Click on the diagnostic text cell
    harness.mouse_click(diag_x + 2, diag_row).unwrap();
    harness.render().unwrap();

    // Clicking on diagnostic text (unmapped cell) should not move cursor past the end of code
    assert!(harness.cursor_position() <= code.len());
}

#[test]
fn test_current_line_highlight_cursor_on_and_off_line() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.highlight_current_line = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    let line1 = "let x = bad;";
    let line2 = "let y = good;";
    harness.type_text(line1).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::empty())
        .unwrap();
    harness.type_text(line2).unwrap();
    harness.render().unwrap();

    // Add error diagnostic on line 0 ("bad")
    harness
        .apply_event(diagnostic_overlay(8..11, 100, "type error"))
        .unwrap();

    // Move cursor back to line 0
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let current_line_bg = Color::Rgb(20, 20, 20);
    let editor_bg = Color::Rgb(0, 0, 0);
    let error_bg = Color::Rgb(50, 0, 0);

    let (code_x, line1_row) = harness.find_text_on_screen(line1).unwrap();
    let (diag_x, diag_row) = harness.find_text_on_screen("type error").unwrap();
    assert_eq!(line1_row, diag_row);
    let code_len = line1.len() as u16;

    // 1. Cursor ON line with diagnostic:
    // Gap gets current_line_bg
    let gap_style = harness
        .get_cell_style(code_x + code_len, line1_row)
        .unwrap();
    assert_eq!(
        gap_style.bg,
        Some(current_line_bg),
        "Gap should have current_line_bg when cursor is on line"
    );

    // Pill gets severity bg (error_bg)
    let diag_style = harness.get_cell_style(diag_x, diag_row).unwrap();
    assert_eq!(
        diag_style.bg,
        Some(error_bg),
        "Pill should have error_bg when cursor is on line"
    );

    // Trailing line gets current_line_bg
    let trailing_x = diag_x + "type error".len() as u16 + 2;
    let trailing_style = harness.get_cell_style(trailing_x, line1_row).unwrap();
    assert_eq!(
        trailing_style.bg,
        Some(current_line_bg),
        "Trailing cell should have current_line_bg when cursor is on line"
    );

    // 2. Move cursor OFF line with diagnostic (to line 1)
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    // Gap and trailing line revert to editor_bg
    let gap_style_off = harness
        .get_cell_style(code_x + code_len, line1_row)
        .unwrap();
    assert_eq!(
        gap_style_off.bg,
        Some(editor_bg),
        "Gap should revert to editor_bg when cursor moves off line"
    );

    let trailing_style_off = harness.get_cell_style(trailing_x, line1_row).unwrap();
    assert_eq!(
        trailing_style_off.bg,
        Some(editor_bg),
        "Trailing line should revert to editor_bg when cursor moves off line"
    );

    // Pill retains severity bg
    let diag_style_off = harness.get_cell_style(diag_x, diag_row).unwrap();
    assert_eq!(
        diag_style_off.bg,
        Some(error_bg),
        "Pill should retain error_bg when cursor moves off line"
    );
}

#[test]
fn test_inline_diagnostic_highest_severity_permutations() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    // Scenario A: Hint (10), Info (30), Warning (50), Error (100) added in increasing order
    {
        let mut harness = EditorTestHarness::with_config(80, 10, config.clone()).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text("let a = 1;").unwrap();
        harness.render().unwrap();

        harness
            .apply_event(diagnostic_overlay(0..3, 10, "hint message"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(0..3, 30, "info message"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(0..3, 50, "warning message"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(0..3, 100, "error message"))
            .unwrap();
        harness.render().unwrap();

        harness.assert_screen_contains("error message");
        harness.assert_screen_not_contains("hint message");
        harness.assert_screen_not_contains("info message");
        harness.assert_screen_not_contains("warning message");

        let (diag_x, diag_row) = harness.find_text_on_screen("error message").unwrap();
        let style = harness.get_cell_style(diag_x, diag_row).unwrap();
        assert_eq!(
            style.bg,
            Some(Color::Rgb(50, 0, 0)),
            "Highest severity Error pill bg should be applied"
        );
    }

    // Scenario B: Error (100) added first, followed by lower priorities
    {
        let mut harness = EditorTestHarness::with_config(80, 10, config.clone()).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text("let b = 2;").unwrap();
        harness.render().unwrap();

        harness
            .apply_event(diagnostic_overlay(0..3, 100, "error first"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(0..3, 50, "warning second"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(0..3, 30, "info third"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(0..3, 10, "hint fourth"))
            .unwrap();
        harness.render().unwrap();

        harness.assert_screen_contains("error first");
        harness.assert_screen_not_contains("warning second");
        harness.assert_screen_not_contains("info third");
        harness.assert_screen_not_contains("hint fourth");
    }

    // Scenario C: Info (30) vs Warning (50) -> Warning wins
    {
        let mut harness = EditorTestHarness::with_config(80, 10, config.clone()).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text("let c = 3;").unwrap();
        harness.render().unwrap();

        harness
            .apply_event(diagnostic_overlay(0..3, 30, "info message"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(4..5, 50, "warning wins"))
            .unwrap();
        harness.render().unwrap();

        harness.assert_screen_contains("warning wins");
        harness.assert_screen_not_contains("info message");

        let (diag_x, diag_row) = harness.find_text_on_screen("warning wins").unwrap();
        let style = harness.get_cell_style(diag_x, diag_row).unwrap();
        assert_eq!(
            style.bg,
            Some(Color::Rgb(35, 30, 0)),
            "Warning pill bg should be applied"
        );
    }
}

#[test]
fn test_inline_diagnostic_multiline_handling() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    // Subcase 1: Diagnostic message containing newline characters
    {
        let mut harness = EditorTestHarness::with_config(80, 10, config.clone()).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text("fn test() {}").unwrap();
        harness.render().unwrap();

        let multiline_msg = "first line of error\nsecond line stacktrace\nthird line details";
        harness
            .apply_event(diagnostic_overlay(0..2, 100, multiline_msg))
            .unwrap();
        harness.render().unwrap();

        harness.assert_screen_contains("first line of error");
        harness.assert_screen_not_contains("second line stacktrace");
        harness.assert_screen_not_contains("third line details");
    }

    // Subcase 2: Multi-line diagnostic byte range spanning lines 1 through 3
    {
        let mut harness = EditorTestHarness::with_config(80, 10, config.clone()).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text("line one").unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::empty())
            .unwrap();
        harness.type_text("line two").unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::empty())
            .unwrap();
        harness.type_text("line three").unwrap();
        harness.render().unwrap();

        // Overlay range covers line 1 through line 3 (bytes 0..26)
        let total_len = "line one\nline two\nline three".len();
        harness
            .apply_event(diagnostic_overlay(0..total_len, 100, "spanning error"))
            .unwrap();
        harness.render().unwrap();

        let (_diag_x, diag_row) = harness.find_text_on_screen("spanning error").unwrap();
        let (_, line1_row) = harness.find_text_on_screen("line one").unwrap();
        let (_, line2_row) = harness.find_text_on_screen("line two").unwrap();
        let (_, line3_row) = harness.find_text_on_screen("line three").unwrap();

        assert_eq!(
            diag_row, line1_row,
            "Inline diagnostic should be rendered on line 1"
        );

        // Verify line 2 and line 3 do NOT render "spanning error"
        let line2_text = harness.get_row_text(line2_row);
        let line3_text = harness.get_row_text(line3_row);
        assert!(
            !line2_text.contains("spanning error"),
            "Line 2 should not have inline diagnostic: {}",
            line2_text
        );
        assert!(
            !line3_text.contains("spanning error"),
            "Line 3 should not have inline diagnostic: {}",
            line3_text
        );
    }
}

#[test]
fn test_inline_diagnostic_mouse_coordinate_mapping_gap_and_text() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    let code = "let answer = 42;";
    harness.type_text(code).unwrap();
    harness.render().unwrap();

    let diag_msg = "type error occurred";
    harness
        .apply_event(diagnostic_overlay(4..10, 100, diag_msg))
        .unwrap();
    harness.render().unwrap();

    let (code_x, content_row) = harness.find_text_on_screen(code).unwrap();
    let (diag_x, diag_row) = harness.find_text_on_screen(diag_msg).unwrap();
    assert_eq!(content_row, diag_row);
    let code_len = code.len() as u16;
    let diag_len = diag_msg.len() as u16;

    // Test clicking various positions:
    // 1. First gap cell
    harness.mouse_click(code_x + code_len, content_row).unwrap();
    assert!(
        harness.cursor_position() <= code.len(),
        "Cursor should not exceed code len after click on gap cell 1"
    );

    // 2. Second gap cell
    harness
        .mouse_click(code_x + code_len + 1, content_row)
        .unwrap();
    assert!(
        harness.cursor_position() <= code.len(),
        "Cursor should not exceed code len after click on gap cell 2"
    );

    // 3. Diagnostic text start
    harness.mouse_click(diag_x, content_row).unwrap();
    assert!(
        harness.cursor_position() <= code.len(),
        "Cursor should not exceed code len after click on diag start"
    );

    // 4. Diagnostic text middle
    harness.mouse_click(diag_x + 5, content_row).unwrap();
    assert!(
        harness.cursor_position() <= code.len(),
        "Cursor should not exceed code len after click on diag middle"
    );

    // 5. Diagnostic text end
    harness
        .mouse_click(diag_x + diag_len - 1, content_row)
        .unwrap();
    assert!(
        harness.cursor_position() <= code.len(),
        "Cursor should not exceed code len after click on diag end"
    );

    // 6. Trailing cell after diagnostic text
    harness
        .mouse_click(diag_x + diag_len + 3, content_row)
        .unwrap();
    assert!(
        harness.cursor_position() <= code.len(),
        "Cursor should not exceed code len after click on trailing line cell"
    );
}

#[test]
fn test_adversarial_inline_diagnostic_left_alignment_tabs_and_lengths() {
    let tab_sizes = [2, 4, 8];

    for tab_size in tab_sizes {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = true;
        config.editor.line_numbers = false;
        config.editor.tab_size = tab_size;

        let mut harness = EditorTestHarness::with_config(160, 10, config).unwrap();
        harness.new_buffer().unwrap();

        harness
            .type_text("short\n\tindent\n\t\tdouble\nlet variable_with_significant_length = 42;")
            .unwrap();
        harness.render().unwrap();

        harness
            .apply_event(diagnostic_overlay(0..5, 100, "err0"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(6..13, 100, "err1"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(14..22, 100, "err2"))
            .unwrap();
        harness
            .apply_event(diagnostic_overlay(23..66, 100, "err3"))
            .unwrap();
        harness.render().unwrap();

        let (d0_x, _) = harness.find_text_on_screen("err0").unwrap();
        assert_eq!(
            d0_x,
            5 + 2,
            "Line 0 diagnostic must be 2 columns after 5-char code (tab_size={tab_size})"
        );

        let (d1_x, _) = harness.find_text_on_screen("err1").unwrap();
        assert_eq!(
            d1_x,
            (tab_size + 6 + 2) as u16,
            "Line 1 diagnostic must be 2 columns after expanded tab (tab_size={tab_size})"
        );

        let (d2_x, _) = harness.find_text_on_screen("err2").unwrap();
        assert_eq!(
            d2_x,
            (2 * tab_size + 6 + 2) as u16,
            "Line 2 diagnostic must be 2 columns after double expanded tab (tab_size={tab_size})"
        );

        let code3 = "let variable_with_significant_length = 42;";
        let (d3_x, _) = harness.find_text_on_screen("err3").unwrap();
        assert_eq!(
            d3_x,
            code3.len() as u16 + 2,
            "Line 3 diagnostic must be 2 columns after code (tab_size={tab_size})"
        );
    }

    let mut config_gutter = fresh::config::Config::default();
    config_gutter.editor.diagnostics_inline_text = true;
    config_gutter.editor.line_numbers = true;

    let mut harness_gutter = EditorTestHarness::with_config(100, 10, config_gutter).unwrap();
    harness_gutter.new_buffer().unwrap();
    let code = "const PI: f64 = 3.14159;";
    harness_gutter.type_text(code).unwrap();
    harness_gutter.render().unwrap();

    harness_gutter
        .apply_event(diagnostic_overlay(0..code.len(), 100, "unused constant"))
        .unwrap();
    harness_gutter.render().unwrap();

    let (code_x, content_row) = harness_gutter
        .find_text_on_screen("const PI")
        .expect("should find code on screen with line numbers");
    let (diag_x, diag_row) = harness_gutter
        .find_text_on_screen("unused constant")
        .expect("should find diagnostic on screen with line numbers");
    assert_eq!(content_row, diag_row);
    assert_eq!(
        diag_x,
        code_x + code.len() as u16 + 2,
        "Diagnostic with line numbers must begin exactly 2 columns after code"
    );
}

#[test]
fn test_adversarial_inline_diagnostic_pill_colors_all_themes() {
    let themes_and_expected = [
        (
            "dark",
            Color::Red,
            Color::Rgb(60, 20, 20),
            Color::Yellow,
            Color::Rgb(60, 50, 0),
            Color::Blue,
            Color::Rgb(0, 30, 60),
            Color::Gray,
            Color::Rgb(30, 30, 30),
        ),
        (
            "light",
            Color::Rgb(180, 0, 0),
            Color::Rgb(255, 210, 210),
            Color::Rgb(150, 110, 0),
            Color::Rgb(255, 248, 170),
            Color::Rgb(0, 0, 180),
            Color::Rgb(210, 230, 255),
            Color::Rgb(100, 100, 100),
            Color::Rgb(235, 235, 240),
        ),
        (
            "high-contrast",
            Color::Rgb(255, 80, 80),
            Color::Rgb(50, 0, 0),
            Color::Rgb(255, 255, 0),
            Color::Rgb(35, 30, 0),
            Color::Rgb(0, 255, 255),
            Color::Rgb(0, 25, 55),
            Color::Rgb(255, 255, 255),
            Color::Rgb(30, 30, 30),
        ),
    ];

    for (theme_name, err_fg, err_bg, warn_fg, warn_bg, info_fg, info_bg, hint_fg, hint_bg) in
        themes_and_expected
    {
        let severities = [
            (100, "error_pill_msg", err_fg, err_bg),
            (50, "warn_pill_msg", warn_fg, warn_bg),
            (30, "info_pill_msg", info_fg, info_bg),
            (10, "hint_pill_msg", hint_fg, hint_bg),
        ];

        for (priority, msg, expected_fg, expected_bg) in severities {
            let config = fresh::config::Config {
                theme: theme_name.into(),
                editor: fresh::config::EditorConfig {
                    diagnostics_inline_text: true,
                    line_numbers: false,
                    ..Default::default()
                },
                ..Default::default()
            };

            let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
            harness.new_buffer().unwrap();
            harness.type_text("let x = 1;").unwrap();
            harness.render().unwrap();

            harness
                .apply_event(diagnostic_overlay(0..5, priority, msg))
                .unwrap();
            harness.render().unwrap();

            let (diag_x, diag_row) = harness
                .find_text_on_screen(msg)
                .unwrap_or_else(|| panic!("Should find '{msg}' on screen in theme '{theme_name}'"));
            let style = harness.get_cell_style(diag_x, diag_row).unwrap();

            assert_eq!(
                style.fg,
                Some(expected_fg),
                "Theme '{theme_name}' priority {priority} fg color mismatch"
            );
            assert_eq!(
                style.bg,
                Some(expected_bg),
                "Theme '{theme_name}' priority {priority} bg color mismatch"
            );
        }
    }
}

#[test]
fn test_adversarial_inline_diagnostic_narrow_viewport_threshold() {
    let code = "0123456789";
    let diag_msg = "error message";

    // Viewport width 22 -> available 12 (<= 12) -> SUPPRESSED
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = true;
        config.editor.line_numbers = false;
        config.editor.show_vertical_scrollbar = false;

        let mut harness = EditorTestHarness::with_config(22, 10, config).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text(code).unwrap();
        harness.render().unwrap();

        harness
            .apply_event(diagnostic_overlay(0..10, 100, diag_msg))
            .unwrap();
        harness.render().unwrap();

        harness.assert_screen_not_contains(diag_msg);
        assert!(
            harness.find_text_on_screen(diag_msg).is_none(),
            "Diagnostic must be suppressed when available == 12 columns"
        );
    }

    // Viewport width 23 -> available 13 (> 12) -> RENDERED
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = true;
        config.editor.line_numbers = false;
        config.editor.show_vertical_scrollbar = false;

        let mut harness = EditorTestHarness::with_config(23, 10, config).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text(code).unwrap();
        harness.render().unwrap();

        harness
            .apply_event(diagnostic_overlay(0..10, 100, diag_msg))
            .unwrap();
        harness.render().unwrap();

        harness.assert_screen_contains("error mess…");
        let (diag_x, _) = harness
            .find_text_on_screen("error mess…")
            .expect("Diagnostic must appear when available == 13 columns");
        assert_eq!(diag_x, 10 + 2, "Diagnostic must start 2 columns after code");
    }

    // Viewports where width <= 15 -> available <= 5 -> SUPPRESSED without panic
    for width in [10, 12, 15] {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = true;
        config.editor.line_numbers = false;
        config.editor.show_vertical_scrollbar = false;

        let mut harness = EditorTestHarness::with_config(width, 10, config).unwrap();
        harness.new_buffer().unwrap();
        harness.type_text(code).unwrap();
        harness.render().unwrap();

        harness
            .apply_event(diagnostic_overlay(0..10, 100, diag_msg))
            .unwrap();
        harness.render().unwrap();

        harness.assert_screen_not_contains(diag_msg);
    }
}

#[test]
fn test_adversarial_inline_diagnostic_multibyte_unicode_truncation() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(35, 10, config).unwrap();
    harness.new_buffer().unwrap();
    harness.type_text("let x = 1;").unwrap();
    harness.render().unwrap();

    let unicode_msg = "error: αβγδε warning: long unicode message here";
    harness
        .apply_event(diagnostic_overlay(0..10, 100, unicode_msg))
        .unwrap();
    harness.render().unwrap();

    let (_, content_row) = harness
        .find_text_on_screen("let x = 1;")
        .expect("should find code on screen");
    let row_text = harness.get_row_text(content_row);
    assert!(
        row_text.contains('…'),
        "Truncated unicode diagnostic must contain ellipsis: '{row_text}'"
    );
    assert!(
        !row_text.contains("message here"),
        "End of long message must be truncated"
    );
}

#[test]
fn test_adversarial_inline_diagnostic_empty_buffer_placement_and_style() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 10, config).unwrap();
    harness.new_buffer().unwrap();
    harness.render().unwrap();

    let msg = "empty buffer diagnostic";
    harness
        .apply_event(diagnostic_overlay(0..0, 100, msg))
        .unwrap();
    harness.render().unwrap();

    let (diag_x, diag_row) = harness
        .find_text_on_screen(msg)
        .expect("Diagnostic must appear on empty buffer");
    assert_eq!(
        diag_x, 2,
        "Diagnostic on empty buffer must begin at column 2 (2-space gap from start)"
    );

    // Empty buffer deliberately does not paint cursor line highlight (lines 718-722 of render_line/mod.rs)
    // so gap cell has the default editor background color.
    let gap_cell = harness.get_cell_style(0, diag_row).unwrap();
    assert_eq!(
        gap_cell.bg,
        Some(Color::Rgb(0, 0, 0)),
        "Gap cell on empty buffer should have editor_bg"
    );

    let diag_cell = harness.get_cell_style(diag_x, diag_row).unwrap();
    assert_eq!(
        diag_cell.fg,
        Some(Color::Rgb(255, 80, 80)),
        "Diagnostic pill on empty buffer must have error_fg"
    );
    assert_eq!(
        diag_cell.bg,
        Some(Color::Rgb(50, 0, 0)),
        "Diagnostic pill on empty buffer must have error_bg"
    );
}

fn run_palette_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn test_toggle_inline_diagnostics_global() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = false;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_temp_project_and_config(140, 15, config).unwrap();
    harness.new_buffer().unwrap();
    let code = "let val = 42;";
    harness.type_text(code).unwrap();
    harness.render().unwrap();

    let diag_msg = "error: type mismatch";
    harness
        .apply_event(diagnostic_overlay(0..10, 100, diag_msg))
        .unwrap();
    harness.render().unwrap();

    // Inline diagnostics is initially false -> diagnostic message not rendered
    harness.assert_screen_not_contains(diag_msg);

    // Toggle via command palette
    run_palette_command(&mut harness, "Toggle Inline Diagnostics");

    // Setting flipped globally and persisted to config
    assert!(harness.editor().config().editor.diagnostics_inline_text);
    harness.assert_screen_contains("Inline diagnostics enabled");
    harness.assert_screen_contains(diag_msg);

    // Toggle again -> disabled
    run_palette_command(&mut harness, "Toggle Inline Diagnostics");
    assert!(!harness.editor().config().editor.diagnostics_inline_text);
    harness.assert_screen_contains("Inline diagnostics disabled");
    harness.assert_screen_not_contains(diag_msg);
}

#[test]
fn test_toggle_inline_diagnostics_current_buffer() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = false;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_temp_project_and_config(140, 15, config).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    let file_a = dir.join("file_a.rs");
    let file_b = dir.join("file_b.rs");
    std::fs::write(&file_a, "let a = 1;\n").unwrap();
    std::fs::write(&file_b, "let b = 2;\n").unwrap();

    harness.open_file(&file_a).unwrap();
    harness.render().unwrap();
    harness
        .apply_event(diagnostic_overlay(0..5, 100, "diagnostic in file A"))
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("diagnostic in file A");

    harness.open_file(&file_b).unwrap();
    harness.render().unwrap();
    harness
        .apply_event(diagnostic_overlay(0..5, 100, "diagnostic in file B"))
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("diagnostic in file B");

    // Toggle current buffer on file B
    run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
    harness.assert_screen_contains("Inline diagnostics enabled (this buffer)");
    harness.assert_screen_contains("diagnostic in file B");

    // Global config is still false
    assert!(!harness.editor().config().editor.diagnostics_inline_text);

    // Switch back to file A
    harness.open_file(&file_a).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("let a = 1;");
    // File A must NOT show diagnostics because it has no override and global is false
    harness.assert_screen_not_contains("diagnostic in file A");
    harness.assert_screen_not_contains("diagnostic in file B");

    // Switch back to file B
    harness.open_file(&file_b).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("let b = 2;");
    harness.assert_screen_contains("diagnostic in file B");
}

#[test]
fn test_reset_buffer_settings_clears_inline_diagnostics_override() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_temp_project_and_config(140, 15, config).unwrap();
    harness.new_buffer().unwrap();
    harness.type_text("let x = 10;").unwrap();
    harness.render().unwrap();

    let diag_msg = "type inference error";
    harness
        .apply_event(diagnostic_overlay(0..5, 100, diag_msg))
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(diag_msg);

    // Override on current buffer to disable inline diagnostics
    run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
    harness.assert_screen_contains("Inline diagnostics disabled (this buffer)");
    harness.assert_screen_not_contains(diag_msg);

    // Reset buffer settings
    run_palette_command(&mut harness, "Reset Buffer Settings");
    harness.render().unwrap();

    // Reverts to global config (which is true)
    harness.assert_screen_contains(diag_msg);
}

#[test]
fn test_workspace_file_state_persists_inline_diagnostics_override() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("sample.rs");
    std::fs::write(&file, "let test_val = 123;\n").unwrap();

    let diag_msg = "sample diagnostic message";

    // Session 1: global is false, buffer override is set to true
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = false;
        config.editor.line_numbers = false;

        let mut harness =
            EditorTestHarness::with_config_and_working_dir(140, 15, config, project_dir.clone())
                .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..8, 100, diag_msg))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_not_contains(diag_msg);

        // Turn on for current buffer
        run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
        harness.assert_screen_contains(diag_msg);

        // Save workspace
        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore workspace, buffer override should be restored
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = false;
        config.editor.line_numbers = false;

        let mut harness =
            EditorTestHarness::with_config_and_working_dir(140, 15, config, project_dir.clone())
                .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();

        // Applying overlay renders inline because buffer override is preserved as true
        harness
            .apply_event(diagnostic_overlay(0..8, 100, diag_msg))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains(diag_msg);
    }
}

#[test]
fn test_adversarial_workspace_persistence_multi_buffer_and_legacy_compatibility() {
    use fresh::workspace::SerializedFileState;
    use tempfile::TempDir;

    // 1. Legacy deserialization check: absence of `diagnostics_inline_text` must default to None
    let legacy_json = r#"{
        "cursor": {"position": 0, "anchor": null, "sticky_column": 0},
        "additional_cursors": [],
        "scroll": {"top_byte": 0, "top_view_line_offset": 0, "left_column": 0},
        "view_mode": "Source"
    }"#;
    let legacy_state: SerializedFileState = serde_json::from_str(legacy_json).unwrap();
    assert_eq!(legacy_state.diagnostics_inline_text, None);

    // Serialization with None must omit the field (skip_serializing_if = "Option::is_none")
    let serialized_legacy = serde_json::to_string(&legacy_state).unwrap();
    assert!(!serialized_legacy.contains("diagnostics_inline_text"));

    // Serialization with Some(true) and Some(false)
    let mut state_true = legacy_state.clone();
    state_true.diagnostics_inline_text = Some(true);
    let serialized_true = serde_json::to_string(&state_true).unwrap();
    assert!(serialized_true.contains("\"diagnostics_inline_text\":true"));

    let mut state_false = legacy_state.clone();
    state_false.diagnostics_inline_text = Some(false);
    let serialized_false = serde_json::to_string(&state_false).unwrap();
    assert!(serialized_false.contains("\"diagnostics_inline_text\":false"));

    // 2. Full multi-buffer workspace persistence across restarts
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file_a = project_dir.join("file_a.rs");
    let file_b = project_dir.join("file_b.rs");
    let file_c = project_dir.join("file_c.rs");
    std::fs::write(&file_a, "let a = 100;\n").unwrap();
    std::fs::write(&file_b, "let b = 200;\n").unwrap();
    std::fs::write(&file_c, "let c = 300;\n").unwrap();

    let diag_a = "error in file a";
    let diag_b = "error in file b";
    let diag_c = "error in file c";

    // Session 1:
    // Global config: diagnostics_inline_text = false
    // File A: turned ON -> Some(true)
    // File B: untouched -> None
    // File C: turned ON then OFF -> Some(false)
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = false;
        config.editor.line_numbers = false;

        let mut harness =
            EditorTestHarness::with_config_and_working_dir(140, 15, config, project_dir.clone())
                .unwrap();

        harness.open_file(&file_a).unwrap();
        harness.render().unwrap();
        run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
        harness.assert_screen_contains("Inline diagnostics enabled (this buffer)");

        harness.open_file(&file_b).unwrap();
        harness.render().unwrap();
        // File B is left untouched (override = None)

        harness.open_file(&file_c).unwrap();
        harness.render().unwrap();
        // Toggle on then off to make it explicitly Some(false)
        run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
        harness.assert_screen_contains("Inline diagnostics enabled (this buffer)");
        run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
        harness.assert_screen_contains("Inline diagnostics disabled (this buffer)");

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: Global is FALSE
    // File A (override Some(true)) -> must show diagnostics
    // File B (override None, global false) -> must NOT show diagnostics
    // File C (override Some(false)) -> must NOT show diagnostics
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = false;
        config.editor.line_numbers = false;

        let mut harness =
            EditorTestHarness::with_config_and_working_dir(140, 15, config, project_dir.clone())
                .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace must be restored");

        // Test File A
        harness.open_file(&file_a).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..5, 100, diag_a))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains(diag_a);

        // Test File B
        harness.open_file(&file_b).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..5, 100, diag_b))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_not_contains(diag_b);

        // Test File C
        harness.open_file(&file_c).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..5, 100, diag_c))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_not_contains(diag_c);
    }

    // Session 3: Global is TRUE
    // File A (override Some(true)) -> must show diagnostics
    // File B (override None, global true) -> must SHOW diagnostics (proves it respects global!)
    // File C (override Some(false)) -> must NOT show diagnostics (proves override survives global change!)
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = true;
        config.editor.line_numbers = false;

        let mut harness =
            EditorTestHarness::with_config_and_working_dir(140, 15, config, project_dir.clone())
                .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace must be restored");

        // Test File A
        harness.open_file(&file_a).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..5, 100, diag_a))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains(diag_a);

        // Test File B
        harness.open_file(&file_b).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..5, 100, diag_b))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains(diag_b);

        // Test File C
        harness.open_file(&file_c).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..5, 100, diag_c))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_not_contains(diag_c);
    }
}

#[test]
fn test_adversarial_inline_diagnostics_global_and_per_buffer_interaction() {
    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = false;
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_temp_project_and_config(140, 15, config).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    let file_a = dir.join("file_a.rs");
    let file_b = dir.join("file_b.rs");
    std::fs::write(&file_a, "let a = 1;\n").unwrap();
    std::fs::write(&file_b, "let b = 2;\n").unwrap();

    let diag_a = "diagnostic file A";
    let diag_b = "diagnostic file B";

    // Open file A, global is false -> no diagnostic
    harness.open_file(&file_a).unwrap();
    harness.render().unwrap();
    harness
        .apply_event(diagnostic_overlay(0..5, 100, diag_a))
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains(diag_a);

    // Toggle per-buffer on file A -> shows diagnostic
    run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
    harness.assert_screen_contains(diag_a);

    // Open file B, global is false -> no diagnostic
    harness.open_file(&file_b).unwrap();
    harness.render().unwrap();
    harness
        .apply_event(diagnostic_overlay(0..5, 100, diag_b))
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains(diag_b);

    // Toggle per-buffer on file B twice: true, then false (explicit override Some(false))
    run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
    harness.assert_screen_contains(diag_b);
    run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
    harness.assert_screen_not_contains(diag_b);

    // Global toggle executed while file B is active:
    // Global flips to true, and file B's active override Some(false) is cleared to None!
    run_palette_command(&mut harness, "Toggle Inline Diagnostics");
    assert!(harness.editor().config().editor.diagnostics_inline_text);
    // File B now shows diagnostic because global is true and its override was cleared
    harness.assert_screen_contains(diag_b);

    // Switch to file A:
    // File A was not active during global toggle, so its override Some(true) remained untouched
    harness.open_file(&file_a).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(diag_a);

    // Now global toggle while file A is active:
    // Global flips to false, and file A's active override Some(true) is cleared to None!
    run_palette_command(&mut harness, "Toggle Inline Diagnostics");
    assert!(!harness.editor().config().editor.diagnostics_inline_text);
    // File A hides diagnostic because global is false and override is cleared
    harness.assert_screen_not_contains(diag_a);

    // Switch to file B: file B had override None, so it follows global false
    harness.open_file(&file_b).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains(diag_b);
}

#[test]
fn test_adversarial_workspace_persistence_all_states() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file1 = project_dir.join("file1.rs");
    let file2 = project_dir.join("file2.rs");
    let file3 = project_dir.join("file3.rs");
    std::fs::write(&file1, "let f1 = 1;\n").unwrap();
    std::fs::write(&file2, "let f2 = 2;\n").unwrap();
    std::fs::write(&file3, "let f3 = 3;\n").unwrap();

    let diag1 = "diagnostic on f1";
    let diag2 = "diagnostic on f2";
    let diag3 = "diagnostic on f3";

    // Session 1:
    // Global = false
    // file1: no toggle -> override None
    // file2: toggle -> override Some(true)
    // file3: toggle twice -> override Some(false)
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = false;
        config.editor.line_numbers = false;

        let mut harness =
            EditorTestHarness::with_config_and_working_dir(140, 15, config, project_dir.clone())
                .unwrap();

        harness.open_file(&file1).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..6, 100, diag1))
            .unwrap();

        harness.open_file(&file2).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..6, 100, diag2))
            .unwrap();
        run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
        harness.assert_screen_contains(diag2);

        harness.open_file(&file3).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..6, 100, diag3))
            .unwrap();
        // Toggle once -> true
        run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
        harness.assert_screen_contains(diag3);
        // Toggle again -> false
        run_palette_command(&mut harness, "Toggle Inline Diagnostics (Current Buffer)");
        harness.assert_screen_not_contains(diag3);

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore with global = true
    // file1 has None -> follows global true -> shows diag1
    // file2 has Some(true) -> shows diag2
    // file3 has Some(false) -> hides diag3
    {
        let mut config = fresh::config::Config::default();
        config.editor.diagnostics_inline_text = true;
        config.editor.line_numbers = false;

        let mut harness =
            EditorTestHarness::with_config_and_working_dir(140, 15, config, project_dir.clone())
                .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should restore successfully");

        harness.open_file(&file1).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..6, 100, diag1))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains(diag1);

        harness.open_file(&file2).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..6, 100, diag2))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains(diag2);

        harness.open_file(&file3).unwrap();
        harness.render().unwrap();
        harness
            .apply_event(diagnostic_overlay(0..6, 100, diag3))
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_not_contains(diag3);

        // On file3, reset buffer settings -> reverts Some(false) to None -> inherits global true -> shows diag3
        run_palette_command(&mut harness, "Reset Buffer Settings");
        harness.render().unwrap();
        harness.assert_screen_contains(diag3);
    }
}
