use crate::common::harness::EditorTestHarness;
use fresh::model::event::{Event, OverlayFace};
use fresh::view::overlay::OverlayNamespace;

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
