//! Test that syntax highlighting works for embedded languages (CSS inside HTML)
//! even when the viewport is far from the embedding tag.
//!
//! Bug: When jumping to a line deep inside a `<style>` block, the TextMate/syntect
//! parser starts from `viewport_start - context_bytes` with a fresh `ParseState`.
//! If `context_bytes` (default 10KB) isn't enough to reach back to the `<style>` tag,
//! syntect doesn't know it's in CSS context and produces zero highlight spans.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;
use std::path::PathBuf;

fn fixture_path(filename: &str) -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest_dir)
        .join("tests/fixtures/syntax_highlighting")
        .join(filename)
}

/// Collect distinct non-default foreground colors from the content area of the screen.
fn collect_highlight_colors(harness: &EditorTestHarness, row_start: u16, row_end: u16) -> usize {
    let mut colors = std::collections::HashSet::new();
    for y in row_start..row_end {
        for x in 8..100 {
            if let Some(style) = harness.get_cell_style(x, y) {
                if let Some(fg) = style.fg {
                    match fg {
                        Color::Indexed(15) => {}  // default white text
                        Color::Indexed(244) => {} // line numbers
                        Color::Indexed(237) => {} // tilde empty lines
                        Color::Indexed(0) => {}   // black
                        Color::Indexed(236) => {} // dark gray UI
                        Color::Reset => {}
                        _ => {
                            colors.insert(format!("{:?}", fg));
                        }
                    }
                }
            }
        }
    }
    colors.len()
}

/// Verify that CSS inside a `<style>` tag is highlighted even when the viewport
/// is more than `context_bytes` (10KB) away from the opening `<style>` tag.
///
/// The fixture `embedded_css_long.html` has ~400 CSS rules inside a `<style>` block,
/// pushing the `.target-rule` CSS past byte 21,000. When jumping directly to that
/// line, the parser must still produce syntax highlighting for the CSS properties.
#[test]
fn test_embedded_css_highlighting_at_large_offset() {
    let path = fixture_path("embedded_css_long.html");
    assert!(path.exists(), "Fixture not found: {}", path.display());

    let mut harness = EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();

    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // Verify highlighting works at the top of the file (sanity check)
    let top_colors = collect_highlight_colors(&harness, 2, 20);
    assert!(
        top_colors >= 2,
        "Sanity check: expected highlighting at top of file, got {} colors",
        top_colors
    );

    // Jump to line 405 where .target-rule CSS is located (past 10KB context_bytes boundary)
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("405").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The screen should show CSS content around line 405
    harness.assert_screen_contains("display");
    harness.assert_screen_contains("background");

    // Verify syntax highlighting is applied to the CSS content.
    // CSS properties like "display", "background", "border-radius" should have
    // highlight colors (not plain white).
    let offset_colors = collect_highlight_colors(&harness, 2, 20);
    assert!(
        offset_colors >= 2,
        "CSS inside <style> at large offset (line 405, >10KB from <style> tag) \
         should have syntax highlighting, but got only {} distinct highlight colors. \
         This indicates the TextMate parser lost embedded language context.",
        offset_colors
    );
}
