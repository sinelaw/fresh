//! Test that syntax highlighting works for embedded languages (CSS inside HTML)
//! even when the viewport is far from the embedding tag.
//!
//! The fixture `embedded_css_long.html` has ~400 CSS rules inside a `<style>` block
//! (21KB), with `.target-rule` CSS at line 405. The `<style>` tag is at byte ~60.
//! The default `context_bytes` is 10KB, so jumping to line 405 requires parse state
//! checkpoints to preserve the embedded CSS context.

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

fn create_harness() -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap()
}

fn goto_line(harness: &mut EditorTestHarness, line: usize) {
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(&line.to_string()).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Jump directly to line 405 (>10KB from `<style>` tag). Checkpoints must be
/// built from byte 0 to preserve embedded CSS context.
#[test]
fn test_embedded_css_highlighting_at_large_offset() {
    let path = fixture_path("embedded_css_long.html");
    assert!(path.exists(), "Fixture not found: {}", path.display());

    let mut harness = create_harness();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // Sanity check: highlighting works at top
    let top_colors = collect_highlight_colors(&harness, 2, 20);
    assert!(
        top_colors >= 2,
        "Sanity check: expected highlighting at top of file, got {} colors",
        top_colors
    );

    // Jump to the target CSS past the 10KB boundary
    goto_line(&mut harness, 405);

    harness.assert_screen_contains("display");
    harness.assert_screen_contains("background");

    let offset_colors = collect_highlight_colors(&harness, 2, 20);
    assert!(
        offset_colors >= 2,
        "CSS inside <style> at large offset (line 405, >10KB from <style> tag) \
         should have syntax highlighting, but got only {} distinct highlight colors. \
         This indicates the TextMate parser lost embedded language context.",
        offset_colors
    );
}

/// Scroll gradually to line 405 via PageDown. Checkpoints are built incrementally
/// as the viewport advances.
#[test]
fn test_embedded_css_highlighting_via_scrolling() {
    let path = fixture_path("embedded_css_long.html");
    let mut harness = create_harness();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // Scroll down with PageDown until we pass line 400
    // The terminal is 40 lines tall, ~36 content lines per page.
    // 405 / 36 ≈ 12 PageDowns to reach the target area.
    for _ in 0..13 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Should now show CSS content near line 400+
    let colors = collect_highlight_colors(&harness, 2, 20);
    assert!(
        colors >= 2,
        "CSS highlighting should work after gradual scrolling, got {} colors",
        colors
    );
}

/// Edit CSS content at line 405, verify highlighting survives cache invalidation.
#[test]
fn test_embedded_css_highlighting_after_edit() {
    let path = fixture_path("embedded_css_long.html");
    let mut harness = create_harness();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // Jump to the CSS target area
    goto_line(&mut harness, 405);

    let colors_before = collect_highlight_colors(&harness, 2, 20);
    assert!(
        colors_before >= 2,
        "Pre-edit: expected CSS highlighting, got {} colors",
        colors_before
    );

    // Type some CSS text (this triggers invalidate_range on the buffer)
    harness
        .send_key(KeyCode::End, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("            color: green;").unwrap();
    harness.render().unwrap();

    // Highlighting should still work after the edit
    let colors_after = collect_highlight_colors(&harness, 2, 20);
    assert!(
        colors_after >= 2,
        "Post-edit: CSS highlighting should survive cache invalidation, got {} colors",
        colors_after
    );
}

/// Edit HTML before the `<style>` tag, then return to the CSS area.
/// This tests that checkpoint invalidation (all checkpoints discarded because
/// the edit is before them) correctly rebuilds parse state.
#[test]
fn test_embedded_css_highlighting_after_edit_before_style() {
    let path = fixture_path("embedded_css_long.html");
    let mut harness = create_harness();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // First, jump to line 405 to build checkpoints
    goto_line(&mut harness, 405);
    let colors_initial = collect_highlight_colors(&harness, 2, 20);
    assert!(
        colors_initial >= 2,
        "Initial: expected CSS highlighting, got {} colors",
        colors_initial
    );

    // Go to line 1 (before <style> tag) and insert a line.
    // This invalidates ALL checkpoints since the edit is at byte ~0.
    goto_line(&mut harness, 1);
    harness
        .send_key(KeyCode::End, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("<!-- inserted -->").unwrap();
    harness.render().unwrap();

    // Return to the CSS area (now line 406 due to insertion)
    goto_line(&mut harness, 406);

    let colors_after = collect_highlight_colors(&harness, 2, 20);
    assert!(
        colors_after >= 2,
        "After editing before <style> tag, CSS highlighting should still work \
         (checkpoints rebuilt from byte 0), got {} colors",
        colors_after
    );
}

/// Verify highlighting at the top of the file still works (regression guard).
#[test]
fn test_highlighting_near_top_still_works() {
    let path = fixture_path("embedded_css_long.html");
    let mut harness = create_harness();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // The top of the file has HTML + the opening of the <style> block with CSS
    let colors = collect_highlight_colors(&harness, 2, 20);
    assert!(
        colors >= 2,
        "Highlighting at top of file should work, got {} colors",
        colors
    );
}
