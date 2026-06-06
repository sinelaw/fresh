//! Regression tests for inlay-hint layout under line wrapping and
//! horizontal scrolling.
//!
//! Inlay hints are inline virtual text (e.g. `: i32` type annotations).
//! Historically they were drawn only at render time — *after* line
//! wrapping and *after* the per-character visual-column map was built —
//! so their on-screen width was invisible to:
//!
//!   1. **Line wrapping.** A wrapped row whose source content already
//!      filled the content width would, once the hint was injected,
//!      overflow the right edge. The renderer clipped the overflow, so
//!      real source characters silently vanished from the display.
//!   2. **Horizontal scrolling.** Scroll-follow computed the cursor's
//!      column from source text only. A hint sitting between the left
//!      edge and the cursor pushed the line's tail past the right edge,
//!      but the scroll math never compensated, leaving the end of the
//!      line clipped even with the cursor parked on it.
//!
//! The fix splices inline hints into the token stream *before* wrapping,
//! so wrap boundaries, the visual-column map, cursor math, and
//! horizontal scroll all share one canonical cell layout. These tests
//! pin both behaviours.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::app::Editor;
use fresh::config::Config;
use lsp_types::{InlayHint, InlayHintLabel, Position};

/// Build a `String`-label inlay hint at an LSP (line, character) position.
fn string_hint(line: u32, character: u32, label: &str) -> InlayHint {
    InlayHint {
        position: Position { line, character },
        label: InlayHintLabel::String(label.to_string()),
        kind: None,
        text_edits: None,
        tooltip: None,
        padding_left: None,
        padding_right: None,
        data: None,
    }
}

fn wrap_config() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

#[test]
fn wrapping_with_inlay_hints_drops_no_source_characters() {
    // A single logical line of 120 'X'. With wrap on at width 40 it
    // occupies a handful of visual rows. An inlay hint near the start
    // widens the row it lands on; pre-fix that hint was invisible to
    // wrapping, so the row overflowed the content width and the renderer
    // clipped real 'X' characters off the right edge — silently dropping
    // them from the display. After the fix, wrap boundaries account for
    // the hint width, so every source 'X' is still drawn on some row.
    const N: usize = 120;
    let source = "X".repeat(N);

    let mut harness = EditorTestHarness::with_config(40, 20, wrap_config()).unwrap();
    harness.load_buffer_from_text(&source).unwrap();

    // Hint built from distinct '#' glyphs (never 'X') a few columns in,
    // so it lands mid first-row and pushes the row past the edge pre-fix.
    let hint = string_hint(0, 5, "##########");
    Editor::apply_inlay_hints_to_state(harness.editor_mut().active_state_mut(), &[hint]);
    harness.render().unwrap();

    let (first, last) = harness.content_area_rows();
    let visible_x: usize = (first..=last)
        .map(|r| harness.get_row_text(r as u16).matches('X').count())
        .sum();

    assert_eq!(
        visible_x,
        N,
        "every source 'X' must remain visible across the wrapped rows when an \
         inlay hint shares a row; got {visible_x} of {N}.\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

#[test]
fn horizontal_scroll_reveals_end_of_line_with_inlay_hints() {
    // No wrap. A line longer than the viewport that ends in a unique
    // sentinel, with a wide inlay hint inserted just before the sentinel
    // (i.e. inside the horizontally-visible window when the cursor is at
    // end of line). Pre-fix, scroll-follow used the cursor's source
    // column only; the hint pushed the sentinel past the right edge but
    // the viewport never scrolled far enough to compensate, so the
    // sentinel stayed clipped. After the fix the cursor's true column
    // includes the hint width, so the viewport scrolls to reveal the end.
    let source = format!("{}END_SENTINEL", "X".repeat(60));

    let mut harness = EditorTestHarness::new_no_wrap(40, 20).unwrap();
    harness.load_buffer_from_text(&source).unwrap();

    // A wide hint ('::' glyphs, distinct from the content) placed at
    // column 55 — among the trailing 'X', before the sentinel at 60.
    let hint = string_hint(0, 55, "::::::::::::::::::::::::");
    Editor::apply_inlay_hints_to_state(harness.editor_mut().active_state_mut(), &[hint]);

    // Park the cursor at end of line; horizontal scroll follows it.
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(
        harness.find_text_on_screen("END_SENTINEL").is_some(),
        "with the cursor at end of a hinted line, horizontal scroll must reveal \
         the end of the line.\nScreen:\n{}",
        harness.screen_to_string(),
    );
}
