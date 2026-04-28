//! Reproducer: an overlay with `extend_to_line_end: true` should fill the
//! trailing cells of a non-wrapping line with the overlay's background,
//! even when the editor's `line_wrap` setting is `true` (the default).
//!
//! Today the renderer's fill code is gated on `!line_wrap`, so the
//! trailing cells stay default-bg. The test below documents that
//! behaviour as a failing assertion until the renderer is fixed.

use crate::common::harness::EditorTestHarness;
use fresh::model::event::{Event, OverlayFace};
use fresh::view::overlay::OverlayNamespace;
use ratatui::style::Color;

/// With the default config (`line_wrap = true`) and a short line that
/// never visually wraps, an overlay covering the line content with
/// `extend_to_line_end: true` should paint its bg out to the right
/// edge of the content area. Today only the cells covered by the
/// overlay's byte range are painted; the trailing cells stay default.
///
/// Marked `#[ignore]` because it documents a known renderer bug — the
/// fill code at `render_line.rs:1086` is gated on `!line_wrap`, so the
/// trailing-fill never runs under the default config. Drop the
/// `#[ignore]` once the renderer is updated to honour
/// `extend_to_line_end` in line-wrap mode.
#[test]
#[ignore = "known-failing repro: extend_to_line_end gated on !line_wrap in renderer"]
fn overlay_extend_to_line_end_fills_under_default_line_wrap() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    assert!(
        harness.config().editor.line_wrap,
        "this test asserts behaviour under the default line_wrap=true config",
    );

    // Short content that won't wrap at width=80.
    harness.type_text("hi").unwrap();
    harness.render().unwrap();

    // Add an overlay covering "hi" with bright-green bg and extendToLineEnd.
    let event = Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("repro".into())),
        range: 0..2,
        face: OverlayFace::Background { color: (0, 80, 0) },
        priority: 50,
        message: None,
        extend_to_line_end: true,
        url: None,
    };
    harness.apply_event(event).unwrap();
    harness.render().unwrap();

    // Find the row that holds "hi".
    let buf = harness.buffer();
    let mut hit_row: Option<u16> = None;
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if row.contains("hi") {
            hit_row = Some(y);
            break;
        }
    }
    let row = hit_row.expect("could not find 'hi' on screen");

    // Pick a cell well past 'hi' but inside the content area. Width=80;
    // gutter is small; col 40 is comfortably past the content.
    let trailing_cell = &buf[(40, row)];
    let bg = trailing_cell.style().bg;

    // The fix: trailing cell should have the overlay's green bg.
    assert_eq!(
        bg,
        Some(Color::Rgb(0, 80, 0)),
        "expected trailing cell on the overlay row to have the overlay bg \
         (Rgb(0,80,0)) under default line_wrap=true; saw {:?}",
        bg,
    );
}
