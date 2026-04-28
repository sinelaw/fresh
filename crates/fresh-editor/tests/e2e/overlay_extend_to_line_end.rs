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
/// edge of the content area.
#[test]
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

/// Empty source lines — the line user pressed Enter on but hasn't typed
/// anything in yet — should also fill with the overlay's bg when there
/// is an `extend_to_line_end` overlay covering that line. Live-diff
/// users see "skipped" rows in the middle of an added block (one
/// blank line is left default-bg while the lines around it are green).
///
/// The plugin emits a zero-width overlay for empty lines, but the
/// renderer's overlay sweep is driven by `byte_pos` from visible chars
/// — there are none on an empty line — so the trailing fill never
/// fires for empty source lines.
#[test]
fn overlay_extend_to_line_end_fills_empty_source_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // Two non-empty lines surrounding one empty line.
    harness.type_text("aa\n\nbb").unwrap();
    harness.render().unwrap();

    // Buffer is "aa\n\nbb" (6 bytes). Line 0 = "aa" (bytes 0..2).
    // Line 1 = "" (just the \n at byte 3). Line 2 = "bb" (bytes 4..6).
    // Add three overlays, one per line, mirroring how live_diff per-line
    // overlays look. Use `start..start+1` for the empty line so the
    // range is non-zero (covers the newline byte).
    for (start, end) in [(0_usize, 2_usize), (3, 4), (4, 6)] {
        harness
            .apply_event(Event::AddOverlay {
                namespace: Some(OverlayNamespace::from_string("repro".into())),
                range: start..end,
                face: OverlayFace::Background { color: (0, 80, 0) },
                priority: 50,
                message: None,
                extend_to_line_end: true,
                url: None,
            })
            .unwrap();
    }
    harness.render().unwrap();

    // Find the rows for "aa" and "bb".
    let buf = harness.buffer();
    let mut row_aa: Option<u16> = None;
    let mut row_bb: Option<u16> = None;
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if row.contains("aa") && row_aa.is_none() {
            row_aa = Some(y);
        }
        if row.contains("bb") && row_bb.is_none() {
            row_bb = Some(y);
        }
    }
    let row_aa = row_aa.expect("could not find 'aa' on screen");
    let row_bb = row_bb.expect("could not find 'bb' on screen");
    let empty_row = row_aa + 1;
    assert!(
        empty_row < row_bb,
        "expected empty line between aa ({row_aa}) and bb ({row_bb})",
    );

    // The trailing cell on each row — including the empty middle row —
    // should have the green bg.
    for (label, y) in [("aa", row_aa), ("empty", empty_row), ("bb", row_bb)] {
        let bg = buf[(40, y)].style().bg;
        assert_eq!(
            bg,
            Some(Color::Rgb(0, 80, 0)),
            "row '{label}' (y={y}): trailing cell should have the overlay's \
             green bg; saw {bg:?}",
        );
    }
}
