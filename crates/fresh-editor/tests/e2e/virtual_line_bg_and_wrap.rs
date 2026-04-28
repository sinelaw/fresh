//! Reproducers for two visible bugs in the live-diff "deleted line"
//! visualization that are actually defects in the renderer's handling of
//! virtual lines (`LineAbove` / `LineBelow`):
//!
//! 1. A virtual line whose `Style` has a `bg` paints that bg only on the
//!    cells covered by the literal text — trailing cells of that visual
//!    row stay default-bg. The plugin can't make the stripe reach the
//!    viewport edge from the JS side because virtual lines have no source
//!    bytes for the overlay sweep to touch, and the `extend_to_line_end`
//!    fill path is gated on `byte_pos.is_some()`.
//!
//! 2. A virtual line whose text is wider than the viewport's content area
//!    is truncated rather than soft-wrapped to additional visual rows,
//!    even when the buffer has `line_wrap = true`. Long deleted source
//!    lines therefore disappear off the right edge.

use crate::common::harness::EditorTestHarness;
use fresh::view::virtual_text::{VirtualTextNamespace, VirtualTextPosition};
use ratatui::style::{Color, Style};
use tempfile::TempDir;

/// Bug 1: the bg color on a `LineAbove` virtual line should fill the
/// trailing cells of its visual row, not stop at the end of the literal
/// text. Live-diff renders deleted-line virtual content in red; without
/// the fix the user sees red only behind the text and default-bg to
/// the right.
#[test]
fn virtual_line_bg_fills_to_viewport_edge() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let red = Color::Rgb(180, 30, 30);
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            7,
            "DELETED".to_string(),
            Style::default().fg(Color::White).bg(red),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("repro".to_string()),
            0,
        );
    }
    harness.render().unwrap();

    let buf = harness.buffer();
    let mut hit_row: Option<u16> = None;
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if row.contains("DELETED") {
            hit_row = Some(y);
            break;
        }
    }
    let row = hit_row.expect("did not find virtual line on screen");

    // Cell well past 'DELETED' but inside the content area.
    let trailing_cell = &buf[(60, row)];
    let bg = trailing_cell.style().bg;
    assert_eq!(
        bg,
        Some(red),
        "trailing cells of the virtual-line row should also have the \
         virtual line's red bg; saw {:?}",
        bg,
    );
}

/// Bug 2: A long virtual line (wider than the viewport's content area)
/// should soft-wrap to additional visual rows, just like a long source
/// line does under `line_wrap = true`. Today it's emitted as a single
/// `ViewLine` with the entire text and only the first viewport-width
/// chars are visible — the rest disappear.
///
/// Marked `#[ignore]` because it reproduces a known renderer
/// limitation — `inject_virtual_lines` produces one ViewLine per
/// virtual entry without splitting at the wrap width. Drop the
/// `#[ignore]` when the renderer wraps long virtual lines.
#[test]
#[ignore = "known-failing repro: long virtual line is truncated, not wrapped"]
fn long_virtual_line_wraps_under_line_wrap_default() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();

    let mut harness = EditorTestHarness::new(40, 24).unwrap();
    assert!(
        harness.config().editor.line_wrap,
        "expects default line_wrap=true"
    );
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Text intentionally wider than the 40-col viewport so wrap is required.
    // Two distinct halves let us look for both before and after the wrap.
    let head = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"; // 32 'A's
    let tail = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"; // 32 'B's
    let long = format!("{head}{tail}");

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            7,
            long.clone(),
            Style::default().fg(Color::White),
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("repro".to_string()),
            0,
        );
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(head),
        "first half of the virtual line should be visible. Screen:\n{screen}",
    );
    assert!(
        screen.contains(tail),
        "second half of the virtual line should also be visible \
         (soft-wrapped to a continuation visual row); saw screen:\n{screen}",
    );
}
