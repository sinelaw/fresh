//! Migration of `tests/e2e/virtual_line_bg_and_wrap.rs` — two
//! renderer defects on virtual lines (`LineAbove` / `LineBelow`).
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Background fill to viewport edge.** A virtual line whose
//!      `Style` has a `bg` paints that bg across the *entire* visual
//!      row, not just the cells under the literal text. Live-diff's
//!      red "deleted line" stripe depends on this — without the
//!      fill, the user sees red only behind the text and default-bg
//!      to the right. The `extend_to_line_end` fill path used to be
//!      gated on `byte_pos.is_some()`, which virtual lines never
//!      satisfy.
//!
//!   2. **Long virtual line soft-wraps.** A virtual line whose text
//!      is wider than the viewport's content area must soft-wrap to
//!      additional visual rows under `line_wrap = true` (the
//!      default), just like a long source line does. Fixed by
//!      splitting the virtual text by display width inside
//!      `inject_virtual_lines`.
//!
//! ## Harness-direct pattern
//!
//! Virtual lines are injected through the plugin-internal
//! `state.virtual_texts.add_line(...)` path (the same pattern
//! `migrated_virtual_lines.rs` uses), and bg-cell inspection probes
//! the rendered ratatui buffer's per-cell styles via `harness.buffer()`
//! — neither has an `EditorTestApi` projection. Both are permitted
//! under the harness-direct exemption in
//! `scripts/check-semantic-test-isolation.sh`.
//!
//! Source: `tests/e2e/virtual_line_bg_and_wrap.rs` (2 tests
//! migrated; no tests deferred).

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{
    RenderSnapshot, RenderSnapshotExpect, RowMatch,
};
use fresh::view::virtual_text::{VirtualTextNamespace, VirtualTextPosition};
use ratatui::style::{Color, Style};

fn ns(name: &str) -> VirtualTextNamespace {
    VirtualTextNamespace::from_string(name.to_string())
}

#[test]
fn migrated_virtual_line_bg_fills_to_viewport_edge() {
    // Original: `virtual_line_bg_fills_to_viewport_edge`.
    let fixture =
        TestFixture::new("virtual_line_bg_fill.txt", "Line 1\nLine 2\nLine 3").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    let red = Color::Rgb(180, 30, 30);
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            7, // byte offset of "Line 2"
            "DELETED".to_string(),
            Style::default().fg(Color::White).bg(red),
            VirtualTextPosition::LineAbove,
            ns("repro"),
            0,
        );
    }
    harness.render().unwrap();

    // Locate the row carrying the virtual-line text.
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

    // Cell well past "DELETED" but inside the content area. The bg
    // must still be the virtual line's red.
    let trailing_cell = &buf[(60, row)];
    let bg = trailing_cell.style().bg;
    assert_eq!(
        bg,
        Some(red),
        "trailing cells of the virtual-line row should also have the \
         virtual line's red bg; saw {bg:?}",
    );
}

#[test]
fn migrated_long_virtual_line_wraps_under_line_wrap_default() {
    // Original: `long_virtual_line_wraps_under_line_wrap_default`.
    let fixture =
        TestFixture::new("virtual_line_wrap.txt", "Line 1\nLine 2\nLine 3").unwrap();
    let mut harness = EditorTestHarness::new(40, 24).unwrap();
    assert!(
        harness.config().editor.line_wrap,
        "expects default line_wrap=true"
    );
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Virtual text wider than the 40-col viewport so wrap is forced.
    // Two distinct halves let us assert both before and after the
    // wrap appear on screen.
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
            ns("repro"),
            0,
        );
    }
    harness.render().unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::AnyRowContains(head.to_string()),
            RowMatch::AnyRowContains(tail.to_string()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "long virtual line should soft-wrap to a continuation \
             visual row under line_wrap=true: {f} expected {e}; \
             actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

/// Anti-test: drop the `add_line` for the bg-fill scenario. Without
/// injecting the virtual line, no row carries the "DELETED" text,
/// so the cell at (60, hit_row) cannot have the red bg — proves
/// the bg-fill claim depends on the actual `add_line` dispatch,
/// not on the harness or buffer accidentally producing red cells.
#[test]
fn anti_virtual_line_bg_without_add_line_has_no_red_trailing_cell() {
    let fixture = TestFixture::new(
        "virtual_line_bg_fill_anti.txt",
        "Line 1\nLine 2\nLine 3",
    )
    .unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    // No add_line call here — that's the load-bearing step we drop.

    let red = Color::Rgb(180, 30, 30);
    let buf = harness.buffer();
    // No row should contain "DELETED".
    let mut found = false;
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if row.contains("DELETED") {
            found = true;
            break;
        }
    }
    assert!(
        !found,
        "anti: without add_line, no row should carry the 'DELETED' \
         virtual-line text"
    );
    // And no cell in the content area should be painted with the
    // virtual-line's red bg.
    let mut red_cells = 0usize;
    for y in 0..buf.area.height {
        for x in 0..buf.area.width {
            if buf[(x, y)].style().bg == Some(red) {
                red_cells += 1;
            }
        }
    }
    assert_eq!(
        red_cells, 0,
        "anti: without the virtual line's add_line, no cell should \
         carry the virtual-line red bg ({red_cells} cells were red)"
    );
}

/// Anti-test: drop the long-text `add_line`. Without it, neither
/// the head nor the tail sentinel may appear — proves the wrap
/// claim depends on actually injecting the long virtual text.
#[test]
fn anti_long_virtual_line_without_add_line_renders_no_sentinels() {
    let fixture =
        TestFixture::new("virtual_line_wrap_anti.txt", "Line 1\nLine 2\nLine 3").unwrap();
    let mut harness = EditorTestHarness::new(40, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    // No add_line call here.

    let head = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
    let tail = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::NoRowContains(head.to_string()),
            RowMatch::NoRowContains(tail.to_string()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "anti: without add_line, neither the head nor tail \
             sentinel should appear: {f} expected {e}; actual {a}\n\
             rows={:#?}",
            snap.rendered_rows
        );
    }
}
