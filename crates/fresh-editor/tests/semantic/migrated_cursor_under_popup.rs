//! DECLARATIVE: Migration of `tests/e2e/cursor_under_popup.rs`.
//!
//! The hardware cursor must not show through a popup drawn over the
//! cell where the cursor would otherwise sit. Background: popups
//! are drawn on top of the buffer content, but the ratatui-managed
//! hardware cursor (`Frame::set_cursor_position`) is rendered by
//! the real terminal *on top of* every cell — including popup
//! cells. If the cursor position stays set when a popup has covered
//! that cell, the user sees the cursor blink through the popup. The
//! fix omits `Frame::set_cursor_position` for the frame when the
//! cursor would land inside any popup rect, so `Terminal::draw`
//! calls `hide_cursor`.
//!
//! Scenarios are pure data; the runner executes them. The popup is
//! injected declaratively via `LayoutScenario.show_popup` with the
//! [`PopupPlacement::AtHardwareCursorOffset`] placement (extension
//! added in this migration) — the runner resolves the offset
//! against the live hardware-cursor position at injection time, so
//! test data doesn't need to hard-code cell coordinates that depend
//! on gutter width or terminal geometry. The cursor-hidden-or-
//! outside-rect claim is expressed via
//! [`RenderSnapshotExpect::hardware_cursor_hidden_or_outside_rect`]
//! (extension added in this migration).
//!
//! Source: `tests/e2e/cursor_under_popup.rs` (1 test migrated + 1
//! anti-test; no tests deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, PopupPlacement, PopupSpec,
};
use crate::common::scenario::render_snapshot::{
    HardwareCursorRect, RenderSnapshotExpect, RowMatch,
};
use fresh::test_api::Action;

// Popup geometry mirrors the e2e: width 40 cells, with 3 content
// lines + 2 borders = 5 rows. Placed at (cursor.col - 2,
// cursor.row - 1) so the popup rect strictly contains the cursor
// cell — that's the condition the fix has to detect.
const POPUP_WIDTH: u16 = 40;
const POPUP_DX: i32 = -2;
const POPUP_DY: i32 = -1;
// Width covered = POPUP_WIDTH; height covered = 3 content + 2
// borders = 5. The cursor sits at offset (+2, +1) from the popup
// origin (we anchored at -2, -1), so it lands well inside.
const POPUP_RECT_H: u16 = 5;

/// `Action::InsertChar` sequence for "hello world". Typing via
/// actions (rather than via `initial_text`) is load-bearing here:
/// it moves the primary cursor to byte 11, which the
/// `AtHardwareCursorOffset` popup placement then anchors against
/// at injection time. Loading the same text via `initial_text`
/// would leave the cursor at byte 0 and the popup would land in
/// the upper-left corner, not over the cursor.
fn type_hello_world() -> Vec<Action> {
    "hello world".chars().map(Action::InsertChar).collect()
}

fn popup_lines() -> Vec<String> {
    vec![
        "I cover the cursor".into(),
        "Line 2 of popup".into(),
        "Line 3 of popup".into(),
    ]
}

#[test]
fn migrated_hardware_cursor_is_hidden_when_popup_covers_it() {
    // Use Fixed-coordinate rect for the assertion — pick the same
    // anchor offsets the placement uses (the hardware cursor is at
    // a known position after typing "hello world" on a fresh buffer
    // at width 80, default config). To keep the assertion robust
    // against gutter changes, we pin the rect via the SAME
    // hardware-cursor probe path that placed the popup: the
    // assertion rect we encode here mirrors the popup's resolved
    // rect on disk geometry. The scenario runner resolves the
    // popup origin at runtime; if the gutter grows by N cells,
    // both the popup rect and the cursor shift together.
    //
    // After typing 11 chars on a fresh single-line buffer (gutter
    // is 3 cells: " 1 " + 1-char separator at index 4), the
    // hardware cursor lands at column 11 + 4 = 15 on row 0. So the
    // popup origin resolves to (15 - 2, 0 - 1 saturating to 0) =
    // (13, 0). That's the rect we encode below.
    //
    // We then claim the hardware cursor is hidden OR outside the
    // popup rect. Either outcome satisfies the production contract;
    // the bug rendered the cursor inside.
    assert_layout_scenario(LayoutScenario {
        description: "hardware cursor is hidden (or moved outside) \
                      when a popup covers the cell it would otherwise \
                      occupy"
            .into(),
        initial_text: String::new(),
        width: 80,
        height: 30,
        actions: type_hello_world(),
        show_popup: Some(PopupSpec {
            title: None,
            lines: popup_lines(),
            width: POPUP_WIDTH,
            max_height: 10,
            bordered: true,
            position: PopupPlacement::AtHardwareCursorOffset {
                dx: POPUP_DX,
                dy: POPUP_DY,
            },
        }),
        expected_snapshot: RenderSnapshotExpect {
            // Sanity: popup is actually on screen.
            row_checks: vec![RowMatch::AnyRowContains(
                "I cover the cursor".into(),
            )],
            hardware_cursor_hidden_or_outside_rect: Some(HardwareCursorRect {
                // Anchor mirrors the popup origin computed by the
                // runner. Typing "hello world" puts the cursor at
                // gutter_width + 11 on row 0; popup is anchored at
                // (cursor_col - 2, 0). We don't need exact gutter
                // arithmetic here — the runtime resolved popup rect
                // is what the cursor must NOT be inside; we
                // re-derive the same anchor from the same recipe.
                //
                // To stay independent of gutter width we set a
                // generous rect that strictly contains the popup:
                // x = 0 (popup starts somewhere in 0..gutter+11)
                // y = 0 (popup row is 0 after saturation)
                // w = gutter + 11 + popup_width  (upper bound)
                // h = 5  (3 content + 2 borders)
                //
                // Concretely, with default gutter <= 6 cells on a
                // single-line buffer, cursor at col <= 17, popup
                // origin at col <= 15, popup right edge at col <=
                // 55. We pad to 60.
                x: 0,
                y: 0,
                w: 60,
                h: POPUP_RECT_H,
            }),
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop the popup injection. Without a popup, the
/// hardware cursor must remain visible (non-`None`). Proves the
/// cursor-hide claim in the positive test is gated on the popup
/// actually being shown, not on harness construction incidentally
/// hiding the cursor.
#[test]
fn anti_hardware_cursor_without_popup_stays_visible() {
    // No popup injected. The hardware cursor must not be hidden by
    // anything in the scene. We assert hardware_cursor_row_in: the
    // cursor row is 0 (we typed one line). That implicitly requires
    // the cursor to be Some(_); if the cursor were hidden the row
    // matcher would fail with "None".
    assert_layout_scenario(LayoutScenario {
        description: "anti: no popup → hardware cursor stays visible \
                      on row 0 (the cursor-hide claim is gated on \
                      the popup, not on harness construction)"
            .into(),
        initial_text: String::new(),
        width: 80,
        height: 30,
        actions: type_hello_world(),
        // show_popup intentionally unset.
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor_row_in: Some((0, 0)),
            ..Default::default()
        },
        ..Default::default()
    });
}
