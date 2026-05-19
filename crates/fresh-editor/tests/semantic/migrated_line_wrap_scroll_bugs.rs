//! DECLARATIVE: Migration of `tests/e2e/line_wrap_scroll_bugs.rs` —
//! scrolling bugs that surface when line wrapping is enabled.
//!
//! Two related root causes preserved here:
//!
//!   1. **Mouse-wheel / Page-Down / scrollbar click-and-drag scroll
//!      under wrap.** Pre-fix, the scroll routines iterated logical
//!      lines (the file may have only one), so they couldn't advance
//!      a viewport whose 20+ visual rows were a single wrapped
//!      logical line.
//!
//!   2. **Multi-line files with one very long wrapped line.** Same
//!      root cause, exercised on a 6-logical-line / ~30-visual-row
//!      file.
//!
//! ## What's declarative here
//!
//! Each migrated test asserts "after the scroll action, later
//! content from the wrapped buffer is visible on screen" via
//! `RowMatch::AnyRowContains` on a sentinel substring that wouldn't
//! be visible from the top of the buffer. The scroll action is
//! either an `Action` (PageDown) or an `InputEvent::Mouse` (wheel,
//! click, drag) — both routed through the production input path.
//!
//! ## Deferred (probes that have no `EditorTestApi` projection)
//!
//!   * `test_scrollbar_shows_scrollable_content_with_wrapped_lines` —
//!     counts scrollbar thumb/track cells by colour via
//!     `harness.get_cell_style()` + `editor().theme()`. No
//!     `RenderSnapshot` equivalent for "scrollbar geometry".
//!   * `test_scrollbar_thumb_drag_no_jump_on_start` — reads
//!     `editor().get_split_areas()` for the thumb's row range and
//!     `harness.top_line_number()` for the scroll position. Both
//!     are renderer-internal; the test asserts on the relative
//!     change in scroll position after a horizontal drag, which
//!     can't be expressed against the snapshot today.
//!
//! Source: `tests/e2e/line_wrap_scroll_bugs.rs` (6 of 8 tests
//! migrated; 2 deferred — file retained).

use crate::common::scenario::context::{MouseButton, MouseEvent};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, MouseDragSpec,
    ScenarioConfigOverrides,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

const TERMINAL_WIDTH_60: u16 = 60;
const TERMINAL_HEIGHT_20: u16 = 20;
const TERMINAL_WIDTH_80: u16 = 80;
const TERMINAL_HEIGHT_24: u16 = 24;

/// Build the 1600-char `AAAA…HHHH` pattern used by the
/// single-logical-line tests. 8 letters × 200 chars each lets a
/// per-row check ("DDDD on screen") prove the viewport scrolled
/// far enough into the wrapped line.
fn striped_long_line() -> String {
    let mut long_line = String::new();
    for ch in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] {
        long_line.push_str(&ch.to_string().repeat(200));
    }
    long_line
}

/// Build a 6-logical-line file with three short lines, one very
/// long line, then two trailing short lines — the multiline shape
/// the production bug reproduced with (mimics `~/Downloads/zz.txt`
/// from the original report).
fn multiline_with_long_x() -> String {
    let short_line1 = "Short line 1";
    let short_line2 = "Short line 2";
    let short_line3 = "Short line 3";
    let long_line = "X".repeat(2000);
    let short_line4 = "Short line 4";
    let short_line5 = "Short line 5";
    format!(
        "{}\n{}\n{}\n{}\n{}\n{}",
        short_line1, short_line2, short_line3, long_line, short_line4, short_line5
    )
}

/// HTML-shaped variant of `multiline_with_long_x` used by the
/// click-and-drag tests in the original — 6 logical lines with
/// the long one being `<div ... CONTENT_CONTENT_CONTENT_ ...>`.
fn multiline_with_long_html() -> String {
    let short_line1 = "<p>Short line 1</p>";
    let short_line2 = "</p>";
    let short_line3 = "</div>";
    let long_line = format!("<div class=\"content\">{}</div>", "CONTENT_".repeat(250));
    let short_line5 = "";
    let short_line6 = "";
    format!(
        "{}\n{}\n{}\n{}\n{}\n{}",
        short_line1, short_line2, short_line3, long_line, short_line5, short_line6
    )
}

fn config_wrap_on() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(true),
        ..Default::default()
    }
}

/// Build a sequence of 20 mouse-wheel-down events at the same
/// (col, row). Each entry becomes one `MouseEventKind::ScrollDown`
/// dispatched through the editor's real `handle_mouse` path.
fn wheel_down_events(count: usize, col: u16, row: u16) -> Vec<InputEvent> {
    (0..count)
        .map(|_| {
            InputEvent::Mouse(MouseEvent::Wheel {
                col,
                row,
                dy: -1,
            })
        })
        .collect()
}

#[test]
fn migrated_mouse_wheel_scrolls_wrapped_content() {
    // Original: `test_mouse_wheel_scrolls_wrapped_content`. Bug:
    // wheel scrolling didn't advance the viewport because the scroll
    // routines iterated logical lines (1), not visual rows. We
    // assert "later-letter content visible after wheel events" — at
    // initial cursor-at-top, the screen shows only `AAAA…`.
    //
    // The wheel events fire at row 5 of the content area. The
    // first content row at width=60/height=20 sits at row 1 (under
    // the tab bar at row 0), so we use row 6 in scenario terms.
    // The scroll handler doesn't depend on the row being inside
    // the buffer area for wheel events — they propagate to the
    // active viewport regardless of cell coordinates as long as the
    // editor consumes them.
    assert_layout_scenario(LayoutScenario {
        description: "20× wheel-down on wrapped 1600-char line: later content visible"
            .into(),
        initial_text: striped_long_line(),
        width: TERMINAL_WIDTH_60,
        height: TERMINAL_HEIGHT_20,
        actions: vec![Action::MoveDocumentStart],
        events: wheel_down_events(20, TERMINAL_WIDTH_60 / 2, 6),
        config_overrides: config_wrap_on(),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                // Later content from the wrapped line must be visible.
                // Any letter past 'A' on screen proves the scroll
                // happened.
                RowMatch::AnyRowContains("HHHH".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_scrollbar_drag_with_wrapped_lines() {
    // Original: `test_scrollbar_drag_with_wrapped_lines`. Drag the
    // vertical scrollbar from near-top to near-bottom; later
    // content from the wrapped line must appear.
    assert_layout_scenario(LayoutScenario {
        description: "scrollbar drag on wrapped 1600-char line: later content visible".into(),
        initial_text: striped_long_line(),
        width: TERMINAL_WIDTH_60,
        height: TERMINAL_HEIGHT_20,
        actions: vec![Action::MoveDocumentStart],
        config_overrides: config_wrap_on(),
        mouse_drags: vec![MouseDragSpec::VerticalScrollbarFullRange],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("HHHH".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_page_down_scrolls_visual_rows_with_wrapped_line() {
    // Original: `test_page_down_scrolls_visual_rows_with_wrapped_line`.
    // PageDown on a single wrapped logical line must advance the
    // viewport by ~one screen of visual rows. We assert later
    // content is visible.
    assert_layout_scenario(LayoutScenario {
        description: "PageDown on wrapped 1600-char line: later content visible".into(),
        initial_text: striped_long_line(),
        width: TERMINAL_WIDTH_60,
        height: TERMINAL_HEIGHT_20,
        actions: vec![Action::MoveDocumentStart, Action::MovePageDown],
        config_overrides: config_wrap_on(),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                // After one PageDown on a width=60/height=20 viewport
                // (~17 visible content rows ≈ 884 chars), the
                // viewport has advanced past 'A' (the first 200
                // chars of the wrapped line). The "AAAA" sentinel
                // that's visible at the top of the buffer must NOT
                // be visible anymore.
                RowMatch::NoRowContains("AAAA".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_mouse_wheel_with_multiline_file_one_long_line() {
    // Original: `test_mouse_wheel_with_multiline_file_one_long_line`.
    // 10 wheel-down events on a 6-logical-line file (one of which
    // is 2000 chars) must scroll past the short header lines and
    // into the long line's `XXXX` body or the trailing `Short
    // line 4/5`.
    assert_layout_scenario(LayoutScenario {
        description: "10× wheel-down on multiline-with-long file: not at top anymore".into(),
        initial_text: multiline_with_long_x(),
        width: TERMINAL_WIDTH_80,
        height: TERMINAL_HEIGHT_24,
        actions: vec![],
        events: wheel_down_events(10, 40, 6),
        config_overrides: config_wrap_on(),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                // After scrolling, the very first short header line
                // must no longer be visible — pre-fix the viewport
                // was stuck at the top of the file.
                RowMatch::NoRowContains("Short line 1".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_scrollbar_click_with_multiline_file_one_long_line() {
    // Original: `test_scrollbar_click_with_multiline_file_one_long_line`.
    // Clicking the scrollbar near the bottom of the track on a
    // 6-logical-line file (one of which is 2000-char HTML) must
    // scroll the viewport. We assert the first short line is no
    // longer visible — pre-fix the click was a no-op because
    // `max_scroll_line` was computed from logical lines (6 ≤ 24).
    assert_layout_scenario(LayoutScenario {
        description: "scrollbar click on multiline+long file: viewport advanced".into(),
        initial_text: multiline_with_long_html(),
        width: TERMINAL_WIDTH_80,
        height: TERMINAL_HEIGHT_24,
        actions: vec![],
        // Click near the bottom of the content area on the
        // rightmost (scrollbar) column. content_last_row at 80x24
        // is row 21 (24 - 1 status - 1 separator - 1 tabline ≈),
        // so we click at row 19 (= content_last_row - 3 in the
        // original e2e test).
        events: vec![InputEvent::Mouse(MouseEvent::Click {
            row: 19,
            col: TERMINAL_WIDTH_80 - 1,
            button: MouseButton::Left,
        })],
        config_overrides: config_wrap_on(),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoRowContains("Short line 1".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_scrollbar_drag_with_multiline_file_one_long_line() {
    // Original: `test_scrollbar_drag_with_multiline_file_one_long_line`.
    // Drag the scrollbar from near-top to near-bottom; the first
    // short line must no longer be visible afterwards.
    assert_layout_scenario(LayoutScenario {
        description: "scrollbar drag on multiline+long file: viewport advanced".into(),
        initial_text: multiline_with_long_html(),
        width: TERMINAL_WIDTH_80,
        height: TERMINAL_HEIGHT_24,
        actions: vec![],
        config_overrides: config_wrap_on(),
        mouse_drags: vec![MouseDragSpec::VerticalScrollbarFullRange],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoRowContains("Short line 1".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop the wheel-down events from
/// `migrated_mouse_wheel_scrolls_wrapped_content`. Without them
/// the viewport stays at the top of the wrapped line and "DDDD"
/// must NOT be visible — proves the positive test's assertion is
/// gated on the wheel events.
#[test]
fn anti_mouse_wheel_without_scroll_leaves_screen_unchanged() {
    let scenario = LayoutScenario {
        description: "anti: no wheel events — viewport stuck at top, DDDD off-screen".into(),
        initial_text: striped_long_line(),
        width: TERMINAL_WIDTH_60,
        height: TERMINAL_HEIGHT_20,
        actions: vec![Action::MoveDocumentStart],
        events: Vec::new(),
        config_overrides: config_wrap_on(),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("HHHH".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without wheel-down events the viewport stays at the start \
         of the wrapped line, so DDDD must NOT be visible. The positive test's \
         'DDDD visible' check should fail."
    );
}

/// Anti-test: drop the `PageDown` action. Without it the viewport
/// stays at the top, so "AAAA" remains visible — and the positive
/// test's `NoRowContains("AAAA")` claim must fail.
#[test]
fn anti_page_down_without_keypress_leaves_screen_unchanged() {
    let scenario = LayoutScenario {
        description: "anti: no PageDown — viewport stuck at top, AAAA still on screen".into(),
        initial_text: striped_long_line(),
        width: TERMINAL_WIDTH_60,
        height: TERMINAL_HEIGHT_20,
        actions: vec![Action::MoveDocumentStart],
        config_overrides: config_wrap_on(),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoRowContains("AAAA".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without PageDown the viewport stays at the start of the \
         wrapped line, so AAAA remains on screen. The positive test's \
         NoRowContains('AAAA') check should fail."
    );
}
