//! DECLARATIVE: Migration of `tests/e2e/side_by_side_diff_scroll.rs`
//! — mouse wheel, scrollbar click-to-jump, and scrollbar-drag
//! interactions inside a side-by-side diff composite buffer.
//!
//! Scenarios are pure data: each test is a single `LayoutScenario`
//! literal with a `composite_buffer: Some(CompositeBufferSpec)` for
//! the diff setup and an `events` list for the post-setup mouse /
//! scrollbar events. No direct harness usage; no imperative
//! `let mut harness = ...; for _ in 0..N { harness.mouse_X; }`
//! blocks anywhere in this file.
//!
//! Load-bearing claims preserved here (one declarative scenario per
//! claim, same coordinates and counts as the e2e original):
//!
//!   1. Mouse wheel scroll-down on a side-by-side diff hides the
//!      first line of content.
//!   2. Wheel scroll-up reverses a prior scroll-down — `Line 1`
//!      becomes visible again.
//!   3. Wheel scroll-up at the top is clamped (no-op), `Line 1`
//!      stays visible.
//!   4. Clicking the scrollbar track near the bottom jumps the
//!      viewport so `Line 1` disappears.
//!   5. Clicking the scrollbar track at the top jumps the viewport
//!      back so `Line 1` reappears.
//!   6. Dragging the scrollbar thumb down hides `Line 1`.
//!   7. Dragging the scrollbar thumb back up brings `Line 1` back.
//!   8. The vertical scrollbar is rendered in the rightmost column.
//!   9. Wheel scroll over the left pane and the right pane produce
//!      the same scroll result.
//!  10. Scrolling past the first half of the diff exposes later
//!      content (one of: Line 80/90/95/100, or hunk replacement
//!      text containing "modified content").
//!
//! Source: `tests/e2e/side_by_side_diff_scroll.rs` (10 tests
//! migrated; no tests deferred).

use crate::common::scenario::context::{MouseButton, MouseEvent};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, CompositeBufferSpec, LayoutScenario,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};

/// Build (old_content, new_content, hunks) for a diff with
/// `line_count` total lines and a single hunk near the middle that
/// inserts `extra_new_lines` extra lines on the NEW side. Mirrors
/// the e2e `generate_diff_content` helper byte-for-byte.
fn generate_diff_content(
    line_count: usize,
    extra_new_lines: usize,
) -> (String, String, Vec<(usize, usize, usize, usize)>) {
    let old_content: String = (1..=line_count)
        .map(|i| format!("Line {i} original content here\n"))
        .collect();

    let hunk_start = line_count / 2;
    let hunk_old_count = 3;
    let hunk_new_count = hunk_old_count + extra_new_lines;

    let mut new_lines: Vec<String> = (1..=line_count)
        .map(|i| format!("Line {i} original content here\n"))
        .collect();
    let replacement: Vec<String> = (0..hunk_new_count)
        .map(|i| {
            if i < hunk_old_count {
                format!("Line {} modified content here\n", hunk_start + 1 + i)
            } else {
                format!("Line NEW-{} added content\n", i - hunk_old_count + 1)
            }
        })
        .collect();
    new_lines.splice(hunk_start..hunk_start + hunk_old_count, replacement);
    let new_content: String = new_lines.join("");

    let hunks = vec![(hunk_start, hunk_old_count, hunk_start, hunk_new_count)];
    (old_content, new_content, hunks)
}

fn diff_spec(line_count: usize, extra_new_lines: usize) -> CompositeBufferSpec {
    let (old_content, new_content, hunks) = generate_diff_content(line_count, extra_new_lines);
    CompositeBufferSpec {
        name: "Diff View".into(),
        mode: "diff-view".into(),
        old_content,
        new_content,
        hunks,
        ..Default::default()
    }
}

fn wheel_down(col: u16, row: u16, count: u16) -> Vec<InputEvent> {
    (0..count)
        .map(|_| InputEvent::Mouse(MouseEvent::Wheel { row, col, dy: -1 }))
        .collect()
}

fn wheel_up(col: u16, row: u16, count: u16) -> Vec<InputEvent> {
    (0..count)
        .map(|_| InputEvent::Mouse(MouseEvent::Wheel { row, col, dy: 1 }))
        .collect()
}

#[test]
fn migrated_side_by_side_diff_mouse_wheel_scroll_down() {
    assert_layout_scenario(LayoutScenario {
        description: "wheel-down on side-by-side diff hides Line 1".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(100, 5)),
        events: wheel_down(60, 20, 5),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_mouse_wheel_scroll_up() {
    let mut events = wheel_down(60, 20, 10);
    events.extend(wheel_up(60, 20, 15));
    assert_layout_scenario(LayoutScenario {
        description: "wheel-up after wheel-down brings Line 1 back".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(100, 5)),
        events,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_mouse_wheel_scroll_clamps_at_top() {
    assert_layout_scenario(LayoutScenario {
        description: "wheel-up at top is clamped — Line 1 stays visible".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(100, 5)),
        events: wheel_up(60, 20, 10),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_scrollbar_click_jump() {
    assert_layout_scenario(LayoutScenario {
        description: "click near bottom of scrollbar hides Line 1".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(200, 10)),
        events: vec![InputEvent::Mouse(MouseEvent::Click {
            col: 119,
            row: 30,
            button: MouseButton::Left,
        })],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_scrollbar_click_near_top() {
    let mut events = wheel_down(60, 20, 20);
    events.push(InputEvent::Mouse(MouseEvent::Click {
        col: 119,
        row: 2,
        button: MouseButton::Left,
    }));
    assert_layout_scenario(LayoutScenario {
        description: "after scroll-down, click near top of scrollbar brings Line 1 back".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(200, 10)),
        events,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_scrollbar_drag_down() {
    assert_layout_scenario(LayoutScenario {
        description: "drag scrollbar thumb down hides Line 1".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(200, 10)),
        events: vec![InputEvent::Mouse(MouseEvent::Drag {
            from_col: 119,
            from_row: 3,
            to_col: 119,
            to_row: 20,
            button: MouseButton::Left,
        })],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_scrollbar_drag_up() {
    let mut events = wheel_down(60, 20, 20);
    // Sleep past the double-click window (harness default is 10ms,
    // so 25ms is safe). Mirrors the e2e wait between drags.
    events.push(InputEvent::SleepMs(25));
    events.push(InputEvent::Mouse(MouseEvent::Drag {
        from_col: 119,
        from_row: 20,
        to_col: 119,
        to_row: 0,
        button: MouseButton::Left,
    }));
    assert_layout_scenario(LayoutScenario {
        description: "drag scrollbar thumb back up brings Line 1 visible again".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(200, 10)),
        events,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_scrollbar_visible() {
    assert_layout_scenario(LayoutScenario {
        description: "vertical scrollbar is rendered in the rightmost column".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(100, 5)),
        expected_scrollbar_at_column: Some(119),
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_scroll_works_on_both_panes() {
    // Scroll over left pane (col 20), record grid, undo the scroll
    // with 10 wheel-ups (clamps at top), then scroll over right
    // pane (col 80). Final assertion: the post-right-scroll grid
    // equals the recorded post-left-scroll grid.
    let mut events = wheel_down(20, 20, 5);
    events.push(InputEvent::RecordRenderedRows { slot: 0 });
    events.extend(wheel_up(20, 20, 10));
    events.extend(wheel_down(80, 20, 5));
    events.push(InputEvent::AssertRenderedRowsMatch { slot: 0 });
    assert_layout_scenario(LayoutScenario {
        description: "scroll on left pane and right pane produce identical grids".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(100, 5)),
        events,
        ..Default::default()
    });
}

#[test]
fn migrated_side_by_side_diff_scroll_to_later_content() {
    let events = wheel_down(60, 20, 30);
    assert_layout_scenario(LayoutScenario {
        description: "after deep wheel-down, Line 1 gone and later content visible".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(100, 5)),
        events,
        expected_snapshot: RenderSnapshotExpect {
            // Line 1 must not be visible AND one of the later
            // lines (or the hunk's "modified content" replacement)
            // must be on screen — exact same disjunction as the
            // e2e original.
            row_checks: vec![
                RowMatch::NoRowContains("Line 1 original".into()),
                RowMatch::AnyRowContainsAny(vec![
                    "Line 80 original".into(),
                    "Line 90 original".into(),
                    "Line 95 original".into(),
                    "Line 100 original".into(),
                    "modified content".into(),
                ]),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

// =============================================================================
// Anti-tests
// =============================================================================

/// Anti-test: drop the mouse-wheel scroll-down events. Without
/// them, `Line 1` must remain visible. Proves the visibility
/// claim in `migrated_side_by_side_diff_mouse_wheel_scroll_down`
/// depends on the actual wheel events.
#[test]
fn anti_side_by_side_diff_without_wheel_scroll_keeps_line_1_visible() {
    let scenario = LayoutScenario {
        description:
            "anti: without wheel scroll-down, Line 1 must NOT have been hidden".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(100, 5)),
        events: vec![], // dropped
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without wheel scroll-down Line 1 should remain visible \
         (the NoRowContains check should fail)"
    );
}

/// Anti-test: drop the scrollbar drag-down. Without it, `Line 1`
/// must remain visible. Proves the drag is what shifts the
/// viewport in `migrated_side_by_side_diff_scrollbar_drag_down`,
/// not the harness setup itself.
#[test]
fn anti_side_by_side_diff_without_scrollbar_drag_keeps_line_1_visible() {
    let scenario = LayoutScenario {
        description: "anti: without scrollbar drag, Line 1 must NOT have been hidden".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(diff_spec(200, 10)),
        events: vec![], // dropped
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoRowContains("Line 1 original".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without drag Line 1 should still be at top of viewport"
    );
}
