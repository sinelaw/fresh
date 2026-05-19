//! Migration of `tests/e2e/side_by_side_diff_scroll.rs` — mouse
//! wheel, scrollbar click-to-jump, and scrollbar-drag interactions
//! inside a side-by-side diff composite buffer.
//!
//! Load-bearing claims preserved here:
//!
//!   1. Mouse wheel scroll-down on a side-by-side diff hides the
//!      first line of content (the viewport advances by the wheel
//!      step accumulated across multiple wheel events).
//!   2. Wheel scroll-up reverses a prior scroll-down — `Line 1`
//!      becomes visible again.
//!   3. Wheel scroll-up at the top is clamped (no-op), `Line 1`
//!      stays visible.
//!   4. Clicking the scrollbar track near the bottom jumps the
//!      viewport so `Line 1` disappears.
//!   5. Clicking the scrollbar track at the top jumps the viewport
//!      back so `Line 1` reappears (after a prior scroll-down).
//!   6. Dragging the scrollbar thumb down hides `Line 1`.
//!   7. Dragging the scrollbar thumb back up brings `Line 1` back.
//!   8. The vertical scrollbar is rendered in the rightmost column.
//!   9. Wheel scroll over the left pane and the right pane produce
//!      the same scroll result (one wheel-event handler covers both
//!      panes).
//!  10. Scrolling past the first half of the diff exposes later
//!      content (one of: Line 80/90/95/100, or hunk replacement
//!      text containing "modified content").
//!
//! ## Harness-direct pattern
//!
//! These tests reach into the composite-buffer setup
//! (`create_virtual_buffer`, `set_virtual_buffer_content`,
//! `create_composite_buffer`, `set_composite_alignment`) — there is
//! no `EditorTestApi` projection for composite buffers, and the
//! scrollbar geometry probes (`has_scrollbar_at_column`,
//! `mouse_scroll_down`, `mouse_drag`, `mouse_click`) also live on
//! `EditorTestHarness`. Per-row text assertions go through
//! `RenderSnapshot::extract_with_rendered_rows`, the framework path
//! for per-row text inspection unblocked by issue #2058.
//!
//! Source: `tests/e2e/side_by_side_diff_scroll.rs` (10 tests
//! migrated; no tests deferred).

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};
use fresh::model::composite_buffer::{
    CompositeLayout, DiffHunk, LineAlignment, PaneStyle, SourcePane,
};
use fresh::model::event::BufferId;
use fresh::primitives::text_property::TextPropertyEntry;

/// Helper to create a side-by-side diff view with two buffers.
/// Mirrors the e2e helper exactly so the regression-triggering
/// fixture (two virtual buffers + composite SideBySide layout +
/// alignment from hunks) is unchanged.
fn setup_side_by_side_diff(
    harness: &mut EditorTestHarness,
    old_content: &str,
    new_content: &str,
    hunks: &[DiffHunk],
) -> BufferId {
    let old_buffer_id = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("OLD".to_string(), "text".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(old_buffer_id, vec![TextPropertyEntry::text(old_content)])
        .unwrap();

    let new_buffer_id = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("NEW".to_string(), "text".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(new_buffer_id, vec![TextPropertyEntry::text(new_content)])
        .unwrap();

    let sources = vec![
        SourcePane::new(old_buffer_id, "OLD", false).with_style(PaneStyle::old_diff()),
        SourcePane::new(new_buffer_id, "NEW", false).with_style(PaneStyle::new_diff()),
    ];
    let layout = CompositeLayout::SideBySide {
        ratios: vec![0.5, 0.5],
        show_separator: true,
    };
    let composite_id = harness.editor_mut().create_composite_buffer(
        "Diff View".to_string(),
        "diff-view".to_string(),
        layout,
        sources,
    );

    let old_line_count = old_content.lines().count();
    let new_line_count = new_content.lines().count();
    let alignment = LineAlignment::from_hunks(hunks, old_line_count, new_line_count);
    harness
        .editor_mut()
        .active_window_mut()
        .set_composite_alignment(composite_id, alignment);

    harness.editor_mut().switch_buffer(composite_id);
    harness.render().unwrap();

    composite_id
}

/// Generate old and new content for a diff with many lines. Mirrors
/// the e2e helper.
fn generate_diff_content(
    line_count: usize,
    extra_new_lines: usize,
) -> (String, String, Vec<DiffHunk>) {
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

    let hunks = vec![DiffHunk::new(
        hunk_start,
        hunk_old_count,
        hunk_start,
        hunk_new_count,
    )];

    (old_content, new_content, hunks)
}

/// Snapshot helper: extract rendered rows and assert a `RowMatch`.
fn assert_row_match(harness: &mut EditorTestHarness, check: RowMatch, label: &str) {
    let snap = RenderSnapshot::extract_with_rendered_rows(harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![check],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "{label}: {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_side_by_side_diff_mouse_wheel_scroll_down() {
    // Original: `test_side_by_side_diff_mouse_wheel_scroll_down`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "initial view should show Line 1",
    );

    for _ in 0..5 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }

    assert_row_match(
        &mut harness,
        RowMatch::NoRowContains("Line 1 original".into()),
        "after scrolling down, Line 1 should not be visible",
    );
}

#[test]
fn migrated_side_by_side_diff_mouse_wheel_scroll_up() {
    // Original: `test_side_by_side_diff_mouse_wheel_scroll_up`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    for _ in 0..10 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }
    assert_row_match(
        &mut harness,
        RowMatch::NoRowContains("Line 1 original".into()),
        "Line 1 should not be visible after scrolling down",
    );

    for _ in 0..15 {
        harness.mouse_scroll_up(60, 20).unwrap();
    }
    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "after scrolling back up, Line 1 should be visible again",
    );
}

#[test]
fn migrated_side_by_side_diff_mouse_wheel_scroll_clamps_at_top() {
    // Original: `test_side_by_side_diff_mouse_wheel_scroll_clamps_at_top`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    for _ in 0..10 {
        harness.mouse_scroll_up(60, 20).unwrap();
    }

    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "after scrolling up at top, Line 1 should still be visible",
    );
}

#[test]
fn migrated_side_by_side_diff_scrollbar_click_jump() {
    // Original: `test_side_by_side_diff_scrollbar_click_jump`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "should show Line 1 initially",
    );

    harness.mouse_click(119, 30).unwrap();
    harness.render().unwrap();

    assert_row_match(
        &mut harness,
        RowMatch::NoRowContains("Line 1 original".into()),
        "after clicking near bottom of scrollbar, Line 1 should not be visible",
    );
}

#[test]
fn migrated_side_by_side_diff_scrollbar_click_near_top() {
    // Original: `test_side_by_side_diff_scrollbar_click_near_top`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    for _ in 0..20 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }
    assert_row_match(
        &mut harness,
        RowMatch::NoRowContains("Line 1 original".into()),
        "Line 1 should not be visible after scrolling down",
    );

    harness.mouse_click(119, 2).unwrap();
    harness.render().unwrap();

    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "after clicking at top of scrollbar, Line 1 should be visible",
    );
}

#[test]
fn migrated_side_by_side_diff_scrollbar_drag_down() {
    // Original: `test_side_by_side_diff_scrollbar_drag_down`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "should show Line 1 initially",
    );

    harness.mouse_drag(119, 3, 119, 20).unwrap();
    harness.render().unwrap();

    assert_row_match(
        &mut harness,
        RowMatch::NoRowContains("Line 1 original".into()),
        "after dragging scrollbar down, Line 1 should not be visible",
    );
}

#[test]
fn migrated_side_by_side_diff_scrollbar_drag_up() {
    // Original: `test_side_by_side_diff_scrollbar_drag_up`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    for _ in 0..20 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }
    assert_row_match(
        &mut harness,
        RowMatch::NoRowContains("Line 1 original".into()),
        "Line 1 should not be visible after scrolling down",
    );

    // Wait to avoid double-click detection (mirrors e2e).
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);
    std::thread::sleep(double_click_delay);

    harness.mouse_drag(119, 20, 119, 0).unwrap();
    harness.render().unwrap();

    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "after dragging scrollbar back up, Line 1 should be visible",
    );
}

#[test]
fn migrated_side_by_side_diff_scrollbar_visible() {
    // Original: `test_side_by_side_diff_scrollbar_visible`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    assert!(
        harness.has_scrollbar_at_column(119),
        "scrollbar should be visible in the rightmost column of the diff view"
    );
}

#[test]
fn migrated_side_by_side_diff_scroll_works_on_both_panes() {
    // Original: `test_side_by_side_diff_scroll_works_on_both_panes`.
    // Mirrors the e2e exactly: scroll over col 20 (left pane), reset
    // by scrolling up, scroll over col 80 (right pane), and the two
    // resulting screens must match.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    for _ in 0..5 {
        harness.mouse_scroll_down(20, 20).unwrap();
    }
    let snap_left = RenderSnapshot::extract_with_rendered_rows(&mut harness);

    for _ in 0..10 {
        harness.mouse_scroll_up(20, 20).unwrap();
    }
    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "should have scrolled back to top",
    );

    for _ in 0..5 {
        harness.mouse_scroll_down(80, 20).unwrap();
    }
    let snap_right = RenderSnapshot::extract_with_rendered_rows(&mut harness);

    assert_eq!(
        snap_left.rendered_rows, snap_right.rendered_rows,
        "scrolling on left pane and right pane should produce the same view"
    );
}

#[test]
fn migrated_side_by_side_diff_scroll_to_later_content() {
    // Original: `test_side_by_side_diff_scroll_to_later_content`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    for _ in 0..30 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }
    harness.render().unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let rows = &snap.rendered_rows;

    let line_1_visible = rows.iter().any(|r| r.contains("Line 1 original"));
    assert!(
        !line_1_visible,
        "Line 1 should not be visible after scrolling past it. Rows:\n{}",
        rows.join("\n"),
    );

    let has_later_content = rows.iter().any(|r| {
        r.contains("Line 80 original")
            || r.contains("Line 90 original")
            || r.contains("Line 95 original")
            || r.contains("Line 100 original")
            || r.contains("modified content")
    });
    assert!(
        has_later_content,
        "after scrolling down, should see later content. Rows:\n{}",
        rows.join("\n"),
    );
}

/// Anti-test: drop the mouse-wheel scroll-down loop. Without the
/// scroll, `Line 1` must remain visible — proves the visibility
/// claim in `migrated_side_by_side_diff_mouse_wheel_scroll_down`
/// depends on the actual wheel events, not on incidental viewport
/// state.
#[test]
fn anti_side_by_side_diff_without_wheel_scroll_keeps_line_1_visible() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // No mouse_scroll_down loop — that's the load-bearing step we drop.

    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "anti: without wheel scroll-down, Line 1 must remain visible",
    );
}

/// Anti-test: drop the scrollbar drag-down. Without it, `Line 1`
/// must remain visible — proves the drag is what shifts the
/// viewport in `migrated_side_by_side_diff_scrollbar_drag_down`,
/// not the harness setup itself.
#[test]
fn anti_side_by_side_diff_without_scrollbar_drag_keeps_line_1_visible() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id =
        setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // No mouse_drag — that's the load-bearing step we drop.
    harness.render().unwrap();

    assert_row_match(
        &mut harness,
        RowMatch::AnyRowContains("Line 1 original".into()),
        "anti: without scrollbar drag, Line 1 must remain visible at top of viewport",
    );
}
