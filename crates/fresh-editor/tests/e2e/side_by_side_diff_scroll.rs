// End-to-end tests for side-by-side diff view scrolling
//
// Tests mouse wheel scrolling, scrollbar click-to-jump, and scrollbar drag
// interactions within composite buffer diff views.

use crate::common::harness::EditorTestHarness;
use fresh::model::composite_buffer::{
    CompositeLayout, DiffHunk, LineAlignment, PaneStyle, SourcePane,
};
use fresh::model::event::BufferId;
use fresh::primitives::text_property::TextPropertyEntry;

/// Helper to create a side-by-side diff view with two buffers.
///
/// Creates two virtual buffers with `old_content` and `new_content`, then creates
/// a composite buffer showing them side-by-side with the given diff hunks.
/// Returns the composite buffer id.
fn setup_side_by_side_diff(
    harness: &mut EditorTestHarness,
    old_content: &str,
    new_content: &str,
    hunks: &[DiffHunk],
) -> BufferId {
    // Create two hidden virtual buffers for old and new content
    let old_buffer_id =
        harness
            .editor_mut()
            .create_virtual_buffer("OLD".to_string(), "text".to_string(), true);

    // Set content on the old buffer
    harness
        .editor_mut()
        .set_virtual_buffer_content(old_buffer_id, vec![TextPropertyEntry::text(old_content)])
        .unwrap();

    let new_buffer_id =
        harness
            .editor_mut()
            .create_virtual_buffer("NEW".to_string(), "text".to_string(), true);

    // Set content on the new buffer
    harness
        .editor_mut()
        .set_virtual_buffer_content(new_buffer_id, vec![TextPropertyEntry::text(new_content)])
        .unwrap();

    // Create composite buffer with side-by-side layout
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

    // Set alignment from hunks
    let old_line_count = old_content.lines().count();
    let new_line_count = new_content.lines().count();
    let alignment = LineAlignment::from_hunks(hunks, old_line_count, new_line_count);
    harness
        .editor_mut()
        .set_composite_alignment(composite_id, alignment);

    // Switch to the composite buffer
    harness.editor_mut().switch_buffer(composite_id);
    harness.render().unwrap();

    composite_id
}

/// Generate old and new content for a diff with many lines.
/// Old content has `line_count` lines, new content has `line_count + extra_new_lines` lines.
/// A hunk is created around the middle of the file.
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
    // Replace old lines in the hunk with modified + added lines
    let replacement: Vec<String> = (0..hunk_new_count)
        .map(|i| {
            if i < hunk_old_count {
                format!("Line {} modified content here\n", hunk_start + 1 + i)
            } else {
                format!("Line NEW-{} added content\n", i - hunk_old_count + 1)
            }
        })
        .collect();

    // Replace lines [hunk_start..hunk_start+hunk_old_count] with replacement
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

/// Test mouse wheel scrolling down in a side-by-side diff view.
/// After scrolling, the first line should no longer be visible and later lines should appear.
#[test]
fn test_side_by_side_diff_mouse_wheel_scroll_down() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // Initial screen should show early lines
    let initial_screen = harness.screen_to_string();
    assert!(
        initial_screen.contains("Line 1 original"),
        "Initial view should show Line 1. Screen:\n{}",
        initial_screen
    );

    // Scroll down with mouse wheel (multiple times to accumulate scroll)
    // Each scroll event moves 3 lines
    for _ in 0..5 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }

    let screen_after = harness.screen_to_string();
    // After scrolling down 15 lines, Line 1 should no longer be visible
    assert!(
        !screen_after.contains("Line 1 original"),
        "After scrolling down, Line 1 should not be visible. Screen:\n{}",
        screen_after
    );
}

/// Test mouse wheel scroll up reverses scroll down.
#[test]
fn test_side_by_side_diff_mouse_wheel_scroll_up() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // Scroll down first so Line 1 is off-screen
    for _ in 0..10 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }

    let screen_scrolled_down = harness.screen_to_string();
    assert!(
        !screen_scrolled_down.contains("Line 1 original"),
        "Line 1 should not be visible after scrolling down"
    );

    // Now scroll all the way back up
    for _ in 0..15 {
        harness.mouse_scroll_up(60, 20).unwrap();
    }

    let screen_after_up = harness.screen_to_string();
    assert!(
        screen_after_up.contains("Line 1 original"),
        "After scrolling back up, Line 1 should be visible again. Screen:\n{}",
        screen_after_up
    );
}

/// Test that mouse wheel scroll doesn't go below zero (stays at top).
#[test]
fn test_side_by_side_diff_mouse_wheel_scroll_clamps_at_top() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // Try to scroll up when already at top -- should be a no-op
    for _ in 0..10 {
        harness.mouse_scroll_up(60, 20).unwrap();
    }

    // Line 1 should still be visible (we didn't scroll anywhere)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Line 1 original"),
        "After scrolling up at top, Line 1 should still be visible. Screen:\n{}",
        screen
    );
}

/// Test scrollbar click-to-jump in a side-by-side diff view.
/// Clicking on the scrollbar track near the bottom should jump the viewport
/// so early lines are no longer visible.
#[test]
fn test_side_by_side_diff_scrollbar_click_jump() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // Initial screen should show Line 1
    let initial_screen = harness.screen_to_string();
    assert!(
        initial_screen.contains("Line 1 original"),
        "Should show Line 1 initially"
    );

    // The scrollbar is at the rightmost column (119 for 120-width terminal).
    // Click near the bottom of the scrollbar to jump to a later position.
    harness.mouse_click(119, 30).unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    assert!(
        !screen_after.contains("Line 1 original"),
        "After clicking near bottom of scrollbar, Line 1 should not be visible. Screen:\n{}",
        screen_after
    );
}

/// Test scrollbar click near the top of the track jumps back to earlier content.
#[test]
fn test_side_by_side_diff_scrollbar_click_near_top() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // First scroll down via mouse wheel so Line 1 is off-screen
    for _ in 0..20 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }

    let screen_scrolled = harness.screen_to_string();
    assert!(
        !screen_scrolled.contains("Line 1 original"),
        "Line 1 should not be visible after scrolling down"
    );

    // Click at the very top of the scrollbar to jump back to the start.
    // The scrollbar starts at row 2 (below menu bar and tab bar).
    harness.mouse_click(119, 2).unwrap();
    harness.render().unwrap();

    let screen_after_click = harness.screen_to_string();
    assert!(
        screen_after_click.contains("Line 1 original"),
        "After clicking at top of scrollbar, Line 1 should be visible. Screen:\n{}",
        screen_after_click
    );
}

/// Test scrollbar drag in a side-by-side diff view.
/// Dragging the scrollbar from top to middle should scroll the view down.
#[test]
fn test_side_by_side_diff_scrollbar_drag_down() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // Verify starting position
    let initial_screen = harness.screen_to_string();
    assert!(
        initial_screen.contains("Line 1 original"),
        "Should show Line 1 initially"
    );

    // Drag scrollbar from top to middle of content area.
    // Scrollbar is at column 119, content area is roughly rows 2..38.
    harness.mouse_drag(119, 3, 119, 20).unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    assert!(
        !screen_after.contains("Line 1 original"),
        "After dragging scrollbar down, Line 1 should not be visible. Screen:\n{}",
        screen_after
    );
}

/// Test scrollbar drag back up.
/// After scrolling down, dragging the scrollbar up should bring back earlier content.
#[test]
fn test_side_by_side_diff_scrollbar_drag_up() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(200, 10);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // First scroll down via mouse wheel so Line 1 is off-screen
    for _ in 0..20 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }

    let screen_scrolled = harness.screen_to_string();
    assert!(
        !screen_scrolled.contains("Line 1 original"),
        "Line 1 should not be visible after scrolling down"
    );

    // Wait to avoid double-click detection
    let double_click_delay =
        std::time::Duration::from_millis(harness.config().editor.double_click_time_ms * 2);
    std::thread::sleep(double_click_delay);

    // Drag scrollbar from middle back toward the very top.
    // Dragging to row 0 (above scrollbar) ensures we clamp to the start.
    harness.mouse_drag(119, 20, 119, 0).unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    assert!(
        screen_after.contains("Line 1 original"),
        "After dragging scrollbar back up, Line 1 should be visible. Screen:\n{}",
        screen_after
    );
}

/// Test that the diff view renders with a scrollbar visible.
#[test]
fn test_side_by_side_diff_scrollbar_visible() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // The scrollbar should be rendered at the rightmost column (119)
    assert!(
        harness.has_scrollbar_at_column(119),
        "Scrollbar should be visible in the rightmost column of the diff view"
    );
}

/// Test mouse wheel scrolling works on both left and right panes.
/// Scrolling over either pane should produce the same result.
#[test]
fn test_side_by_side_diff_scroll_works_on_both_panes() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // Scroll over the left pane (column 20)
    for _ in 0..5 {
        harness.mouse_scroll_down(20, 20).unwrap();
    }
    let screen_after_left = harness.screen_to_string();

    // Reset scroll by scrolling up
    for _ in 0..10 {
        harness.mouse_scroll_up(20, 20).unwrap();
    }

    // Verify we're back at the top
    let screen_reset = harness.screen_to_string();
    assert!(
        screen_reset.contains("Line 1 original"),
        "Should have scrolled back to top"
    );

    // Scroll over the right pane (column 80)
    for _ in 0..5 {
        harness.mouse_scroll_down(80, 20).unwrap();
    }
    let screen_after_right = harness.screen_to_string();

    // Both should produce the same screen output (same scroll position)
    assert_eq!(
        screen_after_left, screen_after_right,
        "Scrolling on left pane and right pane should produce the same view"
    );
}

/// Test that scrolling a large diff view shows later content correctly.
#[test]
fn test_side_by_side_diff_scroll_to_later_content() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    let (old_content, new_content, hunks) = generate_diff_content(100, 5);
    let _composite_id = setup_side_by_side_diff(&mut harness, &old_content, &new_content, &hunks);

    // Scroll down past the first half of the document
    for _ in 0..30 {
        harness.mouse_scroll_down(60, 20).unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Line 1 should not be visible
    assert!(
        !screen.contains("Line 1 original"),
        "Line 1 should not be visible after scrolling past it"
    );

    // Lines from later in the file should be visible
    let has_later_content = screen.contains("Line 80 original")
        || screen.contains("Line 90 original")
        || screen.contains("Line 95 original")
        || screen.contains("Line 100 original")
        || screen.contains("modified content");
    assert!(
        has_later_content,
        "After scrolling down, should see later content. Screen:\n{}",
        screen
    );
}
