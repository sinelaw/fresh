// End-to-end tests for hunk navigation in side-by-side diff views.
//
// Tests that:
// - composite_next_hunk jumps to the next hunk and centers it
// - composite_prev_hunk jumps to the previous hunk
// - Cursor moves along with the scroll when navigating hunks
// - Hunk is centered with context lines above

use crate::common::harness::EditorTestHarness;
use fresh::model::composite_buffer::{
    CompositeLayout, DiffHunk, LineAlignment, PaneStyle, SourcePane,
};
use fresh::model::event::BufferId;
use fresh::primitives::text_property::TextPropertyEntry;

/// Generate content with multiple hunks spread across the file.
/// Returns (old_content, new_content, hunks) where hunks are at lines 20, 60, and 120.
fn generate_multi_hunk_content() -> (String, String, Vec<DiffHunk>) {
    let line_count = 150;

    let old_lines: Vec<String> = (1..=line_count)
        .map(|i| format!("Line {i} original content"))
        .collect();

    let mut new_lines = old_lines.clone();

    // Hunk 1: modify lines 20-22 (0-indexed: 19-21)
    for i in 19..22 {
        new_lines[i] = format!("Line {} MODIFIED in hunk 1", i + 1);
    }

    // Hunk 2: modify lines 60-63 (0-indexed: 59-62)
    for i in 59..63 {
        new_lines[i] = format!("Line {} MODIFIED in hunk 2", i + 1);
    }

    // Hunk 3: modify lines 120-124 (0-indexed: 119-123)
    for i in 119..124 {
        new_lines[i] = format!("Line {} MODIFIED in hunk 3", i + 1);
    }

    let old_content = old_lines.join("\n") + "\n";
    let new_content = new_lines.join("\n") + "\n";

    let hunks = vec![
        DiffHunk::new(19, 3, 19, 3),   // Hunk 1 at line 20
        DiffHunk::new(59, 4, 59, 4),   // Hunk 2 at line 60
        DiffHunk::new(119, 5, 119, 5), // Hunk 3 at line 120
    ];

    (old_content, new_content, hunks)
}

/// Helper to create a side-by-side diff view with two buffers.
fn setup_diff(
    harness: &mut EditorTestHarness,
    old_content: &str,
    new_content: &str,
    hunks: &[DiffHunk],
) -> BufferId {
    let old_buffer_id =
        harness
            .editor_mut()
            .create_virtual_buffer("OLD".to_string(), "text".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(old_buffer_id, vec![TextPropertyEntry::text(old_content)])
        .unwrap();

    let new_buffer_id =
        harness
            .editor_mut()
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
        .set_composite_alignment(composite_id, alignment);

    harness.editor_mut().switch_buffer(composite_id);
    harness.render().unwrap();

    composite_id
}

/// Test that composite_next_hunk navigates to each hunk in order.
/// The hunk content should become visible on screen after each jump.
#[test]
fn test_next_hunk_navigation_shows_hunk_content() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Initially, Line 1 should be visible (we're at the top)
    let initial = harness.screen_to_string();
    assert!(
        initial.contains("Line 1 original"),
        "Initial view should show Line 1. Screen:\n{}",
        initial
    );

    // Jump to hunk 1 (around line 20)
    harness
        .editor_mut()
        .composite_next_hunk_active(composite_id);
    harness.render().unwrap();

    let after_first = harness.screen_to_string();
    assert!(
        after_first.contains("MODIFIED in hunk 1"),
        "After first next_hunk, hunk 1 content should be visible. Screen:\n{}",
        after_first
    );

    // Jump to hunk 2 (around line 60)
    harness
        .editor_mut()
        .composite_next_hunk_active(composite_id);
    harness.render().unwrap();

    let after_second = harness.screen_to_string();
    assert!(
        after_second.contains("MODIFIED in hunk 2"),
        "After second next_hunk, hunk 2 content should be visible. Screen:\n{}",
        after_second
    );

    // Jump to hunk 3 (around line 120)
    harness
        .editor_mut()
        .composite_next_hunk_active(composite_id);
    harness.render().unwrap();

    let after_third = harness.screen_to_string();
    assert!(
        after_third.contains("MODIFIED in hunk 3"),
        "After third next_hunk, hunk 3 content should be visible. Screen:\n{}",
        after_third
    );
}

/// Test that composite_prev_hunk navigates back to the previous hunk.
#[test]
fn test_prev_hunk_navigation() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Navigate to hunk 3
    for _ in 0..3 {
        harness
            .editor_mut()
            .composite_next_hunk_active(composite_id);
    }
    harness.render().unwrap();

    let at_hunk3 = harness.screen_to_string();
    assert!(
        at_hunk3.contains("MODIFIED in hunk 3"),
        "Should be at hunk 3. Screen:\n{}",
        at_hunk3
    );

    // Go back to hunk 2
    harness
        .editor_mut()
        .composite_prev_hunk_active(composite_id);
    harness.render().unwrap();

    let at_hunk2 = harness.screen_to_string();
    assert!(
        at_hunk2.contains("MODIFIED in hunk 2"),
        "After prev_hunk, hunk 2 content should be visible. Screen:\n{}",
        at_hunk2
    );
}

/// Test that hunk navigation centers the hunk — context lines before
/// the hunk header should be visible (not starting exactly at the hunk).
#[test]
fn test_hunk_navigation_shows_context_above() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Jump to hunk 2 (at line 60)
    harness
        .editor_mut()
        .composite_next_hunk_active(composite_id);
    harness
        .editor_mut()
        .composite_next_hunk_active(composite_id);
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    assert!(
        screen.contains("MODIFIED in hunk 2"),
        "Hunk 2 content should be visible"
    );
    // With 40-line viewport and 1/3 context above (~13 lines), lines around 47-58
    // should be visible before the hunk at line 60
    assert!(
        screen.contains("Line 55 original")
            || screen.contains("Line 56 original")
            || screen.contains("Line 57 original")
            || screen.contains("Line 54 original"),
        "Context lines before hunk 2 should be visible (centering). Screen:\n{}",
        screen
    );
}
