//! Migration of `tests/e2e/side_by_side_diff_hunk_nav.rs` — hunk
//! navigation in side-by-side diff composite buffers (issue #2058
//! per-row-text sweep).
//!
//! Load-bearing claims preserved here:
//!
//!   1. `composite_next_hunk_active` jumps to each successive hunk
//!      in a multi-hunk diff and centers it so the hunk's MODIFIED
//!      content becomes visible.
//!   2. `composite_prev_hunk_active` jumps back to the previous
//!      hunk.
//!   3. The hunk is centered with context lines above it (not just
//!      placed at the top row of the viewport).
//!   4. `initial_focus_hunk = Some(0)` auto-scrolls to the first
//!      hunk on the first render — no imperative `composite_next_hunk`
//!      call required, AND Line 1 (the buffer start) is pushed off
//!      the top of the viewport.
//!   5. `initial_focus_hunk = Some(2)` auto-scrolls to the third
//!      hunk on the first render.
//!   6. `initial_focus_hunk` is a one-shot — after the first render
//!      consumes it, the field is set back to `None`, and a
//!      subsequent user scroll is not snapped back to the focus hunk.
//!   7. `flush_layout` materializes `CompositeViewState` before a
//!      render, enabling `composite_next_hunk_active` to succeed; the
//!      same call returns `false` if no view state exists yet.
//!   8. `flush_layout` + multiple `composite_next_hunk_active` calls
//!      let a test reach hunk 3 before the first render — the full
//!      imperative alternative to `initial_focus_hunk`.
//!   9. Keybinding `n` in a composite buffer view navigates to the
//!      next hunk via the Action-based keymap (not the hardcoded
//!      router).
//!  10. Keybindings `]` / `[` / `p` work as forward / back-aliases
//!      for hunk navigation, alongside `n`.
//!
//! ## Harness-direct pattern
//!
//! Composite-buffer construction (`create_virtual_buffer`,
//! `set_virtual_buffer_content`, `create_composite_buffer`,
//! `set_composite_alignment`, `get_composite_mut`,
//! `composite_next_hunk_active`, `composite_prev_hunk_active`,
//! `flush_layout`, `switch_buffer`, `initial_focus_hunk`) has no
//! `EditorTestApi` projection — these are direct accessors on
//! `Editor` / `Window` / `CompositeBuffer`. The migrated tests
//! therefore use the harness-direct pattern (the same pattern
//! `migrated_horizontal_scrollbar.rs` uses for scrollbar geometry,
//! and `migrated_line_wrap_parity.rs` uses for cursor parity).
//!
//! Per-row screen-text assertions go through
//! `RenderSnapshot::extract_with_rendered_rows` +
//! `RowMatch::{AnyRowContains, NoRowContains}` so the assertions
//! run against the real vt100 round-trip output, matching the e2e
//! `harness.screen_to_string().contains(..)` semantics.
//!
//! Source: `tests/e2e/side_by_side_diff_hunk_nav.rs` (10 tests
//! migrated; no tests deferred).

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};
use crossterm::event::{KeyCode, KeyModifiers};
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

/// Helper that creates a composite buffer with initial_focus_hunk set,
/// WITHOUT calling compositeNextHunk afterwards. The first render should
/// auto-scroll to the specified hunk.
fn setup_diff_with_initial_focus(
    harness: &mut EditorTestHarness,
    old_content: &str,
    new_content: &str,
    hunks: &[DiffHunk],
    initial_focus_hunk: usize,
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

    harness
        .editor_mut()
        .active_window_mut()
        .get_composite_mut(composite_id)
        .unwrap()
        .initial_focus_hunk = Some(initial_focus_hunk);

    harness.editor_mut().switch_buffer(composite_id);
    harness.render().unwrap();

    composite_id
}

/// Assert that some rendered row contains `substring` (after the
/// real vt100 round-trip). Mirrors the e2e
/// `harness.screen_to_string().contains(..)` pattern via
/// `RenderSnapshot::extract_with_rendered_rows`.
fn assert_any_row_contains(harness: &mut EditorTestHarness, substring: &str, msg: &str) {
    let snap = RenderSnapshot::extract_with_rendered_rows(harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::AnyRowContains(substring.into())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "{msg}: {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

/// Assert that NO rendered row contains `substring`.
fn assert_no_row_contains(harness: &mut EditorTestHarness, substring: &str, msg: &str) {
    let snap = RenderSnapshot::extract_with_rendered_rows(harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::NoRowContains(substring.into())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "{msg}: {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_next_hunk_navigation_shows_hunk_content() {
    // Original: `test_next_hunk_navigation_shows_hunk_content`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Initially, Line 1 should be visible (we're at the top).
    assert_any_row_contains(
        &mut harness,
        "Line 1 original",
        "Initial view should show Line 1",
    );

    // Jump to hunk 1 (around line 20).
    harness
        .editor_mut()
        .active_window_mut()
        .composite_next_hunk_active(composite_id);
    harness.render().unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 1",
        "After first next_hunk, hunk 1 content should be visible",
    );

    // Jump to hunk 2 (around line 60).
    harness
        .editor_mut()
        .active_window_mut()
        .composite_next_hunk_active(composite_id);
    harness.render().unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 2",
        "After second next_hunk, hunk 2 content should be visible",
    );

    // Jump to hunk 3 (around line 120).
    harness
        .editor_mut()
        .active_window_mut()
        .composite_next_hunk_active(composite_id);
    harness.render().unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 3",
        "After third next_hunk, hunk 3 content should be visible",
    );
}

#[test]
fn migrated_prev_hunk_navigation() {
    // Original: `test_prev_hunk_navigation`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Navigate to hunk 3.
    for _ in 0..3 {
        harness
            .editor_mut()
            .active_window_mut()
            .composite_next_hunk_active(composite_id);
    }
    harness.render().unwrap();
    assert_any_row_contains(&mut harness, "MODIFIED in hunk 3", "Should be at hunk 3");

    // Go back to hunk 2.
    harness
        .editor_mut()
        .active_window_mut()
        .composite_prev_hunk_active(composite_id);
    harness.render().unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 2",
        "After prev_hunk, hunk 2 content should be visible",
    );
}

#[test]
fn migrated_hunk_navigation_shows_context_above() {
    // Original: `test_hunk_navigation_shows_context_above`. Hunk is
    // centered with ~1/3 of the viewport as context above it.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Jump to hunk 2 (at line 60).
    harness
        .editor_mut()
        .active_window_mut()
        .composite_next_hunk_active(composite_id);
    harness
        .editor_mut()
        .active_window_mut()
        .composite_next_hunk_active(composite_id);
    harness.render().unwrap();

    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 2",
        "Hunk 2 content should be visible",
    );
    // With 40-line viewport and 1/3 context above (~13 lines), lines
    // around 54-57 should be visible before the hunk at line 60.
    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let context_visible = ["Line 54 original", "Line 55 original", "Line 56 original", "Line 57 original"]
        .iter()
        .any(|needle| {
            snap.rendered_rows
                .iter()
                .any(|r| r.trim_end().contains(needle))
        });
    assert!(
        context_visible,
        "Context lines before hunk 2 should be visible (centering). rows={:#?}",
        snap.rendered_rows
    );
}

#[test]
fn migrated_initial_focus_hunk_scrolls_to_first_hunk_on_first_render() {
    // Original: `test_initial_focus_hunk_scrolls_to_first_hunk_on_first_render`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let _composite_id =
        setup_diff_with_initial_focus(&mut harness, &old_content, &new_content, &hunks, 0);

    // First hunk is at line 20. With initial_focus_hunk=0, first render
    // should scroll there. Line 1 should NOT be visible.
    assert_no_row_contains(
        &mut harness,
        "Line 1 original",
        "Line 1 should NOT be visible when initial_focus_hunk=0",
    );
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 1",
        "Hunk 1 content should be visible on first render",
    );
}

#[test]
fn migrated_initial_focus_hunk_scrolls_to_nth_hunk() {
    // Original: `test_initial_focus_hunk_scrolls_to_nth_hunk`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let _composite_id =
        setup_diff_with_initial_focus(&mut harness, &old_content, &new_content, &hunks, 2);

    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 3",
        "Hunk 3 content should be visible on first render with initial_focus_hunk=2",
    );
}

#[test]
fn migrated_initial_focus_hunk_is_consumed_after_first_render() {
    // Original: `test_initial_focus_hunk_is_consumed_after_first_render`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let composite_id =
        setup_diff_with_initial_focus(&mut harness, &old_content, &new_content, &hunks, 2);

    // First render scrolled to hunk 3.
    assert_any_row_contains(&mut harness, "MODIFIED in hunk 3", "First render at hunk 3");

    // Manually scroll back to top.
    for _ in 0..50 {
        harness.mouse_scroll_up(60, 20).unwrap();
    }
    harness.render().unwrap();

    // Should see early content, NOT snapped back to hunk 3.
    assert_any_row_contains(
        &mut harness,
        "Line 1 original",
        "After scrolling up, should see Line 1 (initial_focus_hunk should not re-apply)",
    );

    // Verify the flag was consumed.
    assert!(
        harness
            .editor_mut()
            .active_window_mut()
            .get_composite_mut(composite_id)
            .unwrap()
            .initial_focus_hunk
            .is_none(),
        "initial_focus_hunk should be None after first render consumed it"
    );
}

#[test]
fn migrated_flush_layout_enables_hunk_nav_before_render() {
    // Original: `test_flush_layout_enables_hunk_nav_before_render`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let _composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Build a second composite buffer that we will switch to WITHOUT
    // rendering — so its view state isn't yet materialized.
    let old_buffer_id = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("OLD2".to_string(), "text".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(old_buffer_id, vec![TextPropertyEntry::text(&old_content)])
        .unwrap();

    let new_buffer_id = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("NEW2".to_string(), "text".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(new_buffer_id, vec![TextPropertyEntry::text(&new_content)])
        .unwrap();

    let sources = vec![
        SourcePane::new(old_buffer_id, "OLD", false).with_style(PaneStyle::old_diff()),
        SourcePane::new(new_buffer_id, "NEW", false).with_style(PaneStyle::new_diff()),
    ];

    let layout = CompositeLayout::SideBySide {
        ratios: vec![0.5, 0.5],
        show_separator: true,
    };

    let composite_id2 = harness.editor_mut().create_composite_buffer(
        "Diff View 2".to_string(),
        "diff-view".to_string(),
        layout,
        sources,
    );

    let old_line_count = old_content.lines().count();
    let new_line_count = new_content.lines().count();
    let alignment = LineAlignment::from_hunks(&hunks, old_line_count, new_line_count);
    harness
        .editor_mut()
        .active_window_mut()
        .set_composite_alignment(composite_id2, alignment);

    // Switch to the new composite buffer WITHOUT rendering.
    harness.editor_mut().switch_buffer(composite_id2);

    // Without flushLayout, composite_next_hunk returns false.
    let result_without_flush = harness
        .editor_mut()
        .active_window_mut()
        .composite_next_hunk_active(composite_id2);
    assert!(
        !result_without_flush,
        "composite_next_hunk should fail without flushLayout (no view state)"
    );

    // Call flushLayout to materialize the view state.
    harness.editor_mut().flush_layout();

    // Now composite_next_hunk should succeed.
    let result_with_flush = harness
        .editor_mut()
        .active_window_mut()
        .composite_next_hunk_active(composite_id2);
    assert!(
        result_with_flush,
        "composite_next_hunk should succeed after flushLayout"
    );

    // Render and verify the hunk is visible.
    harness.render().unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 1",
        "Hunk 1 should be visible after flushLayout + composite_next_hunk",
    );
}

#[test]
fn migrated_flush_layout_jump_to_third_hunk_before_render() {
    // Original: `test_flush_layout_jump_to_third_hunk_before_render`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();

    let old_buffer_id = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("OLD".to_string(), "text".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(old_buffer_id, vec![TextPropertyEntry::text(&old_content)])
        .unwrap();

    let new_buffer_id = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("NEW".to_string(), "text".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(new_buffer_id, vec![TextPropertyEntry::text(&new_content)])
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
    let alignment = LineAlignment::from_hunks(&hunks, old_line_count, new_line_count);
    harness
        .editor_mut()
        .active_window_mut()
        .set_composite_alignment(composite_id, alignment);

    // Switch without rendering.
    harness.editor_mut().switch_buffer(composite_id);

    // flushLayout, then jump 3 times to reach hunk 3.
    harness.editor_mut().flush_layout();
    for _ in 0..3 {
        harness
            .editor_mut()
            .active_window_mut()
            .composite_next_hunk_active(composite_id);
    }

    // First render should show hunk 3.
    harness.render().unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 3",
        "Hunk 3 should be visible after flushLayout + 3x next_hunk (no prior render)",
    );
}

#[test]
fn migrated_keybinding_n_navigates_to_next_hunk() {
    // Original: `test_keybinding_n_navigates_to_next_hunk`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let _composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Initially at the top — Line 1 should be visible.
    assert_any_row_contains(
        &mut harness,
        "Line 1 original",
        "Initial view should show Line 1",
    );

    // Press 'n' to navigate to the next hunk (hunk 1, around line 20).
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 1",
        "After pressing 'n', hunk 1 content should be visible",
    );

    // Press 'n' again to navigate to hunk 2 (around line 60).
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 2",
        "After pressing 'n' twice, hunk 2 content should be visible",
    );
}

#[test]
fn migrated_keybinding_p_and_brackets_navigate_hunks() {
    // Original: `test_keybinding_p_and_brackets_navigate_hunks`.
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let _composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // Use ']' to navigate forward to hunk 1.
    harness
        .send_key(KeyCode::Char(']'), KeyModifiers::NONE)
        .unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 1",
        "After pressing ']', hunk 1 content should be visible",
    );

    // Use ']' again to go to hunk 2.
    harness
        .send_key(KeyCode::Char(']'), KeyModifiers::NONE)
        .unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 2",
        "After pressing ']' twice, hunk 2 content should be visible",
    );

    // Use '[' to go back to hunk 1.
    harness
        .send_key(KeyCode::Char('['), KeyModifiers::NONE)
        .unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 1",
        "After pressing '[', hunk 1 content should be visible",
    );

    // Navigate forward past hunk 1 again with 'n', then back with 'p'.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 3",
        "After 'n' twice from hunk 1, should be at hunk 3",
    );

    // Press 'p' to go back to hunk 2.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::NONE)
        .unwrap();
    assert_any_row_contains(
        &mut harness,
        "MODIFIED in hunk 2",
        "After pressing 'p', hunk 2 content should be visible",
    );
}

// =============================================================================
// Anti-tests
// =============================================================================

/// Anti-test: drop the second + third `composite_next_hunk_active`
/// calls. Without them, the viewport never reaches hunk 2 (line 60,
/// outside the default 40-row viewport that starts at line 1) and
/// "MODIFIED in hunk 2" must NOT be on screen. Proves the positive
/// `migrated_next_hunk_navigation_shows_hunk_content` claim depends
/// on the repeated nav calls actually advancing through the hunk
/// list, not on the viewport accidentally containing hunk 2.
#[test]
fn anti_next_hunk_without_call_keeps_hunk_off_screen() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let _composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // No composite_next_hunk_active calls here — that's the dropped step.
    // Hunk 2 lives at line 60, well past the default viewport (lines 1-34).
    assert_no_row_contains(
        &mut harness,
        "MODIFIED in hunk 2",
        "anti: without next_hunk, hunk 2 content must NOT be visible \
         (proves the positive test depends on the actual nav call)",
    );
    assert_no_row_contains(
        &mut harness,
        "MODIFIED in hunk 3",
        "anti: without next_hunk, hunk 3 content must NOT be visible",
    );
}

/// Anti-test: drop the `'n'` keypresses. Without them, the viewport
/// never reaches hunk 2 (line 60, outside the default viewport) so
/// "MODIFIED in hunk 2" must NOT appear. Proves the positive
/// `migrated_keybinding_n_navigates_to_next_hunk` claim depends on
/// the keypress dispatch routing through the Action-based keymap.
#[test]
fn anti_keybinding_n_without_press_keeps_hunk_off_screen() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    let _composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    // No send_key('n') here — that's the dropped step. Hunk 2 is at
    // line 60, off the bottom of the default 40-row viewport.
    assert_no_row_contains(
        &mut harness,
        "MODIFIED in hunk 2",
        "anti: without pressing 'n', hunk 2 content must NOT be visible \
         (proves the positive keybinding test depends on the keypress)",
    );
}

/// Anti-test: drop the `initial_focus_hunk = Some(2)` assignment.
/// Without it, the first render leaves the viewport at the top
/// of the buffer and Line 1 must be visible (hunk 3 must NOT be).
/// Proves the positive
/// `migrated_initial_focus_hunk_scrolls_to_nth_hunk` claim depends
/// on the `initial_focus_hunk` field actually being set, not on
/// the composite buffer auto-snapping somewhere.
#[test]
fn anti_initial_focus_hunk_unset_starts_at_buffer_top() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let (old_content, new_content, hunks) = generate_multi_hunk_content();
    // setup_diff does NOT set initial_focus_hunk — this is the
    // dropped step (compared to setup_diff_with_initial_focus).
    let _composite_id = setup_diff(&mut harness, &old_content, &new_content, &hunks);

    assert_any_row_contains(
        &mut harness,
        "Line 1 original",
        "anti: without initial_focus_hunk, first render shows Line 1",
    );
    assert_no_row_contains(
        &mut harness,
        "MODIFIED in hunk 3",
        "anti: without initial_focus_hunk=Some(2), hunk 3 must NOT \
         auto-appear on the first render",
    );
}
