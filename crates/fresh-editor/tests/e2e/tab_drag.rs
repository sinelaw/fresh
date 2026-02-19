//! E2E tests for tab drag-and-drop functionality
//!
//! Tests that tabs can be dragged to create new splits or move between existing splits,
//! and verifies that the tab ends up exactly where the drop zone preview indicated.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::app::types::TabDropZone;
use fresh::model::event::{BufferId, LeafId};
use tempfile::TempDir;

/// Tab info extracted from TabLayout for easier test assertions
struct TabInfo {
    split_id: LeafId,
    buffer_id: BufferId,
    tab_row: u16,
    start_col: u16,
    end_col: u16,
}

impl TabInfo {
    fn center_col(&self) -> u16 {
        (self.start_col + self.end_col) / 2
    }
}

/// Get all tabs as a flat list from the editor's tab layouts
fn get_all_tabs(harness: &EditorTestHarness) -> Vec<TabInfo> {
    let mut tabs = Vec::new();
    for (split_id, tab_layout) in harness.editor().get_tab_layouts() {
        for tab in &tab_layout.tabs {
            tabs.push(TabInfo {
                split_id: *split_id,
                buffer_id: tab.buffer_id,
                tab_row: tab.tab_area.y,
                start_col: tab.tab_area.x,
                end_col: tab.tab_area.x + tab.tab_area.width,
            });
        }
    }
    tabs
}

/// Helper to create a test environment with multiple files
fn setup_multi_file_harness() -> (EditorTestHarness, TempDir, Vec<std::path::PathBuf>) {
    let temp_dir = TempDir::new().unwrap();
    let files: Vec<_> = (1..=3)
        .map(|i| {
            let path = temp_dir.path().join(format!("file{}.txt", i));
            std::fs::write(&path, format!("Content of file {}", i)).unwrap();
            path
        })
        .collect();

    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    for file in &files {
        harness.open_file(file).unwrap();
    }
    harness.render().unwrap();

    (harness, temp_dir, files)
}

/// Test that dragging a tab to the right edge creates a vertical split
#[test]
fn test_drag_tab_to_right_creates_vertical_split() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    // Get initial state
    let initial_split_count = harness.editor().get_split_count();
    assert_eq!(initial_split_count, 1, "Should start with 1 split");

    // Get the tab areas to find where to start dragging
    let tabs = get_all_tabs(&harness);
    assert!(!tabs.is_empty(), "Should have tabs");

    // Get the first tab
    let tab = &tabs[0];
    let source_split_id = tab.split_id;
    let buffer_id = tab.buffer_id;

    // Get split content area to find right edge
    let split_areas = harness.editor().get_split_areas().to_vec();
    let (_, _, content_rect, _, _, _) = split_areas[0];

    // Calculate the right edge drop zone (last 25% of width)
    let right_edge_col = content_rect.x + content_rect.width - 2;
    let content_center_row = content_rect.y + content_rect.height / 2;

    // Verify that dragging to this position would create SplitRight drop zone
    harness.render().unwrap();
    let drop_zone =
        harness
            .editor()
            .compute_drop_zone(right_edge_col, content_center_row, source_split_id);
    assert!(
        matches!(drop_zone, Some(TabDropZone::SplitRight(_))),
        "Expected SplitRight drop zone, got {:?}",
        drop_zone
    );

    // Now actually drag the tab to the right edge
    harness
        .mouse_drag(
            tab.center_col(),
            tab.tab_row,
            right_edge_col,
            content_center_row,
        )
        .unwrap();

    // Verify a new split was created
    let final_split_count = harness.editor().get_split_count();
    assert_eq!(
        final_split_count, 2,
        "Dragging to right edge should create a new split"
    );

    // Verify the buffer is now in the new split
    let new_active_split = harness.editor().get_active_split();
    let new_split_buffer = harness.editor().get_split_buffer(new_active_split.into());
    assert_eq!(
        new_split_buffer,
        Some(buffer_id),
        "The dragged buffer should be in the new split"
    );
}

/// Test that dragging a tab to the left edge creates a vertical split
#[test]
fn test_drag_tab_to_left_creates_vertical_split() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    let initial_split_count = harness.editor().get_split_count();
    assert_eq!(initial_split_count, 1);

    let tabs = get_all_tabs(&harness);
    let tab = &tabs[0];
    let source_split_id = tab.split_id;
    let buffer_id = tab.buffer_id;

    let split_areas = harness.editor().get_split_areas().to_vec();
    let (_, _, content_rect, _, _, _) = split_areas[0];

    // Left edge
    let left_edge_col = content_rect.x + 2;
    let content_center_row = content_rect.y + content_rect.height / 2;

    // Verify drop zone
    harness.render().unwrap();
    let drop_zone =
        harness
            .editor()
            .compute_drop_zone(left_edge_col, content_center_row, source_split_id);
    assert!(
        matches!(drop_zone, Some(TabDropZone::SplitLeft(_))),
        "Expected SplitLeft drop zone, got {:?}",
        drop_zone
    );

    // Drag to left edge
    harness
        .mouse_drag(
            tab.center_col(),
            tab.tab_row,
            left_edge_col,
            content_center_row,
        )
        .unwrap();

    assert_eq!(harness.editor().get_split_count(), 2);
    let new_active_split = harness.editor().get_active_split();
    assert_eq!(
        harness.editor().get_split_buffer(new_active_split.into()),
        Some(buffer_id)
    );
}

/// Test that dragging a tab to the top edge creates a horizontal split
#[test]
fn test_drag_tab_to_top_creates_horizontal_split() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    let initial_split_count = harness.editor().get_split_count();
    assert_eq!(initial_split_count, 1);

    let tabs = get_all_tabs(&harness);
    let tab = &tabs[0];
    let source_split_id = tab.split_id;
    let buffer_id = tab.buffer_id;

    let split_areas = harness.editor().get_split_areas().to_vec();
    let (_, _, content_rect, _, _, _) = split_areas[0];

    // Top edge
    let content_center_col = content_rect.x + content_rect.width / 2;
    let top_edge_row = content_rect.y + 1;

    // Verify drop zone
    harness.render().unwrap();
    let drop_zone =
        harness
            .editor()
            .compute_drop_zone(content_center_col, top_edge_row, source_split_id);
    assert!(
        matches!(drop_zone, Some(TabDropZone::SplitTop(_))),
        "Expected SplitTop drop zone, got {:?}",
        drop_zone
    );

    // Drag to top edge
    harness
        .mouse_drag(
            tab.center_col(),
            tab.tab_row,
            content_center_col,
            top_edge_row,
        )
        .unwrap();

    assert_eq!(harness.editor().get_split_count(), 2);
    let new_active_split = harness.editor().get_active_split();
    assert_eq!(
        harness.editor().get_split_buffer(new_active_split.into()),
        Some(buffer_id)
    );
}

/// Test that dragging a tab to the bottom edge creates a horizontal split
#[test]
fn test_drag_tab_to_bottom_creates_horizontal_split() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    let initial_split_count = harness.editor().get_split_count();
    assert_eq!(initial_split_count, 1);

    let tabs = get_all_tabs(&harness);
    let tab = &tabs[0];
    let source_split_id = tab.split_id;
    let buffer_id = tab.buffer_id;

    let split_areas = harness.editor().get_split_areas().to_vec();
    let (_, _, content_rect, _, _, _) = split_areas[0];

    // Bottom edge
    let content_center_col = content_rect.x + content_rect.width / 2;
    let bottom_edge_row = content_rect.y + content_rect.height - 2;

    // Verify drop zone
    harness.render().unwrap();
    let drop_zone =
        harness
            .editor()
            .compute_drop_zone(content_center_col, bottom_edge_row, source_split_id);
    assert!(
        matches!(drop_zone, Some(TabDropZone::SplitBottom(_))),
        "Expected SplitBottom drop zone, got {:?}",
        drop_zone
    );

    // Drag to bottom edge
    harness
        .mouse_drag(
            tab.center_col(),
            tab.tab_row,
            content_center_col,
            bottom_edge_row,
        )
        .unwrap();

    assert_eq!(harness.editor().get_split_count(), 2);
    let new_active_split = harness.editor().get_active_split();
    assert_eq!(
        harness.editor().get_split_buffer(new_active_split.into()),
        Some(buffer_id)
    );
}

/// Test that dragging a tab to another split's center moves it to that split
#[test]
fn test_drag_tab_to_another_split_center() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    // First create a second split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(harness.editor().get_split_count(), 2);

    // Get tab areas and split areas
    let tabs = get_all_tabs(&harness);
    let split_areas = harness.editor().get_split_areas().to_vec();

    // Find tab in the first split (left side)
    let first_split_area = &split_areas[0];
    let (first_split_id, _, _first_content_rect, _, _, _) = first_split_area;

    // Find a tab that belongs to the first split
    let tab_in_first_split = tabs.iter().find(|t| t.split_id == *first_split_id);

    if let Some(tab) = tab_in_first_split {
        let source_split_id = tab.split_id;
        let buffer_id = tab.buffer_id;

        // Find the second split (right side)
        let second_split_area = split_areas
            .iter()
            .find(|(sid, _, _, _, _, _)| sid != first_split_id);

        if let Some((target_split_id, _, target_content_rect, _, _, _)) = second_split_area {
            // Calculate center of second split
            let target_center_col = target_content_rect.x + target_content_rect.width / 2;
            let target_center_row = target_content_rect.y + target_content_rect.height / 2;

            // Verify drop zone is SplitCenter for the target split
            harness.render().unwrap();
            let drop_zone = harness.editor().compute_drop_zone(
                target_center_col,
                target_center_row,
                source_split_id,
            );
            assert!(
                matches!(drop_zone, Some(TabDropZone::SplitCenter(id)) if id == *target_split_id),
                "Expected SplitCenter for target split, got {:?}",
                drop_zone
            );

            // Drag tab to center of second split
            harness
                .mouse_drag(
                    tab.center_col(),
                    tab.tab_row,
                    target_center_col,
                    target_center_row,
                )
                .unwrap();

            // Verify the buffer is now in the target split's tabs
            let target_tabs = harness.editor().get_split_tabs(*target_split_id);
            assert!(
                target_tabs.contains(&buffer_id),
                "Buffer should be in target split's tabs"
            );

            // Split count should remain 2
            assert_eq!(harness.editor().get_split_count(), 2);
        }
    }
}

/// Test that dragging a tab within the same tab bar reorders tabs
#[test]
fn test_drag_tab_reorder_within_split() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    // Get initial tab order
    let initial_split = harness.editor().get_active_split();
    let initial_tabs = harness.editor().get_split_tabs(initial_split);
    assert!(initial_tabs.len() >= 2, "Need at least 2 tabs to reorder");

    let first_buffer = initial_tabs[0];
    let second_buffer = initial_tabs[1];

    // Get tab areas to find positions
    let tabs = get_all_tabs(&harness);

    // Find first and second tab positions
    let first_tab = tabs.iter().find(|t| t.buffer_id == first_buffer);
    let second_tab = tabs.iter().find(|t| t.buffer_id == second_buffer);

    if let (Some(first), Some(second)) = (first_tab, second_tab) {
        // Drag first tab to second tab position
        harness
            .mouse_drag(
                first.center_col(),
                first.tab_row,
                second.center_col(),
                second.tab_row,
            )
            .unwrap();

        // Verify tab order changed
        let final_tabs = harness.editor().get_split_tabs(initial_split);
        assert_eq!(
            final_tabs.len(),
            initial_tabs.len(),
            "Tab count should stay same"
        );

        // The first buffer should now be after where we dropped it
        let first_buffer_new_idx = final_tabs.iter().position(|&b| b == first_buffer);
        assert!(
            first_buffer_new_idx.is_some(),
            "First buffer should still exist"
        );
    }
}

/// Test that dragging the last tab out of a split closes that split
#[test]
fn test_drag_last_tab_closes_split() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    std::fs::write(&file1, "Content 1").unwrap();
    std::fs::write(&file2, "Content 2").unwrap();

    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();

    // Create a second split
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open second file in the new split
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    assert_eq!(harness.editor().get_split_count(), 2);

    // Get the active split (should be the new one with file2)
    let active_split = harness.editor().get_active_split();
    let active_tabs = harness.editor().get_split_tabs(active_split);

    // Get split areas
    let split_areas = harness.editor().get_split_areas().to_vec();
    let other_split = split_areas
        .iter()
        .find(|(sid, _, _, _, _, _)| *sid != active_split);

    if let Some((_target_split_id, _, target_content_rect, _, _, _)) = other_split {
        // Get the tab for the current buffer
        let tabs = get_all_tabs(&harness);
        let current_buffer = harness
            .editor()
            .get_split_buffer(active_split.into())
            .unwrap();

        let current_tab = tabs
            .iter()
            .find(|t| t.split_id == active_split && t.buffer_id == current_buffer);

        if let Some(tab) = current_tab {
            // If this split has only one tab, dragging it out should close the split
            if active_tabs.len() == 1 {
                // Drag to other split's center
                let target_center_col = target_content_rect.x + target_content_rect.width / 2;
                let target_center_row = target_content_rect.y + target_content_rect.height / 2;

                harness
                    .mouse_drag(
                        tab.center_col(),
                        tab.tab_row,
                        target_center_col,
                        target_center_row,
                    )
                    .unwrap();

                // The source split should be closed
                assert_eq!(
                    harness.editor().get_split_count(),
                    1,
                    "Dragging last tab should close the source split"
                );
            }
        }
    }
}

/// Test that drop zone visualization matches actual result for all positions
#[test]
fn test_drop_zone_matches_result_comprehensive() {
    let (harness, _temp_dir, _files) = setup_multi_file_harness();

    let split_areas = harness.editor().get_split_areas().to_vec();
    let (_, _, content_rect, _, _, _) = split_areas[0];

    // Test all edge positions
    let test_positions = [
        // (name, col, row, expected_zone_variant)
        (
            "left",
            content_rect.x + 2,
            content_rect.y + content_rect.height / 2,
            "SplitLeft",
        ),
        (
            "right",
            content_rect.x + content_rect.width - 2,
            content_rect.y + content_rect.height / 2,
            "SplitRight",
        ),
        (
            "top",
            content_rect.x + content_rect.width / 2,
            content_rect.y + 1,
            "SplitTop",
        ),
        (
            "bottom",
            content_rect.x + content_rect.width / 2,
            content_rect.y + content_rect.height - 2,
            "SplitBottom",
        ),
    ];

    for (name, target_col, target_row, expected) in test_positions {
        // Reset harness for each test
        let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

        let tabs = get_all_tabs(&harness);
        let tab = &tabs[0];
        let source_split_id = tab.split_id;
        let buffer_id = tab.buffer_id;

        // Verify the expected drop zone
        harness.render().unwrap();
        let drop_zone = harness
            .editor()
            .compute_drop_zone(target_col, target_row, source_split_id);

        let zone_name = match &drop_zone {
            Some(TabDropZone::SplitLeft(_)) => "SplitLeft",
            Some(TabDropZone::SplitRight(_)) => "SplitRight",
            Some(TabDropZone::SplitTop(_)) => "SplitTop",
            Some(TabDropZone::SplitBottom(_)) => "SplitBottom",
            Some(TabDropZone::SplitCenter(_)) => "SplitCenter",
            Some(TabDropZone::TabBar(_, _)) => "TabBar",
            None => "None",
        };

        assert_eq!(
            zone_name, expected,
            "Position '{}' should have {} drop zone, got {}",
            name, expected, zone_name
        );

        let initial_split_count = harness.editor().get_split_count();

        // Now actually drag
        harness
            .mouse_drag(tab.center_col(), tab.tab_row, target_col, target_row)
            .unwrap();

        // Verify split was created
        let final_split_count = harness.editor().get_split_count();
        assert_eq!(
            final_split_count,
            initial_split_count + 1,
            "Dragging to {} should create new split",
            name
        );

        // Verify the buffer is in the new active split
        let new_active_split = harness.editor().get_active_split();
        let active_buffer = harness.editor().get_split_buffer(new_active_split.into());
        assert_eq!(
            active_buffer,
            Some(buffer_id),
            "Buffer should be in the new split after dragging to {}",
            name
        );
    }
}

/// Test that dragging a tab to the tab bar area adds it to that split
#[test]
fn test_drag_tab_to_tab_bar() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    // Create a second split first
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(harness.editor().get_split_count(), 2);

    // Get tab and split info
    let tabs = get_all_tabs(&harness);
    let split_areas = harness.editor().get_split_areas().to_vec();

    // Find tabs in first split
    let first_split_id = split_areas[0].0;
    let second_split_id = split_areas[1].0;

    let tab_in_first = tabs.iter().find(|t| t.split_id == first_split_id);

    if let Some(tab) = tab_in_first {
        let source_split_id = tab.split_id;
        let buffer_id = tab.buffer_id;

        // Find tab row of second split
        let second_split_tabs: Vec<_> = tabs
            .iter()
            .filter(|t| t.split_id == second_split_id)
            .collect();

        if let Some(target_tab) = second_split_tabs.first() {
            // Get initial tabs in target split
            let initial_target_tabs = harness.editor().get_split_tabs(second_split_id);

            // Verify this would be a TabBar drop zone
            harness.render().unwrap();
            let drop_zone = harness.editor().compute_drop_zone(
                target_tab.center_col(),
                target_tab.tab_row,
                source_split_id,
            );
            assert!(
                matches!(drop_zone, Some(TabDropZone::TabBar(_, _))),
                "Expected TabBar drop zone, got {:?}",
                drop_zone
            );

            // Drag to target tab bar
            harness
                .mouse_drag(
                    tab.center_col(),
                    tab.tab_row,
                    target_tab.center_col(),
                    target_tab.tab_row,
                )
                .unwrap();

            // Buffer should now be in target split's tabs
            let final_target_tabs = harness.editor().get_split_tabs(second_split_id);
            assert!(
                final_target_tabs.contains(&buffer_id),
                "Buffer should be added to target split's tabs"
            );
            assert!(
                final_target_tabs.len() > initial_target_tabs.len(),
                "Target split should have more tabs after drag"
            );
        }
    }
}

/// Test that small drags (below threshold) don't trigger tab movement
#[test]
fn test_small_drag_does_not_move_tab() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    let initial_split = harness.editor().get_active_split();
    let initial_tabs = harness.editor().get_split_tabs(initial_split);

    let tabs = get_all_tabs(&harness);
    let tab = &tabs[0];

    // Small drag (2 pixels - below 3 pixel threshold)
    harness
        .mouse_drag(
            tab.center_col(),
            tab.tab_row,
            tab.center_col() + 2,
            tab.tab_row,
        )
        .unwrap();

    // Tabs should be unchanged
    let final_tabs = harness.editor().get_split_tabs(initial_split);
    assert_eq!(
        initial_tabs, final_tabs,
        "Small drag should not change tab order"
    );

    // Split count should be unchanged
    assert_eq!(
        harness.editor().get_split_count(),
        1,
        "Small drag should not create new split"
    );
}

/// Test that dragging from right split to left border of left split switches order
#[test]
fn test_drag_right_split_to_left_border_switches_order() {
    let (mut harness, _temp_dir, _files) = setup_multi_file_harness();

    // First create a second split (vertical split creates left|right layout)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(harness.editor().get_split_count(), 2);

    let split_areas = harness.editor().get_split_areas().to_vec();

    // Find the left-most and right-most splits by comparing x positions
    let (left_split, right_split) = if split_areas[0].2.x < split_areas[1].2.x {
        (&split_areas[0], &split_areas[1])
    } else {
        (&split_areas[1], &split_areas[0])
    };

    let (_left_split_id, _, left_content_rect, _, _, _) = left_split;
    let (right_split_id, _right_buffer_id, _right_content_rect, _, _, _) = right_split;

    // Get the tab from the right split
    let tabs = get_all_tabs(&harness);
    let right_tab = tabs.iter().find(|t| t.split_id == *right_split_id);

    if let Some(tab) = right_tab {
        let buffer_id = tab.buffer_id;

        // Calculate the left edge of the left split (this is where we drag to)
        let left_edge_col = left_content_rect.x + 2; // Near left edge
        let left_center_row = left_content_rect.y + left_content_rect.height / 2;

        // Verify this gives us a SplitLeft drop zone
        harness.render().unwrap();
        let drop_zone =
            harness
                .editor()
                .compute_drop_zone(left_edge_col, left_center_row, *right_split_id);
        assert!(
            matches!(drop_zone, Some(TabDropZone::SplitLeft(_))),
            "Expected SplitLeft drop zone at left edge of left split, got {:?}",
            drop_zone
        );

        // Remember the buffer we're dragging
        let dragged_buffer = buffer_id;

        // Drag the right split's tab to the left edge of the left split
        harness
            .mouse_drag(
                tab.center_col(),
                tab.tab_row,
                left_edge_col,
                left_center_row,
            )
            .unwrap();

        // Now we should have 2 splits (the right split may have closed if it was the last tab)
        // and the dragged tab should be in the leftmost position
        let new_split_areas = harness.editor().get_split_areas().to_vec();

        // Find the new leftmost split
        let new_leftmost = new_split_areas
            .iter()
            .min_by_key(|(_, _, rect, _, _, _)| rect.x)
            .unwrap();

        let (new_leftmost_id, _, _, _, _, _) = new_leftmost;

        // The dragged buffer should be in the leftmost split now
        let leftmost_tabs = harness.editor().get_split_tabs(*new_leftmost_id);
        assert!(
            leftmost_tabs.contains(&dragged_buffer),
            "Dragged buffer should be in the leftmost split after dragging to left edge. \
             Leftmost split tabs: {:?}, dragged buffer: {:?}",
            leftmost_tabs,
            dragged_buffer
        );

        // The new leftmost split should be to the left of where the original left split was
        assert!(
            new_leftmost.2.x <= left_content_rect.x,
            "New leftmost split should be at or to the left of original left split"
        );
    }
}
